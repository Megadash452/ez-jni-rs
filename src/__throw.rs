pub use either::Either;
use jni::{
    objects::{GlobalRef, JObject, JString, JThrowable}, JNIEnv
};
use std::{any::Any, sync::RwLock};
use crate::FromException;
use crate::eprintln;

/// A lot like [std::panic::Location], but uses a [String] instead of `&str`.
struct PanicLocation {
    file: String,
    line: u32,
    col: u32,
}
impl<'a> From<&std::panic::Location<'a>> for PanicLocation {
    fn from(value: &std::panic::Location<'a>) -> Self {
        Self {
            file: value.file().to_string(),
            line: value.line(),
            col: value.column(),
        }
    }
}
impl std::fmt::Display for PanicLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.col)
    }
}
static PANIC_LOCATION: RwLock<Option<PanicLocation>> = RwLock::new(None);

/// Tells whether [`set_panic_hook()`] was called and the *hook* was set.
/// 
/// This will only be `true` if the Rust code was called from Java (i.e. a [`jni_fn`][ez_jni::jni_fn!]).
static PANIC_HOOK_SET: RwLock<bool> = RwLock::new(false);
/// Sets a panic hook to grab [`PANIC_LOCATION`] data.
fn set_panic_hook() {
    std::panic::set_hook(Box::new(|info| {
        if let Ok(mut panic_hook_set) = PANIC_HOOK_SET.write() {
            *panic_hook_set = true;
        }
        if let Ok(mut panic_location) = PANIC_LOCATION.write() {
            if let Some(location) = info.location() {
                *panic_location = Some(PanicLocation::from(location))
            }
        }
    }));
}

/// Runs a Rust function and returns its value, catching any `panics!` and throwing them as Java Exceptions.
///
/// If **f** `panics!`, this will return the [`Zeroed`][std::mem::zeroed()] representation of the return type.
/// This means that this function should only return directly to Java.
/// 
/// DO NOT try to operate on the return value because you will have no indication wether **f** panicked and returned *zeroed*.
/// 
/// This function is used by [ez_jni_macros::jni_fn].
pub fn catch_throw<'local, R>(
    env: &mut JNIEnv<'local>,
    f: impl FnOnce(&mut JNIEnv<'local>) -> R,
) -> R {
    set_panic_hook();
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| f(env))) {
        Ok(r) => r,
        Err(payload) => {
            throw_panic(env, payload);
            unsafe { std::mem::zeroed() }
        }
    }
}
/// Same as [`catch_throw()`], but maps the returned `R` to a Java value `J`.
pub fn catch_throw_map<'local, R, J>(
    env: &mut JNIEnv<'local>,
    f: impl FnOnce(&mut JNIEnv<'local>) -> R,
    // map function should not capture variables
    map: fn(R, &mut JNIEnv<'local>) -> J,
) -> J {
    set_panic_hook();
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| f(env)))
        .and_then(|r| std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| map(r, env))))
    {
        Ok(j) => j,
        Err(payload) => {
            throw_panic(env, payload);
            unsafe { std::mem::zeroed() }
        }
    }
}
fn throw_panic(env: &mut JNIEnv, payload: Box<dyn Any + Send>) {
    let panic_msg = match payload.downcast::<&'static str>() {
        Ok(msg) => Some(msg.as_ref().to_string()),
        Err(payload) => match payload.downcast::<String>() {
            Ok(msg) => Some(*msg),
            Err(payload) => match payload.downcast::<GlobalRef>() {
                Ok(exception) => {
                    env.throw(<&JThrowable>::from(exception.as_obj())).unwrap();
                    return;
                },
                // Unexpected panic type
                Err(_) => None,
            },
        },
    };
    let msg = match (panic_msg, &*PANIC_LOCATION.read().unwrap()) {
        (Some(msg), Some(info)) => format!("panicked at {info}: {msg}"),
        (Some(msg), None) => format!("panicked at unknown location: {msg}"),
        (None, Some(info)) => format!("Rust panicked at {info}, but could not obtain message"),
        (None, None) => "Rust had a panic! but could not obtain any panic data".to_string(),
    };
    // clear any exceptions before throwing the new exception
    let _ = env.exception_clear();
    let _ = env.throw_new("java/lang/Exception", msg);
}

/// Checks if an exception has been thrown from a previous JNI function call,
/// and tries to convert it to an `E` so that it can be [`mapped`][Result::map] to get a `Result<T, E>`.
/// 
/// If the `Exception` can't be converted to an `E`,
/// the program will `panic!` and the `Exception` will be rethrown,
/// in a similar way to how [`panic_uncaught_exception()`] does it.
/// 
/// See [`try_catch`] if panicking is not intended.
///
/// This function is used by [ez_jni_macros::call!].
pub fn catch<'local, E: FromException<'local>>(env: &mut JNIEnv<'local>) -> Result<(), E> {
    match catch_exception(env) {
        Some(ex) => match E::from_exception(&ex, env) {
            Ok(e) => Err(e),
            Err(err) => {
                eprintln!("Attempted to catch an exception, but failed to convert it to a concrete type:\n{err}");
                env.throw(ex).unwrap();
                ::std::panic::panic_any(());
            }
        }
        None => Ok(()),
    }
}
/// Similar to [`catch()`], but will not panic if the exception's type did not match the desired type.
/// Instead it will continue to be thrown until another [`try_catch`] is called with the correct type.
/// 
/// Returns the Exception if it was caught.
pub fn try_catch<'local, E: FromException<'local>>(env: &mut JNIEnv<'local>) -> Option<E> {
    catch_exception(env)
        .and_then(|ex|
            E::from_exception(&ex, env)
                .inspect_err(|_| env.throw(ex).unwrap())
                .ok()
        )
}

/// Panics with a *Java Exception* instead of  *Rust String message*.
pub fn panic_exception(ex: JThrowable, env: &mut JNIEnv) -> ! {
    let ex = env.new_global_ref(ex)
        .unwrap_or_else(|err| panic!("Unable to create Global Reference for Exception Object: {err}"));
    ::std::panic::panic_any(ex)
}

/// Checks if there is an **exception** that was thrown by a Java method called from Rust,
/// and that was not caught by the user (did not use [`Result`] for the return type),
/// and **panics** with that exception.
///
/// **target** is a *class* or an *object* (whose class will be determined).
/// **method_name** is the name of the Java Method that threw the *exception*
///
/// This function is used by [ez_jni_macros::call!].
pub fn panic_uncaught_exception(
    env: &mut JNIEnv,
    target: Either<&str, &JObject>,
    method_name: impl AsRef<str>,
) {
    if let Some(ex) = catch_exception(env) {
        // If the panic hook was set, this means that the panic payload should be the Exception so that it can be rethrown to Java.
        if *PANIC_HOOK_SET.read().unwrap() {
            let class = match target {
                Either::Left(s) => s.to_string(),
                Either::Right(obj) => env
                    .get_object_class(obj)
                    .and_then(|class| env.call_method(class, "getName", "()Ljava/lang/String;", &[]))
                    .and_then(|class| class.l())
                    .and_then(|class| {
                        unsafe { env.get_string_unchecked(&JString::from(class)) }.map(String::from)
                    })
                    .unwrap_or_else(|_| "<Object>".to_string()),
            };
            eprintln!("Rust panic: Encountered an uncaught Java Exception after calling {class}.{}():", method_name.as_ref());
            panic_exception(ex, env)
        } else {
            // Otherwise, just panic with the exception message and exit execution.
            let msg = <String as FromException>::from_exception(&ex, env).unwrap(); // Always returns Ok or panics
            panic!("{msg}");
        }
    }
}

/// Checks if an `Exception` was thrown after calling a Java Method, and returns said `Exception`.
/// The `Exception` will be cleared so that it will be possible to do other jni_calls to handle the exception (e.g. rethrowing it).
///
/// The returned Object will NEVER be NULL.
///
/// NOTICE: Can't call any function (including print) between the time when the exception is thrown and when `JNIEnv::exception_clear()` is called.
/// This means that if a method call could throw, the checks (call, type, and null) should be done AFTER the exception is caught.
fn catch_exception<'local>(env: &mut JNIEnv<'local>) -> Option<JThrowable<'local>> {
    env.exception_occurred().ok().and_then(|ex| {
        if !ex.is_null() {
            env.exception_clear().unwrap();
            Some(ex)
        } else {
            None
        }
    })
}
