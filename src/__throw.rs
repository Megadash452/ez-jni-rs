pub use either::Either;
use jni::{
    objects::{GlobalRef, JClass, JObject, JString, JThrowable, JValue}, JNIEnv
};
use std::{any::Any, cell::{Cell, RefCell}, collections::HashMap};
use crate::FromException;
use crate::{compile_java_class, eprintln};

/// A lot like [std::panic::Location], but uses a [String] instead of `&str`.
#[derive(Debug, Clone)]
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

thread_local! {
    static PANIC_LOCATION: RefCell<Option<PanicLocation>> = const { RefCell::new(None) };
    /// Tells whether [`set_panic_hook()`] was called and the *hook* was set.
    /// 
    /// This will only be `true` if [`catch_throw()`] was called (i.e. Rust was enetered from Java via a [`jni_fn`][ez_jni::jni_fn!]).
    static PANIC_HOOK_SET: Cell<bool> = const { Cell::new(false) };
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
    catch_throw_main(env, |env|
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| f(env)))
    )
}
/// Same as [`catch_throw()`], but maps the returned `R` to a Java value `J`.
pub fn catch_throw_map<'local, R, J>(
    env: &mut JNIEnv<'local>,
    f: impl FnOnce(&mut JNIEnv<'local>) -> R,
    // map function should not capture variables
    map: fn(R, &mut JNIEnv<'local>) -> J,
) -> J {
    catch_throw_main(env, |env|
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| f(env)))
            .and_then(|r| std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| map(r, env))))
    )
}
/// Handles the setting up the *panic hook* and throwing the *panic payload*.
/// 
/// This function exists to avoid repeating code in [`catch_throw()`] and [`catch_throw_map()`].
#[allow(unused_must_use)]
fn catch_throw_main<'local, R>(env: &mut JNIEnv<'local>, catch: impl FnOnce(&mut JNIEnv<'local>) -> std::thread::Result<R>) -> R {
    // Set panic hook to grab [`PANIC_LOCATION`] data.
    std::panic::set_hook(Box::new(|info| {
        PANIC_LOCATION.set(Some(info.location().unwrap().into()));
    }));
    PANIC_HOOK_SET.set(true);

    let result = match catch(env) {
        Ok(r) => r,
        Err(payload) => {
            throw_panic(env, payload)
                .unwrap_or_else(|err| {
                    std::panic::take_hook();
                    eprintln!("Aborting: {err}");
                    std::process::abort();
                });
            unsafe { std::mem::zeroed() }
        }
    };

    // Reset panic hook so that rust behaves normally after this.
    std::panic::take_hook();
    PANIC_HOOK_SET.set(false);

    result
}

/// This function MUST NEVER `panic!`.
fn throw_panic(env: &mut JNIEnv, payload: Box<dyn Any + Send>) -> Result<(), Box<dyn std::error::Error>> {
    let panic_msg = match payload.downcast::<&'static str>() {
        Ok(msg) => msg.as_ref().to_string(),
        Err(payload) => match payload.downcast::<String>() {
            Ok(msg) => *msg,
            Err(payload) => match payload.downcast::<GlobalRef>() {
                // Panicked with an Exception (should be rethrown)
                Ok(exception) => {
                    env.throw(<&JThrowable>::from(exception.as_obj()))
                        .map_err(|err| format!("Failed to rethrow exception: {err}"))?;
                    return Ok(());
                },
                // Unexpected panic type
                Err(_) => "Rust panicked!; Unable to obtain panic message".to_string(),
            },
        },
    };
    let location = PANIC_LOCATION.try_with(|loc| {
        loc.try_borrow()
            .ok()
            .as_deref()
            .map(|loc| loc.as_ref().map(|loc| loc.clone()))
    }).ok()
        .flatten()
        .flatten()
        .ok_or_else(|| format!("Failed to get panic location"))?;
    // TODO: get backtrace

    let classes: HashMap<&str, JClass> = Result::from_iter(
        compile_java_class!("./src/me/marti/ezjni/RustPanic.java").iter()
            .map(|(class_path, binary)|
                env.define_class(class_path, &JObject::null(), binary)
                    .map(|jclass| (*class_path, jclass))
            )
    )?;

    // Create the RustPanic object to throw
    env.exception_clear()?;

    let panic_class = classes.get("me/marti/ezjni/RustPanic")
        .ok_or_else(|| format!("No class \"RustPanic\" in the Map of compiled Java files."))?;

    let file = env.new_string(location.file)?;
    let msg = env.new_string(panic_msg)?;
    // Call constructor RustPanic(java.lang.String, int, int, java.lang.String)
    let exception = env.new_object(panic_class, "(Ljava/lang/String;IILjava/lang/String;)V", &[
        JValue::Object(&file),
        JValue::Int(unsafe { std::mem::transmute(location.line) }),
        JValue::Int(unsafe { std::mem::transmute(location.line) }),
        JValue::Object(&msg)
    ])?;
    // Finally, throw the new Exception
    env.throw(JThrowable::from(exception))?;
    
    Ok(())
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
        if PANIC_HOOK_SET.get() {
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
