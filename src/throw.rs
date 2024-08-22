use std::{any::Any, sync::RwLock};
use either::Either;
use jni::{objects::{JObject, JString, JThrowable, JValue}, JNIEnv};
use crate::utils::get_string;

/// A lot like [std::panic::Location], but uses a [String] instead of `&str`.
struct PanicLocation {
    file: String, line: u32, col: u32
}
impl <'a> From<&std::panic::Location<'a>> for PanicLocation {
    fn from(value: &std::panic::Location<'a>) -> Self {
        Self { file: value.file().to_string(), line: value.line(), col: value.column() }
    }
}
impl std::fmt::Display for PanicLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.col)
    }
}
static PANIC_LOCATION: RwLock<Option<PanicLocation>> = RwLock::new(None);

/// Runs a Rust function, and returns its value if successful.
/// If the function panics, the panic is caught and a Java Exception will be thrown and [None] will be returned.
pub fn catch_throw<R>(env: &mut JNIEnv, f: impl FnOnce() -> R) -> Option<R> {
    __set_panic_hook();
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)) {
        Ok(r) => Some(r),
        Err(payload) => {
            __throw_panic(env, payload);
            None
        }
    }
}
// Macro is necessary to prevent mutable borrow of env (in throw) while also borriwing it in the closure.
/// Runs a Rust function, and returns its value if successful.
/// If the function panics, the panic is caught and a Java Exception will be thrown and [None] will be returned.
#[macro_export]
macro_rules! catch_throw {
    ($env:expr, $f:expr) => { {
        crate::throw::__set_panic_hook();
        match std::panic::catch_unwind(::std::panic::AssertUnwindSafe($f)) {
            Ok(r) => Some(r),
            Err(payload) => {
                crate::throw::__throw_panic($env, payload);
                None
            }
        }
    } };
}

/// Set a Panic hook that will be called before the panic is caught, so that it can collect panic data (i.e. location)
#[doc(hidden)]
pub fn __set_panic_hook() {
    std::panic::set_hook(Box::new(|info| {
        if let Ok(mut panic_location) = PANIC_LOCATION.write() {
            if let Some(location) = info.location() {
                *panic_location = Some(PanicLocation::from(location))
            }
        }
    }));
}

#[doc(hidden)]
pub fn __throw_panic(env: &mut JNIEnv, payload: Box<dyn Any + Send>) {
    let panic_msg = match payload.downcast::<&'static str>() {
        Ok(msg) => Some(msg.as_ref().to_string()),
        Err(payload) => match payload.downcast::<String>() {
            Ok(msg) => Some(*msg),
            Err(payload) => match payload.downcast::<()>() {
                // Did not panic with a message, but with an "uncaught" exception that has already been rethrown
                Ok(_) => return,
                // Unexpected panic type
                Err(_) => None
            }
        }
    };
    let msg = match (panic_msg, &*PANIC_LOCATION.read().unwrap()) {
        (Some(msg), Some(info)) => format!("panicked at {info}: {msg}"),
        (Some(msg), None) => format!("panicked at unknown location: {msg}"),
        (None, Some(info)) => format!("Rust panicked at {info}, but could not obtain message"),
        (None, None) => "Rust had a panic! but could not obtain any panic data".to_string()
    };
    let exception_obj = env.new_string(msg)
        .map(JObject::from)
        .and_then(|msg| env.new_object("me/marti/calprovexample/jni/RustPanic", format!("(Ljava/lang/String;)V"), &[
            JValue::Object(&msg)
        ]));
    match exception_obj {
        Ok(obj) => env.throw(JThrowable::from(obj)).unwrap(),
        Err(err) => env.throw(format!("Failed to construct RustPanic object: {err}")).unwrap()
    }
}

/// Chekcs if an exception has been thrown from a previous JNI function call,
/// and converts that exception to a `Err(String)` so that it can be [`mapped`][Result::map].
/// 
/// This function is used by [jni_macros::call!].
// TODO: let the erorr be any type that implements `FromJThrowable`
#[doc(hidden)]
pub fn __catch_exception(env: &mut JNIEnv) -> Result<(), String> {
    match catch_exception(env) {
        Some(ex) => {
            let msg = env.call_method(ex, "getMessage", "()Ljava/lang/String;", &[])
                .inspect_err(|err| panic!("Failed to call getMessage() on Throwable: {err}")).unwrap()
                .l().inspect_err(|err| panic!("Value returned by getMessage() is not a String: {err}")).unwrap();
            if msg.is_null() {
                panic!("Throwable message is NULL");
            }
            return Err(get_string(env, JString::from(msg)))
        },
        None => Ok(())
    }
}
/// Checks if there is an **exception** that was thrown by a Java method called from Rust,
/// and that was not caught by the user (did not use [`Result`] for the return type),
/// and **panics** with that exception.
/// 
/// **target** is a *class* or an *object* (whose class will be determined).
/// **method_name** is the name of the Java Method that threw the *exception*
/// 
/// This function is used by [jni_macros::call!].
#[doc(hidden)]
pub fn __panic_uncaught_exception<'local>(env: &mut JNIEnv<'local>, target: Either<&'static str, &JObject>, method_name: &'static str) {
    let class = match target {
        Either::Left(s) => s.to_string(),
        Either::Right(obj) => env.get_object_class(obj)
            .and_then(|class| env.call_method(class, "getName", "()Ljava/lang/String;", &[]))
            .and_then(|class| class.l())
            .and_then(|class| unsafe { env.get_string_unchecked(&JString::from(class)) }.map(String::from))
            .unwrap_or_else(|_| "<Object>".to_string()),
    };
    if let Some(ex) = catch_exception(env) {
        crate::utils::__eprintln(env, format!("Rust panic: Encountered an uncaught Java Exception after calling {class}.{method_name}():"));
        env.throw(ex).unwrap();
        ::std::panic::panic_any(());
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
    env.exception_occurred()
        .ok()
        .and_then(|ex| if !ex.is_null() {
            env.exception_clear().unwrap();
            Some(ex)
        } else {
            None
        })
}
