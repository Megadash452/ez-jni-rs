//! These functions are used in [`call!`] and *similar macros*.
use jni::{errors::Error as JNIError, objects::GlobalRef};
use super::*;
use crate::{utils::JNI_CALL_GHOST_EXCEPTION, FromObject, JavaException};

/// Checks if there is a *pending Exception* that was thrown from a previous JNI call,
/// and tries to convert it to an `E` and the Exception is caught and cleared.
/// 
/// If the `Exception` can't be converted to an `E`,
/// this function will *re-throw* the Exception so that another [`try_catch`] call can catch the exception.
pub fn try_catch<'local, E>(env: &mut JNIEnv<'local>) -> Option<E>
where E: for<'a> FromObject<'a, 'local, 'local> {
    catch_exception(env)
        .and_then(|ex|
            E::from_object_env(&ex, env)
                .inspect_err(|_| env.throw(ex).unwrap())
                .ok()
        )
}

/// Panics with a *Java Exception* instead of *Rust String message*.
/// 
/// If this is called within the context of a [`jni_fn`][crate::jni_fn!],
/// This will panic with the **Exception Object** instead of a regular `panic!` message.
/// In this case, the panic payload will be a [`GlobalRef`][jni::objects::GlobalRef].
/// Otherwise, the panic payload will be a [`String`].
pub fn panic_exception(ex: JavaException) -> ! {
    // If the panic hook was set, this means that the panic payload will be thrown to Java,
    // so the payload should be the exception itself.
    if PANIC_HOOK_SET.get() {
        // DO NOT inject bactrace here. That is done in throw_panic().
        ::std::panic::panic_any::<GlobalRef>(ex.into())
    } else {
        // Otherwise, just panic with the exception message and exit execution.
        // There is no need to inject backtrace if panics are not being caught.
        panic!("{ex}");
    }
}

/// Handles the error returned by any [*JNI Call*](https://docs.rs/jni/0.21.1/jni/struct.JNIEnv.html#implementations) by `panic!king` with the error.
/// 
/// The error most likely is an `Exception`.
/// If that's the case, and the current stack frame is in a [`jni_fn!`][crate::jni_fn],
/// this will `panic!` with the Exception and it will be *rethrown* to Java.
/// Otherwise this will just panic with the *Exception's message*.
pub fn handle_jni_call_error(error: JNIError, env: &mut JNIEnv<'_>) -> ! {
    match error {
        JNIError::JavaException => {
            let ex = catch_exception(env).expect(JNI_CALL_GHOST_EXCEPTION);
            let ex = JavaException::from_object_env(&ex, env)
                .unwrap_or_else(|error| panic!("{error}"));
            panic_exception(ex)
        },
        error => panic!("{error}")
    }
}

/// Checks if an `Exception` was thrown after calling a Java Method, and returns said `Exception`.
/// The `Exception` will be cleared so that it will be possible to do other jni_calls to handle the exception (e.g. rethrowing it).
///
/// The returned Object will NEVER be NULL.
///
/// NOTICE: Can't call any function (including print) between the time when the exception is thrown and when `JNIEnv::exception_clear()` is called.
/// This means that if a method call could throw, the checks (call, type, and null) should be done AFTER the exception is caught.
pub fn catch_exception<'local>(env: &mut JNIEnv<'local>) -> Option<JThrowable<'local>> {
    env.exception_occurred().ok().and_then(|ex| {
        if !ex.is_null() {
            env.exception_clear().unwrap();
            Some(ex)
        } else {
            None
        }
    })
}

/// Gets the message associated with the [`JNIError`] either from the [`Exception`] or the 
pub fn get_jni_error_msg(error: JNIError, env: &mut JNIEnv<'_>) -> String {
    match error {
        JNIError::JavaException => {
            let exception = catch_exception(env)
                .expect(JNI_CALL_GHOST_EXCEPTION);

            call!(env=> exception.getMessage() -> Option<String>)
                .unwrap_or_else(|| {
                    let class = call!(env=> call!(env=> exception.getClass() -> Class).getName() -> String);
                    panic!("Could not get message from exception {class}");
                })
        },
        error => panic!("{error}")
    }
}
