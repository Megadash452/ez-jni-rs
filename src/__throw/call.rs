//! These functions are used in [`call!`] and *similar macros*.
use std::sync::OnceLock;
use jni::{errors::Error as JNIError, objects::GlobalRef};
use super::*;
use crate::{utils::{ResultExt, JNI_CALL_GHOST_EXCEPTION}, FromObject, JavaException};

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
#[track_caller]
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
#[track_caller]
pub fn handle_jni_call_error(error: JNIError, env: &mut JNIEnv<'_>) -> ! {
    match error {
        JNIError::JavaException => {
            let ex = catch_exception(env).expect(JNI_CALL_GHOST_EXCEPTION);
            let ex = JavaException::from_object_env(&ex, env).unwrap_display();
            panic_exception(ex)
        },
        error => panic!("{error}")
    }
}

/// Checks if an `Exception` was thrown after calling a Java Method, and returns said `Exception`.
/// The `Exception` will be cleared so that it will be possible to do other jni_calls to handle the exception (e.g. rethrowing it).
/// 
/// If the `Exception` Object is a descendant of `java.lang.Error`,
/// this function will `panic!` instead of returning the object.
/// This is because a `java.lang.Error` [should ***never*** be caught](https://docs.oracle.com/javase/8/docs/api/java/lang/Error.html).
///
/// The returned Object will NEVER be NULL.
pub fn catch_exception<'local>(env: &mut JNIEnv<'local>) -> Option<JThrowable<'local>> {
    /// This function checks if the object is of class `java.lang.Error`.
    /// To make the check faster, **cache** the Class object in memory.
    static ERROR_CLASS: OnceLock<GlobalRef> = OnceLock::new();

    env.exception_occurred().ok().and_then(|ex| {
        // NOTICE: Can't call any function (including print) between the time when the exception is thrown and when `JNIEnv::exception_clear()` is called.
        // This means that if a method call could throw, the checks (call, type, and null) should be done AFTER the exception is caught.
        env.exception_clear().unwrap();

        if ex.is_null() {
            None
        } else {
            // FIXME: This causes problems with call error handling because they expect to catch NoSuchMethodError, but this prevents that.
            //        Perhaps I need to make catch_throwable() ??
            let error_class = ERROR_CLASS.get_or_init(|| {
                let class = env.find_class("java/lang/Error")
                    .unwrap_or_else(|error| handle_jni_call_error(error, env));
                env.new_global_ref(class)
                    .unwrap_or_else(|error| handle_jni_call_error(error, env))
            });
            // java.lang.Error should NOT be caught, as per class documentation.
            // Instead, it should cause a panic!.
            if env.is_instance_of(&ex, error_class)
                .unwrap_or_else(|error| handle_jni_call_error(error, env))
            {
                panic_exception(JavaException::from_object_env(&ex, env).unwrap_display())
            }

            Some(ex)
        }
    })
}

/// Gets the message associated with the [`JNIError`] either from the [`Exception`] or the 
pub fn get_jni_error_msg(error: JNIError, env: &mut JNIEnv<'_>) -> String {
    match error {
        JNIError::JavaException => {
            let exception = catch_exception(env)
                .expect(JNI_CALL_GHOST_EXCEPTION);
            JavaException::from_object_env(&exception, env)
                .unwrap_display()
                .message()
                .to_string()
        },
        error => error.to_string()
    }
}
