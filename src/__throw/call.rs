//! These functions are used in [`call!`][ez_jni_macros::call!] and *similar macros*.
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
where E: FromObject<'local> {
    catch_throwable(env)
        .and_then(|ex| {
            // Check if Object is `java.lang.Error` and `panic!`.
            if is_error(&ex, env) {
                panic_throwable(&ex, env)
            }
            
            E::from_object_env(&ex, env)
                .inspect_err(|_| env.throw(ex).unwrap())
                .ok()
        })
}
/// Same as [`try_catch()`], but also catches `java.lang.Error`.
pub(crate) fn try_catch_throwable<'local, E>(env: &mut JNIEnv<'local>) -> Option<E>
where E: FromObject<'local> {
    catch_throwable(env)
        .and_then(|ex|
            E::from_object_env(&ex, env)
                .inspect_err(|_| env.throw(ex).unwrap())
                .ok()
        )
}


/// Panics with a *Java Exception* instead of *Rust String message*.
/// 
/// If this is called within the *context* of a [`jni_fn`][crate::jni_fn!],
/// this will `panic!` with the [**Exception Object**][jni::objects::GlobalRef] as the payload instead of a regular `panic!` message.
/// Otherwise, the panic payload will be a [`String`].
#[track_caller]
pub fn panic_exception(ex: JavaException) -> ! {
    // If the panic hook was set, this means that the panic payload will be thrown to Java,
    // so the payload should be the exception itself.
    if INTEGRATION_TEST.get() {
        // DO NOT inject bactrace here. That is done in throw_panic().
        ::std::panic::panic_any::<GlobalRef>(ex.into())
    } else {
        // Otherwise, just panic with the exception message and exit execution.
        // There is no need to inject backtrace if panics are not being caught.
        panic!("{ex}");
    }
}

/// Like [`panic_exception()`], but can take any [`Throwable`][JThrowable] Object.
#[inline(always)]
pub fn panic_throwable(object: &JThrowable<'_>, env: &mut JNIEnv<'_>) -> ! {
    panic_exception(JavaException::from_throwable(object, env))
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
            let object = catch_throwable(env).expect(JNI_CALL_GHOST_EXCEPTION);
            panic_throwable(&object, env)
        },
        error => panic!("{error}")
    }
}

/// Checks if an `Exception` was *thrown* after calling a Java Method, and returns said `Exception`.
/// The `Exception` will be cleared so that it will be possible to do other jni_calls to handle the exception (e.g. rethrowing it).
/// 
/// Note: This function will only return `java.lang.Exception` and `panic!` if the Object is `java.lang.Error`.
/// This is according to the [Java documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/Error.html), that `java.lang.Error` should *never* be caught.
pub fn catch_exception(env: &mut JNIEnv<'_>) -> Option<JavaException> {
    let ex = catch_throwable(env)?;
    let ex = JavaException::from_object_env(&ex, env).unwrap_display();
    
    // Check if Object is `java.lang.Error` and `panic!`.
    if is_error(&ex, env) {
        panic_exception(ex)
    } else {
        Some(ex)
    }
}

/// Same as [`catch_exception()`], but can catch any `Throwable`, including `java.lang.Error`.
/// 
/// The returned Object will NEVER be `NULL`.
pub(crate) fn catch_throwable<'local>(env: &mut JNIEnv<'local>) -> Option<JThrowable<'local>> {
     env.exception_occurred().ok().and_then(|ex| {
        // NOTICE: Can't call any function (including print) between the time when the exception is thrown and when `JNIEnv::exception_clear()` is called.
        // This means that if a method call could throw, the checks (call, type, and null) should be done AFTER the exception is caught.
        env.exception_clear().unwrap();

        if ex.is_null() {
            None
        } else {
            Some(ex)
        }
    })
}

/// Checks if the **Object** is of *Class* `java.lang.Error`,
/// which should cause a `panic!`.
pub(crate) fn is_error<'local>(object: &JThrowable<'_>, env: &mut JNIEnv<'local>) -> bool {
    /// To make the check faster, **cache** the Class object in memory.
    static ERROR_CLASS: OnceLock<GlobalRef> = OnceLock::new();

    let error_class = ERROR_CLASS.get_or_init(|| {
        let class = env.find_class("java/lang/Error")
            .unwrap_or_else(|error| handle_jni_call_error(error, env));
        env.new_global_ref(class)
            .unwrap_or_else(|error| handle_jni_call_error(error, env))
    });

    env.is_instance_of(object, error_class)
        .unwrap_or_else(|error| handle_jni_call_error(error, env))
}

/// Gets the message associated with the [`JNIError`] either from the error enum itself,
/// or from the [`JavaException`] object.
pub fn get_jni_error_msg(error: JNIError, env: &mut JNIEnv<'_>) -> String {
    match error {
        JNIError::JavaException => {
            let object = catch_throwable(env)
                .expect(JNI_CALL_GHOST_EXCEPTION);
            JavaException::from_throwable(&object, env)
                .message()
                .to_string()
        },
        error => error.to_string()
    }
}
