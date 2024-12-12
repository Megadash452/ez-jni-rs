//! These functions are used in [`call!`] and *similar macros*.
use jni::errors::Error as JNIError;
use super::*;
use crate::FromException;

/// Error message for when a *JNI call* returns [`Exception`][JNIError::JavaException],
/// but no Exception was found.
static JNI_CALL_GHOST_EXCEPTION: &str = "JNI Call returned with Error::JavaException, but no exception was found.";

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

/// Panics with a *Java Exception* instead of *Rust String message*.
/// 
/// 
pub fn panic_exception(ex: JThrowable, env: &mut JNIEnv) -> ! {
    // If the panic hook was set, this means that the panic payload will be thrown to Java,
    // so the payload should be the exception itself.
    if PANIC_HOOK_SET.get() {
        // DO NOT inject bactrace here. That is done in throw_panic().
        let ex = env.new_global_ref(ex)
            .unwrap_or_else(|err| panic!("Unable to create Global Reference for Exception Object: {err}"));
        ::std::panic::panic_any(ex)
    } else {
        // Otherwise, just panic with the exception message and exit execution.
        let msg = <String as FromException>::from_exception(&ex, env).unwrap(); // Always returns Ok or panics
        // There is no need to inject backtrace if panics are not being caught.
        panic!("{msg}")
    }
}

/// Handles the error returned by any [*JNI Call*](https://docs.rs/jni/0.21.1/jni/struct.JNIEnv.html#implementations) by `panic!king` with the error.
/// 
/// The error most likely is an `Exception`.
/// If that's the case, and the current stack frame is in a [`jni_fn!`][crate::jni_fn],
/// this will `panic!` with the Exception and it will be *rethrown* to Java.
/// Otherwise this will just panic with the *Exception's message*.
pub fn handle_jni_call_error(error: JNIError, env: &mut JNIEnv) -> ! {
    match error {
        JNIError::JavaException => panic_exception(
            catch_exception(env)
                .expect(JNI_CALL_GHOST_EXCEPTION),
        env),
        error => panic!("{error}")
    }
}

/// Handles the error returned by any [*JNI Call*](https://docs.rs/jni/0.21.1/jni/struct.JNIEnv.html#implementations)
/// by converting the `Exception` to a *Rust Type*.
/// 
/// If the **error** was not [`Exception`][JNIError::JavaException] this will `panic!`.
///
/// This function is used by [ez_jni_macros::call!].
pub fn handle_exception_conversion<'local, E: FromException<'local>>(error: JNIError, env: &mut JNIEnv<'local>) -> E {
    match error {
        JNIError::JavaException => {
            let exception = catch_exception(env)
                .expect(JNI_CALL_GHOST_EXCEPTION);

            E::from_exception(&exception, env)
                .unwrap_or_else(|err| {
                    eprintln!("Attempted to catch an exception, but failed to convert it to a concrete type:\n{err}");
                    panic_exception(exception, env)
                })
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
