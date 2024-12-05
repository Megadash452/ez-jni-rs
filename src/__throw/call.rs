//! These functions are used in [`call!`] and *similar macros*.
use std::panic::AssertUnwindSafe;
use jni::objects::{JClass, JObject};

use super::*;
use crate::FromException;

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

/// The Class/Object that a Java method was called on (i.e. the callee).
pub enum CallTarget<'a, 'local> {
    /// A *static method* was called on a ClassPath string.
    Class(&'a str),
    /// A *static method* was called on a [`Class Object`][JClass];
    ClassObject(&'a JClass<'local>),
    /// A method was called on a normal [`Object`][JObject];
    Object(&'a JObject<'local>)
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
    target: CallTarget,
    method_name: impl AsRef<str>,
) {
    if let Some(ex) = catch_exception(env) {
        // If the panic hook was set, this means that the panic payload will be thrown to Java, so the payload should be the exception itself.
        // DO NOT inject bactrace here. That is done in throw_panic().
        if PANIC_HOOK_SET.get() {
            // Disable hook temporarily to allow normal panic catching
            let hook = std::panic::take_hook();

            // This function MUST NOT panic!, so wrap this block with a panic catch to do the JNI calls.
            let class = std::panic::catch_unwind(AssertUnwindSafe(|| match target {
                CallTarget::Class(class) => class.to_string(),
                CallTarget::ClassObject(class) => call!(class.getName() -> String),
                CallTarget::Object(object) => {
                    let class = env.get_object_class(object).unwrap();
                    call!(class.getName() -> String)
                }
            }))
                .unwrap_or("<Object>".to_string());
            
            // Restore the panic hook
            std::panic::set_hook(hook);

            eprintln!("Rust panic: Encountered an uncaught Java Exception after calling {class}.{}():", method_name.as_ref());
            panic_exception(ex, env)
        } else {
            // Otherwise, just panic with the exception message and exit execution.
            let msg = <String as FromException>::from_exception(&ex, env).unwrap(); // Always returns Ok or panics
            // There is no need to inject backtrace if panics are not being caught.
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
