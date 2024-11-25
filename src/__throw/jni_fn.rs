use std::any::Any;

use super::*;
use jni::objects::{GlobalRef, JObject, JValue};
use crate::compile_java_class;

/// Runs a Rust function and returns its value, catching any `panics!` and throwing them as Java Exceptions.
/// Specifically, this will throw a `me.marti.ezjni.RustPanic` exception.
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
        // Get full Backtrace`] as a String and store it to process it in `throw_panic()`.
        PANIC_BACKTRACE.set(Some(Backtrace::force_capture()));
    }));
    PANIC_HOOK_SET.set(true);

    let result = catch(env);

    // Reset panic hook so that rust behaves normally after this,
    // though this is only considered for tests.
    std::panic::take_hook();
    PANIC_HOOK_SET.set(false);

    match result {
        Ok(r) => r,
        Err(payload) => {
            // Unwind panicking accross language barriers is undefined behavior, so ensure it doesnt happen.
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                throw_panic(env, payload)
            }))
                .unwrap_or_else(|payload| {
                    let panic_msg = match payload.downcast::<&'static str>() {
                        Ok(msg) => msg.as_ref().to_string(),
                        Err(payload) => match payload.downcast::<String>() {
                            Ok(msg) => *msg,
                            // Unexpected panic type
                            Err(_) => "Rust panicked!; Unable to obtain panic message".to_string(),
                        },
                    };
                    eprintln!("Aborting: {panic_msg}");
                    std::process::abort();
                });
            unsafe { std::mem::zeroed() }
        }
    }
}

/// This function is allowed to `panic!``
fn throw_panic(env: &mut JNIEnv, payload: Box<dyn Any + Send>) {
    let panic_msg = match payload.downcast::<&'static str>() {
        Ok(msg) => msg.as_ref().to_string(),
        Err(payload) => match payload.downcast::<String>() {
            Ok(msg) => *msg,
            Err(payload) => match payload.downcast::<GlobalRef>() {
                // Panicked with an Exception (should be rethrown)
                Ok(exception) => {
                    let exception = <&JThrowable>::from(exception.as_obj());
                    // Inject Backtrace to Exception
                    prepare_backtrace().map(|backtrace| {
                        inject_backtrace(exception, &backtrace, env);
                    }).unwrap();
                    // Finally, rethrow the new Exception
                    env.throw(exception)
                        .map_err(|err| format!("Failed to rethrow exception: {err}"))
                        .unwrap();
                    return;
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
        .expect("Failed to get panic location");
    // Get backtrace

    // Create the RustPanic object to throw
    env.exception_clear().unwrap();

    let panic_class = env.define_class("me/marti/ezjni/RustPanic", &JObject::null(), compile_java_class!("./src/", "me/marti/ezjni/RustPanic")).unwrap();

    let file = env.new_string(location.file).unwrap();
    let msg = env.new_string(panic_msg).unwrap();
    // Call constructor RustPanic(java.lang.String, int, int, java.lang.String)
    let exception = env.new_object(panic_class, "(Ljava/lang/String;IILjava/lang/String;)V", &[
        JValue::Object(&file),
        JValue::Int(unsafe { std::mem::transmute(location.line) }),
        JValue::Int(unsafe { std::mem::transmute(location.line) }),
        JValue::Object(&msg)
    ]).unwrap();
    // Inject Backtrace to Exception
    prepare_backtrace().map(|backtrace| {
        inject_backtrace(<&JThrowable>::from(&exception), &backtrace, env);
    }).unwrap();

    // Finally, throw the new Exception
    env.throw(JThrowable::from(exception)).unwrap();
}
