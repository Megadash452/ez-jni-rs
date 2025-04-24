use super::*;
use std::{any::Any, panic::UnwindSafe};
use jni::objects::{GlobalRef, JObject};
use crate::{compile_java_class, LOCAL_JNIENV_STACK};

/// Runs a Rust function and returns its value, catching any `panics!` and throwing them as *Java Exceptions*.
/// Specifically, this will throw a `me.marti.ezjni.RustPanic` exception.
/// This funciton also sets up the [`JNIEnv`] to be used for the duration of the current *Java stack frame*.
///
/// If **f** `panics!`, this will return the [`Zeroed`][std::mem::zeroed()] representation of the return type.
/// This means that this function should only return directly to Java.
/// 
/// DO NOT try to operate on the return value because you will have no indication wether **f** panicked and returned *zeroed*.
/// 
/// This function is used by [ez_jni_macros::jni_fn].
pub fn run_with_jnienv<'local, R: Sized>(
    env: JNIEnv<'local>,
    f: impl FnOnce() -> R + UnwindSafe,
) -> R {
    run_with_jnienv_main(env, ||
        std::panic::catch_unwind(|| f())
    )
}
/// Same as [`run_with_jnienv()`], but maps the returned `R` to a Java value `J`.
pub fn run_with_jnienv_map<'local, R: UnwindSafe, J: Sized>(
    env: JNIEnv<'local>,
    f: impl FnOnce() -> R + UnwindSafe,
    // map function should not capture variables
    map: fn(R) -> J,
) -> J {
    run_with_jnienv_main(env, ||
        std::panic::catch_unwind(|| f())
            .and_then(|r| std::panic::catch_unwind(|| map(r)))
    )
}
/// Handles setting up the *panic hook* and throwing the *panic payload*.
/// 
/// This function exists to avoid repeating code in [`catch_throw()`] and [`catch_throw_map()`].
/// 
/// Unwind panicking accross language barriers is undefined behavior, so it MUST NOT happen in this function.
#[allow(unused_must_use)]
fn run_with_jnienv_main<'local, R: Sized>(mut env: JNIEnv<'local>, f: impl FnOnce() -> std::thread::Result<R>) -> R {
    // Helper to catch unwinding panics of regular rust functions
    fn my_catch<'local, R>(f: impl FnOnce() -> R) -> Result<R, String> {
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| f()))
            .map_err(|payload| {
                match payload.downcast::<&'static str>() {
                    Ok(msg) => msg.as_ref().to_string(),
                    Err(payload) => match payload.downcast::<String>() {
                        Ok(msg) => *msg,
                        // Unexpected panic type
                        Err(_) => "Rust panicked!; Unable to obtain panic message".to_string(),
                    },
                }
            })
    }

    // Set panic hook to grab [`PANIC_LOCATION`] data.
    std::panic::set_hook(Box::new(|info| {
        PANIC_LOCATION.set(Some(info.location().unwrap().into()));
        // Get full Backtrace as a String and store it to process it in `throw_panic()`.
        PANIC_BACKTRACE.set(Some(Backtrace::force_capture()));
    }));
    PANIC_HOOK_SET.set(true);

    // Assign the JNIEnv used for this jni call
    let pushed_env = env.get_raw();
    let pushed_idx = my_catch(|| {
        LOCAL_JNIENV_STACK.with_borrow_mut(|stack| {
            let idx = stack.len();
            stack.push_back(unsafe { std::mem::transmute::<JNIEnv<'local>, JNIEnv<'static>>(env) });
            idx
        })
    })
        .unwrap_or_else(|panic_msg| {
            eprintln!("Aborting due to error in ez_jni::__throw::jni_fn::run_with_jnienv_main():\n{panic_msg}\n");
            std::process::abort();
        });

    // Run the function
    let result = f();

    // Remove the JNIEnv when the function finishes running
    let (env_r, popped_idx) = my_catch(|| {
        LOCAL_JNIENV_STACK.with_borrow_mut(|stack| {(
            stack.pop_back()
                .expect("JNIEnv stack is empty, can't pop_back"),
            stack.len()
        )})
    })
        .unwrap_or_else(|panic_msg| {
            eprintln!("Aborting due to error in ez_jni::__throw::jni_fn::run_with_jnienv_main():\n{panic_msg}\n");
            std::process::abort();
        });
    env = unsafe { std::mem::transmute::<JNIEnv<'static>, JNIEnv<'local>>(env_r) };

    // Check sanity
    if popped_idx != pushed_idx {
        eprintln!("The index of the JNIEnv that was pushed ({pushed_idx}) to the stack is different from the index of the one that was popped ({popped_idx}); Aborting.");
        std::process::abort();
    }
    if env.get_raw() != pushed_env {
        eprintln!("The JNIEnv that was pushed to the stack is different from the one that was popped; Aborting.");
        std::process::abort();
    }

    // Reset panic hook so that rust behaves normally after this,
    // though this is only considered for tests.
    std::panic::take_hook();
    PANIC_HOOK_SET.set(false);

    match result {
        Ok(r) => r,
        Err(payload) => {
            my_catch(|| throw_panic(&mut env, payload))
                .unwrap_or_else(|panic_msg| {
                    eprintln!("Aborting due to error in ez_jni::__throw::jni_fn::throw_panic():\n{panic_msg}\n");
                    std::process::abort();
                });
            unsafe { std::mem::zeroed() }
        }
    }
}

/// This function is allowed to `panic!`.
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
                    let _ = prepare_backtrace().map(|backtrace| {
                        inject_backtrace(exception, &backtrace, env);
                    });
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

    env.exception_clear().unwrap();

    // Do not try to build Java Class when building documentation for Docs.rs because it does not allow macros to write to the filesystem.
    ::cfg_if::cfg_if! {
        if #[cfg(not(docsrs))] {
            let panic_class = env.find_class("me/marti/ezjni/RustPanic")
                .or_else(|_| env.define_class("me/marti/ezjni/RustPanic", &JObject::null(), compile_java_class!("./src/", "me/marti/ezjni/RustPanic")))
                .expect("Failed loading/finding RustPanic class");
        } else {
            let panic_class = jni::objects::JClass::from(JObject::null());
        }
    }
        

    let exception = new!(panic_class(String(location.file), u32(location.line), u32(location.col), String(panic_msg)));
    // Inject Backtrace to Exception
    let _ = prepare_backtrace().map(|backtrace| {
        inject_backtrace(<&JThrowable>::from(&exception), &backtrace, env);
    });

    // Finally, throw the new Exception
    env.throw(JThrowable::from(exception)).unwrap();
}
