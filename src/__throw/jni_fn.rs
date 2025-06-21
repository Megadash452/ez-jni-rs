use super::*;
use std::{any::Any, panic::{AssertUnwindSafe, UnwindSafe}};
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
pub fn run_with_jnienv<'local, R: UnwindSafe>(
    env: JNIEnv<'local>,
    f: impl FnOnce(&mut JNIEnv<'local>) -> R + UnwindSafe,
) -> R {
    run_with_jnienv_main(env, |env|
        std::panic::catch_unwind(AssertUnwindSafe(|| f(env)))
    )
}
/// Same as [`run_with_jnienv()`], but maps the returned `R` to a Java value `J`.
pub fn run_with_jnienv_map<'local, R: UnwindSafe, J: Sized>(
    env: JNIEnv<'local>,
    f: impl FnOnce(&mut JNIEnv<'local>) -> R + UnwindSafe,
    // map function should not capture variables
    map: fn(R, &mut JNIEnv<'local>) -> J,
) -> J {
    run_with_jnienv_main(env, |env|
        std::panic::catch_unwind(AssertUnwindSafe(|| f(env)))
            .and_then(|r| std::panic::catch_unwind(AssertUnwindSafe(|| map(r, env))))
    )
}
/// Handles setting up the *panic hook* and throwing the *panic payload*.
/// 
/// This function exists to avoid repeating code in [`catch_throw()`] and [`catch_throw_map()`].
/// 
/// Unwind panicking accross language barriers is undefined behavior, so it MUST NOT happen in this function.
#[allow(unused_must_use)]
fn run_with_jnienv_main<'local, R: Sized>(mut env: JNIEnv<'local>, f: impl FnOnce(&mut JNIEnv<'local>) -> std::thread::Result<R>) -> R {
    // TODO: Investigate bug that doesn't set PANIC_LOCATION sometimes
    // Set panic hook to grab [`PANIC_LOCATION`] data.
    std::panic::set_hook(Box::new(|info| {
        PANIC_LOCATION.set(Some(info.location().unwrap().into()));
        // Get full Backtrace as a String and store it to process it in `throw_panic()`.
        PANIC_BACKTRACE.set(Some(Backtrace::force_capture()));
    }));
    PANIC_HOOK_SET.set(true);

    // Assign the JNIEnv used for this jni call
    let stack_env = StackEnv::push(env);

    // Run the function
    // Pass a reference of the JNIEnv that was just pushed; for conversions
    let result = f(crate::utils::get_env::<'_, 'local>());

    // Remove the JNIEnv when the function finishes running
    env = stack_env.pop();

    // Reset panic hook so that rust behaves normally after this,
    // though this is only considered for tests.
    std::panic::take_hook();
    PANIC_HOOK_SET.set(false);

    match result {
        Ok(r) => r,
        Err(payload) => {
            my_catch(|| throw_panic(&mut env, payload));
            unsafe { std::mem::zeroed() }
        }
    }
}

/// Helper to catch unwinding panics of regular rust functions.
/// Aborts if a panic occurs.
fn my_catch<'local, R>(f: impl FnOnce() -> R) -> R {
    std::panic::catch_unwind(AssertUnwindSafe(|| f()))
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
        .unwrap_or_else(|panic_msg| {
            eprintln!("Aborting due to error in ez_jni::__throw::jni_fn::run_with_jnienv_main():\n{panic_msg}\n");
            std::process::abort();
        })
}

/// Represents a [`JNIEnv`] that currently lives in the [`LOCAL_JNIENV_STACK`].
/// That is to say that during the *lifetime* of this object,
/// the [`JNIEnv`] that this object points to is owned by the [`LOCAL_JNIENV_STACK`].
#[doc(hidden)]
pub struct StackEnv {
    env: *mut jni::sys::JNIEnv,
    /// The actual index of the [`JNIEnv`] in the stack.
    idx: usize,
}
impl StackEnv {
    pub fn push(env: JNIEnv<'_>) -> Self {
        Self {
            env: env.get_raw(),
            idx: my_catch(|| {
                LOCAL_JNIENV_STACK.with_borrow_mut(|stack| {
                    let idx = stack.len();
                    stack.push_back(unsafe { std::mem::transmute::<JNIEnv<'_>, JNIEnv<'static>>(env) });
                    idx
                })
            })
        }
    }
    pub fn pop<'local>(self) -> JNIEnv<'local> {
        let (popped_env, popped_idx) = my_catch(|| {
            LOCAL_JNIENV_STACK.with_borrow_mut(|stack| {(
                stack.pop_back()
                    .expect("JNIEnv stack is empty, can't pop_back"),
                stack.len()
            )})
        });

        // Check sanity
        if popped_idx != self.idx {
            eprintln!("The index of the JNIEnv that was pushed ({}) to the stack is different from the index of the one that was popped ({popped_idx}); Aborting.", self.idx);
            std::process::abort();
        }
        if popped_env.get_raw() != self.env {
            eprintln!("The JNIEnv that was pushed to the stack is different from the one that was popped; Aborting.");
            std::process::abort();
        }

        unsafe { std::mem::transmute::<JNIEnv<'static>, JNIEnv<'local>>(popped_env) }
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
    let location = match PANIC_LOCATION.try_with(|loc| {
        loc.try_borrow()
            .map_err(|err| format!("Error borrowing PANIC_LOCATION: {err}"))?
            .as_ref()
            .map(|loc| loc.clone())
            .ok_or("value is None".to_string())
    })
    .map_err(|err| err.to_string()) {
        // Result::flatten() is not stable :(
        Ok(Ok(loc)) => loc,
        Ok(Err(err)) | Err(err) => panic!("Failed to get panic location: {err}")
    };

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

    let exception = new!(env=> panic_class(String(location.file), u32(location.line), u32(location.col), String(panic_msg)));
    // Inject Backtrace to Exception
    let _ = prepare_backtrace().map(|backtrace| {
        inject_backtrace(<&JThrowable>::from(&exception), &backtrace, env);
    });

    // Finally, throw the new Exception
    env.throw(JThrowable::from(exception)).unwrap();
}
