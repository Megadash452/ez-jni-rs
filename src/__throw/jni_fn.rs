use super::*;
use std::{marker::PhantomData, panic::{AssertUnwindSafe, UnwindSafe}, sync::OnceLock};
use jni::objects::JObject;
use crate::{compile_java_class, utils::get_env, LOCAL_JNIENV_STACK};

/// Runs a Rust function and returns its value, catching any `panics!` and throwing them as *Java Exceptions*.
/// Specifically, this will throw a `me.marti.ezjni.RustPanic` exception.
/// This funciton also sets up the [`JNIEnv`] to be used for the duration of the current *Java stack frame*,
/// which can be obtained with [`get_env()`].
///
/// If **f** `panics!`, this will return the [`Zeroed`][std::mem::zeroed()] representation of the return type.
/// This means that this function should only return directly to Java.
/// 
/// ## Safety:
/// 
/// DO NOT try to operate on the return value because you will have no indication wether **f** panicked and returned *zeroed*.
/// Doing this will cause *undefined behavior*.
/// 
/// Unwind panicking accross language barriers is *undefined behavior*.
/// **f** is allowed to `panic!` because the panic is caught.
/// The caller ensures that **f** is [`UnwindSafe`] (other than passing the [`JNIEnv`])
/// and doesn't cause any `panic!s` that can't be *caught* (i.e. non-unwind).
/// 
/// All that said, this function is NOT meant to be used by users of the library (thus it's hidden).
/// This function is used by [ez_jni_macros::jni_fn].
// This function ***MUST NOT*** `panic!`.
pub unsafe fn run_with_jnienv<'local, R: Sized>(mut env: JNIEnv<'local>, f: impl FnOnce(&mut JNIEnv<'local>) -> R) -> R {
    let result = unsafe { run_with_jnienv_helper(env, f) };
    env = result.1;

    match result.0 {
        Ok(r) => r,
        Err(payload) => {
            my_catch(|| throw_panic(payload, &mut env));
            unsafe { std::mem::zeroed() }
        }
    }
}
/// Same as [`run_with_jnienv()`], but maps the returned `R` to a Java value `J`.
pub unsafe fn run_with_jnienv_map<'local, R: UnwindSafe, J: Sized>(
    env: JNIEnv<'local>,
    f: impl FnOnce(&mut JNIEnv<'local>) -> R + UnwindSafe,
    // map function should not capture variables
    map: fn(R, &mut JNIEnv<'local>) -> J,
) -> J {
    unsafe {
        run_with_jnienv(env, |env| map(f(env), env))
    }
}

/// Runs the code for [`run_with_jnienv()`], but returns a [`Result`] instead of a could-be-zeroed of [`R`].
/// Also returns the [`JNIEnv`] that was passed in.
/// 
/// This is also used in integration tests.
#[doc(hidden)]
pub unsafe fn run_with_jnienv_helper<'local, R: Sized>(mut env: JNIEnv<'local>, f: impl FnOnce(&mut JNIEnv<'local>) -> R) -> (Result<R, PanicPayloadRepr>, JNIEnv<'local>) {
    #![allow(unused_must_use)]
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
    let result = std::panic::catch_unwind(AssertUnwindSafe(|| f(get_env::<'_, 'local>())))
        .map_err(|payload| PanicPayloadRepr::from(payload));

    // Remove the JNIEnv when the function finishes running
    env = stack_env.pop();

    // Reset panic hook so that rust behaves normally after this,
    // though this is only considered for tests.
    std::panic::take_hook();
    PANIC_HOOK_SET.set(false);

    (result, env)
}

/// Helper to catch unwinding panics of regular rust functions.
/// Aborts if a panic occurs.
fn my_catch<'local, R>(f: impl FnOnce() -> R) -> R {
    std::panic::catch_unwind(AssertUnwindSafe(|| f()))
        .map_err(|payload| match PanicPayloadRepr::from(payload) {
            PanicPayloadRepr::Message(msg) => msg,
            PanicPayloadRepr::Object(_) => panic!("UNREACHABLE"),
            PanicPayloadRepr::Unknown => Cow::Borrowed(PanicPayloadRepr::UNKNOWN_PAYLOAD_TYPE_MSG)
        })
        .unwrap_or_else(|panic_msg| {
            eprintln!("Aborting due to error in ez_jni::__throw::jni_fn::run_with_jnienv_main():\n{panic_msg}\n");
            std::process::abort();
        })
}

/// Represents a [`JNIEnv`] that currently lives in the [`LOCAL_JNIENV_STACK`].
/// That is to say that during the *lifetime* of this object,
/// the [`JNIEnv`] that this object points to is owned by the [`LOCAL_JNIENV_STACK`].
struct StackEnv<'local> {
    env: *mut jni::sys::JNIEnv,
    /// The actual index of the [`JNIEnv`] in the stack.
    idx: usize,
    lt: PhantomData<&'local ()>
}
impl<'local> StackEnv<'local> {
    pub fn push(env: JNIEnv<'_>) -> Self {
        Self {
            env: env.get_raw(),
            idx: my_catch(|| {
                LOCAL_JNIENV_STACK.with_borrow_mut(|stack| {
                    let idx = stack.len();
                    stack.push_back(unsafe { std::mem::transmute::<JNIEnv<'_>, JNIEnv<'static>>(env) });
                    idx
                })
            }),
            lt: PhantomData,
        }
    }
    pub fn pop(self) -> JNIEnv<'local> {
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
/// However, it should not call any other ez_jni functions.
fn throw_panic(payload: PanicPayloadRepr, env: &mut JNIEnv<'_>) {
    /// In-memory cache of custom `Exception` class `me.marti.ez_jni.RustPanic`
    static PANIC_CLASS: OnceLock<GlobalRef> = OnceLock::new();

    let panic_msg = match PanicPayloadRepr::from(payload) {
        PanicPayloadRepr::Message(msg) => msg,
        PanicPayloadRepr::Unknown => Cow::Borrowed(PanicPayloadRepr::UNKNOWN_PAYLOAD_TYPE_MSG),
        // Panicked with an Exception (should be rethrown)
        PanicPayloadRepr::Object(exception) => {
            let exception = <&JThrowable>::from(exception.as_obj());
            // Inject Backtrace to Exception
            let _ = prepare_backtrace().map(|backtrace| {
                inject_backtrace(exception, &backtrace, env);
            });
            // Finally, rethrow the new Exception
            env.throw(exception)
                .unwrap_or_else(|err| panic!("Failed to rethrow exception: {err}"));
            return;
        }
    };
    let location = PANIC_LOCATION.try_with(|loc| {
        loc.try_borrow()
            .map_err(|err| format!("Error borrowing PANIC_LOCATION: {err}"))?
            .as_ref()
            .map(|loc| loc.clone())
            .ok_or("value is None".to_string())
    })
        .map_err(|err| err.to_string())
        .flatten()
        .unwrap_or_else(|err| panic!("Failed to get panic location: {err}"));

    env.exception_clear().unwrap();

    // Do not try to build Java Class when building documentation for Docs.rs because it does not allow macros to write to the filesystem.
    ::cfg_if::cfg_if! {
        if #[cfg(not(docsrs))] {
            let panic_class = PANIC_CLASS.get_or_init(|| {
                let class = env.find_class("me/marti/ezjni/RustPanic")
                    .or_else(|_| env.define_class("me/marti/ezjni/RustPanic", &JObject::null(), compile_java_class!("./src/", "me/marti/ezjni/RustPanic")))
                    .expect("Failed loading/finding RustPanic class");
                env.new_global_ref(class)
                    .expect("Failed to make JClass into GlobalRef")
            });
        } else {
            let panic_class = jni::objects::JClass::from(JObject::null());
        }
    }

    let panic_class = jni::objects::JClass::from(env.new_local_ref(panic_class).expect("Could not allocate new local reference for RustPanic class"));
    // TODO: allow macros to take GlobalRef AsRef of JObject and JClass
    let exception = new!(env=> panic_class(String(location.file), u32(location.line), u32(location.col), String(panic_msg)));
    // Inject Backtrace to Exception
    let _ = prepare_backtrace().map(|backtrace| {
        inject_backtrace(<&JThrowable>::from(&exception), &backtrace, env);
    });

    // Finally, throw the new Exception
    env.throw(JThrowable::from(exception)).unwrap();
}
