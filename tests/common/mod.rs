#![allow(unused)]

pub mod compile_fail;

use std::panic::{UnwindSafe, catch_unwind};
use jni::{JNIEnv, objects::JThrowable};
use ez_jni::{__throw::PanicType, FromObject, JavaException, utils::ResultExt as _};
use utils::TEST_JVM;

/// Gets a [`JNIEnv`] from the global [`JVM`][JavaVM].
/// 
/// This is NOT the same [`get_env`][ez_jni::utils::get_env()] from the library.
/// It operates completely differently and is only for running tests.
pub fn get_env<'local>() -> JNIEnv<'local> {
    TEST_JVM.attach_current_thread_permanently()
        .unwrap_or_else(|err| panic!("Error attaching current thread to JavaVM: {err}"))
}

/// Set up the [`JNIEnv`] from the test [`JVM`][JavaVM] and run some test code `f`.
/// 
/// This function can `panic!`.
/// Panics are not caught in `test` builds because the panic will not unwind thrugh a *language boundary*,
/// and it allows the test process to display error outputs correctly.
pub fn run_with_jnienv(f: impl FnOnce() + UnwindSafe) {
    let (result, mut env) = unsafe { ez_jni::__throw::run_with_jnienv_helper(get_env(), true, |_| f()) };
    // Must use local variable env because the env passed to run_with_jnienv_helper() was popped, possibly leaving no JNIEnvs in the stack.
    let env = &mut env;

    if let Err(panic) = result {
        let mut backtrace = panic.rust_backtrace;
        
        let msg = match &panic.panic {
            PanicType::Message(msg) => msg.to_string(),
            PanicType::Unknown => PanicType::UNKNOWN_PAYLOAD_TYPE_MSG.to_string(),
            PanicType::Object(object) => {
                JavaException::from_object(object.as_obj())
                    .unwrap_display()
                    .to_string()
            },
        };

        let backtrace = match backtrace {
            Some(backtrace) => &backtrace.to_string(),
            None => ""
        };

        let (thread_name, thread_id) = {
            let thread = std::thread::current();
            (
                match thread.name() {
                    Some(name) => &format!(" '{name}'"),
                    None => ""
                },
                thread.id()
            )
        };

        // Try to simulate the default panic hook by printing panic! information.
        // This makes the test runner act normally, unlike how it would with the panic configuration of run_with_jnienv_helper().
        println!("thread{thread_name} {thread_id:?} panicked at {loc}:\n{msg}\n{backtrace}", loc=panic.location);
        std::panic::resume_unwind(Box::new(msg));
    }
}

/// Assert that a test (**f**) should **fail** (`panic!`) with a specific **error message**.
pub fn fail_with(f: impl FnOnce() + UnwindSafe, expected_error: &str) {
    let msg = catch_unwind(f)
        .map_err(|payload| match PanicType::from(payload) {
            PanicType::Message(msg) => msg.into_owned(),
            PanicType::Object(object) => {
                JavaException::from_object(object.as_obj())
                    .unwrap_display()
                    .to_string()
            },
            PanicType::Unknown => panic!("{}", PanicType::UNKNOWN_PAYLOAD_TYPE_MSG),
        })
        .expect_err("Expected function to fail, but it succeeded");

    if msg != expected_error {
        panic!("Expected function to fail with error:\n    {expected_error}\nBut the actual error was:\n    {msg}")
    }
}
