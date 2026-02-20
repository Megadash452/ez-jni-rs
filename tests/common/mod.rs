#![allow(unused)]
pub mod compile_fail;

use std::{panic::{catch_unwind, AssertUnwindSafe, UnwindSafe}, process::Command, sync::LazyLock};
use jni::{objects::JThrowable, JNIEnv, JavaVM};
use ez_jni::{__throw::{PanicType, JniRunPanic}, call, utils::ResultExt, FromObject, JavaException};
use utils::CLASS_DIR;

static JVM: LazyLock<JavaVM> = LazyLock::new(|| {
    compile_java()
        .unwrap_or_else(|err| panic!("Error compiling Java file:\n{err}"));
    JavaVM::new(jni::InitArgsBuilder::new()
        .option(format!("-Djava.class.path={CLASS_DIR}"))
        .option("-ea")
        .build()
        .unwrap()
    )
        .unwrap_or_else(|err| panic!("Error starting JavaVM: {err}"))
});
fn compile_java() -> Result<(), Box<dyn std::error::Error>> {
    std::fs::create_dir_all(CLASS_DIR)?;
    let output = Command::new("javac")
        .args(["./tests/Test.java", "-d", CLASS_DIR])
        .output()?;
    if !output.status.success() {
        return Err(String::from_utf8_lossy(&output.stderr).into())
    }
    
    Ok(())
}

/// Gets a [`JNIEnv`] from the global [`JVM`][JavaVM].
/// 
/// This is NOT the same [`get_env`][ez_jni::utils::get_env()] from the library.
/// It operates completely differently and is only for running tests.
pub fn get_env<'local>() -> JNIEnv<'local> {
    JVM.attach_current_thread_permanently()
        .unwrap_or_else(|err| panic!("Error attaching current thread to JavaVM: {err}"))
}

/// Set up the [`JNIEnv`] from the test [`JVM`][JavaVM] and run some test code `f`.
/// 
/// This function can `panic!`.
/// Panics are not caught in `test` builds because the panic will not unwind thrugh a *language boundary*,
/// and it allows the test process to display error outputs correctly.
pub fn run_with_jnienv(f: impl FnOnce() + UnwindSafe) {
    let (result, mut env) = unsafe { ez_jni::__throw::run_with_jnienv_helper(get_env(), true, |_| f()) };
    let env = &mut env;

    /// This is allowed to panic
    fn create_panic_stmnt(panic: JniRunPanic, env: &mut JNIEnv<'_>) -> String {
        let mut backtrace = panic.rust_backtrace;
        
        let thread_name = match std::thread::current().name() {
            Some(name) => &format!(" '{name}'"),
            None => ""
        };
        let msg = match &panic.panic {
            PanicType::Message(msg) => &msg,
            PanicType::Unknown => PanicType::UNKNOWN_PAYLOAD_TYPE_MSG,
            PanicType::Object(exception) => {
                let exception = JavaException::from_throwable(<&JThrowable>::from(exception.as_obj()), env);
                // Inject Java StackTrace to Rust Backtrace
                if let Some(backtrace) = &mut backtrace {
                    backtrace.append_stacktrace(exception.as_ref(), env);
                }
                
                &exception.to_string()
            }
        };

        let backtrace = match backtrace {
            Some(backtrace) => &backtrace.to_string(),
            None => ""
        };

        format!("thread{thread_name} panicked at {loc}:\n{msg}\n{backtrace}", loc=panic.location)
    }

    if let Err(panic) = result {
        // Catch a panic caused by generating the panic print
        std::panic::resume_unwind(Box::new(create_panic_stmnt(panic, env)))
    }
}

/// Assert that a test (**f**) should **fail** (`panic!`) with a specific **error message**.
pub fn fail_with(f: impl FnOnce() + UnwindSafe, expected_error: &str) {
    let (result, env) = unsafe { ez_jni::__throw::run_with_jnienv_helper(get_env(), true, |_| f()) };

    let msg = result
        .map_err(|panic| match panic.panic {
            PanicType::Message(msg) => msg.into_owned(),
            PanicType::Object(exception) => {
                JavaException::from_throwable(<&JThrowable>::from(exception.as_obj()), ez_jni::utils::get_env())
                    .to_string()
            },
            PanicType::Unknown => panic!("{}", PanicType::UNKNOWN_PAYLOAD_TYPE_MSG),
        })
        .expect_err("Expected function to fail, but it succeeded");

    if msg != expected_error {
        panic!("Expected function to fail with error:\n    {expected_error}\nBut the actual error was:\n    {msg}")
    }
}
