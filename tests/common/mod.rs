#![allow(unused)]
pub mod compile_fail;

use std::{panic::{catch_unwind, AssertUnwindSafe, UnwindSafe}, process::Command, sync::LazyLock};
use jni::{JNIEnv, JavaVM};
use ez_jni::{call, __throw::PanicPayloadRepr};
use utils::CLASS_DIR;

static JVM: LazyLock<JavaVM> = LazyLock::new(|| {
    compile_java()
        .unwrap_or_else(|err| panic!("Error compiling Java file:\n{err}"));
    JavaVM::new(jni::InitArgsBuilder::new()
        .option(format!("-Djava.class.path={CLASS_DIR}"))
        .build()
        .unwrap()
    )
        .unwrap_or_else(|err| panic!("Error starting JavaVM: {err}"))
});

// /// Must call it this exact same way: `setup_env!(env)`;
// #[macro_export]
// macro_rules! setup_env {
//     ($var:ident) => {
//         let mut $var = common::JVM.attach_current_thread_permanently()
//             .unwrap_or_else(|err| panic!("Error attaching current thread to JavaVM: {err}"));
//     };
// }

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
/// The funciton `f` is allowed to `panic!`.
/// 
/// This is NOT the same [`run_with_jnienv`][ez_jni::__throw::run_with_jnienv()] from the library.
/// It operates completely differently and is only for running tests.
pub fn run_with_jnienv<'local>(f: impl FnOnce() + UnwindSafe) {
    // Assign the JNIEnv used for this jni call
    let stack_env = ::ez_jni::__throw::StackEnv::push(get_env());

    // Run the function
    // Pass a reference of the JNIEnv that was just pushed; for conversions
    let result = f();

    // Remove the JNIEnv when the function finishes running
    stack_env.pop();
}

/// Assert that a test (**f**) should **fail** (`panic!`) with a specific **error message**.
pub fn fail_with(f: impl Fn(), expected_error: &str) {
    // Set hook to prevent printing of the payload
    std::panic::set_hook(Box::new(|info| { }));
    let result = catch_unwind(AssertUnwindSafe(f));
    std::panic::take_hook();

    let msg = result
        .map_err(|payload| match PanicPayloadRepr::from(payload) {
            PanicPayloadRepr::Message(msg) => msg.to_string(),
            PanicPayloadRepr::Object(exception) => call!(exception.getMessage() -> String),
            PanicPayloadRepr::Unknown => panic!("{}", PanicPayloadRepr::UNKNOWN_PAYLOAD_TYPE_MSG),
        })
        .expect_err("Expected function to fail, but it succeeded");

    if msg != expected_error {
        panic!("Expected function to fail with error:\n    {expected_error}\nBut the actual error was:\n    {msg}")
    }
}

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
