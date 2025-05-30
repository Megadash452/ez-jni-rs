#![allow(unused)]
pub mod compile_fail;

use std::{process::Command, sync::LazyLock};
use jni::{JNIEnv, JavaVM};
use utils::CLASS_DIR;

static JVM: LazyLock<JavaVM> = LazyLock::new(|| {
    compile_java()
        .unwrap_or_else(|err| panic!("Error compiling Java file: {err}"));
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