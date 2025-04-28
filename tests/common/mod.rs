#![allow(unused)]
pub mod compile_fail;

use std::{process::Command, sync::LazyLock};
use jni::JavaVM;
use utils::CLASS_DIR;

pub static JVM: LazyLock<JavaVM> = LazyLock::new(|| {
    compile_java()
        .unwrap_or_else(|err| panic!("Error compiling Java file: {err}"));
    JavaVM::new(jni::InitArgsBuilder::new()
        .option(format!("-Djava.class.path={CLASS_DIR}"))
        .build()
        .unwrap()
    )
        .unwrap_or_else(|err| panic!("Error starting JavaVM: {err}"))
});

/// Must call it this exact same way: `setup_env!(env)`;
#[macro_export]
macro_rules! setup_env {
    ($var:ident) => {
        let mut $var = common::JVM.attach_current_thread_permanently()
            .unwrap_or_else(|err| panic!("Error attaching current thread to JavaVM: {err}"));
    };
}

pub fn setup_jvm() {
    todo!()
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