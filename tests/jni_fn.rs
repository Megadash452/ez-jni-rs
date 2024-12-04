mod common;

use std::{path::PathBuf, process::Command, sync::LazyLock};
use utils::{CLASS_DIR, run, absolute_path};

static NATIVE_TEST_DIR: LazyLock<PathBuf> = LazyLock::new(|| absolute_path("./tests/native_test"));

/// Tests building a binary library (`/tests/native_test`) that exports Rust functions to be called from Java.
#[test]
fn jni_fn() {
    // Create directory where Class binaries are stored (if it's not created already)
    run(Command::new("mkdir")
        .args(["-p", CLASS_DIR])
    ).unwrap();
    // Compile Java code
    run(Command::new("javac")
        .current_dir(&*NATIVE_TEST_DIR)
        .args(["./src/Native.java", "-d"])
        .arg(absolute_path(CLASS_DIR))
    ).unwrap();
    // Compile Rust library
    run(Command::new("cargo")
        .current_dir(&*NATIVE_TEST_DIR)
        .args(["build"])
    ).unwrap();
    // Run Java binary
    run(Command::new("java")
        .env("LD_LIBRARY_PATH", absolute_path("./target/debug"))
        .current_dir(absolute_path(CLASS_DIR))
        .stdout(std::io::stdout())
        .args(["-ea", "me/test/Native"])
    ).unwrap();
}

/// Tests when a jni_fn `panic!s` and the panic data jas to be thrown to the JVM.
#[test]
fn throw_panic() {
    setup_env!(env);

    ez_jni::__throw::catch_throw(&mut env, |_| panic!("Release me!"));
    let exception = ez_jni::__throw::try_catch::<String>(&mut env).unwrap();
    assert_eq!(exception, "me.marti.ezjni.RustPanic: Release me!");
}