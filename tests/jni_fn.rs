mod common;

use std::{path::PathBuf, process::Command, sync::LazyLock};
use utils::{CLASS_DIR, absolute_path, run};
use common::{get_env, run_with_jnienv};

static NATIVE_TEST_DIR: LazyLock<PathBuf> = LazyLock::new(|| absolute_path("./tests/native_test"));

/// Tests building a binary library (`/tests/native_test`) that exports Rust functions to be called from Java.
#[test]
fn jni_fn() {
    // Create directory where Class binaries are stored (if it's not created already)
    run(Command::new("mkdir")
        .args(["-p", CLASS_DIR])
    ).unwrap_or_else(|err| panic!("{err}"));
    // Compile Java code
    run(Command::new("javac")
        .current_dir(&*NATIVE_TEST_DIR)
        .args(["./src/Native.java", "-d"])
        .arg(absolute_path(CLASS_DIR))
    ).unwrap_or_else(|err| panic!("{err}"));
    // Compile Rust library
    run(Command::new("cargo")
        .current_dir(&*NATIVE_TEST_DIR)
        .args(["build"])
    ).unwrap_or_else(|err| panic!("{err}"));
    // Run Java binary
    run(Command::new("java")
        .env("LD_LIBRARY_PATH", absolute_path("./target/debug"))
        .current_dir(absolute_path(CLASS_DIR))
        .stdout(std::io::stdout())
        .args(["-ea", "me/test/Native"])
    ).unwrap_or_else(|err| panic!("{err}"));
}

/// Tests when a jni_fn `panic!s` and the panic data jas to be thrown to the JVM.
#[test]
fn throw_panic() {
    ez_jni::__throw::run_with_jnienv::<()>(get_env(), |_| panic!("Release me!"));
    let exception = ez_jni::__throw::try_catch::<String>(&mut get_env()).unwrap();
    assert_eq!(exception, "me.marti.ezjni.RustPanic: Release me!");

    run_with_jnienv(|| {
        let err = std::panic::catch_unwind(|| {
            panic!("Release me!")
        }).unwrap_err();
        assert_eq!(&"Release me!", err.downcast::<&'static str>().unwrap().as_ref());
    });
}
