mod common;

use std::{path::PathBuf, process::Command, sync::{Arc, Barrier, LazyLock}};
use jni::JNIEnv;
use utils::{CLASS_DIR, absolute_path, run};

static NATIVE_TEST_DIR: LazyLock<PathBuf> = LazyLock::new(|| absolute_path("./tests/native_test"));

fn attach_to_jvm<'local>() -> JNIEnv<'local> {
    utils::TEST_JVM.attach_current_thread_permanently()
        .unwrap_or_else(|err| panic!("Error attaching thread to JVM: {err}"))
}

/// Tests building a binary library (`/tests/native_test`) that exports Rust functions to be called from Java.
#[test]
fn jni_fn() {
    let library_path = absolute_path("./target/debug");

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
        .current_dir(absolute_path(CLASS_DIR))
        .arg(format!("-Djava.library.path={}", library_path.display()))
        .args(["-ea", "me/test/Native"])
    ).unwrap_or_else(|err| panic!("{err}"));
}

/// Tests when a jni_fn `panic!s`, it catches it, and a RustPanic Exception is thrown to the JVM.
#[test]
fn throw_panic() {
    let mut env = attach_to_jvm();
    unsafe { ez_jni::__throw::run_with_jnienv::<()>(env, |_| panic!("Release me!")) };
    env = attach_to_jvm();
    unsafe { ez_jni::__throw::run_with_jnienv::<()>(env, |env| {
        let exception = ez_jni::__throw::catch_exception(env).unwrap();
        assert_eq!(exception.to_string(), "me.marti.ezjni.RustPanic: Release me!");
    }) };
}
/// Tests the same as [`throw_panic()`], but with a [`catch_unwind`] inside.
#[test]
fn throw_panic_catch() {
    unsafe { ez_jni::__throw::run_with_jnienv(attach_to_jvm(), |_| {
        let err = std::panic::catch_unwind(|| {
            panic!("Release me!")
        }).unwrap_err();
        assert_eq!(&"Release me!", err.downcast::<&'static str>().unwrap().as_ref());
    }) };
}

fn panic_with_jni(i: usize) {
    if i == 5 {
        panic!("Release me {i}");
    }
    unsafe { ez_jni::__throw::run_with_jnienv_helper(attach_to_jvm(), false, |_| {
        panic_with_jni(i + 1);
    }).0.unwrap_err() };
    if i > 0 {
        panic!("Release me {i}");
    }
}

#[test]
fn run_with_jni_recursion_multithreaded() {
    // Use a Barrier to start executing on both threads at the EXACT same time.
    // Barrier is set to 3 because we have 2 spawned threads + the main thread
    let barrier = Arc::new(Barrier::new(3));
    let t1_barrier = barrier.clone();
    let t2_barrier = barrier.clone();

    let t1 = std::thread::spawn(move || {
        t1_barrier.wait();
        panic_with_jni(0)
    });
    let t2 = std::thread::spawn(move || {
        t2_barrier.wait();
        panic_with_jni(0)
    });

    barrier.wait();
    t1.join().unwrap();
    t2.join().unwrap();
}
