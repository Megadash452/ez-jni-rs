mod common;

use std::{path::{Path, PathBuf}, process::Command, sync::LazyLock};
use common::CLASS_DIR;

static NATIVE_TEST_DIR: LazyLock<PathBuf> = LazyLock::new(|| absolute_path("./tests/native_test"));

/// Tests building a binary library (`/tests/native_test`) that exports Rust functions to be called from Java.
#[test]
fn jni_fn() {
    // Create directory where Class binaries are stored (if it's not created already)
    run(Command::new("mkdir")
        .args(["-p", CLASS_DIR])
    );
    // Compile Java code
    run(Command::new("javac")
        .current_dir(&*NATIVE_TEST_DIR)
        .args(["./src/Native.java", "-d"])
        .arg(absolute_path(CLASS_DIR))
    );
    // Compile Rust library
    run(Command::new("cargo")
        .current_dir(&*NATIVE_TEST_DIR)
        .args(["build"])
    );
    // Run Java binary
    run(Command::new("java")
        .env("LD_LIBRARY_PATH", absolute_path("./target/debug"))
        .current_dir(absolute_path(CLASS_DIR))
        .args(["me/test/Native"])
    );
}

fn absolute_path(path: impl AsRef<Path>) -> PathBuf {
    let path = path.as_ref();
    path.canonicalize()
        .unwrap_or_else(|err| panic!("Failed to make path \"{}\" absolute: {err}", path.display()))
}

fn run(command: &mut Command) -> String {
    let command_name = command.get_program().to_string_lossy().to_string();
    let output = command.output()
        .unwrap_or_else(|err| panic!("Failed to spawn command \"{command_name}\": {err}"));
    if !output.status.success() {
        let error = {
            String::from_utf8_lossy(&output.stderr)
        };
        panic!("Command \"{command_name}\" exited with error:\n{error}")
    }

    String::from_utf8(output.stdout)
        .unwrap_or_else(|err| panic!("Failed to decode output of command \"{command_name}\": {err}"))
}