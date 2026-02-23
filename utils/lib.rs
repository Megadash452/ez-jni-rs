//! This is an utils crate with functions that are used in the main crate, macros, and tests
use std::{fmt::Write, io, path::{Path, PathBuf}, process::Command, sync::LazyLock};

use jni::JavaVM;

pub static CLASS_DIR: &'static str = "./target/tmp/classes";

pub static TEST_JVM: LazyLock<JavaVM> = LazyLock::new(new_test_jvm);
fn new_test_jvm() -> JavaVM {
    // Compile Test class
    std::fs::create_dir_all(CLASS_DIR)
        .unwrap_or_else(|err| panic!("Error creating directory \"{CLASS_DIR}\": {err}"));
    let output = Command::new("javac")
        .args(["./tests/Test.java", "-d", CLASS_DIR])
        .output()
        .unwrap_or_else(|err| panic!("Error spawning Java compiler: {err}"));
    if !output.status.success() {
        panic!("Error compiling Java file:\n{}", String::from_utf8_lossy(&output.stderr));
    }

    // Start JVM with the classes directory and Assertions.
    JavaVM::new(jni::InitArgsBuilder::new()
        .option(format!("-Djava.class.path={CLASS_DIR}"))
        .option("-ea")
        .build()
        .unwrap()
    )
        .unwrap_or_else(|err| panic!("Error starting JVM: {err}"))
}

/// Convert the first letter of a String into uppercase
pub fn first_char_uppercase(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        None => String::new(),
    }
}

/// Converts a ClassPath that uses *slashes* `/` to separate components to a Class that uses *dots* `.`.
/// 
/// e.g. `"[Ljava/lang/String;" -> "[Ljava.lang.String;"`
pub fn java_path_to_dot_notation(path: &str) -> String {
    path.replace(['/', '$'], ".")
}

/// Convert the *fully-qualified* name of a *Java method* to the name that should be used in the native code.
pub fn java_method_to_symbol(class: &str, method: &str) -> String {
    format!("Java_{class}_{name}",
        class = class
            .replace('.', "_")
            .replace('/', "_"),
        name = method.replace('_', "_1"),
    )
}

pub fn absolute_path(path: impl AsRef<Path>) -> PathBuf {
    let path = path.as_ref();
    path.canonicalize()
        .unwrap_or_else(|err| panic!("Failed to make path \"{}\" absolute: {err}", path.display()))
}

pub fn run(command: &mut Command) -> io::Result<String> {
    let command_name = command.get_program().to_string_lossy().to_string();
    // Spawn the command, waiting for it to return.
    let output = command.output()
        .map_err(|err| io::Error::new(err.kind(), format!("Failed to spawn command \"{command_name}\": {err}")))?;
    // Return the command's error stream if it returned an error
    if !output.status.success() {
        let mut buf = String::new();
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);

        writeln!(buf, "Command \"{command_name}\" exited with error:").unwrap();
        if !stdout.is_empty() {
            writeln!(buf, "stdout:").unwrap();
            for line in stdout.lines() {
                writeln!(buf, "  {line}").unwrap();
            }
        }
        if !stderr.is_empty() {
            writeln!(buf, "stderr:").unwrap();
            for line in stderr.lines() {
                writeln!(buf, "  {line}").unwrap();
            }
        }

        return Err(io::Error::other(buf))
    }

    String::from_utf8(output.stdout)
        .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, format!("Failed to decode output of command \"{command_name}\": {err}")))
}
