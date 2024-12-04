//! This is an utils crate with functions that are used in the main crate, macros, and tests
use std::{io, path::{Path, PathBuf}, process::Command};

pub static CLASS_DIR: &'static str = "./target/tmp/classes";

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
        let error = String::from_utf8_lossy(&output.stderr);
        return Err(io::Error::other(format!("Command \"{command_name}\" exited with error:\n{error}")))
    }

    String::from_utf8(output.stdout)
        .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, format!("Failed to decode output of command \"{command_name}\": {err}")))
}
