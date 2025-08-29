//! # Test > Compile Fail
//! 
//! This file contains tools for testing that certain inputs for the macros in [`jni_macros`](./../../jni_macros/) will cause errors.
//! 
//! This uses [`trybuild`](https://crates.io/crates/trybuild) under the hood.

use std::{io::Write as _, path::PathBuf, sync::LazyLock};
use trybuild::TestCases;
use utils::absolute_path;

#[macro_export]
macro_rules! assert_compile_fail {
    ($t:ident, $name:literal, $input:literal) => {
        assert_compile_fail!($t, $name, $input, None, __private)
    };
    ($t:ident, $name:literal, $input:literal, $error:expr) => {
        assert_compile_fail!($t, $name, $input, Some($error), __private)
    };
    ($t:ident, $name:literal, $input:literal, $error:expr, __private) => {
        $crate::common::compile_fail::assert_compile_fail(
            $t, $name, ::indoc::indoc!($input), $error
        )
    }
}

pub struct ErrorContent {
    pub code: Option<&'static str>,
    pub msg: &'static str,
    pub loc: &'static str,
    pub preview: &'static str,
}

/// Attempts to compile the Rust code passed in as a `.rs` file and `panic!`s if compilation succeeds.
pub fn assert_compile_fail(t: &TestCases, name: &str, input: &str, error: Option<ErrorContent>) {
    let dir = PathBuf::from("./target/tmp/compile_fail");
    // Gather resources
    std::fs::create_dir_all(&dir).unwrap();
    let file_path = dir.join(format!("{name}.rs"));
    let error_path = dir.join(format!("{name}.stderr"));
    // Write Rust file content
    std::fs::File::create(&file_path)
        .unwrap_or_else(|err| panic!("Error opeining compile_fail file: {err}"))
        // .write_all(format!("fn main() {{ #[allow(unused, invalid_value)] let env: &mut ::jni::JNIEnv<'_> = unsafe{{std::mem::zeroed()}};\n\n{input}\n\n}}").as_bytes())
        .write_all(format!("fn main() {{\n\n{input}\n\n}}").as_bytes())
        .unwrap_or_else(|err| panic!("Error writing to file: {err}"));
    // Write error content
    if let Some(error) = &error {
        let code = match error.code {
            Some(code) => format!("[{code}]"),
            None => String::new(),
        };

        std::fs::File::create(&error_path)
            .unwrap_or_else(|err| panic!("Error opening error file: {err}"))
            .write_all(format!("error{code}: {}\n --> {}:{}\n{}", error.msg, absolute_path(&file_path).display(), error.loc, error.preview).as_bytes())
            .unwrap_or_else(|err| panic!("Error writing to error file: {err}"));
    }
    // Run test
    t.compile_fail(&file_path);
}