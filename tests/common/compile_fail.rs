//! # Test > Compile Fail
//! 
//! This file contains tools for testing that certain inputs for the macros in [`jni_macros`](./../../jni_macros/) will cause errors.
//! 
//! This uses [`trybuild`](https://crates.io/crates/trybuild) under the hood.

use std::{io::Write as _, path::PathBuf};
use trybuild::TestCases;

static DIR: &str = "./target/tmp/compile_fail";

#[macro_export]
macro_rules! assert_compile_fail_quote {
    ($t:ident, $input:tt) => {
        assert_compile_fail_quote!($t, $input, None, __private)
    };
    ($t:ident, $input:tt, $error:expr) => {
        assert_compile_fail_quote!($t, $input, Some($error), __private)
    };
    ($t:ident, $input:tt, $error:expr, __private) => {
        $crate::common::compile_fail::assert_compile_fail($t,
            ::prettyplease::unparse(&::syn::parse2(quote! $input)
                .unwrap_or_else(|err| panic!("Error parsing input: {err}"))
            ).as_str(),
            $error
        )
    }
}

/// Attempts to compile the Rust code passed in as a `.rs` file and `panic!`s if compilation succeeds.
pub fn assert_compile_fail(t: &TestCases, name: &str, input: &str, error: Option<&str>) {
    // Gather resources
    std::fs::create_dir_all(DIR).unwrap();
    let file_path = PathBuf::from(DIR).join(format!("{name}.rs"));
    let error_path = PathBuf::from(DIR).join(format!("{name}.stderr"));
    // Write Rust file content
    std::fs::File::create(&file_path)
        .unwrap_or_else(|err| panic!("Error opeining compile_fail file: {err}"))
        .write_all(format!("fn main() {{let env = std::mem::zeroed();\n\n{input}\n\n}}").as_bytes())
        .unwrap_or_else(|err| panic!("Error writing to file: {err}"));
    // Write error content
    if let Some(error) = &error {
        std::fs::File::create(&error_path)
            .unwrap_or_else(|err| panic!("Error opening error file: {err}"))
            .write_all(error.as_bytes())
            .unwrap_or_else(|err| panic!("Error writing to error file: {err}"));
    }
    // Run test
    t.compile_fail(&file_path);
}