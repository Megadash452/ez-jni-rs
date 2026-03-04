#![allow(unused)]

pub mod compile_fail;

use std::panic::{UnwindSafe, catch_unwind};
use ez_jni::{__throw::PanicType, FromObject, JavaException, utils::ResultExt as _};

/// Assert that a test (**f**) should **fail** (`panic!`) with a specific **error message**.
pub fn fail_with(f: impl FnOnce() + UnwindSafe, expected_error: &str) {
    let msg = catch_unwind(f)
        .map_err(|payload| match PanicType::from(payload) {
            PanicType::Message(msg) => msg.into_owned(),
            PanicType::Object(object) => {
                JavaException::from_object(object.as_obj())
                    .unwrap_display()
                    .to_string()
            },
            PanicType::Unknown => panic!("{}", PanicType::UNKNOWN_PAYLOAD_TYPE_MSG),
        })
        .expect_err("Expected function to fail, but it succeeded");

    if msg != expected_error {
        panic!("Expected function to fail with error:\n    {expected_error}\nBut the actual error was:\n    {msg}")
    }
}
