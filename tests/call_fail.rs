mod common;

use common::compile_fail::{assert_compile_fail, ErrorContent};
use indoc::indoc;
use trybuild::TestCases;

#[test]
fn return_() {
    let t = &TestCases::new();

    assert_compile_fail(t, "option_void", indoc!("
        use ez_jni::call;
        call!(static me.test.Test.method() -> Option<void>);
    "), Some(ErrorContent {
        code: None,
        msg: "'void' is not allowed in arguments or within 'Option' in the return type.",
        loc: "4:46",
        preview: indoc!("
          |
        4 | call!(static me.test.Test.method() -> Option<void>);
          |                                              ^^^^
    ")}));
    // assert_compile_fail(t, "option_prim", indoc!("
    //     use ez_jni::call;
    //     call!(static me.test.Test.method() -> Option<int>);
    // "), Some(ErrorContent {
    //     code: None,
    //     msg: "Primitives are not allowed as the Option's inner type. Only Classes are allowed here.",
    //     loc: "4:46",
    //     preview: indoc!("
    //       |
    //     4 | call!(static me.test.Test.method() -> Option<int>);
    //       |                                              ^^^
    // ")}));
    assert_compile_fail(t, "option_result", indoc!("
        use ez_jni::call;
        call!(static me.test.Test.method() -> Option<Result<int, String>>);
    "), Some(ErrorContent {
        code: None,
        msg: "'Result' is not allowed as an inner type, it must be the outermost type.",
        loc: "4:46",
        preview: indoc!("
          |
        4 | call!(static me.test.Test.method() -> Option<Result<int, String>>);
          |                                              ^^^^^^
    ")}));
    assert_compile_fail(t, "option_option", indoc!("
        use ez_jni::call;
        call!(static me.test.Test.method() -> Option<Option<int>>);
    "), Some(ErrorContent {
        code: None,
        msg: "'Option' is not allowed to be nested within itself.",
        loc: "4:46",
        preview: indoc!("
          |
        4 | call!(static me.test.Test.method() -> Option<Option<int>>);
          |                                              ^^^^^^
    ")}));
    assert_compile_fail(t, "result_result", indoc!("
        use ez_jni::call;
        call!(static me.test.Test.method() -> Result<Result<int, String>, String>);
    "), Some(ErrorContent {
        code: None,
        msg: "'Result' is not allowed as an inner type, it must be the outermost type.",
        loc: "4:46",
        preview: indoc!("
          |
        4 | call!(static me.test.Test.method() -> Result<Result<int, String>, String>);
          |                                              ^^^^^^
    ")}));
    // assert_compile_fail(t, "array_option_prim", indoc!("
    //     use ez_jni::call;
    //     call!(static me.test.Test.getIntArray() -> [Option<int>]);
    // "), Some(ErrorContent {
    //     code: None,
    //     msg: "Primitives are not allowed as the Option's inner type. Only Classes are allowed here.",
    //     loc: "4:52",
    //     preview: indoc!("
    //       |
    //     4 | call!(static me.test.Test.getIntArray() -> [Option<int>]);
    //       |                                                    ^^^
    // ")}));
}

#[test]
fn arguments() {
    let t = &TestCases::new();

    // assert_compile_fail(t, "param_option", indoc!("
    //     use ez_jni::call;
    //     call!(static me.test.Test.method(Option<int>(null)) -> void);
    // "), Some(ErrorContent {
    //     code: None,
    //     msg: "Primitives are not allowed as the Option's inner type. Only Classes are allowed here.",
    //     loc: "4:41",
    //     preview: indoc!("
    //       |
    //     4 | call!(static me.test.Test.method(Option<int>(null)) -> void);
    //       |                                         ^^^
    // ")}));
    assert_compile_fail(t, "param_option_array", indoc!("
        use ez_jni::call;
        call!(static me.test.Test.method(Option<[int]>(null)) -> void);
    "), Some(ErrorContent {
        code: None,
        msg: "Can't use 'Option' in call! or new! arguments. Instead, use 'null' as the value.",
        loc: "4:34",
        preview: indoc!("
          |
        4 | call!(static me.test.Test.method(Option<[int]>(null)) -> void);
          |                                  ^^^^^^
    ")}));
    // assert_compile_fail(t, "param_array_option", indoc!("
    //     use ez_jni::call;
    //     call!(static me.test.Test.method([Option<int>](null)) -> void);
    // "), Some(ErrorContent {
    //     code: None,
    //     msg: "Primitives are not allowed as the Option's inner type. Only Classes are allowed here.",
    //     loc: "4:42",
    //     preview: indoc!("
    //       |
    //     4 | call!(static me.test.Test.method([Option<int>](null)) -> void);
    //       |                                          ^^^
    // ")}));
    assert_compile_fail(t, "param_prim_null", indoc!("
        use ez_jni::call;
        call!(static me.test.Test.method(int(null)) -> void);
    "), Some(ErrorContent {
        code: None,
        msg: "Can't use 'null' as value of primitive argument type.",
        loc: "4:38",
        preview: indoc!("
          |
        4 | call!(static me.test.Test.method(int(null)) -> void);
          |                                      ^^^^
    ")}));
    
    // TODO: "This expression resolves to a raw object, which is not allowed here. If you want to pass a null value, try using '{NULL_KEYWORD}' as the value."
    // assert_compile_fail(t, "param_rust_type_null", indoc!("
    //     use ez_jni::call;
    //     call!(static me.test.Test.method(Integer(null)) -> void);
    // "), Some(ErrorContent {
    //     code: None,
    //     msg: "This expression resolves to a raw object, which is not allowed here. If you want to pass a null value, try using 'null' as the value.",
    //     loc: "4:38",
    //     preview: indoc!("
    //       |
    //     4 | call!(static me.test.Test.method(int(null)) -> void);
    //       |                                      ^^^^
    // ")}));
}