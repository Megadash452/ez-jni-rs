mod common;

use common::compile_fail::assert_compile_fail;
use trybuild::TestCases;

#[test]
fn return_() {
    let t = &TestCases::new();

assert_compile_fail(t, "option_void", "
use ez_jni::call;
call!(static me.test.Test.method() -> Option<void>);
    ", Some("\
error: 'void' is not allowed here.
 --> ./target/tmp/compile_fail/option_void.rs:5:46
  |
5 | call!(static me.test.Test.method() -> Option<void>);
  |                                              ^^^^
"));

assert_compile_fail(t, "option_prim", "
use ez_jni::call;
call!(static me.test.Test.method() -> Option<int>);
    ", Some("\
error: Option can't be used with primitives, only Classes.
 --> ./target/tmp/compile_fail/option_prim.rs:5:39
  |
5 | call!(static me.test.Test.method() -> Option<int>);
  |                                       ^^^^^^
"));

assert_compile_fail(t, "option_result", "
use ez_jni::call;
call!(static me.test.Test.method() -> Option<Result<int, String>>);
    ", Some("\
error: 'Result' is not allowed as an inner type, it must be the outermost type.
 --> ./target/tmp/compile_fail/option_result.rs:5:46
  |
5 | call!(static me.test.Test.method() -> Option<Result<int, String>>);
  |                                              ^^^^^^
"));

assert_compile_fail(t, "option_option", "
use ez_jni::call;
call!(static me.test.Test.method() -> Option<Option<int>>);
    ", Some("\
error: 'Option' is not allowed here.
 --> ./target/tmp/compile_fail/option_option.rs:5:46
  |
5 | call!(static me.test.Test.method() -> Option<Option<int>>);
  |                                              ^^^^^^
"));

assert_compile_fail(t, "result_result", "
use ez_jni::call;
call!(static me.test.Test.method() -> Result<Result<int, String>, String>);
    ", Some("\
error: Can't nest a Result within a Result.
 --> ./target/tmp/compile_fail/result_result.rs:5:46
  |
5 | call!(static me.test.Test.method() -> Result<Result<int, String>, String>);
  |                                              ^^^^^^
"));

// assert_compile_fail(t, "array_option_prim", "
// use ez_jni::call;
// call!(static me.test.Test.getIntArray() -> [Option<int>]);
//     ", Some("\
// error: Option can't be used with primitives, only Classes.
//  --> ./target/tmp/compile_fail/array_option_prim.rs:5:45
//   |
// 5 | call!(static me.test.Test.getIntArray() -> [Option<int>]);
//   |                                             ^^^^^^
// "));
}

#[test]
fn arguments() {
    let t = &TestCases::new();

assert_compile_fail(t, "param_prim_null", "
use ez_jni::call;
call!(static me.test.Test.method(int(null)) -> void);
    ", Some("\
error: Can't use 'null' as value of primitive parameter type
 --> ./target/tmp/compile_fail/param_prim_null.rs:5:38
  |
5 | call!(static me.test.Test.method(int(null)) -> void);
  |                                      ^^^^
"));

// assert_compile_fail(t, "param_array_prim_null", "
// use ez_jni::call;
// call!(static me.test.Test.method([int]([1, null])) -> void);
//     ", Some("\
// error: Can't use 'null' as value of primitive parameter type
//  --> ./target/tmp/compile_fail/param_prim_null.rs:5:38
//   |
// 5 | call!(static me.test.Test.method(int(null)) -> void);
//   |                                      ^^^^
// "));
}