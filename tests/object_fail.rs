mod common;

use common::compile_fail::{assert_compile_fail, ErrorContent};
use indoc::indoc;
use trybuild::TestCases;

#[test]
fn from_object_derive() {
    let t = &TestCases::new();

    assert_compile_fail(t, "name_and_call_attrs_mutually_exclusive", indoc!("
        use ez_jni::FromObject;

        #[derive(FromObject)]
        #[class(me.test.Test)]
        struct MyClass {
            #[field(name = myMember, call = memberField)]
            member: String,
        }
    "), [ErrorContent {
        code: None,
        msg: "The field attributes 'name' and 'call' are mutually exclusive; only one can be used.",
        loc: "8:6",
        preview: indoc!("
          |
        8 |     #[field(name = myMember, call = memberField)]
          |      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ")}]);
    assert_compile_fail(t, "jobject_require_class", indoc!("
        use ez_jni::FromObject;
        use jni::objects::JObject;

        #[derive(FromObject)]
        #[class(me.test.Test)]
        struct MyClass<'local> {
            member: JObject<'local>,
        }
    "), [
        ErrorContent {
            code: Some("E0277"),
            msg: "the trait bound `JObject<'local>: FieldFromJValue<'_, '_>` is not satisfied",
            loc: "9:13",
            preview: indoc!("
              |
            9 |     member: JObject<'local>,
              |             ^^^^^^^^^^^^^^^ the trait `for<'a> FromJValue<'a, '_, '_>` is not implemented for `JObject<'local>`
              |
              = help: the trait `FromJValue<'a, '_, '_>` is not implemented for `JObject<'local>`
                      but trait `FromJValue<'_, '_, '_>` is implemented for `&JObject<'_>`
              = help: for that trait implementation, expected `&JObject<'_>`, found `JObject<'local>`
              = note: required for `JObject<'local>` to implement `FieldFromJValue<'_, '_>`
        ")},
        ErrorContent {
            code: Some("E0277"),
            msg: "the trait bound `JObject<'local>: FieldFromJValue<'_, '_>` is not satisfied",
            loc: "9:13",
            preview: indoc!("
              |
            9 |     member: JObject<'local>,
              |             ^^^^^^^^^^^^^^^ the trait `Class` is not implemented for `JObject<'local>`
              |
              = help: the following other types implement trait `Class`:
                        &T
                        &[T]
                        &str
                        (dyn std::error::Error + 'static)
                        Box<(dyn std::error::Error + 'static)>
                        Box<[T]>
                        JClass<'_>
                        JString<'_>
                      and $N others
              = note: required for `JObject<'local>` to implement `FieldFromJValue<'_, '_>`
        ")},

    ]);
    assert_compile_fail(t, "jthrowable_require_class", indoc!("
        use ez_jni::FromObject;
        use jni::objects::JThrowable;

        #[derive(FromObject)]
        #[class(me.test.Test)]
        struct MyClass<'local> {
            member: JThrowable<'local>,
        }
    "), [ErrorContent {
        code: Some("E0277"),
        msg: "the trait bound `JThrowable<'local>: FieldFromJValue<'_, '_>` is not satisfied",
        loc: "9:13",
        preview: indoc!("
          |
        9 |     member: JThrowable<'local>,
          |             ^^^^^^^^^^^^^^^^^^ the trait `for<'a> FromJValue<'a, '_, '_>` is not implemented for `JThrowable<'local>`
          |
          = help: the trait `FromJValue<'a, '_, '_>` is not implemented for `JThrowable<'local>`
                  but trait `FromJValue<'_, '_, '_>` is implemented for `&JThrowable<'_>`
          = help: for that trait implementation, expected `&JThrowable<'_>`, found `JThrowable<'local>`
          = note: required for `JThrowable<'local>` to implement `FieldFromJValue<'_, '_>`
    ")}]);
}