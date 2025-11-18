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
            msg: "the trait bound `JObject<'local>: Class` is not satisfied",
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
            note: required by a bound in `field_from_jvalue`
             --> src/utils/object.rs
              |
              | pub trait FieldFromJValue<'obj, 'local>: Class + Sized {
              |                                          ^^^^^ required by this bound in `FieldFromJValue::field_from_jvalue`
              |     fn field_from_jvalue(val: JValueOwned<'obj>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError>;
              |        ----------------- required by a bound in this associated function
        ")},
        ErrorContent {
            code: Some("E0277"),
            msg: "the trait bound `JObject<'local>: FieldFromJValue<'_, '_>` is not satisfied",
            loc: "9:13",
            preview: indoc!("
              |
            9 |     member: JObject<'local>,
              |             ^^^^^^^^^^^^^^^ the trait `FromJValue<'_>` is not implemented for `JObject<'local>`
              |
              = help: the following other types implement trait `FromJValue<'local>`:
                        `()` implements `FromJValue<'_>`
                        `Box<[T]>` implements `FromJValue<'local>`
                        `MyClass<'local>` implements `FromJValue<'local>`
                        `ObjectArray<T, Array>` implements `FromJValue<'local>`
                        `Option<T>` implements `FromJValue<'local>`
                        `Vec<T>` implements `FromJValue<'local>`
                        `bool` implements `FromJValue<'_>`
                        `char` implements `FromJValue<'_>`
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
    "), [
        ErrorContent {
            code: Some("E0277"),
            msg: "the trait bound `JThrowable<'local>: Class` is not satisfied",
            loc: "9:13",
            preview: indoc!("
              |
            9 |     member: JThrowable<'local>,
              |             ^^^^^^^^^^^^^^^^^^ the trait `Class` is not implemented for `JThrowable<'local>`
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
            note: required by a bound in `field_from_jvalue`
             --> src/utils/object.rs
              |
              | pub trait FieldFromJValue<'obj, 'local>: Class + Sized {
              |                                          ^^^^^ required by this bound in `FieldFromJValue::field_from_jvalue`
              |     fn field_from_jvalue(val: JValueOwned<'obj>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError>;
              |        ----------------- required by a bound in this associated function
        ")},
        ErrorContent {
            code: Some("E0277"),
            msg: "the trait bound `JThrowable<'local>: FieldFromJValue<'_, '_>` is not satisfied",
            loc: "9:13",
            preview: indoc!("
              |
            9 |     member: JThrowable<'local>,
              |             ^^^^^^^^^^^^^^^^^^ the trait `FromJValue<'_>` is not implemented for `JThrowable<'local>`
              |
              = help: the following other types implement trait `FromJValue<'local>`:
                        `()` implements `FromJValue<'_>`
                        `Box<[T]>` implements `FromJValue<'local>`
                        `MyClass<'local>` implements `FromJValue<'local>`
                        `ObjectArray<T, Array>` implements `FromJValue<'local>`
                        `Option<T>` implements `FromJValue<'local>`
                        `Vec<T>` implements `FromJValue<'local>`
                        `bool` implements `FromJValue<'_>`
                        `char` implements `FromJValue<'_>`
                      and $N others
              = note: required for `JThrowable<'local>` to implement `FieldFromJValue<'_, '_>`
        ")},
    ]);
    assert_compile_fail(t, "jobject_array_require_class", indoc!("
        use ez_jni::{FromObject, ObjectArray};
        use jni::objects::JObject;

        #[derive(FromObject)]
        #[class(me.test.Test)]
        struct MyClass<'local> {
            member: ObjectArray<JObject<'local>>,
        }
    "), [ErrorContent {
        code: Some("E0277"),
        msg: "the trait bound `JObject<'local>: Class` is not satisfied",
        loc: "9:13",
        preview: indoc!("
          |
        9 |     member: ObjectArray<JObject<'local>>,
          |             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ the trait `Class` is not implemented for `JObject<'local>`
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
          = note: required for `ObjectArray<JObject<'local>>` to implement `Class`
        note: required by a bound in `field_from_jvalue`
         --> src/utils/object.rs
          |
          | pub trait FieldFromJValue<'obj, 'local>: Class + Sized {
          |                                          ^^^^^ required by this bound in `FieldFromJValue::field_from_jvalue`
          |     fn field_from_jvalue(val: JValueOwned<'obj>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError>;
          |        ----------------- required by a bound in this associated function
    ")}]);
    assert_compile_fail(t, "jthrowable_array_require_class", indoc!("
        use ez_jni::{FromObject, ObjectArray};
        use jni::objects::JThrowable;

        #[derive(FromObject)]
        #[class(me.test.Test)]
        struct MyClass<'local> {
            member: ObjectArray<JThrowable<'local>>,
        }
    "), [ErrorContent {
        code: Some("E0277"),
        msg: "the trait bound `JThrowable<'local>: Class` is not satisfied",
        loc: "9:13",
        preview: indoc!("
          |
        9 |     member: ObjectArray<JThrowable<'local>>,
          |             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ the trait `Class` is not implemented for `JThrowable<'local>`
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
          = note: required for `ObjectArray<JThrowable<'local>>` to implement `Class`
        note: required by a bound in `field_from_jvalue`
         --> src/utils/object.rs
          |
          | pub trait FieldFromJValue<'obj, 'local>: Class + Sized {
          |                                          ^^^^^ required by this bound in `FieldFromJValue::field_from_jvalue`
          |     fn field_from_jvalue(val: JValueOwned<'obj>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError>;
          |        ----------------- required by a bound in this associated function
    ")}]);
}