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
    "), Some(ErrorContent {
        msg: "",
        loc: "",
        preview: indoc!("
        TODO
    ")}));
    // FIXME: comiler complains that JObject does not implement FromJValue, but that trait is not used at all here.
    assert_compile_fail(t, "jobject_require_class", indoc!("
        use ez_jni::FromObject;
        use jni::objects::JObject;

        #[derive(FromObject)]
        #[class(me.test.Test)]
        struct MyClass<'local> {
            member: JObject<'local>,
        }
    "), Some(ErrorContent {
        msg: "",
        loc: "",
        preview: indoc!("
        TODO
    ")}));
    assert_compile_fail(t, "jthrowable_require_class", indoc!("
        use ez_jni::FromObject;
        use jni::objects::JThrowable;

        #[derive(FromObject)]
        #[class(me.test.Test)]
        struct MyClass<'local> {
            member: JThrowable<'local>,
        }
    "), Some(ErrorContent {
        msg: "",
        loc: "",
        preview: indoc!("
        TODO
    ")}));
}