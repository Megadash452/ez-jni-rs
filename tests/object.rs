mod common;

use ez_jni::{call, new, utils::get_string, FromException, FromObject};
use jni::{
    objects::{JObject, JString},
    JNIEnv,
};

#[derive(FromObject)]
#[class(me.test.Test)]
struct MyClass {
    member_field: i32,
}

#[derive(FromObject)]
#[class(me.test.Test)]
struct MyClass1 {
    #[field(call = memberGetter)]
    member: i32,
}

#[derive(FromObject)]
#[class(me.test.Test)]
struct MyClass2<'local> {
    #[field(call = memberObject, class = java.lang.Integer)]
    member: JObject<'local>,
}

#[derive(Debug, FromObject, PartialEq, Eq)]
#[class(me.test.Test.SumClass)]
enum MyEnumClass {
    #[class(me.test.Test$SumClass$SumClass1)]
    Variant1 { number: i32 },
    #[class(me.test.Test$SumClass$SumClass2)]
    Variant2 { str: String },
}

#[test]
fn from_object() {
    setup_env!(env);
    const VAL: i32 = 3;
    let mut object = new!(me.test.Test(int(3)));

    assert_eq!(MyClass::from_object(&object, &mut env).unwrap().member_field, VAL);
    assert_eq!(MyClass1::from_object(&object, &mut env).unwrap().member, VAL);
    let int = MyClass2::from_object(&object, &mut env).unwrap().member;
    assert_eq!(
        call!(int.intValue() -> int),
        VAL
    );

    object = new!(me.test.Test$SumClass$SumClass1(int(VAL)));
    assert_eq!(MyEnumClass::from_object(&object, &mut env).unwrap(), MyEnumClass::Variant1 { number: VAL });

    const S: &str = "Hello, World!";
    object = new!(me.test.Test$SumClass$SumClass2(java.lang.String(env.new_string(S).unwrap())));
    assert_eq!(MyEnumClass::from_object(&object, &mut env).unwrap(), MyEnumClass::Variant1 { number: VAL });
}

#[derive(FromException)]
#[class(java.lang.Exception)]
struct MyErr1 {
    // Implicitly calls getMessage()
    message: String,
}

#[derive(FromException)]
#[class(java.lang.Exception)]
struct MyErr2 {
    #[field(call = getMessage)]
    msg: String,
}

#[derive(FromException)]
#[class(java.lang.Exception)]
struct Exception<'local>(#[field(call = getMessage, class = java.lang.String)] JObject<'local>);
impl<'local> Exception<'local> {
    fn message(&self, env: &mut JNIEnv) -> String {
        get_string(JString::from(env.new_local_ref(&self.0).unwrap()), env)
    }
}

#[test]
fn from_exception() {
    setup_env!(env);
    assert_eq!(
        call!(static me.test.Test::throwObj() -> Result<java.lang.Object, MyErr1>)
            .unwrap_err()
            .message,
        "exception"
    );
    assert_eq!(
        call!(static me.test.Test::throwObj() -> Result<java.lang.Object, MyErr2>)
            .unwrap_err()
            .msg,
        "exception"
    );
    assert_eq!(
        call!(static me.test.Test::throwObj() -> Result<java.lang.Object, Exception>)
            .unwrap_err()
            .message(&mut env),
        "exception"
    );
}
