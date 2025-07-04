mod common;

use ez_jni::{call, new, utils::create_object_array_converted, FromException, FromObject, ToObject};
use jni::objects::JObject;

use crate::common::run_with_jnienv;

/// Tests the implementations of FromObject, etc. for *standard library* types.
#[test]
fn implementations() { run_with_jnienv(|| {
    static S: &str = "Hello, World!";
    static N: i8 = -3;
    static UN: i8 = 3;
    static F: f32 = -3.3;

    // Test String
    let mut obj = S.to_object();
    let s = String::from_object(&obj).unwrap();
    assert_eq!(s, S);

    // Testing Option
    obj = Some(S).to_object();
    let s = Option::<String>::from_object(&obj).unwrap();
    assert_eq!(s.unwrap(), S);

    assert_eq!(
        Option::<String>::from_object(&JObject::null()).unwrap(),
        None
    );

    // Testing IO Error
    obj = new!(java.io.FileNotFoundException(String(S)));
    let err = std::io::Error::from_object(&obj).unwrap();
    assert_eq!(err.to_string(), S);

    // Testing Number types
    obj = (N as i8).to_object();
    assert_eq!(N as i8, i8::from_object(&obj).unwrap());
    obj = (N as i16).to_object();
    assert_eq!(N as i16, i16::from_object(&obj).unwrap());
    obj = (N as i32).to_object();
    assert_eq!(N as i32, i32::from_object(&obj).unwrap());
    obj = (N as i64).to_object();
    assert_eq!(N as i64, i64::from_object(&obj).unwrap());
    obj = (F as f32).to_object();
    assert_eq!(F as f32, f32::from_object(&obj).unwrap());
    obj = (F as f64).to_object();
    assert_eq!(F as f64, f64::from_object(&obj).unwrap());
    obj = (UN as u8).to_object();
    assert_eq!(UN as u8, u8::from_object(&obj).unwrap());
    obj = (UN as u16).to_object();
    assert_eq!(UN as u16, u16::from_object(&obj).unwrap());
    obj = (UN as u32).to_object();
    assert_eq!(UN as u32, u32::from_object(&obj).unwrap());
    obj = (UN as u64).to_object();
    assert_eq!(UN as u64, u64::from_object(&obj).unwrap());
    obj = true.to_object();
    assert_eq!(true, bool::from_object(&obj).unwrap());
    obj = 'a'.to_object();
    assert_eq!('a', char::from_object(&obj).unwrap());
    // Testing Array types
    // String Array
    obj = ["Hello", "World"].to_object();
    assert_eq!(
        ["Hello", "World"],
        Box::<[String]>::from_object(&obj)
            .unwrap()
            .as_ref()
    );
    obj = [Some("Hello"), None].to_object();
    assert_eq!(
        [Some("Hello".to_string()), None],
        Box::<[Option<String>]>::from_object(&obj)
            .unwrap()
            .as_ref()
    );
    // Object Array
    obj = ("java/lang/String", ["Hello".to_object(), "World".to_object()]).to_object();
    assert_eq!(
        ["Hello", "World"],
        Box::<[String]>::from_object(&obj)
            .unwrap()
            .as_ref()
    );
    let _ = Box::<[JObject]>::from_object(&obj).unwrap();
    obj = ("java/lang/String", ["Hello".to_object(), JObject::null()]).to_object();
    assert_eq!(
        [Some("Hello".to_string()), None],
        Box::<[Option<String>]>::from_object(&obj)
            .unwrap()
            .as_ref()
    );
    let _ = Box::<[Option<JObject>]>::from_object(&obj).unwrap();
    // Primitives Arrays
    obj = [1i8, 2, 3].to_object();
    assert_eq!(
        [1, 2, 3],
        Box::<[i8]>::from_object(&obj).unwrap().as_ref()
    );
    obj = [1i16, 2, 3].to_object();
    assert_eq!(
        [1, 2, 3],
        Box::<[i16]>::from_object(&obj).unwrap().as_ref()
    );
    obj = [1i32, 2, 3].to_object();
    assert_eq!(
        [1, 2, 3],
        Box::<[i32]>::from_object(&obj).unwrap().as_ref()
    );
    obj = [1i64, 2, 3].to_object();
    assert_eq!(
        [1, 2, 3],
        Box::<[i64]>::from_object(&obj).unwrap().as_ref()
    );
    obj = [1.1f32, 2.2, 3.3].to_object();
    assert_eq!(
        [1.1f32, 2.2, 3.3],
        Box::<[f32]>::from_object(&obj).unwrap().as_ref()
    );
    obj = [1.1f64, 2.2, 3.3].to_object();
    assert_eq!(
        [1.1f64, 2.2, 3.3],
        Box::<[f64]>::from_object(&obj).unwrap().as_ref()
    );
    obj = [1u8, 2, 3].to_object();
    assert_eq!(
        [1, 2, 3],
        Box::<[u8]>::from_object(&obj).unwrap().as_ref()
    );
    obj = [1u16, 2, 3].to_object();
    assert_eq!(
        [1, 2, 3],
        Box::<[u16]>::from_object(&obj).unwrap().as_ref()
    );
    obj = [1u32, 2, 3].to_object();
    assert_eq!(
        [1, 2, 3],
        Box::<[u32]>::from_object(&obj).unwrap().as_ref()
    );
    obj = [1u64, 2, 3].to_object();
    assert_eq!(
        [1, 2, 3],
        Box::<[u64]>::from_object(&obj).unwrap().as_ref()
    );
    obj = [true, false].to_object();
    assert_eq!(
        [true, false],
        Box::<[bool]>::from_object(&obj).unwrap().as_ref()
    );
    obj = ['a', 'b', 'c'].to_object();
    assert_eq!(
        ['a', 'b', 'c'],
        Box::<[char]>::from_object(&obj).unwrap().as_ref()
    );

    // Multi-dimensional Arrays
    obj = [["Hello"], ["World"]].to_object();
    assert_eq!(
        [["Hello"], ["World"]],
        Box::<[Box::<[String]>]>::from_object(&obj).unwrap().as_ref()
    );
}) }

#[derive(Debug, FromObject, PartialEq, Eq)]
#[class(me.test.Test)]
struct MyClass {
    member_field: i32,
}

#[derive(FromObject)]
#[class(me.test.Test)]
struct MyClass1 {
    #[field(call = memberGetter)]
    member: i32,
    array_field: Box<[String]>,
}

#[derive(FromObject)]
#[class(me.test.Test)]
struct MyClass2<'local> {
    #[field(call = memberObject, class = java.lang.Integer)]
    member: JObject<'local>,
    array_field: Box<[Option<String>]>,
}

#[derive(FromObject)]
#[class(me.test.Test)]
struct MyClass3 {
    #[field(call = memberObject, class = java.lang.Integer)]
    member: i32,
}

#[derive(Debug, FromObject, PartialEq, Eq)]
#[class(me.test.Test$SumClass)]
enum MyEnumClass {
    #[class(me.test.Test$SumClass$SumClass1)]
    Variant1 { number: i32 },
    #[class(me.test.Test$SumClass$SumClass2)]
    Variant2 { str: String },
}

/// This one is not tested at runtime, as it is identical to [`MyEnumClass`].
#[allow(dead_code)]
#[derive(Debug, FromObject)]
enum MyEnumClass2 {
    #[class(me.test.Test$SumClass$SumClass1)]
    Variant1 { number: i32 },
    #[class(me.test.Test$SumClass$SumClass2)]
    Variant2 { str: String },
}

#[test]
fn from_object() { run_with_jnienv(|| {
    const VAL: i32 = 3;
    const ARRAY_VAL: &[&'static str] = &["Hello", "World"];
    let mut object = new!(me.test.Test(int(VAL)));

    assert_eq!(
        MyClass::from_object(&object).unwrap()
            .member_field,
        VAL
    );
    assert_eq!(
        MyClass1::from_object(&object).unwrap()
            .member,
        VAL
    );
    assert_eq!(
        MyClass1::from_object(&object).unwrap()
            .array_field
            .iter()
            .map(|s| s.as_str())
            .collect::<Box<[_]>>()
            .as_ref(),
        ARRAY_VAL
    );
    let int = MyClass2::from_object(&object).unwrap().member;
    assert_eq!(call!(int.intValue() -> int), VAL);
    assert_eq!(
        MyClass2::from_object(&object).unwrap()
            .array_field
            .iter()
            .map(|s| s.as_ref().unwrap())
            .collect::<Box<[_]>>()
            .as_ref(),
        ARRAY_VAL
    );
    assert_eq!(
        MyClass3::from_object(&object).unwrap()
            .member,
        VAL
    );

    object = new!(me.test.Test$SumClass$SumClass1(int(VAL)));
    assert_eq!(
        MyEnumClass::from_object(&object).unwrap(),
        MyEnumClass::Variant1 { number: VAL }
    );

    const S: &str = "Hello, World!";
    object = new!(me.test.Test$SumClass$SumClass2(String(S)));
    assert_eq!(
        MyEnumClass::from_object(&object).unwrap(),
        MyEnumClass::Variant2 { str: S.to_string() }
    );

    // -- Other implementations for user-defined types
    // Option
    object = Some(new!(me.test.Test(int(VAL)))).to_object();
    assert_eq!(
        Option::<MyClass>::from_object(&object).unwrap(),
        Some(MyClass { member_field: VAL })
    );
    // Array
    object = create_object_array_converted(
        &[new!(me.test.Test(int(1))), new!(me.test.Test(int(2))), new!(me.test.Test(int(3)))],
        MyClass::CLASS_PATH,
        |obj, env| obj.to_object_env(env),
    ez_jni::utils::get_env());
    assert_eq!(
        Box::<[MyClass]>::from_object(&object).unwrap(),
        [MyClass { member_field: 1 }, MyClass { member_field: 2 }, MyClass { member_field:3 }]
    );
    // Array Option
    object = create_object_array_converted(
        &[None, Some(new!(me.test.Test(int(1)))), None],
        MyClass::CLASS_PATH,
        |obj, env| obj.to_object_env(env),
    ez_jni::utils::get_env());
    assert_eq!(
        Box::<[Option<MyClass>]>::from_object(&object).unwrap(),
        [None, Some(MyClass { member_field: 1 }), None]
    );
    // Option Array
    object = Some(create_object_array_converted(
        &[new!(me.test.Test(int(1))), new!(me.test.Test(int(2))), new!(me.test.Test(int(3)))],
        MyClass::CLASS_PATH,
        |obj, env| obj.to_object_env(env),
    ez_jni::utils::get_env())).to_object();
    assert_eq!(
        Option::<Box<[MyClass]>>::from_object(&object).unwrap(),
        Some([MyClass { member_field: 1 }, MyClass { member_field: 2 }, MyClass { member_field:3 }])
    );
    // Multidimensional Array
    object = create_object_array_converted(
        &[None, Some(new!(me.test.Test(int(1)))), None],
        &format!("[L{};", MyClass::CLASS_PATH),
        |obj, env| obj.to_object_env(env),
    ez_jni::utils::get_env());
    assert_eq!(
        Box::<[Box<[MyClass]>]>::from_object(&object).unwrap(),
        [None, Some(MyClass { member_field: 1 }), None]
    );
}) }

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
struct Exception(#[field(call = getMessage, class = java.lang.String)] String);
impl Exception {
    fn message(&self) -> &str {
        &self.0
    }
}

#[test]
fn from_exception() { run_with_jnienv(|| {
    assert_eq!(
        call!(static me.test.Test.throwObj() -> Result<java.lang.Object, MyErr1>)
            .unwrap_err()
            .message,
        "exception"
    );
    assert_eq!(
        call!(static me.test.Test.throwObj() -> Result<java.lang.Object, MyErr2>)
            .unwrap_err()
            .msg,
        "exception"
    );
    assert_eq!(
        call!(static me.test.Test.throwObj() -> Result<java.lang.Object, Exception>)
            .unwrap_err()
            .message(),
        "exception"
    );
}) }
