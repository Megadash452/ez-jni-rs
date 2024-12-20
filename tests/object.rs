mod common;

use ez_jni::{call, new, FromException, FromObject, ToObject};
use jni::objects::JObject;

/// Tests the implementations of FromObject, etc. for *standard library* types.
#[test]
fn implementations() {
    setup_env!(env);
    static S: &str = "Hello, World!";
    static N: i8 = -3;
    static UN: i8 = 3;
    static F: f32 = -3.3;

    // Test String
    let mut obj = S.to_object(&mut env);
    let s = String::from_object(&obj, &mut env).unwrap();
    assert_eq!(s, S);

    // Testing Option
    obj = Some(S).to_object(&mut env);
    let s = Option::<String>::from_object(&obj, &mut env).unwrap();
    assert_eq!(s.unwrap(), S);

    assert_eq!(
        Option::<String>::from_object(&JObject::null(), &mut env).unwrap(),
        None
    );

    // Testing IO Error
    obj = new!(java.io.FileNotFoundException(java.lang.String(S)));
    let err = std::io::Error::from_object(&obj, &mut env).unwrap();
    assert_eq!(err.to_string(), S);

    // Testing Number types
    obj = (N as i8).to_object(&mut env);
    assert_eq!(N as i8, i8::from_object(&obj, &mut env).unwrap());
    obj = (N as i16).to_object(&mut env);
    assert_eq!(N as i16, i16::from_object(&obj, &mut env).unwrap());
    obj = (N as i32).to_object(&mut env);
    assert_eq!(N as i32, i32::from_object(&obj, &mut env).unwrap());
    obj = (N as i64).to_object(&mut env);
    assert_eq!(N as i64, i64::from_object(&obj, &mut env).unwrap());
    obj = (F as f32).to_object(&mut env);
    assert_eq!(F as f32, f32::from_object(&obj, &mut env).unwrap());
    obj = (F as f64).to_object(&mut env);
    assert_eq!(F as f64, f64::from_object(&obj, &mut env).unwrap());
    obj = (UN as u8).to_object(&mut env);
    assert_eq!(UN as u8, u8::from_object(&obj, &mut env).unwrap());
    obj = (UN as u16).to_object(&mut env);
    assert_eq!(UN as u16, u16::from_object(&obj, &mut env).unwrap());
    obj = (UN as u32).to_object(&mut env);
    assert_eq!(UN as u32, u32::from_object(&obj, &mut env).unwrap());
    obj = (UN as u64).to_object(&mut env);
    assert_eq!(UN as u64, u64::from_object(&obj, &mut env).unwrap());
    obj = true.to_object(&mut env);
    assert_eq!(true, bool::from_object(&obj, &mut env).unwrap());
    obj = 'a'.to_object(&mut env);
    assert_eq!('a', char::from_object(&obj, &mut env).unwrap());
    // Testing Array types
    // String Array
    obj = ["Hello", "World"].to_object(&mut env);
    assert_eq!(
        ["Hello", "World"],
        Box::<[String]>::from_object(&obj, &mut env)
            .unwrap()
            .as_ref()
    );
    obj = [Some("Hello"), None].to_object(&mut env);
    assert_eq!(
        [Some("Hello".to_string()), None],
        Box::<[Option<String>]>::from_object(&obj, &mut env)
            .unwrap()
            .as_ref()
    );
    // Primitives Arrays
    obj = [1i8, 2, 3].to_object(&mut env);
    assert_eq!(
        [1, 2, 3],
        Box::<[i8]>::from_object(&obj, &mut env).unwrap().as_ref()
    );
    obj = [1i16, 2, 3].to_object(&mut env);
    assert_eq!(
        [1, 2, 3],
        Box::<[i16]>::from_object(&obj, &mut env).unwrap().as_ref()
    );
    obj = [1i32, 2, 3].to_object(&mut env);
    assert_eq!(
        [1, 2, 3],
        Box::<[i32]>::from_object(&obj, &mut env).unwrap().as_ref()
    );
    obj = [1i64, 2, 3].to_object(&mut env);
    assert_eq!(
        [1, 2, 3],
        Box::<[i64]>::from_object(&obj, &mut env).unwrap().as_ref()
    );
    obj = [1.1f32, 2.2, 3.3].to_object(&mut env);
    assert_eq!(
        [1.1f32, 2.2, 3.3],
        Box::<[f32]>::from_object(&obj, &mut env).unwrap().as_ref()
    );
    obj = [1.1f64, 2.2, 3.3].to_object(&mut env);
    assert_eq!(
        [1.1f64, 2.2, 3.3],
        Box::<[f64]>::from_object(&obj, &mut env).unwrap().as_ref()
    );
    obj = [1u8, 2, 3].to_object(&mut env);
    assert_eq!(
        [1, 2, 3],
        Box::<[u8]>::from_object(&obj, &mut env).unwrap().as_ref()
    );
    obj = [1u16, 2, 3].to_object(&mut env);
    assert_eq!(
        [1, 2, 3],
        Box::<[u16]>::from_object(&obj, &mut env).unwrap().as_ref()
    );
    obj = [1u32, 2, 3].to_object(&mut env);
    assert_eq!(
        [1, 2, 3],
        Box::<[u32]>::from_object(&obj, &mut env).unwrap().as_ref()
    );
    obj = [1u64, 2, 3].to_object(&mut env);
    assert_eq!(
        [1, 2, 3],
        Box::<[u64]>::from_object(&obj, &mut env).unwrap().as_ref()
    );
    obj = [true, false].to_object(&mut env);
    assert_eq!(
        [true, false],
        Box::<[bool]>::from_object(&obj, &mut env).unwrap().as_ref()
    );
    obj = ['a', 'b', 'c'].to_object(&mut env);
    assert_eq!(
        ['a', 'b', 'c'],
        Box::<[char]>::from_object(&obj, &mut env).unwrap().as_ref()
    );
}

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
fn from_object() {
    setup_env!(env);
    const VAL: i32 = 3;
    let mut object = new!(me.test.Test(int(VAL)));

    assert_eq!(
        MyClass::from_object(&object, &mut env)
            .unwrap()
            .member_field,
        VAL
    );
    assert_eq!(
        MyClass1::from_object(&object, &mut env)
            .unwrap()
            .member,
        VAL
    );
    let int = MyClass2::from_object(&object, &mut env).unwrap().member;
    assert_eq!(call!(int.intValue() -> int), VAL);
    assert_eq!(
        MyClass3::from_object(&object, &mut env)
            .unwrap()
            .member,
        VAL
    );

    object = new!(me.test.Test$SumClass$SumClass1(int(VAL)));
    assert_eq!(
        MyEnumClass::from_object(&object, &mut env).unwrap(),
        MyEnumClass::Variant1 { number: VAL }
    );

    const S: &str = "Hello, World!";
    object = new!(me.test.Test$SumClass$SumClass2(java.lang.String(S)));
    assert_eq!(
        MyEnumClass::from_object(&object, &mut env).unwrap(),
        MyEnumClass::Variant2 { str: S.to_string() }
    );
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
struct Exception(#[field(call = getMessage, class = java.lang.String)] String);
impl Exception {
    fn message(&self) -> &str {
        &self.0
    }
}

#[test]
fn from_exception() {
    setup_env!(env);
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
}
