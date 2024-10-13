mod common;

use jni::objects::JObject;
use ez_jni::{call, new};

#[test]
fn return_primitives() {
    setup_env!(env);
    // Java primitive return types
    let _: () = call!(static me.test.Test.getVoid() -> void);
    let _: bool = call!(static me.test.Test.getBoolean() -> boolean);
    let _: char = call!(static me.test.Test.getChar() -> char);
    let _: i8 = call!(static me.test.Test.getByte() -> byte);
    let _: i16 = call!(static me.test.Test.getShort() -> short);
    let _: i32 = call!(static me.test.Test.getInt() -> int);
    let _: i64 = call!(static me.test.Test.getLong() -> long);
    let _: f32 = call!(static me.test.Test.getFloat() -> float);
    let _: f64 = call!(static me.test.Test.getDouble() -> double);
    // Rust primitive return types
    let _: bool = call!(static me.test.Test.getBoolean() -> bool);
    let _: char = call!(static me.test.Test.getChar() -> char);
    let _: u8 = call!(static me.test.Test.getByte() -> u8);
    let _: u16 = call!(static me.test.Test.getShort() -> u16);
    let _: u32 = call!(static me.test.Test.getInt() -> u32);
    let _: u64 = call!(static me.test.Test.getLong() -> u64);
    let _: i8 = call!(static me.test.Test.getByte() -> i8);
    let _: i16 = call!(static me.test.Test.getShort() -> i16);
    let _: i32 = call!(static me.test.Test.getInt() -> i32);
    let _: i64 = call!(static me.test.Test.getLong() -> i64);
    let _: f32 = call!(static me.test.Test.getFloat() -> f32);
    let _: f64 = call!(static me.test.Test.getDouble() -> f64);
}

#[test]
fn return_other() {
    setup_env!(env);
    // Object
    let _: JObject = call!(static me.test.Test.getObject() -> java.lang.Object);
    let _: String = call!(static me.test.Test.getString() -> java.lang.String);
    let _: String = call!(static me.test.Test.getString() -> String);
    // Result Primitive
    let r: Result<bool, String> = call!(static me.test.Test.getBoolean() -> Result<bool, String>);
    r.unwrap();
    let r: Result<bool, String> = call!(static me.test.Test.throwPrim() -> Result<bool, String>);
    r.unwrap_err();
    // Result Object
    let r: Result<JObject, String> = call!(static me.test.Test.getObject() -> Result<java.lang.Object, String>);
    r.unwrap();
    let r: Result<JObject, String> = call!(static me.test.Test.throwObj() -> Result<java.lang.Object, String>);
    r.unwrap_err();
    // Option
    let r: Option<JObject> = call!(static me.test.Test.getObject() -> Option<java.lang.Object>);
    r.unwrap();
    let r: Option<JObject> = call!(static me.test.Test.nullable() -> Option<java.lang.Object>);
    assert!(r.is_none());
    // Result<Option<_>, _>
    let r: Result<Option<JObject>, String> = call!(static me.test.Test.getObject() -> Result<Option<java.lang.Object>, String>);
    r.unwrap().unwrap();
    let r: Result<Option<JObject>, String> = call!(static me.test.Test.nullable() -> Result<Option<java.lang.Object>, String>);
    assert!(r.unwrap().is_none());
}

#[test]
fn return_arrays() {
    setup_env!(env);
    // Java primitives
    let _: Box<[bool]> = call!(static me.test.Test.getBooleanArray() -> [boolean]);
    let _: Box<[char]> = call!(static me.test.Test.getCharArray() -> [char]);
    let _: Box<[i8]> = call!(static me.test.Test.getByteArray() -> [byte]);
    let _: Box<[i16]> = call!(static me.test.Test.getShortArray() -> [short]);
    let _: Box<[i32]> = call!(static me.test.Test.getIntArray() -> [int]);
    let _: Box<[i64]> = call!(static me.test.Test.getLongArray() -> [long]);
    let _: Box<[f32]> = call!(static me.test.Test.getFloatArray() -> [float]);
    let _: Box<[f64]> = call!(static me.test.Test.getDoubleArray() -> [double]);
    // Rust primitives
    let _: Box<[bool]> = call!(static me.test.Test.getBooleanArray() -> [bool]);
    let _: Box<[char]> = call!(static me.test.Test.getCharArray() -> [char]);
    let _: Box<[u8]> = call!(static me.test.Test.getByteArray() -> [u8]);
    let _: Box<[u16]> = call!(static me.test.Test.getShortArray() -> [u16]);
    let _: Box<[u32]> = call!(static me.test.Test.getIntArray() -> [u32]);
    let _: Box<[u64]> = call!(static me.test.Test.getLongArray() -> [u64]);
    let _: Box<[i8]> = call!(static me.test.Test.getByteArray() -> [i8]);
    let _: Box<[i16]> = call!(static me.test.Test.getShortArray() -> [i16]);
    let _: Box<[i32]> = call!(static me.test.Test.getIntArray() -> [i32]);
    let _: Box<[i64]> = call!(static me.test.Test.getLongArray() -> [i64]);
    let _: Box<[f32]> = call!(static me.test.Test.getFloatArray() -> [f32]);
    let _: Box<[f64]> = call!(static me.test.Test.getDoubleArray() -> [f64]);
}

#[test]
fn return_arrays_other() {
    setup_env!(env);
    // Object
    let _: Box<[JObject]> = call!(static me.test.Test.getObjectArray() -> [java.lang.Object]);
    let _: Box<[String]> = call!(static me.test.Test.getStringArray() -> [java.lang.String]);
    let _: Box<[String]> = call!(static me.test.Test.getStringArray() -> [String]);
    // Result Primitive
    let r: Result<Box<[bool]>, String> = call!(static me.test.Test.getBooleanArray() -> Result<[bool], String>);
    r.unwrap();
    let r: Result<Box<[bool]>, String> = call!(static me.test.Test.throwPrimArray() -> Result<[bool], String>);
    r.unwrap_err();
    // Result Object
    let r: Result<Box<[JObject]>, String> = call!(static me.test.Test.getObjectArray() -> Result<[java.lang.Object], String>);
    r.unwrap();
    let r: Result<Box<[JObject]>, String> = call!(static me.test.Test.throwObjArray() -> Result<[java.lang.Object], String>);
    r.unwrap_err();
    // Option
    let r: Option<Box<[bool]>> = call!(static me.test.Test.getBooleanArray() -> Option<[bool]>);
    r.unwrap();
    let r: Option<Box<[bool]>> = call!(static me.test.Test.nullPrimArray() -> Option<[bool]>);
    assert!(r.is_none());
    let r: Option<Box<[JObject]>> = call!(static me.test.Test.getObjectArray() -> Option<[java.lang.Object]>);
    r.unwrap();
    let r: Option<Box<[JObject]>> = call!(static me.test.Test.nullObjArray() -> Option<[java.lang.Object]>);
    assert!(r.is_none());
    // Result<Option<_>, _>
    let r: Result<Option<Box<[bool]>>, String> = call!(static me.test.Test.getBooleanArray() -> Result<Option<[bool]>, String>);
    r.unwrap().unwrap();
    let r: Result<Option<Box<[bool]>>, String> = call!(static me.test.Test.nullPrimArray() -> Result<Option<[bool]>, String>);
    assert!(r.unwrap().is_none());
    let r: Result<Option<Box<[JObject]>>, String> = call!(static me.test.Test.getObjectArray() -> Result<Option<[java.lang.Object]>, String>);
    r.unwrap().unwrap();
    let r: Result<Option<Box<[JObject]>>, String> = call!(static me.test.Test.nullObjArray() -> Result<Option<[java.lang.Object]>, String>);
    assert!(r.unwrap().is_none());
}

#[test]
fn arguments() {
    setup_env!(env);
    // -- Non-Array Arguments
    call!(static me.test.Test.primArgs(boolean(true), char('a'), byte(1i8), short(1i16), int(1i32), long(1i64), float(1f32), double(1f64)) -> void);
    call!(static me.test.Test.objArgs(java.lang.Object(new!(java.lang.Object())), java.lang.String("hi")) -> void);
    call!(static me.test.Test.objArgs(java.lang.Object(JObject::null()), java.lang.String(String::from("hi"))) -> void);
    call!(static me.test.Test.objArgs(java.lang.Object(null), java.lang.String(null)) -> void);
    // -- Primitive Array Arguments
    // Rust slices stored in variables
    // Also tests types that can be coerced to slice
    let z = Box::new([true, false]);
    let c = vec!['a', 'b'];
    let b = &[1i8, 2];
    let s = [1i16, 2];
    let i = [1i32, 2];
    let j = [1i64, 2];
    let f = [1f32, 2.0];
    let d = [1f64, 2.0];
    call!(static me.test.Test.primArrayArgs([boolean](&z), [char](c), [byte](b), [short](s), [int](i), [long](j), [float](f), [double](d)) -> void);
    // Array literals
    call!(static me.test.Test.primArrayArgs(
        [boolean]([true, false]),
        [char](['a', 'b']),
        [byte]([1i8, 2]),
        [short]([1i16, 2]),
        [int]([1i32, 2]),
        [long]([1i64, 2]),
        [float]([1f32, 2.0]),
        [double]([1f64, 2.0]),
    ) -> void);
    // Arrays with Rust primitive as Inner type
    call!(static me.test.Test.primArrayArgs(
        [bool]([0 != 0, 1 != 0]),
        [char](['a', 'b']),
        [i8]([i8::MIN, i8::MAX]),
        [i16]([i16::MIN, i16::MAX]),
        [i32]([i32::MIN, i32::MAX]),
        [i64]([i64::MIN, i64::MAX]),
        [f32]([f32::MIN, f32::MAX]),
        [f64]([f64::MIN, f64::MAX]),
    ) -> void);
    // Arrays with unsigned Rust integers
    call!(static me.test.Test.primArrayArgs(
        [bool]([true, false]),
        [char](['a', 'b']),
        [u8]([1u8, 2]),
        [u16]([1u16, 2]),
        [u32]([1u32, 2]),
        [u64]([1u64, 2]),
        [f32]([1f32, 2.0]),
        [f64]([1f64, 2.0]),
    ) -> void);

    // -- Object Array Arguments
    // Rust slices stored in variables
    let l = [new!(java.lang.Object()), JObject::null()];
    let s = ["Hello", "World"];
    call!(static me.test.Test.objArrayArgs([java.lang.Object](l), [java.lang.String](s)) -> void);
    // Array literals
    call!(static me.test.Test.objArrayArgs(
        [java.lang.Object]([new!(java.lang.Object()), JObject::null()]),
        [java.lang.String](["Hello", "World"])
    ) -> void);
    // Empty Array literal
    call!(static me.test.Test.objArrayArgs(
        [java.lang.Object](&[] as &[JObject]),
        [String]([] as [&str;0])
    ) -> void);
    // Null keyword
    call!(static me.test.Test.objArrayArgs(
        [java.lang.Object]([new!(java.lang.Object()), null]),
        [java.lang.String](["Hello", null])
    ) -> void);
    //Optional Strings
    let s = [Some("Hello"), None];
    call!(static me.test.Test.objArrayArgs([java.lang.Object]([null, null]), [java.lang.String](s)) -> void);
}

#[test]
fn constructor() {
    setup_env!(env);
    
    new!(me.test.Test());
    new!(me.test.Test(int(3)));
    new!(me.test.Test(java.lang.String("Hello, World!")));
    new!(me.test.Test(java.lang.String("Hello, World!")) throws String).unwrap();
    new!(me.test.Test(java.lang.String(null)) throws String).unwrap_err();

    // Test other class
    new!(java.lang.Object());
}
#[test]
#[should_panic]
fn constructor_fail() {
    setup_env!(env);
    // Should panic if theconstructor throws, but user did not indicate that the constructor could throw
    new!(me.test.Test(java.lang.String(null)));
}

#[test]
fn obj_method() {
    setup_env!(env);

    let obj: JObject<'_> = new!(me.test.Test$Instanced());
    call!(obj.getBoolean() -> boolean);
    call!(obj.getObject() -> java.lang.Object);
    call!(obj.args(boolean(true), java.lang.Object(JObject::null())) -> void);
    call!(obj.arrayArgs([boolean]([true, false]), [java.lang.Object]([JObject::null()])) -> void);
    // Test object expression
    call!((obj).getBoolean() -> boolean);
    call!({ obj }.getBoolean() -> boolean);
}