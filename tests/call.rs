mod common;

use std::panic::{catch_unwind, AssertUnwindSafe};

use jni::objects::JObject;
use ez_jni::{call, new, println, eprintln};

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
    let r: bool = call!(static me.test.Test.getBoolean() -> bool);
    assert_eq!(r, true);
    let r: char = call!(static me.test.Test.getChar() -> char);
    assert_eq!(r, 'a');
    let r: u8 = call!(static me.test.Test.getByte() -> u8);
    assert_eq!(r, 3);
    let r: u16 = call!(static me.test.Test.getShort() -> u16);
    assert_eq!(r, 3);
    let r: u32 = call!(static me.test.Test.getInt() -> u32);
    assert_eq!(r, 3);
    let r: u64 = call!(static me.test.Test.getLong() -> u64);
    assert_eq!(r, 3);
    let r: i8 = call!(static me.test.Test.getByte() -> i8);
    assert_eq!(r, 3);
    let r: i16 = call!(static me.test.Test.getShort() -> i16);
    assert_eq!(r, 3);
    let r: i32 = call!(static me.test.Test.getInt() -> i32);
    assert_eq!(r, 3);
    let r: i64 = call!(static me.test.Test.getLong() -> i64);
    assert_eq!(r, 3);
    let r: f32 = call!(static me.test.Test.getFloat() -> f32);
    assert_eq!(r, 3.3);
    let r: f64 = call!(static me.test.Test.getDouble() -> f64);
    assert_eq!(r, 3.3);
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
    let r: Box<[bool]> = call!(static me.test.Test.getBooleanArray() -> [bool]);
    assert_eq!(r.as_ref(), &[true, false]);
    let r: Box<[char]> = call!(static me.test.Test.getCharArray() -> [char]);
    assert_eq!(r.as_ref(), &['a', 'b', 'c']);
    let r: Box<[u8]> = call!(static me.test.Test.getByteArray() -> [u8]);
    assert_eq!(r.as_ref(), &[1, 2, 3]);
    let r: Box<[u16]> = call!(static me.test.Test.getShortArray() -> [u16]);
    assert_eq!(r.as_ref(), &[1, 2, 3]);
    let r: Box<[u32]> = call!(static me.test.Test.getIntArray() -> [u32]);
    assert_eq!(r.as_ref(), &[1, 2, 3]);
    let r: Box<[u64]> = call!(static me.test.Test.getLongArray() -> [u64]);
    assert_eq!(r.as_ref(), &[1, 2, 3]);
    let r: Box<[i8]> = call!(static me.test.Test.getByteArray() -> [i8]);
    assert_eq!(r.as_ref(), &[1, 2, 3]);
    let r: Box<[i16]> = call!(static me.test.Test.getShortArray() -> [i16]);
    assert_eq!(r.as_ref(), &[1, 2, 3]);
    let r: Box<[i32]> = call!(static me.test.Test.getIntArray() -> [i32]);
    assert_eq!(r.as_ref(), &[1, 2, 3]);
    let r: Box<[i64]> = call!(static me.test.Test.getLongArray() -> [i64]);
    assert_eq!(r.as_ref(), &[1, 2, 3]);
    let r: Box<[f32]> = call!(static me.test.Test.getFloatArray() -> [f32]);
    assert_eq!(r.as_ref(), &[1.1, 2.2, 3.3]);
    let r: Box<[f64]> = call!(static me.test.Test.getDoubleArray() -> [f64]);
    assert_eq!(r.as_ref(), &[1.1, 2.2, 3.3]);
    // Multi-Dimensional Array
    let r: Box<[Box<[i32]>]> = call!(static me.test.Test.get2DIntArray() -> [[int]]);
    let expect: Box<[Box<[i32]>]> = Box::new([Box::new([1, 2]), Box::new([3, 4])]);
    assert_eq!(r, expect);
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
    // Array Option
    let r: Box<[Option<String>]> = call!(static me.test.Test.getStringArray() -> [Option<String>]);
    let expect: Box<[Option<String>]> = Box::new(["Hello", "World"].map(|s| Some(s.to_string())));
    assert_eq!(r, expect);
    let r: Box<[Option<String>]> = call!(static me.test.Test.getNullStringArray() -> [Option<String>]);
    let expect: Box<[Option<String>]> = Box::new([Some("Hello"), None].map(|s| s.map(|s| s.to_string())));
    assert_eq!(r, expect);
    let r: Box<[Option<JObject>]> = call!(static me.test.Test.getObjectArray() -> [Option<java.lang.Object>]);
    r.into_vec().into_iter().for_each(|s| { s.unwrap(); });
    let r: Option<Box<[Option<String>]>> = call!(static me.test.Test.getStringArray() -> Option<[Option<String>]>);
    r.unwrap().into_vec().into_iter().for_each(|s| { s.unwrap(); });
    let r: Option<Box<[Option<JObject>]>> = call!(static me.test.Test.getObjectArray() -> Option<[Option<java.lang.Object>]>);
    r.unwrap().into_vec().into_iter().for_each(|s| { s.unwrap(); });
    // Multi-Dimensional Arrays
    let r: Box<[Box<[String]>]> = call!(static me.test.Test.get2DStringArray() -> [[String]]);
    let expect: Box<[Box<[String]>]> = Box::new([Box::new(["Hello", "World"].map(|s| s.to_string())), Box::new(["How", "are", "you"].map(|s| s.to_string()))]);
    assert_eq!(r, expect);
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
fn return_fail() {
    setup_env!(env);
    catch_unwind(AssertUnwindSafe(|| call!(static me.test.Test.nullable() -> java.lang.Object)))
        .unwrap_err();
    catch_unwind(AssertUnwindSafe(|| call!(static me.test.Test.nullObjArray() -> [java.lang.Object])))
        .unwrap_err();
    catch_unwind(AssertUnwindSafe(|| call!(static me.test.Test.getNullObjectArray() -> [java.lang.Object])))
        .unwrap_err();
    catch_unwind(AssertUnwindSafe(|| call!(static me.test.Test.nullObjArray() -> [Option<java.lang.Object>])))
        .unwrap_err();
}

#[test]
fn arguments() {
    setup_env!(env);
    // -- Non-Array Arguments
    call!(static me.test.Test.primArgs(boolean(true), char('a'), byte(1i8), short(1i16), int(1i32), long(1i64), float(1f32), double(1f64)) -> void);
    call!(static me.test.Test.objArgs(java.lang.Object(new!(java.lang.Object())), java.lang.String("hi")) -> void);
    call!(static me.test.Test.objArgs(java.lang.Object(JObject::null()), java.lang.String(String::from("hi"))) -> void);
    // call!(static me.test.Test.objArgs(java.lang.Object(None), java.lang.String(Some("hi"))) -> void);
    call!(static me.test.Test.objArgs(java.lang.Object(null), java.lang.String(null)) -> void);
    // -- Primitive Array Arguments
    // Rust slices stored in variables
    // Also tests types that can be coerced to slice
    let z = vec![true, false].into_boxed_slice();
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
    // Optional Strings
    call!(static me.test.Test.objArrayArgs(
        [Option<java.lang.Object>]([Some(JObject::null()), None]),
        [Option<String>]([Some("Hello"), None])
    ) -> void);

    // -- Multidimensional Arrays
    call!(static me.test.Test.prim2DArrayArgs(
        [[bool]]([[true], [false]]),
        [[char]]([['a', 'b'], ['c', 'd']]),
        [[int]]([[1, 2], [3, 4]]),
    ) -> void);
}

#[test]
fn constructor() {
    setup_env!(env);
    
    new!(me.test.Test());
    new!(me.test.Test(int(3)));
    new!(me.test.Test(String("Hello, World!")));
    new!(me.test.Test(String("Hello, World!")) throws String).unwrap();
    new!(me.test.Test(String(null)) throws String).unwrap_err();

    let class = env.find_class("me/test/Test").unwrap();
    new!(class());
    new!(( class )(int(3)));
    new!({ class }(String("Hello, World!")));

    // Test other class
    new!(java.lang.Object());
}
#[test]
fn constructor_fail() {
    setup_env!(env);
    // Should panic if the constructor throws, but user did not indicate that the constructor could throw
    catch_unwind(AssertUnwindSafe(|| new!(me.test.Test(java.lang.String(null)))))
        .unwrap_err();
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

    // Test method on class Object
    let class = env.get_object_class(&obj).unwrap();
    assert_eq!(call!(class.getName() -> String), "me.test.Test$Instanced");
}

#[test]
fn print() {
    // This test will NOT run in the android environment, so it is impossible to test if the call to android.util.Log will be successful.
    println!("Hello, World!");
    eprintln!("Hello, World!");
}