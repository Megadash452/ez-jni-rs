mod common;

use common::run_with_jnienv;
use ez_jni::{call, class, eprintln, field, new, println, singleton, FromObjectError, JavaException};
use jni::objects::{JClass, JObject, JString, JThrowable};

use crate::common::fail_with;

#[test]
fn return_primitives() { run_with_jnienv(|| {
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
    // Return primitive Object
    let r: bool = call!(static me.test.Test.getBooleanObj() -> Boolean);
    assert_eq!(r, true);
    let r: char = call!(static me.test.Test.getCharObj() -> Character);
    assert_eq!(r, 'a');
    let r: i8 = call!(static me.test.Test.getByteObj() -> Byte);
    assert_eq!(r, 3);
    let r: i16 = call!(static me.test.Test.getShortObj() -> Short);
    assert_eq!(r, 3);
    let r: i32 = call!(static me.test.Test.getIntObj() -> Integer);
    assert_eq!(r, 3);
    let r: i64 = call!(static me.test.Test.getLongObj() -> Long);
    assert_eq!(r, 3);
    let r: f32 = call!(static me.test.Test.getFloatObj() -> Float);
    assert_eq!(r, 3.3);
    let r: f64 = call!(static me.test.Test.getDoubleObj() -> Double);
    assert_eq!(r, 3.3);
    // Return primitive Object with option
    let r: Option<bool> = call!(static me.test.Test.getBooleanObj() -> Option<bool>);
    assert_eq!(r, Some(true));
    let r: Option<char> = call!(static me.test.Test.getCharObj() -> Option<char>);
    assert_eq!(r, Some('a'));
    let r: Option<u8> = call!(static me.test.Test.getByteObj() -> Option<u8>);
    assert_eq!(r, Some(3));
    let r: Option<u16> = call!(static me.test.Test.getShortObj() -> Option<u16>);
    assert_eq!(r, Some(3));
    let r: Option<u32> = call!(static me.test.Test.getIntObj() -> Option<u32>);
    assert_eq!(r, Some(3));
    let r: Option<u64> = call!(static me.test.Test.getLongObj() -> Option<u64>);
    assert_eq!(r, Some(3));
    let r: Option<i8> = call!(static me.test.Test.getByteObj() -> Option<i8>);
    assert_eq!(r, Some(3));
    let r: Option<i16> = call!(static me.test.Test.getShortObj() -> Option<i16>);
    assert_eq!(r, Some(3));
    let r: Option<i32> = call!(static me.test.Test.getIntObj() -> Option<i32>);
    assert_eq!(r, Some(3));
    let r: Option<i64> = call!(static me.test.Test.getLongObj() -> Option<i64>);
    assert_eq!(r, Some(3));
    let r: Option<f32> = call!(static me.test.Test.getFloatObj() -> Option<f32>);
    assert_eq!(r, Some(3.3));
    let r: Option<f64> = call!(static me.test.Test.getDoubleObj() -> Option<f64>);
    assert_eq!(r, Some(3.3));
    let r: Option<bool> = call!(static me.test.Test.getNullPrim() -> Option<bool>);
    assert!(r.is_none());
}) }

#[test]
fn return_object() { run_with_jnienv(|| {
    // Object
    let _: JObject = call!(static me.test.Test.getObject() -> java.lang.Object);
    let _: JString = call!(static me.test.Test.getString() -> java.lang.String);
    let _: JClass = call!(static me.test.Test.getMyClass() -> Class);
    let _: JThrowable = call!(static me.test.Test.getException() -> Exception);
    let _: String = call!(static me.test.Test.getString() -> String);
    // Result Primitive
    let r: Result<bool, JavaException> = call!(static me.test.Test.getBoolean() -> Result<bool, Exception>);
    r.unwrap();
    let r: Result<bool, JavaException> = call!(static me.test.Test.throwPrim() -> Result<bool, Exception>);
    r.unwrap_err();
    // Result Object
    let r: Result<JObject, JavaException> = call!(static me.test.Test.getObject() -> Result<java.lang.Object, Exception>);
    r.unwrap();
    let r: Result<JObject, JavaException> = call!(static me.test.Test.throwObj() -> Result<java.lang.Object, Exception>);
    r.unwrap_err();
    // Option
    let r: Option<JObject> = call!(static me.test.Test.getObject() -> Option<java.lang.Object>);
    r.unwrap();
    let r: Option<JObject> = call!(static me.test.Test.nullable() -> Option<java.lang.Object>);
    assert!(r.is_none());
    // Result<Option<_>, _>
    let r: Result<Option<JObject>, JavaException> = call!(static me.test.Test.getObject() -> Result<Option<java.lang.Object>, Exception>);
    r.unwrap().unwrap();
    let r: Result<Option<JObject>, JavaException> = call!(static me.test.Test.nullable() -> Result<Option<java.lang.Object>, Exception>);
    assert!(r.unwrap().is_none());
}) }

#[test]
fn return_arrays() { run_with_jnienv(|| {
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
    // 2 Dimensional
    let r: Box<[Box<[i32]>]> = call!(static me.test.Test.get2DIntArray() -> [[int]]);
    let expect: Box<[Box<[i32]>]> = Box::new([Box::new([1, 2]), Box::new([3, 4])]);
    assert_eq!(r, expect);
    // 3 Dimensional
    let r: Box<[Box<[Box<[i32]>]>]> = call!(static me.test.Test.get3DIntArray() -> [[[int]]]);
    let expect: Box<[Box<[Box<[i32]>]>]> = Box::new([Box::new([Box::new([1, 2]), Box::new([3, 4])]), Box::new([Box::new([5, 6]), Box::new([7, 8])])]);
    assert_eq!(r, expect);
    // Primitive Object Array with option
    let r: Box<[Option<bool>]> = call!(static me.test.Test.getBooleanObjArray() -> [Option<bool>]);
    assert_eq!(r.as_ref(), &[Some(true), None]);
    let r: Box<[Option<char>]> = call!(static me.test.Test.getCharObjArray() -> [Option<char>]);
    assert_eq!(r.as_ref(), &[Some('a'), None]);
    let r: Box<[Option<u8>]> = call!(static me.test.Test.getByteObjArray() -> [Option<u8>]);
    assert_eq!(r.as_ref(), &[Some(1), None]);
    let r: Box<[Option<u16>]> = call!(static me.test.Test.getShortObjArray() -> [Option<u16>]);
    assert_eq!(r.as_ref(), &[Some(1), None]);
    let r: Box<[Option<u32>]> = call!(static me.test.Test.getIntObjArray() -> [Option<u32>]);
    assert_eq!(r.as_ref(), &[Some(1), None]);
    let r: Box<[Option<u64>]> = call!(static me.test.Test.getLongObjArray() -> [Option<u64>]);
    assert_eq!(r.as_ref(), &[Some(1), None]);
    let r: Box<[Option<i8>]> = call!(static me.test.Test.getByteObjArray() -> [Option<i8>]);
    assert_eq!(r.as_ref(), &[Some(1), None]);
    let r: Box<[Option<i16>]> = call!(static me.test.Test.getShortObjArray() -> [Option<i16>]);
    assert_eq!(r.as_ref(), &[Some(1), None]);
    let r: Box<[Option<i32>]> = call!(static me.test.Test.getIntObjArray() -> [Option<i32>]);
    assert_eq!(r.as_ref(), &[Some(1), None]);
    let r: Box<[Option<i64>]> = call!(static me.test.Test.getLongObjArray() -> [Option<i64>]);
    assert_eq!(r.as_ref(), &[Some(1), None]);
    let r: Box<[Option<f32>]> = call!(static me.test.Test.getFloatObjArray() -> [Option<f32>]);
    assert_eq!(r.as_ref(), &[Some(1.1), None]);
    let r: Box<[Option<f64>]> = call!(static me.test.Test.getDoubleObjArray() -> [Option<f64>]);
    assert_eq!(r.as_ref(), &[Some(1.1), None]);
}) }

#[test]
fn return_arrays_other() { run_with_jnienv(|| {
    // Object
    let _: Box<[JObject]> = call!(static me.test.Test.getObjectArray() -> [java.lang.Object]);
    let _: Box<[JString]> = call!(static me.test.Test.getStringArray() -> [java.lang.String]);
    let _: Box<[String]> = call!(static me.test.Test.getStringArray() -> [String]);
    // Result Primitive
    let r: Result<Box<[bool]>, JavaException> = call!(static me.test.Test.getBooleanArray() -> Result<[bool], java.lang.IndexOutOfBoundsException>);
    r.unwrap();
    let r: Result<Box<[bool]>, JavaException> = call!(static me.test.Test.throwPrimArray() -> Result<[bool], java.lang.IndexOutOfBoundsException>);
    r.unwrap_err();
    // Result Object
    let r: Result<Box<[JObject]>, JavaException> = call!(static me.test.Test.getObjectArray() -> Result<[java.lang.Object], java.lang.IndexOutOfBoundsException>);
    r.unwrap();
    let r: Result<Box<[JObject]>, JavaException> = call!(static me.test.Test.throwObjArray() -> Result<[java.lang.Object], java.lang.IndexOutOfBoundsException>);
    r.unwrap_err();
    // Option
    let r: Option<Box<[bool]>> = call!(static me.test.Test.getBooleanArray() -> Option<[bool]>);
    r.unwrap();
    let r: Option<Box<[bool]>> = call!(static me.test.Test.primNullArray() -> Option<[bool]>);
    assert!(r.is_none());
    let r: Option<Box<[JObject]>> = call!(static me.test.Test.getObjectArray() -> Option<[java.lang.Object]>);
    r.unwrap();
    let r: Option<Box<[JObject]>> = call!(static me.test.Test.objNullArray() -> Option<[java.lang.Object]>);
    assert!(r.is_none());
    // Array Option
    let r: Box<[Option<String>]> = call!(static me.test.Test.getStringArray() -> [Option<String>]);
    assert_eq!(r.as_ref(), ["Hello", "World"].map(|s| Some(s.to_string())));
    let r: Box<[Option<String>]> = call!(static me.test.Test.getNullStringArray() -> [Option<String>]);
    assert_eq!(r.as_ref(), [Some("Hello"), None].map(|s| s.map(|s| s.to_string())));
    let r: Box<[Option<JObject>]> = call!(static me.test.Test.getObjectArray() -> [Option<java.lang.Object>]);
    r.into_vec().into_iter().map(Option::unwrap).for_each(|_| {});
    let r: Option<Box<[Option<String>]>> = call!(static me.test.Test.getNullStringArray() -> Option<[Option<String>]>);
    assert_eq!(r.unwrap().as_ref(), &[Some("Hello"), None].map(|s| s.map(|s| s.to_string())));
    let r: Option<Box<[Option<JObject>]>> = call!(static me.test.Test.getObjectArray() -> Option<[Option<java.lang.Object>]>);
    r.unwrap().into_vec().into_iter().map(Option::unwrap).for_each(|_| {});

    // Multi-Dimensional Arrays
    let r: Box<[Box<[String]>]> = call!(static me.test.Test.get2DStringArray() -> [[String]]);
    let expect: Box<[Box<[String]>]> = Box::new([
        Box::new(["Hello", "World"].map(|s| s.to_string())),
        Box::new(["How", "are", "you"].map(|s| s.to_string())),
    ]);
    assert_eq!(r, expect);
    // Result<Option<_>, _>
    let r: Result<Option<Box<[bool]>>, JavaException> = call!(static me.test.Test.getBooleanArray() -> Result<Option<[bool]>, Exception>);
    r.unwrap().unwrap();
    let r: Result<Option<Box<[bool]>>, JavaException> = call!(static me.test.Test.primNullArray() -> Result<Option<[bool]>, Exception>);
    assert!(r.unwrap().is_none());
    let r: Result<Option<Box<[JObject]>>, JavaException> = call!(static me.test.Test.getObjectArray() -> Result<Option<[java.lang.Object]>, Exception>);
    r.unwrap().unwrap();
    let r: Result<Option<Box<[JObject]>>, JavaException> = call!(static me.test.Test.objNullArray() -> Result<Option<[java.lang.Object]>, Exception>);
    assert!(r.unwrap().is_none());
}) }

#[test]
fn return_fail() { run_with_jnienv(|| {
    // Returned Object should NOT be NULL
    fail_with(
        || { call!(static me.test.Test.nullable() -> java.lang.Object); },
        FromObjectError::Null.to_string().as_str()
    );
    // Returned primitive should NOT be NULL
    fail_with(
        || { call!(static me.test.Test.getNullPrim() -> Boolean); },
        FromObjectError::Null.to_string().as_str()
    );
    // Returned Array should NOT be NULL
    fail_with(
        || { call!(static me.test.Test.objNullArray() -> [java.lang.Object]); },
        FromObjectError::Null.to_string().as_str()
    );
    // Objects in returned Array should NOT be NULL
    fail_with(
        || { call!(static me.test.Test.getNullObjectArray() -> [java.lang.Object]); },
        FromObjectError::ArrayElement { index: 1, error: Box::new(FromObjectError::Null) }.to_string().as_str()
    );
    // Returned Array should NOT be NULL, even if its Objects can
    fail_with(
        || { call!(static me.test.Test.objNullArray() -> [Option<java.lang.Object>]); },
        FromObjectError::Null.to_string().as_str()
    );
    // Incorrect Error Class
    fail_with(
        || { call!(static me.test.Test.throwPrimArray() -> Result<[bool], java.lang.WrongException>).unwrap_err(); },
        FromObjectError::ClassNotFound("java/lang/WrongException".to_string()).to_string().as_str()
    );
}) }

#[test]
fn arguments() { run_with_jnienv(|| {
    // -- Non-Array Arguments
    call!(static me.test.Test.primArgs(boolean(true), char('a'), byte(1i8), short(1i16), int(1i32), long(1i64), float(1f32), double(1f64)) -> void);
    call!(static me.test.Test.objArgs(java.lang.Object(new!(java.lang.Object())), String("hi")) -> void);
    call!(static me.test.Test.objArgs(Object(JObject::null()), String("hi".to_string())) -> void);
    call!(static me.test.Test.otherArgs(Class(null), Exception(null)) -> void);
    // call!(static me.test.Test.objArgs(java.lang.Object(None), java.lang.String(Some("hi"))) -> void);
    call!(static me.test.Test.objArgs(java.lang.Object(null), java.lang.String(null)) -> void);
    call!(static me.test.Test.primObjArgs(
        Boolean(true),
        Character('a'),
        Byte(1i8),
        Short(1i16),
        Integer(1i32),
        Long(1i64),
        Float(1f32),
        Double(1f64)
    ) -> void);
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
    // Arrays with Option<primitive>
    call!(static me.test.Test.primObjArrayArgs(
        [Option<bool>]([Some(true), None]),
        [Option<char>]([Some('a'), None]),
        [Option<byte>]([Some(1i8), None]),
        [Option<short>]([Some(1i16), None]),
        [Option<int>]([Some(1i32), None]),
        [Option<long>]([Some(1i64), None]),
        [Option<float>]([Some(1f32), None]),
        [Option<double>]([Some(1f64), None]),
    ) -> void);
    // Object primitive Arrays with extreme values
    call!(static me.test.Test.primObjArrayArgs(
        [Boolean]([0 != 0, 1 != 0]),
        [Character](['a', 'b', 'ඞ']),
        [Byte]([i8::MIN, i8::MAX]),
        [Short]([i16::MIN, i16::MAX]),
        [Integer]([i32::MIN, i32::MAX]),
        [Long]([i64::MIN, i64::MAX]),
        [Float]([f32::MIN, f32::MAX, f32::INFINITY, f32::NEG_INFINITY, f32::NAN]),
        [Double]([f64::MIN, f64::MAX, f64::INFINITY, f64::NEG_INFINITY, f64::NAN]),
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
    call!(static me.test.Test.objArrayArgs([java.lang.Object](l), [String](s)) -> void);
    // Array literals
    call!(static me.test.Test.objArrayArgs(
        [java.lang.Object]([new!(java.lang.Object()), JObject::null()]),
        [String](["Hello", "World"])
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
    call!(static me.test.Test.prim3DArrayArgs(
        [[[int]]]([
            [[1, 2, 3], [4, 5, 6]],
            [[7, 8, 9], [10, 11, 12]]
        ]),
        [[[float]]]([
            [[1.0, 1.5, 2.0], [3.0, 3.5, 4.0]],
            [[5.0, 5.5, 6.0], [7.0, 7.5, 8.0]]
        ]),
    ) -> void);
}) }

#[test]
fn constructor() { run_with_jnienv(|| {
    new!(me.test.Test());
    new!(me.test.Test(int(3)));
    new!(me.test.Test(String("Hello, World!")));
    new!(me.test.Test(String("Hello, World!")) throws Exception).unwrap();
    new!(me.test.Test(String(null)) throws Exception).unwrap_err();

    let class = class!(me.test.Test);
    new!(class());
    new!((class)(int(3)));
    new!({ &class }(String("Hello, World!")));
    // Allow arbitrary Expressions as the Object
    new!(Some(Some(&class)).unwrap().unwrap()());
    // new!("me/test/Test"() throws String).unwrap(); Class lookups are not supported; user must do that manually

    // Test other class
    new!(java.lang.Object());
}) }
#[test]
fn constructor_fail() { run_with_jnienv(|| {
    // Should panic if the constructor throws, but user did not indicate that the constructor could throw
    fail_with(
        || { new!(me.test.Test(java.lang.String(null))); },
        "java.lang.NullPointerException: String was null"
    );
}) }

#[test]
fn obj_method() { run_with_jnienv(|| {
    let obj: JObject<'_> = new!(me.test.Test$Instanced());
    call!(obj.getBoolean() -> boolean);
    call!(obj.getObject() -> java.lang.Object);
    call!(obj.args(boolean(true), java.lang.Object(JObject::null())) -> void);
    call!(obj.arrayArgs([boolean]([true, false]), [java.lang.Object]([JObject::null()])) -> void);
    // Test object expression
    call!((obj).getBoolean() -> boolean);
    call!({ &obj }.getBoolean() -> boolean);
    // Allow arbitrary Expressions as the Object
    call!(Some(Some(&obj)).unwrap().unwrap().getBoolean() -> boolean);

    // Test method on class Object
    let class = call!(obj.getClass() -> Class);
    assert_eq!(call!(class.getName() -> String), "me.test.Test$Instanced");
}) }

#[test]
fn field() { run_with_jnienv(|| {
    assert_eq!(field!(static me.test.Test.member1: u32), 3);
    assert_eq!(field!(static me.test.Test.member2: String), "Hello, World!");
    assert_eq!(field!(static me.test.Test.member3: char), 'a');

    let class = class!(me.test.Test);
    assert_eq!(field!(static class.member1: u32), 3);

    let obj = new!(me.test.Test$Instanced());
    assert_eq!(field!(obj.member1: u32), 3);
    assert_eq!(field!(obj.member2: String), "Hello, World!");
    assert_eq!(field!(obj.member3: char), 'a');

    // Set field values
    field!(static me.test.Test.member1: u32 = 5);
    field!(static me.test.Test.member2: String = "Goodbye, World!");
    field!(static me.test.Test.member3: char = 'b');

    field!(obj.member1: u32 = 5);
    field!(obj.member2: String = "Goodbye, World!");
    field!(obj.member3: char = 'b');
}) }

#[test]
fn class() { run_with_jnienv(|| {
    let mut class: JClass<'_>;

    class = class!(me.test.Test);
    assert_eq!(call!(class.getName() -> String), "me.test.Test");
    class = class!(me.test.Test$Instanced);
    assert_eq!(call!(class.getName() -> String), "me.test.Test$Instanced");
}) }

#[test]
fn singleton() { run_with_jnienv(|| {
    let obj = singleton!(me.test.Test$Singleton);
    assert_eq!(call!(obj.method() -> int), 3);
    assert_eq!(field!(obj.member: int), 3);
}) }

#[test]
fn print() {
    // This test will NOT run in the android environment, so it is impossible to test if the call to android.util.Log will be successful.
    println!("Hello, World!");
    eprintln!("Hello, World!");
}
