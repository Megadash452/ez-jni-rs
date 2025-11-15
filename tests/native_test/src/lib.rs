use ez_jni::{FromObject as _, ObjectArray, call, jni_fn};
use jni::objects::{JClass, JObject};

jni_fn! { me.test.Native =>
    pub fn native_test_bool<'local>(b: bool)     -> bool   { let b: bool = b; assert_eq!(b, true); b }
    pub fn native_test_char<'local>(c: char)     -> char   { let c: char = c; assert_eq!(c, 'a'); c }
    pub fn native_test_byte<'local>(i: byte)     -> byte   { let i: i8 = i; assert_eq!(i, 3); i }
    pub fn native_test_short<'local>(i: short)   -> short  { let i: i16 = i; assert_eq!(i, 3); i }
    pub fn native_test_int<'local>(i: int)       -> int    { let i: i32 = i; assert_eq!(i, 3); i }
    pub fn native_test_long<'local>(i: long)     -> long   { let i: i64 = i; assert_eq!(i, 3); i }
    pub fn native_test_float<'local>(i: float)   -> float  { let i: f32 = i; assert_eq!(i, 3.3); i }
    pub fn native_test_double<'local>(i: double) -> double { let i: f64 = i; assert_eq!(i, 3.3); i }
    // pub fn native_test_ubyte<'local>(i: u8)   -> u8 { let i: u8 = i; assert_eq!(i, 3); i }
    // pub fn native_test_ushort<'local>(i: u16) -> u16 { let i: u16 = i; assert_eq!(i, 3); i }
    // pub fn native_test_uint<'local>(i: u32)   -> u32 { let i: u32 = i; assert_eq!(i, 3); i }
    // pub fn native_test_ulong<'local>(i: u64)  -> u64 { let i: u64 = i; assert_eq!(i, 3); i }
    pub fn native_test_bool_obj<'local>(b: Option<bool>)     -> Option<bool>   { let b: Option<bool> = b; assert_eq!(b, Some(true)); b }
    pub fn native_test_char_obj<'local>(c: Option<char>)     -> Option<char>   { let c: Option<char> = c; assert_eq!(c, Some('a')); c }
    pub fn native_test_byte_obj<'local>(i: Option<byte>)     -> Option<byte>   { let i: Option<i8> = i; assert_eq!(i, Some(3)); i }
    pub fn native_test_short_obj<'local>(i: Option<short>)   -> Option<short>  { let i: Option<i16> = i; assert_eq!(i, Some(3)); i }
    pub fn native_test_int_obj<'local>(i: Option<int>)       -> Option<int>    { let i: Option<i32> = i; assert_eq!(i, Some(3)); i }
    pub fn native_test_long_obj<'local>(i: Option<long>)     -> Option<long>   { let i: Option<i64> = i; assert_eq!(i, Some(3)); i }
    pub fn native_test_float_obj<'local>(i: Option<float>)   -> Option<float>  { let i: Option<f32> = i; assert_eq!(i, Some(3.3)); i }
    pub fn native_test_double_obj<'local>(i: Option<double>) -> Option<double> { let i: Option<f64> = i; assert_eq!(i, Some(3.3)); i }
}
jni_fn! { me.test.Native =>
    pub fn native_test_bool_array<'local>(b: [bool])     -> [bool]   { let b: Box<[bool]> = b; assert_eq!(b.as_ref(), &[true, false]); b }
    pub fn native_test_char_array<'local>(c: [char])     -> [char]   { let c: Box<[char]> = c; assert_eq!(c.as_ref(), &['a', 'b']); c }
    pub fn native_test_byte_array<'local>(i: [byte])     -> [byte]   { let i: Box<[i8]> = i; assert_eq!(i.as_ref(), &[1, 2, 3]); i }
    pub fn native_test_short_array<'local>(i: [short])   -> [short]  { let i: Box<[i16]> = i; assert_eq!(i.as_ref(), &[1, 2, 3]); i }
    pub fn native_test_int_array<'local>(i: [int])       -> [int]    { let i: Box<[i32]> = i; assert_eq!(i.as_ref(), &[1, 2, 3]); i }
    pub fn native_test_long_array<'local>(i: [long])     -> [long]   { let i: Box<[i64]> = i; assert_eq!(i.as_ref(), &[1, 2, 3]); i }
    pub fn native_test_float_array<'local>(i: [float])   -> [float]  { let i: Box<[f32]> = i; assert_eq!(i.as_ref(), &[1.1, 2.2, 3.3]); i }
    pub fn native_test_double_array<'local>(i: [double]) -> [double] { let i: Box<[f64]> = i; assert_eq!(i.as_ref(), &[1.1, 2.2, 3.3]); i }
    // pub fn native_test_ubyte_array<'local>(i: [u8])   -> [u8]  { let i: Box<[u8]> = i; assert_eq!(i.as_ref(), &[1, 2, 3]); i }
    // pub fn native_test_ushort_array<'local>(i: [u16]) -> [u16] { let i: Box<[u16]> = i; assert_eq!(i.as_ref(), &[1, 2, 3]); i }
    // pub fn native_test_uint_array<'local>(i: [u32])   -> [u32] { let i: Box<[u32]> = i; assert_eq!(i.as_ref(), &[1, 2, 3]); i }
    // pub fn native_test_ulong_array<'local>(i: [u64])  -> [u64] { let i: Box<[u64]> = i; assert_eq!(i.as_ref(), &[1, 2, 3]); i }
    pub fn native_test_bool_obj_array<'local>(b: [Option<bool>])     -> [Option<bool>]   { let b: Box<[Option<bool>]> = b; assert_eq!(b.as_ref(), &[Some(true), None]); b }
    pub fn native_test_char_obj_array<'local>(c: [Option<char>])     -> [Option<char>]   { let c: Box<[Option<char>]> = c; assert_eq!(c.as_ref(), &[Some('a'), None]); c }
    pub fn native_test_byte_obj_array<'local>(i: [Option<byte>])     -> [Option<byte>]   { let i: Box<[Option<i8>]> = i; assert_eq!(i.as_ref(), &[Some(1), None]); i }
    pub fn native_test_short_obj_array<'local>(i: [Option<short>])   -> [Option<short>]  { let i: Box<[Option<i16>]> = i; assert_eq!(i.as_ref(), &[Some(1), None]); i }
    pub fn native_test_int_obj_array<'local>(i: [Option<int>])       -> [Option<int>]    { let i: Box<[Option<i32>]> = i; assert_eq!(i.as_ref(), &[Some(1), None]); i }
    pub fn native_test_long_obj_array<'local>(i: [Option<long>])     -> [Option<long>]   { let i: Box<[Option<i64>]> = i; assert_eq!(i.as_ref(), &[Some(1), None]); i }
    pub fn native_test_float_obj_array<'local>(i: [Option<float>])   -> [Option<float>]  { let i: Box<[Option<f32>]> = i; assert_eq!(i.as_ref(), &[Some(1.1), None]); i }
    pub fn native_test_double_obj_array<'local>(i: [Option<double>]) -> [Option<double>] { let i: Box<[Option<f64>]> = i; assert_eq!(i.as_ref(), &[Some(1.1), None]); i }
}

jni_fn! { me.test.Native =>
    pub fn native_test_void<'local>() { }

    pub fn native_test_recursion<'local>(mut i: byte) {
        if i == 7 {
            return;
        }
        i += 1;
        call!(static me.test.Native.native_test_recursion(byte(i)) -> void);
    }

    /// Also test implicit variable `class`.
    pub static fn native_test_static<'local>(s: String) -> int {
        let _: JClass = class;
        let s: String = s;
        assert_eq!(s, "Hello, World!");
        s.len() as i32
    }

    /// Also tests java.lang.String vs String
    pub fn native_test_str<'local>(s: java.lang.String) -> String {
        let s: String = String::from_object(&s).unwrap();
        assert_eq!(s, "Hello, World!");
        s
    }

    /// Also test implicit variable `this`.
    pub fn native_test_obj<'local>(obj: java.lang.Object) -> java.lang.Object {
        let _: JObject = this;
        obj
    }
}

jni_fn! { me.test.Native =>
    pub fn native_test_str_arr<'local>(arr: [String]) -> [String] {
        let arr: Box<[String]> = arr;
        assert_eq!(arr.as_ref(), &["Hello", "World"]);
        arr
    }

    pub fn native_test_obj_arr<'local>(arr: [java.lang.Object]) -> [java.lang.Object] {
        let arr: ObjectArray = arr;
        assert!(arr.len() == 1);
        arr
    }

    pub fn native_test_null_str<'local>(s: Option<String>) -> Option<String> {
        let s: Option<String> = s;
        assert_eq!(s, None);
        s
    }

    pub fn native_test_str_null_arr<'local>(arr: Option<[String]>) -> Option<[String]> {
        let _: Option<Box<[String]>> = arr;
        None
    }

    pub fn native_test_null_str_arr<'local>(arr: [Option<String>]) -> [Option<String>] {
        let arr: Box<[Option<String>]> = arr;
        assert_eq!(arr.as_ref(), &[Some("Hello".to_string()), None]);
        arr
    }

    pub fn native_test_2d_str_arr<'local>(arr: [[String]]) -> [[String]] {
        let arr: Box<[Box<[String]>]> = arr;
        assert_eq!(arr.as_ref(), &[
            Box::new(["Hello", "Rust"].map(|s| s.to_string())) as Box<[_]>,
            Box::new(["From", "Java"].map(|s| s.to_string())) as Box<[_]>,
        ]);
        arr
    }
    pub fn native_test_3d_str_arr<'local>(arr: [[[String]]]) -> [[[String]]] {
        let _: Box<[Box<[Box<[String]>]>]> = arr;
        assert_eq!(arr.as_ref(), &[
            Box::new([
                Box::new(["I", "am", "Groot"].map(|s| s.to_string())) as Box<[_]>,
                Box::new(["here", "it", "is"].map(|s| s.to_string())) as Box<[_]>,
            ]) as Box<[_]>,
            Box::new([
                Box::new(["Hello", "Rust"].map(|s| s.to_string())) as Box<[_]>,
                Box::new(["From", "Java"].map(|s| s.to_string())) as Box<[_]>,
            ]) as Box<[_]>,
        ]);
        arr
    }
    pub fn native_test_2d_str_null_arr<'local>(arr: [Option<[String]>]) -> [Option<[String]>] {
        let _: Box<[Option<Box<[String]>>]> = arr;
        assert_eq!(arr.as_ref(), &[
            Some(Box::new(["Hello", "World"].map(|s| s.to_string())) as Box<[_]>),
            None,
        ]);
        arr
    }
    pub fn native_test_3d_null_str_arr<'local>(arr: [[[Option<String>]]]) -> [[[Option<String>]]] {
        let _: Box<[Box<[Box<[Option<String>]>]>]> = arr;
        assert_eq!(arr.as_ref(), &[
            Box::new([
                Box::new([Some("I"), Some("am"), None].map(|s| s.map(|s| s.to_string()))) as Box<[_]>,
                Box::new([Some("here"), Some("it"), Some("is")].map(|s| s.map(|s| s.to_string()))) as Box<[_]>,
            ]) as Box<[_]>,
            Box::new([
                Box::new([Some("Hello"), None].map(|s| s.map(|s| s.to_string()))) as Box<[_]>,
                Box::new([Some("From"), Some("Java")].map(|s| s.map(|s| s.to_string()))) as Box<[_]>,
            ]) as Box<[_]>,
        ]);
        arr
    }
}

jni_fn! { me.test.Native =>
    pub fn native_test_panic<'local>() {
        panic!("LALALA")
    }

    // pub fn test_jni_fn_10<'local>() -> Result<String, java.lang.Exception> {
    //
    // }

    // pub fn test_jni_fn_11<'local>() -> Result<Option<String>, java.lang.Exception> {
    //
    // }
}
