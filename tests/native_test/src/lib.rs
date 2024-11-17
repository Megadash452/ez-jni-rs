use ez_jni::jni_fn;
use jni::objects::JObject;

jni_fn! { me.test.Native =>
    pub fn native_test_bool<'local>(b: bool) -> bool { let b: bool = b; assert_eq!(b, true); b }
    pub fn native_test_char<'local>(c: char) -> char { let c: char = c; assert_eq!(c, 'a'); c }
    pub fn native_test_byte<'local>(i: byte) -> byte { let i: i8 = i; assert_eq!(i, 3); i }
    pub fn native_test_short<'local>(i: short) -> short { let i: i16 = i; assert_eq!(i, 3); i }
    pub fn native_test_int<'local>(i: int) -> int { let i: i32 = i; assert_eq!(i, 3); i }
    pub fn native_test_long<'local>(i: long) -> long { let i: i64 = i; assert_eq!(i, 3); i }
    pub fn native_test_float<'local>(i: float) -> float { let i: f32 = i; assert_eq!(i, 3.3); i }
    pub fn native_test_double<'local>(i: double) -> double { let i: f64 = i; assert_eq!(i, 3.3); i }
    pub fn native_test_ubyte<'local>(i: u8) -> u8 { let i: u8 = i; assert_eq!(i, 3); i }
    pub fn native_test_ushort<'local>(i: u16) -> u16 { let i: u16 = i; assert_eq!(i, 3); i }
    pub fn native_test_uint<'local>(i: u32) -> u32 { let i: u32 = i; assert_eq!(i, 3); i }
    pub fn native_test_ulong<'local>(i: u64) -> u64 { let i: u64 = i; assert_eq!(i, 3); i }

    pub fn native_test_bool_array<'local>(b: [bool]) -> [bool] { let b: Box<[bool]> = b; assert_eq!(b.as_ref(), &[true, false]); b }
    pub fn native_test_char_array<'local>(c: [char]) -> [char] { let c: Box<[char]> = c; assert_eq!(c.as_ref(), &['a', 'b']); c }
    pub fn native_test_byte_array<'local>(i: [byte]) -> [byte] { let i: Box<[i8]> = i; assert_eq!(i.as_ref(), &[1, 2, 3]); i }
    pub fn native_test_short_array<'local>(i: [short]) -> [short] { let i: Box<[i16]> = i; assert_eq!(i.as_ref(), &[1, 2, 3]); i }
    pub fn native_test_int_array<'local>(i: [int]) -> [int] { let i: Box<[i32]> = i; assert_eq!(i.as_ref(), &[1, 2, 3]); i }
    pub fn native_test_long_array<'local>(i: [long]) -> [long] { let i: Box<[i64]> = i; assert_eq!(i.as_ref(), &[1, 2, 3]); i }
    pub fn native_test_float_array<'local>(i: [float]) -> [float] { let i: Box<[f32]> = i; assert_eq!(i.as_ref(), &[1.1, 2.2, 3.3]); i }
    pub fn native_test_double_array<'local>(i: [double]) -> [double] { let i: Box<[f64]> = i; assert_eq!(i.as_ref(), &[1.1, 2.2, 3.3]); i }
    pub fn native_test_ubyte_array<'local>(i: [u8]) -> [u8] { let i: Box<[u8]> = i; assert_eq!(i.as_ref(), &[1, 2, 3]); i }
    pub fn native_test_ushort_array<'local>(i: [u16]) -> [u16] { let i: Box<[u16]> = i; assert_eq!(i.as_ref(), &[1, 2, 3]); i }
    pub fn native_test_uint_array<'local>(i: [u32]) -> [u32] { let i: Box<[u32]> = i; assert_eq!(i.as_ref(), &[1, 2, 3]); i }
    pub fn native_test_ulong_array<'local>(i: [u64]) -> [u64] { let i: Box<[u64]> = i; assert_eq!(i.as_ref(), &[1, 2, 3]); i }


    pub fn native_test_void<'local>() { }

    pub fn native_test_static<'local>(s: java.lang.String) -> int {
        let s: String = s;
        assert_eq!(s, "Hello, World!");
        s.len() as i32
    }

    pub fn native_test_str<'local>(s: String) -> String {
        let s: String = s;
        assert_eq!(s, "Hello, World!");
        s
    }

    pub fn native_test_obj<'local>(obj: java.lang.Object) -> java.lang.Object {
        obj
    }

    pub fn native_test_str_arr<'local>(arr: [String]) -> [String] {
        let arr: Box<[String]> = arr;
        assert_eq!(arr.as_ref(), &["Hello", "World"]);
        arr
    }

    pub fn native_test_obj_arr<'local>(arr: [java.lang.Object]) -> [java.lang.Object] {
        let arr: Box<[JObject]> = arr;
        assert!(arr.len() == 1);
        arr
    }

    pub fn native_test_null_str<'local>(s: Option<String>) -> Option<String> {
        let s: Option<String> = s;
        assert_eq!(s, None);
        None?
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
        // assert!(arr.len() == 2);
        // assert_eq!(arr[0].as_ref(), &["Hello", "Rust"]);
        // assert_eq!(arr[1].as_ref(), &["From", "Java"]);
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


    // pub fn test_jni_fn_10<'local>() -> Result<String, java.lang.Exception> {
        
    // }


    // pub fn test_jni_fn_11<'local>() -> Result<Option<String>, java.lang.Exception> {
        
    // }
}
