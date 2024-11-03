mod common;

use ez_jni::jni_fn;

jni_fn! {
    #[class(me.test.Test)]
    pub fn no_run_1<'local>(b: bool) -> bool { let b: bool = b; b }
    #[class(me.test.Test)]
    pub fn no_run_2<'local>(c: char) -> char { let c: char = c; c }
    #[class(me.test.Test)]
    pub fn no_run_3<'local>(i: byte) -> byte { let i: i8 = i; i }
    #[class(me.test.Test)]
    pub fn no_run_4<'local>(i: short) -> short { let i: i16 = i; i }
    #[class(me.test.Test)]
    pub fn no_run_5<'local>(i: int) -> int { let i: i32 = i; i }
    #[class(me.test.Test)]
    pub fn no_run_6<'local>(i: long) -> long { let i: i64 = i; i }
    #[class(me.test.Test)]
    pub fn no_run_7<'local>(i: float) -> float { let i: f32 = i; i }
    #[class(me.test.Test)]
    pub fn no_run_8<'local>(i: double) -> double { let i: f64 = i; i }
    #[class(me.test.Test)]
    pub fn no_run_9<'local>(i: u8) -> u8 { let i: u8 = i; i }
    #[class(me.test.Test)]
    pub fn no_run_10<'local>(i: u16) -> u16 { let i: u16 = i; i }
    #[class(me.test.Test)]
    pub fn no_run_11<'local>(i: u32) -> u32 { let i: u32 = i; i }
    #[class(me.test.Test)]
    pub fn no_run_12<'local>(i: u64) -> u64 { let i: u64 = i; i }
}

jni_fn! {
    #[class = "me.test.Test"]
    pub fn test_jni_fn_1<'local>() { }

    #[class(me.test.Test)]
    pub fn test_jni_fn_2<'local>(s: java.lang.String) -> int {
        s.len() as i32
    }

    #[class(me.test.Test)]
    pub fn test_jni_fn_3<'local>(s: String) -> String {
        s
    }

    #[class(me.test.Test)]
    pub fn test_jni_fn_4<'local>(s: [String]) {
        let _: Box<[String]> = s;
    }

    #[class(me.test.Test)]
    pub fn test_jni_fn_5<'local>() -> [String] {
        ["Hello", "World"]
    }

    #[class(me.test.Test)]
    pub fn test_jni_fn_6<'local>(s: Option<String>) {
        let _: Option<String> = s;
    }

    #[class(me.test.Test)]
    pub fn test_jni_fn_7<'local>() -> Option<String> {
        Option::<String>::None
    }

    // #[class(me.test.Test)]
    // pub fn test_jni_fn_8<'local>() -> Result<String, java.lang.Exception> {
        
    // }

    // #[class(me.test.Test)]
    // pub fn test_jni_fn_9<'local>() -> Result<Option<String>, java.lang.Exception> {
        
    // }
}

#[test]
fn jni_fn() {

}

// Can't test runtime because unsure how to link exported functions to Java