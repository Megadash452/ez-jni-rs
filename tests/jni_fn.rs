mod common;

use ez_jni::{jni_fn, FromObject, ToObject};

jni_fn! {
    #[class(me.test.Test)]
    pub fn test_jni_fn_1<'local>() { }

    #[class = "me.test.Test"]
    pub fn test_jni_fn_2<'local>(s: java.lang.String) -> int {
        String::from_object(&s, env).unwrap().len() as i32
    }

    #[class = "me.test.Test"]
    pub fn test_jni_fn_3<'local>(s: java.lang.String) -> java.lang.String {
        String::from_object(&s, env).unwrap().to_object(env).into_raw()
    }
}

// Can't test runtime because unsure how to link exported functions to Java