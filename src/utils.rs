use jni::{objects::JString, JNIEnv};

#[cfg(target_os = "android")]
pub use android::*;
#[cfg(target_os = "android")]
mod android {
    use jni::JNIEnv;
    use jni_macros::call;
    
    #[doc(hidden)]
    /// Does the printing for [`jni_macros::println!`].
    pub fn __println(s: String, env: &mut JNIEnv) {
        call!(static android.util.Log::i(
            java.lang.String(env.new_string("Rust").unwrap()),
            java.lang.String(env.new_string(s).unwrap())
        ) -> int);
    }
    
    #[doc(hidden)]
    /// Does the printing for [`jni_macros::eprintln!`].
    pub fn __eprintln(s: String, env: &mut JNIEnv) {
        call!(static android.util.Log::e(
            java.lang.String(env.new_string("Rust").unwrap()),
            java.lang.String(env.new_string(s).unwrap())
        ) -> int);
    }
}

/// Get a [`String`] from a `java.lang.String`, asserting that the object is NOT **`NULL`**.
pub fn get_string(arg: JString, env: &mut JNIEnv) -> String {
    String::from(
        env.get_string(&arg)
            .expect("String argument can't be NULL")
    )
}
/// Get [`String`] from a `java.lang.String`, which could be **`NULL`**.
pub fn get_nullable_string(arg: JString, env: &mut JNIEnv) -> Option<String> {
    if arg.is_null() {
        None
    } else {
        env.get_string(&arg)
            .ok()
            .map(String::from)
    }
}
