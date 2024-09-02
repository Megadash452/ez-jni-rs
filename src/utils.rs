use jni::{objects::{JObject, JString}, JNIEnv};
use jni_macros::call;

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

/// Check if an Object's class is **class**, or if it **extends** (is descendant of) **class**.
/// 
/// Use this to convert an Object into a rust type, such as in [`FromException`][crate::FromException].
pub fn object_is_descendant_of(env: &mut JNIEnv, obj: &JObject, class: &str) -> bool {
    // First check if the top class of obj is `class`.
    let obj_class = env.get_object_class(obj)
        .expect("Failed to get Object's class");
    let obj_class_name = get_string(call!(obj_class.getName() -> java.lang.String), env);
    if obj_class_name == class {
        return true
    }
    
    let mut current = obj_class;
    while let Some(super_class) = env.get_superclass(&current)
        .expect("Failed to get class' super class")
    {
        let class_name = get_string(call!(super_class.getName() -> java.lang.String), env);
        if class_name == class {
            return true
        }
        current = super_class
    }
    
    false
}
