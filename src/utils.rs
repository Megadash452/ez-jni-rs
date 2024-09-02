use jni::{objects::{JObject, JString}, JNIEnv};
use jni_macros::call;

#[cfg(target_os = "android")]
pub use android::*;
#[cfg(target_os = "android")]
mod android {
    use jni::JNIEnv;
    use jni_macros::call;
    
    #[doc(hidden)]
    /// Does the printing for [`print`] and [`println`].
    pub fn __println(s: String, env: &mut JNIEnv) {
        call!(static android.util.Log::i(
            java.lang.String(env.new_string("Rust").unwrap()),
            java.lang.String(env.new_string(s).unwrap())
        ) -> int);
    }
    
    #[doc(hidden)]
    /// Does the printing for [`eprint`] and [`eprintln`].
    pub fn __eprintln(s: String, env: &mut JNIEnv) {
        call!(static android.util.Log::e(
            java.lang.String(env.new_string("Rust").unwrap()),
            java.lang.String(env.new_string(s).unwrap())
        ) -> int);
    }
}

// #[macro_export]
// /// Printing to STDOUT does not work in Android because apparently it redirects to `/dev/null`.
// /// This macro replaces the regular [`print!()`] so that the output actually goes somewhere (Android app log in this case).
// macro_rules! print {
//     () => { };
//     ($($arg:tt)*) => {
//         $crate::utils::__print(env, format!($($arg)*))
//     };
// }
#[macro_export]
/// Printing to STDOUT does not work in Android because apparently it redirects to `/dev/null`.
/// This macro replaces the regular [`print!()`] so that the output actually goes somewhere (Android app log in this case).
macro_rules! println {
    () => { cfg_if! {
        if #[cfg(target_os = "android")] {
            $crate::utils::__println("".to_string(), env)
        } else {
            ::std::println!()
        }
    } };
    ($($arg:tt)*) => { cfg_if! {
        if #[cfg(target_os = "android")] {
            $crate::utils::__println(format!($($arg)*), env)
        } else {
            ::std::println!($($arg)*)
        }
    } };
}

// #[macro_export]
// /// See [`print`].
// macro_rules! eprint {
//     () => { };
//     ($($arg:tt)*) => {
//         $crate::utils::__print(format!($($arg)*), env)
//     };
// }
#[macro_export]
/// Like [`println`], but prints with Error level.
macro_rules! eprintln {
    () => { ::cfg_if::cfg_if! {
        if #[cfg(target_os = "android")] {
            $crate::utils::__eprintln("".to_string(), env)
        } else {
            ::std::eprintln!()
        }
    } };
    ($($arg:tt)*) => { ::cfg_if::cfg_if! {
        if #[cfg(target_os = "android")] {
            $crate::utils::__eprintln(format!($($arg)*), env)
        } else {
            ::std::eprintln!($($arg)*)
        }
    } };
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
    env.get_string(&arg)
        .ok()
        .map(String::from)
}

/// Check if an Object's class is **class**, or if it **extends** (is descendant of) **class**.
/// 
/// Use this to convert an Object into a rust type, such as in [`FromException`][crate::FromException].
pub fn object_is_descendant_of(env: &mut JNIEnv, obj: &JObject, class: &str) -> bool {
    // First check if the top class of obj is `class`.
    let obj_class = env.get_object_class(obj)
        .expect("Failed to get Object's class");
    let obj_class_name = JString::from(call!(obj_class.getName() -> java.lang.String));
    let obj_class_name = get_string(obj_class_name, env);
    if obj_class_name == class {
        return true
    }
    
    let mut current = obj_class;
    while let Some(super_class) = env.get_superclass(&current)
        .expect("Failed to get class' super class")
    {
        let class_name = get_string(JString::from(
            call!(super_class.getName() -> java.lang.String)
        ), env);
        if class_name == class {
            return true
        }
        current = super_class
    }
    
    false
}
