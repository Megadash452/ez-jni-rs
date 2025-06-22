mod call;
mod object;
mod array;
pub use call::*;
pub use object::*;
pub use array::*;

use jni::JNIEnv;
use crate::{FromException, LOCAL_JNIENV_STACK};

#[doc(hidden)]
pub use cfg_if;

/// Error message for when a *JNI call* returns [`Exception`][JNIError::JavaException],
/// but no Exception was found.
pub(crate) static JNI_CALL_GHOST_EXCEPTION: &str = "JNI Call returned with Error::JavaException, but no exception was found.";

#[derive(FromException)]
#[class(java.lang.NoSuchFieldError)]
struct FieldNotFound;

#[derive(FromException)]
#[class(java.lang.NoSuchMethodError)]
struct MethodNotFound;

#[cfg(target_os = "android")]
pub use android::*;

#[cfg(target_os = "android")]
mod android {
    use ez_jni::call;
    
    #[doc(hidden)]
    /// Does the printing for [`jni_macros::println!`].
    pub fn __println(s: String) {
        // TODO: use NDK instead
        call!(static android.util.Log.i(String("Rust"), String(s)) -> int);
    }
    
    #[doc(hidden)]
    /// Does the printing for [`jni_macros::eprintln!`].
    pub fn __eprintln(s: String) {
        // TODO: use NDK instead
        call!(static android.util.Log.e(String("Rust"), String(s)) -> int);
    }

    // This test will NOT run because it only exists in Android build,
    // but there is no Android device to run it,
    // so it is impossible to test if the call to android.util.Log will be successful.
    // However, it will be built with Android compiler by github workflows,
    // so it is able to test if the macro outputs correct tokens.
    #[doc(hidden)]
    #[allow(unused)]
    fn print() {
        use ez_jni::{println, eprintln};
        println!("Hello, World!");
        eprintln!("Hello, World!");
    }
}

// /// Get a [`String`] from a `java.lang.String`, asserting that the object is NOT **`NULL`**.
// pub fn get_string(arg: JString, env: &mut JNIEnv) -> String {
//     String::from(
//         env.get_string(&arg)
//             .expect("String argument can't be NULL")
//     )
// }
// /// Get [`String`] from a `java.lang.String`, which could be **`NULL`**.
// pub fn get_nullable_string(arg: JString, env: &mut JNIEnv) -> Option<String> {
//     if arg.is_null() {
//         None
//     } else {
//         env.get_string(&arg)
//             .ok()
//             .map(String::from)
//     }
// }
// /// Create a *Java String* from a *Rust [`String`]*.
// pub fn new_string<'local>(s: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
//     env.new_string(s)
//         .unwrap_or_else(|err| panic!("Error creating Java String: {err}"))
//         .into()
// }

// TODO: doc
// TODO: ensure the returned JNIEnv can't escape the function it was called from
pub fn get_env<'a, 'local>() -> &'a mut JNIEnv<'local> {
    LOCAL_JNIENV_STACK.with_borrow_mut(|stack| {
        let env = stack.back_mut().expect("There are no JNIEnvs in the stack, meaning you are not in a JNI enviroment.");
        /* Safety: the reference will remain valid as long as the current Java stack frame lives,
          which is essentially until the root Rust function (that called this one) returns.
          Thus, the return value cannot exit that root function. */
        unsafe { std::mem::transmute::<&'_ mut JNIEnv<'static>, &'a mut JNIEnv<'local>>(env) }
    })
}

#[doc(hidden)]
pub fn jboolean_to_bool(b: jni::sys::jboolean) -> bool { b != 0 }
#[doc(hidden)]
pub fn jchar_to_char(c: jni::sys::jchar) -> char {
    char::decode_utf16(Some(c))
        .next().unwrap()
        .unwrap_or(char::REPLACEMENT_CHARACTER)
}
#[doc(hidden)]
pub fn char_to_jchar(c: char) -> jni::sys::jchar {
    c.encode_utf16(&mut [0;1])[0]
}
