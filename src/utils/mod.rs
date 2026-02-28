mod call;
#[doc(hidden)]
mod object;
mod array;
use std::fmt::{Debug, Display};

pub use call::*;
#[doc(hidden)]
pub use object::*;
pub use array::*;
pub use crate::__throw::JniError;

use jni::{JNIEnv, objects::JObject};
use crate::{LOCAL_JNIENV_STACK, call, private::Sealed};

#[doc(hidden)]
pub use cfg_if;

/// Error message for when a *JNI call* returns [`Exception`][jni::errors::Error::JavaException],
/// but no Exception was found.
pub(crate) static JNI_CALL_GHOST_EXCEPTION: &str = "JNI Call returned with Error::JavaException, but no exception was found.";

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

// TODO: doc
// TODO: ensure the returned JNIEnv can't escape the function it was called from
pub fn get_env<'a, 'local>() -> &'a mut JNIEnv<'local> {
    LOCAL_JNIENV_STACK.with_borrow_mut(|stack| {
        let env = stack.back_mut()
            .expect("There are no JNIEnvs in the stack, meaning you are not in a JNI enviroment.");
        /* Safety: the reference will remain valid as long as the current Java stack frame lives,
          which is essentially until the root Rust function (that called this one) returns.
          Thus, the return value cannot exit that root function. */
        unsafe { std::mem::transmute::<&'_ mut JNIEnv<'static>, &'a mut JNIEnv<'local>>(env) }
    })
}

/// Returns the fully qualified **Class** name of the provided **object** (e.g. `"java.lang.String"`).
/// 
/// `panic!`s if there is an error.
pub fn get_object_class_name(object: &JObject<'_>, env: &mut JNIEnv<'_>) -> String {
    let class = env.get_object_class(object)
        .unwrap_jni(env);
    call!(env=> class.getName() -> String)
}

impl <T, E> Sealed for Result<T, E> { }

pub trait ResultExt<T>: Sealed {
    /// The same as [`Result::unwrap()`], but prints the error with [`Display`] instead of [`Debug`].
    fn unwrap_display(self) -> T;
}
impl<T, E> ResultExt<T> for Result<T, E>
where E: Debug + Display {
    #[inline(always)]
    #[track_caller]
    fn unwrap_display(self) -> T {
        match self {
            Ok(t) => t,
            Err(e) => std::panic::panic_any(e.to_string()),
        }
    }   
}

pub trait JniResultExt<T>: Sealed {
    /// The same as [`Result::unwrap()`], but gets the full **error** message from the [`JniError`].
    /// 
    /// This can catch *exceptions* and print out the class and message,
    /// so the [`JNIEnv`] is required for this method.
    /// 
    /// > Either [`catch`][JniResultExt::catch] or `this` function must be called directly after all *JNI calls*.
    fn unwrap_jni(self, env: &mut JNIEnv<'_>) -> T;
    /// Encapsulates an [`Error`][jni::errors::Error] from a [*JNI Call*](https://docs.rs/jni/0.21.1/jni/struct.JNIEnv.html#implementations)
    /// in a similar type that stores the `Exception` variant with the [`Exception Object`][crate::JavaException].
    /// 
    /// If the *JNI call* returned an error with [`Exception`][jni::errors::Error::JavaException],
    /// the variant does not have a reference to the [thrown object][jni::objects::JThrowable]
    /// and the JVM is kept in the **throw** state.
    /// While in this state, most *JNI calls* will fail and the error will be mixed up with the thrown object.
    /// This function **catches** the [`Exception`][jni::objects::JThrowable] so that the program can handle the error properly.
    /// 
    /// > Either [`unwrap_jni`][JniResultExt::unwrap_jni] or `this` function must be called directly after all *JNI calls*.
    fn catch(self, env: &mut JNIEnv<'_>) -> Result<T, JniError>;
}
impl<T> JniResultExt<T> for Result<T, jni::errors::Error> {
    #[inline(always)]
    #[track_caller]
    fn unwrap_jni(self, env: &mut JNIEnv<'_>) -> T {
        match self {
            Ok(t) => t,
            Err(error) => ez_jni::__throw::__panic_jni_error(error, env),
        }
    }
    #[inline(always)]
    fn catch(self, env: &mut JNIEnv<'_>) -> Result<T, JniError> {
        self.map_err(|error| JniError::new(error, env))
    }
}

#[doc(hidden)]
#[inline(always)]
pub fn jboolean_to_bool(b: jni::sys::jboolean) -> bool { b != 0 }
#[doc(hidden)]
#[inline(always)]
pub fn bool_to_jboolean(b:bool) ->  jni::sys::jboolean { b as _ }
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
