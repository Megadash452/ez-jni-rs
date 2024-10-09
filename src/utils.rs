use jni::{
    errors::Error as JNIError, objects::{JObject, JString, JValueOwned}, JNIEnv
};
use crate::{call, object::FromObjectError, FromException, __throw::{panic_exception, try_catch}};

#[cfg(target_os = "android")]
pub use android::*;

#[cfg(target_os = "android")]
mod android {
    use jni::JNIEnv;
    use jni_macros::call;
    
    #[doc(hidden)]
    /// Does the printing for [`jni_macros::println!`].
    pub fn __println(s: String, env: &mut JNIEnv) {
        call!(static android.util.Log.i(
            java.lang.String(env.new_string("Rust").unwrap()),
            java.lang.String(env.new_string(s).unwrap())
        ) -> int);
    }
    
    #[doc(hidden)]
    /// Does the printing for [`jni_macros::eprintln!`].
    pub fn __eprintln(s: String, env: &mut JNIEnv) {
        call!(static android.util.Log.e(
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

/// Read a **field** from a Java Object, given the **name** of the field and its **ty**pe.
/// 
/// If the field does not exists and **getter_fallback** is `true`,
/// this will call a *getter method* with `get` prepended to the field's name.
/// E.g. if the field `java.lang.String message` did not exist,
/// then `java.lang.String getMessage()` will be called.
pub fn get_field<'local>(
    object: &JObject,
    name: &str,
    ty: &str,
    getter_fallback: bool,
    env: &mut JNIEnv<'local>,
) -> Result<JValueOwned<'local>, FromObjectError> {
    #[derive(FromException)]
    #[class(java.lang.NoSuchFieldError)]
    struct FieldNotFound;
    #[derive(FromException)]
    #[class(java.lang.NoSuchMethodError)]
    struct MethodNotFound;

    let class = env.get_object_class(object)
        .unwrap_or_else(|err| panic!("Error gettig object's Class: {err}"));
    let class = get_string(call!(class.getName() -> java.lang.String), env);

    // What to do if FieldNotFound
    let handle_not_found = |env: &mut JNIEnv<'local>| {
        if getter_fallback {
            let name = format!("get{}", first_char_uppercase(name));
            let error = FromObjectError::FieldNotFound { name: format!("{name}()"), ty: ty.to_string(), target_class: class };
            env.call_method(object, name, format!("(){ty}"), &[])
                .map_err(|err| match err {
                    JNIError::MethodNotFound { .. } => error,
                    JNIError::JavaException
                        => if let Some(MethodNotFound) = try_catch(env) {
                            error
                        } else {
                            panic_exception(env.exception_occurred().unwrap(), env)
                        },
                    err => panic!("Error occurred while calling getter method: {err}")
                })
        } else {
            Err(FromObjectError::FieldNotFound { name: name.to_string(), ty: ty.to_string(), target_class: class })
        }
    };

    env.get_field(object, name, ty)
        .or_else(|err| match err {
            JNIError::FieldNotFound { .. } => handle_not_found(env),
            JNIError::JavaException =>
                if let Some(FieldNotFound) = try_catch(env) {
                    handle_not_found(env)
                } else {
                    panic_exception(env.exception_occurred().unwrap(), env)
                },
            err => panic!("Error occurred while accessing field: {err}")
        })
}

/// Checks that a [`JObject`] is an **Array** (class name starts with `'['`),
/// and returns the **length** of the Array.
/// 
/// Will `panic!` if `obj` is not the correct type.
#[doc(hidden)]
pub fn __obj_array_len(obj: &JObject, inner_ty: &str, env: &mut JNIEnv) -> usize {
    let class = env.get_object_class(obj)
        .unwrap_or_else(|err| panic!("Failed to get Object's class: {err}"));

    let sig = get_string(call!(class.getName() -> java.lang.String), env);

    if !sig.chars().next().is_some_and(|c| c == '[') {
        panic!("Expected object to be an Array, but is actually \"{sig}\"")
    }

    let inner = &sig[1..];
    if inner != inner_ty {
        panic!("Expected Array's inner type to be \"{inner_ty}\", but is actually \"{inner}\"")
    }

    env.get_array_length(<&jni::objects::JObjectArray>::from(obj))
        .unwrap_or_else(|err| panic!("Failed to check Array's length: {err}"))
        as usize
}

/// Convert the first letter of a String into uppercase
fn first_char_uppercase(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        None => String::new(),
    }
}