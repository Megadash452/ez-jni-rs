use jni::{
    errors::Error as JNIError, objects::{JObject, JPrimitiveArray, JString, JValueOwned}, sys::jsize, JNIEnv
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

    let class = env.get_object_class(object)
        .unwrap_or_else(|err| panic!("Error gettig object's Class: {err}"));
    let class = call!(class.getName() -> String);

    // What to do if FieldNotFound
    let handle_not_found = |env: &mut JNIEnv<'local>| {
        if getter_fallback {
            let method = format!("get{}", first_char_uppercase(name));
            call_getter(object, &method, ty, env)
        } else {
            Err(FromObjectError::FieldNotFound {
                name: name.to_string(),
                ty: ty.to_string(),
                target_class: class
            })
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

pub fn call_getter<'local>(
    object: &JObject,
    mathod_name: &str,
    ty: &str,
    env: &mut JNIEnv<'local>,
) -> Result<JValueOwned<'local>, FromObjectError> {
    #[derive(FromException)]
    #[class(java.lang.NoSuchMethodError)]
    struct MethodNotFound;

    let class = env.get_object_class(object)
        .unwrap_or_else(|err| panic!("Error gettig object's Class: {err}"));
    let class = call!(class.getName() -> String);

    let error = FromObjectError::FieldNotFound {
        name: format!("{mathod_name}()"),
        ty: ty.to_string(),
        target_class: class
    };
    env.call_method(object, mathod_name, format!("(){ty}"), &[])
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
}

/// Checks that a [`JObject`] is an **Array** (class name starts with `'['`),
/// and returns the **length** of the Array.
/// 
/// Will `panic!` if `obj` is not the correct type.
#[doc(hidden)]
pub fn __obj_array_len(obj: &JObject, inner_ty: &str, env: &mut JNIEnv) -> usize {
    let class = env.get_object_class(obj)
        .unwrap_or_else(|err| panic!("Failed to get Object's class: {err}"));

    let sig = call!(class.getName() -> String);

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

/// Create a Java **Array** from a Rust [slice](https://doc.rust-lang.org/std/primitive.slice.html),
/// where the element `T` is a *primitive*.
/// 
/// **alloc** is the function that creates/*allocates* the new Java Array.
/// This should be one of `JNIEnv::new_{prim}_array`.
/// 
/// **filler** is the function that copies the elements from the Rust *slice* to the Java *Array*.
/// This should be one of `JNIEnv::set_{prim}_array_region`.
/// 
/// ## Example
/// 
/// ```ignore
/// ez_jni::utils::crate_java_prim_array(&[1, 2, 3],
///     JNIEnv::new_byte_array,
///     JNIEnv::set_byte_array_region,
/// env);
/// ```
pub(crate) fn create_java_prim_array<'local, 'a, T>(
    slice: &[T],
    alloc: fn(&JNIEnv<'local>, jsize) -> jni::errors::Result<JPrimitiveArray<'local, T>>,
    filler: fn(&JNIEnv<'local>, JPrimitiveArray<'local, T>, jsize, &[T]) -> jni::errors::Result<()>,
    env: &JNIEnv<'local>
) -> JObject<'local>
    where T: jni::objects::TypeArray,
{
    // Allocate the array
    let array = alloc(env, slice.len() as jsize)
        .unwrap_or_else(|err| panic!("Failed to create Array: {err}"));
    // Fill the Array
    filler(env, unsafe { JPrimitiveArray::from_raw(array.as_raw()) }, 0, slice)
        .unwrap_or_else(|err| panic!("Error filling Array: {err}"));

    array.into()
}

/// Get a Rust [`Vec`] from a Java **Array**, where the element `T` is a *primitive*.
/// 
/// This function checks that the Object is of the correct [`Type`][crate::FromObject::PATH].
/// 
/// **obj** is the Java Array.
/// 
/// **filler** is the function that copies the elements from the Java *Array* to the *Vec*.
/// This should be one of `JNIEnv::get_{prim}_array_region`.
/// 
/// ## Example
/// 
/// ```ignore
/// ez_jni::utils::get_java_prim_array(object, JNIEnv::get_byte_array_region, env);
/// ```
pub(crate) fn get_java_prim_array<'local, 'other, 'a, T>(
    obj: &'a JObject<'other>,
    filler: fn(&JNIEnv<'local>, &'a JPrimitiveArray<'other, T>, jsize, &mut [T]) -> jni::errors::Result<()>,
    env: &mut JNIEnv<'local>
) -> Vec<T>
    where T: jni::objects::TypeArray + crate::FromObject<'local>
{
    let array = <&'a JPrimitiveArray<'other, T>>::from(obj);
    // Check object's type
    // let class = env.get_object_class(obj)
    //     .unwrap_or_else(|err| panic!("Failed to get Object's class: {err}"));
    // let ty = call!(class.getName() -> String);
    // if ty != T::PATH {
    //     panic!("Expected object's type to be \"{}\", but is actually \"{ty}\"", T::PATH)
    // }

    let len = env.get_array_length(array)
        .unwrap_or_else(|err| panic!("Failed to check Array's length: {err}"))
        as usize;
    // Allocate array
    let mut vec = vec![unsafe { std::mem::zeroed() }; len];

    // Fill array
    filler(env, array, 0, &mut vec)
        .unwrap_or_else(|err| panic!("Failed to read Array elements: {err}"));

    vec
}

/// Convert the first letter of a String into uppercase
fn first_char_uppercase(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        None => String::new(),
    }
}

/// Converts a ClassPath that uses *slashes* `/` to separate components to a Class that uses *dots* `.`.
/// 
/// e.g. `"[Ljava/lang/String;" -> "[Ljava.lang.String;"`
pub(crate) fn java_path_to_dot_notation(path: &str) -> String {
    path.replace(['/', '$'], ".")
}