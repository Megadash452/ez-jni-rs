//! Contains helper functions for the To/FromObject implementatios in `/src/object/impl_array.rs`.
use jni::{objects::{JObject, JObjectArray, JPrimitiveArray}, sys::jsize, JNIEnv};
use utils::java_path_to_dot_notation;
use crate::{FromObjectError, call};


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
/// 
/// If the primitive should be converted before being added to the *Java Array*
/// (e.g. the slice is `u8`, so it must be transmuted to `i8`),
/// then use [`create_java_prim_array_converted()`] instead.
pub fn create_java_prim_array<'local, 'a, T>(
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
/// Like [`create_java_prim_array()`], but performs **conversion** on each element of the **slice*
/// before they are added to the *Java Array*.
pub fn create_java_prim_array_converted<'local, 'a, T, J>(
    slice: &[T],
    alloc: fn(&JNIEnv<'local>, jsize) -> jni::errors::Result<JPrimitiveArray<'local, J>>,
    filler: fn(&JNIEnv<'local>, JPrimitiveArray<'local, J>, jsize, &[J]) -> jni::errors::Result<()>,
    elem_conversion: fn(T) -> J,
    env: &JNIEnv<'local>
) -> JObject<'local>
    where T: Copy,
          J: jni::objects::TypeArray,
{
    let slice = slice.iter()
        .map(#[inline] |&t| elem_conversion(t))
        .collect::<Box<[_]>>();
    create_java_prim_array(&slice, alloc, filler, env)
}

/// Get a Rust [`Vec`] from a Java **Array**, where the element `T` is a *primitive*.
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
pub fn get_java_prim_array<'local, 'other, 'a, T>(
    obj: &'a JObject<'other>,
    filler: fn(&JNIEnv<'local>, &'a JPrimitiveArray<'other, T>, jsize, &mut [T]) -> jni::errors::Result<()>,
    env: &mut JNIEnv<'local>
) -> Box<[T]>
    where T: jni::objects::TypeArray
{
    let array = <&'a JPrimitiveArray<'other, T>>::from(obj);
    // Check object's type
    // let class = env.get_object_class(obj)
    //     .unwrap_or_else(|err| panic!("Failed to get Object's class: {err}"));
    // let ty = call!(env=> class.getName() -> String);
    // if ty != T::PATH {
    //     panic!("Expected object's type to be \"{}\", but is actually \"{ty}\"", T::PATH)
    // }

    let len = env.get_array_length(array)
        .unwrap_or_else(|err| panic!("Failed to check Array's length: {err}"))
        as usize;
    // Allocate array
    let mut vec = vec![unsafe { std::mem::zeroed() }; len].into_boxed_slice();

    // Fill array
    filler(env, array, 0, &mut vec)
        .unwrap_or_else(|err| panic!("Failed to read Array elements: {err}"));

    vec
}

/// Creates a Rust [`Vec`] of [`JObject`]s by reading elements from a **Java Array** (**obj**).
/// 
/// Also checks that the Array Object's class matches the **array_class** (if it is [`Some`])
/// and `panic!s` if they don't match.
/// 
/// If the Object should be converted to a *Rust Type* (e.g. `String`),
/// then use [`get_object_array_converted()`] instead.
pub fn get_object_array<'local>(obj: &JObject<'_>, array_class: Option<&'static str>, env: &mut JNIEnv<'local>) -> Result<Box<[JObject<'local>]>, FromObjectError> {
    let array = <&JObjectArray>::from(obj);
    // Check object's type
    if let Some(class) = array_class {
        // For some reason, class.getName() returns a ClassPath with .dots. instead of /slashes/, so the sig_type needs to be transformed.
        // This is the only place where this happens. why??
        let class = java_path_to_dot_notation(class);
        let obj_class = env.get_object_class(obj)
            .unwrap_or_else(|err| panic!("Failed to get Object's class: {err}"));
        let obj_class = call!(env=> obj_class.getName() -> String);
        if obj_class != class {
            return Err(FromObjectError::ClassMismatch { obj_class, target_class: Some(class) })
        }
    }

    let len = env.get_array_length(array)
        .unwrap_or_else(|err| panic!("Failed to check Array's length: {err}"))
        as usize;
    // Allocate array
    let mut vec = Vec::with_capacity(len);

    // Fill array
    for i in 0..len {
        vec.push(env.get_object_array_element(array, i as jsize)
            .unwrap_or_else(|err| panic!("Failed to read Array elements: {err}"))
        );
    }

    Ok(vec.into_boxed_slice())
}
/// Like [`create_object_array()`], but performs **conversion** on each element of the returned array.
pub fn get_object_array_converted<'local, T>(
    obj: &JObject<'_>,
    array_class: Option<&'static str>,
    elem_conversion: fn(JObject<'local>, &mut JNIEnv<'local>) -> T,
    env: &mut JNIEnv<'local>
) -> Result<Box<[T]>, FromObjectError> {
    Ok(::ez_jni::utils::get_object_array(obj, array_class, env)?
        .into_iter()
        .map(|t| elem_conversion(t, env))
        .collect::<Box<[_]>>()
    )
}

/// Creates a Java Array form a [`Vec`] of [`JObject`]s.
/// 
/// Must provide a **Class**Path for the elements of the array.
/// E.g. if the desired type of the array is `[Ljava/lang/Integer;`,
/// then **elem_class** must be `"java/lang/Integer"`
/// 
/// If the *Rust Type* should be converted before being added to the *Java Array*
/// (e.g. the slice is `String`, so it must be converted to `JObject`),
/// then use [`create_object_array_converted()`] instead.
pub fn create_object_array<'local, 'other>(slice: &[impl AsRef<JObject<'other>>], elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
    // Allocate the array
    let array = env.new_object_array(
        slice.len() as jsize,
        elem_class,
        JObject::null()
    )
        .unwrap_or_else(|err| panic!("Failed to create Java Object \"{elem_class}\" array: {err}"));

    // Fill the array
    for (i, element) in slice.iter().enumerate() {
        env.set_object_array_element(&array, i as jsize, element)
            .unwrap_or_else(|err| panic!("Failed to set the value of Object array at index {i}: {err}"));
    }

    array.into()
}
/// Like [`create_object_array()`], but performs **conversion** on each element of the **slice*
/// before they are added to the *Java Array*.
pub fn create_object_array_converted<'local, T>(
    slice: &[T],
    elem_class: &str,
    elem_conversion: for<'other> fn(&T, &mut JNIEnv<'other>) -> JObject<'other>,
    env: &mut JNIEnv<'local>
) -> JObject<'local> {
    // Push a new Local Frame because the elem_conversion will need to create new Objects that should be dropped when this call finishes.
    env.with_local_frame_returning_local(slice.len() as i32, |env| -> Result<JObject, jni::errors::Error> {
        let slice = slice.iter()
            .map(#[inline] |t| elem_conversion(t, env))
            .collect::<Box<[_]>>();
        Ok(create_object_array(&slice, elem_class, env))
    }).unwrap()
}
