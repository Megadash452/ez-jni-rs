//! Contains helper functions for the To/FromObject implementatios in `/src/object/impl_array.rs`.
use jni::{objects::{JObject, JObjectArray, JPrimitiveArray}, sys::jsize, JNIEnv};
use utils::java_path_to_dot_notation;
use crate::{call, FromObject, FromObjectError, Primitive};


/// Create a Java **Array** from a Rust [slice](https://doc.rust-lang.org/std/primitive.slice.html),
/// where the element `T` is a [`Primitive`].
/// 
/// This funciton automatically handles the conversion between the *Rust type* and the *Java type* (e.g. bool -> u8).
pub(crate) fn create_java_prim_array<'local, T>(slice: &[T], env: &JNIEnv<'local>) -> JObject<'local>
where T: Primitive {
    let slice = match T::CONVERT_RUST_TO_JAVA {
        // If the type declares that it requires a conversion between itself and its Java Type,
        // convert each element in the slice and put them in a Boxed slice.
        Some(elem_conversion) => &slice.iter()
            .map(#[inline] |&t| elem_conversion(t))
            .collect::<Box<[_]>>(),
        // If the type requires no conversion, then T and T::JType are the same and are safe to transmute.
        None => unsafe { std::mem::transmute::<&[T], &[T::JType]>(slice) }
    };

    // Allocate the array
    let array = T::array_alloc(slice.len() as jsize, env)
        .unwrap_or_else(|err| panic!("Failed to create {} Array: {err}", T::JNAME));
    // Fill the Array
    T::array_filler(&array, slice, env)
        .unwrap_or_else(|err| panic!("Error filling {} Array: {err}", T::JNAME));

    array.into()
}

/// Get a Rust [`Vec`] from a Java **Array**, where the element `T` is a *primitive*.
/// 
/// **obj** is the Java Array.
/// 
/// This funciton automatically handles the conversion between the *Rust type* and the *Java type* (e.g. u8 -> bool).
pub(crate) fn get_java_prim_array<T>(obj: &JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Box<[T]>, FromObjectError>
where T: Primitive + for<'a, 'obj, 'local> FromObject<'a, 'obj, 'local> {
    if obj.is_null() {
        return Err(FromObjectError::Null);
    }

    let array = <&JPrimitiveArray<'_, T::JType>>::from(obj);
    let array_class = call!(env=> call!(env=> obj.getClass() -> Class).getName() -> String);

    // Check if array contains primitives or Objects of primitives.
    if array_class == format!("[L{};", T::JNAME) {
        // Array contains primitives
        let len = env.get_array_length(array)
            .map_err(|err| FromObjectError::Other(format!("Failed to check Array's length: {err}")))?
            as usize;
        // Allocate boxed slice
        let mut vec = vec![unsafe { std::mem::zeroed() }; len].into_boxed_slice();

        // Fill slice
        T::slice_filler(&array, &mut vec, env)
            .map_err(|err| FromObjectError::Other(format!("Failed to read Array elements: {err}")))?;

        Ok(match T::CONVERT_JAVA_TO_RUST {
            Some(elem_conversion) => vec.into_iter()
                .map(#[inline] |t| elem_conversion(t))
                .collect::<Box<[_]>>(),
            // If the type requires no conversion, then T and T::JType are the same and are safe to transmute.
            None => unsafe { std::mem::transmute::<Box<[T::JType]>, Box<[T]>>(vec) }
        })
    } else if array_class == format!("[L{};", T::CLASS_PATH) {
        // Array contains Objects of primitives (e.g. java/lang/Integer).
        // Must convert each object to the primitive
        let array = get_object_array(obj, None, env)?;
        Result::from_iter(array.iter()
            .map(|object| T::from_object_env(&object, env))
            .collect::<Box<[_]>>()
        )
    } else {
        Err(FromObjectError::ClassMismatch {
            obj_class: array_class,
            target_class: Some(format!("[L{};", T::JNAME))
        })
    }
}

/// Creates a Rust [`Vec`] of [`JObject`]s by reading elements from a **Java Array** (**obj**).
/// 
/// Also checks that the Array Object's class matches the **array_class** (if it is [`Some`])
/// and `panic!s` if they don't match.
/// 
/// If the Object should be converted to a *Rust Type* (e.g. `String`),
/// then use [`get_object_array_converted()`] instead.
pub fn get_object_array<'local>(obj: &JObject<'_>, elem_class: Option<&str>, env: &mut JNIEnv<'local>) -> Result<Box<[JObject<'local>]>, FromObjectError> {
    if obj.is_null() {
        return Err(FromObjectError::Null);
    }

    let array = <&JObjectArray>::from(obj);
    // Check object's type
    if let Some(class) = elem_class {
        // For some reason, class.getName() returns a ClassPath with .dots. instead of /slashes/, so the sig_type needs to be transformed.
        // This is the only place where this happens. why??
        let class = format!("[L{};", java_path_to_dot_notation(class));
        let obj_class = call!(env=> call!(env=> obj.getClass() -> Class).getName() -> String);
        if obj_class != class {
            return Err(FromObjectError::ClassMismatch { obj_class, target_class: Some(class) })
        }
    }

    let len = env.get_array_length(array)
        .map_err(|err| FromObjectError::Other(format!("Failed to check Array's length: {err}")))?
        as usize;
    // Allocate array
    let mut vec = Vec::with_capacity(len);

    // Fill array
    for i in 0..len {
        vec.push(env.get_object_array_element(array, i as jsize)
            .map_err(|err| FromObjectError::Other(format!("Failed to read Array elements: {err}")))?
        );
    }

    Ok(vec.into_boxed_slice())
}
/// Like [`get_object_array()`], but allows passing a function to convert the each [`JObject`] to a *Rust type*.
pub fn get_object_array_converted<'local, T>(
    obj: &JObject<'_>,
    elem_class: Option<&str>,
    elem_conversion: fn(&JObject<'_>, &mut JNIEnv<'local>) -> T,
    env: &mut JNIEnv<'local>
) -> Result<Box<[T]>, FromObjectError>
where T: 'local {
    Ok(get_object_array(obj, elem_class, env)?
        .into_iter()
        .map(|obj| elem_conversion(&obj, env))
        .collect()
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
pub fn create_object_array<'local, 'other>(items: &[impl AsRef<JObject<'other>>], elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
    // Allocate the array
    let array = env.new_object_array(
        items.len() as jsize,
        elem_class,
        JObject::null()
    )
        .unwrap_or_else(|err| panic!("Failed to create Java Object \"{elem_class}\" array: {err}"));

    // Fill the array
    for (i, element) in items.iter().enumerate() {
        env.set_object_array_element(&array, i as jsize, element)
            .unwrap_or_else(|err| panic!("Failed to set the value of Object array at index {i}: {err}"));
    }

    array.into()
}
/// Like [`create_object_array()`], but performs **conversion** on each element of the **slice*
/// before they are added to the *Java Array*.
/// 
/// This function also creates a new local frame, which will delete any newly created references.
/// This means the called can call [`.to_object()`][crate::ToObject::to_object()],
/// which allocates new object references, without unnecessary leakage.
pub fn create_object_array_converted<'local, T>(
    slice: &[T],
    elem_class: &str,
    elem_conversion: for<'other> fn(&T, &mut JNIEnv<'other>) -> JObject<'other>,
    env: &mut JNIEnv<'local>
) -> JObject<'local> {
    // Push a new Local Frame because the elem_conversion will need to create new Objects that should be dropped when this call finishes.
    env.with_local_frame_returning_local(slice.len() as i32, |env| -> Result<JObject, jni::errors::Error> {
        let items = slice.iter()
            .map(#[inline] |t| elem_conversion(t, env))
            .collect::<Box<[_]>>(); // Must collect in box to consume iterator, which holds a refernce to env
        Ok(create_object_array(&items, elem_class, env))
    }).unwrap()
}
