use jni::{
    errors::Error as JNIError, objects::{JClass, JObject, JObjectArray, JPrimitiveArray, JString, JValueOwned}, sys::jsize, JNIEnv
};
use crate::{call, object::FromObjectError, FromException, __throw::{panic_exception, try_catch}};
use utils::{first_char_uppercase, java_path_to_dot_notation};

#[doc(hidden)]
pub use cfg_if;

#[cfg(target_os = "android")]
pub use android::*;

#[cfg(target_os = "android")]
mod android {
    use jni::JNIEnv;
    use ez_jni::call;
    
    #[doc(hidden)]
    /// Does the printing for [`jni_macros::println!`].
    pub fn __println(s: String, env: &mut JNIEnv) {
        call!(static android.util.Log.i(java.lang.String("Rust"), java.lang.String(s)) -> int);
    }
    
    #[doc(hidden)]
    /// Does the printing for [`jni_macros::eprintln!`].
    pub fn __eprintln(s: String, env: &mut JNIEnv) {
        call!(static android.util.Log.e(java.lang.String("Rust"), java.lang.String(s)) -> int);
    }

    // This test will NOT run because it only exists in Android build,
    // but there is no Android device to run it,
    // so it is impossible to test if the call to android.util.Log will be successful.
    // However, it will be built with Android compiler by github workflows,
    // so it is able to test if the macro outputs correct tokens.
    #[doc(hidden)]
    fn print(env: &mut JNIEnv) {
        use ez_jni::{println, eprintln};
        println!("Hello, World!");
        eprintln!("Hello, World!");
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
/// Create a *Java String* from a *Rust [`String`]*.
pub fn new_string<'local>(s: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
    env.new_string(s)
        .unwrap_or_else(|err| panic!("Error creating Java String: {err}"))
        .into()
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

/// A wrapper for a [`Class`] value that will be used in a **JNI Call**.
pub enum ClassRepr<'a, 'local> {
    /// The class is represented by a **ClassPath** in [`String`][str] form.
    /// An actual *Class Object* will be looked up in the *JNI Call*.
    String(&'static str),
    /// The class is represented by a *Class Object* that has already been looked up
    /// and can be operated on with JNI.
    Object(&'a JClass<'local>),
}

/// Access a **field** of a *Java Object*, given the **name** of the field and its type **signature**.
/// 
/// If the field does not exists this will call a *getter method* with `get` prepended to the field's name.
/// E.g. if the field `java.lang.String message` did not exist,
/// then `java.lang.String getMessage()` will be called.
/// 
/// This function already handles any errors returned by the *JNI Call*.
pub fn get_obj_field<'local>(object: &JObject<'_>, name: &'static str, sig: &'static str, env: &mut JNIEnv<'local>) -> JValueOwned<'local> {
    get_field_impl(GetFieldCallee::Object(object), name, sig, env)
}

/// Like [`get_obj_field()`], but gets a `static` field of a **Class**.
pub fn get_static_field<'local>(class: ClassRepr<'_, '_>, name: &'static str, sig: &'static str, env: &mut JNIEnv<'local>) -> JValueOwned<'local> {
    let callee = match class {
        ClassRepr::String(class) => GetFieldCallee::Class(class),
        ClassRepr::Object(class) => GetFieldCallee::ClassObject(class),
    };
    get_field_impl(callee, name, sig, env)
}

enum GetFieldCallee<'a, 'local> {
    Class(&'static str),
    ClassObject(&'a JClass<'local>),
    Object(&'a JObject<'local>),
}

fn get_field_impl<'local>(callee: GetFieldCallee<'_, '_>, name: &'static str, sig: &'static str, env: &mut JNIEnv<'local>) -> JValueOwned<'local> {
    #[derive(FromException)]
    #[class(java.lang.NoSuchFieldError)]
    struct FieldNotFound;

    // If a Java field was not found, will try to call a Getter method.
    let call_getter = |env: &mut JNIEnv<'local>| {
        let name = format!("get{}", first_char_uppercase(name));
        let sig = format!("(){sig}");

        // Call Object/Static method depending on callee
        match callee {
            GetFieldCallee::Class(class) => env.call_static_method(class, name, sig, &[]),
            GetFieldCallee::ClassObject(class) => env.call_static_method(class, name, sig, &[]),
            GetFieldCallee::Object(object) => env.call_method(object, name, sig, &[]),
        }
        // TODO: perhaps check if a field with that name exists with Class.getFields()
    };

    // Get Object/Static field depending on callee
    let call_result = match callee {
        GetFieldCallee::Class(class) => env.get_static_field(class, name, sig),
        GetFieldCallee::ClassObject(class) => env.get_static_field(class, name, sig),
        GetFieldCallee::Object(object) => env.get_field(object, name, sig),
    };

    call_result
        .or_else(|err| match err {
            JNIError::FieldNotFound { .. } => call_getter(env),
            JNIError::JavaException => {
                if let Some(FieldNotFound) = try_catch(env) {
                    // Try calling Getter if field not found
                    call_getter(env)
                } else {
                    // Other unexpected exception
                    Err(JNIError::JavaException)
                }
            },
            err => Err(err)
        })
        .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env))
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
    // let ty = call!(class.getName() -> String);
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
        let obj_class = call!(obj_class.getName() -> String);
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
    Ok(IntoIterator::into_iter(::ez_jni::utils::get_object_array(obj, array_class, env)?)
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
