//! Contains helper functions for the macros in `/jni_macros/src/object.rs`.
use jni::{
    errors::Error as JNIError,
    objects::{JObject, JValueOwned},
    JNIEnv
};
use utils::first_char_uppercase;
use crate::{call, FromException, FromObjectError, __throw::{panic_exception, try_catch}};

#[derive(FromException)]
#[class(java.lang.NoSuchFieldError)]
struct FieldNotFound;

#[derive(FromException)]
#[class(java.lang.NoSuchMethodError)]
struct MethodNotFound;

/// Read a **field** from a Java Object, given the **name** of the field and its **ty**pe.
/// 
/// If the field does not exists and **getter_fallback** is `true`,
/// this will call a *getter method* with `get` prepended to the field's name.
/// E.g. if the field `java.lang.String message` did not exist,
/// then `java.lang.String getMessage()` will be called.
///
/// This is used by the derive macros of [`FromObject`][crate::FromObject] and [`FromObject`][crate::FromException].
/// This might be removed in the future in favor of [`get_obj_field()`].
#[doc(hidden)]
pub fn get_field<'local>(
    object: &JObject,
    name: &str,
    ty: &str,
    getter_fallback: bool,
    env: &mut JNIEnv<'local>,
) -> Result<JValueOwned<'local>, FromObjectError> {
    let class = env.get_object_class(object)
        .unwrap_or_else(|err| panic!("Error gettig object's Class: {err}"));
    let class = call!(env=> class.getName() -> String);

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
/// This is used by the derive macros of [`FromObject`][crate::FromObject] and [`FromObject`][crate::FromException].
/// This might be removed in the future in favor of [`get_obj_field()`].
#[doc(hidden)]
pub fn call_getter<'local>(
    object: &JObject,
    mathod_name: &str,
    ty: &str,
    env: &mut JNIEnv<'local>,
) -> Result<JValueOwned<'local>, FromObjectError> {
    let class = env.get_object_class(object)
        .unwrap_or_else(|err| panic!("Error gettig object's Class: {err}"));
    let class = call!(env=> class.getName() -> String);

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
