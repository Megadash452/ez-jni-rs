//! Contains helper functions for the macros in `/jni_macros/src/object.rs`.
use std::borrow::Cow;
use jni::{
    JNIEnv, errors::Error as JNIError, objects::{GlobalRef, JClass, JObject, JString, JThrowable, JValue, JValueGen, JValueOwned}
};
use crate::{call, Class, FromJValue, FromJValueError, FromObject, FromObjectError, JValueType, __throw::{get_jni_error_msg, try_catch}, utils::{get_object_class_name, ResultExt}};
use super::{field_helper, getter_name_and_sig, FieldNotFound, MethodNotFound};

/// Trait for `call!` to use to convert the return value.
/// Only used in the macros, so this will `panic!` on error.
pub trait FromJValueOwned<'obj, 'local> {
    fn from_jvalue_owned_env(val: JValueOwned<'obj>, env: &mut JNIEnv<'local>) -> Self;
}

/// The same as [`FromJValueOwned`], but used only to convert **fields** in the [`FromObject`] derive macro.
/// 
/// This separate trait is necessary because `call!` does not need to check the *return* Object's **Class**,
/// but the check is needed on a [`JObject`] or [`JThrowable`] field in the [`FromObject`] derive.
/// To enforce this check, [`JObject`] or [`JThrowable`] do NOT implement this trait, but do implement [`FromJValueOwned`].
pub trait FieldFromJValue<'obj, 'local> {
    fn field_from_jvalue_owned_env(val: JValueOwned<'obj>, env: &mut JNIEnv<'local>) -> Self;
    /// Guesses the *signature* that `Self` expects from a [`JValue`][jni::objects::JValue].
    /// Returns a *Java Type signature* with the [`Class`] if the signature is for Object.
    /// 
    /// This is done by attempting a call to [`FromJValue::from_jvalue_env()`] with a `Boolean(true)`
    /// and checking the *expected type* in the [`FromJValueError`][crate::FromJValueError].
    /// Though, some types just provide a hardcoded value.
    /// 
    /// The [`JNIEnv`] is not used but it needs to be passed to [`FromJValue::from_jvalue_env()`].
    fn guess_sig(env: &mut JNIEnv<'local>) -> Cow<'static, str>;
}

/// Trait that allows instantiating an [`Object`][crate::FromObjectOwned] *Rust type* [`From a JValue`][crate::FromJValue] while also
/// allowing the user to require that the Object is an instance of a specified **Class**.
/// 
/// Most types already require the object to be of thier specific **Class** in their [`FromObject`] implementations.
/// However, for [`JObject`] and [`JThrowable`] it's vague and they can be almost any **Class**.
/// Although all types implement this trait for thoroughness in the macros,
/// only the implementation of [`JObject`] and [`JThrowable`] check that the **Class** matches the **class** argument.
/// 
/// This trait also forces the use of the [`FromObject`] instead of [`FromJValue`].
/// Since a **class** is being specified, it is expected that the [`JValue`] is an [`Object`][jni::objects::JValueGen::Object].
pub trait FromJValueClass<'obj, 'local> {
    /// Same as [`from_jvalue_owned_env`][FromJValueOwned::from_jvalue_owned_env()], but also checks that the object is an instance of a specified **Class** for [`JObject`] and [`JThrowable`].
    fn from_jvalue_class(val: JValueOwned<'obj>, class: &'static str, env: &mut JNIEnv<'local>) -> Self;
}

/// Converts a [`JValueOwned`] to a [`JObject`] without performing any checks on the [`JObject`].
/// 
/// Panics if **val** is not an [`Object`][JValueGen::Object].
fn jvalue_to_jobject<'obj>(val: JValueOwned<'obj>) -> JObject<'obj> {
    match val {
        JValueGen::Object(object) => object,
        val => panic!("{}", FromJValueError::IncorrectType {
            actual: JValueType::from(val.borrow()),
            expected: JValueType::Object,
        })
    }
}

/// Creates implementation of [`FromJValueOwned`] and [`FromJValueClass`] for [`JObject`] and its wrappers, and for `Option<T>`.
macro_rules! impl_from_object_class {
    // A class is required to be passed in to convert from a JValue,
    // so FromJValueOwned is not implemented.
    (@class $ty:ty) => {
        impl_from_object_class!(__owned $ty);
        impl<'obj> FromJValueClass<'obj, '_> for $ty {
            fn from_jvalue_class(val: JValueOwned<'obj>, class: &'static str, env: &mut JNIEnv<'_>) -> Self {
                let object = <Self as FromJValueOwned>::from_jvalue_owned_env(val, env);
                check_object_class(&object, class, env).unwrap_display();
                object
            }
        }
        impl<'obj> FromJValueClass<'obj, '_> for Option<$ty> {
            fn from_jvalue_class(val: JValueOwned<'obj>, class: &'static str, env: &mut JNIEnv<'_>) -> Self {
                let object = <Self as FromJValueOwned>::from_jvalue_owned_env(val, env)?;
                check_object_class(&object, class, env).unwrap_display();
                Some(object)
            }
        }
    };
    // A class is NOT required, so implements both FromJValueOwned and FromJValueClass
    ($ty:ty) => {
        impl_from_object_class!(__owned $ty);
        impl<'obj> FieldFromJValue<'obj, '_> for $ty {
            #[inline(always)]
            fn field_from_jvalue_owned_env(val: JValueOwned<'obj>, env: &mut JNIEnv<'_>) -> Self {
                <Self as FromJValueOwned>::from_jvalue_owned_env(val, env)
            }
            fn guess_sig(_: &mut JNIEnv<'_>) -> Cow<'static, str> {
                Cow::Owned(format!("L{};", <$ty as Class>::class()))
            }
        }
        impl<'obj> FieldFromJValue<'obj, '_> for Option<$ty> {
            #[inline(always)]
            fn field_from_jvalue_owned_env(val: JValueOwned<'obj>, env: &mut JNIEnv<'_>) -> Self {
                <Self as FromJValueOwned>::from_jvalue_owned_env(val, env)
            }
            #[inline(always)]
            fn guess_sig(env: &mut JNIEnv<'_>) -> Cow<'static, str> {
                <$ty as FieldFromJValue>::guess_sig(env)
            }
        }
        impl<'obj> FromJValueClass<'obj, '_> for $ty {
            #[inline(always)]
            fn from_jvalue_class(val: JValueOwned<'obj>, _: &'static str, env: &mut JNIEnv<'_>) -> Self {
                <Self as FromJValueOwned>::from_jvalue_owned_env(val, env)
            }
        }
        impl<'obj> FromJValueClass<'obj, '_> for Option<$ty> {
            #[inline(always)]
            fn from_jvalue_class(val: JValueOwned<'obj>, _: &'static str, env: &mut JNIEnv<'_>) -> Self {
                <Self as FromJValueOwned>::from_jvalue_owned_env(val, env)
            }
        }
    };
    (__owned $ty:ty) => {
        impl<'obj> FromJValueOwned<'obj, '_> for $ty {
            fn from_jvalue_owned_env(val: JValueOwned<'obj>, env: &mut JNIEnv<'_>) -> Self {
                let object = jvalue_to_jobject(val);
                // Call from_object() to perform checks
                <Self as $crate::FromObjectOwned>::from_object_owned_env(object, env).unwrap_display()
            }
        }
        impl<'obj> FromJValueOwned<'obj, '_> for Option<$ty> {
            fn from_jvalue_owned_env(val: JValueOwned<'obj>, env: &mut JNIEnv<'_>) -> Self {
                let object = jvalue_to_jobject(val);
                if object.is_null() {
                    return None;
                }

                // Call from_object() to perform checks
                Some(<$ty as $crate::FromObjectOwned>::from_object_owned_env(object, env).unwrap_display())
            }
        }
    }
}
impl_from_object_class!(@class JObject<'obj>);
impl_from_object_class!(@class JThrowable<'obj>);
impl_from_object_class!(@class GlobalRef);
impl_from_object_class!(JClass<'obj>);
impl_from_object_class!(JString<'obj>);

impl<'local, T> FromJValueOwned<'_, 'local> for T
where T: FromJValue<'local> {
    #[inline(always)] // inline(always) to prevent excesive creation of functions (especially since this is only used in a macro)
    fn from_jvalue_owned_env(val: JValueOwned<'_>, env: &mut JNIEnv<'local>) -> Self {
        Self::from_jvalue_env(val.borrow(), env).unwrap_display()
    }
}

impl<'local, T> FieldFromJValue<'_, 'local> for T
where T: FromJValue<'local> + Class {
    #[inline(always)]
    fn field_from_jvalue_owned_env(val: JValueOwned<'_>, env: &mut JNIEnv<'local>) -> Self {
        Self::from_jvalue_owned_env(val, env)
    }
    fn guess_sig(env: &mut JNIEnv<'local>) -> Cow<'static, str> {
        match T::from_jvalue_env(JValue::Bool(1), env) {
            Ok(_) => Cow::Borrowed("Z"),
            Err(FromJValueError::Object(_)) => panic!("Encountered FromObjectError when JValue is Bool; FromJValue implementation can only call FromObject if JValue is Object"),
            Err(FromJValueError::IncorrectType { expected, .. }) => match expected {
                JValueType::Bool   => panic!("Unreachable"),
                JValueType::Void   => Cow::Borrowed("V"),
                JValueType::Char   => Cow::Borrowed("C"),
                JValueType::Byte   => Cow::Borrowed("B"),
                JValueType::Short  => Cow::Borrowed("S"),
                JValueType::Int    => Cow::Borrowed("I"),
                JValueType::Long   => Cow::Borrowed("J"),
                JValueType::Float  => Cow::Borrowed("F"),
                JValueType::Double => Cow::Borrowed("D"),
                JValueType::Object => {
                    let class = T::class();
                    // L sig does not need to be prepended if class is array.
                    if class.starts_with('[')
                    || (class.starts_with('L') && class.ends_with(';')) {
                        class
                    } else {
                        // Prepend L if it isn't already
                        Cow::Owned(format!("L{class};"))
                    }
                },
            }
        }
    }
}

impl<'obj, 'local, T> FromJValueClass<'obj, 'local> for T
where T: FromObject<'local> {
    #[inline(always)] // inline(always) to prevent excesive creation of functions (especially since this is only used in a macro)
    fn from_jvalue_class(val: JValueOwned<'obj>, _: &'static str, env: &mut JNIEnv<'local>) -> Self {
        // Force use of FromObject
        Self::from_object_env(&jvalue_to_jobject(val), env).unwrap_display()
    }
}

/// Get the value of a **field** from an Object.
/// 
/// This function is the same as [`get_obj_field()`][crate::utils::get_obj_field()]
/// but returns [`FieldNotFound`][FromObjectError::FieldNotFound] if the **field** or **getter** method were not found.
/// 
/// This is used by the derive macros of [`FromObject`][crate::FromObject] and [`FromObject`][crate::FromException].
pub fn from_object_get_field<'local>(object: &JObject<'_>, name: &'static str, ty: &str, env: &mut JNIEnv<'local>) -> Result<JValueOwned<'local>, FromObjectError> {
    field_helper(super::Callee::Object(object), name, ty,
        |env| env.get_field(object, name, ty),
        |env| {
            let (name, sig) = getter_name_and_sig(name, ty);
            env.call_method(object, name, sig, &[])
        },
    env)
        .map_err(|err| {
            // Create the error that will be returned before checking
            let not_found_error = FromObjectError::FieldNotFound {
                name: name.to_string(),
                ty: ty.to_string(),
                target_class: get_object_class_name(object, env)
            };
            match err {
                // NUll was already checked, will not appear here
                JNIError::FieldNotFound { .. }
                | JNIError::MethodNotFound { .. } => not_found_error,
                JNIError::JavaException => {
                    if let Some(FieldNotFound) = try_catch(env) {
                        not_found_error
                    } else if let Some(MethodNotFound) = try_catch(env) {
                        not_found_error
                    } else {
                        crate::__throw::handle_jni_call_error(JNIError::JavaException, env)
                    }
                },
                err => crate::__throw::handle_jni_call_error(err, env)
            }
        })
}

/// Helper function for the [`FromObject`][ez_jni::FromObject] [derive macro][ez_jni_macros::from_object] to check whether the *object*'s Class matches the struct's *target Class*.
pub fn check_object_class(object: &JObject, target_class: &str, env: &mut JNIEnv<'_>) -> Result<(), FromObjectError> {
    if object.is_null() {
        return Err(FromObjectError::Null);
    }

    let class = env.get_object_class(object)
        .unwrap_or_else(|err| panic!("Failed to get Object's class: {}", get_jni_error_msg(err, env)));

    let target_class_obj = env.find_class(target_class)
        .map_err(|_| FromObjectError::ClassNotFound(target_class.to_string()))?;
    
    if !env.is_instance_of(object, target_class_obj)
        .unwrap_or_else(|err| panic!("Failed check Object's classes: {}", get_jni_error_msg(err, env)))
    {
        return Err(FromObjectError::ClassMismatch {
            obj_class: call!(env=> class.getName() -> String),
            target_class: Some(target_class.to_string())
        })
    }

    Ok(())
}

// My urge to avoid duplicate code generated by the macros is running through my veins, but I MUST resist it!

// enum MyEnum { Var1, Var2 }
// impl<> crate::FromObject<'_> for MyEnum {
//     fn from_object_env(object: &JObject<'_>, env: &mut ::jni::JNIEnv<'_>) -> Result<Self, ::ez_jni::FromObjectError> {
//         ::ez_jni::utils::check_classes_enum(object, Some("java/lang/String"), ::std::boxed::Box::new([
//             Constructor { target_class: "java/lang/Int", constructor: Box::new(|env| MyEnum::Var1) },
//             Constructor { target_class: "java/lang/Float", constructor: Box::new(|env| MyEnum::Var2) },
//         ]), env)
//     }
// }
//
// #[doc(hidden)]
// pub struct Constructor<T> {
//     target_class: &'static str,
//     constructor: Box<dyn FnOnce(&mut JNIEnv<'_>) -> T>
// }
// #[doc(hidden)]
// pub fn check_classes_enum<T>(object: &JObject, base_class: Option<&'static str>, constructors: Box<[Constructor<T>]>, env: &mut JNIEnv<'_>) -> Result<T, FromObjectError> {
//     if object.is_null() {
//         return Err(FromObjectError::Null);
//     }
//
//     let class = env.get_object_class(object)
//         .unwrap_or_else(|err| panic!("Failed to get Object's class: {err}"));
//
//     if let Some(base_class) = base_class {
//         if !env.is_instance_of(object, base_class).unwrap() {
//             return Err(::ez_jni::FromObjectError::ClassMismatch {
//                 obj_class: ::ez_jni::call!(env=> class.getName() -> String),
//                 target_class: Some(base_class.to_string())
//             })
//         }
//     }
//
//     // Check if the object's Class matches any of the variants' classes and call the "constructor" for the first variant that matches.
//     for item in constructors {
//         if env.is_instance_of(object, item.target_class).unwrap() {
//             return Ok((item.constructor)(env))
//         }
//     }
//     // None of the variants matched
//     Err(::ez_jni::FromObjectError::ClassMismatch {
//         obj_class: ::ez_jni::call!(env=> class.getName() -> String),
//         target_class: None
//     })
// }
