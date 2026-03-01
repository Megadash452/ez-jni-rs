//! Contains helper functions for the macros in `/jni_macros/src/object.rs`.
use std::borrow::Cow;
use jni::{
    JNIEnv, objects::{GlobalRef, JClass, JObject, JString, JThrowable, JValue, JValueGen, JValueOwned}
};
use nonempty::NonEmpty;
use crate::{Class, FromJValue, FromObject, JValueType, call, error::{FromJValueError, FromObjectError}, utils::JniResultExt as _};
use super::{field_helper, getter_name_and_sig};

/// Trait for [`FromObject derive`][FromObject] to use to convert [`JValue`]s to **Rust Values**.
pub trait FieldFromJValue<'obj, 'local>: Class + Sized {
    fn field_from_jvalue(val: JValueOwned<'obj>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError>;

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

/// Same as [`FieldFromJValue`], but requires the user to pass in a **class** that the [`JValue`][JValueOwned] must be an instance of.
/// 
/// This also means that the [`JValue`][JValueOwned] must be an [`Object`][JValueGen::Object],
/// So the method uses the [`FromObject`] implementation instead of [`FromJValue`].
/// 
/// This separate trait is necessary because most types already implement [`Class`], so the user does not need to provide a **class** for the object.
/// However, [`JObject`], [`JThrowable`], and [`ObjectArray`][crate::array::ObjectArray] don't implement [`Class`] because users *should* specify a *descendant* **class** for these types.
/// To enforce this check in [`FromObject` derive][FromObject], these types don't implement [`FieldFromJValue`], but do implement this trait.
/// 
/// This is used for *getting* a **field** in [`FromObject derive`][FromObject].
pub trait FieldFromJValueClass<'obj, 'local>: Sized {
    /// Same as [`field_from_jvalue`][FieldFromJValue::field_from_jvalue()],
    /// but also checks that the object is an instance of a specified **Class**.
    fn field_from_jvalue_with_class(val: JValueOwned<'obj>, class: &str, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError>;
}

/// Creates implementation of [`FieldFromJValue`] and [`FieldFromJValueClass`] for [`JObject`] and its wrappers, and for `Option<T>`.
macro_rules! impl_from_object_class {
    // A class is required to be passed in to convert from a JValue,
    // so FromJValueOwned is not implemented.
    (@class $ty:ty) => {
        impl<'obj> FieldFromJValueClass<'obj, '_> for $ty {
            #[inline(always)]
            fn field_from_jvalue_with_class(val: JValueOwned<'obj>, class: &str, env: &mut JNIEnv<'_>) -> Result<Self, FromJValueError> {
                let object = jvalue_to_jobject(val)?;
                check_object_class(&object, class, env)?;
                <Self as $crate::FromObjectOwned>::from_object_owned_env(object, env)
                    .map_err(FromJValueError::from)
            }
        }
        impl<'obj> FieldFromJValueClass<'obj, '_> for Option<$ty> {
            #[inline(always)]
            fn field_from_jvalue_with_class(val: JValueOwned<'obj>, class: &str, env: &mut JNIEnv<'_>) -> Result<Self, FromJValueError> {
                let object = jvalue_to_jobject(val)?;
                // Only do class check if Object is not NULL
                if !object.is_null() {
                    check_object_class(&object, class, env)?;
                }
                <Self as $crate::FromObjectOwned>::from_object_owned_env(object, env)
                    .map_err(FromJValueError::from)
            }
        }
    };
    // A class is NOT required, so implements both traits
    ($ty:ty) => {
        impl_from_object_class!(@class $ty);
        impl<'obj> FieldFromJValue<'obj, '_> for $ty {
            #[inline(always)]
            fn field_from_jvalue(val: JValueOwned<'obj>, env: &mut JNIEnv<'_>) -> Result<Self, FromJValueError> {
                let object = jvalue_to_jobject(val)?;
                <Self as $crate::FromObjectOwned>::from_object_owned_env(object, env)
                    .map_err(FromJValueError::from)
            }
            fn guess_sig(_: &mut JNIEnv<'_>) -> Cow<'static, str> {
                Cow::Owned(format!("L{};", <$ty as Class>::class()))
            }
        }
        impl<'obj> FieldFromJValue<'obj, '_> for Option<$ty> {
            #[inline(always)]
            fn field_from_jvalue(val: JValueOwned<'obj>, env: &mut JNIEnv<'_>) -> Result<Self, FromJValueError> {
                if let JValueGen::Object(obj) = &val
                && obj.is_null() {
                    Ok(None)
                } else {
                    Ok(Some(<$ty as FieldFromJValue>::field_from_jvalue(val, env)?))
                }
            }
            #[inline(always)]
            fn guess_sig(env: &mut JNIEnv<'_>) -> Cow<'static, str> {
                <$ty as FieldFromJValue>::guess_sig(env)
            }
        }
    }
}
impl_from_object_class!(@class JObject<'obj>);
impl_from_object_class!(@class JThrowable<'obj>);
impl_from_object_class!(@class GlobalRef);
// TODO: AutoLocal??
// These types don't require the user to specify a class:
impl_from_object_class!(JClass<'obj>);
impl_from_object_class!(JString<'obj>);

impl<'local, T> FieldFromJValue<'_, 'local> for T
where T: FromJValue<'local> + Class {
    #[inline(always)]
    fn field_from_jvalue(val: JValueOwned<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        <Self as FromJValue>::from_jvalue_env(val.borrow(), env)
    }
    #[inline(always)]
    fn guess_sig(env: &mut JNIEnv<'local>) -> Cow<'static, str> {
        match T::from_jvalue_env(JValue::Bool(1) /* Fake value */, env) {
            Ok(_) => Cow::Borrowed("Z"),
            Err(err) => __guess_sig(err, <Self as Class>::class)
        }
    }
}
// In the name of space-efficiency with generic types...
fn __guess_sig(err: FromJValueError, get_class: fn() -> Cow<'static, str>) -> Cow<'static, str> {
    match err {
        FromJValueError::Object(_) => panic!("Encountered FromObjectError when JValue is Bool; FromJValue implementation can only call FromObject if JValue is Object"),
        FromJValueError::IncorrectType { expected, .. } => match expected {
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
                let class = get_class();
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

impl<'local, T> FieldFromJValueClass<'_, 'local> for T
where T: FromObject<'local> {
    #[inline(always)]
    fn field_from_jvalue_with_class(val: JValueOwned<'_>, class: &str, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        let object = jvalue_to_jobject(val)?;
        check_object_class(&object, class, env)?;
        <Self as FromObject>::from_object_env(&object, env)
            .map_err(FromJValueError::from)
    }
}

/// Converts a [`JValueOwned`] to a [`JObject`] without performing any checks on the [`JObject`].
/// 
/// Also used in [`call!`] to convert **Object References** *From JValue*.
pub fn jvalue_to_jobject<'obj>(val: JValueOwned<'obj>) -> Result<JObject<'obj>, FromJValueError> {
    match val {
        JValueGen::Object(object) => Ok(object),
        val => Err(FromJValueError::IncorrectType {
            actual: JValueType::from(val.borrow()),
            expected: JValueType::Object,
        })
    }
}

/// Get the value of a **field** from an Object.
/// 
/// This function is the same as [`get_obj_field()`][crate::utils::get_obj_field()]
/// but returns [`FieldNotFound`][FromObjectError::FieldNotFound] if the **field** or **getter** method were not found.
/// 
/// This is used by [`FromObject` derive][crate::FromObject].
pub fn from_object_get_field<'local>(object: &JObject<'_>, name: &'static str, ty: &str, env: &mut JNIEnv<'local>) -> Result<JValueOwned<'local>, FromObjectError> {
    field_helper(super::Callee::Object(object), name, ty,
        |env| env.get_field(object, name, ty),
        |env| {
            let (name, sig) = getter_name_and_sig(name, ty);
            env.call_method(object, name, sig, &[])
        },
    env)
        .map_err(FromObjectError::from)
}

/// Helper function for the [`FromObject`][ez_jni::FromObject] [derive macro][ez_jni_macros::FromObject] to check whether the *object*'s Class matches the struct's *target Class*.
pub fn check_object_class(object: &JObject, target_class: &str, env: &mut JNIEnv<'_>) -> Result<(), FromObjectError> {
    if object.is_null() {
        return Err(FromObjectError::Null);
    }

    let class = env.get_object_class(object)
        .catch(env)
        .map_err(|err| FromObjectError::from_jni_with_msg("Failed to get Object's class", err))?;

    let target_class_obj = env.find_class(target_class).catch(env)?;
    
    if !env.is_instance_of(object, target_class_obj).catch(env)? {
        return Err(FromObjectError::ClassMismatch {
            obj_class: call!(env=> class.getName() -> String),
            target_classes: NonEmpty::new(target_class.to_string()),
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
