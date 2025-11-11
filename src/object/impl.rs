use std::mem::MaybeUninit;
use jni::objects::{GlobalRef, JClass, JString};
use crate::{__throw::get_jni_error_msg, Class, new, utils::check_object_class};
use super::*;

impl<T> ToObject for &T
where T: ToObject {
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <T as ToObject>::to_object_env(self, env)
    }
}

// Implementation for Option type

impl<'a, 'obj, 'local, T> FromObject<'a, 'obj, 'local> for Option<T>
where T: FromObject<'a, 'obj, 'local> {
    fn from_object_env(object: &'a JObject<'obj>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        if object.is_null() {
            Ok(None)
        } else {
            T::from_object_env(object, env).map(Some)
        }
    }
}
impl<T> ToObject for Option<T>
where T: ToObject {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        match self {
            Some(t) => t.to_object_env(env),
            None => JObject::null()
        }
    }
}

// --- Implementation for Array Types

impl<'local, T> FromObject<'_, '_, 'local> for Box<[T]>
where T: for<'a, 'obj> FromObject<'a, 'obj, 'local> {
    #[inline(always)]
    fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        todo!()
    }
}
impl<'local, T> FromObject<'_, '_, 'local> for Vec<T>
where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local> {
    fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        Ok(Box::<[T]>::from_object_env(object, env)?.into_vec())
    }
}
impl<'local, const N: usize, T> FromObject<'_, '_, 'local> for [T; N]
where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local> {
    fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        // Get an unized array from the Java Array
        let boxed = Box::<[T]>::from_object_env(object, env)?;
        if boxed.len() != N {
            return Err(FromObjectError::ArrayTooLong { expected_len: N, actual_len: boxed.len() });
        }

        // SAFETY: taken from uninit crate: https://docs.rs/uninit/0.6.2/uninit/macro.uninit_array.html, but can't use it directly because of generic const
        let mut array = unsafe { MaybeUninit::<[MaybeUninit<T>; N]>::uninit().assume_init() };
        // Move the elements from the unsized array to the sized/inline array
        for (i, v) in boxed.into_iter().enumerate() {
            array[i] = MaybeUninit::new(v)
        }

        Ok(array.map(|v| unsafe { v.assume_init() }))
    }
}

impl<T> ToObject for [T]
where T: ToObject + Class + Sized {
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        todo!()
    }
}
impl<T> ToObject for &[T]
where [T]: ToObject {
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <[T] as ToObject>::to_object_env(&self, env)
    }
}
impl<const N: usize, T> ToObject for [T; N]
where [T]: ToObject {
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <[T] as ToObject>::to_object_env(self.as_ref(), env)
    }
}

// ---

// TODO: Support Box for any type

// TODO: Support Callbacks

// Implementation for String types

impl FromObject<'_, '_, '_> for String {
    /// Get a [`String`] from some random Object.
    /// 
    /// Don't use this function, it only exist for compatibility.
    /// Use [`get_string()`][crate::utils::get_string] instead because you will mostly be using it with [`JString`][jni::objects::JString].
    fn from_object_env(object: &JObject, env: &mut JNIEnv) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        // Already checked that it is java.lang.String and is not NULL
        Ok(unsafe {
            env.get_string_unchecked(object.into())
                .unwrap_or_else(|err| panic!("ENV error while getting String: {err}"))
                .into()
        })
    }
}
impl ToObject for String {
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <str as ToObject>::to_object_env(self, env)
    }
}
impl ToObject for str {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        env.new_string(self)
            .unwrap_or_else(|err| panic!("Error converting Rust string to Java String: {err}"))
            .into()
    }
}
impl ToObject for &str {
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <str as ToObject>::to_object_env(self, env)
    }
}

// Implementation for number types
// Can't put condense this with macro_rules for some reason (???)
impl FromObject<'_, '_, '_> for i8 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        Ok(call!(env=> object.byteValue() -> byte))
    }
}
impl ToObject for i8 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Byte(byte(*self)))
    }
}
impl FromObject<'_, '_, '_> for i16 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        Ok(call!(env=> object.shortValue() -> short))
    }
}
impl ToObject for i16 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Short(short(*self)))
    }
}
impl FromObject<'_, '_, '_> for i32 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        Ok(call!(env=> object.intValue() -> int))
    }
}
impl ToObject for i32 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Integer(int(*self)))
    }
}
impl FromObject<'_, '_, '_> for i64 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        Ok(call!(env=> object.longValue() -> long))
    }
}
impl ToObject for i64 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Long(long(*self)))
    }
}
impl FromObject<'_, '_, '_> for f32 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        Ok(call!(env=> object.floatValue() -> float))
    }
}
impl ToObject for f32 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Float(float(*self)))
    }
}
impl FromObject<'_, '_, '_> for f64 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        Ok(call!(env=> object.doubleValue() -> double))
    }
}
impl ToObject for f64 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Double(double(*self)))
    }
}

// Implementation for unsigned number types
impl FromObject<'_, '_, '_> for u8 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        Ok(call!(env=> object.byteValue() -> u8))
    }
}
impl ToObject for u8 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Byte(u8(*self)))
    }
}
impl FromObject<'_, '_, '_> for u16 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        Ok(call!(env=> object.shortValue() -> u16))
    }
}
impl ToObject for u16 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Short(u16(*self)))
    }
}
impl FromObject<'_, '_, '_> for u32 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        Ok(call!(env=> object.intValue() -> u32))
    }
}
impl ToObject for u32 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Integer(u32(*self)))
    }
}
impl FromObject<'_, '_, '_> for u64 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        Ok(call!(env=> object.longValue() -> u64))
    }
}
impl ToObject for u64 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Long(u64(*self)))
    }
}

// Implementations for other primitives

impl FromObject<'_, '_, '_> for bool {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        Ok(call!(env=> object.booleanValue() -> boolean))
    }
}
impl ToObject for bool {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Boolean(boolean(*self)))
    }
}

impl FromObject<'_, '_, '_> for char {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        Ok(call!(env=> object.charValue() -> char))
    }
}
impl ToObject for char {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Character(char(*self)))
    }
}

/// Hidden trait for a macro to use to convert a java value.
/// Only used in the macros and for element conversion in [`FromArrayObject`].
#[doc(hidden)]
pub trait FromObjectOwned<'obj>: Sized {
    fn from_object_owned_env(object: JObject<'obj>, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError>;
}
#[doc(hidden)]
impl<'obj> FromObjectOwned<'obj> for JObject<'obj> {
    #[inline]
    fn from_object_owned_env(object: JObject<'obj>, _: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        // Call from_object() to perform checks
        <&Self>::from_object(&object)?;
        Ok(object)
    }
}
#[doc(hidden)]
impl<'obj> FromObjectOwned<'obj> for JClass<'obj> {
    fn from_object_owned_env(object: JObject<'obj>, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        // Call from_object() to perform checks
        <&Self>::from_object_env(&object, env)?;
        Ok(Self::from(object))
    }
}
#[doc(hidden)]
impl<'obj> FromObjectOwned<'obj> for JThrowable<'obj> {
    fn from_object_owned_env(object: JObject<'obj>, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        // Call from_object() to perform checks
        <&Self>::from_object_env(&object, env)?;
        Ok(Self::from(object))
    }
}
#[doc(hidden)]
impl<'obj> FromObjectOwned<'obj> for JString<'obj> {
    fn from_object_owned_env(object: JObject<'obj>, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        // Call from_object() to perform checks
        <&Self>::from_object_env(&object, env)?;
        Ok(Self::from(object))
    }
}
impl FromObjectOwned<'_> for GlobalRef {
    fn from_object_owned_env(object: JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        if object.is_null() {
            return Err(FromObjectError::Null);
        }
        env.new_global_ref(object)
            .map_err(|error| FromObjectError::Other(format!("Failed to create new Global reference: {}", get_jni_error_msg(error, env))))
    }
}
impl<'obj, T> FromObjectOwned<'obj> for Option<T>
where T: FromObjectOwned<'obj> {
    fn from_object_owned_env(object: JObject<'obj>, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(if object.is_null() {
            None
        } else {
            Some(T::from_object_owned_env(object, env)?)
        })
    }
}
