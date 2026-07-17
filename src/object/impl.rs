use std::mem::MaybeUninit;
use jni::objects::{GlobalRef, JClass, JString};
use crate::{Class, private::SealedMethod, utils::{JniResultExt as _, check_object_class, create_java_prim_array, get_java_prim_array}};
use super::*;

impl<T> ToObject for &T
where T: ToObject {
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        <T as ToObject>::to_object_env(self, env)
    }
}

// Implementation for Option type

impl<'a, 'obj, 'local, T> FromObject<'local> for Option<T>
where T: FromObject<'local> {
    #[inline(always)]
    fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        if object.is_null() {
            Ok(None)
        } else {
            T::from_object_env(object, env).map(Some)
        }
    }
}
impl<T> ToObject for Option<T>
where T: ToObject {
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        match self {
            Some(t) => t.to_object_env(env),
            None => Ok(JObject::null())
        }
    }
}

// --- Implementation for Array Types

impl<'local, T> FromObject<'local> for Box<[T]>
where T: for<'a, 'obj> FromObject<'local> {
    #[inline(always)]
    fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        T::__from_array_object(object, env, SealedMethod)
    }
}
impl<'local, T> FromObject<'local> for Vec<T>
where Box<[T]>: for<'a, 'obj> FromObject<'local> {
    #[inline(always)]
    fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        Ok(Box::<[T]>::from_object_env(object, env)?.into_vec())
    }
}
impl<'local, const N: usize, T> FromObject<'local> for [T; N]
where Box<[T]>: for<'a, 'obj> FromObject<'local> {
    fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        // Get an unsized array from the Java Array.
        // Note: Can't fill Java elements straight into the stack array because
        // the __from_array_object() implementation can treat primitives and Objects differently (and it returns Box<[T]>).
        let boxed = Box::<[T]>::from_object_env(object, env)?;
        if boxed.len() != N {
            return Err(FromObjectError::ArraySizeMismatch {
                expected_len: N,
                actual_len: boxed.len()
            });
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
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        T::__to_array_object(self, env, crate::private::SealedMethod)
    }
}
impl<T> ToObject for &[T]
where [T]: ToObject {
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        <[T] as ToObject>::to_object_env(&self, env)
    }
}
impl<const N: usize, T> ToObject for [T; N]
where [T]: ToObject {
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        <[T] as ToObject>::to_object_env(self.as_ref(), env)
    }
}

// ---

impl<'local, T> FromObject<'local> for Box<T>
where T: FromObject<'local> {
    fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        Ok(Box::new(<T as FromObject>::from_object_env(object, env)?))
    }
}
impl<T> ToObject for Box<T>
where T: ToObject {
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        <T as ToObject>::to_object_env(self, env)
    }
}

// TODO: Support Callbacks

// Implementation for String types

impl FromObject<'_> for String {
    fn from_object_env(object: &JObject, env: &mut JNIEnv) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        // Already checked that it is java.lang.String and is not NULL
        unsafe { env.get_string_unchecked(object.into()) }
            .catch(env)
            .map(String::from)
            .map_err(FromObjectError::from)
    }
}
impl ToObject for String {
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        <str as ToObject>::to_object_env(self, env)
    }
}
impl ToObject for str {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        env.new_string(self)
            .catch(env)
            .map(JObject::from)
            .map_err(ToObjectError::from)
    }
}
impl ToObject for &str {
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        <str as ToObject>::to_object_env(self, env)
    }
}

// Implementation for number types
// Can't put condense this with macro_rules for some reason (???)
impl FromObject<'_> for i8 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        call!(env, ?=> object.byteValue() -> byte)
            .map_err(FromObjectError::from)
    }
    #[inline(always)]
    fn __from_array_object(object: &'_ JObject<'_>, env: &mut JNIEnv<'_>, _: SealedMethod) -> Result<Box<[Self]>, FromObjectError> {
        get_java_prim_array(object, env)
    }
}
impl ToObject for i8 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        call!(env, ?=> static java.lang.Byte.valueOf(byte(*self)) -> java.lang.Byte)
            .map_err(ToObjectError::from)
    }
    #[inline(always)]
    fn __to_array_object<'local>(slice: &[Self], env: &mut JNIEnv<'local>, _: SealedMethod) -> Result<JObject<'local>, ToObjectError> {
        create_java_prim_array(slice, env)
    }
}
impl FromObject<'_> for i16 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        call!(env, ?=> object.shortValue() -> short)
            .map_err(FromObjectError::from)
    }
    #[inline(always)]
    fn __from_array_object(object: &'_ JObject<'_>, env: &mut JNIEnv<'_>, _: SealedMethod) -> Result<Box<[Self]>, FromObjectError> {
        get_java_prim_array(object, env)
    }
}
impl ToObject for i16 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        call!(env, ?=> static java.lang.Short.valueOf(short(*self)) -> java.lang.Short)
            .map_err(ToObjectError::from)
    }
    #[inline(always)]
    fn __to_array_object<'local>(slice: &[Self], env: &mut JNIEnv<'local>, _: SealedMethod) -> Result<JObject<'local>, ToObjectError> {
        create_java_prim_array(slice, env)
    }
}
impl FromObject<'_> for i32 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        call!(env, ?=> object.intValue() -> int)
            .map_err(FromObjectError::from)
    }
    #[inline(always)]
    fn __from_array_object(object: &'_ JObject<'_>, env: &mut JNIEnv<'_>, _: SealedMethod) -> Result<Box<[Self]>, FromObjectError> {
        get_java_prim_array(object, env)
    }
}
impl ToObject for i32 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        call!(env, ?=> static java.lang.Integer.valueOf(int(*self)) -> java.lang.Integer)
            .map_err(ToObjectError::from)
    }
    #[inline(always)]
    fn __to_array_object<'local>(slice: &[Self], env: &mut JNIEnv<'local>, _: SealedMethod) -> Result<JObject<'local>, ToObjectError> {
        create_java_prim_array(slice, env)
    }
}
impl FromObject<'_> for i64 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        call!(env, ?=> object.longValue() -> long)
            .map_err(FromObjectError::from)
    }
    #[inline(always)]
    fn __from_array_object(object: &'_ JObject<'_>, env: &mut JNIEnv<'_>, _: SealedMethod) -> Result<Box<[Self]>, FromObjectError> {
        get_java_prim_array(object, env)
    }
}
impl ToObject for i64 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        call!(env, ?=> static java.lang.Long.valueOf(long(*self)) -> java.lang.Long)
            .map_err(ToObjectError::from)
    }
    #[inline(always)]
    fn __to_array_object<'local>(slice: &[Self], env: &mut JNIEnv<'local>, _: SealedMethod) -> Result<JObject<'local>, ToObjectError> {
        create_java_prim_array(slice, env)
    }
}
impl FromObject<'_> for f32 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        call!(env, ?=> object.floatValue() -> float)
            .map_err(FromObjectError::from)
    }
    #[inline(always)]
    fn __from_array_object(object: &'_ JObject<'_>, env: &mut JNIEnv<'_>, _: SealedMethod) -> Result<Box<[Self]>, FromObjectError> {
        get_java_prim_array(object, env)
    }
}
impl ToObject for f32 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        call!(env, ?=> static java.lang.Float.valueOf(float(*self)) -> java.lang.Float)
            .map_err(ToObjectError::from)
    }
    #[inline(always)]
    fn __to_array_object<'local>(slice: &[Self], env: &mut JNIEnv<'local>, _: SealedMethod) -> Result<JObject<'local>, ToObjectError> {
        create_java_prim_array(slice, env)
    }
}
impl FromObject<'_> for f64 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        call!(env, ?=> object.doubleValue() -> double)
            .map_err(FromObjectError::from)
    }
    #[inline(always)]
    fn __from_array_object(object: &'_ JObject<'_>, env: &mut JNIEnv<'_>, _: SealedMethod) -> Result<Box<[Self]>, FromObjectError> {
        get_java_prim_array(object, env)
    }
}
impl ToObject for f64 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        call!(env, ?=> static java.lang.Double.valueOf(double(*self)) -> java.lang.Double)
            .map_err(ToObjectError::from)
    }
    #[inline(always)]
    fn __to_array_object<'local>(slice: &[Self], env: &mut JNIEnv<'local>, _: SealedMethod) -> Result<JObject<'local>, ToObjectError> {
        create_java_prim_array(slice, env)
    }
}

// Implementation for unsigned number types
impl FromObject<'_> for u8 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        call!(env, ?=> object.byteValue() -> u8)
            .map_err(FromObjectError::from)
    }
    #[inline(always)]
    fn __from_array_object(object: &'_ JObject<'_>, env: &mut JNIEnv<'_>, _: SealedMethod) -> Result<Box<[Self]>, FromObjectError> {
        get_java_prim_array(object, env)
    }
}
impl ToObject for u8 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        call!(env, ?=> static java.lang.Byte.valueOf(u8(*self)) -> java.lang.Byte)
            .map_err(ToObjectError::from)
    }
    #[inline(always)]
    fn __to_array_object<'local>(slice: &[Self], env: &mut JNIEnv<'local>, _: SealedMethod) -> Result<JObject<'local>, ToObjectError> {
        create_java_prim_array(slice, env)
    }
}
impl FromObject<'_> for u16 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        call!(env, ?=> object.shortValue() -> u16)
            .map_err(FromObjectError::from)
    }
    #[inline(always)]
    fn __from_array_object(object: &'_ JObject<'_>, env: &mut JNIEnv<'_>, _: SealedMethod) -> Result<Box<[Self]>, FromObjectError> {
        get_java_prim_array(object, env)
    }
}
impl ToObject for u16 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        call!(env, ?=> static java.lang.Short.valueOf(u16(*self)) -> java.lang.Short)
            .map_err(ToObjectError::from)
    }
    #[inline(always)]
    fn __to_array_object<'local>(slice: &[Self], env: &mut JNIEnv<'local>, _: SealedMethod) -> Result<JObject<'local>, ToObjectError> {
        create_java_prim_array(slice, env)
    }
}
impl FromObject<'_> for u32 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        call!(env, ?=> object.intValue() -> u32)
            .map_err(FromObjectError::from)
    }
    #[inline(always)]
    fn __from_array_object(object: &'_ JObject<'_>, env: &mut JNIEnv<'_>, _: SealedMethod) -> Result<Box<[Self]>, FromObjectError> {
        get_java_prim_array(object, env)
    }
}
impl ToObject for u32 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        call!(env, ?=> static java.lang.Integer.valueOf(u32(*self)) -> java.lang.Integer)
            .map_err(ToObjectError::from)
    }
    #[inline(always)]
    fn __to_array_object<'local>(slice: &[Self], env: &mut JNIEnv<'local>, _: SealedMethod) -> Result<JObject<'local>, ToObjectError> {
        create_java_prim_array(slice, env)
    }
}
impl FromObject<'_> for u64 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        call!(env, ?=> object.longValue() -> u64)
            .map_err(FromObjectError::from)
    }
    #[inline(always)]
    fn __from_array_object(object: &'_ JObject<'_>, env: &mut JNIEnv<'_>, _: SealedMethod) -> Result<Box<[Self]>, FromObjectError> {
        get_java_prim_array(object, env)
    }
}
impl ToObject for u64 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        call!(env, ?=> static java.lang.Long.valueOf(u64(*self)) -> java.lang.Long)
            .map_err(ToObjectError::from)
    }
    #[inline(always)]
    fn __to_array_object<'local>(slice: &[Self], env: &mut JNIEnv<'local>, _: SealedMethod) -> Result<JObject<'local>, ToObjectError> {
        create_java_prim_array(slice, env)
    }
}

// Implementations for other primitives

impl FromObject<'_> for bool {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        call!(env, ?=> object.booleanValue() -> boolean)
            .map_err(FromObjectError::from)
    }
    #[inline(always)]
    fn __from_array_object(object: &'_ JObject<'_>, env: &mut JNIEnv<'_>, _: SealedMethod) -> Result<Box<[Self]>, FromObjectError> {
        get_java_prim_array(object, env)
    }
}
impl ToObject for bool {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        call!(env, ?=> static java.lang.Boolean.valueOf(boolean(*self)) -> java.lang.Boolean)
            .map_err(ToObjectError::from)
    }
    #[inline(always)]
    fn __to_array_object<'local>(slice: &[Self], env: &mut JNIEnv<'local>, _: SealedMethod) -> Result<JObject<'local>, ToObjectError> {
        create_java_prim_array(slice, env)
    }
}

impl FromObject<'_> for char {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, &Self::class(), env)?;
        call!(env, ?=> object.charValue() -> char)
            .map_err(FromObjectError::from)
    }
    #[inline(always)]
    fn __from_array_object(object: &'_ JObject<'_>, env: &mut JNIEnv<'_>, _: SealedMethod) -> Result<Box<[Self]>, FromObjectError> {
        get_java_prim_array(object, env)
    }
}
impl ToObject for char {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> Result<JObject<'local>, ToObjectError> {
        call!(env, ?=> static java.lang.Character.valueOf(char(*self)) -> java.lang.Character)
            .map_err(ToObjectError::from)
    }
    #[inline(always)]
    fn __to_array_object<'local>(slice: &[Self], env: &mut JNIEnv<'local>, _: SealedMethod) -> Result<JObject<'local>, ToObjectError> {
        create_java_prim_array(slice, env)
    }
}

/// Hidden trait for a macro to use to convert a java value.
/// Only used in the macros and for element conversion in [`ObjectArray`].
#[doc(hidden)]
pub trait FromObjectOwned<'obj>: Sized {
    fn from_object_owned_env(object: JObject<'obj>, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError>;
}
impl<'obj> FromObjectOwned<'obj> for JObject<'obj> {
    #[inline]
    fn from_object_owned_env(object: JObject<'obj>, _: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        if object.is_null() {
            return Err(FromObjectError::Null);
        }
        Ok(object)
    }
}
impl<'obj> FromObjectOwned<'obj> for JClass<'obj> {
    fn from_object_owned_env(object: JObject<'obj>, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(&object, &Self::class(), env)?;
        Ok(Self::from(object))
    }
}
impl<'obj> FromObjectOwned<'obj> for JThrowable<'obj> {
    fn from_object_owned_env(object: JObject<'obj>, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(&object, &<JavaException as Class>::class(), env)?;
        Ok(Self::from(object))
    }
}
impl<'obj> FromObjectOwned<'obj> for JString<'obj> {
    fn from_object_owned_env(object: JObject<'obj>, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(&object, &Self::class(), env)?;
        Ok(Self::from(object))
    }
}
impl FromObjectOwned<'_> for GlobalRef {
    fn from_object_owned_env(object: JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        if object.is_null() {
            return Err(FromObjectError::Null);
        }
        env.new_global_ref(object)
            .catch(env)
            .map_err(FromObjectError::from)
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
