use ez_jni_macros::new;
use jni::objects::JClass;
use crate::utils::check_object_class;
use super::*;

impl<'a, 'obj, 'local> FromObject<'a, 'obj, 'local> for &'a JObject<'obj> {
    // The one case where from_object() is implemented manually to avoid calling get_env(). This happens NOWHERE else.
    // There is no need for a JNIEnv here.
    fn from_object(object: &'a JObject<'obj>) -> Result<Self, FromObjectError> {
        if object.is_null() {
            return Err(FromObjectError::Null);
        }
        Ok(object)
    }
    fn from_object_env(object: &'a JObject<'obj>, _: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        Self::from_object(object)
    }
}
impl ToObject for JObject<'_> {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        env.new_local_ref(self).unwrap()
    }
}
impl<'a, 'obj, 'local> FromObject<'a, 'obj, 'local> for &'a JClass<'obj> {
    fn from_object_env(object: &'a JObject<'obj>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        check_object_class(object, "java/lang/Class", env)?;
        Ok(<&JClass>::from(object))
    }
}
impl ToObject for JClass<'_> {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <JObject as ToObject>::to_object_env(self, env)
    }
}
impl<'a, 'obj, 'local> FromObject<'a, 'obj, 'local> for &'a JThrowable<'obj> {
    fn from_object_env(object: &'a JObject<'obj>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        check_object_class(object, "java/lang/Exception", env)?;
        Ok(<&JThrowable>::from(object))
    }
}
impl ToObject for JThrowable<'_> {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <JObject as ToObject>::to_object_env(self, env)
    }
}

// Implementation for Option type

impl<'a, 'obj, 'local, T> FromObject<'a, 'obj, 'local> for Option<T>
where T: FromObject<'a, 'obj, 'local> {
    fn from_object_env(object: &'a JObject<'obj>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        if object.is_null() {
            Ok(None)
        } else {
            T::from_object_env(object, env)
                .map(|t| Some(t))
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
impl<T> ToObject for &Option<&T>
where T: ToObject {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        match self {
            Some(t) => t.to_object_env(env),
            None => JObject::null()
        }
    }
}

// Implementation for String types

impl FromObject<'_, '_, '_> for String {
    /// Get a [`String`] from some random Object.
    /// 
    /// Don't use this function, it only exist for compatibility.
    /// Use [`get_string()`][crate::utils::get_string] instead because you will mostly be using it with [`JString`][jni::objects::JString].
    fn from_object_env(object: &JObject, env: &mut JNIEnv) -> Result<Self, FromObjectError> {
        check_object_class(object, "java/lang/String", env)?;
        // Already checked that it is java.lang.String and is not NULL
        Ok(unsafe {
            env.get_string_unchecked(object.into())
                .unwrap_or_else(|err| panic!("ENV error while getting String: {err}"))
                .into()
        })
    }
}
impl ToObject for String {
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
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <str as ToObject>::to_object_env(self, env)
    }
}

// Implementation for number types

impl FromObject<'_, '_, '_> for i8 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        check_object_class(object, "java/lang/Byte", env)?;
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
        check_object_class(object, "java/lang/Short", env)?;
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
        check_object_class(object, "java/lang/Integer", env)?;
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
        check_object_class(object, "java/lang/Long", env)?;
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
        check_object_class(object, "java/lang/Float", env)?;
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
        check_object_class(object, "java/lang/Double", env)?;
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
        check_object_class(object, "java/lang/Byte", env)?;
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
        check_object_class(object, "java/lang/Short", env)?;
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
        check_object_class(object, "java/lang/Integer", env)?;
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
        check_object_class(object, "java/lang/Long", env)?;
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
        check_object_class(object, "java/lang/Boolean", env)?;
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
        check_object_class(object, "java/lang/Character", env)?;
        Ok(call!(env=> object.charValue() -> char))
    }
}
impl ToObject for char {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Character(char(*self)))
    }
}
