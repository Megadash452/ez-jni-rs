use ez_jni_macros::new;
use super::*;

impl<'local> FromObject<'local> for JObject<'local> {
    fn from_object(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        if object.is_null() {
            return Err(FromObjectError::Null);
        }
        Ok(env.new_local_ref(object).unwrap())
    }
}
impl ToObject for JObject<'_> {
    fn to_object<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        env.new_local_ref(self).unwrap()
    }
}

impl<'local, T> FromObject<'local> for Option<T>
where T: FromObject<'local> {
    fn from_object(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        if object.is_null() {
            Ok(None)
        } else {
            T::from_object(object, env)
                .map(|t| Some(t))
        }
    }
}
impl<T> ToObject for Option<T>
where T: ToObject {
    fn to_object<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        match self {
            Some(t) => t.to_object(env),
            None => JObject::null()
        }
    }
}

// Implementation for String types

impl FromObject<'_> for String {
    /// Get a [`String`] from some random Object.
    /// 
    /// Don't use this function, it only exist for compatibility.
    /// Use [`get_string()`][crate::utils::get_string] instead because you will mostly be using it with [`JString`][jni::objects::JString].
    fn from_object(object: &JObject, env: &mut JNIEnv) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/String", env)?;
        // Already checked that it is java.lang.String and is not NULL
        Ok(unsafe {
            env.get_string_unchecked(object.into())
                .unwrap_or_else(|err| panic!("ENV error while getting String: {err}"))
                .into()
        })
    }
}
impl ToObject for String {
    fn to_object<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        self.as_str().to_object(env)
    }
}
impl ToObject for str {
    fn to_object<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        env.new_string(self)
            .unwrap_or_else(|err| panic!("Error converting Rust string to Java String: {err}"))
            .into()
    }
}
impl ToObject for &str {
    fn to_object<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        (**self).to_object(env)
    }
}

// Implementation for number types

impl FromObject<'_> for i8 {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Byte", env)?;
        Ok(call!(object.byteValue() -> byte))
    }
}
impl ToObject for i8 {
    fn to_object<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Byte(byte(*self)))
    }
}
impl FromObject<'_> for i16 {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Short", env)?;
        Ok(call!(object.shortValue() -> short))
    }
}
impl ToObject for i16 {
    fn to_object<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Short(short(*self)))
    }
}
impl FromObject<'_> for i32 {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Integer", env)?;
        Ok(call!(object.intValue() -> int))
    }
}
impl ToObject for i32 {
    fn to_object<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Integer(int(*self)))
    }
}
impl FromObject<'_> for i64 {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Long", env)?;
        Ok(call!(object.longValue() -> long))
    }
}
impl ToObject for i64 {
    fn to_object<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Long(long(*self)))
    }
}
impl FromObject<'_> for f32 {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Float", env)?;
        Ok(call!(object.floatValue() -> float))
    }
}
impl ToObject for f32 {
    fn to_object<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Float(float(*self)))
    }
}
impl FromObject<'_> for f64 {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Double", env)?;
        Ok(call!(object.doubleValue() -> double))
    }
}
impl ToObject for f64 {
    fn to_object<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Double(double(*self)))
    }
}

// Implementation for unsigned number types
impl FromObject<'_> for u8 {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Byte", env)?;
        Ok(call!(object.byteValue() -> u8))
    }
}
impl ToObject for u8 {
    fn to_object<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Byte(u8(*self)))
    }
}
impl FromObject<'_> for u16 {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Short", env)?;
        Ok(call!(object.shortValue() -> u16))
    }
}
impl ToObject for u16 {
    fn to_object<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Short(u16(*self)))
    }
}
impl FromObject<'_> for u32 {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Integer", env)?;
        Ok(call!(object.intValue() -> u32))
    }
}
impl ToObject for u32 {
    fn to_object<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Integer(u32(*self)))
    }
}
impl FromObject<'_> for u64 {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Long", env)?;
        Ok(call!(object.longValue() -> u64))
    }
}
impl ToObject for u64 {
    fn to_object<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Long(u64(*self)))
    }
}

// Implementations for other primitives

impl FromObject<'_> for bool {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Boolean", env)?;
        Ok(call!(object.booleanValue() -> boolean))
    }
}
impl ToObject for bool {
    fn to_object<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Boolean(boolean(*self)))
    }
}

impl FromObject<'_> for char {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Character", env)?;
        Ok(call!(object.charValue() -> char))
    }
}
impl ToObject for char {
    fn to_object<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Character(char(*self)))
    }
}
