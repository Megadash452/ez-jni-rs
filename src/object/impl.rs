use ez_jni_macros::new;
use super::*;

impl<'local> FromObjectImpl<'local> for JObject<'local> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        if object.is_null() {
            return Err(FromObjectError::Null);
        }
        Ok(env.new_local_ref(object).unwrap())
    }
}
impl ToObjectImpl for JObject<'_> {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        env.new_local_ref(self).unwrap()
    }
}

impl<'local, T> FromObjectImpl<'local> for Option<T>
where T: FromObjectImpl<'local> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        if object.is_null() {
            Ok(None)
        } else {
            T::from_object_env(object, env)
                .map(|t| Some(t))
        }
    }
}
impl<T> ToObjectImpl for Option<T>
where T: ToObjectImpl {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        match self {
            Some(t) => t.to_object_env(env),
            None => JObject::null()
        }
    }
}

// Implementation for String types

impl FromObjectImpl<'_> for String {
    /// Get a [`String`] from some random Object.
    /// 
    /// Don't use this function, it only exist for compatibility.
    /// Use [`get_string()`][crate::utils::get_string] instead because you will mostly be using it with [`JString`][jni::objects::JString].
    fn from_object_env(object: &JObject, env: &mut JNIEnv) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/String", env)?;
        // Already checked that it is java.lang.String and is not NULL
        Ok(unsafe {
            env.get_string_unchecked(object.into())
                .unwrap_or_else(|err| panic!("ENV error while getting String: {err}"))
                .into()
        })
    }
}
impl ToObjectImpl for String {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        self.as_str().to_object_env(env)
    }
}
impl ToObjectImpl for str {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        env.new_string(self)
            .unwrap_or_else(|err| panic!("Error converting Rust string to Java String: {err}"))
            .into()
    }
}
impl ToObjectImpl for &str {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        (**self).to_object_env(env)
    }
}

// Implementation for number types

impl FromObjectImpl<'_> for i8 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Byte", env)?;
        Ok(call!(env=> object.byteValue() -> byte))
    }
}
impl ToObjectImpl for i8 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Byte(byte(*self)))
    }
}
impl FromObjectImpl<'_> for i16 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Short", env)?;
        Ok(call!(env=> object.shortValue() -> short))
    }
}
impl ToObjectImpl for i16 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Short(short(*self)))
    }
}
impl FromObjectImpl<'_> for i32 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Integer", env)?;
        Ok(call!(env=> object.intValue() -> int))
    }
}
impl ToObjectImpl for i32 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Integer(int(*self)))
    }
}
impl FromObjectImpl<'_> for i64 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Long", env)?;
        Ok(call!(env=> object.longValue() -> long))
    }
}
impl ToObjectImpl for i64 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Long(long(*self)))
    }
}
impl FromObjectImpl<'_> for f32 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Float", env)?;
        Ok(call!(env=> object.floatValue() -> float))
    }
}
impl ToObjectImpl for f32 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Float(float(*self)))
    }
}
impl FromObjectImpl<'_> for f64 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Double", env)?;
        Ok(call!(env=> object.doubleValue() -> double))
    }
}
impl ToObjectImpl for f64 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Double(double(*self)))
    }
}

// Implementation for unsigned number types
impl FromObjectImpl<'_> for u8 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Byte", env)?;
        Ok(call!(env=> object.byteValue() -> u8))
    }
}
impl ToObjectImpl for u8 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Byte(u8(*self)))
    }
}
impl FromObjectImpl<'_> for u16 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Short", env)?;
        Ok(call!(env=> object.shortValue() -> u16))
    }
}
impl ToObjectImpl for u16 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Short(u16(*self)))
    }
}
impl FromObjectImpl<'_> for u32 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Integer", env)?;
        Ok(call!(env=> object.intValue() -> u32))
    }
}
impl ToObjectImpl for u32 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Integer(u32(*self)))
    }
}
impl FromObjectImpl<'_> for u64 {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Long", env)?;
        Ok(call!(env=> object.longValue() -> u64))
    }
}
impl ToObjectImpl for u64 {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Long(u64(*self)))
    }
}

// Implementations for other primitives

impl FromObjectImpl<'_> for bool {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Boolean", env)?;
        Ok(call!(env=> object.booleanValue() -> boolean))
    }
}
impl ToObjectImpl for bool {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Boolean(boolean(*self)))
    }
}

impl FromObjectImpl<'_> for char {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, "java/lang/Character", env)?;
        Ok(call!(env=> object.charValue() -> char))
    }
}
impl ToObjectImpl for char {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(env=> java.lang.Character(char(*self)))
    }
}
