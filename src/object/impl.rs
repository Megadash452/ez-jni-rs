use ez_jni_macros::new;
use super::*;


impl<'local, T> FromObject<'local> for Option<T>
where T: FromObject<'local> {
    const PATH: &'static str = T::PATH;

    fn from_object(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        if object.is_null() {
            Ok(None)
        } else {
            T::from_object(object, env)
                .map(|t| Some(t))
        }
    }
}
impl<'local, T> ToObject<'local> for Option<T>
where T: ToObject<'local> {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        match self {
            Some(t) => t.to_object(env),
            None => JObject::null()
        }
    }
}

// Implementation for String types

impl FromObject<'_> for String {
    const PATH: &'static str = "java/lang/String";

    /// Get a [`String`] from some random Object.
    /// 
    /// Don't use this function, it only exist for compatibility.
    /// Use [`get_string()`][crate::utils::get_string] instead because you will mostly be using it with [`JString`][jni::objects::JString].
    fn from_object(object: &JObject, env: &mut JNIEnv) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, Self::PATH, env)?;
        // Already checked that it is java.lang.String and is not NULL
        Ok(unsafe {
            env.get_string_unchecked(object.into())
                .unwrap_or_else(|err| panic!("ENV error while getting String: {err}"))
                .into()
        })
    }
}
impl<'local> ToObject<'local> for String {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        self.as_str().to_object(env)
    }
}
impl<'local> ToObject<'local> for str {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        env.new_string(self)
            .unwrap_or_else(|err| panic!("Error converting Rust string to Java String: {err}"))
            .into()
    }
}
impl<'local> ToObject<'local> for &str {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        (**self).to_object(env)
    }
}

// Implementation for number types

impl FromObject<'_> for i8 {
    const PATH: &'static str = "java/lang/Byte";

    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, Self::PATH, env)?;
        Ok(call!(object.byteValue() -> byte))
    }
}
impl<'local> ToObject<'local> for i8 {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Byte(byte(*self)))
    }
}
impl FromObject<'_> for i16 {
    const PATH: &'static str = "java/lang/Short";

    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, Self::PATH, env)?;
        Ok(call!(object.shortValue() -> short))
    }
}
impl<'local> ToObject<'local> for i16 {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Short(short(*self)))
    }
}
impl FromObject<'_> for i32 {
    const PATH: &'static str = "java/lang/Integer";

    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, Self::PATH, env)?;
        Ok(call!(object.intValue() -> int))
    }
}
impl<'local> ToObject<'local> for i32 {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Integer(int(*self)))
    }
}
impl FromObject<'_> for i64 {
    const PATH: &'static str = "java/lang/Long";

    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, Self::PATH, env)?;
        Ok(call!(object.longValue() -> long))
    }
}
impl<'local> ToObject<'local> for i64 {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Long(long(*self)))
    }
}
impl FromObject<'_> for f32 {
    const PATH: &'static str = "java/lang/Float";

    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, Self::PATH, env)?;
        Ok(call!(object.floatValue() -> float))
    }
}
impl<'local> ToObject<'local> for f32 {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Float(float(*self)))
    }
}
impl FromObject<'_> for f64 {
    const PATH: &'static str = "java/lang/Double";

    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, Self::PATH, env)?;
        Ok(call!(object.doubleValue() -> double))
    }
}
impl<'local> ToObject<'local> for f64 {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Double(double(*self)))
    }
}

// Implementation for unsigned number types
impl FromObject<'_> for u8 {
    const PATH: &'static str = "java/lang/Byte";

    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, Self::PATH, env)?;
        Ok(call!(object.byteValue() -> u8))
    }
}
impl<'local> ToObject<'local> for u8 {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Byte(u8(*self)))
    }
}
impl FromObject<'_> for u16 {
    const PATH: &'static str = "java/lang/Short";

    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, Self::PATH, env)?;
        Ok(call!(object.shortValue() -> u16))
    }
}
impl<'local> ToObject<'local> for u16 {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Short(u16(*self)))
    }
}
impl FromObject<'_> for u32 {
    const PATH: &'static str = "java/lang/Integer";

    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, Self::PATH, env)?;
        Ok(call!(object.intValue() -> u32))
    }
}
impl<'local> ToObject<'local> for u32 {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Integer(u32(*self)))
    }
}
impl FromObject<'_> for u64 {
    const PATH: &'static str = "java/lang/Long";

    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, Self::PATH, env)?;
        Ok(call!(object.longValue() -> u64))
    }
}
impl<'local> ToObject<'local> for u64 {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Long(u64(*self)))
    }
}

// Implementations for other primitives

impl FromObject<'_> for bool {
    const PATH: &'static str = "java/lang/Boolean";

    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, Self::PATH, env)?;
        Ok(call!(object.booleanValue() -> boolean))
    }
}
impl<'local> ToObject<'local> for bool {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Boolean(boolean(*self)))
    }
}

impl FromObject<'_> for char {
    const PATH: &'static str = "java/lang/Character";

    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        object_check_boilerplate(object, Self::PATH, env)?;
        Ok(call!(object.charValue() -> char))
    }
}
impl<'local> ToObject<'local> for char {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        new!(java.lang.Character(char(*self)))
    }
}
