use jni::{objects::{JObject, JValue, JValueGen, JValueOwned}, JNIEnv};
use thiserror::Error;
use crate::{utils::get_env, FromObject, FromObjectError, ToObject};

/// Get a **Rust** value from a **Java** value.
/// 
/// *Users* of this trait should only use [`from_jvalue()`][FromJValue::from_jvalue()],
/// but *implementors* of this trait should only implement [`from_jvalue_env()`][FromJValue::from_jvalue_env()].
/// 
/// > This trait is directly used by the [`macros`][ez_jni_macros].
pub trait FromJValue<'a, 'local>
where Self: Sized {
    /// Get a **Rust** value from a **Java** value.
    /// 
    /// Automatically captures the [`JNIEnv`] from the local stack.
    ///
    /// Returns an [`Error`][FromJValueError] if the *value* was not the correct type.
    // TODO: check lifetime for get_env()
    fn from_jvalue(val: JValue<'_, 'a>) -> Result<Self, FromJValueError> {
        Self::from_jvalue_env(val, get_env::<'_, 'local>())
    }
    /// Same as [`from_jvalue`][FromJValue::from_jvalue], but does not capture the [`JNIEnv`] automatically; the caller must provide it themselves.
    /// 
    /// This is the *only* function that must be *implemented* for the trait.
    fn from_jvalue_env(val: JValue<'_, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError>;
}

/// Convert a **Rust** value to a **Java** value.
/// 
/// *Users* of this trait should only use [`to_jvalue()`][ToJValue::to_jvalue()],
/// but *implementors* of this trait should only implement [`to_jvalue_env()`][ToJValue::to_jvalue_env()].
/// 
/// > This trait is directly used by the [`macros`][ez_jni_macros].
pub trait ToJValue {
    /// Convert a **Rust** value to a **Java** value.
    /// 
    /// Automatically captures the [`JNIEnv`] from the local stack.
    // TODO: check lifetime for get_env()
    fn to_jvalue<'local>(&self) -> JValueOwned<'local> {
        self.to_jvalue_env(get_env::<'_, 'local>())
    }
    /// Same as [`to_jvalue`][ToJValue::to_jvalue], but does not capture the [`JNIEnv`] automatically; the caller must provide it themselves.
    ///
    /// This is the *only* function that must be *implemented* for the trait.
    fn to_jvalue_env<'local>(&self, env: &mut JNIEnv<'local>) -> JValueOwned<'local>;
}

#[derive(Debug, Error)]
pub enum FromJValueError {
    #[error("Found 'void' type; can't use this type")]
    Void,
    #[error("Expected java type '{expected}', found java type '{actual}'")]
    IncorrectType {
        actual: &'static str,
        expected: &'static str,
    },
    #[error("{0}")]
    Object(#[from] FromObjectError)
}

static JTYPE_VOID:   &'static str = "void";
static JTYPE_BOOL:   &'static str = "boolean";
static JTYPE_CHAR:   &'static str = "char";
static JTYPE_BYTE:   &'static str = "byte";
static JTYPE_SHORT:  &'static str = "short";
static JTYPE_INT:    &'static str = "int";
static JTYPE_LONG:   &'static str = "long";
static JTYPE_FLOAT:  &'static str = "float";
static JTYPE_DOUBLE: &'static str = "double";
static JTYPE_OBJECT: &'static str = "Object";

/// Gets the [`JValue`] as a string (e.g. `"boolean"`).
fn jvalue_to_str(val: JValue<'_, '_>) -> &'static str {
    match val {
        JValueGen::Void      => JTYPE_VOID,
        JValueGen::Bool(_)   => JTYPE_BOOL,
        JValueGen::Char(_)   => JTYPE_CHAR,
        JValueGen::Byte(_)   => JTYPE_BYTE,
        JValueGen::Short(_)  => JTYPE_SHORT,
        JValueGen::Int(_)    => JTYPE_INT,
        JValueGen::Long(_)   => JTYPE_LONG,
        JValueGen::Float(_)  => JTYPE_FLOAT,
        JValueGen::Double(_) => JTYPE_DOUBLE,
        JValueGen::Object(_) => JTYPE_OBJECT,
    }
}

/// Creates a [`FromJValueError`] from the [`JValue`] passed in.
/// 
/// This doesn't check whether the type of the value is incorrect, only builds the error.
fn from_jvalue_err(val: JValue<'_, '_>, expected: &'static str) -> FromJValueError {
    match val {
        JValueGen::Void => FromJValueError::Void,
        val => FromJValueError::IncorrectType {
            actual: jvalue_to_str(val),
            expected,
        }
    }
}

// ---- IMPLEMENTATIONS ----

// -- Primitives

impl FromJValue<'_, '_> for bool {
    fn from_jvalue_env(val: JValue<'_, '_>, env: &mut JNIEnv<'_>) -> Result<Self, FromJValueError> {
        match val {
            JValueGen::Bool(val) => Ok(val != 0),
            JValueGen::Object(object) => Ok(Self::from_object(object, env)?),
            val => Err(from_jvalue_err(val, JTYPE_BOOL))
        }
    }
}
impl ToJValue for bool {
    fn to_jvalue_env<'local>(&self, _: &mut JNIEnv<'local>) -> JValueOwned<'local> {
        JValueGen::Bool(*self as jni::sys::jboolean)
    }
}

impl FromJValue<'_, '_> for char {
    fn from_jvalue_env<'a, 'local>(val: JValue<'_, '_>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            // Decode UTF-16
            JValueGen::Char(val) => Ok(
                char::decode_utf16(Some(val))
                    .next().unwrap()
                    .unwrap_or(char::REPLACEMENT_CHARACTER)
            ),
            JValueGen::Object(object) => Ok(Self::from_object(object, env)?),
            val => Err(from_jvalue_err(val, JTYPE_CHAR))
        }
    }
}
impl ToJValue for char {
    fn to_jvalue_env<'local>(&self, _: &mut JNIEnv<'local>) -> JValueOwned<'local> {
        JValueGen::Char(self.encode_utf16(&mut [0;1])[0])
    }
}

/// Implements the [`FromJValue`] and [`ToJValue`] traits with simple primitive conversion.
/// If value was [`JObject`] in [`FromJValue`], this calls the [`FromObject`] implementation.
macro_rules! map_primitive_impl {
    (for $ty:ty, $jvariant:ident, $jty:ident, expected $expected:ident) => {
        impl FromJValue<'_, '_> for $ty {
            fn from_jvalue_env(val: ::jni::objects::JValue<'_, '_>, env: &mut ::jni::JNIEnv<'_>) -> Result<Self, FromJValueError> {
                match val {
                    ::jni::objects::JValueGen::$jvariant(val) => Ok(val as Self),
                    ::jni::objects::JValueGen::Object(object) => Ok(Self::from_object(object, env)?),
                    val => Err(from_jvalue_err(val, $expected))
                }
            }
        }
        impl ToJValue for $ty {
            fn to_jvalue_env<'local>(&self, _: &mut JNIEnv<'local>) -> JValueOwned<'local> {
                ::jni::objects::JValueGen::$jvariant(*self as ::jni::sys::$jty)
            }
        }
    };
}

map_primitive_impl!(for i8,  Byte,   jbyte,   expected JTYPE_BYTE);
map_primitive_impl!(for i16, Short,  jshort,  expected JTYPE_SHORT);
map_primitive_impl!(for i32, Int,    jint,    expected JTYPE_INT);
map_primitive_impl!(for i64, Long,   jlong,   expected JTYPE_LONG);
map_primitive_impl!(for u8,  Byte,   jbyte,   expected JTYPE_BYTE);
map_primitive_impl!(for u16, Short,  jshort,  expected JTYPE_SHORT);
map_primitive_impl!(for u32, Int,    jint,    expected JTYPE_INT);
map_primitive_impl!(for u64, Long,   jlong,   expected JTYPE_LONG);
map_primitive_impl!(for f32, Float,  jfloat,  expected JTYPE_FLOAT);
map_primitive_impl!(for f64, Double, jdouble, expected JTYPE_DOUBLE);

//  -- Objects

/// Implements the [`FromJValue`] and [`ToJValue`] traits by using the existing [`FromObject`] implementation.
macro_rules! map_to_object_impl {
    (for [$ty:ty]) => {
        map_to_object_impl!(FromJValue for Box<[$ty]>);
        map_to_object_impl!(ToJValue for [$ty]);
    };
    (for $ty:ty) => {
        map_to_object_impl!(FromJValue for $ty);
        map_to_object_impl!(ToJValue for $ty);
    };
    (FromJValue for $ty:ty) => {
        impl<'a, 'local> FromJValue<'a, 'local> for $ty {
            fn from_jvalue_env(val: ::jni::objects::JValue<'_, 'a>, env: &mut ::jni::JNIEnv<'local>) -> Result<Self, FromJValueError> {
                match val {
                    ::jni::objects::JValueGen::Object(object) => Ok(Self::from_object(object, env)?),
                    val => Err(from_jvalue_err(val, JTYPE_OBJECT))
                }
            }
        }
    };
    (ToJValue for $ty:ty) => {
        impl ToJValue for $ty {
            fn to_jvalue_env<'local>(&self, env: &mut JNIEnv<'local>) -> JValueOwned<'local> {
                ::jni::objects::JValueGen::Object(self.to_object(env))
            }
        }
    };
}

impl<'a, 'local> FromJValue<'a, 'local> for JObject<'local>  {
    fn from_jvalue_env(val: JValue<'_, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            JValueGen::Object(object) => Ok(Self::from_object(object, env)?),
            val => Err(from_jvalue_err(val, JTYPE_OBJECT))
        }
    }
}
impl ToJValue for JObject<'_> {
    fn to_jvalue_env<'local>(&self, env: &mut JNIEnv<'local>) -> JValueOwned<'local> {
        JValueGen::Object(self.to_object(env))
    }
}

impl<'a, 'local, T> FromJValue<'a, 'local> for Option<T>
where T: FromObject<'local> {
    fn from_jvalue_env(val: JValue<'_, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            JValueGen::Object(object) => Ok(Self::from_object(object, env)?),
            val => Err(from_jvalue_err(val, JTYPE_OBJECT))
        }
    }
}
impl<T> ToJValue for Option<T>
where T: ToObject {
    fn to_jvalue_env<'local>(&self, env: &mut JNIEnv<'local>) -> JValueOwned<'local> {
        JValueGen::Object(self.to_object(env))
    }
}

map_to_object_impl!(for String);
map_to_object_impl!(ToJValue for str);
map_to_object_impl!(ToJValue for &str);

// -- Arrays

impl<'local, T> FromJValue<'_, 'local> for Vec<T>
where Box<[T]>: FromObject<'local> {
    fn from_jvalue_env<'a>(val: JValue<'_, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            JValueGen::Object(object) => Ok(Self::from_object(object, env)?),
            val => Err(from_jvalue_err(val, JTYPE_OBJECT))
        }
    }
}

// map_to_object_impl!(for Box<[JObject<'local>]>);
map_to_object_impl!(for [String]);
map_to_object_impl!(ToJValue for [&str]);
map_to_object_impl!(for [Option<String>]);
map_to_object_impl!(ToJValue for [Option<&str>]);
map_to_object_impl!(for [bool]);
map_to_object_impl!(for [char]);
map_to_object_impl!(for [i8]);
map_to_object_impl!(for [i16]);
map_to_object_impl!(for [i32]);
map_to_object_impl!(for [i64]);
map_to_object_impl!(for [f32]);
map_to_object_impl!(for [f64]);
map_to_object_impl!(for [u8]);
map_to_object_impl!(for [u16]);
map_to_object_impl!(for [u32]);
map_to_object_impl!(for [u64]);