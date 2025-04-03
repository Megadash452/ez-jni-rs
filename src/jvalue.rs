use jni::{objects::JValue, JNIEnv};

use crate::{utils::get_env, FromObject, FromObjectError};

pub trait FromJValue
where Self: Sized {
    // TODO: doc
    fn from_jvalue<'a, 'local>(val: JValue<'local, 'a>) -> Result<Self, FromJValueError> {
        Self::from_jvalue_env(val, get_env::<'_, 'local>())
    }
    /// Same as [`from_jvalue`][FromJValue::from_jvalue], but does not capture the [`JNIEnv`] automatically; the caller must provide it themselves.
    /// 
    /// This is the *only* function that must be *implemented* for the trait.
    fn from_jvalue_env<'a, 'local, 'other_local>(val: JValue<'other_local, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError>;
}

pub trait ToJValue {
    // TODO: doc
    fn to_jvalue<'a, 'local>(&'a self) -> JValue<'local, 'a> {
        self.to_jvalue_env(get_env::<'_, 'local>())
    }
    /// Same as [`to_jvalue`][ToJValue::to_jvalue], but does not capture the [`JNIEnv`] automatically; the caller must provide it themselves.
    fn to_jvalue_env<'a, 'local, 'other_local>(&'a self, env: &mut JNIEnv<'local>) -> JValue<'other_local, 'a>;
}

// TODO: doc
pub enum FromJValueError {
    Void,
    IncorrectType {
        actual: &'static str,
        expected: &'static str,
    },
    Object(FromObjectError)
}
impl From<FromObjectError> for FromJValueError {
    fn from(value: FromObjectError) -> Self {
        Self::Object(value)
    }
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
        JValue::Void      => JTYPE_VOID,
        JValue::Bool(_)   => JTYPE_BOOL,
        JValue::Char(_)   => JTYPE_CHAR,
        JValue::Byte(_)   => JTYPE_BYTE,
        JValue::Short(_)  => JTYPE_SHORT,
        JValue::Int(_)    => JTYPE_INT,
        JValue::Long(_)   => JTYPE_LONG,
        JValue::Float(_)  => JTYPE_FLOAT,
        JValue::Double(_) => JTYPE_DOUBLE,
        JValue::Object(_) => JTYPE_OBJECT,
    }
}

/// Creates a [`FromJValueError`] from the [`JValue`] passed in.
/// 
/// This doesn't check whether the type of the value is incorrect, only builds the error.
fn from_jvalue_err(val: JValue<'_, '_>, expected: &'static str) -> FromJValueError {
    match val {
        JValue::Void => FromJValueError::Void,
        val => FromJValueError::IncorrectType {
            actual: jvalue_to_str(val),
            expected,
        }
    }
}

// ---- IMPLEMENTATIONS ----

// impl<T> FromJValue for T
// where T: FromObject {
//     fn from_jvalue<'a, 'local>(val: JValue<'local, 'a>) -> Self {
//         match val {
//             JValue::Object(object) => T::from_object(object, todo!())
//         }
//     }
// }

// -- Primitives

impl FromJValue for bool {
    fn from_jvalue_env<'a, 'local, 'other_local>(val: JValue<'other_local, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            JValue::Bool(val) => Ok(val != 0),
            JValue::Object(object) => Ok(Self::from_object(object, env)?),
            val => Err(from_jvalue_err(val, JTYPE_BOOL))
        }
    }
}
impl ToJValue for bool {
    fn to_jvalue_env<'a, 'local, 'other_local>(&'a self, _: &mut JNIEnv<'local>) -> JValue<'other_local, 'a> {
        JValue::Bool(*self as jni::sys::jboolean)
    }
}

impl FromJValue for char {
    fn from_jvalue_env<'a, 'local, 'other_local>(val: JValue<'other_local, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            // Decode UTF-16
            JValue::Char(val) => Ok(
                char::decode_utf16(Some(val))
                    .next().unwrap()
                    .unwrap_or(char::REPLACEMENT_CHARACTER)
            ),
            JValue::Object(object) => Ok(Self::from_object(object, env)?),
            val => Err(from_jvalue_err(val, JTYPE_CHAR))
        }
    }
}
impl ToJValue for char {
    fn to_jvalue_env<'a, 'local, 'other_local>(&'a self, _: &mut JNIEnv<'local>) -> JValue<'other_local, 'a> {
        JValue::Char(self.encode_utf16(&mut [0;1])[0])
    }
}

impl FromJValue for i8 {
    fn from_jvalue_env<'a, 'local, 'other_local>(val: JValue<'other_local, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            JValue::Byte(val) => Ok(val),
            JValue::Object(object) => Ok(Self::from_object(object, env)?),
            val => Err(from_jvalue_err(val, JTYPE_BYTE))
        }
    }
}
impl ToJValue for i8 {
    fn to_jvalue_env<'a, 'local, 'other_local>(&'a self, _: &mut JNIEnv<'local>) -> JValue<'other_local, 'a> {
        JValue::Byte(*self as jni::sys::jbyte)
    }
}

impl FromJValue for i16 {
    fn from_jvalue_env<'a, 'local, 'other_local>(val: JValue<'other_local, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            JValue::Short(val) => Ok(val),
            JValue::Object(object) => Ok(Self::from_object(object, env)?),
            val => Err(from_jvalue_err(val, JTYPE_SHORT))
        }
    }
}
impl ToJValue for i16 {
    fn to_jvalue_env<'a, 'local, 'other_local>(&'a self, _: &mut JNIEnv<'local>) -> JValue<'other_local, 'a> {
        JValue::Short(*self as jni::sys::jshort)
    }
}

impl FromJValue for i32 {
    fn from_jvalue_env<'a, 'local, 'other_local>(val: JValue<'other_local, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            JValue::Int(val) => Ok(val),
            JValue::Object(object) => Ok(Self::from_object(object, env)?),
            val => Err(from_jvalue_err(val, JTYPE_INT))
        }
    }
}
impl ToJValue for i32 {
    fn to_jvalue_env<'a, 'local, 'other_local>(&'a self, _: &mut JNIEnv<'local>) -> JValue<'other_local, 'a> {
        JValue::Int(*self as jni::sys::jint)
    }
}

impl FromJValue for i64 {
    fn from_jvalue_env<'a, 'local, 'other_local>(val: JValue<'other_local, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            JValue::Long(val) => Ok(val),
            JValue::Object(object) => Ok(Self::from_object(object, env)?),
            val => Err(from_jvalue_err(val, JTYPE_LONG))
        }
    }
}
impl ToJValue for i64 {
    fn to_jvalue_env<'a, 'local, 'other_local>(&'a self, _: &mut JNIEnv<'local>) -> JValue<'other_local, 'a> {
        JValue::Long(*self as jni::sys::jlong)
    }
}

impl FromJValue for f32 {
    fn from_jvalue_env<'a, 'local, 'other_local>(val: JValue<'other_local, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            JValue::Float(val) => Ok(val),
            JValue::Object(object) => Ok(Self::from_object(object, env)?),
            val => Err(from_jvalue_err(val, JTYPE_FLOAT))
        }
    }
}
impl ToJValue for f32 {
    fn to_jvalue_env<'a, 'local, 'other_local>(&'a self, _: &mut JNIEnv<'local>) -> JValue<'other_local, 'a> {
        JValue::Float(*self as jni::sys::jfloat)
    }
}

impl FromJValue for f64 {
    fn from_jvalue_env<'a, 'local, 'other_local>(val: JValue<'other_local, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            JValue::Double(val) => Ok(val),
            JValue::Object(object) => Ok(Self::from_object(object, env)?),
            val => Err(from_jvalue_err(val, JTYPE_DOUBLE))
        }
    }
}
impl ToJValue for f64 {
    fn to_jvalue_env<'a, 'local, 'other_local>(&'a self, _: &mut JNIEnv<'local>) -> JValue<'other_local, 'a> {
        JValue::Double(*self as jni::sys::jdouble)
    }
}

// -- Unsigned Integers

impl FromJValue for u8 {
    fn from_jvalue_env<'a, 'local, 'other_local>(val: JValue<'other_local, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            JValue::Byte(val) => Ok(val as u8),
            JValue::Object(object) => Ok(Self::from_object(object, env)?),
            val => Err(from_jvalue_err(val, JTYPE_BYTE))
        }
    }
}
impl ToJValue for u8 {
    fn to_jvalue_env<'a, 'local, 'other_local>(&'a self, _: &mut JNIEnv<'local>) -> JValue<'other_local, 'a> {
        JValue::Byte(*self as jni::sys::jbyte)
    }
}

impl FromJValue for u16 {
    fn from_jvalue_env<'a, 'local, 'other_local>(val: JValue<'other_local, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            JValue::Short(val) => Ok(val as u16),
            JValue::Object(object) => Ok(Self::from_object(object, env)?),
            val => Err(from_jvalue_err(val, JTYPE_SHORT))
        }
    }
}
impl ToJValue for u16 {
    fn to_jvalue_env<'a, 'local, 'other_local>(&'a self, _: &mut JNIEnv<'local>) -> JValue<'other_local, 'a> {
        JValue::Short(*self as jni::sys::jshort)
    }
}

impl FromJValue for u32 {
    fn from_jvalue_env<'a, 'local, 'other_local>(val: JValue<'other_local, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            JValue::Int(val) => Ok(val as u32),
            JValue::Object(object) => Ok(Self::from_object(object, env)?),
            val => Err(from_jvalue_err(val, JTYPE_INT))
        }
    }
}
impl ToJValue for u32 {
    fn to_jvalue_env<'a, 'local, 'other_local>(&'a self, _: &mut JNIEnv<'local>) -> JValue<'other_local, 'a> {
        JValue::Int(*self as jni::sys::jint)
    }
}

impl FromJValue for u64 {
    fn from_jvalue_env<'a, 'local, 'other_local>(val: JValue<'other_local, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            JValue::Long(val) => Ok(val as u64),
            JValue::Object(object) => Ok(Self::from_object(object, env)?),
            val => Err(from_jvalue_err(val, JTYPE_LONG))
        }
    }
}
impl ToJValue for u64 {
    fn to_jvalue_env<'a, 'local, 'other_local>(&'a self, _: &mut JNIEnv<'local>) -> JValue<'other_local, 'a> {
        JValue::Long(*self as jni::sys::jlong)
    }
}

// TODO: -- Objects
