use jni::{objects::{JClass, JObject, JThrowable, JValue, JValueGen, JValueOwned}, JNIEnv};
use thiserror::Error;
use crate::{utils::get_env, FromObject, FromObjectError, FromObjectOwned, ToObject};

/// Get a **Rust** value from a **Java** value.
/// 
/// ## Implementation
/// 
/// This trait contains 2 methods that essentially do the same thing.
/// [`from_jvalue`][FromJValue::from_jvalue] exists for ease of use when a [`JNIEnv`] is not in scope and gets it from a global stack.
/// This method should **not** be re-implemented.
/// [`from_jvalue_env`][FromJValue::from_jvalue_env] is the method that has the actual implementation and can take any [`JNIEnv`].
/// All calls in the code of [`from_jvalue_env`][FromJValue::from_jvalue_env] should only use the `_env` counterparts so that the same [`JNIEnv`] is guaranteed to be used throughout the call.
pub trait FromJValue<'a, 'obj, 'local>
where Self: Sized {
    /// Get a **Rust** value from a **Java** value.
    /// 
    /// Returns an [`Error`][FromJValueError] if the *value* was not the correct type.
    /// Will [`panic!`] if any of the underlying JNI calls fail.
    /// 
    /// Automatically captures the [`JNIEnv`] from the local stack.
    /// To pass in your own [`JNIEnv`], see [`FromJValue::from_jvalue_env`].
    // NOTE: This method is only manually implemented for void/unit/() and JObject (NOTHING else) to avoid calling get_env().
    fn from_jvalue(val: JValue<'obj, 'a>) -> Result<Self, FromJValueError> {
        Self::from_jvalue_env(val, get_env::<'_, 'local>())
    }
    /// Same as [`from_jvalue`][FromJValue::from_jvalue], but does not capture the [`JNIEnv`] automatically; the caller must provide it themselves.
    /// 
    /// The implementation should also only methods that allow passing a [`JNIEnv`] so that the same [`JNIEnv`] is guaranteed to be used all throughout the call.
    /// For example, use [`from_jvalue_env`][FromJValue::from_jvalue_env] instead of [`from_jvalue`][FromJValue::from_jvalue].
    /// For jni macros, the env can be specified with this syntax: `macro!(env=> ...)`.
    /// 
    /// Only implement *this* method for the trait.
    fn from_jvalue_env(val: JValue<'obj, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError>;
}

/// Convert a **Rust** value to a **Java** value.
/// 
/// ## Implementation
/// 
/// Just like [`FromJValue`][FromJValue#implementation], this trait has 2 methods that do the same thing:
/// [`to_jvalue`][ToJValue::to_jvalue] automatically gets the [`JNIEnv`],
/// and [`to_jvalue_env`][ToJValue::to_jvalue_env] is the method that has the implementation.
pub trait ToJValue {
    /// Convert a **Rust** value to a **Java** value.
    /// 
    /// Will [`panic!`] if any of the underlying JNI calls fail.
    /// 
    /// Automatically captures the [`JNIEnv`] from the local stack.
    /// To pass in your own [`JNIEnv`], see [`ToJValue::to_jvalue_env`].
    fn to_jvalue<'local>(&self) -> JValueOwned<'local> {
        self.to_jvalue_env(get_env::<'_, 'local>())
    }
    /// Same as [`to_jvalue`][ToJValue::to_jvalue], but does not capture the [`JNIEnv`] automatically; the caller must provide it themselves.
    ///
    /// The implementation should also only methods that allow passing a [`JNIEnv`] so that the same [`JNIEnv`] is guaranteed to be used all throughout the call.
    /// For example, use [`to_jvalue_env`][ToJValue::to_jvalue_env] instead of [`to_jvalue`][ToJValue::to_jvalue].
    /// For jni macros, the env can be specified with this syntax: `macro!(env=> ...)`.
    /// 
    /// Only implement *this* method for the trait.
    fn to_jvalue_env<'local>(&self, env: &mut JNIEnv<'local>) -> JValueOwned<'local>;
}

/// Error returned by [`FromJValue::from_jvalue()`].
#[derive(Debug, Error)]
pub enum FromJValueError {
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

// ---- IMPLEMENTATIONS ----

// -- Primitives

impl FromJValue<'_, '_, '_> for () {
    // There is no need for a JNIEnv here.
    fn from_jvalue(val: JValue<'_, '_>) -> Result<Self, FromJValueError> {
        match val {
            JValueGen::Void => Ok(()),
            val => Err(FromJValueError::IncorrectType {
                actual: jvalue_to_str(val),
                expected: JTYPE_VOID,
            })
        }
    }
    fn from_jvalue_env(val: JValue<'_, '_>, _: &mut JNIEnv<'_>) -> Result<Self, FromJValueError> {
        Self::from_jvalue(val)
    }
}
impl ToJValue for () {
    #[inline]
    fn to_jvalue_env<'local>(&self, _: &mut JNIEnv<'local>) -> JValueOwned<'local> {
        JValueGen::Void
    }
}

impl FromJValue<'_, '_, '_> for bool {
    fn from_jvalue_env(val: JValue<'_, '_>, env: &mut JNIEnv<'_>) -> Result<Self, FromJValueError> {
        match val {
            JValueGen::Bool(b) => Ok(crate::utils::jboolean_to_bool(b)),
            JValueGen::Object(object) => Ok(Self::from_object_env(object, env)?),
            val => Err(FromJValueError::IncorrectType {
                actual: jvalue_to_str(val),
                expected: JTYPE_BOOL,
            })
        }
    }
}
impl ToJValue for bool {
    #[inline]
    fn to_jvalue_env<'local>(&self, _: &mut JNIEnv<'local>) -> JValueOwned<'local> {
        JValueGen::Bool(*self as jni::sys::jboolean)
    }
}

impl FromJValue<'_, '_, '_> for char {
    fn from_jvalue_env<'a, 'local>(val: JValue<'_, '_>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            // Decode UTF-16
            JValueGen::Char(c) => Ok(crate::utils::jchar_to_char(c)),
            JValueGen::Object(object) => Ok(Self::from_object_env(object, env)?),
            val => Err(FromJValueError::IncorrectType {
                actual: jvalue_to_str(val),
                expected: JTYPE_CHAR,
            })
        }
    }
}
impl ToJValue for char {
    fn to_jvalue_env<'local>(&self, _: &mut JNIEnv<'local>) -> JValueOwned<'local> {
        JValueGen::Char(crate::utils::char_to_jchar(*self))
    }
}

/// Implements the [`FromJValue`] and [`ToJValue`] traits with simple primitive conversion.
/// If value was [`JObject`] in [`FromJValue`], this calls the [`FromObject`] implementation.
macro_rules! map_primitive_impl {
    (for $ty:ty, $jvariant:ident, $jty:ident, expected $expected:ident) => {
        impl FromJValue<'_, '_, '_> for $ty {
            fn from_jvalue_env(val: ::jni::objects::JValue<'_, '_>, env: &mut ::jni::JNIEnv<'_>) -> Result<Self, FromJValueError> {
                match val {
                    ::jni::objects::JValueGen::$jvariant(val) => Ok(val as Self),
                    ::jni::objects::JValueGen::Object(object) => Ok(Self::from_object_env(object, env)?),
                    val => Err(FromJValueError::IncorrectType {
                        actual: jvalue_to_str(val),
                        expected: $expected,
                    })
                }
            }
        }
        impl ToJValue for $ty {
            #[inline]
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
        impl<'a, 'obj, 'local> FromJValue<'a, 'obj, 'local> for $ty {
            fn from_jvalue_env(val: ::jni::objects::JValue<'obj, 'a>, env: &mut ::jni::JNIEnv<'local>) -> Result<Self, FromJValueError> {
                match val {
                    ::jni::objects::JValueGen::Object(object) => Ok(Self::from_object_env(object, env)?),
                    val => Err(FromJValueError::IncorrectType {
                        actual: jvalue_to_str(val),
                        expected: JTYPE_OBJECT,
                    })
                }
            }
        }
    };
    (ToJValue for $ty:ty) => {
        impl ToJValue for $ty {
            fn to_jvalue_env<'local>(&self, env: &mut JNIEnv<'local>) -> JValueOwned<'local> {
                ::jni::objects::JValueGen::Object(self.to_object_env(env))
            }
        }
    };
}

impl<'a, 'obj, 'local> FromJValue<'a, 'obj, '_> for &'a JObject<'obj> {
    // There is no need for a JNIEnv here.
    fn from_jvalue(val: JValue<'obj, 'a>) -> Result<Self, FromJValueError> {
        match val {
            JValueGen::Object(object) => Ok(Self::from_object(object)?),
            val => Err(FromJValueError::IncorrectType {
                actual:jvalue_to_str(val),expected:JTYPE_OBJECT,
            })
        }
    }
    fn from_jvalue_env(val: JValue<'obj, 'a>, _: &mut JNIEnv<'_>) -> Result<Self, FromJValueError> {
        Self::from_jvalue(val)
    }
}
map_to_object_impl!(ToJValue for JObject<'_>);
impl<'a, 'obj, 'local> FromJValue<'a, 'obj, '_> for &'a JClass<'obj> {
    fn from_jvalue_env(val: JValue<'obj, 'a>, env: &mut JNIEnv<'_>) -> Result<Self, FromJValueError> {
        Self::from_object_env(<&JObject>::from_jvalue(val)?, env)
            .map_err(|err| err.into())
    }
}
map_to_object_impl!(ToJValue for JClass<'_>);
impl<'a, 'obj, 'local> FromJValue<'a, 'obj, '_> for &'a JThrowable<'obj> {
    fn from_jvalue_env(val: JValue<'obj, 'a>, env: &mut JNIEnv<'_>) -> Result<Self, FromJValueError> {
        Self::from_object_env(<&JObject>::from_jvalue(val)?, env)
            .map_err(|err| err.into())
    }
}
map_to_object_impl!(ToJValue for JThrowable<'_>);

impl<'a, 'obj, 'local, T> FromJValue<'a, 'obj, 'local> for Option<T>
where T: FromObject<'a, 'obj, 'local> {
    fn from_jvalue_env(val: JValue<'obj, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            JValueGen::Object(object) => Ok(Self::from_object_env(object, env)?),
            val => Err(FromJValueError::IncorrectType {
                actual: jvalue_to_str(val),
                expected: JTYPE_OBJECT,
            })
        }
    }
}
impl<T> ToJValue for Option<T>
where T: ToObject {
    fn to_jvalue_env<'local>(&self, env: &mut JNIEnv<'local>) -> JValueOwned<'local> {
        JValueGen::Object(self.to_object_env(env))
    }
}

map_to_object_impl!(for String);
map_to_object_impl!(ToJValue for str);
map_to_object_impl!(ToJValue for &str);

// -- Arrays

// TODO: Ignore 'a and 'obj because the resulting object will always be JObject<'local>
impl<'a, 'obj, 'local, T> FromJValue<'a, 'obj,  'local> for Vec<T>
where Box<[T]>: FromObject<'a, 'obj, 'local> {
    fn from_jvalue_env(val: JValue<'obj, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            JValueGen::Object(object) => Ok(Self::from_object_env(object, env)?),
            val => Err(FromJValueError::IncorrectType {
                actual: jvalue_to_str(val),
                expected: JTYPE_OBJECT,
            })
        }
    }
}

// TODO: Ignore 'a and 'obj because the resulting object will always be JObject<'local>
impl<'a, 'obj, 'local, T> FromJValue<'a, 'obj, 'local> for Box<[T]>
where Self: FromObject<'a, 'obj, 'local> {
    fn from_jvalue_env(val: JValue<'obj, 'a>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            ::jni::objects::JValueGen::Object(object) => Ok(Self::from_object_env(object, env)?),
            val => Err(FromJValueError::IncorrectType {
                actual: jvalue_to_str(val),
                expected: JTYPE_OBJECT,
            })
        }
    }
}
impl<T> ToJValue for [T]
where Self: ToObject {
    fn to_jvalue_env<'local>(&self, env: &mut JNIEnv<'local>) -> JValueOwned<'local> {
        ::jni::objects::JValueGen::Object(self.to_object_env(env))
    }
}
// For [JObject] and [Option<JObject>]
impl<T> ToJValue for (&str, &[T])
where Self: ToObject {
    fn to_jvalue_env<'local>(&self, env: &mut JNIEnv<'local>) -> JValueOwned<'local> {
        ::jni::objects::JValueGen::Object(self.to_object_env(env))
    }
}


/// Hidden trait for `call!` to use to convert the return value
/// Only used in the macros, so this will `panic!` on error.
#[doc(hidden)]
pub trait FromJValueOwned<'obj> {
    fn from_jvalue_owned_env(val: JValueOwned<'obj>, env: &mut JNIEnv<'_>) -> Self;
}
#[doc(hidden)]
impl<'obj> FromJValueOwned<'obj> for JObject<'obj> {
    fn from_jvalue_owned_env(val: JValueOwned<'obj>, env: &mut JNIEnv<'_>) -> Self {
        match val {
            ::jni::objects::JValueGen::Object(object) => Self::from_object_owned_env(object, env),
            val => panic!("{}", FromJValueError::IncorrectType {
                actual: jvalue_to_str(val.borrow()),
                expected: JTYPE_OBJECT,
            })
        }
    }
}
#[doc(hidden)]
impl<'obj> FromJValueOwned<'obj> for JClass<'obj> {
    fn from_jvalue_owned_env(val: JValueOwned<'obj>, env: &mut JNIEnv<'_>) -> Self {
        let object = JObject::from_jvalue_owned_env(val, env);
        Self::from_object_owned_env(object, env)
    }
}
#[doc(hidden)]
impl<'obj> FromJValueOwned<'obj> for JThrowable<'obj> {
    fn from_jvalue_owned_env(val: JValueOwned<'obj>, env: &mut JNIEnv<'_>) -> Self {
        let object = JObject::from_jvalue_owned_env(val, env);
        Self::from_object_owned_env(object, env)
    }
}
#[doc(hidden)]
impl<'obj, T> FromJValueOwned<'obj> for Option<T>
where T: FromObjectOwned<'obj> {
    fn from_jvalue_owned_env(val: JValueOwned<'obj>, env: &mut JNIEnv<'_>) -> Self {
        let object = match val {
            ::jni::objects::JValueGen::Object(object) => object,
            val => panic!("{}", FromJValueError::IncorrectType {
                actual: jvalue_to_str(val.borrow()),
                expected: JTYPE_OBJECT,
            })
        };
        if object.is_null() {
            None
        } else {
            Some(T::from_object_owned_env(object, env))
        }
    }
}
