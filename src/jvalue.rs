use std::fmt::Display;
use jni::{objects::{JValue, JValueGen, JValueOwned}, JNIEnv};
use thiserror::Error;
use crate::{utils::get_env, FromObject, FromObjectError, ObjectArray, Primitive, ToObject};
use crate::object::array::ObjectArrayElement;

/// Get a **Rust** value from a **Java** value.
/// 
/// ## Implementation
/// 
/// This trait contains 2 methods that essentially do the same thing.
/// [`from_jvalue`][FromJValue::from_jvalue] exists for ease of use when a [`JNIEnv`] is not in scope and gets it from a global stack.
/// This method should **not** be re-implemented.
/// [`from_jvalue_env`][FromJValue::from_jvalue_env] is the method that has the actual implementation and can take any [`JNIEnv`].
/// All calls in the code of [`from_jvalue_env`][FromJValue::from_jvalue_env] should only use the `_env` counterparts so that the same [`JNIEnv`] is guaranteed to be used throughout the call.
pub trait FromJValue<'local>
where Self: Sized + 'local {
    /// Get a **Rust** value from a **Java** value.
    /// 
    /// Returns an [`Error`][FromJValueError] if the *value* was not the correct type.
    /// Will [`panic!`] if any of the underlying JNI calls fail.
    /// 
    /// Automatically captures the [`JNIEnv`] from the local stack.
    /// To pass in your own [`JNIEnv`], see [`FromJValue::from_jvalue_env`].
    // NOTE: This method is only manually implemented for void/unit/() and JObject (NOTHING else) to avoid calling get_env().
    fn from_jvalue(val: JValue<'_, '_>) -> Result<Self, FromJValueError> {
        Self::from_jvalue_env(val, get_env::<'_, 'local>())
    }
    /// Same as [`from_jvalue`][FromJValue::from_jvalue], but does not capture the [`JNIEnv`] automatically; the caller must provide it themselves.
    /// 
    /// The implementation should also only methods that allow passing a [`JNIEnv`] so that the same [`JNIEnv`] is guaranteed to be used all throughout the call.
    /// For example, use [`from_jvalue_env`][FromJValue::from_jvalue_env] instead of [`from_jvalue`][FromJValue::from_jvalue].
    /// For jni macros, the env can be specified with this syntax: `macro!(env=> ...)`.
    /// 
    /// Only implement *this* method for the trait.
    fn from_jvalue_env(val: JValue<'_, '_>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError>;
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
        actual: JValueType,
        expected: JValueType,
    },
    /// This variant occurs only when the [`JValue`] is [`Object`][JObject] and the [`FromObject`] call returned an error.
    #[error("{0}")]
    Object(#[from] FromObjectError)
}

#[derive(Debug)]
pub enum JValueType {
    Void, Bool, Char, Byte, Short, Int, Long, Float, Double, Object
}
impl JValueType {
    pub const fn from_jvalue(val: JValue<'_, '_>) -> Self {
        match val {
            JValue::Void      => Self::Void,
            JValue::Bool(_)   => Self::Bool,
            JValue::Char(_)   => Self::Char,
            JValue::Byte(_)   => Self::Byte,
            JValue::Short(_)  => Self::Short,
            JValue::Int(_)    => Self::Int,
            JValue::Long(_)   => Self::Long,
            JValue::Float(_)  => Self::Float,
            JValue::Double(_) => Self::Double,
            JValue::Object(_) => Self::Object,
        }
    }
    pub const fn to_str(&self) -> &'static str {
        match self {
            Self::Void   => "void",
            Self::Bool   => <bool as Primitive>::JNAME,
            Self::Char   => <char as Primitive>::JNAME,
            Self::Byte   => <i8   as Primitive>::JNAME,
            Self::Short  => <i16  as Primitive>::JNAME,
            Self::Int    => <i32  as Primitive>::JNAME,
            Self::Long   => <i64  as Primitive>::JNAME,
            Self::Float  => <f32  as Primitive>::JNAME,
            Self::Double => <f64  as Primitive>::JNAME,
            Self::Object => "object",
        }
    }
}
impl From<JValue<'_, '_>> for JValueType {
    fn from(val: JValue<'_, '_>) -> Self {
        Self::from_jvalue(val)
    }
}
impl Display for JValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str())
    }
}

// ---- IMPLEMENTATIONS ----

// -- Primitives

impl FromJValue<'_> for () {
    // There is no need for a JNIEnv here.
    fn from_jvalue(val: JValue<'_, '_>) -> Result<Self, FromJValueError> {
        match val {
            JValueGen::Void => Ok(()),
            val => Err(FromJValueError::IncorrectType {
                actual: JValueType::from(val),
                expected: JValueType::Void,
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

impl FromJValue<'_> for bool {
    fn from_jvalue_env(val: JValue<'_, '_>, env: &mut JNIEnv<'_>) -> Result<Self, FromJValueError> {
        match val {
            JValueGen::Bool(b) => Ok(crate::utils::jboolean_to_bool(b)),
            JValueGen::Object(object) => Ok(Self::from_object_env(object, env)?),
            val => Err(FromJValueError::IncorrectType {
                actual: JValueType::from(val),
                expected: JValueType::Bool,
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

impl FromJValue<'_> for char {
    fn from_jvalue_env<'a, 'local>(val: JValue<'_, '_>, env: &mut JNIEnv<'local>) -> Result<Self, FromJValueError> {
        match val {
            // Decode UTF-16
            JValueGen::Char(c) => Ok(crate::utils::jchar_to_char(c)),
            JValueGen::Object(object) => Ok(Self::from_object_env(object, env)?),
            val => Err(FromJValueError::IncorrectType {
                actual: JValueType::from(val),
                expected: JValueType::Char,
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
    (for $ty:ty, $jvariant:ident, $jty:ident) => {
        impl FromJValue<'_> for $ty {
            fn from_jvalue_env(val: ::jni::objects::JValue<'_, '_>, env: &mut ::jni::JNIEnv<'_>) -> Result<Self, FromJValueError> {
                match val {
                    ::jni::objects::JValueGen::$jvariant(val) => Ok(val as Self),
                    ::jni::objects::JValueGen::Object(object) => Ok(Self::from_object_env(object, env)?),
                    val => Err(FromJValueError::IncorrectType {
                        actual: JValueType::from(val),
                        expected: JValueType::$jvariant,
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

map_primitive_impl!(for i8,  Byte,   jbyte);
map_primitive_impl!(for i16, Short,  jshort);
map_primitive_impl!(for i32, Int,    jint);
map_primitive_impl!(for i64, Long,   jlong);
map_primitive_impl!(for u8,  Byte,   jbyte);
map_primitive_impl!(for u16, Short,  jshort);
map_primitive_impl!(for u32, Int,    jint);
map_primitive_impl!(for u64, Long,   jlong);
map_primitive_impl!(for f32, Float,  jfloat);
map_primitive_impl!(for f64, Double, jdouble);

//  -- Objects

#[doc(hidden)]
#[macro_export]
macro_rules! impl_from_jvalue_env {
    () => { impl_from_jvalue_env! { <'_> } };
    (< $lt_local:lifetime >) => {
        fn from_jvalue_env(val: ::jni::objects::JValue<'_, '_>, env: &mut ::jni::JNIEnv<$lt_local>) -> ::std::result::Result<Self, ::ez_jni::FromJValueError> {
            match val {
                ::jni::objects::JValue::Object(object) => <Self as ::ez_jni::FromObject>::from_object_env(object, env).map_err(::ez_jni::FromJValueError::from),
                val => ::std::result::Result::Err(::ez_jni::FromJValueError::IncorrectType {
                    actual: ::ez_jni::JValueType::from(val),
                    expected: ::ez_jni::JValueType::Object,
                })
            }
        }
    };
}
macro_rules! impl_to_jvalue_env {
    () => {
        fn to_jvalue_env<'local>(&self, env: &mut JNIEnv<'local>) -> JValueOwned<'local> {
            ::jni::objects::JValueGen::Object(self.to_object_env(env))
        }
    };
}

impl<T> ToJValue for &T
where T: ToJValue {
    fn to_jvalue_env<'local>(&self, env: &mut JNIEnv<'local>) -> JValueOwned<'local> {
        <T as ToJValue>::to_jvalue_env(self, env)
    }
}

impl FromJValue<'_> for String { impl_from_jvalue_env!(); }
impl ToJValue for String { impl_to_jvalue_env!(); }
impl ToJValue for str { impl_to_jvalue_env!(); }
impl ToJValue for &str { impl_to_jvalue_env!(); }

impl<'local, T> FromJValue<'local> for Option<T>
where T: FromObject<'local> { impl_from_jvalue_env!(<'local>); }
impl<T> ToJValue for Option<T>
where T: ToObject { impl_to_jvalue_env!(); }

impl<'local, T> FromJValue<'local> for Box<[T]>
where Self: for<'a, 'obj> FromObject<'local> + 'local { impl_from_jvalue_env!(<'local>); }
impl<'local, T> FromJValue<'local> for Vec<T>
where Self: for<'a, 'obj> FromObject<'local> + 'local { impl_from_jvalue_env!(<'local>); }
impl<T> ToJValue for [T]
where Self: ToObject { impl_to_jvalue_env!(); }
impl<T> ToJValue for &[T]
where Self: ToObject { impl_to_jvalue_env!(); }
impl<const N: usize, T> ToJValue for [T; N]
where Self: ToObject { impl_to_jvalue_env!(); }

impl<'local, T, Array> FromJValue<'local> for ObjectArray<'local, T, Array>
where Self: for<'a, 'obj> FromObject<'local>,
      Array: AsRef<[T]>,
          T: ObjectArrayElement
{ impl_from_jvalue_env!(<'local>); }
impl<T, Array> ToJValue for ObjectArray<'_, T, Array>
where Array: AsRef<[T]>,
          T: ObjectArrayElement
{ impl_to_jvalue_env!(); }