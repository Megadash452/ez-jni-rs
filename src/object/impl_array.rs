use jni::objects::JClass;
use crate::utils::{create_java_prim_array, get_java_prim_array, get_object_array_converted, create_object_array_converted};
use super::*;

/// A trait that allows converting a *Java Object* to a Rust `Boxed slice` of a *Rust Type*.
/// 
/// This trait only exists because of innevitable collisions when trying to do a blanket implementation of [`FromObject`] for `Box<[T]>` where `T: FromObject`
/// because *primitives* must have a different implementation than every other type.
/// Users SHOULD NOT use this trait other than for implementing it.
/// 
/// ## Implementation
/// 
/// This trait is automatically implemented by the [`FromObject`] **derive macro**.
/// If this trait must be implemented manually, it must be implemented as follows:
/// (Note: the `'local` lifetiem can be ellided if your type does not use it)
/// ```ignore
/// impl<'local> FromArrayObject<'local> for MyType<'local> {
///     #[inline(always)]
///     fn from_array_object(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Self]>, FromObjectError> {
///         get_object_array_converted(object, |obj, env| Self::from_object_env(&obj, env), env)
///     }
///     #[inline(always)]
///     fn from_array_object_nullable(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Option<Self>]>, FromObjectError> {
///         get_object_array_converted(object, |obj, env| Option::<Self>::from_object_env(&obj, env), env)
///     }
/// }
/// ```
pub trait FromArrayObject<'local>
where Self: Sized + 'local {
    /// Creates a `boxed slice` of a Type that can be created [from a Java Object][FromObject].
    /// 
    /// Users should NOT use this trait method.
    /// Instead use the implementation of [`FromObject`] for `boxed slice` or [`Vec`].
    fn from_array_object(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Self]>, FromObjectError>;
    /// Same as [`from_array_object`][FromArrayObject::from_array_object()],
    /// but `NULL` *Object elements* in the *Java Array* become [`None`] elements in the *boxed slice*.
    /// 
    /// Users should NOT use this trait method.
    /// Instead use the implementation of [`FromObject`] for `boxed slice` or [`Vec`].
    fn from_array_object_nullable(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Option<Self>]>, FromObjectError>;
}
/// A trait that allows converting a Rust `slice` to a *Java Object*.
/// 
/// This trait only exists because of innevitable collisions when trying to do a blanket implementation of [`ToObject`] for `[T]` where `T: ToObject`
/// because *primitives* must have a different implementation than every other type.
/// Users SHOULD NOT use this trait other than for implementing it.
pub trait ToArrayObject
where Self: ToObject + Sized {
    /// Creates an *Object Array* from a `slice` of a Type that can be converted **[to a Java Object][ToObject]**.
    /// 
    /// Users should NOT use this trait method.
    /// Instead use the implementation of [`ToObject`] for `slices`.
    #[inline(always)]
    fn to_array_object<'local>(slice: &[Self], env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(slice, Self::to_object_env, env)
    }
}

// -- Blanket Implementations --

impl<'local, T> FromObject<'_, '_, 'local> for Box<[T]>
where T: FromArrayObject<'local> + 'local {
    fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        <T as FromArrayObject>::from_array_object(object, env)
    }
}
impl<T> ToObject for [T]
where T: ToArrayObject + ToObject {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <T as ToArrayObject>::to_array_object(self, env)
    }
}
impl<T> ToObject for &[T]
where T: ToArrayObject + ToObject {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <[T] as ToObject>::to_object_env(self, env)
    }
}
impl<const N: usize, T> ToObject for [T; N]
where [T]: ToObject {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <[T] as ToObject>::to_object_env(self, env)
    }
}

impl<'local, T> FromObject<'_, '_, 'local> for Box<[Option<T>]>
where T: FromArrayObject<'local> + 'local {
    fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        <T as FromArrayObject>::from_array_object_nullable(object, env)
    }
}
impl<T> ToObject for [Option<T>]
where T: ToObject {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(self, Option::<T>::to_object_env, env)
    }
}
impl<T> ToObject for [&Option<T>]
where T: ToObject {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(self, <&Option::<T>>::to_object_env, env)
    }
}

impl<'local, T> FromObject<'_, '_, 'local> for Vec<T>
where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local> {
    fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        Ok(Box::<[T]>::from_object_env(object, env)?.into_vec())
    }
}

// -- --

// -- Implementations --

macro_rules! impl_prim_array {
    ($prim:ty) => {
        impl FromArrayObject<'_> for $prim {
            #[inline(always)]
            fn from_array_object(object: &JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Box<[Self]>, FromObjectError> {
                get_java_prim_array(object, env)
            }
            #[inline(always)]
            fn from_array_object_nullable(object: &JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Box<[Option<Self>]>, FromObjectError> {
                get_object_array_converted(object, |obj, env| Option::<Self>::from_object_env(&obj, env), env)
            }
        }
        impl ToArrayObject for $prim {
            #[inline(always)]
            fn to_array_object<'local>(slice: &[Self], env: &mut JNIEnv<'local>) -> JObject<'local> {
                create_java_prim_array(slice, env)
            }
        }
    };
}
impl_prim_array!(bool);
impl_prim_array!(char);
impl_prim_array!(i8);
impl_prim_array!(i16);
impl_prim_array!(i32);
impl_prim_array!(i64);
impl_prim_array!(f32);
impl_prim_array!(f64);
impl_prim_array!(u8);
impl_prim_array!(u16);
impl_prim_array!(u32);
impl_prim_array!(u64);

// -- Object types
impl<'local> FromArrayObject<'local> for JObject<'local> {
    #[inline(always)]
    fn from_array_object(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Self]>, FromObjectError> {
        get_object_array_converted(object, Self::from_object_owned_env, env)
    }
    #[inline(always)]
    fn from_array_object_nullable(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Option<Self>]>, FromObjectError> {
        get_object_array_converted(object, Option::<Self>::from_object_owned_env, env)
    }
}
impl ToArrayObject for JObject<'_> { }
impl<'local> FromArrayObject<'local> for JClass<'local> {
    #[inline(always)]
    fn from_array_object(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Self]>, FromObjectError> {
        get_object_array_converted(object, Self::from_object_owned_env, env)
    }
    #[inline(always)]
    fn from_array_object_nullable(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Option<Self>]>, FromObjectError> {
        get_object_array_converted(object, Option::<Self>::from_object_owned_env, env)
    }
}
impl ToArrayObject for JClass<'_> { }
impl<'local> FromArrayObject<'local> for JThrowable<'local> {
    #[inline(always)]
    fn from_array_object(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Self]>, FromObjectError> {
        get_object_array_converted(object, Self::from_object_owned_env, env)
    }
    #[inline(always)]
    fn from_array_object_nullable(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Option<Self>]>, FromObjectError> {
        get_object_array_converted(object, Option::<Self>::from_object_owned_env, env)
    }
}
impl ToArrayObject for JThrowable<'_> { }

// -- Rust Types
impl FromArrayObject<'_> for String {
    #[inline(always)]
    fn from_array_object(object: &JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Box<[Self]>, FromObjectError> {
        get_object_array_converted(object, |obj, env| Self::from_object_env(&obj, env), env)
    }
    #[inline(always)]
    fn from_array_object_nullable(object: &JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Box<[Option<Self>]>, FromObjectError> {
        get_object_array_converted(object, |obj, env| Option::<Self>::from_object_env(&obj, env), env)
    }
}
impl ToArrayObject for String { }
impl ToArrayObject for &str { }

impl<T> ToArrayObject for &T
where T: ToArrayObject { }

// For recursive blanket implementation
impl<'local, T> FromArrayObject<'local> for Box<[T]>
where T: FromArrayObject<'local> + 'local {
    #[inline(always)]
    fn from_array_object(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Self]>, FromObjectError> {
        get_object_array_converted(object, |obj, env| Self::from_object_env(&obj, env), env)
    }
    #[inline(always)]
    fn from_array_object_nullable(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Option<Self>]>, FromObjectError> {
        get_object_array_converted(object, |obj, env| Option::<Self>::from_object_env(&obj, env), env)
    }
}
impl<T> ToArrayObject for &[T]
where T: ToArrayObject { }
impl<const N: usize, T> ToArrayObject for [T; N]
where T: ToArrayObject { }
