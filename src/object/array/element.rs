use std::mem::MaybeUninit;
use jni::{JNIEnv, objects::{GlobalRef, JClass, JObject, JString, JThrowable}};
use crate::{FromObjectError, utils::{create_object_array_converted, get_object_array_owned}};
use super::ObjectArray;

pub trait ObjectArrayElement: ToObject2 { }

impl<T> ObjectArrayElement for Option<T>
where T: ObjectArrayElement { }
impl<T> ObjectArrayElement for Box<[T]>
where T: ObjectArrayElement { }
impl<T> ObjectArrayElement for Vec<T>
where T: ObjectArrayElement { }
impl<T> ObjectArrayElement for &[T]
where T: ObjectArrayElement { }
impl<T, const N: usize> ObjectArrayElement for [T; N]
where T: ObjectArrayElement { }
impl<T, Array> ObjectArrayElement for ObjectArray<'_, T, Array>
where Array: AsRef<[T]>,
      T: ObjectArrayElement { }

trait FromObject2<'local>: ObjectArrayElement + Sized {
    fn from_object(object: JObject<'local>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError>;
}
// Also acts like a seal
trait ToObject2 {
    fn to_object<'local>(&self, elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local>;
}

/// A trait that allows converting a *Java Object* to a Rust `Boxed slice` of a *Rust Type*.
pub(super) trait FromArrayObject<'local>
where Self: FromObject2<'local> + 'local {
    /// Creates a `boxed slice` of a Type that can be created [from a Java Object][FromObject].
    /// 
    /// Users should NOT use this trait method.
    /// Instead use the implementation of [`FromObject`] for `boxed slice` or [`Vec`].
    fn from_array_object(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Self]>, FromObjectError>;
}
/// A trait that allows converting a Rust `slice` to a *Java Object*.
pub(super) trait ToArrayObject: Sized {
    fn to_array_object<'local>(slice: &[Self], elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local>;
}

impl<'local, T> FromArrayObject<'local> for T
where T: FromObject2<'local> + 'local {
    #[inline(always)]
    fn from_array_object(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Self]>, FromObjectError> {
        get_object_array_owned(object, <T as FromObject2>::from_object, env)
    }
}
impl<T> ToArrayObject for T
where T: ToObject2 {
    #[inline(always)]
    fn to_array_object<'local>(slice: &[Self], elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <[T] as ToObject2>::to_object(slice, elem_class, env)
    }
}

macro_rules! impl_obj_array {
    ($ty:ty) => {
        impl<'local> ObjectArrayElement for $ty { }
        impl<'local> FromObject2<'local> for $ty {
            #[inline(always)]
            fn from_object(object: JObject<'local>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
                // TODO: maybe skip class check? If we know the Array's element class, we can check if its a descendant of Class, Exception (or whetever class the ObjectRef is for) without having to check it for every element.
                <Self as crate::FromObjectOwned>::from_object_owned_env(object, env)
            }
        }
        impl<'local> ToObject2 for $ty {
            #[inline(always)]
            fn to_object<'env>(&self, _: &str, env: &mut JNIEnv<'env>) -> JObject<'env> {
                // Can create a new_local_ref because this is done inside a local frame
                env.new_local_ref(self)
                    .unwrap_or_else(|err| $crate::__throw::handle_jni_call_error(err, env))
            }
        }
    };
}
impl_obj_array!(JObject<'local>);
impl_obj_array!(JClass<'local>);
impl_obj_array!(JThrowable<'local>);
impl_obj_array!(JString<'local>);
impl_obj_array!(GlobalRef);

impl<'local, T> FromObject2<'local> for Option<T>
where T: FromObject2<'local> {
    #[inline(always)]
    fn from_object(object: JObject<'local>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        Ok(if object.is_null() {
            None
        } else {
            Some(T::from_object(object, env)?)
        })
    }
}
impl<T> ToObject2 for Option<T>
where T: ToObject2 {
    #[inline(always)]
    fn to_object<'local>(&self, elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        match self {
            Some(t) => T::to_object(t, elem_class, env),
            None => JObject::null(),
        }
    }
}

impl<T> ToObject2 for &T
where T: ToObject2 {
    #[inline(always)]
    fn to_object<'local>(&self, elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        T::to_object(self, elem_class, env)
    }
}

impl<'local, T> FromObject2<'local> for Box<[T]>
where T: FromObject2<'local> {
    #[inline(always)]
    fn from_object(object: JObject<'local>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array_owned(&object, <T as FromObject2>::from_object, env)
    }
}
impl<'local, T> FromObject2<'local> for Vec<T>
where Box<[T]>: FromObject2<'local>,
             T: ObjectArrayElement,
{
    #[inline(always)]
    fn from_object(object: JObject<'local>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        <Box<[T]> as FromObject2>::from_object(object, env).map(<[T]>::into_vec)
    }
}
impl<'local, const N: usize, T> FromObject2<'local> for [T; N]
where Box<[T]>: for<'a, 'obj> FromObject2<'local>,
             T: ObjectArrayElement,
{
    fn from_object(object: JObject<'local>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        // Get an unized array from the Java Array
        let boxed = <Box<[T]> as FromObject2>::from_object(object, env)?;
        if boxed.len() != N {
            return Err(FromObjectError::ArrayTooLong { expected_len: N, actual_len: boxed.len() });
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
impl<'local, T, Array> FromObject2<'local> for ObjectArray<'local, T, Array>
where Array: AsRef<[T]> + FromObject2<'local>,
      T: ObjectArrayElement,
{
    fn from_object(object: JObject<'local>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        // Get the elem_class early to do the class check.
        let elem_class = crate::utils::get_elem_class(&object, env)?;
        let array = <Array as FromObject2>::from_object(object, env)?;
        Ok(Self::new(array, elem_class.into()))
    }
}

impl<T> ToObject2 for [T]
where T: ToObject2 {
    #[inline(always)]
    fn to_object<'local>(&self, elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(self, |elem, env| <T as ToObject2>::to_object(elem, elem_class, env), elem_class, env)
    }
}
impl<T> ToObject2 for Box<[T]>
where T: ToObject2 {
    #[inline(always)]
    fn to_object<'local>(&self, elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <[T] as ToObject2>::to_object(&self, elem_class, env)
    }
}
impl<T> ToObject2 for Vec<T>
where T: ToObject2 {
    #[inline(always)]
    fn to_object<'local>(&self, elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <[T] as ToObject2>::to_object(&self, elem_class, env)
    }
}
impl<T> ToObject2 for &[T]
where T: ToObject2 {
    #[inline(always)]
    fn to_object<'local>(&self, elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <[T] as ToObject2>::to_object(self, elem_class, env)
    }
}
impl<const N: usize, T> ToObject2 for [T; N]
where [T]: ToObject2 {
    #[inline(always)]
    fn to_object<'local>(&self, elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <[T] as ToObject2>::to_object(self, elem_class, env)
    }
}
impl<T, Array> ToObject2 for ObjectArray<'_, T, Array>
where Array: AsRef<[T]>,
      T: ObjectArrayElement,
{
    fn to_object<'local>(&self, elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        if elem_class != self.elem_class() {
            panic!("The expected class (\"{elem_class}\") did not match the ObjectArray's stored class (\"{}\")", self.elem_class())
        }
        <Self as crate::ToObject>::to_object_env(self, env)
    }
}
