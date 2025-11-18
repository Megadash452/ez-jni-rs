use std::mem::MaybeUninit;
use jni::{JNIEnv, objects::{GlobalRef, JClass, JObject, JString, JThrowable}};
use crate::{FromObjectError, utils::{create_object_array_converted, get_object_array_owned}};
use super::ObjectArray;

// This pattern allows renaming ToObject2 to Seal to make it clear to the user.
mod sealed {
    use super::ToObject2 as Seal;

    // NOTE: All types named in this file implement ObjectArrayElement.

    /// Sealed Marker trait for Types that can be Elements of [`ObjectArray`][super::ObjectArray].
    #[allow(private_bounds)]
    pub trait ObjectArrayElement: Seal {
        // TODO: make const
        fn get_dimensions_count() -> usize;
    }
}
pub use sealed::ObjectArrayElement;

// Using only 1 lifetime here wouldn't usually work for both lone Object References and Arrays of Object References,
// but it works in this case because Objects coming out of get_object_array() come straight out of the env,
// so they are both going to have the same lifetime.
pub(super) trait FromObject2<'local>
where Self: ObjectArrayElement + Sized + 'local {
    fn from_object(object: JObject<'local>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError>;
}
// Also acts like a seal
trait ToObject2 {
    /// **class** refers to the class of `Self`;
    /// it is an *array  class* if `Self` is an *Array Type* (e.g. `[T]`),
    /// or a *base component class* if `Self` is an *Object Reference*.
    fn to_object<'local>(&self, class: &str, env: &mut JNIEnv<'local>) -> JObject<'local>;
}

/// A trait that allows converting a Rust `slice` to a *Java Object*.
pub(super) trait ToArrayObject: Sized {
    fn to_array_object<'local>(slice: &[Self], class: &str, env: &mut JNIEnv<'local>) -> JObject<'local>;
}

impl<T> ToArrayObject for T
where T: ToObject2 {
    #[inline(always)]
    fn to_array_object<'local>(slice: &[Self], class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <[T] as ToObject2>::to_object(slice, class, env)
    }
}

macro_rules! impl_obj_array {
    ($ty:ty) => {
        impl<'obj> ObjectArrayElement for $ty {
            #[inline(always)]
            fn get_dimensions_count() -> usize { 0 }
        }
        impl<'obj> FromObject2<'obj> for $ty {
            #[inline(always)]
            fn from_object(object: JObject<'obj>, _: &mut JNIEnv<'obj>) -> Result<Self, FromObjectError> {
                // Trivial conversion between Object Refs.
                // Class check is not needed here because ObjectArray already checks array class.
                // We only need to check for null here.
                if object.is_null() {
                    return Err(FromObjectError::Null);
                }
                Ok(Self::from(object))
            }
        }
        impl<'obj> ToObject2 for $ty {
            #[inline(always)]
            fn to_object<'local>(&self, _: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
                // Can create a new_local_ref because this is done inside a local frame
                env.new_local_ref(self)
                    .unwrap_or_else(|err| $crate::__throw::handle_jni_call_error(err, env))
            }
        }
    };
}
impl_obj_array!(JObject<'obj>);
impl_obj_array!(JClass<'obj>);
impl_obj_array!(JThrowable<'obj>);
impl_obj_array!(JString<'obj>);

impl ObjectArrayElement for GlobalRef {
    #[inline(always)]
    fn get_dimensions_count() -> usize { 1 }
}
impl<'local> FromObject2<'local> for GlobalRef {
    #[inline(always)]
    fn from_object(object: JObject<'local>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        <Self as crate::FromObjectOwned>::from_object_owned_env(object, env)
    }
}
impl ToObject2 for GlobalRef {
    #[inline(always)]
    fn to_object<'local>(&self, _: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        // Can create a new_local_ref because this is done inside a local frame
        env.new_local_ref(self)
            .unwrap_or_else(|err| crate::__throw::handle_jni_call_error(err, env))
    }
}

impl<T> ObjectArrayElement for Option<T>
where T: ObjectArrayElement {
    #[inline(always)]
    fn get_dimensions_count() -> usize {
        T::get_dimensions_count()
    }
}
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
    fn to_object<'local>(&self, class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        match self {
            Some(t) => T::to_object(t, class, env),
            None => JObject::null(),
        }
    }
}

impl<T> ObjectArrayElement for &T
where T: ObjectArrayElement {
    #[inline(always)]
    fn get_dimensions_count() -> usize {
        T::get_dimensions_count()
    }
}
impl<T> ToObject2 for &T
where T: ToObject2 {
    #[inline(always)]
    fn to_object<'local>(&self, class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <T as ToObject2>::to_object(self, class, env)
    }
}

impl<T> ObjectArrayElement for Box<[T]>
where T: ObjectArrayElement {
    #[inline(always)]
    fn get_dimensions_count() -> usize {
        T::get_dimensions_count() + 1
    }
}
impl<T> ObjectArrayElement for Vec<T>
where T: ObjectArrayElement {
    #[inline(always)]
    fn get_dimensions_count() -> usize {
        <Box<[T]> as ObjectArrayElement>::get_dimensions_count()
    }
}
impl<T> ObjectArrayElement for &[T]
where T: ObjectArrayElement {
    #[inline(always)]
    fn get_dimensions_count() -> usize {
        <Box<[T]> as ObjectArrayElement>::get_dimensions_count()
    }
}
impl<T, const N: usize> ObjectArrayElement for [T; N]
where T: ObjectArrayElement {
    #[inline(always)]
    fn get_dimensions_count() -> usize {
        <Box<[T]> as ObjectArrayElement>::get_dimensions_count()
    }
}
impl<T, Array> ObjectArrayElement for ObjectArray<T, Array>
where T: ObjectArrayElement,
  Array: AsRef<[T]>,
{
    #[inline(always)]
    fn get_dimensions_count() -> usize {
        <Box<[T]> as ObjectArrayElement>::get_dimensions_count()
    }
}

impl<'local, T> FromObject2<'local> for Box<[T]>
where T: FromObject2<'local> {
    #[inline(always)]
    fn from_object(object: JObject<'local>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        // SAFETY: Callers of FromObject2 for Array types assume that object is only used as borrow (even though its owned in the signature).
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
impl<'local, T, Array> FromObject2<'local> for ObjectArray<T, Array>
where T: ObjectArrayElement + 'local,
  Array: AsRef<[T]> + FromObject2<'local>,
{
    #[inline(always)]
    fn from_object(object: JObject<'local>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        <Self as crate::FromObject>::from_object_env(&object, env)
    }
}

impl<T> ToObject2 for [T]
where T: ToObject2 {
    #[inline(always)]
    fn to_object<'local>(&self, class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        let elem_class = take_array_class_bracket(class);
        create_object_array_converted(self, |elem, env| {
            <T as ToObject2>::to_object(elem, elem_class, env)
        }, elem_class, env)
    }
}
impl<T> ToObject2 for Box<[T]>
where T: ToObject2 {
    #[inline(always)]
    fn to_object<'local>(&self, class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <[T] as ToObject2>::to_object(&self, class, env)
    }
}
impl<T> ToObject2 for Vec<T>
where T: ToObject2 {
    #[inline(always)]
    fn to_object<'local>(&self, class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <[T] as ToObject2>::to_object(&self, class, env)
    }
}
impl<T> ToObject2 for &[T]
where T: ToObject2 {
    #[inline(always)]
    fn to_object<'local>(&self, class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <[T] as ToObject2>::to_object(self, class, env)
    }
}
impl<const N: usize, T> ToObject2 for [T; N]
where [T]: ToObject2 {
    #[inline(always)]
    fn to_object<'local>(&self, class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <[T] as ToObject2>::to_object(self, class, env)
    }
}
impl<T, Array> ToObject2 for ObjectArray<T, Array>
where T: ObjectArrayElement,
  Array: AsRef<[T]>,
{
    fn to_object<'local>(&self, class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        if class != self.base_elem_class() {
            panic!("The expected class (\"{class}\") did not match the ObjectArray's stored class (\"{}\")", self.base_elem_class())
        }
        <Self as crate::ToObject>::to_object_env(self, env)
    }
}

/// Removes a *bracket* (`[`) from the **array class**, subtracting one dimension.
/// 
/// `panic!`s if the class was not an array (i.e. did not contain a bracket).
fn take_array_class_bracket(array_class: &str) -> &str {
    array_class.strip_prefix('[')
        .unwrap_or_else(|| panic!("Expected elem_class \"{array_class}\" to be an Array Class."))
}
