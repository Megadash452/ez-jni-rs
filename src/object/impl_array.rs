use std::{borrow::Cow, marker::PhantomData, mem::MaybeUninit, ops::{Deref, DerefMut}};
use jni::objects::{JClass, JString};
use crate::{
    utils::{create_java_prim_array, create_object_array, create_object_array_converted, get_java_prim_array, get_object_array_converted, get_object_array_owned},
    __throw::get_jni_error_msg
};
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
where Self: ToObject + Class + Sized {
    /// Creates an *Object Array* from a `slice` of a Type that can be converted **[to a Java Object][ToObject]**.
    /// 
    /// Users should NOT use this trait method.
    /// Instead use the implementation of [`ToObject`] for `slices`.
    #[inline(always)]
    // TODO: use T: AsRef<> for the element type
    fn to_array_object<'local>(slice: &[Self], env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(slice, Self::to_object_env, &Self::class(), env)
    }
}

// -- Blanket Implementations --

impl<'local, const N: usize, T> FromObject<'_, '_, 'local> for [T; N]
where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local> {
    fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        // Get an unized array from the Java Array
        let boxed = Box::<[T]>::from_object_env(object, env)?;
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
impl<'local, T> FromObject<'_, '_, 'local> for Box<[T]>
where T: FromArrayObject<'local> + 'local {
    #[inline(always)]
    fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        <T as FromArrayObject>::from_array_object(object, env)
    }
}
impl<T> ToObject for [T]
where T: ToArrayObject {
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <T as ToArrayObject>::to_array_object(self, env)
    }
}
impl<T> ToObject for &[T]
where T: ToArrayObject {
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <[T] as ToObject>::to_object_env(self, env)
    }
}
impl<const N: usize, T> ToObject for [T; N]
where [T]: ToObject {
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <[T] as ToObject>::to_object_env(self, env)
    }
}

impl<'local, T> FromObject<'_, '_, 'local> for Box<[Option<T>]>
where T: FromArrayObject<'local> + 'local {
    #[inline(always)]
    fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        <T as FromArrayObject>::from_array_object_nullable(object, env)
    }
}
impl<T> ToObject for [Option<T>]
where T: ToObject + Class {
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(self, Option::<T>::to_object_env, &T::class(), env)
    }
}
impl<T> ToObject for [&Option<T>]
where T: ToObject + Class {
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(self, <&Option::<T>>::to_object_env, &T::class(), env)
    }
}

impl<'local, T> FromObject<'_, '_, 'local> for Vec<T>
where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local>,
      T: Class {
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
                get_object_array_converted(object, env)
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
macro_rules! impl_obj_array {
    // Implement only FromArrayObject
    (FromArrayObject for $ty:ty) => {
        impl<'local> FromArrayObject<'local> for $ty {
            #[inline(always)]
            fn from_array_object(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Self]>, FromObjectError> {
                get_object_array_owned(object, Self::from_object_owned_env, env)
            }
            #[inline(always)]
            fn from_array_object_nullable(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Option<Self>]>, FromObjectError> {
                get_object_array_owned(object, Option::<Self>::from_object_owned_env, env)
            }
        }
    };
    // Implement both From/ToArrayObject
    ($ty:ty) => {
        impl_obj_array!(FromArrayObject for $ty);
        impl<'local> ToArrayObject for $ty {
            #[inline(always)]
            fn to_array_object<'env>(slice: &[Self], env: &mut JNIEnv<'env>) -> JObject<'env> {
                crate::utils::create_object_array(slice, &<Self as Class>::class(), env)
            }
        }
    };
}
// JObject should NOT implement FromArrayObject because the Array must carry the class data.
// Instead use the ObjectArray struct.
impl_obj_array!(FromArrayObject for JObject<'local>);
impl_obj_array!(JClass<'local>);
impl_obj_array!(JThrowable<'local>);
impl_obj_array!(JString<'local>);

// -- Rust Types
impl FromArrayObject<'_> for String {
    #[inline(always)]
    fn from_array_object(object: &JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Box<[Self]>, FromObjectError> {
        get_object_array_converted(object, env)
    }
    #[inline(always)]
    fn from_array_object_nullable(object: &JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Box<[Option<Self>]>, FromObjectError> {
        get_object_array_converted(object, env)
    }
}
impl ToArrayObject for String { }
impl ToArrayObject for &str { }

impl<T> ToArrayObject for &T
where T: ToArrayObject { }

// For recursive blanket implementation
impl<'local, T> FromArrayObject<'local> for Box<[T]>
where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local> + 'local {
    #[inline(always)]
    fn from_array_object(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Self]>, FromObjectError> {
        get_object_array_converted(object, env)
    }
    #[inline(always)]
    fn from_array_object_nullable(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Option<Self>]>, FromObjectError> {
        get_object_array_converted(object, env)
    }
}
impl<'local, const N: usize, T> FromArrayObject<'local> for [T; N]
where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local> + 'local {
    #[inline(always)]
    fn from_array_object(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Self]>, FromObjectError> {
        get_object_array_converted(object, env)
    }
    #[inline(always)]
    fn from_array_object_nullable(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Box<[Option<Self>]>, FromObjectError> {
        get_object_array_converted(object, env)
    }
}
impl<T> ToArrayObject for &[T]
where T: ToArrayObject { }
impl<const N: usize, T> ToArrayObject for [T; N]
where T: ToArrayObject { }


// Implement ToObject for a pair of an Slice and a Class Path.

/// A *Rust representation* of an **Array** that holds [`JObject`]s.
/// 
/// ## Types
/// 
/// This type contains a generic `Array` and a generic *element* (`T`) type.
/// By default, the `Array` is a **boxed slice** (`Box<[T]>`) and `T` is [`JObject`],
/// but these two types can be anything that fits the constraints of the generics:
/// 
/// The `Array` can be any type that can read as a **slice** of `T`,
/// but when calling [`from_object()`][FromObject::from_object_env()],
/// it can only return an `Array` type of either [`Vec<T>`] or `Box<[T]>`.
/// 
/// The element `T` can be any *JNI Object reference type* (i.e. [`JObject`], `&JObject`, [`GlobalRef`][jni::objects::GlobalRef], etc.).
/// Since all these types are essentially the same under the hood (an *object reference*),
/// they can be trivially cast to [`JObject`].
/// 
/// [`ObjectArray`] also implements [`Deref`] and [`DerefMut`] for the `Array` type,
/// so you can call the `Array` methods directly from this type.
/// Otherwise, you can call [`self.array()`][ObjectArray::array()] or [`self.array_mut()`][ObjectArray::array_mut()] to access the `Array`.
/// 
/// ## Element Class
/// 
/// Java Arrays store information about the **Class** its **elements** can be.
/// Constructing a *Java Array* requires passing in the **element class**.
/// The elements are *constrained* to be instances of the *Class*,
/// but they can really be of any *Class* that extends the Array's **element class**.
/// 
/// It is impossible to have a consistent method of determining the **element class** from a *slice of objects* alone
/// (because of what was mentioned previously, but also because the array can be empty).
/// To resolve this, [`ObjectArray`] stores the Java Array's **element class**.
pub struct ObjectArray<'obj, T = JObject<'obj>, Array = Box<[T]>>
where Array: AsRef<[T]>,
          T: AsRef<JObject<'obj>>
{
    // TODO: Remove all implementations for other Object Reference types?
    // TODO: How to do Multi-dimensional array?
    // TODO: allow options too
    array: Array,
    elem_class: Cow<'static, str>,
    _t: PhantomData<T>,
    _lt: PhantomData<&'obj ()>
}
impl<'obj, T, Array> ObjectArray<'obj, T, Array>
where Array: AsRef<[T]>,
          T: AsRef<JObject<'obj>>
{
    #[inline(always)]
    fn new(array: Array, elem_class: Cow<'static, str>) -> Self {
        Self { array, elem_class, _t: PhantomData, _lt: PhantomData }
    }
    #[inline(always)]
    pub fn from(array: Array, elem_class: &'static str) -> Self {
        Self::new(array, Cow::Borrowed(elem_class))
    }
    
    /// Helps with converting the `Array` type to a different *Rust type*.
    /// 
    /// E.g. convert `ObjectArray<Box<[_]>>` to `ObjectArray<Vec<_>>`.
    #[inline(always)]
    pub fn convert_array<OtherArray>(self, conversion: impl FnOnce(Array) -> OtherArray) -> ObjectArray<'obj, T, OtherArray>
    where OtherArray: AsRef<[T]> {
        ObjectArray::new(conversion(self.array), self.elem_class)
    }

    /// Returns the **slice** of the inner `Array`.
    #[inline(always)]
    pub fn array(&self) -> &[T] {
        self.array.as_ref()
    }
    /// Returns the inner `Array` as **mutable**.
    #[inline(always)]
    pub fn array_mut(&mut self) -> &Array {
        &mut self.array
    }
    
    /// Returns the **class** of the [`JObject`] Array elements.
    #[inline(always)]
    pub fn elem_class(&self) -> &str {
        &self.elem_class
    }
    
    /// Get the **element class** of an *Array Object*.
    /// 
    /// Returns a [`ClassMismatch`][FromObjectError::ClassMismatch] error if the object was *not* an *Array Object*.
    fn get_elem_class(obj: &JObject<'_>, env: &mut JNIEnv<'_>) -> Result<String, FromObjectError> {
        // Get the class of the Array Object.
        let obj_class = env.get_object_class(obj)
            .map_err(|err| FromObjectError::Other(format!("Could not get Object's class: {}", get_jni_error_msg(err, env))))?;
        
        match call!(obj_class.getComponentType() -> Option<Class>) {
            Some(elem_class) => Ok(call!(elem_class.getName() -> String)),
            // When getComponentType returns null, the class was not an Array class.
            None => Err(FromObjectError::ClassMismatch {
                obj_class: call!(obj_class.getName() -> String),
                target_class: Some("".to_string())
            })
        }
    }
}
impl<'local, T> FromObject<'_, '_, 'local> for ObjectArray<'local, T, Box<[T]>>
where T: AsRef<JObject<'local>> + From<JObject<'local>> {
    fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        // Get the elem_class early to do the class check.
        let elem_class = Self::get_elem_class(object, env)?;
        let array = get_object_array_owned(object, |elem, _| Ok(T::from(elem)), env)?;
        Ok(Self::new(array, Cow::Owned(elem_class)))
    }
}
impl<'local, T> FromObject<'_, '_, 'local> for ObjectArray<'local, T, Vec<T>>
where T: AsRef<JObject<'local>> + From<JObject<'local>> {
    fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        Ok(ObjectArray::<'local, T, Box<[T]>>::from_object_env(object, env)?
            .convert_array(Vec::from)
        )
    }
}
impl<'obj, T, Array> ToObject for ObjectArray<'obj, T, Array>
where Array: AsRef<[T]>,
          T: AsRef<JObject<'obj>> // DO NOT use for<'obj> here >:(
{
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array(self.array.as_ref(), &self.elem_class, env)
    }
}
// impl<T, Array> Class for ObjectArray<'_, T, Array>
// where Array: AsRef<[T]>,
//           T: for<'obj> AsRef<JObject<'obj>>
// {
//     fn class() -> Cow<'static, str> {
//         Cow::Owned(format!("[L{};", self.elem_class))
//     }
// }

impl<T, Array> AsRef<[T]> for ObjectArray<'_, T, Array>
where Array: AsRef<[T]>,
          T: for<'obj> AsRef<JObject<'obj>>
{
    #[inline(always)]
    fn as_ref(&self) -> &[T] {
        self.array.as_ref()
    }
}
impl<T, Array> Deref for ObjectArray<'_, T, Array>
where Array: AsRef<[T]>,
          T: for<'obj> AsRef<JObject<'obj>>
{
    type Target = Array;
    
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.array
    }
}
impl<T, Array> DerefMut for ObjectArray<'_, T, Array>
where Array: AsRef<[T]>,
          T: for<'obj> AsRef<JObject<'obj>>
{
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.array
    }
}

// // Direct Rust array conversion methods.
// impl<'obj, T> ObjectArray<'obj, Box<[T]>, T>
// where T: AsRef<JObject<'obj>> {
//     #[inline(always)]
//     pub fn into_vec(self) -> ObjectArray<'obj, Vec<T>, T> {
//         ObjectArray::new(self.array.into_vec(), self.elem_class)
//     }
// }
// impl<'obj, T> ObjectArray<'obj, Vec<T>, T>
// where T: AsRef<JObject<'obj>> {
//     #[inline(always)]
//     pub fn into_vec(self) -> ObjectArray<'obj, Box<[T]>, T> {
//         ObjectArray::new(self.array.into_boxed_slice(), self.elem_class)
//     }
// }