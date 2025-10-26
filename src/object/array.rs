use std::{borrow::Cow, marker::PhantomData, ops::{Deref, DerefMut}};
use jni::objects::{GlobalRef, JClass, JString};

use crate::{__throw::get_jni_error_msg};
use super::*;

// TODO: doc
pub trait ObjectArrayElement<'obj>: Sized + 'obj {
    // get_object_array() is not necessary, that is handled by <T as FromObject>.
    fn create_object_array<'local>(slice: &[Self], elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local>;
}
impl<'obj, T> ObjectArrayElement<'obj> for Option<T>
where T: ObjectArrayElement<'obj> {
    #[inline(always)]
    fn create_object_array<'local>(slice: &[Self], elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        // TODO: need to convert &[Option<T>] to &[T]    &[Option<Vec<T>>] &[Vec<T>]
        // Maybe make the slice argument &[Option<Self>]? but the mapping is innefficient
        T::create_object_array(todo!(), elem_class, env)
    }
}
impl<'obj, T> ObjectArrayElement<'obj> for Vec<T>
where T: ObjectArrayElement<'obj> {
    #[inline(always)]
    fn create_object_array<'local>(slice: &[Self], elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_obj_multid_array(slice, elem_class, env)
    }
}
impl<'obj, T> ObjectArrayElement<'obj> for Box<[T]>
where T: ObjectArrayElement<'obj> {
    #[inline(always)]
    fn create_object_array<'local>(slice: &[Self], elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_obj_multid_array(slice, elem_class, env)
    }
}
impl<'a: 'obj, 'obj, T> ObjectArrayElement<'obj> for &'a [T]
where T: ObjectArrayElement<'obj> {
    #[inline(always)]
    fn create_object_array<'local>(slice: &[Self], elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_obj_multid_array(slice, elem_class, env)
    }
}
impl<'obj, T, const N: usize> ObjectArrayElement<'obj> for [T; N]
where T: ObjectArrayElement<'obj> {
    #[inline(always)]
    fn create_object_array<'local>(slice: &[Self], elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_obj_multid_array(slice, elem_class, env)
    }
}

// No blanket implementation? No custom types? ;-;
/* impl<'obj, Array, T> ObjectArrayElement<'obj> for Array
where Array: AsRef<[T]> + 'obj,
          T: ObjectArrayElement<'obj>
{

}
impl<'obj, R> ObjectArrayElement<'obj> for R
where R: AsRef<JObject<'obj>> + 'obj {

} */

macro_rules! impl_obj_arr_elem {
    ($ty:ty) => {
        impl<'obj> ObjectArrayElement<'obj> for $ty {
            #[inline(always)]
            fn create_object_array<'local>(slice: &[Self], elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
                $crate::utils::create_object_array(slice, elem_class, env)
            }
        }
    };
}
impl_obj_arr_elem!(JObject<'obj>);
impl_obj_arr_elem!(JThrowable<'obj>);
impl_obj_arr_elem!(JClass<'obj>);
impl_obj_arr_elem!(JString<'obj>);
impl_obj_arr_elem!(GlobalRef);

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
#[derive(Debug)]
pub struct ObjectArray<'obj, T = JObject<'obj>, Array = Box<[T]>>
where Array: AsRef<[T]> + 'obj,
          T: ObjectArrayElement<'obj>
{
    // TODO: Remove all implementations for other Object Reference types?
    // TODO: How to do Multi-dimensional array?
    // TODO: allow options too
    // TODO: maybe add local_frame when creating, and pop_local frame when dropped
    // FIXME: Maybe also do Cow<[T]> and remove the Array generic?
    array: Array,
    elem_class: Cow<'static, str>,
    _t: PhantomData<T>,
    _lt: PhantomData<&'obj ()>
}
impl<'obj, T, Array> ObjectArray<'obj, T, Array>
where Array: AsRef<[T]>,
          T: ObjectArrayElement<'obj>
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
where T: ObjectArrayElement<'local>
       + FromArrayObject<'local>
{
    fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        // Get the elem_class early to do the class check.
        let elem_class = Self::get_elem_class(object, env)?;
        let array = <Box<[T]> as FromObject>::from_object_env(object, env)?;
        Ok(Self::new(array, Cow::Owned(elem_class)))
    }
}
impl<'local, T> FromObject<'_, '_, 'local> for ObjectArray<'local, T, Vec<T>>
where T: ObjectArrayElement<'local>
       + FromArrayObject<'local>
{
    fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        Ok(ObjectArray::<'local, T, Box<[T]>>::from_object_env(object, env)?
            .convert_array(Vec::from)
        )
    }
}

impl<'obj, T, Array> ToObject for ObjectArray<'obj, T, Array>
where Array: AsRef<[T]>,
          T: ObjectArrayElement<'obj> // DO NOT use for<'obj> here >:(
{
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        // FIXME: disabling this until i can figure it out
        // <T as ObjectArrayElement>::create_object_array(self.array.as_ref(), &self.elem_class, env)
        todo!()
    }
}
// impl<T, Array> Class for ObjectArray<'_, T, Array>
// where Array: AsRef<[T]>,
//           T: for<'obj> ObjectArrayElement<'obj>
// {
//     fn class() -> Cow<'static, str> {
//         Cow::Owned(format!("[L{};", self.elem_class))
//     }
// }

impl<T, Array> AsRef<[T]> for ObjectArray<'_, T, Array>
where Array: AsRef<[T]>,
          T: for<'obj> ObjectArrayElement<'obj>
{
    #[inline(always)]
    fn as_ref(&self) -> &[T] {
        self.array.as_ref()
    }
}
impl<T, Array> Deref for ObjectArray<'_, T, Array>
where Array: AsRef<[T]>,
          T: for<'obj> ObjectArrayElement<'obj>
{
    type Target = Array;
    
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.array
    }
}
impl<T, Array> DerefMut for ObjectArray<'_, T, Array>
where Array: AsRef<[T]>,
          T: for<'obj> ObjectArrayElement<'obj>
{
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.array
    }
}

// // Direct Rust array conversion methods.
// impl<'obj, T> ObjectArray<'obj, Box<[T]>, T>
// where T: ObjectArrayElement<'obj> {
//     #[inline(always)]
//     pub fn into_vec(self) -> ObjectArray<'obj, Vec<T>, T> {
//         ObjectArray::new(self.array.into_vec(), self.elem_class)
//     }
// }
// impl<'obj, T> ObjectArray<'obj, Vec<T>, T>
// where T: ObjectArrayElement<'obj> {
//     #[inline(always)]
//     pub fn into_vec(self) -> ObjectArray<'obj, Box<[T]>, T> {
//         ObjectArray::new(self.array.into_boxed_slice(), self.elem_class)
//     }
// }

fn create_obj_multid_array<'obj, 'local, A, T>(slice: impl AsRef<[A]>, elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local>
where A: ObjectArrayElement<'obj>
       + AsRef<[T]>,
      T: ObjectArrayElement<'obj>
{
    crate::utils::create_object_array_converted(slice.as_ref(), |slice, env| {
        // TODO: remove one array from elem_class
        // TODO: Cancel that. Instead, user gives Class Path of the bottom-most array class (e.g. for `[[[[String]]]]` it would be `String`) and construct the array type string upwards
        T::create_object_array(slice.as_ref(), elem_class, env)
    }, elem_class, env)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Tests that all applicable types can be used as array elements.
    #[test]
    fn object_array_element_types() {
        let _: ObjectArray<'_, JObject, &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, JThrowable, &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, JClass, &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, JString, &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, GlobalRef, &[_]> = ObjectArray::from(&[], "/");
        // Option
        let _: ObjectArray<'_, Option<JObject>, &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, Option<JThrowable>, &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, Option<JClass>, &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, Option<JString>, &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, Option<GlobalRef>, &[_]> = ObjectArray::from(&[], "/");
        // 2-Dimensional Array
        let _: ObjectArray<'_, &[JObject], &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, &[JThrowable], &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, &[JClass], &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, &[JString], &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, &[GlobalRef], &[_]> = ObjectArray::from(&[], "/");
        // 2-Dimensional Option Array
        let _: ObjectArray<'_, &[Option<JObject>], &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, &[Option<JThrowable>], &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, &[Option<JClass>], &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, &[Option<JString>], &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, &[Option<GlobalRef>], &[_]> = ObjectArray::from(&[], "/");
        // 3-Dimensional Array
        let _: ObjectArray<'_, &[&[JObject]], &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, &[&[JThrowable]], &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, &[&[JClass]], &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, &[&[JString]], &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, &[&[GlobalRef]], &[_]> = ObjectArray::from(&[], "/");
        // 3-Dimensional Mixed
        let _: ObjectArray<'_, &[Box<[JObject]>], &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, &[Box<[JThrowable]>], &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, &[Box<[JClass]>], &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, &[Box<[JString]>], &[_]> = ObjectArray::from(&[], "/");
        let _: ObjectArray<'_, &[Box<[GlobalRef]>], &[_]> = ObjectArray::from(&[], "/");
    }
}