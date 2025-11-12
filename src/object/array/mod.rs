mod element;

use super::*;
use std::{borrow::{Borrow, BorrowMut, Cow}, fmt::Debug, hash::{Hash, Hasher}, marker::PhantomData, ops::{Deref, DerefMut, Index, IndexMut}, slice::SliceIndex};
use crate::utils::get_elem_class;
use element::FromArrayObject;
pub use element::ObjectArrayElement;

// No blanket implementation? No custom types? ;-;
/* impl<'obj, Array, T> ObjectArrayElement<'obj> for Array
where Array: AsRef<[T]> + 'obj,
          T: ObjectArrayElement<'obj>
{

}
impl<'obj, R> ObjectArrayElement<'obj> for R
where R: AsRef<JObject<'obj>> + 'obj {

} */

/// A *Rust representation* of an **Array** that holds [`JObject`]s.
/// It allows storing *multi-dimensional* Rust array types,
/// along with the **class** that the [`JObject`] should have.
/// 
/// ## Types
/// 
/// This type contains a generic `Array` and a generic *element* (`T`) type.
/// By default, the `Array` is a **boxed slice** (`Box<[T]>`) and `T` is [`JObject`],
/// but these two types can be anything that fits the constraints of the generics:
/// 
/// * The `Array` can be any type that can be read as a **slice** of `T`,
/// but when calling [`from_object()`][FromObject::from_object_env()],
/// it can only return an `Array` type of either [`Vec<T>`] or `Box<[T]>`.
/// 
/// * The element `T` can be any *JNI Object reference type* (i.e. [`JObject`], `&JObject`, [`GlobalRef`][jni::objects::GlobalRef], etc.).
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
where Array: AsRef<[T]>,
          T: ObjectArrayElement
{
    // TODO: maybe add local_frame when creating, and pop_local frame when dropped. Essentially, the struct holds a local frame.
    // TODO: Maybe implement it with an enum with states of an Array Object and a Rust array. When Rust needs to operate on the array it calls the necessary jni methods.
    // FIXME: Maybe also do Cow<[T]> and remove the Array generic?
    array: Array,
    elem_class: Cow<'static, str>,
    _t: PhantomData<T>,
    _lt: PhantomData<&'obj ()>
}
impl<'a, 'obj, T> ObjectArray<'obj, T, &'a [T]>
where T: ObjectArrayElement {
    /// Create a new [`ObjectArray`], but only borrowing existing values.
    /// 
    /// Does not create any new Rust values or Java *Object References*.
    /// 
    /// This is useful when you need to convert a Rust *slice of Object References* to a Java *Array Object*.
    #[inline(always)]
    pub fn new_ref(slice: &'a [T], elem_class: &'static str) -> Self {
        Self::new(slice, Cow::Borrowed(elem_class))
    }
}
impl<'obj, T, Array> ObjectArray<'obj, T, Array>
where Array: AsRef<[T]>,
          T: ObjectArrayElement
{
    #[inline(always)]
    fn new(array: Array, elem_class: Cow<'static, str>) -> Self {
        // TODO: check class of elements?
        Self { array, elem_class, _t: PhantomData, _lt: PhantomData }
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
    pub fn as_slice(&self) -> &[T] {
        self.array.as_ref()
    }
    /// Returns a **borrowed** reference to the inner `Array`.
    #[inline(always)]
    pub fn array(&self) -> &Array {
        &self.array
    }
    /// Returns a **mutable** reference to the inner `Array`.
    #[inline(always)]
    pub fn array_mut(&mut self) -> &mut Array {
        &mut self.array
    }
    /// Returns the **owned** inner `Array`.
    #[inline(always)]
    pub fn take_array(self) -> Array {
        self.array
    }
    
    /// Returns the **class** of the [`JObject`] Array elements.
    #[inline(always)]
    pub fn elem_class(&self) -> &str {
        &self.elem_class
    }
}

// TODO: allow Array to be any type that implements FromObject2 (maybe call that trait something better?)
impl<'local, T> FromObject<'local> for ObjectArray<'local, T, Box<[T]>>
where T: FromArrayObject<'local> {
    fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        // Get the elem_class early to do the class check.
        let elem_class = get_elem_class(object, env)?;
        let array = <T as FromArrayObject>::from_array_object(object, env)?;
        Ok(Self::new(array, Cow::Owned(elem_class)))
    }
}
impl<'local, T> FromObject<'local> for ObjectArray<'local, T, Vec<T>>
where T: element::FromArrayObject<'local> {
    fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        Ok(ObjectArray::<'local, T, Box<[T]>>::from_object_env(object, env)?
            .convert_array(Vec::from)
        )
    }
}
impl<'obj, T, Array> ToObject for ObjectArray<'obj, T, Array>
where Array: AsRef<[T]>,
          T: ObjectArrayElement
{
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <T as element::ToArrayObject>::to_array_object(self.array.as_ref(), self.elem_class(), env)
    }
}
// impl<T, Array> Class for ObjectArray<'_, T, Array>
// where Array: AsRef<[T]>,
//           T: ObjectArrayElement
// {
//     fn class() -> Cow<'static, str> {
//         Cow::Owned(format!("[L{};", self.elem_class))
//     }
// }

impl<T, Array> IntoIterator for ObjectArray<'_, T, Array>
where Array: AsRef<[T]> + IntoIterator,
          T: ObjectArrayElement,
{
    type Item = <Array as IntoIterator>::Item;
    type IntoIter = <Array as IntoIterator>::IntoIter;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self.array.into_iter()
    }
}
impl<I, T, Array> Index<I> for ObjectArray<'_, T, Array>
where I: SliceIndex<[T]>,
  Array: AsRef<[T]>,
      T: ObjectArrayElement,
{
    type Output = <I as SliceIndex<[T]>>::Output;

    #[inline(always)]
    fn index(&self, index: I) -> &Self::Output {
        self.as_ref().index(index)
    }
}
impl<I, T, Array> IndexMut<I> for ObjectArray<'_, T, Array>
where I: SliceIndex<[T]>,
  Array: AsRef<[T]> + AsMut<[T]>,
      T: ObjectArrayElement,
{
    #[inline(always)]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        self.as_mut().index_mut(index)
    }
}
impl<T, Array> Hash for ObjectArray<'_, T, Array>
where Array: AsRef<[T]>,
        T: ObjectArrayElement + Hash,
{
    #[inline(always)]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.elem_class().hash(state);
        self.array.as_ref().hash(state);
    }
}
impl<T, Array> PartialEq for ObjectArray<'_, T, Array>
where Array: AsRef<[T]>,
        T: ObjectArrayElement + PartialEq,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.elem_class().eq(other.elem_class())
        && self.array.as_ref().eq(other.array.as_ref())
    }
}
impl<T, Array> PartialEq<&Self> for ObjectArray<'_, T, Array>
where Array: AsRef<[T]>,
        T: ObjectArrayElement + PartialEq,
{
    #[inline(always)]
    fn eq(&self, other: &&Self) -> bool {
        <Self as PartialEq<Self>>::eq(self, other)
    }
}
impl<T, Array> Eq for ObjectArray<'_, T, Array>
where Array: AsRef<[T]>,
        T: ObjectArrayElement + Eq { }
// Don't implement FromIterator, Ord, PartialOrd

// TODO: implement Clone (new_local_ref), Display (.toString()), Extend<T> (check class) if allowing implicit jni calls without manually passing env

impl<T, Array> AsRef<[T]> for ObjectArray<'_, T, Array>
where Array: AsRef<[T]>,
          T: ObjectArrayElement
{
    #[inline(always)]
    fn as_ref(&self) -> &[T] {
        self.array.as_ref()
    }
}
impl<T, Array> AsMut<[T]> for ObjectArray<'_, T, Array>
where Array: AsRef<[T]> + AsMut<[T]>,
          T: ObjectArrayElement
{
    #[inline(always)]
    fn as_mut(&mut self) -> &mut [T] {
        self.array.as_mut()
    }
}
impl<T, Array> Deref for ObjectArray<'_, T, Array>
where Array: AsRef<[T]>,
          T: ObjectArrayElement
{
    type Target = Array;
    
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.array
    }
}
impl<T, Array> DerefMut for ObjectArray<'_, T, Array>
where Array: AsRef<[T]>,
          T: ObjectArrayElement
{
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.array
    }
}
impl<T, Array> Borrow<[T]> for ObjectArray<'_, T, Array>
where Array: AsRef<[T]>,
          T: ObjectArrayElement
{
    #[inline(always)]
    fn borrow(&self) -> &[T] {
        self.array.as_ref()
    }
}
impl<T, Array> BorrowMut<[T]> for ObjectArray<'_, T, Array>
where Array: AsRef<[T]> + AsMut<[T]>,
          T: ObjectArrayElement
{
    #[inline(always)]
    fn borrow_mut(&mut self) -> &mut [T] {
        self.array.as_mut()
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

#[cfg(test)]
mod tests {
    use super::*;
    use jni::objects::{GlobalRef, JClass, JString};

    /// Tests that all applicable types can be used as array elements.
    #[test]
    fn object_array_element_types() {
        let _: ObjectArray<'_, JObject, &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, JThrowable, &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, JClass, &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, JString, &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, GlobalRef, &[_]> = ObjectArray::new_ref(&[], "/");
        // Option
        let _: ObjectArray<'_, Option<JObject>, &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, Option<JThrowable>, &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, Option<JClass>, &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, Option<JString>, &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, Option<GlobalRef>, &[_]> = ObjectArray::new_ref(&[], "/");
        // 2-Dimensional Array
        let _: ObjectArray<'_, &[JObject], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, &[JThrowable], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, &[JClass], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, &[JString], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, &[GlobalRef], &[_]> = ObjectArray::new_ref(&[], "/");
        // 2-Dimensional Option Array
        let _: ObjectArray<'_, &[Option<JObject>], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, &[Option<JThrowable>], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, &[Option<JClass>], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, &[Option<JString>], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, &[Option<GlobalRef>], &[_]> = ObjectArray::new_ref(&[], "/");
        // 3-Dimensional Array
        let _: ObjectArray<'_, &[&[JObject]], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, &[&[JThrowable]], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, &[&[JClass]], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, &[&[JString]], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, &[&[GlobalRef]], &[_]> = ObjectArray::new_ref(&[], "/");
        // 3-Dimensional Mixed
        let _: ObjectArray<'_, &[Box<[JObject]>], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, &[Box<[JThrowable]>], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, &[Box<[JClass]>], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, &[Box<[JString]>], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<'_, &[Box<[GlobalRef]>], &[_]> = ObjectArray::new_ref(&[], "/");
    }
}