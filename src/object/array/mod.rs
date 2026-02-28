mod element;

use super::*;
use std::{borrow::{Borrow, BorrowMut, Cow}, fmt::Debug, hash::{Hash, Hasher}, marker::PhantomData, ops::{Deref, DerefMut, Index, IndexMut}, slice::SliceIndex};
use crate::utils::get_object_class_name;
use element::FromObject2 as ArrayFromObject;
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
/// it can only return an `Array` type of either [`Vec<T>`], `Box<[T]>`, or `[T; N]`.
/// 
/// * The element `T` can be any *JNI Object reference type* (i.e. [`JObject`], `&JObject`, [`GlobalRef`][jni::objects::GlobalRef], etc.).
/// Since all these types are essentially the same under the hood (an *object reference*),
/// they can be trivially cast to [`JObject`].
///   * The element `T` can also be an `Array` itself, so [`ObjectArray`] supports **multi-dimensional** arrays.
/// 
/// [`ObjectArray`] also implements [`Deref`] and [`DerefMut`] for the `Array` type,
/// so you can call the `Array` methods directly from this type.
/// There is also the [`take_array()`][ObjectArray::take_array()] method to move `Array` out of [`ObjectArray`],
/// but you will need to keep track of the **base component's class** independently.
/// 
/// ## Base Element Class
/// 
/// Java Arrays store information about the **Class** its **elements** can be instances of,
/// so creating a Java Array requires passing in the **element class**.
/// It is impossible to have a consistent method of determining the **element class** from a *slice of objects* alone
/// (because of what was mentioned previously, but also because the array can be empty).
/// To resolve this, [`ObjectArray`] stores the Java Array's **element class**.
/// 
/// [`ObjectArray`] takes the class of the **base component** of the array,
/// that is the class of the Type that is not an array within the Array dimensions.
/// When the *Array Object* needs to be created,
/// the *element class* is applied to the *Array Objects* for *all array dimensions*.
/// 
/// ## Example
/// 
/// ```
/// # use ez_jni::ObjectArray;
/// # use jni::objects::JObject;
/// ObjectArray::new(
///     &[ &[ JObject::null() ] ],
///     "me/my/MyClass".into()
/// );
/// ```
#[derive(Debug)]
pub struct ObjectArray<T, Array = Box<[T]>>
where Array: AsRef<[T]>,
          T: ObjectArrayElement
{
    // TODO: Maybe implement it with an enum with states of an Array Object and a Rust array. When Rust needs to operate on the array it calls the necessary jni methods.
    // FIXME: Maybe also do Cow<[T]> and remove the Array generic?
    array: Array,
    base_elem_class: Cow<'static, str>,
    _t: PhantomData<T>,
}
impl<'a, T> ObjectArray<T, &'a [T]>
where T: ObjectArrayElement {
    /// Create a new [`ObjectArray`], but only borrowing existing values.
    /// 
    /// Does not create any new Rust values or Java *Object References*.
    /// 
    /// This is useful when you need to convert a Rust *slice of Object References* to a Java *Array Object*.
    #[inline(always)]
    pub fn new_ref(slice: &'a [T], base_elem_class: &'static str) -> Self {
        Self::new(slice, Cow::Borrowed(base_elem_class))
    }
}
impl<T, Array> ObjectArray<T, Array>
where Array: AsRef<[T]>,
          T: ObjectArrayElement
{
    #[inline(always)]
    pub fn new(array: Array, base_elem_class: Cow<'static, str>) -> Self {
        // TODO: check class of elements? Also accept dot and slash format??
        Self { array, base_elem_class, _t: PhantomData }
    }
    
    /// Helps with converting the `Array` type to a different *Rust type*.
    /// 
    /// E.g. convert `ObjectArray<Box<[_]>>` to `ObjectArray<Vec<_>>`.
    #[inline(always)]
    pub fn convert_array<OtherArray>(self, conversion: impl FnOnce(Array) -> OtherArray) -> ObjectArray<T, OtherArray>
    where OtherArray: AsRef<[T]> {
        ObjectArray::new(conversion(self.array), self.base_elem_class)
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
    
    /// Returns the **class** of the *base component* of the Object Array.
    /// 
    /// In other words, the base **class** of the [`JObject`]s that are stored in the bottom dimension of this type.
    #[inline(always)]
    pub fn base_elem_class(&self) -> &str {
        &self.base_elem_class
    }
}

impl<'local, T, Array> FromObject<'local> for ObjectArray<T, Array>
where T: ObjectArrayElement + 'local,
  Array: AsRef<[T]> + ArrayFromObject<'local>,
{
    fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        /* SAFETY: Implementation of from_object() for Array types does not return the original object that was passed in,
                   and only use the object as borrowed directly to query the array object.
                   And since JObjects aren't dropped, the reference is safe to pass as owned.
                   The lifetime is also cast to 'local, but the same points explain why. */
        let obj = unsafe { JObject::<'local>::from_raw(object.as_raw()) };
        let array = <Array as ArrayFromObject>::from_object(obj, env)?;

        // Get elem_class ***after*** constructing Array so FromObject2 can do the null and class check. This also avoids unnecessary jni calls.
        // FIXME: use Class object instead so that it can do is_instance_of()
        let array_class = get_object_class_name(object, env);
        let base_elem_class = array_class.trim_start_matches('[')
            .strip_prefix('L')
            .and_then(|class| class.strip_suffix(';'))
            .expect(&format!("Class<?[]>.getName() did not return a class string in signature format: \"{array_class}\""));

        let expected_array_class = gen_array_class(Array::get_dimensions_count(), base_elem_class);
        if array_class != expected_array_class {
            return Err(FromObjectError::ClassMismatch {
                target_class: Some(base_elem_class.to_string()),
                obj_class: array_class,
            });
        }

        Ok(Self::new(array, base_elem_class.to_string().into()))
    }
}
impl<T, Array> ToObject for ObjectArray<T, Array>
where T: ObjectArrayElement,
  Array: AsRef<[T]>,
{
    #[inline(always)]
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        let array_class = gen_array_class(T::get_dimensions_count() + 1, self.base_elem_class());
        <T as element::ToArrayObject>::to_array_object(self.array.as_ref(), &array_class, env)
    }
}

impl<T, Array> Class for ObjectArray<T, Array>
where T: ObjectArrayElement + Class,
  Array: AsRef<[T]>,
{
    #[inline(always)]
    fn class() -> Cow<'static, str> {
        <[T] as Class>::class()
    }
}

impl<T, Array> IntoIterator for ObjectArray<T, Array>
where T: ObjectArrayElement,
  Array: AsRef<[T]> + IntoIterator,
{
    type Item = <Array as IntoIterator>::Item;
    type IntoIter = <Array as IntoIterator>::IntoIter;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self.array.into_iter()
    }
}
impl<I, T, Array> Index<I> for ObjectArray<T, Array>
where I: SliceIndex<[T]>,
  T: ObjectArrayElement,
    Array: AsRef<[T]>,
{
    type Output = <I as SliceIndex<[T]>>::Output;

    #[inline(always)]
    fn index(&self, index: I) -> &Self::Output {
        self.as_ref().index(index)
    }
}
impl<I, T, Array> IndexMut<I> for ObjectArray<T, Array>
where I: SliceIndex<[T]>,
  T: ObjectArrayElement,
    Array: AsRef<[T]> + AsMut<[T]>,
{
    #[inline(always)]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        self.as_mut().index_mut(index)
    }
}
impl<T, Array> Hash for ObjectArray<T, Array>
where T: ObjectArrayElement + Hash,
  Array: AsRef<[T]>,
{
    #[inline(always)]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.base_elem_class().hash(state);
        self.array.as_ref().hash(state);
    }
}
impl<T, Array> PartialEq for ObjectArray<T, Array>
where T: ObjectArrayElement + PartialEq,
  Array: AsRef<[T]>,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.base_elem_class().eq(other.base_elem_class())
        && self.array.as_ref().eq(other.array.as_ref())
    }
}
impl<T, Array> PartialEq<&Self> for ObjectArray<T, Array>
where T: ObjectArrayElement + PartialEq,
  Array: AsRef<[T]>,
{
    #[inline(always)]
    fn eq(&self, other: &&Self) -> bool {
        <Self as PartialEq<Self>>::eq(self, other)
    }
}
impl<T, Array> Eq for ObjectArray<T, Array>
where T: ObjectArrayElement + Eq,
  Array: AsRef<[T]> { }
// Don't implement FromIterator, Ord, PartialOrd

// TODO: implement Drop (delete_local_ref), Clone (new_local_ref), Display (.toString()), Extend<T> (check class) if allowing implicit jni calls without manually passing env

impl<T, Array> AsRef<[T]> for ObjectArray<T, Array>
where T: ObjectArrayElement,
  Array: AsRef<[T]>,
{
    #[inline(always)]
    fn as_ref(&self) -> &[T] {
        self.array.as_ref()
    }
}
impl<T, Array> AsMut<[T]> for ObjectArray<T, Array>
where T: ObjectArrayElement,
  Array: AsRef<[T]> + AsMut<[T]>,
{
    #[inline(always)]
    fn as_mut(&mut self) -> &mut [T] {
        self.array.as_mut()
    }
}
impl<T, Array> Deref for ObjectArray<T, Array>
where T: ObjectArrayElement,
  Array: AsRef<[T]>,
{
    type Target = Array;
    
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.array
    }
}
impl<T, Array> DerefMut for ObjectArray<T, Array>
where T: ObjectArrayElement,
  Array: AsRef<[T]>,
{
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.array
    }
}
impl<T, Array> Borrow<[T]> for ObjectArray<T, Array>
where T: ObjectArrayElement,
  Array: AsRef<[T]>,
{
    #[inline(always)]
    fn borrow(&self) -> &[T] {
        self.array.as_ref()
    }
}
impl<T, Array> BorrowMut<[T]> for ObjectArray<T, Array>
where T: ObjectArrayElement,
  Array: AsRef<[T]> + AsMut<[T]>,
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

/// Generates the Array Class in *signature format* for an [`ObjectArray`].
fn gen_array_class(dimensions: usize, base_elem_class: &str) -> String {
    format!("{brackets}L{base_elem_class};", brackets = "[".repeat(dimensions))
}

#[cfg(test)]
mod tests {
    use super::*;
    use jni::objects::{GlobalRef, JClass, JString};

    /// Tests that all applicable types can be used as array elements.
    #[test]
    fn object_array_element_types() {
        let _: ObjectArray<JObject, &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<JThrowable, &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<JClass, &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<JString, &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<GlobalRef, &[_]> = ObjectArray::new_ref(&[], "/");
        // Option
        let _: ObjectArray<Option<JObject>, &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<Option<JThrowable>, &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<Option<JClass>, &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<Option<JString>, &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<Option<GlobalRef>, &[_]> = ObjectArray::new_ref(&[], "/");
        // 2-Dimensional Array
        let _: ObjectArray<&[JObject], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<&[JThrowable], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<&[JClass], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<&[JString], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<&[GlobalRef], &[_]> = ObjectArray::new_ref(&[], "/");
        // 2-Dimensional Option Array
        let _: ObjectArray<&[Option<JObject>], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<&[Option<JThrowable>], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<&[Option<JClass>], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<&[Option<JString>], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<&[Option<GlobalRef>], &[_]> = ObjectArray::new_ref(&[], "/");
        // 3-Dimensional Array
        let _: ObjectArray<&[&[JObject]], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<&[&[JThrowable]], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<&[&[JClass]], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<&[&[JString]], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<&[&[GlobalRef]], &[_]> = ObjectArray::new_ref(&[], "/");
        // 3-Dimensional Mixed
        let _: ObjectArray<&[Box<[JObject]>], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<&[Box<[JThrowable]>], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<&[Box<[JClass]>], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<&[Box<[JString]>], &[_]> = ObjectArray::new_ref(&[], "/");
        let _: ObjectArray<&[Box<[GlobalRef]>], &[_]> = ObjectArray::new_ref(&[], "/");
    }
}