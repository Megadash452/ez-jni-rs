mod r#impl;
mod impl_array;
mod impl_exception;

use std::cmp::Ordering;
use jni::{objects::{JObject, JThrowable}, JNIEnv};
use thiserror::Error;
use ez_jni_macros::call;
use crate::{utils::get_env, Class};

#[doc(hidden)]
pub use r#impl::FromObjectOwned;
pub use impl_exception::JavaException;
pub use impl_array::{ObjectArray, FromArrayObject, ToArrayObject};


#[derive(Debug, Error)]
pub enum FromObjectError {
    #[error("Object can't be NULL")]
    Null,
    #[error("{}", match target_class {
        Some(target_class) => format!("Expected the Object to be the Class or a descendant of the Class \"{target_class}\", but the actual Class of the Object is \"{obj_class}\""),
        None => format!("The Class Object did not match any of the classes of the variants; Found Class \"{obj_class}\"")
    })]
    ClassMismatch { obj_class: String, target_class: Option<String> },
    #[error("Could not find field {name:?} of type {ty} in class {target_class}; maybe its private?")]
    FieldNotFound { name: String, ty: String, target_class: String },
    #[error("Could not find class \"{0}\"")]
    ClassNotFound(String),
    #[error("Could not instantiate Type from element in Java Object Array at index {index}:\n    {error}")]
    ArrayElement { index: usize, error: Box<Self> },
    #[error("{}", match actual_len.cmp(expected_len) {
        Ordering::Equal => panic!("UNREACHABLE"),
        Ordering::Less => format!("Java Array is too long. Can't put {actual_len} elements, in an array of length {expected_len}"),
        Ordering::Greater => format!("Expected Java Array to contain {expected_len} elements, but it has {actual_len} elements")
    })]
    ArrayTooLong { expected_len: usize, actual_len: usize },
    #[error("Error converting Java Object: {0}")]
    Other(String)
}

/// Allows converting a *Java Object* to a Rust type by reading the Object's data.
/// 
/// ## Implementation
/// 
/// This trait contains 2 methods that essentially do the same thing.
/// [`from_object`][FromObject::from_object] exists for ease of use when a [`JNIEnv`] is not in scope and gets it from a global stack.
/// This method should **not** be re-implemented.
/// [`from_object_env`][FromObject::from_object_env] is the method that has the actual implementation and can take any [`JNIEnv`].
/// All calls in the code of [`from_object_env`][FromObject::from_object_env] should only use the `_env` counterparts so that the same [`JNIEnv`] is guaranteed to be used throughout the call.
/// 
/// ## Derive
/// 
/// This trait has a **derive macro** available from [`ez_jni_macros`].
/// Use it on *structs* to indicate that only 1 specific Class is expected.
/// Use it on *enums* to expect different Classes (one for each variant).
/// 
/// **Attributes**:
/// - **`class`**: Specifies the **Class Path** that the *struct or enum variant* expects (*optional* for the enum item).
/// - **`field`**: Specify certain properties to constrol how a *struct's field* is assigned by accessing a *Object's member* (field or getter).
///   By default, the struct's field name and type is used to produce the JNI call, but this can be changed.
///   Note: **name** and **call** are mutually exclusive, and either one MUST be used if the field belongs to a *Tuple struct*.
///   - **`name`**: Use a different name for the Object's field lookup instead of the field's name.
///     Mutually exclusive with `call`.
///   - **`call`**: Instead of accessing a field, Call a *getter method* with this name.
///   - **`class`**: Change the **field's** *java type signature* to use a custom **class** in the jni call.
///                  The class can also be wrapped in *square brackets* if the *Rust type* is wrapped in a `Boxed Slice` or [`Vec`],
///                  with support for multiple dimensions.
///                  Note: [`JObject`] and [`JThrowable`] require the user to provide a specic class using this attribute.
/// 
/// ```
/// # use ez_jni::FromObject;
/// 
/// #[derive(FromObject)]
/// #[class(me.author.MyClass)]
/// struct MyClass {
///     // Implicitly calls gets field or calls getMessage()
///     message: String
/// }
/// 
/// #[derive(FromObject)]
/// #[class(me.author.MyClass)] // Optional
/// enum MyClasses<'local> {
///     #[class(me.author.MyClassDescendant)]
///     Descendant { message: String },
///     #[class(me.author.MyFinalClass)]
///     Final(#[field(call = getMessage)] String),
///     #[class(me.author.MyOtherClass)]
///     Others {
///         #[field(class = [[me.author.MyClass]])]
///         instances: Box<[Box<[JObject<'local>]>]>
///     },
/// }
/// ```
pub trait FromObject<'a, 'obj, 'local>
where Self: Sized {
    /// Construct a [`Self`] by reading data from a *Java Object*.
    /// 
    /// Will [`panic!`] if any of the underlying JNI calls fail.
    /// 
    /// Automatically captures the [`JNIEnv`] from the local stack.
    /// To pass in your own [`JNIEnv`], see [`FromObject::from_object_env`].
    fn from_object(object: &'a JObject<'obj>) -> Result<Self, FromObjectError> {
        Self::from_object_env(object, get_env::<'_, 'local>())
    }
    /// Same as [`from_object`][FromObject::from_object], but does not capture the [`JNIEnv`] automatically; the caller must provide it themselves.
    /// 
    /// The implementation should also only methods that allow passing a [`JNIEnv`] so that the same [`JNIEnv`] is guaranteed to be used all throughout the call.
    /// For example, use [`from_object_env`][FromObject::from_object_env] instead of [`from_object`][FromObject::from_object].
    /// For jni macros, the env can be specified with this syntax: `macro!(env=> ...)`.
    /// 
    /// Only implement *this* method for the trait.
    fn from_object_env(object: &'a JObject<'obj>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError>;
}

/// Allows converting a *Rust type* to a *Java Object*.
/// 
/// ## Implementation
/// 
/// Just like [`FromObject`][FromObject#implementation], this trait has 2 methods that do the same thing:
/// [`to_object`][ToObject::to_object] automatically gets the [`JNIEnv`],
/// and [`to_object_env`][ToObject::to_object_env] is the method that has the implementation.
pub trait ToObject {
    /// Create an instance of a Class by constructing an object from data in a *Rust struct*.
    /// 
    /// Will [`panic!`] if any of the underlying JNI calls fail.
    /// 
    /// Automatically captures the [`JNIEnv`] from the local stack.
    /// To pass in your own [`JNIEnv`], see [`ToObject::to_object_env`].
    fn to_object<'local>(&self) -> JObject<'local> {
        self.to_object_env(get_env::<'_, 'local>())
    }
    /// Same as [`to_object`][ToObject::to_object], but does not capture the [`JNIEnv`] automatically; the caller must provide it themselves.
    /// 
    /// The implementation should also only methods that allow passing a [`JNIEnv`] so that the same [`JNIEnv`] is guaranteed to be used all throughout the call.
    /// For example, use [`to_object_env`][ToObject::to_object_env] instead of [`to_object`][ToObject::to_object].
    /// For jni macros, the env can be specified with this syntax: `macro!(env=> ...)`.
    /// 
    /// Only implement *this* method for the trait.
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local>;
}
