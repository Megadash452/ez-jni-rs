mod r#impl;
mod impl_array;
mod impl_exception;

use jni::{JNIEnv, objects::{JObject, JThrowable}};
use thiserror::Error;
use ez_jni_macros::call;

use crate::utils::get_env;


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
    // #[error("{0}")]
    // Other(String)
}

// The From/ToObject traits are split in 2 so that the from/to_object (which does not take a JNIEnv) method can be locked and only one method has ot be implemented.

/// Allows converting a *Java Object* to a *Rust type* by reading the Object's data.
/// 
/// ## Implementation
/// 
/// This trait cannot be implemented directly.
/// Types must instead implement [`FromObjectImpl`], which allows a caller to pass in their own [`JNIEnv`].
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
///   - **`class`**: If the struct field's type is [`JObject`] require that it be of this class.
///     This property is required for *non-primitives*.
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
/// enum MyClasses {
///     #[class(me.author.MyClassDescendant)]
///     Descendant { message: String },
///     #[class(me.author.MyOtherClass)]
///     Other(#[field(name = message)] String),
///     #[class(me.author.MyFinalClass)]
///     Final(#[field(call = getMessage)] String),
/// }
/// ```
pub trait FromObject<'local>: FromObjectImpl<'local>
where Self: Sized {
    /// Construct a [`Self`] by reading data from a *Java Object*.
    /// 
    /// Will [`panic!`] if any of the underlying JNI calls fail.
    /// 
    /// Automatically captures the [`JNIEnv`] from the local stack.
    /// To pass in your own [`JNIEnv`], see [`FromObjectImpl::from_object_env`].
    fn from_object(object: &JObject) -> Result<Self, FromObjectError> {
        Self::from_object_env(object, get_env::<'_, 'local>())
    }
    
}
/// This is the implementation trait for [`FromObject`].
/// Any type that wants to implement [`FromObject`] must implement this trait instead so that callers have the option to pass their own [`JNIEnv`].
/// 
/// The implementation should also only use methods that allow passing a [`JNIEnv`] so that the same [`JNIEnv`] can be used all throughout the call.
/// For example, use [`FromObjectImpl::from_object_env`] instead of [`FromObject::from_object`].
/// For jni macros, the env can be specified with this syntax: `macro!(env=> ...)`.
pub trait FromObjectImpl<'local>
where Self: Sized {
    /// Same as [`from_object`][FromObject::from_object], but does not capture the [`JNIEnv`] automatically; the caller must provide it themselves.
    /// 
    /// See [`FromObjectImpl`] for implementation details.
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError>;
}
impl<'local, T: FromObjectImpl<'local>> FromObject<'local> for T { }

/// Allows converting a *Rust type* to a *Java Object*.
/// 
/// ## Implementation
/// 
/// This trait cannot be implemented directly.
/// Types must instead implement [`ToObjectImpl`], which allows a caller to pass in their own [`JNIEnv`].
pub trait ToObject: ToObjectImpl {
    /// Create an instance of a Class by constructing an object from data in a *Rust struct*.
    /// 
    /// Will [`panic!`] if any of the underlying JNI calls fail.
    /// 
    /// Automatically captures the [`JNIEnv`] from the local stack.
    /// To pass in your own [`JNIEnv`], see [`ToObjectImpl::to_object_env`].
    fn to_object<'local>(&self) -> JObject<'local> {
        self.to_object_env(get_env::<'_, 'local>())
    }
}
/// This is the implementation trait for [`ToObject`].
/// Any type that wants to implement [`ToObject`] must implement this trait instead so that callers have the option to pass their own [`JNIEnv`].
/// 
/// The implementation should also only use methods that allow passing a [`JNIEnv`] so that the same [`JNIEnv`] can be used all throughout the call.
/// For example, use [`ToObjectImpl::to_object_env`] instead of [`ToObject::to_object`].
/// For jni macros, the env can be specified with this syntax: `macro!(env=> ...)`.
pub trait ToObjectImpl {
    /// Same as [`to_object`][ToObject::to_object], but does not capture the [`JNIEnv`] automatically; the caller must provide it themselves.
    /// 
    /// See [`ToObjectImpl`] for implementation details.
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local>;
}
impl<T: ToObjectImpl> ToObject for T { }


// TODO: get rid of this trait, use Java classes for error types instead
/// Allows converting *Java Exceptions* to Rust types that allow for better error handling.
/// 
/// # Derive
/// Uses the same derive syntax as [`FromObject`].
/// 
/// ```
/// # use ez_jni::FromException;
/// 
/// #[derive(FromException)]
/// #[class(java.lang.Exception)]
/// struct MyStError {
///     // Implicitly calls gets field or calls getMessage()
///     message: String
/// }
/// 
/// #[derive(FromException)]
/// #[class(java.lang.Exception)] // Optional
/// enum MyEnmError {
///     #[class(java.lang.NullPointerException)]
///     Null,
///     #[class(me.author.ElementExists)]
///     AlreadyExists(#[field(name = message)] String),
///     #[class(java.lang.Exception)]
///     Other(#[field(call = getMessage)] String),
/// }
/// ```
pub trait FromException<'local>
where Self: Sized {
    fn from_exception(exception: &JThrowable, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError>;
}

/// Does the required checks to ensure that a Java Object is valid.
fn object_check_boilerplate(object: &JObject, path: &'static str, env: &mut JNIEnv) -> Result<(), FromObjectError> {
    if object.is_null() {
        return Err(FromObjectError::Null)
    }
    
    let obj_class = env.get_object_class(object)
        .unwrap_or_else(|err| panic!("Failed to get Object's class: {err}"));
    
    if !env.is_instance_of(object, path).unwrap() {
        return Err(FromObjectError::ClassMismatch {
            obj_class: call!(env=> obj_class.getName() -> String),
            target_class: Some(path.to_string())
        })
    }

    Ok(())
}
