use jni::{objects::{JObject, JString}, JNIEnv};
use jni_macros::call;
use thiserror::Error;
use crate::utils::get_string;

#[derive(Debug, Error)]
pub enum FromObjectError {
    #[error("Object can't be NULL")]
    Null,
    #[error("The Class of the Object is not the target Class, or a descendant of the target Class {target_class}")]
    ClassMismatch { obj_class: String, target_class: &'static str },
    #[error("Could not find field {name:?} of type {ty} in class {target_class}; maybe its private?")]
    FieldNotFound { name: String, ty: String, target_class: String },
    // #[error("{0}")]
    // Other(String)
}

// This is not object wrapper, but constructors
pub trait Class
where Self: Sized {
    /// The path of the target Class.
    /// Must be in *slash-separeted* form. e.g. "java/lang/String".
    const PATH: &'static str;
    
    /// Construct a [`Self`] by reading data from a *Java Object*.
    fn from_object(object: JObject, env: &mut JNIEnv) -> Result<Self, FromObjectError>;
    /// Create an instance of [`Self::PATH`] by reading data from a *Rust struct*.
    /// Will `panic!` if the JNI call fails.
    fn to_object<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local>;
}

impl <T> Class for Option<T>
where T: Class {
    const PATH: &'static str = T::PATH;

    fn from_object(object: JObject, env: &mut JNIEnv) -> Result<Self, FromObjectError> {
        if object.is_null() {
            Ok(None)
        } else {
            T::from_object(object, env)
                .map(|t| Some(t))
        }
    }
    fn to_object<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        match self {
            Some(t) => t.to_object(env),
            None => JObject::null()
        }
    }
}

impl Class for String {
    const PATH: &'static str = "java/lang/String";
    
    /// Get a [`String`] from some random Object.
    /// 
    /// Don't use this function, it only exist for compatibility.
    /// Use [`utils::get_string()`] instead because you will mostly be using it with [`JString`].
    fn from_object(object: JObject, env: &mut JNIEnv) -> Result<Self, FromObjectError> {
        if object.is_null() {
            return Err(FromObjectError::Null)
        }
        
        let obj_class = env.get_object_class(&object)
            .unwrap_or_else(|err| panic!("Failed to get Object's class: {err}"));
        
        if !env.is_assignable_from(Self::PATH, &obj_class).unwrap() {
            return Err(FromObjectError::ClassMismatch {
                obj_class: get_string(call!(obj_class.getName() -> java.lang.String), env),
                target_class: Self::PATH
            })
        }
        
        // Already checked that it is java.lang.String and is not NULL
        Ok(unsafe {
            env.get_string_unchecked(&JString::from(object))
                .unwrap_or_else(|err| panic!("ENV error while getting String: {err}"))
                .into()
        })
    }
    fn to_object<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        env.new_string(self)
            .unwrap_or_else(|err| panic!("Error converting Rust string to Java String: {err}"))
            .into()
    }
}
