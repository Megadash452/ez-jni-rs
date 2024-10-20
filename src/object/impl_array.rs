use jni::{objects::JObjectArray, sys::jsize};
use crate::utils::java_path_to_dot_notation;

use super::*;

/// The same as [`get_object_array`], but also converts the [`JObject`]s in the array to the desired type `T`.
fn get_t_array_from_object<'local, T>(obj: &JObject, array_class: &'static str, env: &mut JNIEnv<'local>) -> Result<Vec<T>, FromObjectError>
where T: FromObject<'local> {
    get_object_array(obj, Some(array_class), env).and_then(|array|
        Result::from_iter(
            array.into_iter()
                .map(|obj| T::from_object(&obj, env))
                .collect::<Vec<_>>()
        )
    )
}
/// Creates a Rust [`Vec`] of [`JObject`]s by reading elements from a **Java Array** (**obj**).
/// 
/// Also checks that the Array Object's class matches the **array_class** (if it is [`Some`]).
fn get_object_array<'local>(obj: &JObject, array_class: Option<&'static str>, env: &mut JNIEnv<'local>) -> Result<Vec<JObject<'local>>, FromObjectError> {
    let array = <&JObjectArray>::from(obj);
    // Check object's type
    if let Some(class) = array_class {
        let class = java_path_to_dot_notation(class);
        let obj_class = env.get_object_class(obj)
            .unwrap_or_else(|err| panic!("Failed to get Object's class: {err}"));
        let obj_class = call!(obj_class.getName() -> String);
        if obj_class != class {
            return Err(FromObjectError::ClassMismatch { obj_class, target_class: Some(class) });
        }
    }

    let len = env.get_array_length(array)
        .unwrap_or_else(|err| panic!("Failed to check Array's length: {err}"))
        as usize;
    // Allocate array
    let mut vec = Vec::with_capacity(len);

    // Fill array
    for i in 0..len {
        vec.push(env.get_object_array_element(array, i as jsize)
            .unwrap_or_else(|err| panic!("Failed to read Array elements: {err}"))
        );
    }

    Ok(vec)
}

fn create_object_array_from_t<'local, T>(slice: &[T], elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local>
where T: ToObject<'local> {
    let obj_slice = slice.iter()
        .map(|t| t.to_object(env))
        .collect::<Box<_>>();

    create_object_array(
        &obj_slice.iter()
            .collect::<Box<_>>(),
        elem_class,
    env)
}
fn create_object_array<'local>(slice: &[&JObject], elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local> {
    // Allocate the array
    let array = env.new_object_array(
        slice.len() as jsize,
        elem_class,
        JObject::null()
    )
        .unwrap_or_else(|err| panic!("Failed to create Java Object \"{elem_class}\" array: {err}"));

    // Fill the array
    for (i, element) in slice.iter().enumerate() {
        env.set_object_array_element(&array, i as jsize, element)
            .unwrap_or_else(|err| panic!("Failed to set the value of Object array at index {i}: {err}"));
    }

    array.into()
}

// -- Arrays --

impl<'local, T> FromObject<'local> for Box<[T]>
where Vec<T>: FromObject<'local> {
    fn from_object(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        Ok(Vec::<T>::from_object(object, env)?.into_boxed_slice())
    }
}


impl<'local> FromObject<'local> for Vec<JObject<'local>> {
    fn from_object(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array(object, None, env)
    }
}
/// Implementation for an Object slice paired with the element Class
impl<'local> ToObject<'local> for (&str, [JObject<'_>]) {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array(
            &self.1.iter()
                .collect::<Box<_>>(),
            self.0,
        env)
    }
}
impl<'local> ToObject<'local> for (&str, [&JObject<'_>]) {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array(&self.1, self.0, env)
    }
}

// Implementations for String Array

impl FromObject<'_> for Vec<String> {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        get_t_array_from_object(object, "[Ljava/lang/String;", env)
    }
}
impl<'local> ToObject<'local> for [String] {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_from_t(self, "java/lang/String", env)
    }
}
impl<'local> ToObject<'local> for [&str] {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_from_t(self, "java/lang/String", env)
    }
}

// Implementation for [Option<T>]

// impl FromObject<'_> for Vec<Option<JObject<'_>>> {
//     fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
//         get_object_array(object, todo!(), env)
//             .map(|array|
//                 array.into_iter()
//                     .map(|obj| if obj.is_null() { None } else { Some(obj) })
//                     .collect()
//             )
//     }
// }
// impl<'local> ToObject<'local> for [Option<JObject<'_>>] {
//     fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
//         self.iter()
//             .map(|opt| opt.as_ref())
//             .collect::<Vec<_>>()
//             .to_object(env)
//     }
// }
// impl<'local> ToObject<'local> for [Option<&JObject<'_>>] {
//     fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
//         create_object_array(
//             &self.iter()
//                 .map(|opt| opt.as_ref()
//                     // Copy the Object pointer. Reference should be valid throuhgout the whole function call
//                     .map(|&o| unsafe { JObject::from_raw(o.as_raw()) })
//                         .unwrap_or(JObject::null())
//                 )
//                 .collect::<Box<[_]>>(),
//             todo!(),
//         env)
//     }
// }

impl FromObject<'_> for Vec<Option<String>> {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        get_t_array_from_object(object, "[Ljava/lang/String;", env)
    }
}
impl<'local> ToObject<'local> for [Option<String>] {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_from_t(self, "java/lang/String", env)
    }
}
impl<'local> ToObject<'local> for [Option<&str>] {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_from_t(self, "java/lang/String", env)
    }
}

// Implementations for Number Arrays

impl FromObject<'_> for Vec<i8> {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(crate::utils::get_java_prim_array(object, JNIEnv::get_byte_array_region, env))
    }
}
impl<'local> ToObject<'local> for [i8] {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        crate::utils::crate_java_prim_array(self,
            JNIEnv::new_byte_array,
            JNIEnv::set_byte_array_region,
        env)
    }
}

impl FromObject<'_> for Vec<i16> {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(crate::utils::get_java_prim_array(object, JNIEnv::get_short_array_region, env))
    }
}
impl<'local> ToObject<'local> for [i16] {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        crate::utils::crate_java_prim_array(self,
            JNIEnv::new_short_array,
            JNIEnv::set_short_array_region,
        env)
    }
}

impl FromObject<'_> for Vec<i32> {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(crate::utils::get_java_prim_array(object, JNIEnv::get_int_array_region, env))
    }
}
impl<'local> ToObject<'local> for [i32] {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        crate::utils::crate_java_prim_array(self,
            JNIEnv::new_int_array,
            JNIEnv::set_int_array_region,
        env)
    }
}

impl FromObject<'_> for Vec<i64> {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(crate::utils::get_java_prim_array(object, JNIEnv::get_long_array_region, env))
    }
}
impl<'local> ToObject<'local> for [i64] {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        crate::utils::crate_java_prim_array(self,
            JNIEnv::new_long_array,
            JNIEnv::set_long_array_region,
        env)
    }
}

impl FromObject<'_> for Vec<f32> {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(crate::utils::get_java_prim_array(object, JNIEnv::get_float_array_region, env))
    }
}
impl<'local> ToObject<'local> for [f32] {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        crate::utils::crate_java_prim_array(self,
            JNIEnv::new_float_array,
            JNIEnv::set_float_array_region,
        env)
    }
}

impl FromObject<'_> for Vec<f64> {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(crate::utils::get_java_prim_array(object, JNIEnv::get_double_array_region, env))
    }
}
impl<'local> ToObject<'local> for [f64] {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        crate::utils::crate_java_prim_array(self,
            JNIEnv::new_double_array,
            JNIEnv::set_double_array_region,
        env)
    }
}

// Implementation for Unsigned Number types

impl FromObject<'_> for Vec<u8> {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(crate::utils::get_java_prim_array(object, JNIEnv::get_byte_array_region, env)
            .into_iter()
            .map(|t| unsafe { std::mem::transmute(t) })
            .collect()
        )
    }
}
impl<'local> ToObject<'local> for [u8] {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        crate::utils::crate_java_prim_array(
            self.iter()
                .map(|&t| unsafe { std::mem::transmute(t) })
                .collect::<Box<[_]>>()
                .as_ref(),
            JNIEnv::new_byte_array,
            JNIEnv::set_byte_array_region,
        env)
    }
}

impl FromObject<'_> for Vec<u16> {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(crate::utils::get_java_prim_array(object, JNIEnv::get_short_array_region, env)
            .into_iter()
            .map(|t| unsafe { std::mem::transmute(t) })
            .collect()
        )
    }
}
impl<'local> ToObject<'local> for [u16] {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        crate::utils::crate_java_prim_array(
            self.iter()
                .map(|&t| unsafe { std::mem::transmute(t) })
                .collect::<Box<[_]>>()
                .as_ref(),
            JNIEnv::new_short_array,
            JNIEnv::set_short_array_region,
        env)
    }
}

impl FromObject<'_> for Vec<u32> {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(crate::utils::get_java_prim_array(object, JNIEnv::get_int_array_region, env)
            .into_iter()
            .map(|t| unsafe { std::mem::transmute(t) })
            .collect()
        )
    }
}
impl<'local> ToObject<'local> for [u32] {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        crate::utils::crate_java_prim_array(
            self.iter()
                .map(|&t| unsafe { std::mem::transmute(t) })
                .collect::<Box<[_]>>()
                .as_ref(),
            JNIEnv::new_int_array,
            JNIEnv::set_int_array_region,
        env)
    }
}

impl FromObject<'_> for Vec<u64> {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(crate::utils::get_java_prim_array(object, JNIEnv::get_long_array_region, env)
            .into_iter()
            .map(|t| unsafe { std::mem::transmute(t) })
            .collect()
        )
    }
}
impl<'local> ToObject<'local> for [u64] {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        crate::utils::crate_java_prim_array(
            self.iter()
                .map(|&t| unsafe { std::mem::transmute(t) })
                .collect::<Box<[_]>>()
                .as_ref(),
            JNIEnv::new_long_array,
            JNIEnv::set_long_array_region,
        env)
    }
}

// Implementations for other primitive Arrays

impl FromObject<'_> for Vec<bool> {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(crate::utils::get_java_prim_array(object, JNIEnv::get_boolean_array_region, env)
            .into_iter()
            .map(|t| unsafe { std::mem::transmute(t) })
            .collect()
        )
    }
}
impl<'local> ToObject<'local> for [bool] {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        crate::utils::crate_java_prim_array(
            self.iter()
                .map(|&t| unsafe { std::mem::transmute(t) })
                .collect::<Box<[_]>>()
                .as_ref(),
            JNIEnv::new_boolean_array,
            JNIEnv::set_boolean_array_region,
        env)
    }
}

impl FromObject<'_> for Vec<char> {
    fn from_object(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(crate::utils::get_java_prim_array(object, JNIEnv::get_char_array_region, env)
            .into_iter()
            .map(|t| char::decode_utf16(Some(t))
                .next().unwrap()
                .unwrap_or(char::REPLACEMENT_CHARACTER)
            )
            .collect()
        )
    }
}
impl<'local> ToObject<'local> for [char] {
    fn to_object(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        crate::utils::crate_java_prim_array(
            self.iter()
                .map(|&t| t.encode_utf16(&mut [0;1])[0])
                .collect::<Box<[_]>>()
                .as_ref(),
            JNIEnv::new_char_array,
            JNIEnv::set_char_array_region,
        env)
    }
}
