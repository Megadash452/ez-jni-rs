use jni::objects::JClass;
use crate::utils::{create_java_prim_array, create_object_array, get_java_prim_array, get_object_array};
use super::*;

fn create_object_array_from_t<'local, T>(slice: &[T], elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local>
where T: ToObject {
    let obj_slice = slice.iter()
        .map(|t| t.to_object_env(env))
        .collect::<Box<_>>();

    create_object_array(
        &obj_slice.iter()
            .collect::<Box<_>>(),
        elem_class,
    env)
}

// -- Arrays --

// TODO: Ignore 'a and 'obj because the resulting object will always be JObject<'local>
impl<'a, 'obj, 'local, T> FromObject<'a, 'obj, 'local> for Vec<T>
where Box<[T]>: FromObject<'a, 'obj, 'local> {
    fn from_object_env(object: &'a JObject<'obj>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        Ok(Box::<[T]>::from_object_env(object, env)?.into_vec())
    }
}


impl<'local> FromObject<'_, '_, 'local> for Box<[JObject<'local>]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array(object, None, env)
    }
}

/// Implementation for a slice of Objects of an ambiguous Class paired with the element Class
macro_rules! impl_paired_obj_slice {
    (ref $($impl_decl:tt)*) => {
        $($impl_decl)* {
            fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
                create_object_array(
                    &self.1.iter()
                        .collect::<Box<_>>(),
                    self.0,
                env)
            }
        }
    };
    ($($impl_decl:tt)*) => {
        $($impl_decl)* {
            fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
                create_object_array(&self.1, self.0, env)
            }
        }
    };
}
impl_paired_obj_slice!(    impl ToObject for (&str, [JObject<'_>]));
impl_paired_obj_slice!(ref impl ToObject for (&str, [&JObject<'_>]));
impl_paired_obj_slice!(    impl ToObject for (&str, &[JObject<'_>]));
impl_paired_obj_slice!(ref impl ToObject for (&str, &[&JObject<'_>]));
impl_paired_obj_slice!(    impl<const N: usize> ToObject for (&str, [JObject<'_>; N]));
impl_paired_obj_slice!(ref impl<const N: usize> ToObject for (&str, [&JObject<'_>; N]));

impl<'local> FromObject<'_, '_, 'local> for Box<[JClass<'local>]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        crate::utils::get_object_array_converted(
            object,
            Some("[Ljava/lang/Class;"),
            |obj, _| JClass::from(obj),
        env)
    }
}
impl ToObject for [JClass<'_>] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        crate::utils::create_object_array_converted(
            self,
            "java/lang/Class",
            // Can allocate without worry because the function creates a new local frame
            |class, env| <&JObject>::from(class).to_object_env(env),
        env)
    }
}

impl<'local> FromObject<'_, '_, 'local> for Box<[JThrowable<'local>]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        crate::utils::get_object_array_converted(
            object,
            Some("[Ljava/lang/Exception;"),
            |obj, _| JThrowable::from(obj),
        env)
    }
}
impl ToObject for [JThrowable<'_>] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        crate::utils::create_object_array_converted(
            self,
            "java/lang/Exception",
            // Can allocate without worry because the function creates a new local frame
            |class, env| <&JObject>::from(class).to_object_env(env),
            env
        )
    }
}

// Implementations for String Array

impl FromObject<'_, '_, '_> for Box<[String]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        get_object_array(object, Some("[Ljava/lang/String;"), env)
            .and_then(|array|
                array.into_iter()
                    .map(|obj| String::from_object_env(&obj, env))
                    .collect::<Result<Box<[_]>, _>>()
            )
    }
}
impl ToObject for [String] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_from_t(self, "java/lang/String", env)
    }
}
impl ToObject for [&str] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_from_t(self, "java/lang/String", env)
    }
}

// Implementation for [Option<T>]

impl<'local> FromObject<'_, '_, 'local> for Box<[Option<JObject<'local>>]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array(object, None, env)
            .map(|array|
                array.into_iter()
                    .map(|obj| if obj.is_null() { None } else { Some(obj) })
                    .collect()
            )
    }
}
impl ToObject for (&str, &[Option<JObject<'_>>]) {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array(
            &self.1.iter()
                .map(|opt| opt.as_ref()
                    // Copy the Object pointer. Reference should be valid throuhgout the whole function call
                    .map(|o| unsafe { JObject::from_raw(o.as_raw()) })
                        .unwrap_or(JObject::null())
                )
                .collect::<Box<[_]>>(),
            self.0,
        env)
    }
}
impl ToObject for (&str, &[Option<&JObject<'_>>]) {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array(
            &self.1.iter()
                .map(|opt| opt.as_ref()
                    // Copy the Object pointer. Reference should be valid throuhgout the whole function call
                    .map(|&o| unsafe { JObject::from_raw(o.as_raw()) })
                        .unwrap_or(JObject::null())
                )
                .collect::<Box<[_]>>(),
            self.0,
        env)
    }
}

impl FromObject<'_, '_, '_> for Box<[Option<String>]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        get_object_array(object, Some("[Ljava/lang/String;"), env)
            .and_then(|array|
                array.into_iter()
                    .map(|obj| Option::<String>::from_object_env(&obj, env))
                    .collect::<Result<Box<[_]>, _>>()
            )
    }
}
impl ToObject for [Option<String>] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_from_t(self, "java/lang/String", env)
    }
}
impl ToObject for [Option<&str>] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_from_t(self, "java/lang/String", env)
    }
}

// Implementations for Number Arrays

impl FromObject<'_, '_, '_> for Box<[i8]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(get_java_prim_array(object, JNIEnv::get_byte_array_region, env))
    }
}
impl ToObject for [i8] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_java_prim_array(self,
            JNIEnv::new_byte_array,
            JNIEnv::set_byte_array_region,
        env)
    }
}

impl FromObject<'_, '_, '_> for Box<[i16]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(get_java_prim_array(object, JNIEnv::get_short_array_region, env))
    }
}
impl ToObject for [i16] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_java_prim_array(self,
            JNIEnv::new_short_array,
            JNIEnv::set_short_array_region,
        env)
    }
}

impl FromObject<'_, '_, '_> for Box<[i32]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(get_java_prim_array(object, JNIEnv::get_int_array_region, env))
    }
}
impl ToObject for [i32] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_java_prim_array(self,
            JNIEnv::new_int_array,
            JNIEnv::set_int_array_region,
        env)
    }
}

impl FromObject<'_, '_, '_> for Box<[i64]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(get_java_prim_array(object, JNIEnv::get_long_array_region, env))
    }
}
impl ToObject for [i64] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_java_prim_array(self,
            JNIEnv::new_long_array,
            JNIEnv::set_long_array_region,
        env)
    }
}

impl FromObject<'_, '_, '_> for Box<[f32]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(get_java_prim_array(object, JNIEnv::get_float_array_region, env))
    }
}
impl ToObject for [f32] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_java_prim_array(self,
            JNIEnv::new_float_array,
            JNIEnv::set_float_array_region,
        env)
    }
}

impl FromObject<'_, '_, '_> for Box<[f64]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(get_java_prim_array(object, JNIEnv::get_double_array_region, env))
    }
}
impl ToObject for [f64] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_java_prim_array(self,
            JNIEnv::new_double_array,
            JNIEnv::set_double_array_region,
        env)
    }
}

// Implementation for Unsigned Number types

impl FromObject<'_, '_, '_> for Box<[u8]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(get_java_prim_array(object, JNIEnv::get_byte_array_region, env)
            .into_iter()
            .map(|t| unsafe { std::mem::transmute(t) })
            .collect()
        )
    }
}
impl ToObject for [u8] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_java_prim_array(
            self.iter()
                .map(|&t| unsafe { std::mem::transmute(t) })
                .collect::<Box<[_]>>()
                .as_ref(),
            JNIEnv::new_byte_array,
            JNIEnv::set_byte_array_region,
        env)
    }
}

impl FromObject<'_, '_, '_> for Box<[u16]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(get_java_prim_array(object, JNIEnv::get_short_array_region, env)
            .into_iter()
            .map(|t| unsafe { std::mem::transmute(t) })
            .collect()
        )
    }
}
impl ToObject for [u16] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_java_prim_array(
            self.iter()
                .map(|&t| unsafe { std::mem::transmute(t) })
                .collect::<Box<[_]>>()
                .as_ref(),
            JNIEnv::new_short_array,
            JNIEnv::set_short_array_region,
        env)
    }
}

impl FromObject<'_, '_, '_> for Box<[u32]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(get_java_prim_array(object, JNIEnv::get_int_array_region, env)
            .into_iter()
            .map(|t| unsafe { std::mem::transmute(t) })
            .collect()
        )
    }
}
impl ToObject for [u32] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_java_prim_array(
            self.iter()
                .map(|&t| unsafe { std::mem::transmute(t) })
                .collect::<Box<[_]>>()
                .as_ref(),
            JNIEnv::new_int_array,
            JNIEnv::set_int_array_region,
        env)
    }
}

impl FromObject<'_, '_, '_> for Box<[u64]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(get_java_prim_array(object, JNIEnv::get_long_array_region, env)
            .into_iter()
            .map(|t| unsafe { std::mem::transmute(t) })
            .collect()
        )
    }
}
impl ToObject for [u64] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_java_prim_array(
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

impl FromObject<'_, '_, '_> for Box<[bool]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(get_java_prim_array(object, JNIEnv::get_boolean_array_region, env)
            .into_iter()
            .map(|t| unsafe { std::mem::transmute(t) })
            .collect()
        )
    }
}
impl ToObject for [bool] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_java_prim_array(
            self.iter()
                .map(|&t| unsafe { std::mem::transmute(t) })
                .collect::<Box<[_]>>()
                .as_ref(),
            JNIEnv::new_boolean_array,
            JNIEnv::set_boolean_array_region,
        env)
    }
}

impl FromObject<'_, '_, '_> for Box<[char]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        Ok(get_java_prim_array(object, JNIEnv::get_char_array_region, env)
            .into_iter()
            .map(|t| char::decode_utf16(Some(t))
                .next().unwrap()
                .unwrap_or(char::REPLACEMENT_CHARACTER)
            )
            .collect()
        )
    }
}
impl ToObject for [char] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_java_prim_array(
            self.iter()
                .map(|&t| t.encode_utf16(&mut [0;1])[0])
                .collect::<Box<[_]>>()
                .as_ref(),
            JNIEnv::new_char_array,
            JNIEnv::set_char_array_region,
        env)
    }
}
