use jni::objects::JClass;
use crate::{utils::{create_java_prim_array, create_object_array, create_object_array_converted, get_java_prim_array, get_object_array}, Class};
use super::*;

fn create_object_array_from_t<'local, T>(slice: &[T], elem_class: &str, env: &mut JNIEnv<'local>) -> JObject<'local>
where T: ToObject {
    let obj_slice = slice.iter()
        .map(|t| t.to_object_env(env))
        .collect::<Box<_>>();

    create_object_array(
        &obj_slice,
        elem_class,
    env)
}

// -- Arrays --

impl<'a, 'obj, 'local, T> FromObject<'a, 'obj, 'local> for Vec<T>
where Box<[T]>: FromObject<'a, 'obj, 'local> {
    fn from_object_env(object: &'a JObject<'obj>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        Ok(Box::<[T]>::from_object_env(object, env)?.into_vec())
    }
}

// Blanket impls seem impossible :(
// impl<'local, T> FromObject<'_, '_, 'local> for Box<[T]>
// where T: for<'a> FromObject<'a, 'local, 'local> + Class + 'local {
//     fn from_object_env(object: &'_ JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
//         get_object_array(object, Some(T::CLASS_PATH), env)?
//             .into_iter()
//             .map(|obj| T::from_object_env(&obj, env))
//             .collect::<Result<Box<[_]>, _>>()
//     }
// }
// impl<T> ToObject for [T]
// where T: ToObject + Class + 'static {
//     fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
//         create_object_array_converted(
//             self,
//             T::CLASS_PATH,
//             // For JClass and JThroable: Can allocate without worry because the function creates a new local frame
//             |obj, env| obj.to_object_env(env),
//         env)
//     }
// }
impl<const N: usize, T> ToObject for [T; N]
where [T]: ToObject {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <[T] as ToObject>::to_object_env(self, env)
    }
}


impl<'local> FromObject<'_, '_, 'local> for Box<[JObject<'local>]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array(object, None, env)?
            .into_iter()
            .map(|obj| JObject::from_object_owned_env(obj, env))
            .collect::<Result<Self, _>>()
    }
}

/// Implementation for a slice of Objects of an ambiguous Class paired with the element Class
macro_rules! impl_paired_obj_slice {
    (ref $($impl_decl:tt)*) => {
        $($impl_decl)* {
            fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
                create_object_array(&self.1, self.0, env)
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
        get_object_array(object, Some(JClass::CLASS_PATH), env)?
            .into_iter()
            .map(|obj| JClass::from_object_owned_env(obj, env))
            .collect::<Result<Self, _>>()
    }
}
impl ToObject for [JClass<'_>] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(
            self,
            JClass::CLASS_PATH,
            // Can allocate without worry because the function creates a new local frame
            |obj, env| obj.to_object_env(env),
        env)
    }
}
impl ToObject for [&JClass<'_>] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(
            self,
            JClass::CLASS_PATH,
            // Can allocate without worry because the function creates a new local frame
            |obj, env| obj.to_object_env(env),
        env)
    }
}

impl<'local> FromObject<'_, '_, 'local> for Box<[JThrowable<'local>]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array(object, Some(JThrowable::CLASS_PATH), env)?
            .into_iter()
            .map(|obj| JThrowable::from_object_owned_env(obj, env))
            .collect::<Result<Self, _>>()
    }
}
impl ToObject for [JThrowable<'_>] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(
            self,
            JThrowable::CLASS_PATH,
            // Can allocate without worry because the function creates a new local frame
            |obj, env| obj.to_object_env(env),
            env
        )
    }
}
impl ToObject for [&JThrowable<'_>] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(
            self,
            JThrowable::CLASS_PATH,
            // Can allocate without worry because the function creates a new local frame
            |obj, env| obj.to_object_env(env),
            env
        )
    }
}

// Implementations for String Array

impl FromObject<'_, '_, '_> for Box<[String]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        get_object_array(object, Some(String::CLASS_PATH), env)?
            .into_iter()
            .map(|obj| String::from_object_env(&obj, env))
            .collect::<Result<Box<[_]>, _>>()
    }
}
impl ToObject for [String] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_from_t(self, String::CLASS_PATH, env)
    }
}
impl ToObject for [&str] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_from_t(self, String::CLASS_PATH, env)
    }
}

// Implementation for [Option<T>]

impl<'local> FromObject<'_, '_, 'local> for Box<[Option<JObject<'local>>]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array(object, None, env)?
            .into_iter()
            .map(|obj| Option::from_object_owned_env(obj, env))
            .collect::<Result<Self, _>>()
    }
}
impl ToObject for (&str, &[Option<JObject<'_>>]) {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(
            &self.1,
            self.0,
            |opt, env| opt.to_object_env(env),
        env)
    }
}
impl ToObject for (&str, &[Option<&JObject<'_>>]) {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(
            &self.1,
            self.0,
            |opt, env| opt.to_object_env(env),
        env)
    }
}
impl<const N: usize> ToObject for (&str, [Option<JObject<'_>>; N]) {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <(&str, &[Option<JObject<'_>>]) as ToObject>::to_object_env(&(self.0, self.1.as_slice()), env)
    }
}
impl<const N: usize> ToObject for (&str, [Option<&JObject<'_>>; N]) {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        <(&str, &[Option<&JObject<'_>>]) as ToObject>::to_object_env(&(self.0, self.1.as_slice()), env)
    }
}

impl<'local> FromObject<'_, '_, 'local> for Box<[Option<JClass<'local>>]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array(object, None, env)?
            .into_iter()
            .map(|obj| Option::from_object_owned_env(obj, env))
            .collect::<Result<Self, _>>()
    }
}
impl ToObject for [Option<JClass<'_>>] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(
            self,
            JClass::CLASS_PATH,
            |opt, env| opt.to_object_env(env),
        env)
    }
}
impl ToObject for [Option<&JClass<'_>>] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(
            self,
            JClass::CLASS_PATH,
            |opt, env| opt.to_object_env(env),
        env)
    }
}

impl<'local> FromObject<'_, '_, 'local> for Box<[Option<JThrowable<'local>>]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array(object, None, env)?
            .into_iter()
            .map(|obj| Option::from_object_owned_env(obj, env))
            .collect::<Result<Self, _>>()
    }
}
impl ToObject for [Option<JThrowable<'_>>] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(
            self,
            JThrowable::CLASS_PATH,
            |opt, env| opt.to_object_env(env),
        env)
    }
}
impl ToObject for [Option<&JThrowable<'_>>] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_converted(
            self,
            JThrowable::CLASS_PATH,
            |opt, env| opt.to_object_env(env),
        env)
    }
}

impl FromObject<'_, '_, '_> for Box<[Option<String>]> {
    fn from_object_env(object: &JObject, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
        get_object_array(object, Some(String::CLASS_PATH), env)
            .and_then(|array|
                array.into_iter()
                    .map(|obj| Option::<String>::from_object_env(&obj, env))
                    .collect::<Result<Box<[_]>, _>>()
            )
    }
}
impl ToObject for [Option<String>] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_from_t(self, String::CLASS_PATH, env)
    }
}
impl ToObject for [Option<&str>] {
    fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
        create_object_array_from_t(self, String::CLASS_PATH, env)
    }
}


// Primitive Arrays
macro_rules! impl_prim_array {
    ($prim:ty) => {
        impl FromObject<'_, '_, '_> for Box<[$prim]> {
            fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
                get_java_prim_array(object, env)
            }
        }
        impl ToObject for [$prim] {
            fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
                create_java_prim_array(self, env)
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
// Blanket impls seem impossible :(
// impl<T> FromObject<'_, '_, '_> for Box<[T]>
// where T: Primitive + for<'a, 'obj, 'local> FromObject<'a, 'obj, 'local> {
//     fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'_>) -> Result<Self, FromObjectError> {
//         get_java_prim_array(object, env)
//     }
// }
// impl<T> ToObject for [T]
// where T: Primitive {
//     fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {
//         create_java_prim_array(self, env)
//     }
// }


// Multi-dimensional Arrays (up to 10 dimensions)
// 2 Dimensions
impl<'local, T> FromObject<'_, '_, 'local> for Box<[Box<[T]>]>
where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local>,
      T: Class
{
    fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array(object, Some(&format!("[[L{};", T::CLASS_PATH)), env)?
            .into_iter()
            .map(|obj| Box::<[T]>::from_object_env(&obj, env))
            .collect()
    }
}
// 3 Dimensions
impl<'local, T> FromObject<'_, '_, 'local> for Box<[Box<[Box<[T]>]>]>
where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local>,
      T: Class
{
    fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array(object, Some(&format!("[[[L{};", T::CLASS_PATH)), env)?
            .into_iter()
            .map(|obj| Box::<[Box<[T]>]>::from_object_env(&obj, env))
            .collect()
    }
}
// 4 Dimensions
impl<'local, T> FromObject<'_, '_, 'local> for Box<[Box<[Box<[Box<[T]>]>]>]>
where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local>,
      T: Class
{
    fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array(object, Some(&format!("[[[[L{};", T::CLASS_PATH)), env)?
            .into_iter()
            .map(|obj| Box::<[Box<[Box<[T]>]>]>::from_object_env(&obj, env))
            .collect()
    }
}
// 5 Dimensions
impl<'local, T> FromObject<'_, '_, 'local> for Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>
where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local>,
      T: Class
{
    fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array(object, Some(&format!("[[[[[L{};", T::CLASS_PATH)), env)?
            .into_iter()
            .map(|obj| Box::<[Box<[Box<[Box<[T]>]>]>]>::from_object_env(&obj, env))
            .collect()
    }
}
// 6 Dimensions
impl<'local, T> FromObject<'_, '_, 'local> for Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>
where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local>,
      T: Class
{
    fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array(object, Some(&format!("[[[[[[L{};", T::CLASS_PATH)), env)?
            .into_iter()
            .map(|obj| Box::<[Box<[Box<[Box<[Box<[T]>]>]>]>]>::from_object_env(&obj, env))
            .collect()
    }
}
// 7 Dimensions
impl<'local, T> FromObject<'_, '_, 'local> for Box<[Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>]>
where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local>,
      T: Class
{
    fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array(object, Some(&format!("[[[[[[[L{};", T::CLASS_PATH)), env)?
            .into_iter()
            .map(|obj| Box::<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>::from_object_env(&obj, env))
            .collect()
    }
}
// 8 Dimensions
impl<'local, T> FromObject<'_, '_, 'local> for Box<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>]>]>
where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local>,
      T: Class
{
    fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array(object, Some(&format!("[[[[[[[[L{};", T::CLASS_PATH)), env)?
            .into_iter()
            .map(|obj| Box::<[Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>]>::from_object_env(&obj, env))
            .collect()
    }
}
// 9 Dimensions
impl<'local, T> FromObject<'_, '_, 'local> for Box<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>]>]>]>
where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local>,
      T: Class
{
    fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array(object, Some(&format!("[[[[[[[[[L{};", T::CLASS_PATH)), env)?
            .into_iter()
            .map(|obj| Box::<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>]>]>::from_object_env(&obj, env))
            .collect()
    }
}
// 10 Dimensions
impl<'local, T> FromObject<'_, '_, 'local> for Box<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>]>]>]>]>
where Box<[T]>: for<'a, 'obj> FromObject<'a, 'obj, 'local>,
      T: Class
{
    fn from_object_env(object: &JObject<'_>, env: &mut JNIEnv<'local>) -> Result<Self, FromObjectError> {
        get_object_array(object, Some(&format!("[[[[[[[[[[L{};", T::CLASS_PATH)), env)?
            .into_iter()
            .map(|obj| Box::<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>]>]>]>::from_object_env(&obj, env))
            .collect()
    }
}

// // 2 Dimensions
// impl<T> ToObject for Box<[Box<[T]>]>
// where Box<[T]>: ToObject,
//       T: Class
// {
//     fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {

//     }
// }
// // 3 Dimensions
// impl<T> ToObject for Box<[Box<[Box<[T]>]>]>
// where Box<[T]>: ToObject,
//       T: Class
// {
//     fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {

//     }
// }
// // 4 Dimensions
// impl<T> ToObject for Box<[Box<[Box<[Box<[T]>]>]>]>
// where Box<[T]>: ToObject,
//       T: Class
// {
//     fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {

//     }
// }
// // 5 Dimensions
// impl<T> ToObject for Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>
// where Box<[T]>: ToObject,
//       T: Class
// {
//     fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {

//     }
// }
// // 6 Dimensions
// impl<T> ToObject for Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>
// where Box<[T]>: ToObject,
//       T: Class
// {
//     fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {

//     }
// }
// // 7 Dimensions
// impl<T> ToObject for Box<[Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>]>
// where Box<[T]>: ToObject,
//       T: Class
// {
//     fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {

//     }
// }
// // 8 Dimensions
// impl<T> ToObject for Box<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>]>]>
// where Box<[T]>: ToObject,
//       T: Class
// {
//     fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {

//     }
// }
// // 9 Dimensions
// impl<T> ToObject for Box<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>]>]>]>
// where Box<[T]>: ToObject,
//       T: Class
// {
//     fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {

//     }
// }
// // 10 Dimensions
// impl<T> ToObject for Box<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[Box<[T]>]>]>]>]>]>]>]>]>]>
// where Box<[T]>: ToObject,
//       T: Class
// {
//     fn to_object_env<'local>(&self, env: &mut JNIEnv<'local>) -> JObject<'local> {

//     }
// }