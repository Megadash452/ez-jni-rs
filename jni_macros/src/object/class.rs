use either::Either;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{ItemEnum, ItemStruct};
use super::*;

pub fn from_object_st(mut st: ItemStruct) -> syn::Result<TokenStream> {
    let class = get_class_attribute_required(&mut st.attrs, st.ident.span())?
        .to_jni_class_path();
    
    let mut st_generic_params = st.generics.params.clone();
    let env_lt = get_local_lifetime(Either::Left(&st), &mut st_generic_params);
    let st_ident = st.ident;
    let st_generics = st.generics;
    let st_ctor = struct_constructor(&st.fields, &class)?;

    Ok(quote! {
        impl <#st_generic_params> ::ez_jni::FromObject<#env_lt> for #st_ident #st_generics {
            const PATH: &'static str = #class;

            fn from_object(object: &::jni::objects::JObject, env: &mut ::jni::JNIEnv<#env_lt>) -> Result<Self, ::ez_jni::FromObjectError> {
                if object.is_null() {
                    return Err(::ez_jni::FromObjectError::Null);
                }

                let __class = env.get_object_class(object)
                    .unwrap_or_else(|err| panic!("Failed to get Object's class: {err}"));
                
                if !env.is_instance_of(object, Self::PATH).unwrap() {
                    return Err(::ez_jni::FromObjectError::ClassMismatch {
                        obj_class: ::ez_jni::call!(__class.getName() -> String),
                        target_class: Self::PATH
                    })
                }

                Ok(Self #st_ctor)
            }
        }
    })
}

pub fn from_object_enum(mut enm: ItemEnum) -> syn::Result<TokenStream> {
    let mut errors = Vec::new();

    // TODO: make this optional somehow
    let base_class = get_class_attribute_required(&mut enm.attrs, enm.ident.span())
        .map_err(|err| errors.push(err))
        .ok()
        .map(|base_class| base_class.to_jni_class_path());

    if enm.variants.is_empty() {
        errors.push(syn::Error::new(Span::call_site(), "Enum must have at least 1 variant"));
    }

    let class_checks = construct_variants(enm.variants.iter_mut())
        .filter_map(|res| res.map_err(|err| errors.push(err)).ok())
        .collect::<Box<[_]>>();

    merge_errors(errors)?;

    let mut enm_generic_params = enm.generics.params.clone();
    let env_lt = get_local_lifetime(Either::Right(&enm), &mut enm_generic_params);
    let enm_ident = enm.ident;
    let enm_generics = &enm.generics;
    Ok(quote! {
        impl <#enm_generic_params> ::ez_jni::FromObject<#env_lt> for #enm_ident #enm_generics {
            const PATH: &'static str = #base_class; 

            fn from_object(object: &::jni::objects::JObject, env: &mut ::jni::JNIEnv<#env_lt>) -> Result<Self, ::ez_jni::FromObjectError> {
                if object.is_null() {
                    return Err(::ez_jni::FromObjectError::Null);
                }
                
                let __class = env.get_object_class(object)
                    .unwrap_or_else(|err| panic!("Failed to get Object's class: {err}"));
                let __class_mismatch_err = ::ez_jni::FromObjectError::ClassMismatch {
                    obj_class: ::ez_jni::call!(__class.getName() -> String),
                    target_class: Self::PATH
                };

                if !env.is_instance_of(object, Self::PATH).unwrap() {
                    return Err(__class_mismatch_err)
                }

                #(#class_checks)* else {
                    Err(__class_mismatch_err)
                }
            }
        }
    })
}