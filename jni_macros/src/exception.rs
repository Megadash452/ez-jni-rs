use std::str::FromStr;

use crate::utils::{first_char_uppercase, ClassPath, JavaPrimitive, RustPrimitive};
use either::Either;
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{parse::Parse, spanned::Spanned, AngleBracketedGenericArguments, Field, Fields, GenericArgument, Ident, ItemEnum, ItemStruct, Lifetime, LitStr, Token, Type, TypePath};

pub fn from_exception_struct(st: syn::ItemStruct) -> syn::Result<TokenStream> {
    let class = get_class_attribute(&st.attrs, st.ident.span())?
        .to_token_stream()
        .to_string();
    
    // Find the JObject field with a lifetime, and use that lifetime's name for the JNIEnv lifetime
    let env_lt = get_env_lifetime(Either::Left(&st));

    let st_ident = st.ident;
    let st_generics = &st.generics;
    let st_generic_params = &st.generics.params;
    let st_ctor = struct_constructor(st.fields, &class)?;
    
    Ok(quote! {
        impl <#st_generic_params> ::ez_jni::FromException<#env_lt> for #st_ident #st_generics {
            fn from_exception(env: &mut ::jni::JNIEnv<#env_lt>, exception: &::jni::objects::JThrowable) -> Result<Self, ::ez_jni::object::FromObjectError> {
                let __class = env.get_object_class(&exception)
                    .unwrap_or_else(|err| panic!("Failed to get Object's class: {err}"));
                if env.is_assignable_from(#class, &__class).unwrap() {
                    return Err(::ez_jni::object::FromObjectError::ClassMismatch {
                        obj_class: ::ez_jni::utils::get_string(::ez_jni::call!(__class.getName() -> java.lang.String), env),
                        target_class: #class
                    })
                }

                Ok(Self #st_ctor)
            }
        }
    })
}

pub fn from_exception_enum(enm: syn::ItemEnum) -> syn::Result<TokenStream> {
    let mut errors = Vec::new();
    
    if let Some(attr) = enm.attrs.iter()
        .find(|attr| {
            attr.path()
                .get_ident()
                .is_some_and(|ident| ident.to_string() == "class")
        })
    {
        errors.push(syn::Error::new(attr.span(), "The \"class\" attribute can't be used on the enum, only on its variants"));
    }
    
    if enm.variants.is_empty() {
        errors.push(syn::Error::new(Span::call_site(), "Error enum must have at least 1 variant"));
    }
    
    let env_lt = get_env_lifetime(Either::Right(&enm));

    let class_checks = enm.variants.into_iter()
        .enumerate()
        .filter_map(|(i, variant)| {
            // Get class name for this variant
            let class = get_class_attribute(&variant.attrs, variant.ident.span())
                .map_err(|err| errors.push(err))
                .ok()?
                .to_token_stream().to_string();
            let ident = &variant.ident;
            // Get a constructor for this variant
            let ctor = struct_constructor(variant.fields, &class)
                .map_err(|err| errors.push(err))
                .ok()?;
            let _if = if i == 0 {
                quote! { if }
            } else {
                quote! { else if }
            };
            // Check if Exception is the class that this Variant uses, and construct the variant
            Some(quote! {
                #_if env.is_assignable_from(#class, &__class).unwrap() {
                    Ok(Self::#ident #ctor)
                }
            })
        })
        .collect::<Box<_>>();
    

    // Check if there are errors
    if let Some(last) = errors.pop() /*Order doesn't matter*/ {
        let errors = errors.into_iter()
            .fold(last, |mut errors, err| {
                errors.combine(err);
                errors
            });
        return Err(errors);
    }

    let enm_ident = enm.ident;
    let enm_generics = &enm.generics;
    let enm_generic_params = &enm.generics.params;
    Ok(quote! {
        impl <#enm_generic_params> ez_jni::FromException<#env_lt> for #enm_ident #enm_generics {
            fn from_exception(env: &mut ::jni::JNIEnv<#env_lt>, exception: &::jni::objects::JThrowable) -> Result<Self, ::ez_jni::object::FromObjectError> {
                let __class = env.get_object_class(&exception)
                    .unwrap_or_else(|err| panic!("Failed to get Object's class: {err}"));
                #(#class_checks)* else {
                    Err(::ez_jni::object::FromObjectError::ClassMismatch {
                        obj_class: ::ez_jni::utils::get_string(::ez_jni::call!(__class.getName() -> java.lang.String), env),
                        target_class: ""
                    })
                }
            }
        }
    })
}

/// Find the `class` attribute of the **struct** or **enum variant** and return the Path to the Java Class.
/// 
/// Takes the [`Span`] of the struct or enum variant's Name for errors.
fn get_class_attribute(attributes: &[syn::Attribute], item_span: Span) -> syn::Result<ClassPath> {
    let mut iter = attributes.iter()
        .filter(|attr| {
            attr.path()
                .get_ident()
                .is_some_and(|ident| ident.to_string() == "class")
        });
    let attr = iter
        .next()
        .ok_or_else(|| syn::Error::new(item_span, "Must have \"class\" attribute"))?;
    if let Some(attr) = iter.next() {
        return Err(syn::Error::new(
            attr.span(),
            "must have only 1 \"class\" attribute",
        ));
    }

    // Get attribute value
    match &attr.meta {
        // Can be #[class(java.class.path)] or #[class("java.class.path")]
        syn::Meta::List(syn::MetaList { tokens, .. }) => match syn::parse2::<LitStr>(tokens.clone()).ok() {
            Some(s) => syn::parse_str::<ClassPath>(&s.value()),
            None => syn::parse2::<ClassPath>(tokens.clone())
        },
        // Can only be #[class("java.class.path")]
        syn::Meta::NameValue(syn::MetaNameValue { value, .. }) => match value {
            syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Str(value), .. }) => syn::parse_str::<ClassPath>(&value.value()),
            _ => return Err(syn::Error::new(value.span(), "Try using a string literal here"))
        },
        syn::Meta::Path(path) => return Err(syn::Error::new(path.span(), "\"class\" attribute must have a value of a Java ClassPath (e.g. #[class(java.lang.Exception)])")) 
    }
}

// Find the JObject field with a lifetime, and use that lifetime's name for the JNIEnv lifetime,
// Or returns implicit lifetime if there is no JObject
fn get_env_lifetime(item: Either<&ItemStruct, &ItemEnum>) -> Lifetime {
    fn find_in_fields(fields: &Fields) -> Option<Lifetime> {
        fields.iter()
            .find_map(|field| match &field.ty {
                Type::Path(TypePath { path, .. } ) if path.segments.last()
                    .is_some_and(|seg| seg.ident.to_string() == "JObject")
                => match path.segments.last().map(|seg| &seg.arguments) {
                    Some(syn::PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }))
                    => match args.first() {
                        Some(GenericArgument::Lifetime(lt)) => Some(lt.clone()),
                        _ => None
                    },
                    _ => None
                },
                _ => None
            })
    }
    
    match item {
        Either::Left(st) => find_in_fields(&st.fields)
            .unwrap_or(Lifetime::new("'_", st.span())),
        Either::Right(enm) => enm.variants.iter()
            .find_map(|variant| find_in_fields(&variant.fields))
            .unwrap_or(Lifetime::new("'_", enm.span()))
    }
}

/// Consists of *key-value pairs* inside a parenthesis.
/// * **name** - Use this name for the Object's member lookup instead of the field's name.
///                     Mutually exclusive with `call`.
/// * **call** - Instead of a looking up a *member*, call one of the Object's *method*s.
///                     Mutually exclusive with `name`.
/// * **class** - The Class of the object if the field is a `JObject`.
/// 
/// Either *name* or *call* MUST be used if the field belongs to a Tuple struct.
/// 
/// Example: `#[field(call = getInt, class = java.lang.Int)] int: i32`
struct FieldAttr {
    name: Option<Ident>,
    call: Option<Ident>,
    class: Option<ClassPath>,
}
impl FieldAttr {
    /// Parse the `field` attribute from a struct's field.
    pub fn get_from_attrs(field: &Field) -> syn::Result<Self> {
        field.attrs
            .iter()
            .find(|&attr|
                attr.path()
                    .get_ident()
                    .is_some_and(|ident| ident.to_string() == "field")
            )
            .map_or_else(|| Ok(Self { name: None, call: None, class: None }), |attr| Self::parse_attr_meta(&attr.meta))
    }
    
    /// Parses the tokens in the attribute as `comma-separated` `key-value pairs`.
    fn parse_attr_meta(meta: &syn::Meta) -> syn::Result<Self> {
        let input = match meta {
            syn::Meta::List(meta) => meta.tokens.clone(),
            _ => return Err(syn::Error::new(meta.span(), "Attribute \"field\" must have content in parenthesis"))
        };
        let mut unparsed_kvs = Vec::<TokenStream>::new();
        
        // Separate the tokens between commas (,)
        let mut current = TokenStream::new();
        for token in input {
            match token {
                // At every comma (,) push the current tokenstream, and start with a new tokenstream
                proc_macro2::TokenTree::Punct(p) if p.as_char() == ',' => {
                    unparsed_kvs.push(current);
                    current = TokenStream::new();
                },
                _ => current.append(token),
            }
        }
        // Push whatever is left
        if !current.is_empty() {
            unparsed_kvs.push(current);
        }
        
        // Then parse the key-value pairs
        struct KVPair { key: Ident, val: TokenStream }
        impl Parse for KVPair {
            fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
                Ok(Self {
                    key: input.parse::<Ident>()?,
                    val: {
                        input.parse::<Token![=]>()?;
                        input.parse()?
                    }
                })
            }
        }
        fn parse_val<T: Parse>(key: Ident, val: TokenStream, store: &mut Option<T>, ) -> syn::Result<()> {
            if store.is_none() {
                *store = Some(syn::parse2(val)?)
            } else {
                return Err(syn::Error::new(key.span(), "Already used \"{key}\""))
            }
            
            Ok(())
        }
        let mut name = None;
        let mut call = None;
        let mut class = None;
        
        for tokens in unparsed_kvs {
            let KVPair { key, val } = syn::parse2::<KVPair>(tokens)?;
            
            match key.to_string().as_str() {
                "name" => parse_val(key, val, &mut name)?,
                "call" => parse_val(key, val, &mut call)?,
                "class" => parse_val(key, val, &mut class)?,
                _ => return Err(syn::Error::new(key.span(), "Expected one of the following keys: \"name\", \"call\", \"class\""))
            }
        }
        
        Ok(Self { name, call, class })
    }
}

enum FieldType {
    Prim(RustPrimitive),
    Object(ClassPath),
    // The Java Class is obtained from the struct/enum's `Class` implementation.
    Implicit(syn::Type)
}
impl FieldType {
    pub fn from_field(field_ty: &Type, class_attr: Option<ClassPath>) -> syn::Result<Self> {
        // Check if the Field's type is a primitive
        match RustPrimitive::from_str(&field_ty.to_token_stream().to_string()) {
            Ok(prim) => match class_attr {
                Some(class) => Err(syn::Error::new(class.span(), "Can't use \"class\" when the field is prmitive")),
                None => Ok(Self::Prim(prim))
            },
            Err(_) => match class_attr {
                // If not a primitive, then use the provided Class
                Some(class) => match &field_ty {
                    // There is no way to know if the Object is the same `::jni::objects::JObject`,
                    // just that it could be if the type is a Type Path.
                    Type::Path(_) => Ok(Self::Object(class)),
                    _ => Err(syn::Error::new(class.span(), "\"class\" is only allowed for JObject"))
                },
                // If none of the previous, then the class will be obtained from the `Class` impl
                None => Ok(Self::Implicit(field_ty.clone()))
            }
        }
    }
    
    /// The Type string used in the signature
    pub fn sig_type(&self) -> TokenStream {
        match self {
            Self::Prim(prim) => JavaPrimitive::from(*prim).sig_char().to_uppercase().collect::<String>().to_token_stream(),
            Self::Object(class) => class.sig_type().to_token_stream(),
            Self::Implicit(ty) => quote_spanned! {ty.span()=> <#ty as ::ez_jni::object::Class>::PATH }
        }
    }
    /// See [`JavaPrimitive::sig_char()`].
    pub fn sig_char(&self) -> Ident {
        match self {
            Self::Prim(prim) => Ident::new(&JavaPrimitive::from(*prim).sig_char().to_string(), Span::call_site()),
            Self::Object(class) => Ident::new("l", class.span()),
            Self::Implicit(ty) => Ident::new("l", ty.span()),
        }
    }
    
    fn converted(&self, value: TokenStream) -> TokenStream {
        match self {
            FieldType::Prim(prim) => prim.special_case_conversion(value.clone())
                .unwrap_or(value),
            FieldType::Implicit(ty) => quote_spanned! {ty.span()=> <#ty as ::ez_jni::object::Class>::from_object(#value, env)? },
            FieldType::Object(_) => value
        }
    }
}

/// Returns a struct constructor, where all its fields are initialized with JNI calls.
/// Can handle *Unit, Tuple, and Named structs*.
/// Can also be used for enum variants.
/// 
/// The *type* and *name* of the field will be used to create a JNI call to the Object which will be converted from.
/// By default, the **name** will be used to get the value from a *member of the Object*,
/// and the **type** will be converted from a Java Object if it implements `ez_jni::Class`.
/// 
/// **class** is the Java Class of the struct/enum.
/// 
/// See [`FieldAttr`] for syntax.
fn struct_constructor(fields: Fields, class: &str) -> syn::Result<TokenStream> {    
    let mut errors = Vec::new();
    
    // Produce the value that will be assigned for each field
    let values = fields.iter()
        .filter_map(|field| {
            let attr = FieldAttr::get_from_attrs(&field)
                .map_err(|err| errors.push(err))
                .ok()?;
            // Determine the type of the Java member
            let ty = FieldType::from_field(&field.ty, attr.class)
                .map_err(|err| errors.push(err))
                .ok()?;
            
            Some(if let Some(name) = attr.name {
                // Use the "name" of the field attribute
                let name = name.to_string();
                let sig_char = ty.sig_char();
                let sig_ty = ty.sig_type();
                
                ty.converted(quote_spanned! {field.span()=>
                    env.get_field(&exception, #name, #sig_ty)
                        .map_err(|err| if let ::jni::errors::Error::FieldNotFound { name, sig } = err {
                            ::ez_jni::object::FromObjectError::FieldNotFound { name, ty: sig, target_class: #class }
                        } else {
                            panic!("Error occurred while accessing field: {err}")
                        })?
                        .#sig_char().unwrap_or_else(|err| panic!("The method call did not return the expected type: {err}"))
                })
            } else if let Some(call) = attr.call {
                // Call the Java method
                let call = LitStr::new(&call.to_string(), call.span());
                let sig_char = ty.sig_char();
                let sig_ty = ty.sig_type();
                let sig_ty = match ty {
                    FieldType::Implicit(_) => quote!(format!("(){}", #sig_ty)),
                    _ => format!("(){sig_ty}").to_token_stream(),
                };
                
                ty.converted(quote_spanned! {call.span()=>
                    env.call_method(&exception, #call, #sig_ty, &[])
                        .map_err(|err| if let ::jni::errors::Error::MethodNotFound { name, sig } = err {
                            ::ez_jni::object::FromObjectError::FieldNotFound { name, ty: sig, target_class: #class }
                        } else {
                            panic!("Error occurred while calling getter method: {err}")
                        })?
                        .#sig_char()
                        .unwrap_or_else(|err| panic!("The method call did not return the expected type: {err}"))
                })
            } else if let Some(name) = &field.ident {
                // Use the name of the field, and also call "get{Name}" if field not found
                let name = name.to_string();
                let sig_char = ty.sig_char();
                let sig_ty = ty.sig_type();
                let getter_name = format!("get{}", first_char_uppercase(name.to_string()));
                let getter_sig = match ty {
                    FieldType::Implicit(_) => quote!(format!("(){}", #sig_ty)),
                    _ => format!("(){sig_ty}").to_token_stream(),
                };
                
                ty.converted(quote_spanned! {field.span()=>
                    env.get_field(&exception, #name, #sig_ty)
                        .or_else(|err| if let ::jni::errors::Error::FieldNotFound { .. } = err {
                            env.call_method(&exception, #getter_name, #getter_sig, &[])
                        } else {
                            panic!("Error occurred while accessing field: {err}")
                        })
                        .map_err(|err| if let ::jni::errors::Error::MethodNotFound { name, sig } = err {
                            ::ez_jni::object::FromObjectError::FieldNotFound { name, ty: sig, target_class: #class }
                        } else {
                            panic!("Error occurred while calling getter method: {err}")
                        })?
                        .#sig_char().unwrap_or_else(|err| panic!("The method call did not return the expected type: {err}"))
                })
            } else {
                errors.push(syn::Error::new(field.span(), "Field must have \"name\" or \"call\" if it is unnamed. See the 'field' attribute."));
                return None
            })
        })
        .collect::<Box<_>>();
    
    // Check if there are errors
    if let Some(last) = errors.pop() /*Order doesn't matter*/ {
        let errors = errors.into_iter()
            .fold(last, |mut errors, err| {
                errors.combine(err);
                errors
            });
        return Err(errors);
    }
    
    let span = fields.span();
    Ok(match fields {
        Fields::Unit => fields.to_token_stream(), // Unit structs have no fields to construct
        Fields::Unnamed(_) => quote_spanned! {span=> ( #(#values),* ) },
        Fields::Named(syn::FieldsNamed{ named: fields, .. }) => {
            let names = fields.iter()
                .map(|field| field.ident.as_ref().unwrap());
            quote_spanned! {span=> {
                #( #names: #values ),*
            } }
        }
    })
}
