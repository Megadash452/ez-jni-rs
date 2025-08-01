use either::Either;
use convert_case::{Case, Casing};
use proc_macro2::{Delimiter, Span, TokenStream, TokenTree};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{parse::{Parse, ParseStream, Parser}, punctuated::Punctuated, token::Bracket, AngleBracketedGenericArguments, Field, Fields, GenericArgument, GenericParam, Generics, Ident, ItemEnum, ItemStruct, Lifetime, LitStr, Token, TypePath, Variant};
use itertools::Itertools as _;
use std::{cell::RefCell, collections::{HashMap, HashSet}};
use crate::{
    types::{ArrayType, Class, ClassRustType, InnerType, OptionType, SigType, Type}, utils::{merge_errors, take_class_attribute, take_class_attribute_required, Spanned}
};

/// Outputs the trait Implementation of [`FromObject`][https://docs.rs/ez_jni/0.5.2/ez_jni/trait.FromObject.html] for **structs**.
pub fn derive_struct(mut st: ItemStruct) -> syn::Result<TokenStream> {
    let class = take_class_attribute_required(&mut st.attrs, st.ident.span())?
        .to_jni_class_path();
    
    let mut st_generic_params = st.generics.params.clone();
    let env_lt = get_local_lifetime(Either::Left(&st), &mut st_generic_params);
    let st_ident = st.ident;
    let st_generics = st.generics;
    let st_ctor = struct_constructor(&st.fields)?;

    Ok(quote! {
        impl <#st_generic_params> ::ez_jni::FromObject<'_, '_, #env_lt> for #st_ident #st_generics {
            fn from_object_env(object: &::jni::objects::JObject<'_>, env: &mut ::jni::JNIEnv<#env_lt>) -> ::std::result::Result<Self, ::ez_jni::FromObjectError> {
                #[allow(unused_imports)]
                use ::std::borrow::Borrow;

                ::ez_jni::utils::check_object_class(object, <Self as ::ez_jni::Class>::CLASS_PATH, env)?;
                Ok(Self #st_ctor)
            }
        }
        impl <#st_generic_params> ::ez_jni::FromArrayObject<#env_lt> for #st_ident #st_generics {
            #[inline(always)]
            fn from_array_object(object: &::jni::objects::JObject<'_>, env: &mut ::jni::JNIEnv<'local>) -> ::std::result::Result<::std::boxed::Box<[Self]>, ::ez_jni::FromObjectError> {
                ::ez_jni::utils::get_object_array_converted(object, |obj, env| <Self as ::ez_jni::FromObject>::from_object_env(&obj, env), env)
            }
            #[inline(always)]
            fn from_array_object_nullable(object: &::jni::objects::JObject<'_>, env: &mut ::jni::JNIEnv<'local>) -> ::std::result::Result<::std::boxed::Box<[::std::option::Option<Self>]>, ::ez_jni::FromObjectError> {
                ::ez_jni::utils::get_object_array_converted(object, |obj, env| <::std::option::Option::<Self> as ::ez_jni::FromObject>::from_object_env(&obj, env), env)
            }
        }
        impl <#st_generic_params> ::ez_jni::Class for #st_ident #st_generics {
            const CLASS_PATH: &'static str = #class;
        }
    })
}

/// Outputs the trait Implementation of [`FromObject`][https://docs.rs/ez_jni/0.5.2/ez_jni/trait.FromObject.html] for **enums**.
pub fn derive_enum(mut enm: ItemEnum) -> syn::Result<TokenStream> {
    let mut errors = Vec::new();

    if enm.variants.is_empty() {
        errors.push(syn::Error::new(Span::call_site(), "Enum must have at least 1 variant"));
    }

    let base_class = take_class_attribute(&mut enm.attrs)
        .map_err(|err| errors.push(err))
        .ok()
        .and_then(|o| o)
        .map(|class| class.to_jni_class_path());

    let base_class_check = base_class.as_ref()
        .map(|_| quote_spanned! {enm.ident.span()=>
            if !env.is_instance_of(object, <Self as ::ez_jni::Class>::CLASS_PATH).unwrap() {
                return Err(::ez_jni::FromObjectError::ClassMismatch {
                    obj_class: __class,
                    target_class: Some(<Self as ::ez_jni::Class>::CLASS_PATH.to_string())
                })
            }
        })
        .unwrap_or(TokenStream::new());

    let class_checks = construct_variants(enm.variants.iter_mut())
        .filter_map(|res| res.map_err(|err| errors.push(err)).ok())
        .collect::<Box<[_]>>();

    merge_errors(errors)?;

    let mut enm_generic_params = enm.generics.params.clone();
    let env_lt = get_local_lifetime(Either::Right(&enm), &mut enm_generic_params);
    let enm_ident = &enm.ident;
    let enm_generics = &enm.generics;

    // Types that implement FromObject are also given a Class implementation.
    // However, for enums this is optional because only the variants need to specify a class.
    let base_class_impl = base_class.as_ref()
        .map(|class| quote! {
            impl <#enm_generic_params> ::ez_jni::Class for #enm_ident #enm_generics {
                const CLASS_PATH: &'static str = #class;
            }
        })
        .unwrap_or(TokenStream::new());
    let base_class = if base_class.is_some() {
        quote_spanned! {enm.ident.span()=> Some(<#enm_ident as ::ez_jni::Class>::CLASS_PATH) }
    } else {
        quote_spanned! {enm.ident.span()=> None }
    };

    Ok(quote! {
        impl <#enm_generic_params> ::ez_jni::FromObject<'_, '_, #env_lt> for #enm_ident #enm_generics {
            fn from_object_env(object: &::jni::objects::JObject<'_>, env: &mut ::jni::JNIEnv<#env_lt>) -> ::std::result::Result<Self, ::ez_jni::FromObjectError> {
                #[allow(unused_imports)]
                use ::std::borrow::Borrow;

                if object.is_null() {
                    return Err(::ez_jni::FromObjectError::Null);
                }

                let __class = ::ez_jni::call!(env=> call!(env=> object.getClass() -> Class).getName() -> String);

                #base_class_check

                #(#class_checks)* else {
                    Err(::ez_jni::FromObjectError::ClassMismatch {
                        obj_class: __class,
                        target_class: None
                    })
                }
            }
        }
        impl <#enm_generic_params> ::ez_jni::FromArrayObject<#env_lt> for #enm_ident #enm_generics {
            #[inline(always)]
            fn from_object_array_helper(object: &::jni::objects::JObject<'_>, env: &mut ::jni::JNIEnv<'local>) -> ::std::result::Result<::std::boxed::Box<[Self]>, ::ez_jni::FromObjectError> {
                ::ez_jni::utils::get_object_array(object, #base_class, env)?
                    .into_iter()
                    .map(|obj| <#enm_ident as ::ez_jni::FromObject>::from_object_env(&obj, env))
                    .collect()
            }
        }
        #base_class_impl
    })
}

/// Properties parsed from the value of an [`Attribute`][syn::Attribute].
/// 
/// The properties are *Key-Value Pairs*,
/// where the *Key* is an [`Ident`] and the *Value* can be any tokens.
/// The *attribute* must have tokens wrapped in parenthesis, like this: `#[attr(key = val)]`.
/// 
/// Parse the attribute's content to get an instance of [`AttributeProps`],
/// and [`take`][Self::take] properties with a *name*.
/// Then call [`Self::finish`] to make sure all the properties provided by the user are valid for the macro.
struct AttributeProps {
    properties: RefCell<HashMap<String, TokenStream>>,
    /// The keys that were used as the **name** in all calls to [`self.take()`][Self::take].
    probed: RefCell<HashSet<String>>,
}
impl AttributeProps {
    /// Parse the content of an [`Attribute`][syn::Attribute] and
    /// with the parsed *properties* **build** an instance of a Type that stores the properties.
    /// 
    /// ## Example
    /// ```ignore
    /// AttributeProps::parse_attr_with(attr, |props| {
    ///     Ok(Self {
    ///         name: props.take("name")?,
    ///         call: props.take("call")?,
    ///         class: props.take("class")?,
    ///     })
    /// })
    /// ```
    pub fn parse_attr_with<T>(attr: &syn::Attribute, builder: impl FnOnce(&Self) -> syn::Result<T>) -> syn::Result<T> {
        let props = Self::parse_attr(attr)?;
        let rtrn = builder(&props)?;
        props.finish()?;
        Ok(rtrn)
    }
    /// Parse the content of an [`Attribute`][syn::Attribute] to get *properties*.
    pub fn parse_attr(attr: &syn::Attribute) -> syn::Result<Self> {
        let attr_val = match &attr.meta {
            syn::Meta::List(meta) => meta.tokens.clone(),
            _ => return Err(syn::Error::new(attr.meta.span(), "This attribute takes properties as input, so it must have content in parenthesis"))
        };
    
        syn::parse2(attr_val)
    }

    /// Get and *parse* the **value** of a property, marking it as *taken*.
    /// 
    /// `panic!s` if [`take()`][Self::take] was already called with the same **name**.
    /// Return `Ok(None)` if there is no property with this **name**.
    /// Returns [`syn::Error`] if the value could not be parsed with `T`.
    pub fn take<T: Parse>(&self, name: &str) -> syn::Result<Option<T>> {
        if !self.probed.borrow_mut().insert(name.to_string()) {
            panic!("The property with name \"{name}\" was already taken")
        }

        match self.properties.borrow_mut().remove(name) {
            Some(val) => Ok(Some(syn::parse2(val)?)),
            None => Ok(None)
        }
    }

    #[allow(unstable_name_collisions)]
    pub fn finish(self) -> syn::Result<()> {
        let probed = self.probed.borrow();
        let properties = self.properties.borrow();

        if !properties.is_empty() {
            // Collect all the *names* used with [`Self::take()`] to print as the expected keys 
            let expected_props = probed.iter()
                .map(|key| format!("\"{key}\""))
                .intersperse(", ".to_string())
                .collect::<String>();
            // Create errors for the leftover properties
            Err(merge_errors(
                properties.iter()
                    .map(|(key, _)| syn::Error::new(key.span(), format!("Unknown key \"{key}\"; Expected one of the following keys: {expected_props}")))
            ).unwrap_err())
        } else {
            Ok(())
        }
    }
}
impl Parse for AttributeProps {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut unparsed_kvs = Vec::<TokenStream>::new();
        
        // Separate the tokens between commas (,)
        let mut current = TokenStream::new();
        while let Ok(token) = input.parse::<TokenTree>() {
            match token {
                // At every comma (,) push the current tokenstream, and start with a new tokenstream
                TokenTree::Punct(p) if p.as_char() == ',' => {
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

        let mut errors = Vec::new();
        // Make the key Ident so that the span can be used for errors
        let mut properties = HashMap::new();

        // Then parse the key-value pairs
        for tt in unparsed_kvs {
            match Parser::parse2(|input: ParseStream|
                // Syntax: key = value
                Ok((input.parse::<Ident>()?, {
                    input.parse::<Token![=]>()?;
                    input.parse()?
                })), tt
            ) {
                Ok((key, value)) =>
                    // Error when a property is assigned more than once
                    if properties.contains_key(&key) {
                        errors.push(syn::Error::new(key.span(), "Already used property \"{key}\""))
                    } else {
                        properties.insert(key, value);
                    },
                Err(error) => errors.push(error)
            }
        }

        merge_errors(errors)?;

        Ok(Self {
            probed: RefCell::new(HashSet::new()),
            properties: RefCell::new(properties.into_iter()
                .map(|(key, val)| (key.to_string(), val))
                .collect()
            ),
        })
    }
}

/// Find the JObject field with a lifetime, and use that lifetime's name for the JNIEnv lifetime,
/// Defaults to `'local` if such object could not be found, and **appends** the lifetime to the *generics*.
///
/// `P` is just the punctuation type; don't worry about it.
fn get_local_lifetime<P: Default>(item: Either<&ItemStruct, &ItemEnum>, generics: &mut Punctuated<GenericParam, P>) -> Lifetime {
    static DEFAULT: &str = "local";
    /// Find the lifetiem in the struct/enum's generics
    fn find_local(generics: &Generics) -> Option<Lifetime> {
        generics.lifetimes()
            .map(|lt| &lt.lifetime)
            .find(|lt| lt.ident.to_string() == DEFAULT)
            .map(Clone::clone)
    }
    /// Find a field that has a lifetime in its generics
    fn find_in_fields(fields: &Fields) -> Option<Lifetime> {
        fields.iter()
            .find_map(|field| match &field.ty {
                syn::Type::Path(TypePath { path, .. } ) if path.segments.last()
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

    /// Generate the default Lifetime: `'local`.
    /// Appends the lifetime to the generic params.
    /// The failure of [`find_local()`] indicates that there is no `'local` lifetime and it needs to be added.
    fn default<P: Default>(span: Span, generics: &mut Punctuated<GenericParam, P>) -> Lifetime {
        let default = Lifetime::new(&format!("'{DEFAULT}"), span);

        generics.push(syn::GenericParam::Lifetime(syn::LifetimeParam::new(default.clone())));

        default
    }
    
    match item {
        Either::Left(st) =>
            find_local(&st.generics)
                .or_else(|| find_in_fields(&st.fields))
                .unwrap_or_else(|| default(st.ident.span(), generics)),
        Either::Right(enm) =>
            find_local(&enm.generics)
                .or_else(||
                    enm.variants.iter()
                        .find_map(|variant| find_in_fields(&variant.fields))
                )
                .unwrap_or_else(|| default(enm.ident.span(), generics)),
    }
}

/// Content of the `field` attribute: *key-value pairs* inside a parenthesis.
/// * **name** - Use this name for the Object's field lookup instead of the field's name.
///                     Mutually exclusive with `call`.
/// * **call** - Instead of a looking up a *member*, call one of the Object's *method*s.
///                     Mutually exclusive with `name`.
/// * **class** - The Class of the object if the field is a `JObject`.
/// 
/// Either *name* or *call* MUST be used if the field belongs to a Tuple struct.
/// 
/// Example: `#[field(call = getInt, class = java.lang.Integer)] int: i32`
struct FieldAttr {
    name: Option<Ident>,
    call: Option<Ident>,
    class: Option<ClassAttr>,
}
impl FieldAttr {
    /// Find the `field` attribute in a struct's field and parse its content.
    /// 
    /// If there is no `field` attribute, then returns with everything set to [`None`].
    pub fn get_from_attrs(field: &Field) -> syn::Result<Self> {
        field.attrs
            .iter()
            .find(|&attr|
                attr.path()
                    .get_ident()
                    .is_some_and(|ident| ident.to_string() == "field")
            )
            .map_or_else(
                || Ok(Self { name: None, call: None, class: None }),
                |attr| AttributeProps::parse_attr_with(attr, |props| {
                    Ok(Self {
                        name: props.take("name")?,
                        call: props.take("call")?,
                        class: props.take("class")?,
                    })
                })
            )
    }
}
struct ClassAttr {
    dimensions: u32,
    outer_brackets: Option<Bracket>,
    class: Class,
}
impl ClassAttr {
    /// Ensures that the [`ClassAttr`] provided by the user matches the [`Type`] derived from the *field's* [`Rust Type`][syn::Type]
    pub fn match_ty(&self, ty: &Type) -> syn::Result<()> {
        match ty {
            // Number of dimensions in Array must match
            Type::Array(array)
            | Type::Option { ty: OptionType::Array(array), .. }
            if array.dimensions().get() != self.dimensions => Err(syn::Error::new(self.span(), format!(
                "Class Attribute type does not match the field's Type: Class is an array with {} dimensions, while the field Type is an array with {} dimensions",
                self.dimensions, array.dimensions()
            ))),
            _ => Ok(())
        }
    }
}
impl Spanned for ClassAttr {
    fn span(&self) -> Span {
        match self.outer_brackets {
            Some(brackets) => {
                let tokens = &mut TokenStream::new();
                brackets.surround(tokens, |tokens| {
                    tokens.append(Ident::new("a", self.class.span()));
                });
                tokens.span()
            },
            None => self.class.span()
        }
    }
}
impl SigType for ClassAttr {
    fn sig_char(&self) -> Ident {
        Ident::new("l", self.span())
    }
    fn sig_type(&self) -> LitStr {
        LitStr::new(&format!("{}{}", "[".repeat(self.dimensions as usize), self.class.sig_type().value()), self.span())
    }
}
impl Parse for ClassAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Parse the brackets (if any) and determine the number of dimensions
        let mut dimensions = 0;
        let mut outer_brackets = None;

        let mut current = input.cursor();
        while let Some((inner, span, next)) = current.group(Delimiter::Bracket) {
            current = inner;
            if !next.eof() {
                return Err(syn::Error::new(next.span(), "Unexpected tokens"));
            }

            dimensions += 1;
            if outer_brackets.is_none() {
                outer_brackets = Some(Bracket { span: span })
            }
        }
        // Once the next token is no longer a brackets, there should be a Class
        
        Ok(Self { dimensions, outer_brackets, class: input.parse()? })
    }
}

struct FieldType {
    ty: syn::Type,
    java_type: Option<Type>,
}
impl FieldType {
    pub fn new(ty: syn::Type) -> syn::Result<Self> {
        Ok(Self {
            java_type: Self::get_field_type(&ty)?,
            ty,
        })
    }
    /// Try to get a [`java Type`][Type] from a Rust [`Field`] [`Type`][syn::Type].
    /// Returns [`None`] if a [`java Type`][Type] could not be determined because the Rust [`Type`][syn::Type] contains an unknown type.
    /// 
    /// THIS IS A ***RECURSIVE*** FUNCTION
    fn get_field_type(field_ty: &syn::Type) -> syn::Result<Option<Type>> {
        /// Gets the generic arguments of a type (if any)
        fn get_generic_args<'a>(ty: &'a syn::PathSegment) -> syn::Result<Option<&'a syn::AngleBracketedGenericArguments>> {
            match &ty.arguments {
                syn::PathArguments::None => Ok(None),
                syn::PathArguments::AngleBracketed(args) => Ok(Some(args)),
                syn::PathArguments::Parenthesized(_) => Err(syn::Error::new(ty.span(), "Function traits are not allowed in generic arguments here"))
            }
        }

        /// Gets the inner type of a Type that has a generic type
        fn get_first_generic_ty(ty: &syn::PathSegment) -> syn::Result<&syn::Type> {
            let ty_name = ty.ident.to_string();
            let no_generics_error = syn::Error::new(ty.span(), format!("{ty_name} must have 1 generic argument"));

            match get_generic_args(ty)?
                .ok_or(no_generics_error.clone())?
                .args
                .first()
                .ok_or(no_generics_error.clone())?
            {
                syn::GenericArgument::Type(inner) => Ok(inner),
                generic_arg => Err(syn::Error::new(generic_arg.span(), format!("Generic argument in {ty_name} must be a Type")))
            }
        }

        match field_ty {
            syn::Type::BareFn(_) => todo!("support Callbacks"),
            syn::Type::Group(group) => Self::get_field_type(group.elem.as_ref()),
            syn::Type::Paren(group) => Self::get_field_type(group.elem.as_ref()),
            syn::Type::Array(array) => Ok(Some(Type::Array(ArrayType {
                bracket_token: array.bracket_token.clone(),
                ty: match Self::get_field_type(array.elem.as_ref())? {
                    Some(ty) => Box::new(ty),
                    None => return Ok(None),
                }
            }))),
            syn::Type::Path(path) => {
                // Only required to modify the outer layer. Then parse with Type
                let ty = path.path.segments.last().unwrap(); // last segment is the type

                match ty.ident.to_string().as_str() {
                    "Option" => {
                        let inner_ty = Self::get_field_type(get_first_generic_ty(ty)?)?;
                        let option_ty = match inner_ty {
                            Some(Type::Assertive(InnerType::Object(class))) => OptionType::Object(class),
                            Some(Type::Array(array)) => OptionType::Array(array),
                            Some(ty) => return Err(syn::Error::new(ty.span(), format!("Can't use {ty} as the inner Type of Option. Must be an Object or an Array."))),
                            None => return Ok(None),
                        };
                        Ok(Some(Type::Option { ident: ty.ident.clone(), ty: option_ty }))
                    }
                    "Vec" => {
                        let inner_ty = match Self::get_field_type(get_first_generic_ty(ty)?)? {
                            Some(ty) => ty,
                            None => return Ok(None),
                        };
                        let (open_span, close_span) = {
                            let generics = get_generic_args(ty).unwrap().unwrap();
                            (generics.lt_token.span(), generics.gt_token.span())
                        };
                        Ok(Some(Type::Array(ArrayType::new_spanned(open_span, close_span, inner_ty))))
                    },
                    "Box" => { // Boxed Slice
                        let (bracket_token, inner_ty) = match get_first_generic_ty(ty)? {
                            syn::Type::Slice(slice) => (slice.bracket_token, slice.elem.as_ref()),
                            _ => todo!("Support Box for any type")
                        };
                        let inner_ty = match Self::get_field_type(inner_ty)? {
                            Some(ty) => ty,
                            None => return Ok(None),
                        };
                        Ok(Some(Type::Array(ArrayType { bracket_token, ty: Box::new(inner_ty) })))
                    },
                    "JObject" => Ok(Some(Type::Assertive(InnerType::Object(Class::from_rust_type(ClassRustType::JObject, field_ty.span()))))),
                    "JClass" => Ok(Some(Type::Assertive(InnerType::Object(Class::from_rust_type(ClassRustType::JClass, field_ty.span()))))),
                    "JThrowable" => Ok(Some(Type::Assertive(InnerType::Object(Class::from_rust_type(ClassRustType::JThrowable, field_ty.span()))))),
                    // Try to parse the type. Return None if could not be parsed.
                    _ => Ok(syn::parse2::<Type>(field_ty.to_token_stream()).ok())
                }
            },
            // Try to parse the type. Return None if could not be parsed.
            syn::Type::Verbatim(field_ty) => Ok(syn::parse2::<Type>(field_ty.clone()).ok()),
            _ => Ok(None)
        }
    }
}
impl ToTokens for FieldType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.ty.to_tokens(tokens)
    }
}



/// Converts the value obtained from a *field access* or *getter call* to a *Rust value* using [`ez_jni::FromJValue`]
/// (or [`ez_jni::FromObject`] if **class_attr** is [`Some`]).
/// 
/// Passing a **class** will force the use of `FromObject` instead of `FromJValue`.
/// **class** is *required* if the type is [`JObject`][jni::objects::JObject] or [`JThrowable`][jni::objects::JThrowable].
fn convert_field_value(ty: &FieldType, class_attr: Option<&Class>, value: &TokenStream) -> syn::Result<TokenStream> {
    match &ty.java_type {
        // JObjects must use FromJValueOwned
        Some(Type::Assertive(InnerType::Object(class)))
        | Some(Type::Option { ty: OptionType::Object(class), .. })
        if class.is_jobject() => {
            let rust_type = class.rust_type();
            let rust_type_tokens = Ident::new(&rust_type.to_string(), class.span());
            match class.rust_type() {
                // JObject and JThrowable must use the *field::class* attribute
                ClassRustType::JObject | ClassRustType::JThrowable => match class_attr {
                    // Use FromObjectClass for JObject and JThrowable
                    Some(class) => Ok({
                        let class = class.to_jni_class_path();
                        quote_spanned! {value.span()=> <#rust_type_tokens ::ez_jni::utils::FromJValueClass>::from_jvalue_class(#value, #class, env) }
                    }),
                    None => Err(syn::Error::new(ty.span(), "Field must have \"class\" property if it is JObject"))
                },
                _ => Ok(quote_spanned! {value.span()=> <#rust_type_tokens ::ez_jni::FromJValueOwned>::from_jvalue_owned_env(#value, env) })
            }
        }
        // Everything else uses the regular FromJValue or FromObject
        _ => Ok(match class_attr {
            // Use FromObject
            Some(_) => quote_spanned! {ty.span() =>
                <#ty as ::ez_jni::FromObject>::from_object_env(
                    <&JObject<'_> as ::ez_jni::FromJValue>::from_jvalue_env((#value).borrow(), env),
                env).unwrap()
            },
            // Use FromJValue
            None => quote_spanned! {ty.span()=> <#ty as ::ez_jni::FromJValue>::from_jvalue_env((#value).borrow(), env) }
        })
    }
}

/// Builds a struct *constructor literal*, where all its fields are initialized with JNI calls to *read fields* or *call getter methods*.
/// Can handle *Unit, Tuple, and Named structs*.
/// Can also be used for enum variants.
/// 
/// The *type* and *name* of the field will be used to create a JNI call to the Object which will be converted from.
/// By default, the **name** will be used to get the value from a *member of the Object*,
/// and the **type** will be converted from a Java Object if it implements `ez_jni::Class`.
/// 
/// **class** is the Java Class (in *slash-separated* form) of the struct/enum.
/// 
/// See [`FieldAttr`] for syntax.
fn struct_constructor(fields: &Fields) -> syn::Result<TokenStream> {    
    let mut errors = Vec::new();
    
    // Produce the value that will be assigned for each field
    let values = fields.iter()
        .map(|field| {
            let attr = FieldAttr::get_from_attrs(field)?;
            let field_ty = FieldType::new(field.ty.clone())?;
            let class_attr = attr.class
                .as_ref()
                .map(|attr| &attr.class);
            // The type signature of the Java field.
            // Is determined by either the class attribute or the field_ty (in that order).
            let sig_ty = match &attr.class {
                Some(class_attr) => {
                    // Ensure the class attribute matches the Type of the Rust field
                    if let Some(field_ty) = &field_ty.java_type {
                        class_attr.match_ty(field_ty)?;
                    }
                    class_attr.sig_type()
                },
                None => todo!("use guess_jvalue_type()"),
            };

            let get_field = |name: String| {
                convert_field_value(&field_ty, class_attr, &quote_spanned! {field.span()=>
                    ::ez_jni::utils::from_object_get_field(&object, #name, #sig_ty, env)?
                })
            };
            
            if let Some(name) = attr.name {
                // Use the "name" of the field attribute
                get_field(name.to_string())
            } else if let Some(name) = &field.ident {
                // Get name from Rust field convert it to camelCase for the Java field
                get_field(name.to_string().to_case(Case::Camel))
            } else if let Some(call) = attr.call {
                // Call the Java method
                let method = LitStr::new(&call.to_string(), call.span());
                let sig = format!("(){}", sig_ty.value());
                
                convert_field_value(&field_ty, class_attr, &quote_spanned! {method.span()=>
                    ::ez_jni::utils::call_obj_method(&object, #method, #sig, &[], env)
                        .unwrap_or_else(|exception| ::ez_jni::__throw::panic_exception(exception))
                })
            } else {
                Err(syn::Error::new(field.span(), "Field must have \"name\" or \"call\" properties if it is unnamed. See the 'field' attribute."))
            }
        })
        .filter_map(|res| res.map_err(|err| errors.push(err)).ok())
        .collect::<Box<[_]>>();
    
    merge_errors(errors)?;
    
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

/// Creates constructor literals for each of the *enum's variants*,
/// checking that the object is the right class.
/// 
/// The returned results can be [`filter_maped`][`Iterator::filter_map()`],
/// stripping out the error by pushing them to an *error [`Vec`]*.
/// ```ignore
/// .filter_map(|res| res.map_err(|err| errors.push(err)).ok())
/// ```
/// 
/// The constructor literal is built by [`struct_constructor()`].
fn construct_variants<'a>(variants: impl Iterator<Item = &'a mut Variant> + 'a) -> impl Iterator<Item = syn::Result<TokenStream>> + 'a {
    variants.enumerate()
        .map(|(i, variant)| {
            // Get class name for this variant
            let class = take_class_attribute_required(&mut variant.attrs, variant.ident.span())?
                .to_jni_class_path();

            let ident = &variant.ident;
            // Get a constructor for this variant
            let ctor = struct_constructor(&variant.fields)?;
            let _if = if i == 0 { quote!(if) } else { quote!(else if) };
            // Check if Exception is the class that this Variant uses, and construct the variant
            Ok(quote_spanned! {variant.span()=>
                #_if env.is_instance_of(object, #class).unwrap() {
                    Ok(Self::#ident #ctor)
                }
            })
        })
}
