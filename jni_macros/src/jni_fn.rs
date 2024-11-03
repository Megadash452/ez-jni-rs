use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{braced, parenthesized, parse::{Parse, ParseStream}, punctuated::Punctuated, token::{Brace, Paren}, Attribute, GenericParam, Generics, Ident, LifetimeParam, LitStr, Token};
use crate::{
    types::{Class, InnerType, RustPrimitive, SigType, Type}, utils::{gen_signature, merge_errors, take_class_attribute_required, Spanned}
};

/// Processes the input for [`crate::jni_fns`].
/// Converts the parsed [`JniFn`] to a regular function used in Rust.
pub fn jni_fn(input: ParseStream) -> syn::Result<Vec<TokenStream>> {
    let mut inputs = Vec::new();

    // Parse multiple jni_fn
    while !input.is_empty() {
        inputs.push(input.parse::<JniFn>()?.to_token_stream());
    }

    // Convert all JniFn to ItemFn 
    Ok(inputs)
}

// TODO: Allow generics in the arguments and return type if they are a Java Class

/// A rust function that uses Java types (or some Rust types) and is called by Java Code.
/// 
/// A [`JniFn`] is one that MUST be exported by the user library.
pub struct JniFn {
    pub attrs: Vec<Attribute>,
    /// The Java Class the function is a method of.
    /// This takes the form of an attribute that is removed after parsing.
    class: Class,
    pub pub_token: Token![pub],
    pub fn_token: Token![fn],
    pub name: Ident,
    pub lt_token: Token![<],
    pub lifetime: LifetimeParam,
    pub gt_token: Token![>],
    pub paren_token: Paren,
    pub inputs: Punctuated<JniFnArg, Token![,]>,
    // pub variadic: Option<Variadic>, Should this be allowed?
    pub output: JniReturn,
    pub brace_token: Brace,
    pub content: TokenStream,
}
impl Parse for JniFn {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        static LIFETIME_ERROR: &str = "jni_fn must have one and only one lifetime, named \"local\"";

        // -- Parse input function. Collect multiple errors
        let mut errors = Vec::new();

        let attrs = input.call(Attribute::parse_outer)
            .map_err(|err| errors.push(err))
            .ok();

        // Parse `pub`
        let pub_token = input.parse::<Token![pub]>()
            .map_err(|err| errors.push(
                syn::Error::new(err.span(), format!("{err}; jn_fn must have `pub` because they must be exported by the library"))
            ))
            .ok();

        // Parse `fn`
        let fn_token = input.parse::<Token![fn]>()
            .map_err(|err| errors.push(err))
            .ok();

        // Parse function name
        let name = input.parse::<Ident>()
            .map_err(|err| errors.push(syn::Error::new(err.span(), format!("{err}; Expected function name"))))
            .ok();

        // Parse all generics, but will only accept one lifetime
        let generics = input.parse::<Generics>()
            .map_err(|err| errors.push(err))
            .ok()
            .and_then(|generics| {
                let lt_token = match generics.lt_token {
                    Some(lt_token) => Some(lt_token),
                    None => {
                        errors.push(syn::Error::new(Span::call_site(), format!("Must have a generic lifetime")));
                        None
                    },
                }?;
                Some((lt_token, generics.params, generics.gt_token?))
            });

        // Parse arguments with Java types
        let (paren_token, inputs) = {
            let inner;
            (
                parenthesized!(inner in input),
                Punctuated::<JniFnArg, Token![,]>::parse_terminated(&inner)
                    .map_err(|err| errors.push(err))
                    .ok()
            )
        };

        // Parse return arrow `->` and return type (is void if there is none)
        let output = input.parse::<JniReturn>()
            .map_err(|err| errors.push(err))
            .ok();

        // Parse the content of the function inside the braces `{ ... }`
        let (brace_token, content) = {
            let inner;
            (braced!(inner in input), inner.parse()?)
        };

        merge_errors(errors)?;
        // If there were any errors, the function returned on the line above, so reaching here means all tokens were parsed successfully.
        let mut attrs = attrs.unwrap();
        let pub_token = pub_token.unwrap();
        let fn_token = fn_token.unwrap();
        let name = name.unwrap();
        let (lt_token, generics, gt_token) = generics.unwrap();
        let inputs = inputs.unwrap();
        let output = output.unwrap();

        // -- Perform checks on the successfully parsed function
        errors = Vec::new();

        // jni_fn must have a `class` attribute
        let class = take_class_attribute_required(&mut attrs, name.span())
            .map_err(|err| errors.push(err))
            .ok();

        let mut iter = generics.into_iter();

        // Parsed all generics, but will only accept one lifetime, and no other generic types
        let lifetime = match iter.next() {
            Some(GenericParam::Lifetime(lifetime)) =>
                if lifetime.lifetime.ident.to_string() != "local" {
                    errors.push(syn::Error::new(lifetime.span(), LIFETIME_ERROR));
                    None
                } else {
                    Some(lifetime)
                },
            Some(generic) => {
                errors.push(syn::Error::new(generic.span(), "jni_fn can't have generic constants or types"));
                None
            },
            None => {
                errors.push(syn::Error::new(name.span(), LIFETIME_ERROR));
                None
            },
        };

        // Only allow 1 lifetime
        if let Some(lifetime) = iter.next() {
            errors.push(syn::Error::new(lifetime.span(), LIFETIME_ERROR))
        }

        // Check that the function doesn't have arguments named "env" or "_class" because those are implicitly added
        for arg_name in ["env", "_class"] {
            if let Some(arg) = inputs.iter().find(|arg| arg.name.to_string() == arg_name) {
                errors.push(syn::Error::new(
                    arg.span(),
                    format!("Function can't have an argument named \"{arg_name}\"; this is added implicitly")
                ))
            }
        }

        merge_errors(errors)?;

        let class = class.unwrap();
        let lifetime = lifetime.unwrap();

        Ok(Self { attrs, class, pub_token, fn_token, name, lt_token, lifetime, gt_token, paren_token, inputs, output, brace_token, content })
    }
}
impl ToTokens for JniFn {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        #[allow(unused_variables)] let a = 0;
        // The real name of the function as it will be used by Java
        let name = {
            let class = self.class.to_string()
                .replace('.', "_");
            let name = self.name.to_string().replace('_', "_1");
    
            Ident::new(&format!("Java_{class}_{name}"), self.name.span())
        };

        // Build a java method signature, something like (Ljava.lang.String;)I
        let method_sig = gen_signature(self.inputs.iter().map(|i| &i.ty), &self.output).value();

        tokens.append_all(&self.attrs);
        tokens.append_all(quote!(#[doc = ""]));
        tokens.append_all(quote!(#[doc = #method_sig]));
        tokens.append_all(quote!(#[no_mangle]));
        self.pub_token.to_tokens(tokens);
        tokens.append_all(quote!(extern "system"));
        self.fn_token.to_tokens(tokens);
        name.to_tokens(tokens);
        self.lt_token.to_tokens(tokens);
        self.lifetime.to_tokens(tokens);
        self.gt_token.to_tokens(tokens);
        self.paren_token.surround(tokens, |tokens| {
            tokens.append_all(quote_spanned!(self.paren_token.span.span()=> mut env: ::jni::JNIEnv<'local>, _class: ::jni::objects::JClass<'local>,));
            self.inputs.to_tokens(tokens);
        });
        self.output.to_tokens(tokens);
        self.brace_token.surround(tokens, |tokens| {
            let mut content = TokenStream::new();
            self.brace_token.surround(&mut content, |tokens| {
                self.content.to_tokens(tokens);
            });
            tokens.append_all(quote_spanned! {self.brace_token.span=> ::ez_jni::__throw::catch_throw(&mut env, move |#[allow(unused_variables)] env| #content) });
        });
    }
}

pub struct JniFnArg {
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub ty: Type
}
impl Parse for JniFnArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            attrs: Attribute::parse_outer(input)?,
            name: input.parse()?,
            ty: {
                input.parse::<Token![:]>()?;
                input.parse()?
            }
        })
    }
}
impl ToTokens for JniFnArg {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(&self.attrs);
        self.name.to_tokens(tokens);
        tokens.append(proc_macro2::Punct::new(':', proc_macro2::Spacing::Alone));
        
        // Convert Class to JObject (java.lang.String to JString) and leave primitives alone
        tokens.append_all(match &self.ty {
            Type::Assertive(InnerType::RustPrimitive { ident, ty }) => Ident::new(&ty.to_string(), ident.span()).into_token_stream(),
            Type::Assertive(InnerType::JavaPrimitive { ident, ty }) => Ident::new(&RustPrimitive::from(*ty).to_string(), ident.span()).into_token_stream(),
            Type::Assertive(InnerType::Object(class)) if class.to_string() == "java.lang.String" => quote_spanned! {class.span()=> ::jni::objects::JString<'local>},
            Type::Assertive(InnerType::Object(_))
            | Type::Array(_)
            | Type::Option { .. } => quote_spanned! {self.ty.span()=> ::jni::objects::JObject<'local>},
        })
    }
}

pub enum JniReturn {
    Void,
    Type(Type)
}
impl SigType for JniReturn {
    fn sig_char(&self) -> Ident {
        match self {
            Self::Void => Ident::new("v", Span::call_site()),
            Self::Type(ty) => ty.sig_char()
        }
    }
    fn sig_type(&self) -> LitStr {
        match self {
            Self::Void => LitStr::new("V", Span::call_site()),
            Self::Type(ty) => ty.sig_type()
        }
    }
}
impl Parse for JniReturn {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Parse return arrow `->` and return type (is void if there is none)
        Ok(if input.parse::<Token![->]>().is_ok() {
            Self::Type(input.parse()?)
        } else {
            Self::Void
        })
    }
}
impl ToTokens for JniReturn {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Void => { }, // Void return has no tokens
            Self::Type(output) => {
                // Convert Class to *jobject (or *jstring) and leave primitives alone
                let ty = match output {
                    Type::Assertive(InnerType::RustPrimitive { ident, ty }) => Ident::new(&ty.to_string(), ident.span()).into_token_stream(),
                    Type::Assertive(InnerType::JavaPrimitive { ident, ty }) => Ident::new(&RustPrimitive::from(*ty).to_string(), ident.span()).into_token_stream(),
                    Type::Assertive(InnerType::Object(class)) if class.to_string() == "java.lang.String" => quote_spanned! {class.span()=> ::jni::sys::jstring},
                    Type::Assertive(InnerType::Object(_))
                    | Type::Array(_)
                    | Type::Option { .. } => quote_spanned! {output.span()=> ::jni::sys::jobject},
                };
                tokens.append_all(quote_spanned!(output.span()=> -> #ty))
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jni_fn_test() {
        syn::parse_str::<JniFn>("#[class = \"me.test.Test\"] pub fn test_jni_fn_1<'local>() { }")
            .inspect(|f| { assert_eq!(f.to_token_stream().to_string(),
                "# [doc = \"\"] # [doc = \"()V\"] # [no_mangle] pub extern \"system\" fn Java_me_test_Test_test_1jni_1fn_11 < 'local > (mut env : :: jni :: JNIEnv < 'local > , _class : :: jni :: objects :: JClass < 'local > ,) { :: ez_jni :: __throw :: catch_throw (& mut env , move | env | { }) }"
            )})
            .unwrap();
        syn::parse_str::<JniFn>("#[class(me.test.Test)] pub fn test_jni_fn_2<'local>(s: java.lang.String) -> int { String::from_object(&s, env).unwrap().len() as i32 }")
            .inspect(|f| { assert_eq!(f.to_token_stream().to_string(),
                "# [doc = \"\"] # [doc = \"(Ljava/lang/String;)I\"] # [no_mangle] pub extern \"system\" fn Java_me_test_Test_test_1jni_1fn_12 < 'local > (mut env : :: jni :: JNIEnv < 'local > , _class : :: jni :: objects :: JClass < 'local > , s : :: jni :: objects :: JString < 'local >) -> i32 { :: ez_jni :: __throw :: catch_throw (& mut env , move | env | { String :: from_object (& s , env) . unwrap () . len () as i32 }) }"
            )})
            .unwrap();
        syn::parse_str::<JniFn>("#[class(me.test.Test)] pub fn test_jni_fn_3<'local>(s: java.lang.String) -> java.lang.String { String::from_object(&s, env).unwrap().to_object(env).into_raw() }")
            .inspect(|f| { assert_eq!(f.to_token_stream().to_string(),
                "# [doc = \"\"] # [doc = \"(Ljava/lang/String;)Ljava/lang/String;\"] # [no_mangle] pub extern \"system\" fn Java_me_test_Test_test_1jni_1fn_13 < 'local > (mut env : :: jni :: JNIEnv < 'local > , _class : :: jni :: objects :: JClass < 'local > , s : :: jni :: objects :: JString < 'local >) -> :: jni :: sys :: jstring { :: ez_jni :: __throw :: catch_throw (& mut env , move | env | { String :: from_object (& s , env) . unwrap () . to_object (env) . into_raw () }) }"
            )})
            .unwrap();
        syn::parse_str::<JniFn>("#[class(me.test.Test)] pub fn test_jni_fn_4<'local>(s: [String]) { }")
            .inspect(|f| { assert_eq!(f.to_token_stream().to_string(),
                "# [doc = \"\"] # [doc = \"([Ljava/lang/String;)V\"] # [no_mangle] pub extern \"system\" fn Java_me_test_Test_test_1jni_1fn_14 < 'local > (mut env : :: jni :: JNIEnv < 'local > , _class : :: jni :: objects :: JClass < 'local > , s : :: jni :: objects :: JObject < 'local >) { :: ez_jni :: __throw :: catch_throw (& mut env , move | env | { }) }"
            )})
            .unwrap();
        syn::parse_str::<JniFn>("#[class(me.test.Test)] pub fn test_jni_fn_5<'local>() -> [String] { }")
            .inspect(|f| { assert_eq!(f.to_token_stream().to_string(),
                "# [doc = \"\"] # [doc = \"()[Ljava/lang/String;\"] # [no_mangle] pub extern \"system\" fn Java_me_test_Test_test_1jni_1fn_15 < 'local > (mut env : :: jni :: JNIEnv < 'local > , _class : :: jni :: objects :: JClass < 'local > ,) -> :: jni :: sys :: jobject { :: ez_jni :: __throw :: catch_throw (& mut env , move | env | { }) }"
            )})
            .unwrap();
        syn::parse_str::<JniFn>("#[class(me.test.Test)] pub fn test_jni_fn_6<'local>(s: Option<String>) { }")
            .inspect(|f| { assert_eq!(f.to_token_stream().to_string(),
                "# [doc = \"\"] # [doc = \"(Ljava/lang/String;)V\"] # [no_mangle] pub extern \"system\" fn Java_me_test_Test_test_1jni_1fn_16 < 'local > (mut env : :: jni :: JNIEnv < 'local > , _class : :: jni :: objects :: JClass < 'local > , s : :: jni :: objects :: JObject < 'local >) { :: ez_jni :: __throw :: catch_throw (& mut env , move | env | { }) }"
            )})
            .unwrap();
        syn::parse_str::<JniFn>("#[class(me.test.Test)] pub fn test_jni_fn_7<'local>() -> Option<String> { }")
            .inspect(|f| { assert_eq!(f.to_token_stream().to_string(),
                "# [doc = \"\"] # [doc = \"()Ljava/lang/String;\"] # [no_mangle] pub extern \"system\" fn Java_me_test_Test_test_1jni_1fn_17 < 'local > (mut env : :: jni :: JNIEnv < 'local > , _class : :: jni :: objects :: JClass < 'local > ,) -> :: jni :: sys :: jobject { :: ez_jni :: __throw :: catch_throw (& mut env , move | env | { }) }"
            )})
            .unwrap();
        // syn::parse_str::<JniFn>("#[class(me.test.Test)] pub fn test_jni_fn_8<'local>() -> Result<String, java.lang.Exception> { }")
        //     .inspect(|f| { assert_eq!(f.to_token_stream().to_string(),
        //         ""
        //     )})
        //     .unwrap();
        // syn::parse_str::<JniFn>("#[class(me.test.Test)] pub fn test_jni_fn_9<'local>() -> Result<Option<String>, java.lang.Exception> { }")
        //     .inspect(|f| { assert_eq!(f.to_token_stream().to_string(),
        //         ""
        //     )})
        //     .unwrap();
    }
}
