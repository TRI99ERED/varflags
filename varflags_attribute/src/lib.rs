//! Crate containing attribute procedural macro `varflags`. This crate isn't meant to be used without
//! `varflags` crate.

extern crate proc_macro;

use bitworks::{bitset::Bitset, bitset128::Bitset128};
use proc_macro2::Ident;
use quote::quote;
use syn::{
    braced, parse::Parse, parse_macro_input, spanned::Spanned, AttrStyle, Expr, Fields, Lit, Meta,
    Token, Variant, Visibility,
};

static NO_GENERICS: &'static str = "generics aren't allowed";
static NO_ZERO_VAR: &'static str = "enum should have more than 0 variants";
static UNIT_ONLY: &'static str = "only unit-like enum variants are allowed";
static ONE_VAR_ATTR: &'static str = "only 1 attribute is allowed per enum variant";
static NO_INNER_VAR_ATTRS: &'static str = "only outer attributes are allowed for enum variants";
static SINGLE_WORD_ATTR_NAME: &'static str = "enum variant attribute name should be a single word";
static NO_SIGNED_DISCR: &'static str = "variant discriminant should be unsigned";
static BAD_FLAG_ATTR_ARG: &'static str = "bad \"flag\" attribute argument, expected an unsigned integer with 1 set bit in binary representation (power of 2)";
static BAD_SHIFT_ATTR_ARG: &'static str =
    "bad \"shift\" attribute argument, expected an integer in range 0..=127";
static UNKNOWN_VAR_ATTR: &'static str =
    "unknown variant attribute, expected either \"flag\" or \"shift\"";
static BAD_VAR_ATTR_TYPE: &'static str =
    "bad variant attribute type, expected name-value pair in format #[name = value]";
static ONE_SET_BIT: &'static str = "exactly 1 bit should be set in a variant discriminant";
static NAN_DISCR: &'static str = "variant discriminant should be an unsigned integer";
static BAD_DISCR: &'static str = "expected unsigned discriminant expression";
static DISCR_GEN_ERROR: &'static str = "INTERNAL ERROR: couldn't generate discriminants";
static UNSET_DISCR_ERROR: &'static str = "INTERNAL ERROR: discriminants should be set";
static INVALID_DECL_ERROR: &'static str = "INTERNAL ERROR: variant declaration should be valid";
static INVALID_MATC_ERROR: &'static str = "INTERNAL ERROR: variant match should be valid";
static INVALID_MOD_ERROR: &'static str = "INTERNAL ERROR: mod name should be valid";
static INVALID_STRUCT_NAME: &'static str = "INTERNAL ERROR: struct name should be valid";
static INVALID_DOC_COMMENT: &'static str = "INTERNAL ERROR: doc comment should be valid";

/// Attribute marking an enum declaration as input for Varflags.
///
/// This attribute requires a unit-only enum with more than 0 variants and all of the variants'
/// discriminants should have exactly one bit set. It can't have more than 128 variants.
///
/// It does exactly this list of things:
/// * Derives [`Clone`] for the enum. It is required, so you don't have to derive or implement it manually.
/// * Adds `#[repr]` attribute for the enum which can be set to either [`u8`], [`u16`], [`u32`], [`u64`] or [`u128`].<br/>
/// Keep in mind, that `#[repr(u128)]` is currently unstable.
/// * Consumes `#[flag]` and `#[shift]` variant attributes and sets their variants' discriminants accordingly.
/// * Calculates and asigns unused discriminants for variants wihout explicit discriminants or attributes.
/// * Creates a private module called after enum name converted to snake case with _varflags appended.
/// * Generates [`From`] implementation for representing (unsigned integer) type from the enum.
/// * Generates [`Not`][core::ops::Not], [`BitAnd`][core::ops::BitAnd], [`BitOr`][core::ops::BitOr] and
/// [`BitXor`][core::ops::BitXor] for the enum, with output being `Varflags` struct, with `E` being input enum
/// and `B` being one of the built-in [`bitworks`] bitsets with correct representation.
/// * Generates [`TryFrom`] implementation for input enum from [`bitworks::index::Index`] (for the correct `Bitset`).
/// * Generates [`core::fmt::Display`] for input enum, which just outputs a stringified variant name.
/// * Re-exports a type alias for `Varflags` specific for input enum named as the name of input enum's name with
/// "Varflags" appended. The re-export will have the same visibility as the enum.
#[proc_macro_attribute]
pub fn varflags(
    _: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    varflags_impl(item)
}

#[derive(Clone)]
struct VariantData {
    idents: Vec<String>,
    discriminants: Vec<Option<u128>>,
}

struct EnumData {
    vis: Visibility,
    name: Ident,
    count: usize,
    variant_data: VariantData,
}

impl Parse for EnumData {
    #[inline(always)]
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.call(syn::Attribute::parse_outer)?;

        let vis = input.parse::<Visibility>()?;
        
        input.parse::<Token![enum]>()?;
        let name = input.parse::<Ident>()?;

        let _generics = input.parse::<syn::Generics>()?;
        if !_generics.params.is_empty() {
            return Err(syn::Error::new(_generics.span(), NO_GENERICS));
        }
        if let Some(w) = input.parse::<Option<syn::WhereClause>>()? {
            return Err(syn::Error::new(w.span(), NO_GENERICS));
        }
        let content;
        braced!(content in input);
        let variants = content.parse_terminated(Variant::parse, Token![,])?;

        let count = variants.len();
        if count == 0 {
            return Err(syn::Error::new(variants.span(), NO_ZERO_VAR));
        }

        if !variants.iter().all(|v| {
            if let Fields::Unit = v.fields {
                true
            } else {
                false
            }
        }) {
            return Err(syn::Error::new(variants.span(), UNIT_ONLY));
        }

        let mut idents = Vec::with_capacity(count);
        let mut discriminants = Vec::with_capacity(count);
        let mut used_discrs = Bitset128::new(0);
        for variant in variants {
            let ident = variant.ident.to_string();
            if idents.contains(&ident) {
                return Err(syn::Error::new(
                    variant.ident.span(),
                    format!("the name `{}` is defined multiple times", ident),
                ));
            }
            idents.push(ident);

            let mut attr_discriminant = None;
            if variant.attrs.len() > 1 {
                return Err(syn::Error::new(variant.span(), ONE_VAR_ATTR));
            }
            if variant.attrs.len() == 1 {
                let attribute = &variant.attrs[0];
                if let AttrStyle::Inner(_) = attribute.style {
                    return Err(syn::Error::new(variant.span(), NO_INNER_VAR_ATTRS));
                }
                match &attribute.meta {
                    Meta::NameValue(nv) => {
                        match nv
                            .path
                            .get_ident()
                            .expect(SINGLE_WORD_ATTR_NAME)
                            .to_string()
                            .as_str()
                        {
                            "flag" => match &nv.value {
                                Expr::Lit(lit) => match &lit.lit {
                                    Lit::Int(lit_int) => {
                                        attr_discriminant =
                                            Some(lit_int.base10_parse::<u128>().map_err(|_| {
                                                syn::Error::new(lit_int.span(), NO_SIGNED_DISCR)
                                            })?);
                                    }
                                    _ => {
                                        return Err(syn::Error::new(lit.span(), BAD_FLAG_ATTR_ARG))
                                    }
                                },
                                _ => return Err(syn::Error::new(nv.span(), BAD_FLAG_ATTR_ARG)),
                            },
                            "shift" => match &nv.value {
                                Expr::Lit(lit) => match &lit.lit {
                                    Lit::Int(lit_int) => {
                                        let shift =
                                            lit_int.base10_parse::<usize>().map_err(|_| {
                                                syn::Error::new(lit_int.span(), NO_SIGNED_DISCR)
                                            })?;
                                        if shift > 127 {
                                            return Err(syn::Error::new(
                                                lit.span(),
                                                BAD_SHIFT_ATTR_ARG,
                                            ));
                                        }
                                        attr_discriminant = Some(1 << shift);
                                    }
                                    _ => {
                                        return Err(syn::Error::new(lit.span(), BAD_SHIFT_ATTR_ARG))
                                    }
                                },
                                _ => return Err(syn::Error::new(nv.span(), BAD_SHIFT_ATTR_ARG)),
                            },
                            _ => return Err(syn::Error::new(nv.span(), UNKNOWN_VAR_ATTR)),
                        }
                    }
                    _ => return Err(syn::Error::new(attribute.meta.span(), BAD_VAR_ATTR_TYPE)),
                }
            }

            match variant.discriminant {
                Some((_, expr)) => match expr {
                    Expr::Lit(lit) => match lit.lit {
                        Lit::Int(lit_int) => {
                            let discriminant = lit_int
                                .base10_parse::<u128>()
                                .map_err(|_| syn::Error::new(lit_int.span(), NO_SIGNED_DISCR))?;
                            if discriminant.count_ones() != 1 {
                                return Err(syn::Error::new(lit_int.span(), ONE_SET_BIT));
                            }
                            let bitmask = Bitset128::new(discriminant);
                            if used_discrs.includes(&bitmask) {
                                return Err(syn::Error::new(
                                    lit_int.span(),
                                    format!(
                                        "discriminant value `{}` assigned more than once",
                                        discriminant
                                    ),
                                ));
                            }
                            used_discrs.include(bitmask);
                            discriminants.push(Some(discriminant));
                        }
                        _ => return Err(syn::Error::new(lit.span(), NAN_DISCR)),
                    },
                    _ => return Err(syn::Error::new(expr.span(), BAD_DISCR)),
                },
                None => match attr_discriminant {
                    Some(discriminant) => {
                        if discriminant.count_ones() != 1 {
                            return Err(syn::Error::new(variant.span(), ONE_SET_BIT));
                        }
                        let bitmask = Bitset128::new(discriminant);
                        if used_discrs.includes(&bitmask) {
                            return Err(syn::Error::new(
                                variant.span(),
                                format!(
                                    "discriminant value `{}` assigned more than once",
                                    discriminant
                                ),
                            ));
                        }
                        used_discrs.include(bitmask);
                        discriminants.push(Some(discriminant))
                    }
                    None => discriminants.push(None),
                },
            };
        }

        for i in 0..count {
            if discriminants[i].is_none() {
                let mut found_value = None;
                for j in 0..count {
                    let discriminant: u128 = 1 << j;
                    if !used_discrs.includes(&Bitset128::new(discriminant)) {
                        used_discrs.include(Bitset128::new(discriminant));
                        found_value = Some(discriminant);
                        break;
                    }
                }

                if let Some(value) = found_value {
                    discriminants[i] = Some(value);
                } else {
                    panic!("{DISCR_GEN_ERROR}");
                }
            }
        }
        Ok(EnumData {
            vis,
            name,
            count,
            variant_data: VariantData {
                idents,
                discriminants,
            },
        })
    }
}

#[inline(always)]
fn varflags_impl(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let EnumData {
        vis,
        name,
        count,
        variant_data,
    } = parse_macro_input!(item as EnumData);

    let variant_declaration = variant_declaration(variant_data.clone(), count);
    let mod_name = make_mod_name(name.clone());
    let max_discriminant = *variant_data
        .discriminants
        .iter()
        .flatten()
        .max()
        .expect("enum variants should have one biggest discriminant");
    let (bitset, repr) = bitset_repr(max_discriminant);
    let struct_name = make_struct_name(name.clone());
    let try_from_match = try_from_match(variant_data.clone(), count);
    let display_match = display_match(variant_data, count);
    let doc_comment = doc_comment(name.clone());

    quote! {
        #[derive(Clone)]
        #[repr(#repr)]
        #vis enum #name {
            #variant_declaration
        }

        mod #mod_name {
            use varflags::error::ReprToFlagError;
            use bitworks::index::Index;
            use bitworks::prelude::#bitset as Inner;
            type Repr = #repr;

            use super::#name as E;

            #doc_comment
            pub type #struct_name = varflags::Varflags<E, Inner>;

            impl From<E> for Repr {
                #[inline(always)]
                fn from(value: E) -> Self {
                    value as Repr
                }
            }

            impl core::ops::Not for E {
                type Output = #struct_name;

                #[inline(always)]
                fn not(self) -> Self::Output {
                    #struct_name::_from_inner(!Inner::new(self as Repr))
                }
            }

            impl core::ops::BitAnd for E {
                type Output = #struct_name;

                #[inline(always)]
                fn bitand(self, rhs: Self) -> Self::Output {
                    #struct_name::_from_inner(Inner::new(self as Repr) & Inner::new(rhs as Repr))
                }
            }

            impl core::ops::BitOr for E {
                type Output = #struct_name;

                #[inline(always)]
                fn bitor(self, rhs: Self) -> Self::Output {
                    #struct_name::_from_inner(Inner::new(self as Repr) | Inner::new(rhs as Repr))
                }
            }

            impl core::ops::BitXor for E {
                type Output = #struct_name;

                #[inline(always)]
                fn bitxor(self, rhs: Self) -> Self::Output {
                    #struct_name::_from_inner(Inner::new(self as Repr) ^ Inner::new(rhs as Repr))
                }
            }

            impl TryFrom<Index<Inner>> for E {
                type Error = ReprToFlagError<Repr>;

                #[inline(always)]
                fn try_from(value: Index<Inner>) -> Result<Self, Self::Error> {
                    let n: Repr = 1 << value.into_inner();
                    match n {
                        #try_from_match
                        _ => Err(Self::Error::new(n)),
                    }
                }
            }

            impl core::fmt::Display for E {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}", match *self {
                        #display_match
                    })
                }
            }
        }

        #vis use #mod_name::#struct_name;
    }
    .into()
}

fn doc_comment(name: Ident) -> proc_macro2::TokenStream {
    format!("/// [`Varflags`] specific for [`{}`].", name)
        .parse()
        .expect(INVALID_DOC_COMMENT)
}

#[inline(always)]
fn variant_declaration(data: VariantData, count: usize) -> proc_macro2::TokenStream {
    let mut s = "".to_owned();
    for i in 0..count {
        s.push_str(&format!(
            "{} = {},",
            data.idents[i],
            data.discriminants[i].expect(UNSET_DISCR_ERROR)
        ));
    }
    s.parse().expect(INVALID_DECL_ERROR)
}

#[inline(always)]
fn make_mod_name(enum_name: Ident) -> proc_macro2::TokenStream {
    let s = format!("{}_varflags", upper_camel_to_snake(&enum_name.to_string()));
    s.parse().expect(INVALID_MOD_ERROR)
}

#[inline(always)]
fn upper_camel_to_snake(upper_camel: &str) -> String {
    let mut snake = String::with_capacity(upper_camel.len());

    for (i, c) in upper_camel.chars().enumerate() {
        if c.is_uppercase() {
            if i > 0 {
                snake.push('_');
            }
            snake.push(c.to_ascii_lowercase());
        } else {
            snake.push(c);
        }
    }
    snake
}

#[inline(always)]
fn bitset_repr(max_discriminant: u128) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    const U8_MAX: u128 = u8::MAX as u128;
    const U8_MAX_PLUS_1: u128 = U8_MAX + 1;
    const U16_MAX: u128 = u16::MAX as u128;
    const U16_MAX_PLUS_1: u128 = U16_MAX + 1;
    const U32_MAX: u128 = u32::MAX as u128;
    const U32_MAX_PLUS_1: u128 = U32_MAX + 1;
    const U64_MAX: u128 = u64::MAX as u128;
    const U64_MAX_PLUS_1: u128 = U64_MAX + 1;

    let n = match max_discriminant {
        1..=U8_MAX => 8,
        U8_MAX_PLUS_1..=U16_MAX => 16,
        U16_MAX_PLUS_1..=U32_MAX => 32,
        U32_MAX_PLUS_1..=U64_MAX => 64,
        U64_MAX_PLUS_1..=u128::MAX => 128,
        0 => panic!("{NO_ZERO_VAR}"),
    };

    let bitset = format!("Bitset{n}");
    let repr = format!("u{n}");
    (
        bitset.parse().expect("bitset should be a valid type token"),
        repr.parse().expect("repr should be a valid type token"),
    )
}

#[inline(always)]
fn make_struct_name(enum_name: Ident) -> proc_macro2::TokenStream {
    let s = format!("{}Varflags", enum_name.to_string());
    s.parse().expect(INVALID_STRUCT_NAME)
}

#[inline(always)]
fn try_from_match(data: VariantData, count: usize) -> proc_macro2::TokenStream {
    let mut s = "".to_owned();
    for i in 0..count {
        s.push_str(&format!(
            "{} => Ok(E::{}),",
            data.discriminants[i].expect(UNSET_DISCR_ERROR),
            data.idents[i]
        ));
    }
    s.parse().expect(INVALID_MATC_ERROR)
}

#[inline(always)]
fn display_match(data: VariantData, count: usize) -> proc_macro2::TokenStream {
    let mut s = "".to_owned();
    for i in 0..count {
        s.push_str(&format!("E::{} => \"{}\",", data.idents[i], data.idents[i]));
    }
    s.parse().expect(INVALID_MATC_ERROR)
}
