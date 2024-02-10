extern crate proc_macro;

use proc_macro2::Ident;
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, AttrStyle, Expr, Fields, ItemEnum, Lit, Meta, Variant
};

#[proc_macro_attribute]
pub fn varflags(
    _: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    varflags_impl(item)
}

fn varflags_impl(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ItemEnum {
        vis,
        ident,
        generics,
        variants,
        ..
    } = parse_macro_input!(item as ItemEnum);

    if !generics.params.is_empty() {
        panic!("generics aren't allowed");
    }

    let count = variants.len();
    if count == 0 {
        panic!("should have more than 0 variants");
    }

    if !variants.iter().all(|v| {
        if let Fields::Unit = v.fields {
            true
        } else {
            false
        }
    }) {
        panic!("only unit-like variants are allowed")
    }

    let variant_data = parse_discriminant(variants, count);
    let variant_declaration = variant_declaration(variant_data.clone(), count);
    let mod_name = make_mod_name(ident.clone());
    let max_discriminant = *variant_data.discriminants.iter().flatten().max().expect("should have one max discriminant");
    let (bitfield, repr) = bitfield_repr(max_discriminant);
    let count_tok: proc_macro2::TokenStream = count.to_string().parse().unwrap();
    let struct_name = make_struct_name(ident.clone());
    let variant_match = variant_match(variant_data, count);
    quote! {
        #[repr(#repr)]
        #vis enum #ident {
            #variant_declaration
        }

        mod #mod_name {
            use bitworks::error::ConvError;
            use bitworks::error::ConvResult;
            use bitworks::error::ConvTarget;
            use bitworks::index::Index;
            use bitworks::prelude::Bitfield;
            use bitworks::prelude::#bitfield as Inner;
            type Repr = #repr;
            
            use super::#ident as E;

            const VAR_COUNT: usize = #count_tok;

            impl core::ops::Not for E {
                type Output = #struct_name;

                fn not(self) -> Self::Output {
                    #struct_name(!Inner::new(self as Repr))
                }
            }

            impl core::ops::BitAnd for E {
                type Output = #struct_name;

                fn bitand(self, rhs: Self) -> Self::Output {
                    #struct_name(Inner::new(self as Repr) & Inner::new(rhs as Repr))
                }
            }

            impl core::ops::BitOr for E {
                type Output = #struct_name;

                fn bitor(self, rhs: Self) -> Self::Output {
                    #struct_name(Inner::new(self as Repr) | Inner::new(rhs as Repr))
                }
            }

            impl core::ops::BitXor for E {
                type Output = #struct_name;

                fn bitxor(self, rhs: Self) -> Self::Output {
                    #struct_name(Inner::new(self as Repr) ^ Inner::new(rhs as Repr))
                }
            }

            impl TryFrom<Index<Inner>> for E {
                type Error = ConvError;

                fn try_from(value: Index<Inner>) -> ConvResult<Self> {
                    let n: Repr = 1 << value.into_inner();
                    match n {
                        #variant_match
                        _ => Err(ConvError::new(ConvTarget::Index(Inner::BYTE_SIZE), ConvTarget::Enum(VAR_COUNT))),
                    }
                }
            }

            #[derive(Debug, PartialEq, Eq)]
            pub struct #struct_name(pub Inner);

            #[allow(unused)]
            impl #struct_name {
                /// Returns empty variant set.
                pub const fn none() -> Self {
                    Self(Inner::NONE)
                }

                /// Returns filled variant set.
                pub const fn all() -> Self {
                    Self(Inner::ALL)
                }

                /// Returns true, if set contains a flag.
                pub fn contains(&self, variant: &E) -> bool {
                    self.0.super_set(Inner::new(*variant as Repr))
                }

                /// Returns true, if set contains all flags of other.
                pub fn super_set(&self, other: &Self) -> bool {
                    self.0.super_set(other.0)
                }

                /// Returns true, if both self and other sets share a flag.
                pub fn intersects(&self, other: &Self) -> bool {
                    self.0.intersects(other.0)
                }

                /// Returns iterator over set variants
                pub fn variants<'a>(&'a self) -> impl Iterator<Item = E> + 'a {
                    self.0.ones().filter_map(|i| i.try_into().ok())
                }
            }

            impl core::ops::Not for #struct_name {
                type Output = Self;

                fn not(self) -> Self::Output {
                    Self(!self.0)
                }
            }

            impl core::ops::BitAnd for #struct_name {
                type Output = Self;

                fn bitand(self, rhs: Self) -> Self::Output {
                    Self(self.0 & rhs.0)
                }
            }

            impl core::ops::BitAndAssign for #struct_name {
                fn bitand_assign(&mut self, rhs: Self) {
                    self.0 &= rhs.0
                }
            }

            impl core::ops::BitOr for #struct_name {
                type Output = Self;

                fn bitor(self, rhs: Self) -> Self::Output {
                    Self(self.0 | rhs.0)
                }
            }

            impl core::ops::BitOrAssign for #struct_name {
                fn bitor_assign(&mut self, rhs: Self) {
                    self.0 |= rhs.0
                }
            }

            impl core::ops::BitXor for #struct_name {
                type Output = Self;

                fn bitxor(self, rhs: Self) -> Self::Output {
                    Self(self.0 ^ rhs.0)
                }
            }

            impl core::ops::BitXorAssign for #struct_name {
                fn bitxor_assign(&mut self, rhs: Self) {
                    self.0 ^= rhs.0
                }
            }

            impl core::ops::BitAnd<E> for #struct_name {
                type Output = Self;

                fn bitand(self, rhs: E) -> Self::Output {
                    Self(self.0 & Inner::new(rhs as Repr))
                }
            }

            impl core::ops::BitAndAssign<E> for #struct_name {
                fn bitand_assign(&mut self, rhs: E) {
                    self.0 &= Inner::new(rhs as Repr)
                }
            }

            impl core::ops::BitOr<E> for #struct_name {
                type Output = Self;

                fn bitor(self, rhs: E) -> Self::Output {
                    Self(self.0 | Inner::new(rhs as Repr))
                }
            }

            impl core::ops::BitOrAssign<E> for #struct_name {
                fn bitor_assign(&mut self, rhs: E) {
                    self.0 |= Inner::new(rhs as Repr)
                }
            }

            impl core::ops::BitXor<E> for #struct_name {
                type Output = Self;

                fn bitxor(self, rhs: E) -> Self::Output {
                    Self(self.0 ^ Inner::new(rhs as Repr))
                }
            }

            impl core::ops::BitXorAssign<E> for #struct_name {
                fn bitxor_assign(&mut self, rhs: E) {
                    self.0 ^= Inner::new(rhs as Repr)
                }
            }

            impl FromIterator<E> for #struct_name {
                fn from_iter<T: IntoIterator<Item = E>>(iter: T) -> Self {
                    iter.into_iter().fold(Self::none(), |acc, v| acc | v)
                }
            }
        }
        
        use #mod_name::#struct_name;
    }
    .into()
}

#[derive(Clone)]
struct VariantData {
    idents: Vec<String>,
    discriminants: Vec<Option<u128>>,
}

fn parse_discriminant(variants: Punctuated<Variant, Comma>, count: usize) -> VariantData {
    let mut idents = Vec::with_capacity(count);
    let mut discriminants = Vec::with_capacity(count);
    for variant in variants {
        idents.push(variant.ident.to_string());

        let mut attr_discriminant = None;
        if variant.attrs.len() > 1 {
            panic!("only 1 variant attribute is allowed");
        } else if variant.attrs.len() == 1 {
            let attribute = &variant.attrs[0];
        if let AttrStyle::Inner(_) = attribute.style {
            panic!("only outer variant attributes are allowed");
        }
        match &attribute.meta {
            Meta::NameValue(nv) => {
                match nv.path.get_ident().expect("path should be a single ident").to_string().as_str() {
                    "flag" => {
                        match &nv.value {
                            Expr::Lit(lit) => {
                                match &lit.lit {
                                    Lit::Int(lit_int) => {
                                        attr_discriminant = Some(
                                            lit_int
                                                .base10_parse::<u128>()
                                                .expect("variant discriminant should be unsigned"));
                                    },
                                    _ => panic!("bad attribute argument"),
                                }
                            },
                            _ => panic!("bad variant attribute value"),
                        }
                    },
                    "index" => {
                        match &nv.value {
                            Expr::Lit(lit) => {
                                match &lit.lit {
                                    Lit::Int(lit_int) => {
                                        let index = lit_int
                                                .base10_parse::<usize>()
                                                .expect("variant discriminant should be unsigned");
                                        attr_discriminant = Some(1 << index);
                                        
                                    },
                                    _ => panic!("bad attribute argument"),
                                }
                            },
                            _ => panic!("bad variant attribute value"),
                        }
                    }
                    _ => panic!("unknown variant attribute")
                }
            },
            _ => panic!("bad attribute type"),
        }
        }

        match variant.discriminant {
            Some((_, expr)) => match expr {
                Expr::Lit(lit) => match lit.lit {
                    Lit::Int(lit_int) => {
                        let discriminant = lit_int
                            .base10_parse::<u128>()
                            .expect("variant discriminant should be unsigned");
                        if !discriminant.count_ones() == 1 {
                            panic!("only 1 bit should be set in a variant discriminant");
                        }
                        discriminants.push(Some(discriminant));
                    }
                    _ => panic!("variant discriminant should be a number"),
                },
                _ => panic!("variant discriminant should be an expression"),
            },
            None => match attr_discriminant {
                    Some(d) => discriminants.push(Some(d)),
                    None => discriminants.push(None),
            }
        };
    }

    for i in 0..count {
        if let None = discriminants[i] {
            for j in 0..count {
                let n: u128 = 1 << j;
                if !discriminants.contains(&Some(n)) {
                    discriminants[i] = Some(n);
                    break;
                } else if j == count - 1 {
                    panic!("couldn't generate discriminants");
                }
            }
        }
    }
    VariantData {
        idents,
        discriminants,
    }
}

fn variant_declaration(data: VariantData, count: usize) -> proc_macro2::TokenStream {
    let mut s = "".to_owned();
    for i in 0..count {
        s.push_str(&format!(
            "{} = {},",
            data.idents[i],
            data.discriminants[i].expect("discriminants should be set")
        ));
    }
    s.parse().expect("variant declaration should be valid")
}

fn make_mod_name(enum_name: Ident) -> proc_macro2::TokenStream {
    let s = format!("{}_varflags", upper_camel_to_snake(&enum_name.to_string()));
    s.parse().expect("mod name should be valid")
}

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

fn bitfield_repr(max_discriminant: u128) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
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
        _ => panic!("bad variant count"),
    };

    let bitfield = format!("Bitfield{n}");
    let repr = format!("u{n}");
    (bitfield.parse().unwrap(), repr.parse().unwrap())
}

fn make_struct_name(enum_name: Ident) -> proc_macro2::TokenStream {
    let s = format!("{}Varflags", enum_name.to_string());
    s.parse().expect("struct name should be valid")
}

fn variant_match(data: VariantData, count: usize) -> proc_macro2::TokenStream {
    let mut s = "".to_owned();
    for i in 0..count {
        s.push_str(&format!(
            "{} => Ok(E::{}),",
            data.discriminants[i].expect("discriminants should be set"),
            data.idents[i]
        ));
    }
    s.parse().expect("variant match should be valid")
}
