//! Crate exporting [`varflags`] macro, allowing to use unit-like enums in conjunction with
//! [`Varflags`] struct to create easy to use bitflags data structure defined over enum variants.
//! 
//! Enable feature `"serde"` to enable `serde::Serialize` and `serde::Deserialize` for most applicable types.
//!
//! # Examples
//! ```rust
//! use varflags::varflags;
//! 
//! #[derive(Copy, PartialEq, Eq, Debug)]
//! #[varflags]
//! enum TestInput {
//!     // Representation of the unspecified bits will be calculated
//!     A,
//!     B,
//!     C,
//!     // Or you can manually specify:
//!     D = 0b00010000,
//!     // Subattributes allow to change representation too:
//!     #[flag = 0b10000000]
//!     E,
//!     // or like this (corresponds to 0b01000000):
//!     #[shift = 6]
//!     F,
//!     // Representation of the unspecified bits will be calculated
//!     G,
//!     H,
//! }
//! 
//! fn main() {
//!     let a = TestInput::A;
//!     let b = TestInput::B;
//! 
//!     assert_eq!(u8::from(TestInput::D), 0b00010000);
//!     assert_eq!(u8::from(TestInput::E), 0b10000000);
//!     assert_eq!(u8::from(TestInput::F), 0b01000000);
//! 
//!     let c = a | b | TestInput::D;
//!     //                                                                             EFHDGCBA
//!     assert_eq!(c, TestInputVarflags::_from_inner(bitworks::prelude::Bitset8::new(0b00010011)));
//! 
//!     assert!(c.contains(&TestInput::A));
//!     assert!(!c.contains(&TestInput::H));
//! 
//!     let d = TestInput::A | TestInput::B;
//!     let e = TestInput::A | TestInput::C;
//! 
//!     assert!(c.includes(&d));
//!     assert!(!c.includes(&e));
//! 
//!     let f = TestInput::F | TestInput::H;
//! 
//!     assert!(c.intersects(&e));
//!     assert!(!c.intersects(&f));
//! 
//!     let x = TestInputVarflags::ALL;
//!     let mut iter = x.variants();
//! 
//!     assert_eq!(iter.next(), Some(TestInput::A));
//!     assert_eq!(iter.next(), Some(TestInput::B));
//!     assert_eq!(iter.next(), Some(TestInput::C));
//!     assert_eq!(iter.next(), Some(TestInput::G));
//!     assert_eq!(iter.next(), Some(TestInput::D));
//!     assert_eq!(iter.next(), Some(TestInput::H));
//!     assert_eq!(iter.next(), Some(TestInput::F));
//!     assert_eq!(iter.next(), Some(TestInput::E));
//!     assert_eq!(iter.next(), None);
//! 
//!     let iter = c.variants();
//!     let c: TestInputVarflags = iter.collect();
//!     //                                                                             EFHDGCBA
//!     assert_eq!(c, TestInputVarflags::_from_inner(bitworks::prelude::Bitset8::new(0b00010011)));
//! 
//!     let display = format!("{c}");
//!     let debug = format!("{c:?}");
//! 
//!     assert_eq!(display.as_str(), "{A, B, D}");
//!     assert_eq!(debug.as_str(), "Varflags{A, B, D}");
//! }
//! ```

extern crate varflags_attribute;
pub use varflags_attribute::varflags;

use bitworks::{bitset::Bitset, index::Index};
use std::{
    fmt::Display,
    marker::{Copy, PhantomData},
};

pub mod error;

/// Struct representing set over variants of some enum `E` implemented using [`bitworks`] bitset `B`.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Varflags<E, B>(pub(crate) B, pub(crate) PhantomData<E>)
where
    E: Sized + Clone + TryFrom<Index<B>>,
    B: Bitset + Copy,
    B::Repr: From<E>;

impl<E, B> Varflags<E, B>
where
    E: Sized + Clone + TryFrom<Index<B>>,
    B: Bitset + Copy,
    B::Repr: From<E>,
{
    /// Empty set of variants.
    pub const NONE: Self = Self(B::NONE, PhantomData);

    /// Set of variants containing every variant of `E`.
    pub const ALL: Self = Self(B::ALL, PhantomData);

    /// Constructs `Varflags` from the inner [`Bitset`].
    pub const fn _from_inner(bitset: B) -> Self {
        Self(bitset, PhantomData)
    }

    /// Returns true, if `self` contains a `variant`.
    pub fn contains(&self, variant: &E) -> bool {
        self.0
            .includes(&B::from_repr(B::Repr::from(variant.clone())))
    }

    /// Returns true, if `self` contains all variants of `other`.
    pub fn includes(&self, other: &Self) -> bool {
        self.0.includes(&other.0)
    }

    /// Returns true, if both `self` and `other` share at least 1 variant.
    pub fn intersects(&self, other: &Self) -> bool {
        self.0.intersects(&other.0)
    }

    /// Returns iterator over variants contained in `self`.
    pub fn variants<'a>(&'a self) -> impl Iterator<Item = E> + 'a {
        self.0.ones().filter_map(|i| i.try_into().ok())
    }
}

impl<E, B> core::ops::Not for Varflags<E, B>
where
    E: Sized + Clone + TryFrom<Index<B>>,
    B: Bitset + Copy,
    B::Repr: From<E>,
{
    type Output = Self;

    fn not(self) -> Self::Output {
        Self(self.0.complement(), PhantomData)
    }
}

impl<E, B> core::ops::BitAnd for Varflags<E, B>
where
    E: Sized + Clone + TryFrom<Index<B>>,
    B: Bitset + Copy,
    B::Repr: From<E>,
{
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0.intersection(rhs.0), PhantomData)
    }
}

impl<E, B> core::ops::BitAndAssign for Varflags<E, B>
where
    E: Sized + Clone + TryFrom<Index<B>>,
    B: Bitset + Copy,
    B::Repr: From<E>,
{
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 = self.0.intersection(rhs.0);
    }
}

impl<E, B> core::ops::BitOr for Varflags<E, B>
where
    E: Sized + Clone + TryFrom<Index<B>>,
    B: Bitset + Copy,
    B::Repr: From<E>,
{
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0.union(rhs.0), PhantomData)
    }
}

impl<E, B> core::ops::BitOrAssign for Varflags<E, B>
where
    E: Sized + Clone + TryFrom<Index<B>>,
    B: Bitset + Copy,
    B::Repr: From<E>,
{
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 = self.0.union(rhs.0);
    }
}

impl<E, B> core::ops::BitXor for Varflags<E, B>
where
    E: Sized + Clone + TryFrom<Index<B>>,
    B: Bitset + Copy,
    B::Repr: From<E>,
{
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Self(self.0.sym_difference(rhs.0), PhantomData)
    }
}

impl<E, B> core::ops::BitXorAssign for Varflags<E, B>
where
    E: Sized + Clone + TryFrom<Index<B>>,
    B: Bitset + Copy,
    B::Repr: From<E>,
{
    fn bitxor_assign(&mut self, rhs: Self) {
        self.0 = self.0.sym_difference(rhs.0);
    }
}

impl<E, B> core::ops::BitAnd<E> for Varflags<E, B>
where
    E: Sized + Clone + TryFrom<Index<B>>,
    B: Bitset + Copy,
    B::Repr: From<E>,
{
    type Output = Self;

    fn bitand(self, rhs: E) -> Self::Output {
        Self(
            self.0.intersection(B::from_repr(B::Repr::from(rhs))),
            PhantomData,
        )
    }
}

impl<E, B> core::ops::BitAndAssign<E> for Varflags<E, B>
where
    E: Sized + Clone + TryFrom<Index<B>>,
    B: Bitset + Copy,
    B::Repr: From<E>,
{
    fn bitand_assign(&mut self, rhs: E) {
        self.0 = self.0.intersection(B::from_repr(B::Repr::from(rhs)));
    }
}

impl<E, B> core::ops::BitOr<E> for Varflags<E, B>
where
    E: Sized + Clone + TryFrom<Index<B>>,
    B: Bitset + Copy,
    B::Repr: From<E>,
{
    type Output = Self;

    fn bitor(self, rhs: E) -> Self::Output {
        Self(self.0.union(B::from_repr(B::Repr::from(rhs))), PhantomData)
    }
}

impl<E, B> core::ops::BitOrAssign<E> for Varflags<E, B>
where
    E: Sized + Clone + TryFrom<Index<B>>,
    B: Bitset + Copy,
    B::Repr: From<E>,
{
    fn bitor_assign(&mut self, rhs: E) {
        self.0 = self.0.union(B::from_repr(B::Repr::from(rhs)));
    }
}

impl<E, B> core::ops::BitXor<E> for Varflags<E, B>
where
    E: Sized + Clone + TryFrom<Index<B>>,
    B: Bitset + Copy,
    B::Repr: From<E>,
{
    type Output = Self;

    fn bitxor(self, rhs: E) -> Self::Output {
        Self(
            self.0.sym_difference(B::from_repr(B::Repr::from(rhs))),
            PhantomData,
        )
    }
}

impl<E, B> core::ops::BitXorAssign<E> for Varflags<E, B>
where
    E: Sized + Clone + TryFrom<Index<B>>,
    B: Bitset + Copy,
    B::Repr: From<E>,
{
    fn bitxor_assign(&mut self, rhs: E) {
        self.0 = self.0.sym_difference(B::from_repr(B::Repr::from(rhs)));
    }
}

impl<E, B> FromIterator<E> for Varflags<E, B>
where
    E: Sized + Clone + TryFrom<Index<B>>,
    B: Bitset + Copy,
    B::Repr: From<E>,
{
    fn from_iter<T: IntoIterator<Item = E>>(iter: T) -> Self {
        iter.into_iter().fold(Self::NONE, |acc, v| acc | v)
    }
}

impl<E, B> core::fmt::Debug for Varflags<E, B>
where
    E: Sized + Clone + TryFrom<Index<B>> + Display,
    B: Bitset + Copy,
    B::Repr: From<E>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let count = self.variants().count();
        write!(
            f,
            "Varflags{{{}}}",
            self.variants()
                .enumerate()
                .fold("".to_owned(), |mut acc, (i, v)| {
                    acc.push_str(&v.to_string());
                    if i != count - 1 {
                        acc.push_str(", ")
                    }
                    acc
                })
        )
    }
}

impl<E, B> core::fmt::Display for Varflags<E, B>
where
    E: Sized + Clone + TryFrom<Index<B>> + Display,
    B: Bitset + Copy,
    B::Repr: From<E>,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let count = self.variants().count();
        write!(
            f,
            "{{{}}}",
            self.variants()
                .enumerate()
                .fold("".to_owned(), |mut acc, (i, v)| {
                    acc.push_str(&v.to_string());
                    if i != count - 1 {
                        acc.push_str(", ")
                    }
                    acc
                })
        )
    }
}