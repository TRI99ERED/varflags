//! Crate exporting [`varflags`] macro, allowing to use unit-like enums in conjunction with
//! [`Varflags`] struct to create easy to use bitflags data structure defined over enum variants.
//! 
//! Read [`varflags`] documentation for comprehensive description with example.
//! 
//! Enable feature `"serde"` to enable `serde::Serialize` and `serde::Deserialize` for most applicable types.

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
