use std::error::Error;

// Specs for Bitflags attribute

#[rustfmt::skip]

// Required attributes (added manually by user).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
// Attribute target
// #[varflags]
// Will add this attribute to match the Inner of chosen Bitfield.
#[repr(u8)]
enum TestInput {
    // Representation of the unspecified bits will be calculated
    A = /* will be inserted:*/ 0b00000001,
    B = /* will be inserted:*/ 0b00000010,
    C = /* will be inserted:*/ 0b00000100,

    // Or you can manually specify:
    D = 0b00010000,

    // Subattributes allow to change representation too:
    // #[flag = 0b10000000]
    E = /* will be inserted:*/ 0b10000000,

    // or like this:
    // #[index = 6]
    F = /* will be inserted:*/ 0b01000000,

    // Representation of the unspecified bits will be calculated
    G = /* will be inserted:*/ 0b00001000,
    H = /* will be inserted:*/ 0b00100000,
}

// Generate private module, containing the generated code.
mod test_input_varflags {
    use bitworks::error::ConvError;
    use bitworks::error::ConvResult;
    use bitworks::error::ConvTarget;
    use bitworks::index::Index;
    use bitworks::prelude::Bitfield;
    // Pick appropriate Bitfield and generate Repr depending on the choice.
    use bitworks::prelude::Bitfield8 as Inner;
    type Repr = u8;
    
    // Use the enum.
    use super::TestInput as E;
    
    // Generated based on number of variants
    const VAR_COUNT: usize = 8;

    // Should generate bitwise operators for the enum with itself, except shift and asign operators.
    // Using fully qualified name. Output should be the generated struct.
    impl core::ops::Not for E {
        type Output = TestInputVarflags;

        fn not(self) -> Self::Output {
            TestInputVarflags(!Inner::new(self as Repr))
        }
    }

    impl core::ops::BitAnd for E {
        type Output = TestInputVarflags;

        fn bitand(self, rhs: Self) -> Self::Output {
            TestInputVarflags(Inner::new(self as Repr) & Inner::new(rhs as Repr))
        }
    }

    impl core::ops::BitOr for E {
        type Output = TestInputVarflags;

        fn bitor(self, rhs: Self) -> Self::Output {
            TestInputVarflags(Inner::new(self as Repr) | Inner::new(rhs as Repr))
        }
    }

    impl core::ops::BitXor for E {
        type Output = TestInputVarflags;

        fn bitxor(self, rhs: Self) -> Self::Output {
            TestInputVarflags(Inner::new(self as Repr) ^ Inner::new(rhs as Repr))
        }
    }

    // Generate conversion from index to input
    impl TryFrom<Index<Inner>> for E {
        type Error = ConvError;

        fn try_from(value: Index<Inner>) -> ConvResult<Self> {
            let n: Repr = 1 << value.into_inner();
            match n {
                0b00000001 => Ok(E::A),
                0b00000010 => Ok(E::B),
                0b00000100 => Ok(E::C),
                0b00010000 => Ok(E::D),
                0b10000000 => Ok(E::E),
                0b01000000 => Ok(E::F),
                0b00001000 => Ok(E::G),
                0b00100000 => Ok(E::H),
                _ => Err(ConvError::new(ConvTarget::Index(Inner::BYTE_SIZE), ConvTarget::Enum(VAR_COUNT))),
            }
        }
    }

    // This struct will be generated with Bitflags appended to enum's name.
    // Should derive Debug, PartialEq and Eq.
    #[derive(Debug, PartialEq, Eq)]
    pub struct TestInputVarflags(
        // Attribute should calculate, that Inner is to be used here.
        // Can't use ByteField here. Should be private.
        #[rustfmt::skip]
        /* FOR DEBUG ONLY IN THIS TEST CRATE: */ pub /* */
        Inner,
    );

    // Generate the following impl block.
    // Will add this attribute, to allow unused items to avoid warnings in generated code.
    #[allow(unused)]
    impl TestInputVarflags {
        // Returns empty variant. Not a constant (e.g. None or NONE),
        // to avoid confusion or name collisions.
        pub const fn none() -> Self {
            Self(Inner::NONE)
        }

        // Returns filled variant. Not a constant (e.g. All or ALL),
        // to avoid confusion or name collisions.
        pub const fn all() -> Self {
            Self(Inner::ALL)
        }

        // Returns true, if contains a flag
        pub fn contains(&self, variant: &E) -> bool {
            self.0.super_set(Inner::new(*variant as Repr))
        }

        // Returns true, if contains all flags of other.
        pub fn super_set(&self, other: &Self) -> bool {
            self.0.super_set(other.0)
        }

        // Returns true, if both self and other share a flag.
        pub fn intersects(&self, other: &Self) -> bool {
            self.0.intersects(other.0)
        }

        // Returns iterator over variants
        pub fn variants<'a>(&'a self) -> impl Iterator<Item = E> + 'a {
            self.0.ones().filter_map(|i| i.try_into().ok())
        }
    }

    // Should generate bitwise operators for the struct with itself, except shift operators.
    // Using fully qualified name. Output should be Self.
    impl core::ops::Not for TestInputVarflags {
        type Output = Self;

        fn not(self) -> Self::Output {
            Self(!self.0)
        }
    }

    impl core::ops::BitAnd for TestInputVarflags {
        type Output = Self;

        fn bitand(self, rhs: Self) -> Self::Output {
            Self(self.0 & rhs.0)
        }
    }

    impl core::ops::BitAndAssign for TestInputVarflags {
        fn bitand_assign(&mut self, rhs: Self) {
            self.0 &= rhs.0
        }
    }

    impl core::ops::BitOr for TestInputVarflags {
        type Output = Self;

        fn bitor(self, rhs: Self) -> Self::Output {
            Self(self.0 | rhs.0)
        }
    }

    impl core::ops::BitOrAssign for TestInputVarflags {
        fn bitor_assign(&mut self, rhs: Self) {
            self.0 |= rhs.0
        }
    }

    impl core::ops::BitXor for TestInputVarflags {
        type Output = Self;

        fn bitxor(self, rhs: Self) -> Self::Output {
            Self(self.0 ^ rhs.0)
        }
    }

    impl core::ops::BitXorAssign for TestInputVarflags {
        fn bitxor_assign(&mut self, rhs: Self) {
            self.0 ^= rhs.0
        }
    }

    // Should generate bitwise operators for the struct with enum, except shift operators.
    // Using fully qualified name. Output should be Self.
    impl core::ops::BitAnd<E> for TestInputVarflags {
        type Output = Self;

        fn bitand(self, rhs: E) -> Self::Output {
            Self(self.0 & Inner::new(rhs as Repr))
        }
    }

    impl core::ops::BitAndAssign<E> for TestInputVarflags {
        fn bitand_assign(&mut self, rhs: E) {
            self.0 &= Inner::new(rhs as Repr)
        }
    }

    impl core::ops::BitOr<E> for TestInputVarflags {
        type Output = Self;

        fn bitor(self, rhs: E) -> Self::Output {
            Self(self.0 | Inner::new(rhs as Repr))
        }
    }

    impl core::ops::BitOrAssign<E> for TestInputVarflags {
        fn bitor_assign(&mut self, rhs: E) {
            self.0 |= Inner::new(rhs as Repr)
        }
    }

    impl core::ops::BitXor<E> for TestInputVarflags {
        type Output = Self;

        fn bitxor(self, rhs: E) -> Self::Output {
            Self(self.0 ^ Inner::new(rhs as Repr))
        }
    }

    impl core::ops::BitXorAssign<E> for TestInputVarflags {
        fn bitxor_assign(&mut self, rhs: E) {
            self.0 ^= Inner::new(rhs as Repr)
        }
    }

    // Generate implementation of FromIterator for Bitflags
    impl FromIterator<E> for TestInputVarflags {
        fn from_iter<T: IntoIterator<Item = E>>(iter: T) -> Self {
            iter.into_iter().fold(Self::none(), |acc, v| acc | v)
        }
    }
}
// Reexport the struct locally.
use test_input_varflags::TestInputVarflags;

#[test]
fn example() -> Result<(), Box<dyn Error>> {
    use bitworks::prelude::*;

    let a = TestInput::A;
    let b = TestInput::B;

    let c = a | b | TestInput::D;
    //                                               EFHDGCBA
    assert_eq!(c, TestInputVarflags(Bitfield8::new(0b00010011)));

    assert!(c.contains(&TestInput::A));
    assert!(!c.contains(&TestInput::H));

    let d = TestInput::A | TestInput::B;
    let e = TestInput::A | TestInput::C;

    assert!(c.super_set(&d));
    assert!(!c.super_set(&e));

    let f = TestInput::F | TestInput::H;

    assert!(c.intersects(&e));
    assert!(!c.intersects(&f));

    let x = TestInputVarflags::all();
    let mut iter = x.variants();

    assert_eq!(iter.next(), Some(TestInput::A));
    assert_eq!(iter.next(), Some(TestInput::B));
    assert_eq!(iter.next(), Some(TestInput::C));
    assert_eq!(iter.next(), Some(TestInput::G));
    assert_eq!(iter.next(), Some(TestInput::D));
    assert_eq!(iter.next(), Some(TestInput::H));
    assert_eq!(iter.next(), Some(TestInput::F));
    assert_eq!(iter.next(), Some(TestInput::E));
    assert_eq!(iter.next(), None);

    let iter = c.variants();
    let c: TestInputVarflags = iter.collect();
    //                                               EFHDGCBA
    assert_eq!(c, TestInputVarflags(Bitfield8::new(0b00010011)));

    Ok(())
}
