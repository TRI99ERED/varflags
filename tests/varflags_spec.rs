use std::error::Error;

// Specs for Varflags attribute

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
    use varflags::error::ReprToFlagError;
    use bitworks::index::Index;
    // Pick appropriate Bitfield and generate Repr depending on the choice.
    use bitworks::prelude::Bitset8 as Inner;
    type Repr = u8;

    // Use the enum.
    use super::TestInput as E;
    
    pub type TestInputVarflags = varflags::Varflags<E, Inner>;

    // Generate From<E> for Repr
    impl From<E> for Repr {
        fn from(value: E) -> Self {
            value as Repr
        }
    }

    // Should generate bitwise operators for the enum with itself, except shift and asign operators.
    // Using fully qualified name. Output should be the generated struct.
    impl core::ops::Not for E {
        type Output = TestInputVarflags;

        fn not(self) -> Self::Output {
            TestInputVarflags::_from_inner(!Inner::new(self as Repr))
        }
    }

    impl core::ops::BitAnd for E {
        type Output = TestInputVarflags;

        fn bitand(self, rhs: Self) -> Self::Output {
            TestInputVarflags::_from_inner(Inner::new(self as Repr) & Inner::new(rhs as Repr))
        }
    }

    impl core::ops::BitOr for E {
        type Output = TestInputVarflags;

        fn bitor(self, rhs: Self) -> Self::Output {
            TestInputVarflags::_from_inner(Inner::new(self as Repr) | Inner::new(rhs as Repr))
        }
    }

    impl core::ops::BitXor for E {
        type Output = TestInputVarflags;

        fn bitxor(self, rhs: Self) -> Self::Output {
            TestInputVarflags::_from_inner(Inner::new(self as Repr) ^ Inner::new(rhs as Repr))
        }
    }

    // Generate conversion from index to input
    impl TryFrom<Index<Inner>> for E {
        type Error = ReprToFlagError<Repr>;

        fn try_from(value: Index<Inner>) -> Result<Self, Self::Error> {
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
                _ => Err(Self::Error::new(n)),
            }
        }
    }

    // Generate Display for input
    impl core::fmt::Display for E {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{}",
                match *self {
                    E::A => "A",
                    E::B => "B",
                    E::C => "C",
                    E::D => "D",
                    E::E => "E",
                    E::F => "F",
                    E::G => "G",
                    E::H => "H",
                }
            )
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

    assert_eq!(u8::from(TestInput::D), 0b00010000);
    assert_eq!(u8::from(TestInput::E), 0b10000000);
    assert_eq!(u8::from(TestInput::F), 0b01000000);

    let c = a | b | TestInput::D;
    //                                                          EFHDGCBA
    assert_eq!(c, TestInputVarflags::_from_inner(Bitset8::new(0b00010011)));

    assert!(c.contains(&TestInput::A));
    assert!(!c.contains(&TestInput::H));

    let d = TestInput::A | TestInput::B;
    let e = TestInput::A | TestInput::C;

    assert!(c.includes(&d));
    assert!(!c.includes(&e));

    let f = TestInput::F | TestInput::H;

    assert!(c.intersects(&e));
    assert!(!c.intersects(&f));

    let x = TestInputVarflags::ALL;
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
    //                                                          EFHDGCBA
    assert_eq!(c, TestInputVarflags::_from_inner(Bitset8::new(0b00010011)));

    println!("{c}");
    
    println!("{c:?}");

    Ok(())
}
