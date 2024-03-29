use std::error::Error;

use varflags::varflags;

// Required attributes (added manually by user).
#[derive(Copy, PartialEq, Eq, Debug)]
#[varflags]
enum TestInput {
    // Representation of the unspecified bits will be calculated
    A,
    B,
    C,

    // Or you can manually specify:
    D = 0b00010000,

    // Subattributes allow to change representation too:
    #[flag = 0b10000000]
    E,

    // or like this:
    #[shift = 6]
    F,

    // Representation of the unspecified bits will be calculated
    G,
    H,
}

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

    let display = format!("{c}");
    let debug = format!("{c:?}");

    assert_eq!(display.as_str(), "{A, B, D}");
    assert_eq!(debug.as_str(), "Varflags{A, B, D}");

    Ok(())
}
