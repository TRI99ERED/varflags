use std::error::Error;

use varflags::varflags;

// Required attributes (added manually by user).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[varflags(Clone, Copy, Hash)]
enum TestInput2 {
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

    let a = TestInput2::A;
    let b = TestInput2::B;

    assert_eq!(TestInput2::D as u8, 0b00010000);
    assert_eq!(TestInput2::E as u8, 0b10000000);
    assert_eq!(TestInput2::F as u8, 0b01000000);

    let c = a | b | TestInput2::D;
    //                                             EFHDGCBA
    assert_eq!(c, TestInput2Varflags(Bitset8::new(0b00010011)));

    assert!(c.contains(&TestInput2::A));
    assert!(!c.contains(&TestInput2::H));

    let d = TestInput2::A | TestInput2::B;
    let e = TestInput2::A | TestInput2::C;

    assert!(c.includes(&d));
    assert!(!c.includes(&e));

    let f = TestInput2::F | TestInput2::H;

    assert!(c.intersects(&e));
    assert!(!c.intersects(&f));

    let x = TestInput2Varflags::all();
    let mut iter = x.variants();

    assert_eq!(iter.next(), Some(TestInput2::A));
    assert_eq!(iter.next(), Some(TestInput2::B));
    assert_eq!(iter.next(), Some(TestInput2::C));
    assert_eq!(iter.next(), Some(TestInput2::G));
    assert_eq!(iter.next(), Some(TestInput2::D));
    assert_eq!(iter.next(), Some(TestInput2::H));
    assert_eq!(iter.next(), Some(TestInput2::F));
    assert_eq!(iter.next(), Some(TestInput2::E));
    assert_eq!(iter.next(), None);

    let iter = c.variants();
    let c: TestInput2Varflags = iter.collect();
    //                                             EFHDGCBA
    assert_eq!(c, TestInput2Varflags(Bitset8::new(0b00010011)));

    println!("{c}");

    println!("{c:?}");

    Ok(())
}
