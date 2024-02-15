Crate exporting `varflags` macro, allowing to use unit-like enums in conjunction with `Varflags` struct to create easy to use bitflags data structure defined over enum variants.

Enable feature `"serde"` to enable `serde::Serialize` and `serde::Deserialize` for most applicable types.

Heavily inspired by the famous [enumflags2](https://crates.io/crates/enumflags2) crate. It's likely
better to use that crate, as it is well tested and adopted.
`varflags` was created as a hands-on learning experience for me at building procedural macros (contains no code from `enumflags2`). It's also depending on my other crate [bitworks](https://crates.io/crates/bitworks), which
is a generic bitset data structure implementation from scratch. Unless you use `bitworks`, there's little reason to use `varflags`.

## Examples
```rust
use varflags::varflags;
 
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
    // or like this (corresponds to 0b01000000):
    #[shift = 6]
    F,
    // Representation of the unspecified bits will be calculated
    G,
    H,
}
 
fn example() {
    // Input enum variants are still intact.
    let a = TestInput::A;
    let b = TestInput::B;
    
    // All variants have correctly set discriminants.
    assert_eq!(u8::from(TestInput::D), 0b00010000);
    assert_eq!(u8::from(TestInput::E), 0b10000000);
    assert_eq!(u8::from(TestInput::F), 0b01000000);
 
    // Operators allow easy construction of Varflags values.
    // Here c is Varflags<TestInput, Bitset8>.
    let c = a | b | TestInput::D;
    //                                                                             EFHDGCBA
    assert_eq!(c, TestInputVarflags::_from_inner(bitworks::prelude::Bitset8::new(0b00010011)));
 
    // Can check if Varflags value contains a flag.
    assert!(c.contains(&TestInput::A));
    assert!(!c.contains(&TestInput::H));
 
    // Can check if Varflags value includes all flags of another Varflags value.
    let d = TestInput::A | TestInput::B;
    let e = TestInput::A | TestInput::C;
    
    assert!(c.includes(&d));
    assert!(!c.includes(&e));
 
 
    // Can check if two Varflags values share at least one flag between themselves.
    let f = TestInput::F | TestInput::H;
 
    assert!(c.intersects(&e));
    assert!(!c.intersects(&f));
 
    // Can iterate over contained flags.
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
 
    // Can collect from iterator over flags.
    let iter = c.variants();
    let c: TestInputVarflags = iter.collect();
    //                                                                             EFHDGCBA
    assert_eq!(c, TestInputVarflags::_from_inner(bitworks::prelude::Bitset8::new(0b00010011)));
 
    // Can be made into string with Display or Debug.
    let display = format!("{c}");
    let debug = format!("{c:?}");

    assert_eq!(display.as_str(), "{A, B, D}");
    assert_eq!(debug.as_str(), "Varflags{A, B, D}");
}
```

## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.