Crate exporting `varflags` macro, allowing to use unit-like enums in conjunction with `Varflags` struct to create easy to use bitflags data structure defined over enum variants.

Enable feature `"serde"` to enable `serde::Serialize` and `serde::Deserialize` for most applicable types.

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

let a = TestInput::A;
let b = TestInput::B;

assert_eq!(u8::from(TestInput::D), 0b00010000);
assert_eq!(u8::from(TestInput::E), 0b10000000);
assert_eq!(u8::from(TestInput::F), 0b01000000);

let c = a | b | TestInput::D;
//                                                                             EFHDGCBA
assert_eq!(c, TestInputVarflags::_from_inner(bitworks::prelude::Bitset8::new(0b00010011)));

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
//                                                                             EFHDGCBA
assert_eq!(c, TestInputVarflags::_from_inner(bitworks::prelude::Bitset8::new(0b00010011)));

println!("{c}");

println!("{c:?}");
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