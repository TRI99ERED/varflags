use std::{
    error::Error,
    fmt::{Binary, Debug, Display},
};

/// Error indicating failure to convert from integer representation to a flag.<br/>
/// Contains the value, which failed to be converted into a flag.
#[derive(Debug)]
pub struct ReprToFlagError<Repr: Sized + Binary + Debug>(Repr);

impl<Repr: Sized + Binary + Debug> ReprToFlagError<Repr> {
    pub const fn new(repr: Repr) -> Self {
        Self(repr)
    }
}

impl<Repr: Sized + Binary + Debug> Error for ReprToFlagError<Repr> {}

impl<Repr: Sized + Binary + Debug> Display for ReprToFlagError<Repr> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "failed to construct a flag from {{integer}}, no variant has this discriminant: {:#b}",
            self.0
        )
    }
}
