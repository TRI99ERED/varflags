//! Module defining error type.

use std::{
    error::Error,
    fmt::{Binary, Debug, Display},
};

/// Error indicating failure to convert from integer representation to a flag.<br/>
/// Contains the value, which failed to be converted into a flag.
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ReprToFlagError<Repr: Sized + Binary + Debug>(Repr);

impl<Repr: Sized + Binary + Debug> ReprToFlagError<Repr> {
    /// Constructs a new value of `ReprToFlagError`.
    ///
    /// # Examples
    /// ```rust
    /// # use std::error::Error;
    /// # fn main() -> Result<(), Box<dyn Error>> {
    /// use varflags::error::ReprToFlagError;
    /// 
    /// // Oh no, I couldn't convert from u8 to my enum of 8 variants.
    /// let error = ReprToFlagError::new(3u8);
    /// 
    /// assert_eq!(error.to_string(), "failed to construct a flag from {integer}, no variant has this discriminant: 0b11");
    /// #   Ok(())
    /// # }
    /// ```
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
