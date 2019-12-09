use std::fmt;
use std::fmt::{Error, Formatter};

#[derive(Clone, Debug)]
pub struct Symbol(pub(super) String);

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        self.0.fmt(f)
    }
}
