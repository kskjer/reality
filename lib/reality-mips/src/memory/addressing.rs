use crate::isa::{Pc, PcNotWordAligned};
use std::convert::{TryFrom, TryInto};
use std::fmt::{Error, Formatter};

use std::{fmt, iter, ops};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Addr(u32);

impl Addr {
    pub fn iter(mut self) -> impl Iterator<Item = Addr> {
        iter::from_fn(move || {
            let result = Some(self);

            self = Addr(self.0 + 1);

            result
        })
    }
}

impl From<Addr> for u32 {
    fn from(x: Addr) -> Self {
        x.0
    }
}

impl From<Pc> for Addr {
    fn from(x: Pc) -> Self {
        Addr(x.into())
    }
}

impl From<u32> for Addr {
    fn from(x: u32) -> Self {
        Addr(x)
    }
}

impl ops::Sub for Addr {
    type Output = i32;

    fn sub(self, rhs: Self) -> Self::Output {
        self.0.wrapping_sub(rhs.0) as i32
    }
}

impl ops::Add<u32> for Addr {
    type Output = Addr;

    fn add(self, rhs: u32) -> Self::Output {
        Addr(self.0 + rhs)
    }
}

impl TryFrom<Addr> for Pc {
    type Error = PcNotWordAligned;

    #[inline(always)]
    fn try_from(value: Addr) -> Result<Self, Self::Error> {
        value.0.try_into()
    }
}

impl fmt::Display for Addr {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "0x{:08X}", self.0)
    }
}

impl fmt::Debug for Addr {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "Addr(0x{:08X})", self.0)
    }
}
