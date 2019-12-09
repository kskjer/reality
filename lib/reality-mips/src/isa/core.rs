use super::operand::Offset;
use super::regs::Gpr;
#[cfg(feature = "serialize")]
use serde::{Deserialize, Serialize};
use std::convert::{TryFrom, TryInto};
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::str::FromStr;

/// Represents the program counter (currently executing address).
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pc(pub(super) u32);

/// Represents a jump from one PC to another. Used for building jump targets and branch targets.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Jump(pub Pc, pub Pc);

/// Represents a word that is definitely an instruction. Used internally to ensure that only decoded
/// instructions have their operands parsed.
#[derive(Copy, Clone, Debug)]
pub struct InstructionWord(pub(super) u32);

/// Represents a displacement performed by memory access instructions (lw, sw etc). May be freely
/// constructed since Offset and Gpr are guaranteed to be well formed.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Displace(pub Offset, pub Gpr);

impl Jump {
    pub const fn delta(&self) -> i32 {
        let Jump(Pc(src), Pc(dst)) = *self;

        dst.wrapping_sub(src) as i32
    }
}

impl fmt::Debug for Pc {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "Pc(0x{:08X})", self.0)
    }
}

impl Display for Pc {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "0x{:08X}", self.0)
    }
}

impl Display for Jump {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{} -> {}", self.0, self.1)
    }
}

impl Display for Displace {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}({})", self.0, self.1)
    }
}

impl TryFrom<u32> for Pc {
    type Error = PcNotWordAligned;

    #[inline(always)]
    fn try_from(value: u32) -> Result<Self, Self::Error> {
        if value % 4 != 0 {
            Err(PcNotWordAligned(value))
        } else {
            Ok(Pc(value))
        }
    }
}

impl From<Pc> for u32 {
    fn from(x: Pc) -> Self {
        x.0
    }
}

impl From<InstructionWord> for u32 {
    fn from(x: InstructionWord) -> Self {
        x.0
    }
}

impl FromStr for Pc {
    type Err = PcParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        u32::from_str_radix(s.trim_start_matches("0x"), 16)
            .map_err(|_| PcParseError::IntParse)?
            .try_into()
            .map_err(|e| PcParseError::NotWordAligned(e))
    }
}

#[derive(Debug, PartialEq)]
pub struct PcNotWordAligned(u32);

#[derive(Debug, PartialEq)]
pub enum PcParseError {
    IntParse,
    NotWordAligned(PcNotWordAligned),
}

impl Error for PcNotWordAligned {}
impl Display for PcNotWordAligned {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "Attempted to construct PC from address 0x{:08X} which is not 4 byte aligned",
            self.0
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::convert::TryInto;

    #[test]
    fn test_pc() {
        let pc_ok: Result<Pc, _> = 0x8080_1234.try_into();
        let pc_bad: Result<Pc, _> = 0x8080_1111.try_into();

        assert_eq!(Ok(Pc(0x8080_1234)), pc_ok);
        assert_eq!(Err(PcNotWordAligned(0x8080_1111)), pc_bad);
    }

    #[test]
    fn test_jump_delta() {
        assert_eq!(Jump(Pc(0x8000_0000), Pc(0x7FFF_FFFC)).delta(), -4);
        assert_eq!(Jump(Pc(0x8000_0000), Pc(0x8000_0004)).delta(), 4);
    }
}
