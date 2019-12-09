use super::operand::RawInstructionOperand;
use std::fmt::{Display, Error, Formatter};

include!(concat!(env!("OUT_DIR"), "/regs.rs"));
