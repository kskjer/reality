use super::core::*;
use super::operand::*;
use super::regs::*;
use std::fmt;
use std::fmt::{Error, Formatter};
use DecodeError::*;
use Instruction::*;

#[derive(Copy, Clone, Debug)]
pub struct InstructionAt(Instruction, Pc);

impl From<(Instruction, Pc)> for InstructionAt {
    fn from(x: (Instruction, Pc)) -> Self {
        InstructionAt(x.0, x.1)
    }
}

impl From<(Pc, Instruction)> for InstructionAt {
    fn from(x: (Pc, Instruction)) -> Self {
        InstructionAt(x.1, x.0)
    }
}

impl fmt::Display for DecodeError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{:?}", self)
    }
}

include!(concat!(env!("OUT_DIR"), "/instruction.rs"));

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum PseudoInstruction {
    NOP,
    MOVE(Gpr, Gpr),
    LI(Gpr, i16),
    B(BranchTarget),
}

impl Instruction {
    pub fn is_nop(self) -> bool {
        if let Some(PseudoInstruction::NOP) = self.pseudo() {
            true
        } else {
            false
        }
    }

    pub fn pseudo(self) -> Option<PseudoInstruction> {
        use Instruction::*;
        use PseudoInstruction::*;

        Some(match self {
            SLL(Gpr::Zero, Gpr::Zero, ShiftAmount(0)) => NOP,
            OR(rd, rs, rt) | ADDU(rd, rs, rt) if rt == Gpr::Zero => MOVE(rd, rs),
            ADDIU(rt, rs, imm) if rs == Gpr::Zero => LI(rt, imm),
            BEQ(rs, rt, target) if rs == rt => B(target),
            _ => return None,
        })
    }
}
