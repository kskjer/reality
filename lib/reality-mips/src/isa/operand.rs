use super::regs::{Fcr, Fpr, Gpr, Mmu};
use crate::isa::core::{InstructionWord, Jump, Pc};
use std::convert::{TryFrom, TryInto};
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::marker::PhantomData;

pub trait InstructionOperand {
    type Output;

    const SHIFT: u32;
    const WIDTH: u32;
    const MASK: u32 = (1 << Self::WIDTH) - 1;
}

pub trait InstructionOperandParser {
    type Output;

    fn read(src: InstructionWord) -> Self::Output;
    fn build(src: Self::Output) -> u32;
}

/// Helper wrapper designating that the contained u32 may be losslessly converted to its desired
/// operand, based on the SHIFT and MASK defined in an InstructionOperand implementation.
pub struct RawInstructionOperand(pub(super) u32);

impl<T, O> InstructionOperandParser for T
where
    T: InstructionOperand<Output = O>,
    O: From<RawInstructionOperand>,
    RawInstructionOperand: From<O>,
{
    type Output = O;

    fn read(src: InstructionWord) -> Self::Output {
        RawInstructionOperand((src.0 >> Self::SHIFT) & Self::MASK).into()
    }

    fn build(src: Self::Output) -> u32 {
        let RawInstructionOperand(v) = src.into();

        (v & Self::MASK) << Self::SHIFT
    }
}

include!(concat!(env!("OUT_DIR"), "/reg_operands.rs"));

macro_rules! impl_shortcut {
    ($ty:tt, $backing:ty, $shift:expr, $width:expr) => {
        #[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
        pub struct $ty(pub(super) $backing);

        impl InstructionOperand for $ty {
            type Output = $ty;

            const SHIFT: u32 = $shift;
            const WIDTH: u32 = $width;
        }

        impl From<RawInstructionOperand> for $ty {
            fn from(x: RawInstructionOperand) -> Self {
                $ty(x.0 as $backing)
            }
        }

        impl From<$ty> for RawInstructionOperand {
            fn from(x: $ty) -> Self {
                RawInstructionOperand((x.0 as u32 & $ty::MASK) << $ty::SHIFT)
            }
        }

        impl From<$ty> for $backing {
            fn from(x: $ty) -> Self {
                x.0
            }
        }

        impl_shortcut!(try_from; $ty, $backing, $shift, $width);
    };

    (try_from; JumpTarget, $backing:ty, $shift:expr, $width:expr) => {};
    (try_from; $ty:tt, i16, $shift:expr, $width:expr) => {};

    (try_from; $ty:tt, $backing:ty, $shift:expr, $width:expr) => {
        impl std::convert::TryFrom<$backing> for $ty {
            type Error = ValueTooLargeError<$backing, $ty>;

            fn try_from(value: $backing) -> Result<Self, Self::Error> {
                if value as u32 >= 1 << $shift {
                    return Err(ValueTooLargeError(value, PhantomData));
                }

                Ok($ty(value))
            }
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ValueTooLargeError<V, T>(V, PhantomData<T>);

impl_shortcut!(BranchTarget, i16, 0, 16);
impl_shortcut!(Offset, i16, 0, 16);
impl_shortcut!(JumpTarget, u32, 0, 26);
impl_shortcut!(CacheOp, u8, 16, 5);
impl_shortcut!(BreakCode, u16, 16, 11);
impl_shortcut!(SyscallCode, u32, 6, 20);
impl_shortcut!(ShiftAmount, u8, 6, 5);

impl BranchTarget {
    pub const fn full(&self, pc: Pc) -> Pc {
        Pc((pc.0 as i32 + 4 + (self.0 as i32 * 4)) as u32)
    }

    pub const fn delta(&self) -> i32 {
        Jump(Pc(0), self.full(Pc(0))).delta()
    }
}

impl JumpTarget {
    const MAX_JUMP: i32 = (1 << (Self::WIDTH as i32 + 2)) - 4;
    const SEGMENT_MASK: u32 = !((1 << (Self::WIDTH + 2)) - 1);

    const fn segment(pc: Pc) -> u8 {
        ((pc.0 & Self::SEGMENT_MASK) >> Self::WIDTH + 2) as u8
    }

    pub const fn full(&self, pc: Pc) -> Pc {
        Pc((pc.0 & Self::SEGMENT_MASK) | (self.0 << 2))
    }
}

impl TryFrom<Jump> for BranchTarget {
    type Error = BranchOffsetTooLarge;

    fn try_from(x: Jump) -> Result<Self, Self::Error> {
        // Branch deltas are calculated from src + 4
        let adjusted = Jump(Pc((x.0).0 + 4), x.1);

        Ok(BranchTarget(
            (adjusted.delta() / 4)
                .try_into()
                .map_err(|_| BranchOffsetTooLarge(x))?,
        ))
    }
}

impl TryFrom<Jump> for JumpTarget {
    type Error = JumpSegmentDiffers;

    fn try_from(x: Jump) -> Result<Self, JumpSegmentDiffers> {
        if x.delta().abs() > Self::MAX_JUMP {
            Err(JumpSegmentDiffers(
                x,
                JumpTarget::segment(x.0),
                JumpTarget::segment(x.1),
            ))
        } else {
            let Jump(_, Pc(dst)) = x;

            Ok(JumpTarget((dst >> 2) & Self::MASK))
        }
    }
}

impl Display for Offset {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct BranchOffsetTooLarge(Jump);

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct JumpSegmentDiffers(Jump, u8, u8);

impl Error for BranchOffsetTooLarge {}
impl Display for BranchOffsetTooLarge {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "Branch {} too large ({} bytes). Max is +/- {} bytes.",
            self.0,
            self.0.delta(),
            (1 << 16) - 1
        )
    }
}

impl Error for JumpSegmentDiffers {}
impl Display for JumpSegmentDiffers {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "Jump {} has targets with differing segments: 0x{:X} and 0x{:X} respectively.",
            self.0, self.1, self.2
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::isa::core::{Jump, Pc};
    use std::convert::TryInto;
    use std::i16;

    fn make_jump(a: u32, b: u32) -> Jump {
        Jump(Pc(a), Pc(b))
    }

    #[test]
    fn test_branch() {
        let big = make_jump(0x8000_0000, 0x8000_0000 + (1 << 15) - 4);
        let big_target: Result<BranchTarget, _> = big.try_into();

        let small = make_jump(0x8000_0000, 0x8000_0004);
        let small_target: Result<BranchTarget, _> = small.try_into();

        let upper = make_jump(0x8000_0000, 0x8000_0000 + (1 << 17) - 4);
        let upper_target: Result<BranchTarget, _> = upper.try_into();

        let max = make_jump(0x8000_0000, 0x8000_0000 + (1 << 17));
        let max_target: Result<BranchTarget, _> = max.try_into();

        let min_err = make_jump(0x8000_0000 + (1 << 17), 0x8000_0000);
        let min_err_target: Result<BranchTarget, _> = min_err.try_into();

        let min = make_jump(0x8000_0000 + (1 << 17) - 4, 0x8000_0000);
        let min_target: Result<BranchTarget, _> = min.try_into();

        assert_eq!(big_target, Ok(BranchTarget(0x1FFE)));
        assert_eq!(small_target, Ok(BranchTarget(0)));
        assert_eq!(upper_target, Ok(BranchTarget(0x7FFE)));
        assert_eq!(max_target, Ok(BranchTarget(0x7FFF)));
        assert_eq!(min_err_target, Err(BranchOffsetTooLarge(min_err)));
        assert_eq!(min_target, Ok(BranchTarget(i16::MIN)));

        assert_eq!(
            small_target.map(|t| t.full(Pc(0x8000_0000))),
            Ok(Pc(0x8000_0004))
        );
    }

    #[test]
    fn test_jump() {
        let start = 0x8000_0000;

        let right = (
            make_jump(start, start + JumpTarget::MAX_JUMP as u32),
            make_jump(start + JumpTarget::MAX_JUMP as u32, start),
        );

        let wrong = (
            make_jump(start, start + JumpTarget::MAX_JUMP as u32 + 4),
            make_jump(start + JumpTarget::MAX_JUMP as u32 + 4, start),
        );

        let results: Vec<Result<JumpTarget, _>> = vec![
            right.0.try_into(),
            right.1.try_into(),
            wrong.0.try_into(),
            wrong.1.try_into(),
        ];

        assert_eq!(results[0], Ok(JumpTarget(0x03FF_FFFF)));
        assert_eq!(results[1], Ok(JumpTarget(0)));
        assert_eq!(results[2], Err(JumpSegmentDiffers(wrong.0, 8, 9)));
        assert_eq!(results[3], Err(JumpSegmentDiffers(wrong.1, 9, 8)));

        assert_eq!(
            results[0].map(|t| t.full(Pc(0x8000_0000))),
            Ok(Pc(0x8FFF_FFFC))
        );
    }

    #[test]
    fn test_branch_parse() {
        let x = BranchTarget::read(InstructionWord(0xFFFF));

        assert_eq!(x.full(Pc(0x8000_0000)), Pc(0x8000_0000));
    }

    #[test]
    fn test_jump_parse() {
        let x = JumpTarget::read(InstructionWord(0x03FF_FFFF));

        assert_eq!(x.full(Pc(0x8000_0000)), Pc(0x8FFF_FFFC));
    }

    #[test]
    fn test_shift_amount() {
        let sa = u8::from(ShiftAmount::read(InstructionWord(0x00073880)));

        assert_eq!(sa, 2);
    }

    #[test]
    fn test_try_from_shift_amount() {
        let sa: Result<ShiftAmount, _> = 4.try_into();

        assert_eq!(sa, Ok(ShiftAmount(4)));
    }
}
