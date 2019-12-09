use crate::isa::operand::Offset;
use crate::isa::regs::Gpr;
use crate::isa::{decode, DecodeError, Displace, Pc};
use crate::memory::{Addr, MemoryMap};
use reality_util::U8SliceUtils;

struct UpperImmediate(u16);

enum DetectorState {
    NeedJr,
    NeedLw(Gpr),
    NeedAddu(Gpr, Offset),
    NeedLui(Gpr, Offset, Gpr),
    NeedSll(Gpr, Offset, Gpr, UpperImmediate),
    NeedSltiu(Gpr, Offset, Gpr, UpperImmediate),
    Complete(Gpr, Offset, Gpr, UpperImmediate, i16),
}

#[rustfmt::skip]
fn jt_detect_step(word: u32, state: DetectorState) -> Result<DetectorState, DecodeError> {
    use crate::isa::Instruction::*;
    use DetectorState::*;

    Ok(match (state, decode(word)?) {
        (NeedJr, JR(rs)) =>
            NeedLw(rs),

        (NeedLw(reg), LW(rt, Displace(offset, base))) if rt == reg =>
            NeedAddu(base, offset),

        (NeedAddu(reg, offset), ADDU(rd, rs, rt)) if rd == reg =>
            NeedLui(rs, offset, rt),

        (NeedLui(reg, offset, addu_rt), LUI(rt, upper)) if rt == reg =>
            NeedSll(rt, offset, addu_rt, UpperImmediate(upper)),

        (NeedSll(_, offset, addu_rt, upper), SLL(rd, rt, _)) if rd == addu_rt =>
            NeedSltiu(rt, offset, addu_rt, upper),

        (NeedSltiu(reg, offset, addu_rt, upper), SLTIU(_, rs, imm)) |
        (NeedSltiu(reg, offset, addu_rt, upper), SLTI(_, rs, imm)) if rs == reg =>
            Complete(rs, offset, addu_rt, upper, imm),

        (state, _) => state,
    })
}

#[derive(Debug)]
pub enum JtDetectionError {
    Decode(Pc, DecodeError),
    OutOfBounds(Addr),
    EntryOutOfBounds(u16, Addr),
    NotFound,
}

pub fn jt_detector(
    start: Pc,
    memory: &impl MemoryMap,
) -> Result<impl Iterator<Item = u32>, JtDetectionError> {
    let mut state = DetectorState::NeedJr;

    for pc in start.iter_backwards().take(32) {
        let addr: Addr = pc.into();

        state = jt_detect_step(
            memory
                .lookup(addr)
                .ok_or(JtDetectionError::OutOfBounds(addr))?
                .read_u32(),
            state,
        )
        .map_err(|e| JtDetectionError::Decode(pc, e))?;

        if let DetectorState::Complete(_rs, offset, _addu_rt, upper, imm) = state {
            let offset: i16 = offset.into();
            let start = (((upper.0 as u32) << 16) as i32 + offset as i32) as u32;
            let start: Addr = start.into();
            let entries = (0u32..imm as u32 - 1)
                .map(|i| (i, start + i as u32 * 4))
                .map(|(i, addr)| {
                    memory
                        .lookup(addr)
                        .ok_or(JtDetectionError::EntryOutOfBounds(i as u16, addr))
                        .map(|d| d.read_u32())
                })
                .collect::<Result<Vec<_>, _>>()?;

            return Ok(entries.into_iter());
        }
    }

    Err(JtDetectionError::NotFound)
}

mod error_impl {
    use super::*;
    use std::error::Error;
    use std::fmt;
    use std::fmt::Formatter;

    impl Error for JtDetectionError {}

    impl fmt::Display for JtDetectionError {
        fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
            match self {
                JtDetectionError::OutOfBounds(addr) => {
                    write!(f, "Memory access to {} was out of bounds", addr)
                }
                JtDetectionError::EntryOutOfBounds(i, addr) => write!(
                    f,
                    "Memory access to {} was out of bounds while reading entry {}",
                    addr, i
                ),
                JtDetectionError::Decode(pc, err) => {
                    write!(f, "Decoding instruction at {} failed: {}", pc, err)
                }
                JtDetectionError::NotFound => write!(f, "No jump table was detected"),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::memory::MemoryMapFrom;
    use reality_util::to_u8_array;
    use std::convert::TryInto;
    use std::error::Error;

    #[test]
    fn test_jt_discover() -> Result<(), Box<dyn Error>> {
        let start_pc: Pc = 0x8000_0000.try_into()?;

        let map = (&to_u8_array!([
            0x3C010001, // lui             $at,0x0001
            0x0C208D52, // jal             function_00823548
            0x02202025, // move            a0,s1
            0x3C088016, // lui             t0,0x8016
            0x2508FA90, // addiu           t0,t0,-1392
            0x3C010001, // lui             $at,0x0001
            0x34210760, // ori             $at,$at,0x0760
            0x02218021, // addu            s0,s1,$at
            0x961801D4, // lhu             t8,468(s0)
            0x2719FFFD, // addiu           t9,t8,-3
            0x2F210004, // sltiu           $at,t9,4
            0x10200AD6, // beq             $at,$zero,0x00016120 (+11100)
            0x0019C880, // sll             t9,t9,2
            0x3C018000, // lui             $at,0x8000
            0x00390821, // addu            $at,$at,t9
            0x8C390200, // lw              t9,512($at)
            0x03200008, // jr              t9
            0x00000000  // nop
        ]))
            .map_to(start_pc)
            .also_map(
                start_pc + 512,
                &to_u8_array!([0x8040_0000, 0x8040_0020, 0x8040_0040, 0x8040_0060]),
            );

        assert_eq!(
            jt_detector((start_pc + (16 * 4)).try_into()?, &map)?.collect::<Vec<_>>(),
            [0x8040_0000, 0x8040_0020, 0x8040_0040,]
        );

        Ok(())
    }

    #[test]
    fn test_boss_ganon_jt() -> Result<(), Box<dyn Error>> {
        let jt = vec![0; 0x40];

        let map = (&to_u8_array!([
            0x28810064, // slti            $at,a0,100
            0x2861000A, // slti            $at,v1,10
            0x14200009, // bne             $at,$zero,0x808D941C (+40)
            0x00037080, // sll             t6,v1,2
            0x2DA1000A, // sltiu           $at,t5,10
            0x102005DA, // beq             $at,$zero,0x808DAB6C (+5996)
            0x000D6880, // sll             t5,t5,2
            0x3C01808F, // lui             $at,%hi16(data_808F7EF0)
            0x002D0821, // addu            $at,$at,t5
            0x8C2D7EF0, // lw              t5,%lo16(data_808F7EF0)($at)
            0x01A00008, // jr              t5
            0x00000000, // nop
            0x2C61000A, // sltiu           $at,v1,10
            0x102005D2, // beq             $at,$zero,0x808DAB6C (+5964)
            0x3C01808F, // lui             $at,%hi16(data_808F7F18)
            0x002E0821, // addu            $at,$at,t6
            0x8C2E7F18, // lw              t6,%lo16(data_808F7F18)($at)
            0x01C00008, // jr              t6
            0x00000000  // nop
        ]))
            .map_to(0x808D93EC)
            .also_map(0x808F7F18, &jt);

        let _ = jt_detector(0x808D9430.try_into()?, &map).unwrap();

        Ok(())

        // Didn't work since we only checked for SLTIU, and not also SLTI
    }
}
