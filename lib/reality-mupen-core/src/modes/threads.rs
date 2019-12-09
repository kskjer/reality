use crate::emu::Emu;
use reality_mips::isa::{regs::Gpr, Instruction, Instruction::*, Pc};
#[cfg(feature = "serialize")]
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::convert::TryInto;

#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
struct Sp(u32);

#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[derive(PartialEq)]
struct Ra(u32);

#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, PartialEq)]
pub struct ThreadId(u32);

#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[derive(Debug, PartialEq)]
pub struct Frame(pub Pc, Sp, Ra);

#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[derive(Debug, PartialEq)]
pub struct CallStack {
    pub calls: Vec<Frame>,
}

#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[derive(Debug, PartialEq)]
pub struct Threads {
    pub set: Vec<CallStack>,
    resumes: BTreeMap<Sp, ThreadId>,
    current: Option<ThreadId>,
    exception: bool,
}

mod debug_impl {
    use super::*;
    use std::fmt::{Debug, Error, Formatter};

    macro_rules! do_impl {
        ($for:ty, $fmt:expr) => {
            impl Debug for $for {
                fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
                    write!(f, concat!(stringify!($for), "(", $fmt, ")"), self.0)
                }
            }
        };
    }

    do_impl!(Sp, "0x{:08X}");
    do_impl!(Ra, "0x{:08X}");
    do_impl!(ThreadId, "{}");
}

impl Sp {
    fn get(emu: &Emu) -> Sp {
        Sp(emu.gpr(Gpr::Sp) as u32)
    }
}

macro_rules! trace {
    ($x:expr $(, $arg:expr)*) => {
        #[cfg(test)]
        println!($x $(, $arg)*);
    };
}

impl Threads {
    fn thread_by_sp_mut(&mut self, sp: Sp) -> Option<(ThreadId, &mut CallStack)> {
        let (resumes, set) = (&self.resumes, &mut self.set);

        resumes.get(&sp).map(move |&t| (t, &mut set[t.0 as usize]))
    }

    fn call(&mut self, emu: &Emu, tgt: Pc) {
        let sp = Sp::get(emu);
        let frame = Frame(tgt, sp, Ra(emu.gpr(Gpr::Ra) as u32));
        let set = &mut self.set;

        let thread_id = *self.resumes.entry(sp).or_insert_with(|| {
            let id = ThreadId(set.len() as u32);

            set.push(CallStack { calls: Vec::new() });

            println!(
                "{:?}: new thread while calling {:?}, $sp = {:?}",
                id, tgt, sp
            );

            id
        });

        trace!(
            "{:?}: calling {:?} with {:?}, return to {:?}. num calls = {}",
            thread_id,
            tgt,
            sp,
            frame.2,
            set[thread_id.0 as usize].calls.len() + 1
        );

        set[thread_id.0 as usize].calls.push(frame);
        self.resumes.insert(sp, thread_id);
        self.current = Some(thread_id);
    }

    pub fn insn_post(&mut self, emu: &Emu, pc: Pc, insn: Instruction) {
        self.insn_post_inner(emu, pc, Sp::get(emu), insn);
    }

    fn insn_post_inner(&mut self, emu: &Emu, pc: Pc, sp: Sp, insn: Instruction) {
        match (self.exception, insn) {
            _ if pc == 0x8000_0000
                || pc == 0x8000_0080
                || pc == 0x8000_0100
                || pc == 0x8000_0180 =>
            {
                self.exception = true;
            }
            (true, ERET) => {
                self.exception = false;
            }
            // Note: ERET and no exception is here, because some functions deliberately induce
            // an exceptional state, at least in OOT. the offender here is 0x80002030.
            // This is a bit of a kludge.
            // todo: write a test for this
            (false, ERET) => {
                if let Some(sp) = self
                    .current
                    .and_then(|c| self.set[c.0 as usize].calls.last().map(|f| f.1))
                {
                    trace!("ERET translated to return...");

                    self.insn_post_inner(emu, pc, sp, JR(Gpr::Ra));
                }
            }
            (false, JAL(tgt)) => {
                self.call(emu, tgt.full(pc));
            }
            (false, JALR(Gpr::Ra, rs)) => {
                self.call(
                    emu,
                    (emu.gpr(rs) as u32)
                        .try_into()
                        .expect("Failed deriving JALR target"),
                );
            }
            // Returning
            (false, JR(Gpr::Ra)) => {
                if let Some((id, thread)) = self.thread_by_sp_mut(sp) {
                    thread.calls.pop();

                    trace!(
                        "{:?}: Return at {:?} with {:?}, num calls = {}",
                        id,
                        pc,
                        sp,
                        thread.calls.len()
                    );

                    self.current = Some(id);

                    // Remove resumes up to our current stack frame
                    let to_remove: Vec<_> = self
                        .resumes
                        .range(..sp)
                        .filter(|(_, &oid)| id == oid)
                        .map(|(sp, _)| *sp)
                        .collect();

                    trace!("{:?} / {:?} -- removing {:?}", id, sp, to_remove);

                    for x in to_remove {
                        self.resumes.remove(&x);
                    }
                } else {
                    trace!("---- No resume for {:?} at {:?}", sp, pc);
                }
            }
            // Creating a new stack frame
            (false, ADDIU(Gpr::Sp, Gpr::Sp, imm)) => {
                let cur = sp;
                let prev = Sp((cur.0 as i32 - imm as i32) as u32);

                trace!("$sp += {} (b4: {:?}, now: {:?})", imm, prev, cur);

                if let Some((id, thread)) = self.thread_by_sp_mut(prev) {
                    if let Some(Frame(_, sp, _)) = thread.calls.last_mut() {
                        *sp = cur;

                        trace!("{:?}: sp in latest frame adjusted", id);
                    } else {
                        trace!("{:?}: no call stack for {:?}", id, prev);
                    }

                    // Earlier we removed the existing one here, but sometimes the SP can be
                    // saved to S8 and restored later. S8 = frame pointer.
                    self.resumes.insert(cur, id);
                    self.current = Some(id);

                    trace!("{:?}: resume updated, {:?} -> {:?}", id, prev, cur);
                } else {
                    trace!("---- no resume found for {:?} -> {:?}", prev, cur);
                }
            }
            _ => {}
        }
    }

    pub fn current(&self) -> Option<(ThreadId, &CallStack)> {
        self.current
            .map(|t| (t, self.set.get(t.0 as usize).unwrap()))
    }
}

impl Default for Threads {
    fn default() -> Self {
        Threads {
            set: Vec::new(),
            current: None,
            resumes: BTreeMap::new(),
            exception: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::emu::Emu;
    use reality_mips::assembler::*;
    use reality_mips::isa::{self, Displace, PseudoInstruction};
    use std::convert::TryFrom;
    use std::iter;

    fn interp(
        pc: Pc,
        ram: &mut [u32],
        regs: &mut [u64; 32],
        post: &mut impl FnMut(Pc, u32),
    ) -> (Pc, u32) {
        macro_rules! reg {
            ($r:expr) => {
                regs[{
                    let r: usize = u8::from($r).into();

                    r
                }]
            };
        }

        let insn = ram[(u32::from(pc) as usize & 0x007F_FFFF) / 4];
        let decoded = isa::decode(insn).expect("Couldn't interp");

        match decoded {
            _ if decoded.pseudo() == Some(PseudoInstruction::NOP) => {}
            JAL(tgt) => {
                interp(pc.next(), ram, regs, post);
                regs[31] = u32::from(pc.next_n(2)).into();

                post(pc, insn);

                return (tgt.full(pc), insn);
            }
            J(tgt) => {
                interp(pc.next(), ram, regs, post);
                post(pc, insn);

                return (tgt.full(pc), insn);
            }
            JR(rs) => {
                interp(pc.next(), ram, regs, post);

                post(pc, insn);

                return ((reg!(rs) as u32).try_into().unwrap(), insn);
            }
            OR(rd, rs, rt) => {
                reg!(rd) = reg!(rs) | reg!(rt);
            }
            LW(rt, Displace(offs, base)) => {
                reg!(rt) = ram
                    [((reg!(base) as i32 + i16::from(offs) as i32) as usize & 0x007F_FFFF) / 4]
                    as i32 as i64 as u64;
            }
            SW(rt, Displace(offs, base)) => {
                ram[((reg!(base) as i32 + i16::from(offs) as i32) as usize & 0x007F_FFFF) / 4] =
                    reg!(rt) as u32;
            }
            ADDIU(rt, rs, imm) => {
                reg!(rt) = (reg!(rs) as i64 + imm as i64) as u64;
            }
            x => panic!("{:?} unhandled", x),
        }

        post(pc, insn);

        (pc.next(), insn)
    }

    #[test]
    fn test_single_thread() {
        let (mut dummy_a, mut dummy_b) = (Emu::dummy(), Emu::dummy());
        let (emu_a, emu_b) = (dummy_a.emu(), dummy_b.emu());
        let mut threads: Threads = Default::default();

        const EP: u32 = 0x8000_0400;

        let insns = assemble_mips!(EP, {
            j           xxx         // 0
            nop                     // 1
        start:
            move        a0,zero     // 2
            jr          ra          // 3
            addiu       sp,sp,32    // 4

        test:
            addiu       sp,sp,-16   // 5
            sw          ra,0(sp)    // 6
            jal         test_2      // 7
            nop                     // 8
            jal         test_4      // 9
            nop                     // 10
            lw          ra,0(sp)    // 11
            jr          ra          // 12
            addiu       sp,sp,16    // 13

        test_2:
            jr          ra          // 14
            nop                     // 15

        test_3:
            jr          ra          // 16
            nop                     // 17

        test_4:
            addiu       sp,sp,-24
            sw          ra,0(sp)
            jal         test_3
            nop
            lw          ra,0(sp)
            addiu       sp,sp,24
            jr          ra
            nop
        xxx:
            addiu       sp,sp,-32
            jal         test_3
            nop
            jal         test
            nop
            j           start
            nop
        });

        for x in &mut [&mut dummy_a, &mut dummy_b] {
            x.write(EP, &insns);
        }

        let sp = u8::from(Gpr::Sp) as usize;

        dummy_a.gpr[sp] = 0x8040_0000;
        dummy_b.gpr[sp] = 0x8030_0000;

        let start = Pc::try_from(EP).unwrap();
        let stop = start.next_n(3);

        [(&mut dummy_a, &emu_a), (&mut dummy_b, &emu_b)]
            .iter_mut()
            .map(|(dummy, emu)| {
                iter::successors(Some(start), |pc| {
                    Some({
                        let (new_pc, _insn) =
                            interp(*pc, &mut dummy.ram, &mut dummy.gpr, &mut |pc, insn| {
                                threads.insn_post(emu, pc, isa::decode(insn).unwrap());
                            });

                        new_pc
                    })
                })
                .take_while(|pc| *pc != stop)
                .last()
            })
            .last();

        assert_eq!(
            threads,
            Threads {
                set: vec![
                    CallStack {
                        calls: vec![Frame(
                            0x80000414.try_into().unwrap(),
                            Sp(0x803FFFD0),
                            Ra(0x8000040C),
                        ),],
                    },
                    CallStack {
                        calls: vec![Frame(
                            0x80000414.try_into().unwrap(),
                            Sp(0x802FFFD0),
                            Ra(0x8000040C),
                        ),],
                    },
                ],
                resumes: [(Sp(0x802FFFD0), ThreadId(1)), (Sp(0x803FFFD0), ThreadId(0)),]
                    .iter()
                    .copied()
                    .collect(),
                current: Some(ThreadId(1),),
                exception: false,
            }
        );
    }
}
