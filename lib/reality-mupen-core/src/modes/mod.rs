use crate::emu::Emu;
use reality_mips::isa::{self, Instruction, Pc};
use std::convert::TryInto;

mod dma;
mod insn_diff;
mod offset;
mod threads;

pub struct Modes {
    pub threads: threads::Threads,
    pub insn_diff: insn_diff::InsnDiff,
    pub dma: dma::Dma,
    pub offset: offset::Offset,
    last_insn: Vec<Instruction>,
}

pub fn init() -> Modes {
    Modes {
        threads: Default::default(),
        insn_diff: Default::default(),
        dma: Default::default(),
        offset: Default::default(),
        last_insn: Vec::with_capacity(2),
    }
}

fn valid_pc(pc: Pc) -> bool {
    pc >= 0x8000_0000 && pc < 0x8080_0000
}

impl Modes {
    pub fn insn_pre(&mut self, emu: &Emu, pc: u32) {
        let pc = pc.try_into().expect("Invalid PC");

        if !valid_pc(pc) {
            return;
        }

        let insn = isa::decode(emu.read_insn(pc)).expect("Insn decode");

        self.insn_diff.insn_pre(pc);
        self.offset.insn_pre(pc, insn, &|r| emu.gpr(r) as u32);
        self.last_insn.push(insn);
    }

    pub fn insn_post(&mut self, emu: &Emu, pc: u32) {
        let pc = pc.try_into().expect("Invalid PC");

        if !valid_pc(pc) {
            return;
        }

        self.threads
            .insn_post(emu, pc, self.last_insn.pop().expect("No previous insn???"));
    }
}
