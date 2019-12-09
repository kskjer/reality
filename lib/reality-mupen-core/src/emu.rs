use reality_mips::isa::{regs::Gpr, Pc};
use std::convert::TryInto;

pub type Registers<T> = [T; 32];

pub const MEM_SIZE: usize = 8 * 1024 * 1024;
pub const MEM_SIZE_WORDS: usize = MEM_SIZE / 4;

pub struct Emu {
    pub ram: *const u32,
    pub rom: *const u8,
    pub rom_size: u32,
    pub gpr: *const Registers<u64>,
    pub fpr: *const Registers<f64>,
    pub mmu: *const Registers<u32>,
}

pub struct DummyEmu {
    pub ram: Box<[u32]>,
    pub rom: Box<[u8]>,
    pub gpr: Registers<u64>,
    pub fpr: Registers<f64>,
    pub mmu: Registers<u32>,
}

impl Emu {
    pub fn read_insn(&self, pc: Pc) -> u32 {
        let idx: usize = (pc - Pc::MEM_START).insns().try_into().unwrap();

        unsafe { *self.ram.add(idx) }
    }

    pub fn gpr(&self, reg: Gpr) -> u64 {
        let id: u8 = reg.into();

        unsafe { (*self.gpr)[id as usize] }
    }

    pub fn dummy() -> DummyEmu {
        DummyEmu {
            ram: vec![0; 8 * 1024 * 1024 / 4].into_boxed_slice(),
            rom: vec![0; 8 * 1024 * 1024].into_boxed_slice(),
            gpr: [0; 32],
            fpr: [0.0; 32],
            mmu: [0; 32],
        }
    }
}

impl DummyEmu {
    pub fn emu(&self) -> Emu {
        Emu {
            ram: self.ram.as_ptr(),
            rom: self.rom.as_ptr(),
            rom_size: self.rom.len() as u32,
            gpr: &self.gpr as *const Registers<_>,
            fpr: &self.fpr as *const Registers<_>,
            mmu: &self.mmu as *const Registers<_>,
        }
    }

    pub fn write(&mut self, at: u32, words: &[u32]) {
        let idx = (at as usize - 0x8000_0000) / 4;

        self.ram[idx..(idx + words.len())].copy_from_slice(words);
    }
}
