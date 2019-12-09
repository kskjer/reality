use crate::server::Server;
use crate::trainer::Trainer;

use reality_mupen_core::emu::Emu;
use reality_mupen_core::modes;

use std::{mem, ptr};

//
// Initialization
//

#[no_mangle]
pub extern "C" fn trainer_init(
    ram: *const u32,
    rom: *const u8,
    rom_size: u32,
    gpr: *const [u64; 32],
    fpr: *const [f64; 32],
    mmu: *const [u32; 32],
) -> Box<Trainer> {
    println!(
        "Starting reality trainer. Ctx size: {}",
        mem::size_of::<Trainer>()
    );

    Box::new(Trainer {
        emu: Emu {
            ram,
            rom,
            rom_size,
            gpr,
            fpr,
            mmu,
        },
        modes: modes::init(),
        server: Server::start(),
    })
}

#[no_mangle]
pub extern "C" fn trainer_deinit(trainer: &mut &mut Trainer) {
    unsafe {
        let trainer = trainer as *mut &mut Trainer;
        let trainer = trainer as *mut *mut Trainer;

        ptr::drop_in_place(*trainer);
        *trainer = ptr::null_mut();
    }

    println!("Reality trainer freed");
}

//
// Emulator events follow
//

/// This function shall be called before the execution of an instruction. The state of the machine
/// will not yet have been changed by this instruction, or its potential delay slot.
#[no_mangle]
pub extern "C" fn trainer_insn_pre(trainer: &mut Trainer, pc: u32) {
    trainer.modes.insn_pre(&trainer.emu, pc);
}

/// This function shall be called after the execution of an instruction. It has already affected the
/// state of the machine. The PC is of the executed instruction, not of the succeeding one.
#[no_mangle]
pub extern "C" fn trainer_insn_post(trainer: &mut Trainer, pc: u32) {
    trainer.modes.insn_post(&trainer.emu, pc);
}

/// When a vblank interrupt occurs, basically when a new frame is ready to be drawn.
#[no_mangle]
pub extern "C" fn trainer_vblank(trainer: &mut Trainer) {
    trainer.server.lend(&mut trainer.emu, &mut trainer.modes);
}

/// When data is transferred via DMA from ROM to RAM
#[no_mangle]
pub extern "C" fn trainer_dma(
    trainer: &mut Trainer,
    pc: u32,
    rom_addr: u32,
    ram_addr: u32,
    len: u32,
) {
    trainer
        .modes
        .dma
        .record(&mut trainer.emu, pc, rom_addr, ram_addr, len);
}
