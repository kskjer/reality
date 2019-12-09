use crate::emu::Emu;
use std::collections::BTreeSet;

pub struct Dma {
    pub dma_seen: BTreeSet<(u32, u32)>,
}

impl Default for Dma {
    fn default() -> Self {
        Self {
            dma_seen: BTreeSet::new(),
        }
    }
}

impl Dma {
    pub fn record(&mut self, _emu: &Emu, _pc: u32, rom_addr: u32, _ram_addr: u32, len: u32) {
        let key = (rom_addr, len);

        if self.dma_seen.contains(&key) {
            return;
        }

        self.dma_seen.insert(key);

        /*if let Some((id, thread)) = self.modes.threads.current() {
            if thread.calls[0].0 == 0x80090070 {
                return;
            }

            println!("DMA: {:08X} -> {:08X}, {} bytes", rom_addr, ram_addr, len);

            /*println!(
                "{:?} {}",
                id,
                thread
                    .calls
                    .iter()
                    .map(|f| format!("{:?}", f.0))
                    .collect::<Vec<_>>()
                    .join(" -> ")
            );*/
        }*/
    }
}
