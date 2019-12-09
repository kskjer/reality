use crate::server::Server;

use reality_mupen_core::emu::Emu;
use reality_mupen_core::modes::Modes;

pub struct Trainer {
    pub(super) emu: Emu,
    pub(super) modes: Modes,
    pub(super) server: Server,
}
