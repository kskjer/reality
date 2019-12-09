use crate::isa::regs::*;
use crate::memory::Addr;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UpperImmediate(pub(super) u16);

#[allow(dead_code)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Location<V> {
    ConstU8(u8),
    ConstI16(i16),
    ConstU16(u16),
    ConstU32(u32),
    RegGpr(Gpr),
    RegFpr(Fpr),
    RegMmu(Mmu),
    Hi,
    Lo,
    Displace(V, i16),
    Global(Addr),
}

impl<V> Location<V> {
    pub fn is_const(&self) -> bool {
        use Location::*;

        match self {
            ConstU8(_) | ConstI16(_) | ConstU16(_) | ConstU32(_) => true,
            _ => false,
        }
    }
}
