use crate::binary::{NoLookup, Relocations, Symbols};
use crate::isa::Pc;

pub struct DisassemblyBuilder<P, S, R> {
    word: u32,
    pc: P,
    sym_lookup: S,
    relocs_lookup: R,
    left_col: u8,
}

impl<P, S, R> DisassemblyBuilder<P, S, R>
where
    S: Symbols,
    R: Relocations,
{
    pub fn with_syms<X>(self, syms: X) -> DisassemblyBuilder<P, X, R>
    where
        X: Symbols,
    {
        DisassemblyBuilder {
            word: self.word,
            sym_lookup: syms,
            relocs_lookup: self.relocs_lookup,
            left_col: self.left_col,
            pc: self.pc,
        }
    }

    pub fn with_relocs<X>(self, relocs: X) -> DisassemblyBuilder<P, S, X>
    where
        X: Relocations,
    {
        DisassemblyBuilder {
            word: self.word,
            sym_lookup: self.sym_lookup,
            relocs_lookup: relocs,
            left_col: self.left_col,
            pc: self.pc,
        }
    }

    pub fn with_insn_width(self, width: u8) -> DisassemblyBuilder<P, S, R> {
        DisassemblyBuilder {
            left_col: width,
            ..self
        }
    }

    pub fn at(self, pc: Pc) -> DisassemblyBuilder<Pc, S, R> {
        DisassemblyBuilder {
            word: self.word,
            sym_lookup: self.sym_lookup,
            relocs_lookup: self.relocs_lookup,
            left_col: self.left_col,
            pc,
        }
    }
}

pub fn disassemble(word: u32) -> DisassemblyBuilder<(), &'static NoLookup, &'static NoLookup> {
    DisassemblyBuilder {
        word,
        relocs_lookup: &NoLookup,
        sym_lookup: &NoLookup,
        pc: (),
        left_col: 12,
    }
}

pub struct Wrapper<Lookup, Pc, Needle>(Lookup, Pc, Needle);

mod disassembler_build_fmt {
    use super::super::operand::*;
    use super::disassembler_impl::write_disassembly;
    use super::DisassemblyBuilder;
    use super::Wrapper;
    use crate::binary::{Relocations, Symbols};
    use crate::isa::Displace;

    use std::fmt::{Display, Error, Formatter};

    impl<S, R, P> Display for DisassemblyBuilder<P, S, R>
    where
        S: Symbols + Copy,
        R: Relocations + Copy,
        P: Copy,
        Wrapper<S, P, JumpTarget>: Display,
        Wrapper<S, P, BranchTarget>: Display,
        Wrapper<R, P, Displace>: Display,
        Wrapper<R, P, u16>: Display,
    {
        fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
            write_disassembly(
                f,
                self.word,
                self.left_col as usize,
                self.pc,
                self.sym_lookup,
                self.relocs_lookup,
            )
        }
    }
}

mod disassembler_fmt {
    use super::super::operand::*;
    use super::Wrapper;
    use crate::binary::{Relocation, RelocationType, Relocations, Symbols};
    use crate::isa::{Displace, Pc};
    use std::fmt::{Display, Error, Formatter};

    impl Display for CacheOp {
        fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
            write!(f, "0x{:02x}", self.0)
        }
    }

    impl Display for ShiftAmount {
        fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
            write!(f, "{}", self.0)
        }
    }

    impl Display for SyscallCode {
        fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
            write!(f, "0x{:X}", self.0)
        }
    }

    impl Display for BreakCode {
        fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
            write!(f, "0x{:X}", self.0)
        }
    }

    impl<D: Display, S: Symbols<Output = D>> Display for Wrapper<S, Pc, BranchTarget> {
        fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
            if let Some(sym) = self.0.sym_at(self.2.full(self.1)) {
                write!(f, "{}", sym)
            } else {
                write!(f, "0x{:08X}", self.2.full(self.1).0)
            }
        }
    }

    impl<D: Display, S: Symbols<Output = D>> Display for Wrapper<S, (), BranchTarget> {
        fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
            write!(f, "{:+}", self.2.delta())
        }
    }

    impl<D: Display, S: Symbols<Output = D>> Display for Wrapper<S, Pc, JumpTarget> {
        fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
            let full = self.2.full(self.1);

            if let Some(sym) = self.0.sym_at(full) {
                write!(f, "{}", sym)
            } else {
                write!(f, "0x{:08X}", full.0)
            }
        }
    }

    impl<D: Display, S: Symbols<Output = D>> Display for Wrapper<S, (), JumpTarget> {
        fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
            write!(f, "0x{:08X}", self.2.full(Pc(0)).0)
        }
    }

    impl<'a, D: 'a + Display, R: Relocations<Output = &'a Relocation<D>>> Display
        for Wrapper<R, Pc, Displace>
    {
        fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
            if let Some(Relocation(RelocationType::Lo16, sym)) = self.0.reloc_at(self.1) {
                write!(f, "%lo({})({})", sym, self.1)
            } else {
                write!(f, "{}", self.2)
            }
        }
    }

    impl<'a, D: 'a + Display, R: Relocations<Output = &'a Relocation<D>>> Display
        for Wrapper<R, (), Displace>
    {
        fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
            write!(f, "{}", self.2)
        }
    }

    impl<'a, D: 'a + Display, R: Relocations<Output = &'a Relocation<D>>> Display
        for Wrapper<R, Pc, u16>
    {
        fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
            if let Some(Relocation(RelocationType::Hi16, sym)) = self.0.reloc_at(self.1) {
                write!(f, "%hi({})", sym)
            } else {
                write!(f, "0x{:04X}", self.2)
            }
        }
    }

    impl<'a, D: 'a + Display, R: Relocations<Output = &'a Relocation<D>>> Display
        for Wrapper<R, (), u16>
    {
        fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
            write!(f, "0x{:04X}", self.2)
        }
    }
}

mod disassembler_impl {
    use super::super::decode;
    use super::super::operand::*;
    use super::super::Displace;
    use super::super::Instruction::*;
    use super::super::PseudoInstruction::*;
    use super::Wrapper;
    use crate::binary::{Relocations, Symbols};
    use std::fmt::{self, Display, Formatter};

    include!(concat!(env!("OUT_DIR"), "/disassemble.rs"));
}

#[cfg(test)]
mod tests {
    use super::disassemble;
    use crate::binary::Relocation;
    use crate::isa::Pc;
    use std::collections::BTreeMap;

    #[test]
    fn test_disasm() {
        let relocs = BTreeMap::<Pc, Relocation<&'static str>>::new();
        let syms = BTreeMap::new();

        let dis = disassemble(0x27BD_FFFF)
            .at(Pc(0x8080_0000))
            .with_relocs(&relocs)
            .with_syms(&syms);

        //format!("{}", Wrapper(&NoLookup, Pc(0), 0u16));
        assert_eq!("asdasd", format!("{}", dis));
        //let x = Box::new((&dis).fmt);
    }
}
