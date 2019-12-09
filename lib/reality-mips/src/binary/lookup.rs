use super::relocation::Relocation;
use super::symbol::Symbol;
use crate::binary::RelocationType;
use crate::isa::Pc;
use std::collections::BTreeMap;
use std::marker::PhantomData;

pub trait Symbols {
    type Output;

    fn sym_at(&self, pc: Pc) -> Option<Self::Output>;
}

pub trait Relocations {
    type Output;

    fn reloc_at(&self, pc: Pc) -> Option<Self::Output>;
}

impl<'a> Symbols for &'a BTreeMap<Pc, Symbol> {
    type Output = &'a Symbol;

    fn sym_at(&self, pc: Pc) -> Option<Self::Output> {
        self.get(&pc)
    }
}

impl<'a, S> Relocations for &'a BTreeMap<Pc, Relocation<S>> {
    type Output = &'a Relocation<S>;

    fn reloc_at(&self, pc: Pc) -> Option<Self::Output> {
        self.get(&pc)
    }
}

pub struct NoLookup;

impl<'a> Symbols for &'a NoLookup {
    type Output = &'static str;

    fn sym_at(&self, _pc: Pc) -> Option<Self::Output> {
        None
    }
}

impl<'a> Relocations for &'a NoLookup {
    type Output = &'a Relocation<&'static str>;

    fn reloc_at(&self, _pc: Pc) -> Option<Self::Output> {
        None
    }
}

pub struct Lookup {
    syms: BTreeMap<Pc, Symbol>,
    relocs: BTreeMap<Pc, Relocation<String>>,
}

impl Lookup {
    pub fn add_sym(&mut self, pc: Pc, sym: Symbol) -> Option<Symbol> {
        self.syms.insert(pc, sym)
    }

    pub fn add_reloc(&mut self, pc: Pc, reloc: Relocation<String>) -> Option<Relocation<String>> {
        self.relocs.insert(pc, reloc)
    }
}

pub struct NoSyms;
pub struct NoRelocs;
pub struct HaveSyms;
pub struct HaveRelocs;

pub struct LookupBuilder<SymState = NoSyms, RelocState = NoRelocs> {
    syms: Option<BTreeMap<Pc, Symbol>>,
    relocs: Option<BTreeMap<Pc, Relocation<String>>>,
    _phantom: PhantomData<(SymState, RelocState)>,
}

impl LookupBuilder<NoSyms, NoRelocs> {
    pub fn new() -> Self {
        LookupBuilder {
            syms: None,
            relocs: None,
            _phantom: PhantomData,
        }
    }
}

impl<R> LookupBuilder<NoSyms, R> {
    pub fn with_syms<I, S>(self, syms: I) -> LookupBuilder<HaveSyms, R>
    where
        I: Iterator<Item = (Pc, S)>,
        S: Into<String>,
    {
        LookupBuilder {
            syms: Some(syms.map(|(pc, s)| (pc, Symbol(s.into()))).collect()),
            relocs: self.relocs,
            _phantom: PhantomData,
        }
    }
}

impl<S> LookupBuilder<S, NoRelocs> {
    pub fn with_relocs<I, R>(self, relocs: I) -> LookupBuilder<S, HaveRelocs>
    where
        I: Iterator<Item = (Pc, RelocationType, R)>,
        R: Into<String>,
    {
        LookupBuilder {
            syms: self.syms,
            relocs: Some(
                relocs
                    .map(|(pc, rtype, sym)| (pc, Relocation(rtype, sym.into())))
                    .collect(),
            ),
            _phantom: PhantomData,
        }
    }
}

impl<S, R> LookupBuilder<S, R> {
    pub fn build(self) -> Lookup {
        Lookup {
            syms: self.syms.unwrap_or_else(|| BTreeMap::new()),
            relocs: self.relocs.unwrap_or_else(|| BTreeMap::new()),
        }
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_builder() {
        unimplemented!();
    }
}
