//#![feature(trace_macros)]
//trace_macros!(true);

pub mod binary;
pub mod block;
pub(crate) mod common;
pub mod flow;
pub mod isa;
pub mod memory;

pub mod assembler {
    pub use reality_mips_assembler::assemble_mips as assemble;
    pub use reality_mips_assembler::*;
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
