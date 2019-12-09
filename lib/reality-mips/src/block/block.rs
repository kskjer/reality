use crate::impl_counter;
use crate::isa::{self, Instruction, Pc};
use crate::memory::AddrVec;
use crate::memory::MemoryMap;
use reality_util::graph::{dot, SccNode};
use std::borrow::Borrow;
use std::collections::{BTreeMap, BTreeSet};
use std::convert::TryInto;
use std::fmt::{Error, Formatter};
use std::num::NonZeroU16;
use std::{fmt, u16};

impl_counter!(BlockId);

// Used for maintaining ordering of instructions within a block. This can be
// different than the PC if the first instruction in the block resides in the
// delay slot of a branch likely instruction.
impl_counter!(BlockOrd);

#[derive(Debug)]
pub enum TopoBlock<B> {
    Block(B, BlockInfo),
    Component(Vec<TopoBlock<B>>),
}

#[derive(Debug)]
pub struct BlockInfo {
    is_terminal: bool,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockEdge(BlockEdgeDirection, BlockId, BlockId);

#[derive(Clone, Debug, PartialEq)]
pub enum BranchAction {
    Taken,
    NotTaken,
    JumpTable(Vec<JumpTableIndex>),
    Unconditional,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct JumpTableIndex(pub(super) u16);

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum BlockEdgeDirection {
    InEdge,
    OutEdge,
}

#[derive(Debug)]
pub struct FunctionBlocks {
    pub(super) base_pc: Pc,
    pub(super) upper_pc: Pc,
    pub(super) num_blocks: usize,
    pub(super) in_edges: BTreeSet<(BlockId, BlockId)>,
    pub(super) out_edges: BTreeMap<(BlockId, BlockId), BranchAction>,
    pub(super) insns: BTreeMap<(BlockId, BlockOrd), Pc>,
    pub components: Vec<SccNode<BlockId>>,
    pub calls: BTreeSet<Pc>,
}
// todo: somehow handle aborts, aka BREAK instructions. they are counted as leaves for now.

impl FunctionBlocks {
    pub fn start(&self) -> Pc {
        self.base_pc
    }
    pub fn end(&self) -> Pc {
        self.upper_pc
    }

    pub fn out_edges(&self, id: BlockId) -> impl Iterator<Item = (BlockId, &BranchAction)> {
        self.out_edges
            .range((id, BlockId::MIN)..(id.next(), BlockId::MIN))
            .map(|((_, d), a)| (*d, a))
    }

    pub fn in_edges(&self, id: BlockId) -> impl Iterator<Item = BlockId> + '_ {
        // todo: what about edges coming from inside a component?
        self.in_edges
            .range((id, BlockId::MIN)..(id.next(), BlockId::MIN))
            .map(|(_dst, src)| *src)
    }

    pub fn num_blocks(&self) -> usize {
        self.num_blocks
    }

    pub fn block_ids(&self) -> impl Iterator<Item = BlockId> {
        BlockId::MIN.next_n(self.num_blocks.try_into().unwrap())
    }

    pub fn insns<'a>(
        &'a self,
        id: BlockId,
        mem: &'a impl MemoryMap,
    ) -> impl Iterator<Item = (Instruction, Pc)> + 'a {
        let range = (id, BlockOrd::MIN)..(id.next(), BlockOrd::MIN);

        // Neither memory read nor decode should fail here, since this has already been done.
        // todo: should have already decoded instructions in the block.insns...
        self.insns
            .range(range)
            .map(move |((_, _), &pc)| (isa::decode(mem.read_u32(pc).unwrap()).unwrap(), pc))
    }
}

struct FunctionsBlocksDisplay<M, B>(M, B);

impl FunctionBlocks {
    pub fn display<'a, M>(&'a self, memory: &'a M) -> impl fmt::Display + 'a
    where
        M: MemoryMap,
    {
        FunctionsBlocksDisplay(memory, self)
    }
}

impl<'a, M, B> fmt::Display for FunctionsBlocksDisplay<&'a M, B>
where
    B: Borrow<FunctionBlocks>,
    M: MemoryMap,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use crate::isa::disassemble;

        let mem = self.0;
        let blocks = self.1.borrow();

        let lookup = blocks.insns.iter().fold(
            AddrVec::new_from(blocks.base_pc, blocks.upper_pc, None),
            |mut acc, ((blk, ord), &pc)| {
                acc[pc] = Some((blk, ord));

                acc
            },
        );

        let mut last_block = None;

        for (i, pc) in blocks.base_pc.until(blocks.upper_pc).enumerate() {
            let our_block = lookup[pc].map(|(blk, _)| blk);

            if last_block != our_block && i != 0 {
                writeln!(f, "--------------------------------------------------------------------------------")?;
            }

            let word = mem.read_u32(pc).unwrap();

            let connections = [
                // in edges, if any
                match lookup[pc].map(|(blk, ord)| {
                    (
                        blk,
                        ord.get(),
                        blocks
                            .in_edges(*blk)
                            .map(|b| format!("{}", b.get()))
                            .collect::<Vec<_>>(),
                    )
                }) {
                    Some((_blk, 1, p)) if p.len() > 0 => format!("v {}", p.join(", ")),
                    _ => String::new(),
                },
                // out edges, if any
                our_block
                    .and_then(|blk| {
                        blocks
                            .insns
                            .range((*blk, BlockOrd::MIN)..(blk.next(), BlockOrd::MIN))
                            .rev()
                            .take(1)
                            .filter_map(|((blk, ord), _)| {
                                lookup[pc].and_then(|(_, cur_ord)| {
                                    let edges = blocks
                                        .out_edges(*blk)
                                        .map(|(b, _)| b.get().to_string())
                                        .collect::<Vec<_>>()
                                        .join(", ");

                                    if ord == cur_ord && edges.len() > 0 {
                                        Some(format!("^ {}", edges))
                                    } else {
                                        None
                                    }
                                })
                            })
                            .nth(0)
                    })
                    .unwrap_or_default(),
            ]
            .iter()
            .filter(|x| x.len() > 0)
            .cloned()
            .collect::<Vec<_>>();

            writeln!(
                f,
                "{:>4}    {:>3}    {:>3}    {}    {:08X}    {:<32}   {}",
                i,
                our_block.map(|b| format!("{}", b)).unwrap_or_default(),
                lookup[pc]
                    .map(|(_, ord)| format!("{}", ord))
                    .unwrap_or_default(),
                pc,
                word,
                format!("{}", disassemble(word).at(pc)),
                connections.join("   ")
            )?;

            last_block = our_block;
        }

        writeln!(f)?;
        writeln!(f, "Calls = {:#?}", blocks.calls)?;

        writeln!(f)?;
        writeln!(f, "Components = {:#?}", blocks.components)?;

        writeln!(f)?;
        writeln!(
            f,
            "Edges = {}",
            dot(
                BlockId::MIN,
                &|v| v.get().into(),
                &|s, d| format!("{:?}", blocks.out_edges[&(s, d)]),
                &|v| blocks
                    .out_edges
                    .range((v, BlockId::MIN)..(v.next(), BlockId::MIN))
                    .map(|((_s, d), _)| *d)
            )
        )?;

        Ok(())
    }
}

pub fn create_super_dot(blocks: &FunctionBlocks, mem: &impl MemoryMap) -> String {
    let declare_nodes = blocks
        .block_ids()
        .map(|src| {
            let content = blocks
                .insns
                .iter()
                .filter(|(&(blk, _), _)| blk == src)
                .enumerate()
                .map(|(i, ((_, _), &pc))| {
                    let disasm = format!(
                        "{}",
                        isa::disassemble(mem.read_u32(pc).expect("Read")).at(pc)
                    );

                    if i == 0 {
                        format!(
                            "<B>{} / BLOCK {}</B>\
                             <BR ALIGN=\"LEFT\"/>\
                             <BR ALIGN=\"LEFT\"/>\
                             {}",
                            pc, src, disasm
                        )
                    } else {
                        disasm
                    }
                })
                .collect::<Vec<_>>()
                .join("\n<BR ALIGN=\"LEFT\"/>\n");

            format!("{} [label=<{} <BR ALIGN=\"LEFT\"/>>];", src, content)
        })
        .collect::<Vec<_>>()
        .join("\n");

    let edges = blocks
        .out_edges
        .iter()
        .map(|((src, dst), edge)| {
            format!(
                "{} -> {} {}",
                src,
                dst,
                match edge {
                    BranchAction::Unconditional => "".to_owned(),
                    x => format!("[xlabel=<<I>  {:?}  </I>>]", x),
                }
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    format!(
        "\
         digraph G {{\
         graph [splines=ortho, nodesep=1];\
         node [shape=box fontname=\"courier new\"];\
         {}\
         {}\
         }}",
        declare_nodes, edges
    )
}
