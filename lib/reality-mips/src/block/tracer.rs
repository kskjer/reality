use crate::block::jump_table::jt_detector;
use crate::block::{
    tracer_algo::tracer_algo, BlockId, BlockOrd, BranchAction, FunctionBlocks, JumpTableIndex,
};
use crate::isa::regs::Gpr;
use crate::isa::Pc;
use crate::isa::{decode, DecodeError, Instruction};
use crate::memory::{LookupError, MemoryMap};

use reality_util::graph::derive_scc_nodes;
use reality_util::{graph, IterGroupBy};
use std::collections::{BTreeMap, BTreeSet};
use std::convert::{TryFrom, TryInto};
use std::num::NonZeroU16;
use std::{cmp, iter};

#[derive(Debug)]
pub enum TraceError {
    Lookup(Pc, LookupError),
    Decode(Pc, u32, DecodeError),
    BlockIdOverflow(usize),
}

#[derive(Debug, PartialEq)]
pub enum DecodedBranch {
    NoBranch,
    Unconditional(Pc),
    Branch(bool, Pc),
    Branches(Vec<(Pc, BranchAction)>),
    Return,
    Abort,
}

pub fn trace(start: Pc, memory: &impl MemoryMap) -> Result<FunctionBlocks, TraceError> {
    let mut calls = BTreeSet::new();

    let mut groups = BTreeMap::new();
    let mut edges = BTreeMap::new();

    /*println!(
        "Pre-simplify dot:\n{}",
        graph::dot(
            start,
            &|pc| (pc - start).insns().try_into().unwrap(),
            &|pc| insn_to_successors(start, pc, memory)
                .unwrap()
                .map(|(pc, _)| pc)
        )
    );*/

    tracer_algo(
        start,
        &|pc| {
            (pc - start)
                .insns()
                .try_into()
                .unwrap_or_else(|_| panic!("{} - {} overflowed while tracing", pc, start))
        },
        // todo: wonder if it's possible to cache decoded instructions here? affects FunctionBlocks
        &mut |pc| {
            // todo: lazy kludge
            match memory.read_u32(pc).ok().and_then(|w| decode(w).ok()) {
                Some(Instruction::JAL(tgt)) => {
                    calls.insert(tgt.full(pc));
                }
                _ => {}
            }

            insn_to_successors(start, pc, memory)
        },
        &mut |src, dst, e| {
            edges.insert((src, dst), e);
        },
        &mut |grp, insns| {
            groups.insert(grp, insns);
        },
    )?;

    //println!("groups = {:?}", groups);

    /*println!(
        "Post-simplify dot:\n{}",
        graph::dot(
            start,
            &|pc| (pc - start).insns().try_into().unwrap(),
            &|v| {
                edges
                    .range((v, Pc::MIN)..(v.next(), Pc::MIN))
                    .map(|((_src, dst), _)| *dst)
            }
        )
    );*/

    let components = graph::scc(
        groups.keys().cloned(),
        &|v| Ok((v - start).insns().try_into().unwrap()),
        &|v| {
            edges
                .range((v, Pc::MIN)..(v.next(), Pc::MIN))
                .map(|((_src, dst), _)| *dst)
        },
    )?;

    let id_map = components
        .iter()
        .flat_map(|c| c.iter())
        .rev()
        .enumerate()
        .map(|(i, v)| {
            BlockId::new(i + 1)
                .map(|i| (*v, i))
                .ok_or(TraceError::BlockIdOverflow(i + 1))
        })
        .collect::<Result<BTreeMap<_, _>, _>>()?;

    let (in_edges, out_edges) = edges.into_iter().fold(
        (BTreeSet::new(), BTreeMap::new()),
        |(mut in_edges, mut out_edges), ((src, dst), action)| {
            let (src, dst) = (id_map[&src], id_map[&dst]);

            in_edges.insert((dst, src));
            out_edges.insert((src, dst), action);

            (in_edges, out_edges)
        },
    );

    let num_blocks = groups.len();

    let (upper_pc, insns) = groups.into_iter().fold(
        (start, BTreeMap::new()),
        |(mut upper_pc, mut insns), (group, group_insns)| {
            let group = id_map[&group];

            for (i, pc) in group_insns.into_iter().enumerate() {
                let ord = BlockOrd(NonZeroU16::new((i + 1).try_into().unwrap()).unwrap());

                upper_pc = cmp::max(upper_pc, pc.next());

                insns.insert((group, ord), pc);
            }

            (upper_pc, insns)
        },
    );

    Ok(FunctionBlocks {
        components: derive_scc_nodes(
            BlockId::MIN.next_n(num_blocks as u16),
            &|v| -> Result<_, ()> { Ok(v.get().into()) },
            &|v| {
                out_edges
                    .range((v, BlockId::MIN)..(v.next(), BlockId::MIN))
                    .map(|((_src, dst), _)| *dst)
            },
        )
        .unwrap(),
        base_pc: start,
        in_edges,
        out_edges,
        num_blocks,
        upper_pc,
        insns,
        calls,
    })
}

pub fn trace_all(start: Pc, memory: &impl MemoryMap) -> impl Iterator<Item = FunctionBlocks> + '_ {
    iter::successors(trace(start, memory).ok(), move |prev| {
        let mut next_pc = prev.upper_pc;

        // Ignore the NOPs at the end of a function, for alignment
        while memory.read_u32(next_pc).ok()? == 0 {
            next_pc = next_pc.next();
        }

        trace(next_pc, memory).ok()
    })
}

fn decode_branch(pc: Pc, word: u32, memory: &impl MemoryMap) -> Result<DecodedBranch, TraceError> {
    use BranchAction::*;
    use DecodedBranch::*;
    use Instruction::*;

    Ok(
        match decode(word).map_err(|e| TraceError::Decode(pc, word, e))? {
            J(target) => DecodedBranch::Unconditional(target.full(pc)),
            BEQ(rs, rt, offset) if rs == rt => DecodedBranch::Unconditional(offset.full(pc)),
            BC1F(offset)
            | BC1T(offset)
            | BEQ(_, _, offset)
            | BGEZ(_, offset)
            | BGTZ(_, offset)
            | BLEZ(_, offset)
            | BLTZ(_, offset)
            | BNE(_, _, offset) => Branch(false, offset.full(pc)),
            BC1FL(offset)
            | BC1TL(offset)
            | BEQL(_, _, offset)
            | BGEZL(_, offset)
            | BGTZL(_, offset)
            | BLEZL(_, offset)
            | BLTZL(_, offset)
            | BNEL(_, _, offset) => Branch(true, offset.full(pc)),
            JR(rs) if rs != Gpr::Ra => {
                let targets_by_index = jt_detector(pc, memory)
                    .into_iter()
                    .flat_map(|v| v)
                    .enumerate()
                    .map(|(i, t)| {
                        (
                            u16::try_from(i).expect("Jump table index overflowed u16"),
                            Pc::try_from(t).expect("Invalid PC while reading jump table"),
                        )
                    })
                    .group_by(|(_, t)| *t, |(i, _)| JumpTableIndex(i));

                Branches(
                    targets_by_index
                        .into_iter()
                        .map(|(target, indexes)| (target, JumpTable(indexes)))
                        .collect(),
                )
            }
            JR(rs) if rs == Gpr::Ra => Return,
            ERET => Abort, // interrupt handlers
            BREAK(_) => Abort,
            _ => NoBranch,
        },
    )
}

fn insn_to_successors(
    start: Pc,
    pc: Pc,
    map: &impl MemoryMap,
) -> Result<impl Iterator<Item = (Pc, BranchAction)>, TraceError> {
    let read_branch = |pc: Pc| {
        decode_branch(
            pc,
            map.read_u32(pc).map_err(|e| TraceError::Lookup(pc, e))?,
            map,
        )
    };

    let (before, now) = (
        if pc > start {
            Some(read_branch(pc.prev())?)
        } else {
            None
        },
        read_branch(pc)?,
    );

    Ok(match (before, now) {
        (Some(DecodedBranch::Unconditional(tgt)), _) => vec![(tgt, BranchAction::Unconditional)],
        (_, DecodedBranch::Branch(true, _tgt)) => vec![
            (pc.next(), BranchAction::Taken),
            (pc.next().next(), BranchAction::NotTaken),
        ],
        (Some(DecodedBranch::Branch(true, tgt)), _) => vec![(tgt, BranchAction::Unconditional)],
        (Some(DecodedBranch::Branch(false, tgt)), _) => vec![
            (tgt, BranchAction::Taken),
            (pc.next(), BranchAction::NotTaken),
        ],
        (Some(DecodedBranch::Return), _) | (_, DecodedBranch::Abort) => Vec::new(),
        (Some(DecodedBranch::Branches(b)), _) => b.into_iter().collect(),
        _ => vec![(pc.next(), BranchAction::Unconditional)],
    }
    .into_iter())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assembler::*;
    use crate::isa::Pc;
    use crate::memory::MemoryMapFrom;
    use reality_util::Slice32Tobytes;

    fn do_trace(pc: Pc, words: &[u32]) {
        let map = words.to_be_bytes().map_to(pc);

        println!("{}", trace(pc, &map).expect("Trace").display(&map));
    }

    #[test]
    fn test_tracer() {
        /*
                For posterity:

          0      1      1    0x80000000    00000000    nop
          1      1      2    0x80000004    10000006    beq             $zero,$zero,0x80000020
          2      1      3    0x80000008    00000000    nop
        --------------------------------------------------------------------------------
          3     13      2    0x8000000C    03E00008    jr              $ra
          4     13      3    0x80000010    24020013    addiu           v0,$zero,19
        --------------------------------------------------------------------------------
          5                  0x80000014    00000000    nop
          6                  0x80000018    00000000    nop
          7                  0x8000001C    00000000    nop
        --------------------------------------------------------------------------------
          8      1      4    0x80000020    00000000    nop
          9      1      5    0x80000024    00000000    nop
         10      1      6    0x80000028    00000000    nop
        --------------------------------------------------------------------------------
         11      2      1    0x8000002C    8C880000    lw              t0,0(a0)
         12      2      2    0x80000030    24C6FFFC    addiu           a2,a2,-4
         13      2      3    0x80000034    ACA80000    sw              t0,0(a1)
        --------------------------------------------------------------------------------
         14      3      1    0x80000038    24840004    addiu           a0,a0,4
         15      3      2    0x8000003C    14C0FFFB    bne             a2,$zero,0x8000002C
         16      3      3    0x80000040    24A50004    addiu           a1,a1,4
        --------------------------------------------------------------------------------
         17      4      1    0x80000044    1485FFFC    bne             a0,a1,0x80000038
         18      4      2    0x80000048    00000000    nop
        --------------------------------------------------------------------------------
         19      5      1    0x8000004C    509AFFFA    beql            a0,k0,0x80000038
        --------------------------------------------------------------------------------
         20      6      1    0x80000050    001AD080    sll             k0,k0,2
        --------------------------------------------------------------------------------
         21      7      1    0x80000054    535BFFED    beql            k0,k1,0x8000000C
        --------------------------------------------------------------------------------
         22     13      1    0x80000058    00010900    sll             $at,$at,4
        --------------------------------------------------------------------------------
         23      8      1    0x8000005C    2CE80003    sltiu           t0,a3,3
         24      8      2    0x80000060    1100000A    beq             t0,$zero,0x8000008C
         25      8      3    0x80000064    00073880    sll             a3,a3,2
        --------------------------------------------------------------------------------
         26     10      1    0x80000068    3C198000    lui             t9,0x8000
         27     10      2    0x8000006C    0327C821    addu            t9,t9,a3
         28     10      3    0x80000070    8F390094    lw              t9,148(t9)
         29     10      4    0x80000074    03200008    jr              t9
         30     10      5    0x80000078    00000000    nop
        --------------------------------------------------------------------------------
         31     12      1    0x8000007C    03E00008    jr              $ra
         32     12      2    0x80000080    24020004    addiu           v0,$zero,4
        --------------------------------------------------------------------------------
         33     11      1    0x80000084    03E00008    jr              $ra
         34     11      2    0x80000088    24020007    addiu           v0,$zero,7
        --------------------------------------------------------------------------------
         35      9      1    0x8000008C    03E00008    jr              $ra
         36      9      2    0x80000090    24020011    addiu           v0,$zero,17
                */

        do_trace(
            Pc::MEM_START,
            &assemble!(0x8000_0000, {
                nop
                beq     zero,zero,skip
                nop

            hidden:
                jr      ra
                addiu   v0,zero,19

                nop
                nop
                nop

            skip:
                nop
                nop
                nop

            start:
                lw      t0,0(a0)
                addiu   a2,a2,-4
                sw      t0,0(a1)
            contrived: // 4
                addiu   a0,a0,4
                bne     a2,zero,start
                addiu   a1,a1,4
            // expected block boundary
            // 7
                bne     a0,a1,contrived
                nop

                beql    a0,k0,contrived
                sll     k0,k0,2

                beql    k0,k1,hidden
                sll     at,at,4

                sltiu   t0,a3,3
                beq     t0,zero,ret
                sll     a3,a3,2
                lui     t9,%hi(jump_table)
                addu    t9,t9,a3
                lw      t9,%lo(jump_table)(t9)
                jr      t9
                nop

            L0:
                jr      ra
                addiu   v0,zero,4
            L1:
                jr      ra
                addiu   v0,zero,7

            ret:
            // 9
                jr      ra
                addiu   v0,zero,17
                // 11

            jump_table:
                .word_sym L0
                .word_sym L1
            }),
        );
    }

    #[test]
    fn test_tracer_2() {
        do_trace(
            Pc::MEM_START,
            &assemble!(0x8000_0000, {
                or      a0,zero,zero    // 0
                li      a1,0x4000       // 4

            again:
                sw      zero,0(a1)      // 8
                bnel    a0,a1,again     // C
                addiu   a0,a0,4         // 10

                bnel    a2,a3,other     // 14
                li      a0,4            // 18

                li      a0,7            // 1C

            other:
                jr      ra              // 20
                move    v0,a0           // 24
            }),
        );
    }
}
