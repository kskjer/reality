use reality_mips::isa::regs::Gpr;
use reality_mips::isa::{Instruction, Pc};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

#[derive(Clone, Debug)]
pub struct DefaultBehavior;

#[derive(Clone, Debug)]
pub struct ExactBehavior {
    excluded: BTreeSet<Pc>,
}

pub trait OffsetBehavior
where
    Self: Sized,
{
    fn init_offset() -> Offset<Self>;
    fn apply(
        snap: &mut OffsetSnapshot<Self>,
        serial: u64,
        pc: Pc,
        insn: Instruction,
        reg_read: &impl Fn(Gpr) -> u32,
    );
}

fn decompose_insn(insn: Instruction, reg_read: &impl Fn(Gpr) -> u32) -> Option<(u32, u32)> {
    use Instruction::*;

    Some(match insn {
        SLL(_, rt, sa) => (reg_read(rt), 1 << u8::from(sa) as u32),
        MULT(a, b) | MULTU(a, b) => (reg_read(a), reg_read(b)),
        _ => return None,
    })
}

mod behaviors {
    use super::*;

    impl OffsetBehavior for DefaultBehavior {
        fn init_offset() -> Offset<Self> {
            Offset {
                serial: 0,
                snaps: vec![OffsetSnapshot {
                    records: BTreeMap::new(),
                    behavior: Default::default(),
                }],
            }
        }

        fn apply(
            snap: &mut OffsetSnapshot<Self>,
            serial: u64,
            pc: Pc,
            insn: Instruction,
            reg_read: &impl Fn(Gpr) -> u32,
        ) {
            snap.records.insert(
                (pc, insn),
                match decompose_insn(insn, reg_read) {
                    Some((lhs, rhs)) => OffsetRecord { lhs, rhs, serial },
                    None => return,
                },
            );
        }
    }

    impl OffsetBehavior for ExactBehavior {
        fn init_offset() -> Offset<Self> {
            Offset {
                serial: 0,
                snaps: Vec::new(),
            }
        }

        fn apply(
            snap: &mut OffsetSnapshot<Self>,
            serial: u64,
            pc: Pc,
            insn: Instruction,
            reg_read: &impl Fn(Gpr) -> u32,
        ) {
            match (
                snap.behavior.excluded.contains(&pc),
                snap.records.get(&(pc, insn)),
                decompose_insn(insn, reg_read),
            ) {
                (true, _, _) | (_, _, None) => {}
                (
                    _,
                    Some(OffsetRecord {
                        lhs: ol, rhs: or, ..
                    }),
                    Some((l, r)),
                ) if *ol != l && *or != r => {
                    // Both sides have deviated, blacklist
                    snap.behavior.excluded.insert(pc);
                    snap.records.remove(&(pc, insn));
                }
                (_, _, Some((lhs, rhs))) => {
                    snap.records
                        .insert((pc, insn), OffsetRecord { lhs, rhs, serial });
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct Offset<B = DefaultBehavior> {
    snaps: Vec<OffsetSnapshot<B>>,
    serial: u64,
}

#[derive(Clone, Debug, PartialEq)]
struct OffsetRecord {
    lhs: u32,
    rhs: u32,
    serial: u64,
}

#[derive(Clone, Debug)]
pub struct OffsetSnapshot<B = DefaultBehavior> {
    records: BTreeMap<(Pc, Instruction), OffsetRecord>,
    behavior: B,
}

#[derive(Debug, PartialEq)]
pub struct Operation(u32, u32);

#[derive(Debug, PartialEq)]
pub struct DeltaResult(Pc, Operation, Operation);

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{} × {}", self.0, self.1)
    }
}

impl fmt::Display for DeltaResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}: {} → {}", self.0, self.1, self.2)
    }
}

impl<B: OffsetBehavior> OffsetSnapshot<B> {
    fn apply(&mut self, serial: u64, pc: Pc, insn: Instruction, reg_read: &impl Fn(Gpr) -> u32) {
        B::apply(self, serial, pc, insn, reg_read)
    }
}

fn same_ish(a: &OffsetRecord, b: &OffsetRecord) -> bool {
    (b.lhs == a.lhs || b.rhs == a.rhs) && (b.lhs != a.lhs || b.rhs != a.rhs)
}

fn same(a: &OffsetRecord, b: &OffsetRecord) -> bool {
    b.lhs == a.lhs && b.rhs == a.rhs
}

impl<B: Default + Clone + OffsetBehavior> Offset<B> {
    pub fn insn_pre(&mut self, pc: Pc, insn: Instruction, reg_read: &impl Fn(Gpr) -> u32) {
        self.serial += 1;

        if let Some(snap) = self.snaps.last_mut() {
            snap.apply(self.serial, pc, insn, reg_read);
        }
    }

    pub fn push(&mut self) {
        self.snaps
            .push(self.snaps.last().cloned().unwrap_or_default());
    }

    pub fn pop(&mut self) {
        self.snaps.pop();
    }

    fn last_two(&self) -> Option<(&OffsetSnapshot<B>, &OffsetSnapshot<B>)> {
        let len = self.snaps.len();

        if len < 2 {
            None
        } else {
            Some((&self.snaps[len - 2], &self.snaps[len - 1]))
        }
    }

    pub fn delta(&self) -> Option<impl Iterator<Item = DeltaResult> + '_> {
        let (before, after) = self.last_two()?;

        Some({
            let mut tmp: Vec<_> = before
                .records
                .iter()
                .filter_map(move |(k, b)| after.records.get(k).map(|a| (k, b, a)))
                .filter(|(_, b, a)| same_ish(a, b))
                .collect();

            tmp.sort_unstable_by_key(|(_, _, a)| a.serial);

            tmp.into_iter().map(|((pc, _), b, a)| {
                DeltaResult(*pc, Operation(b.lhs, b.rhs), Operation(a.lhs, a.rhs))
            })
        })
    }

    pub fn delta_all(&self) -> Option<impl Iterator<Item = DeltaResult> + '_> {
        if self.snaps.len() < 2 {
            return None;
        }

        let resulting = self.snaps[1..].iter().enumerate().fold(
            self.snaps[0].records.clone(),
            |mut acc, (i, next)| {
                let is_last = i == self.snaps.len() - 2;
                let mut to_remove = Vec::new();

                for (k, v) in acc.iter_mut() {
                    match next.records.get(k) {
                        None => to_remove.push(*k),
                        Some(nv)
                            if {
                                if !is_last {
                                    !same(v, nv)
                                } else {
                                    !same_ish(v, nv)
                                }
                            } =>
                        {
                            to_remove.push(*k)
                        }
                        Some(nv) => {
                            *v = nv.clone();
                        }
                    }
                }

                for k in &to_remove {
                    acc.remove(k);
                }

                acc
            },
        );

        Offset {
            snaps: vec![
                self.snaps[0].clone(),
                OffsetSnapshot {
                    records: resulting,
                    behavior: Default::default(),
                },
            ],
            serial: 0,
        }
        .delta()
        .map(|d| d.collect::<Vec<_>>().into_iter())
    }
}

mod defaults {
    use super::*;

    impl Default for DefaultBehavior {
        fn default() -> Self {
            Self
        }
    }

    impl Default for ExactBehavior {
        fn default() -> Self {
            Self {
                excluded: BTreeSet::new(),
            }
        }
    }

    impl<B: OffsetBehavior> Default for Offset<B> {
        fn default() -> Self {
            <B as OffsetBehavior>::init_offset()
        }
    }

    impl<B: Default> Default for OffsetSnapshot<B> {
        fn default() -> Self {
            OffsetSnapshot {
                records: BTreeMap::new(),
                behavior: Default::default(),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::convert::TryInto;
    use Instruction::*;

    #[test]
    fn test_sll() {
        let mut s: OffsetSnapshot = Default::default();

        let insn = SLL(Gpr::A0, Gpr::A0, 2.try_into().unwrap());

        s.apply(1, Pc::MEM_START, insn, &|_| 16);

        assert_eq!(
            s.records[&(Pc::MEM_START, insn)],
            OffsetRecord {
                lhs: 16,
                rhs: 4,
                serial: 1
            }
        );
    }

    #[test]
    fn test_mult() {
        let mut s: OffsetSnapshot = Default::default();

        let insn = MULT(Gpr::A1, Gpr::A0);

        s.apply(1, Pc::MEM_START, insn, &|r| match r {
            Gpr::A0 => 12,
            Gpr::A1 => 4,
            _ => 0,
        });

        assert_eq!(
            s.records[&(Pc::MEM_START, insn)],
            OffsetRecord {
                lhs: 4,
                rhs: 12,
                serial: 1
            }
        );
    }

    #[test]
    fn test_delta() {
        let mut ctx: Offset = Default::default();
        let reg_reader = |r| match r {
            Gpr::A0 => 4,
            Gpr::A1 => 12,
            _ => 0,
        };
        let reg_reader_2 = |r| match r {
            Gpr::A0 => 5,
            Gpr::A1 => 12,
            _ => 0,
        };
        let reg_reader_3 = |r| match r {
            Gpr::A0 => 5,
            Gpr::A1 => 13,
            _ => 0,
        };
        let reg_reader_4 = |r| match r {
            Gpr::A0 => 5,
            Gpr::A1 => 14,
            _ => 0,
        };

        ctx.insn_pre(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1), &reg_reader);
        ctx.push();
        ctx.insn_pre(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1), &reg_reader_2);

        assert_eq!(
            Some(vec![DeltaResult(
                Pc::MEM_START,
                Operation(4, 12),
                Operation(5, 12)
            )]),
            ctx.delta().map(|x| x.collect())
        );

        ctx.insn_pre(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1), &reg_reader);
        ctx.push();
        ctx.insn_pre(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1), &reg_reader_3);

        assert_eq!(Some(Vec::new()), ctx.delta().map(|x| x.collect()));

        ctx.push();
        ctx.insn_pre(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1), &reg_reader_4);

        assert_eq!(
            Some(vec![DeltaResult(
                Pc::MEM_START,
                Operation(5, 13),
                Operation(5, 14)
            )]),
            ctx.delta().map(|x| x.collect())
        );

        let sll = SLL(Gpr::A0, Gpr::A0, 2.try_into().unwrap());

        ctx.insn_pre(Pc::MEM_START, sll, &reg_reader);
        ctx.push();
        ctx.insn_pre(Pc::MEM_START, sll, &reg_reader_2);

        assert_eq!(
            Some(vec![DeltaResult(
                Pc::MEM_START,
                Operation(4, 4),
                Operation(5, 4)
            )]),
            ctx.delta().map(|x| x.collect())
        );
    }

    fn make_reader(a: u32, b: u32) -> Box<dyn Fn(Gpr) -> u32> {
        Box::new(move |reg: Gpr| match reg {
            Gpr::A0 => a,
            Gpr::A1 => b,
            _ => 0,
        })
    }

    #[test]
    fn test_delta_all() {
        let mut ctx: Offset = Default::default();

        ctx.insn_pre(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1), &make_reader(4, 12));
        ctx.push();
        ctx.insn_pre(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1), &make_reader(4, 12));
        ctx.push();
        ctx.insn_pre(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1), &make_reader(4, 12));
        ctx.push();
        ctx.insn_pre(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1), &make_reader(4, 12));
        ctx.push();
        ctx.insn_pre(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1), &make_reader(4, 12));
        ctx.push();
        ctx.insn_pre(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1), &make_reader(5, 12));

        assert_eq!(
            Some(vec![DeltaResult(
                Pc::MEM_START,
                Operation(4, 12),
                Operation(5, 12)
            )]),
            ctx.delta_all().map(|x| x.collect())
        );
    }

    #[test]
    fn test_exact_delta() {
        let mut ctx: Offset<ExactBehavior> = Default::default();

        ctx.insn_pre(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1), &make_reader(4, 12));

        assert_eq!(
            None,
            ctx.snaps
                .last()
                .and_then(|x| x.records.get(&(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1))))
        );

        ctx.push();
        ctx.insn_pre(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1), &make_reader(4, 12));
        ctx.insn_pre(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1), &make_reader(4, 12));
        ctx.insn_pre(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1), &make_reader(4, 12));

        assert_eq!(
            OffsetRecord {
                serial: 4,
                lhs: 4,
                rhs: 12,
            },
            ctx.snaps.last().unwrap().records[&(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1))]
        );

        ctx.insn_pre(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1), &make_reader(4, 13));

        assert_eq!(
            OffsetRecord {
                serial: 5,
                lhs: 4,
                rhs: 13,
            },
            ctx.snaps.last().unwrap().records[&(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1))]
        );

        ctx.insn_pre(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1), &make_reader(5, 14));

        assert_eq!(
            None,
            ctx.snaps
                .last()
                .unwrap()
                .records
                .get(&(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1)))
        );

        ctx.insn_pre(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1), &make_reader(5, 14));

        assert_eq!(
            None,
            ctx.snaps
                .last()
                .unwrap()
                .records
                .get(&(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1)))
        );

        ctx.insn_pre(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1), &make_reader(5, 14));
        ctx.insn_pre(Pc::MEM_START, MULT(Gpr::A0, Gpr::A1), &make_reader(5, 15));
    }
}
