use crate::block::{BlockId, FunctionBlocks};
use crate::flow::condition::Condition;
use crate::flow::location::Location;
use crate::flow::value::Value;
use crate::flow::value_expr::ValueExpr;
use crate::flow::value_store::{ValueId, ValueStore, ValueStoreError, VecValueStore};
use crate::impl_counter;
use crate::isa::regs::Gpr;
use crate::isa::{Displace, Instruction};
use std::collections::{BTreeMap, BTreeSet};
use std::num::NonZeroU16;
use std::{fmt, u16};

impl_counter!(ComponentId);

fn create_value<V, E>(
    values: &mut VecValueStore<V>,
    value_blocks: &mut BTreeMap<V, BlockId>,
    block: BlockId,
    value: Value<V>,
) -> Result<V, E>
where
    VecValueStore<V>: ValueStore<V, Id = V, Error = E>,
    V: Ord + Copy,
{
    let id = values.push(value)?;

    value_blocks.insert(id, block);

    Ok(id)
}

mod snapshot {
    use super::*;
    use crate::block::BranchAction;
    use std::fmt::Debug;
    use std::ops;

    pub trait Snapshot<V> {
        type Err;

        fn get(&self, id: V) -> &Value<V>;
        fn read(&mut self, loc: Location<V>) -> Result<V, Self::Err>;
        fn create(&mut self, val: Value<V>) -> Result<V, Self::Err>;
        fn assign(&mut self, loc: Location<V>, id: V);
        fn clear(&mut self, loc: Location<V>);
        fn condition(&mut self, cond: Condition<V>);
        fn set_stack_size(&mut self, size: i16);
    }

    pub struct ScopeSnapshot<'a, V> {
        pub(super) function: FunctionContext<'a, V>,
        pub(super) block: BlockId,
        pub(super) scope: BTreeMap<Location<V>, V>,
        pub(super) outputs: BTreeMap<Location<V>, ComponentId>,
    }

    impl<'a, V: Ord> ScopeSnapshot<'a, V> {
        pub fn new(
            id: BlockId,
            function: FunctionContext<'a, V>,
            scope: BTreeMap<Location<V>, V>,
            outputs: BTreeMap<Location<V>, ComponentId>,
        ) -> Self {
            ScopeSnapshot {
                block: id,
                function,
                scope,
                outputs,
            }
        }
    }

    impl<'a, V, E> Snapshot<V> for ScopeSnapshot<'a, V>
    where
        V: Copy + Ord + PartialOrd + Eq + PartialEq + Debug,
        VecValueStore<V>: ValueStore<V, Id = V, Error = E> + ops::Index<V, Output = Value<V>>,
    {
        type Err = E;

        fn get(&self, id: V) -> &Value<V> {
            &self.function.values[id]
        }

        fn read(&mut self, loc: Location<V>) -> Result<V, Self::Err> {
            match (loc, self.scope.get(&loc), self.outputs.get(&loc)) {
                (Location::Displace(v, offs), _, None) => {
                    let key = (v, offs);

                    if !self.function.displacements.contains_key(&key) {
                        let val = self.create(Value(loc, ValueExpr::Input))?;

                        self.function.displacements.insert(key, val);
                    }

                    return Ok(self.function.displacements[&key]);
                }
                (_, None, Some(c)) => {
                    self.scope.insert(
                        loc,
                        self.function
                            .values
                            .push(Value(loc, ValueExpr::ComponentOutput(*c)))?,
                    );
                }
                (_, None, None) => {
                    let val = self.create(Value(
                        loc,
                        if loc.is_const() {
                            ValueExpr::Constant
                        } else {
                            ValueExpr::Input
                        },
                    ))?;

                    self.scope.insert(loc, val);
                }
                _ => {}
            }

            Ok(self.scope[&loc])
        }

        fn create(&mut self, val: Value<V>) -> Result<V, Self::Err> {
            create_value(
                &mut self.function.values,
                &mut self.function.value_blocks,
                self.block,
                val,
            )
        }

        fn assign(&mut self, loc: Location<V>, id: V) {
            match loc {
                Location::RegGpr(Gpr::Zero) => {}
                Location::Displace(v, offs) => {
                    self.function.displacements.insert((v, offs), id);
                }
                _ => {
                    self.outputs.remove(&loc);
                    self.scope.insert(loc, id);
                }
            }
        }

        fn clear(&mut self, loc: Location<V>) {
            if loc == Location::RegGpr(Gpr::Zero) {
                return;
            }

            self.outputs.remove(&loc);
            self.scope.remove(&loc);
        }

        fn condition(&mut self, cond: Condition<V>) {
            let cond_active = self.function.conditions.get(&self.block).and_then(|c| {
                c.iter()
                    .filter_map(|(blk, c)| {
                        Some(match *c {
                            c if c == cond => (BranchAction::Taken, blk),
                            c if c == !cond => (BranchAction::NotTaken, blk),
                            _ => return None,
                        })
                    })
                    .nth(0)
            });

            if let Some((action, blk)) = cond_active {
                let skipped = self
                    .function
                    .blocks
                    .out_edges(self.block)
                    .filter_map(|(dst, a)| if *a != action { Some(dst) } else { None })
                    .nth(0)
                    .unwrap();

                println!(
                    "\n-!- While applying condition {:?} in {:?}:\n\
                     \x20   Condition satisfying action `{:?}` already known to be true from {:?}.\n\
                     \x20   As such, {:?} is unreachable from {:?}.\n",
                    cond,
                    self.block,
                    action,
                    blk,
                    skipped,
                    self.block
                );

                self.function.unreachable.insert((self.block, skipped));
                return;
            }

            for (dst, action) in self.function.blocks.out_edges(self.block) {
                self.function
                    .conditions
                    .entry(dst)
                    .or_insert_with(|| Vec::new())
                    .push((
                        self.block,
                        match action {
                            BranchAction::Taken => cond,
                            BranchAction::NotTaken => !cond,
                            x => panic!("Not expecting other branch types here: {:?}", x),
                        },
                    ));
            }
        }

        fn set_stack_size(&mut self, size: i16) {
            self.function.set_stack_size(size);
        }
    }
}

use crate::memory::MemoryMap;
use reality_util::graph::SccNode;
use reality_util::{dump_map, IterGroupBy};
use snapshot::{ScopeSnapshot, Snapshot};
use std::iter;

fn unique_conditions<'a, B, V>(
    active: impl Iterator<Item = &'a Vec<(B, Condition<V>)>> + 'a,
) -> Vec<(B, Condition<V>)>
where
    B: Copy + Ord + 'a,
    V: Copy + Ord + 'a,
{
    let mut conditions = BTreeMap::new();
    let mut handled = BTreeSet::new(); // todo: maybe we're not doing this correctly

    for (blk, cond) in active.flat_map(|vec| vec.iter().copied()) {
        if handled.contains(&(blk, cond)) {
            continue;
        }

        handled.insert((blk, cond));

        if conditions.contains_key(&!cond) {
            conditions.remove(&!cond);
            conditions.remove(&cond);
            continue;
        }

        conditions.insert(cond, blk);
    }

    conditions
        .into_iter()
        .map(|(cond, blk)| (blk, cond))
        .collect()
}

/// Reach back one step in the history to replace a current true/false check with the actual check
/// that was performed (e.g., a < b)
fn propagate_condition<'a, V>(
    cond: Condition<V>,
    mapper: impl Fn(V) -> &'a Value<V>,
) -> Condition<V>
where
    V: 'a + Copy,
{
    use Condition::*;
    use Gpr::Zero;
    use Location::RegGpr;
    use ValueExpr::*;

    // todo: cop1 branch conds here.
    match (cond.map_lr(|&v| *mapper(v)), cond) {
        // (l < r) == false
        ((Value(RegGpr(Zero), Constant), Value(_, SetOnLessThan(l, r))), Equal(..))
        | ((Value(_, SetOnLessThan(l, r)), Value(RegGpr(Zero), Constant)), Equal(..)) => {
            !LessThan(l, r)
        }
        ((Value(RegGpr(Zero), Constant), Value(_, SetOnLessThan(l, r))), NotEqual(..))
        | ((Value(_, SetOnLessThan(l, r)), Value(RegGpr(Zero), Constant)), NotEqual(..)) => {
            LessThan(l, r)
        }
        _ => cond,
    }
}

fn trace_insn<S, V>(snap: &mut S, insn: Instruction) -> Result<(), S::Err>
where
    V: Copy,
    S: Snapshot<V>,
{
    use Instruction::*;

    macro_rules! assign {
        ($dst:expr, $src:expr) => {{
            let tmp = $src; // to avoid 2 mut borrows

            snap.assign($dst, tmp);
        }};
    }

    macro_rules! condition {
        ($cond:expr, $($arg:ident),*) => {{
            $(
                let $arg = snap.read(Location::RegGpr($arg))?;
            )*
            $cond($($arg),*)
        }};
    }

    macro_rules! op {
        ($dst:expr, $op:expr, $($arg:ident),*) => {{
            $(
                let $arg = snap.read(Location::RegGpr($arg))?;
            )*

            let val = snap.create(Value(
                $dst,
                $op($($arg),*)
            ))?;

            assign!($dst, val);
        }};
    }

    macro_rules! op_imm {
        ($dst:expr, $imm:expr, $op:expr, $($arg:ident),* ) => {{
            let imm = snap.create(Value(Location::ConstI16($imm), ValueExpr::Constant))?;
            $(
                let $arg = snap.read(Location::RegGpr($arg))?;
            )*

            let val = snap.create(Value(
                $dst,
                $op($($arg,)* imm)
            ))?;

            assign!($dst, val);
        }};
    }

    macro_rules! mem {
        (kind = load, $mem:expr, $reg:expr) => {
            assign!($reg, snap.read($mem)?);
        };
        (kind = store, $mem:expr, $reg:expr) => {
            assign!($mem, snap.read($reg)?);
        };
        ($rt:expr, $offset:expr, $base:expr, $kind:tt) => {
            let base = snap.read(Location::RegGpr($base))?;
            let mem_loc = match snap.get(base) {
                Value(Location::ConstU32(w), _) => {
                    Location::Global(((*w as i32 + i16::from($offset) as i32) as u32).into())
                }
                _ => Location::Displace(base, $offset.into()),
            };

            let reg_loc = Location::RegGpr($rt);

            mem!(kind = $kind, mem_loc, reg_loc);
        };
    }

    macro_rules! op_uimm {
        ($dst:expr, $imm:expr, $op:expr, $($arg:ident),* ) => {{
            let imm = snap.create(Value(Location::ConstU16($imm), ValueExpr::Constant))?;
            $(
                let $arg = snap.read(Location::RegGpr($arg))?;
            )*

            let val = snap.create(Value(
                $dst,
                $op($($arg,)* imm)
            ))?;

            assign!($dst, val);
        }};
    }

    match insn {
        x if x.is_nop() => {}

        // Move/copy operation
        OR(rd, rs, rt) | ADD(rd, rs, rt) | ADDU(rd, rs, rt)
            if rs == Gpr::Zero || rt == Gpr::Zero =>
        {
            assign!(
                Location::RegGpr(rd),
                snap.read(Location::RegGpr(if rs == Gpr::Zero { rt } else { rs }))?
            );
        }
        //
        // Branch
        //
        BEQL(rt, rs, _) | BEQ(rt, rs, _) if rt != rs => {
            let cond = propagate_condition(condition!(Condition::Equal, rt, rs), |v| snap.get(v));

            snap.condition(cond);
        }
        BNEL(rt, rs, _) | BNE(rt, rs, _) => {
            let cond =
                propagate_condition(condition!(Condition::NotEqual, rt, rs), |v| snap.get(v));

            snap.condition(cond);
        }

        //
        // Math
        //
        ADD(rd, rs, rt) | ADDU(rd, rs, rt) => op!(Location::RegGpr(rd), ValueExpr::Add, rs, rt),
        SUB(rd, rs, rt) | SUBU(rd, rs, rt) => op!(Location::RegGpr(rd), ValueExpr::Sub, rs, rt),
        SLL(rd, rt, sa) => {
            let sa = snap.read(Location::ConstU8(sa.into()))?;
            let rt = snap.read(Location::RegGpr(rt))?;
            let val = snap.create(Value(
                Location::RegGpr(rd),
                ValueExpr::ShiftLeftLogical(rt, sa),
            ))?;

            assign!(Location::RegGpr(rd), val);
        }
        SRA(rd, rt, sa) => {
            let sa = snap.read(Location::ConstU8(sa.into()))?;
            let rt = snap.read(Location::RegGpr(rt))?;
            let val = snap.create(Value(
                Location::RegGpr(rd),
                ValueExpr::ShiftRightArithmetic(rt, sa),
            ))?;

            assign!(Location::RegGpr(rd), val);
        }

        //
        // Math IMM
        //
        LUI(rt, imm) => {
            let imm = snap.read(Location::ConstU32((imm as u32) << 16))?;

            assign!(Location::RegGpr(rt), imm);
        }
        ADDIU(rt, rs, imm) => {
            if rt == rs && rs == Gpr::Sp && imm < 0 {
                snap.set_stack_size(-imm);
            }

            op_imm!(Location::RegGpr(rt), imm, ValueExpr::Add, rs)
        }
        ORI(rt, rs, imm) => op_uimm!(Location::RegGpr(rt), imm, ValueExpr::BitOr, rs),
        ANDI(rt, rs, imm) => op_uimm!(Location::RegGpr(rt), imm, ValueExpr::BitOr, rs),

        //
        // Compare
        //
        SLT(rd, rs, rt) | SLTU(rd, rs, rt) => {
            op!(Location::RegGpr(rd), ValueExpr::SetOnLessThan, rs, rt)
        }

        //
        // Compare IMM
        //
        SLTI(rt, rs, imm) => op_imm!(Location::RegGpr(rt), imm, ValueExpr::SetOnLessThan, rs),

        //
        // Load/Store
        //
        LB(rt, Displace(offset, base))
        | LBU(rt, Displace(offset, base))
        | LH(rt, Displace(offset, base))
        | LHU(rt, Displace(offset, base))
        | LW(rt, Displace(offset, base))
        | LD(rt, Displace(offset, base)) => {
            mem!(rt, offset, base, load);
        }
        SB(rt, Displace(offset, base))
        | SH(rt, Displace(offset, base))
        | SW(rt, Displace(offset, base))
        | SD(rt, Displace(offset, base)) => {
            mem!(rt, offset, base, store);
        }

        // todo: handle function calls
        JAL(_) => {}

        JR(_) | BEQ(_, _, _) => {}

        x => panic!("not handled: {:?}", x),
    }

    Ok(())
}

#[allow(unused_assignments)]
fn trace_block<'a, V>(
    mut snap: ScopeSnapshot<'a, V>,
    mem: &impl MemoryMap,
) -> Result<ScopeSnapshot<'a, V>, ValueStoreError>
where
    V: Copy + Ord,
    VecValueStore<V>: ValueStore<V, Id = V>,
    ScopeSnapshot<'a, V>: Snapshot<V, Err = ValueStoreError>,
{
    let mut last_jt = None;

    for (insn, pc) in snap.function.blocks.insns(snap.block, mem) {
        trace_insn(&mut snap, insn)?;

        if let Some(target) = last_jt {
            use Gpr::*;

            // todo: also handle function args here
            // todo: maybe we should handle function outputs in the same way we do components? as in
            //       keep it in the snapshot context.

            for r in &[
                At, V0, V1, A0, A1, A2, A3, T0, T1, T2, T3, T4, T5, T6, T7, T8,
            ] {
                //let mut to_remove = Vec::new();
                let reg_loc = Location::RegGpr(*r);

                /*for (loc, _) in &snap.scope {
                    match loc {
                        Location::Displace(v, _) if snap.get(*v).0 == reg_loc => {
                            to_remove.push(*loc)
                        }
                        _ => {}
                    }
                }

                for x in to_remove {
                    snap.clear(x);
                }*/

                // todo: clear all values here? $sp are mostly safe, except for when a ptr is sent
                // down. other pointers are volatile however.

                snap.clear(reg_loc);
            }

            let ret_val = snap.create(Value(
                Location::RegGpr(V0),
                ValueExpr::FunctionOutput(target),
            ))?;
            snap.assign(Location::RegGpr(V0), ret_val);

            last_jt = None;
        }

        last_jt = match insn {
            Instruction::JAL(jt) => Some(snap.read(Location::ConstU32(jt.full(pc).into()))?),
            Instruction::JALR(Gpr::Ra, rs) => Some(snap.read(Location::RegGpr(rs))?),
            _ => None,
        };
    }

    Ok(snap)
}

type ScopesCollection = BTreeMap<BlockId, BTreeMap<Location<ValueId>, ValueId>>;
type ComponentOutputs = BTreeMap<Location<ValueId>, ComponentId>;

struct ComponentTrace {
    id: ComponentId,
    scopes: ScopesCollection,
    outputs: ComponentOutputs,
}

fn trace_component<'a>(
    mem: &impl MemoryMap,
    mut function: FunctionContext<'a, ValueId>,
    nodes: &Vec<SccNode<BlockId>>,
    id: ComponentId,
) -> Result<(FunctionContext<'a, ValueId>, ComponentTrace), ValueStoreError> {
    let mut scopes: ScopesCollection = BTreeMap::new();
    let mut outputs: ComponentOutputs = BTreeMap::new();
    let mut our_blocks = vec![false; function.blocks.num_blocks() + 1];

    for n in nodes {
        match n {
            SccNode::Node(n, _) => {
                our_blocks[n.get() as usize] = true;
            }
            _ => {}
        }
    }

    for (i, b) in nodes.iter().enumerate() {
        match b {
            SccNode::Node(blk, _blk_type) => {
                // todo: what about edges coming from inside a component?

                // when getting the parents, only return those nodes which exist in our component
                let parent_blocks = if i == 0 {
                    // First node should have no real inputs, but in a component they might.
                    // so just ensure this is always empty.
                    Vec::new()
                } else {
                    function
                        .blocks
                        .in_edges(*blk)
                        .filter(|b| our_blocks[b.get() as usize])
                        .collect::<Vec<_>>()
                };

                if parent_blocks.len() > 0
                    && parent_blocks
                        .iter()
                        .all(|b| function.unreachable.contains(&(*b, *blk)))
                {
                    continue;
                }

                // Now we need to deduce which values may be live in this block.
                // We produce a set of live values that are active in ALL parent blocks.
                let parents_vals = parent_blocks
                    .iter()
                    .copied()
                    .map(|b| {
                        scopes
                            .get(&b)
                            .unwrap_or_else(|| {
                                panic!("Couldn't get parent {:?} while in {:?}", b, blk)
                            })
                            .keys()
                            .copied()
                            .collect::<BTreeSet<_>>()
                    })
                    .fold(Option::<BTreeSet<_>>::None, |acc, cur| {
                        Some(if let Some(set) = acc {
                            set.intersection(&cur).copied().collect()
                        } else {
                            cur
                        })
                    })
                    .unwrap_or_else(|| BTreeSet::new());

                // Now we need to fetch all the potential values, and group them by their location.
                // If there's more than one per location, we create a new value indicating that its
                // value depends on a condition.
                let mut grouped_vals = parents_vals
                    .into_iter()
                    .flat_map(|v| {
                        parent_blocks
                            .iter()
                            .cloned()
                            .map(move |b| (b, v))
                            .map(|(b, v)| {
                                let val = scopes.get(&b).and_then(|s| s.get(&v)).unwrap();

                                (v, b, *val)
                            })
                    })
                    .group_by(|(loc, _b, _val)| *loc, |x| x);

                // Remove duplicate value IDs in same location. should probably do this somewhere else
                for (_, g) in grouped_vals.iter_mut() {
                    g.sort_unstable_by_key(|x| x.2);
                    g.dedup_by_key(|x| x.2);
                }

                let empty_conds = Vec::new(); // for where we're mapping to Vecs and have to return a ref

                // Inherit conditions, fetch all the condition arrays from the parent, and reduce them
                // into one using unique_conditions,
                let conditions = function.conditions.remove(blk).unwrap_or_default();

                //println!("conditions now = {:?}", conditions);

                let uniq_conds = unique_conditions(
                    parent_blocks
                        .iter()
                        .cloned()
                        .map(|b| function.conditions.get(&b).unwrap_or(&empty_conds))
                        .chain(iter::once(&conditions)),
                );

                //println!("conditions after = {:?}", uniq_conds);

                function.conditions.insert(*blk, uniq_conds);

                // Build current scope, based on parents. Also compose Phi values here.
                let mut scope = grouped_vals
                    .iter()
                    .fold(BTreeMap::new(), |mut acc, (loc, cur)| {
                        acc.insert(
                            *loc,
                            if cur.len() > 1 {
                                let conditions = cur
                                    .iter()
                                    //.map(|(_loc, _blk, val)| function.value_blocks[&val])
                                    .map(|(_loc, blk, _val)| blk)
                                    .map(|b| function.conditions.get(&b).unwrap_or(&empty_conds))
                                    .collect::<Vec<_>>();

                                let uniq_conditions = unique_conditions(conditions.iter().copied());

                                //println!("cond b4 = {:?}, cond after = {:?}", conditions, uniq_conditions);

                                let conditions_per_val = conditions
                                    .iter()
                                    .map(|c| {
                                        c.iter()
                                            .filter(|c| uniq_conditions.contains(c))
                                            .cloned()
                                            .collect::<Vec<_>>()
                                    })
                                    .collect::<Vec<_>>();

                                // create the IfElse phi here!
                                // now how about for more Phis? we need to investigate how they present.

                                // Construct distinct conditions for each value
                                let set_per_val = conditions
                                    .iter()
                                    .map(|c| c.iter().copied().collect::<BTreeSet<_>>())
                                    .collect::<Vec<_>>();

                                let mut uniq_per_val = set_per_val
                                    .iter()
                                    .enumerate()
                                    .map(|(i, c)| {
                                        set_per_val
                                            .iter()
                                            .enumerate()
                                            .fold(c.clone(), |acc, (j, cur)| {
                                                if j == i {
                                                    return acc;
                                                }

                                                acc.difference(&cur).copied().collect()
                                            })
                                            .into_iter()
                                            .collect::<Vec<_>>()
                                    })
                                    .collect::<Vec<_>>();

                                let uniq_per_val = {
                                    let mut done = BTreeSet::new();

                                    for (i, c) in uniq_per_val.iter_mut().enumerate() {
                                        for (_, c) in c.iter() {
                                            done.insert(!*c);
                                        }

                                        if i == 0 {
                                            continue;
                                        }

                                        *c = c
                                            .iter()
                                            .filter(|(_, c)| !done.contains(&*c))
                                            .copied()
                                            .collect();
                                    }

                                    uniq_per_val
                                };

                                match (conditions_per_val, uniq_per_val) {
                                    (c, _) if c.len() == 2 && c[0].len() == 0 && c[1].len() > 0 => {
                                        create_value(
                                            &mut function.values,
                                            &mut function.value_blocks,
                                            *blk,
                                            Value(
                                                *loc,
                                                ValueExpr::IfElse(c[1][0].0, cur[1].2, cur[0].2),
                                            ),
                                        )
                                        .expect("Value ID")
                                    }
                                    //(_, u) if u.iter().all(|c| )
                                    /*
                                    8084E6D4 in BlockId(20), phi @ V0

                                    [
                                        blk 16 = [
                                            (BlockId(14), `ValueId(54) == ValueId(58)`),
                                            (BlockId(1), `ValueId(13) != ValueId(1)`),
                                            (BlockId(2), `ValueId(14) != ValueId(1)`),
                                            (BlockId(13), `ValueId(56) != ValueId(1)`),
                                            (BlockId(15), `ValueId(1) < ValueId(59)`)
                                        ],
                                        blk 19 = [
                                            (BlockId(14), `ValueId(54) == ValueId(58)`),
                                            (BlockId(1), `ValueId(13) != ValueId(1)`),
                                            (BlockId(2), `ValueId(14) != ValueId(1)`),
                                            (BlockId(13), `ValueId(56) != ValueId(1)`),
                                            (BlockId(16), `ValueId(63) >= ValueId(64)`)
                                        ]
                                    ]

                                    uniq conditions:
                                    [
                                        [(BlockId(15), `ValueId(1) < ValueId(59)`)],
                                        [(BlockId(16), `ValueId(63) >= ValueId(64)`)]
                                    ]
                                    */
                                    (_, u)
                                        if u.len() == 2
                                            && u[0].len() == 1
                                            && u[1].len() == 1
                                            && u[0] != u[1] =>
                                    {
                                        create_value(
                                            &mut function.values,
                                            &mut function.value_blocks,
                                            *blk,
                                            Value(
                                                *loc,
                                                ValueExpr::IfElse(u[0][0].0, cur[0].2, cur[1].2),
                                            ),
                                        )
                                        .expect("Value ID")
                                    }
                                    (_, u)
                                        if u.len() == 2
                                            && u[0].len() == 1
                                            && u[1].len() == 0
                                            && u[0] != u[1] =>
                                    {
                                        create_value(
                                            &mut function.values,
                                            &mut function.value_blocks,
                                            *blk,
                                            Value(
                                                *loc,
                                                ValueExpr::IfElse(u[0][0].0, cur[0].2, cur[1].2),
                                            ),
                                        )
                                        .expect("Value ID")
                                    }
                                    /*
                                    in BlockId(13), phi @ Displace(
                                    ValueId(
                                        6,
                                    ),
                                    2128,
                                    ): [
                                        (Displace(ValueId(6), 2128), BlockId(11), ValueId(51)),
                                        (Displace(ValueId(6), 2128), BlockId(12), ValueId(55))
                                    ]
                                    conditions = [
                                        [(BlockId(1), `ValueId(13) != ValueId(1)`)],
                                        [(BlockId(1), `ValueId(13) != ValueId(1)`), (BlockId(2), `ValueId(51) != ValueId(1)`)]
                                    ]
                                    */
                                    (c, u) => {
                                        panic!(
                                            "in {:?}, phi @ {:#?}: {:?}\n\
                                             conditions = {:?}\n\
                                             uniq conditions = {:?}\n\
                                             uniq per val = {:?}\n\
                                             displacements = {}\n\
                                             all conditions = {}\n\
                                             other conditions = {:?}\n\
                                             value blocks = [\n{}\n]\n\
                                             values = {}\n\
                                             scopes = {}\n",
                                            blk,
                                            loc,
                                            cur,
                                            c,
                                            uniq_conditions,
                                            u,
                                            dump_map(&function.displacements, 0, |d, _| format!(
                                                "{:?}",
                                                d
                                            )),
                                            dump_map(&function.conditions, 0, |d, _| format!(
                                                "{:?}",
                                                d
                                            )),
                                            conditions,
                                            function
                                                .value_blocks
                                                .iter()
                                                .map(|(v, b)| format!("    {:?}: {:?},", v, b))
                                                .collect::<Vec<_>>()
                                                .join("\n"),
                                            function.values.display(),
                                            dump_map(&scopes, 0, |s, depth| {
                                                dump_map(s, depth, |v, _| format!("{:?}", v))
                                            })
                                        );
                                    }
                                }
                            } else {
                                cur[0].2
                            },
                        );

                        acc
                    });

                function.seed_scope(&mut scope);

                //println!("scope @ {:?} = {:?}", blk, scope);

                let ScopeSnapshot {
                    function: f,
                    scope,
                    outputs: o,
                    ..
                } = trace_block(ScopeSnapshot::new(*blk, function, scope, outputs), mem)?;

                scopes.insert(*blk, scope);

                outputs = o;
                function = f;
            }
            // we need to determine:
            // - what values are input
            // - what input values are mutated
            // - output?
            SccNode::Scc(nodes) => {
                let last = function.values.last_id()?.unwrap_or(ValueId::MIN);
                let (f, component) = trace_component(mem, function, &nodes, id.next())?;

                // Values should now be updated. We can extract those that are "input" to determine
                // what the component expects in.

                let inputs = f
                    .values
                    .iter_from(last)
                    .filter(|v| {
                        if let Value(_, ValueExpr::Input) = v {
                            true
                        } else {
                            false
                        }
                    })
                    .map(|Value(loc, _)| loc)
                    .collect::<Vec<_>>();

                println!("inputs for {:?} = {:?}", id, inputs);
                println!(
                    "scopes for {:?} = {}",
                    id,
                    dump_map(&component.scopes, 0, |s, depth| {
                        dump_map(s, depth, |v, _| v.to_string())
                    })
                );

                // to get the loop vars, check the conditions set on the entry block of the component
                // as this is considered the loop back.

                // tag the potential output locations so that we can handled their read and connect
                // them to the component output

                outputs = outputs
                    .iter()
                    .chain(component.outputs.iter())
                    .chain(
                        component
                            .scopes
                            .iter()
                            .flat_map(|(_, s)| s.iter().map(|(loc, _v)| (loc, &component.id))),
                    )
                    // todo: ^ potentially way too many outputs here. maybe we should just get it from terminal nodes
                    .fold(BTreeMap::new(), |mut acc, (loc, c)| {
                        acc.insert(*loc, *c);

                        acc
                    });

                function = f;
            }
        }
    }

    if id == ComponentId::MIN {
        println!("Values = {:#?}", function.values);
        println!(
            "Conditions = {}",
            dump_map(&function.conditions, 0, |s, _| { format!("{:?}", s) })
        );
        println!(
            "Scopes = {}",
            dump_map(&scopes, 0, |s, depth| {
                dump_map(s, depth, |v, _| format!("{:?}", v))
            })
        );
    }

    Ok((
        function,
        ComponentTrace {
            scopes,
            outputs,
            id,
        },
    ))
}

pub struct FunctionContext<'a, V> {
    blocks: &'a FunctionBlocks,
    values: VecValueStore<V>,
    value_blocks: BTreeMap<V, BlockId>,
    conditions: BTreeMap<BlockId, Vec<(BlockId, Condition<V>)>>,
    unreachable: BTreeSet<(BlockId, BlockId)>,
    displacements: BTreeMap<(V, i16), V>,
    stack_size: Option<i16>,
    zero: V,
}

impl<'a, V> FunctionContext<'a, V>
where
    V: Copy + Ord,
{
    pub fn seed_scope(&self, scope: &mut BTreeMap<Location<V>, V>) {
        scope.insert(Location::RegGpr(Gpr::Zero), self.zero);
    }

    pub fn set_stack_size(&mut self, size: i16) {
        self.stack_size = match self.stack_size {
            Some(x) => panic!(
                "Trying to set stack size to {}, but it is already set to {}",
                size, x
            ),
            None => Some(size),
        };
    }
}

pub fn trace_function(
    mem: &impl MemoryMap,
    blocks: &FunctionBlocks,
) -> Result<(), ValueStoreError> {
    let mut store = VecValueStore::new();

    let zero = store.push(Value(Location::RegGpr(Gpr::Zero), ValueExpr::Constant))?;

    let _out = trace_component(
        mem,
        FunctionContext {
            blocks,
            values: store,
            value_blocks: BTreeMap::new(),
            conditions: BTreeMap::new(),
            unreachable: BTreeSet::new(),
            displacements: BTreeMap::new(),
            stack_size: None,
            zero,
        },
        &blocks.components,
        ComponentId::MIN,
    )?;

    Ok(())
}

// What now: the SCC components, write the trace_component function

#[cfg(test)]
mod tests {
    use crate::assembler::*;
    use crate::flow::condition::Condition;
    use crate::flow::location::Location;
    use crate::flow::tracer::propagate_condition;
    use crate::flow::value::Value;
    use crate::flow::value_expr::ValueExpr;
    use crate::isa::regs::Gpr;
    use crate::isa::Pc;
    use crate::memory::MemoryMapFrom;
    use reality_util::Slice32Tobytes;

    /* C source:
        typedef struct {
            int aaa;
            int bbb;
            int ccc;
            int variant;
        }
        Thingy;

        int our_func (Thingy *ptr)
        {
            if (ptr->ccc < 7)
                ptr->variant++;

            switch (ptr->variant)
            {
                case 0: return ptr->aaa + ptr->bbb;
                case 1: return ptr->aaa += 4;
                case 2: return ptr->bbb += 8;
            }
        }
    */

    static CODE: &[u32] = &assemble!(0x8000_0000, {
        our_func:
                lw      v0,8(a0)
                nop
                slti    v0,v0,7
                beq     v0,zero,L2
                nop

                lw      v0,12(a0)
                nop
                addiu   v0,v0,1
                sw      v0,12(a0)
        L2:
                lw      v0,12(a0)
                li      v1,1
                beq     v0,v1,L4
                li      v1,2

                beq     v0,v1,L5
                nop

                bne     v0,zero,L10
                nop

                lw      v0,0(a0)
                lw      v1,4(a0)
                jr      ra
                addu    v0,v0,v1

        L4:
                lw      v0,0(a0)
                nop
                addiu   v0,v0,4
                jr      ra
                sw      v0,0(a0)

        L5:
                lw      v0,4(a0)
                nop
                addiu   v0,v0,8
                sw      v0,4(a0)
        L10:
                jr      ra
                nop
    });

    #[test]
    fn test_condition_propagation() {
        let values = [
            Value(Location::RegGpr(Gpr::Zero), ValueExpr::Constant),
            Value(Location::RegGpr(Gpr::V0), ValueExpr::SetOnLessThan(2, 3)),
        ];

        let mapper = |v| &values[v];

        assert_eq!(
            propagate_condition(Condition::NotEqual(1, 0), mapper),
            Condition::LessThan(2, 3)
        );
        assert_eq!(
            propagate_condition(Condition::Equal(1, 0), mapper),
            Condition::GreaterThanOrEqualTo(2, 3)
        );
    }

    #[test]
    fn test_condition_merge() {
        use super::unique_conditions;
        use crate::flow::condition::Condition::*;

        assert_eq!(
            &unique_conditions(
                [
                    vec![(0, NotEqual(1, 2))],
                    vec![(1, Equal(1, 2))] //vec![(BlockId)]
                ]
                .iter()
            ),
            &[]
        );

        assert_eq!(
            &unique_conditions(
                [
                    vec![(0, LessThan(1, 2))],
                    vec![(1, GreaterThanOrEqualTo(1, 2))] //vec![(BlockId)]
                ]
                .iter()
            ),
            &[]
        );

        assert_eq!(
            &unique_conditions(
                [
                    vec![(0, NotEqual(1, 2)), (2, GreaterThanOrEqualTo(1, 2))],
                    vec![(1, Equal(1, 2))] //vec![(BlockId)]
                ]
                .iter()
            ),
            &[(2, GreaterThanOrEqualTo(1, 2))]
        );
    }

    #[test]
    fn test_trace_condition() {
        test_code(&assemble!(0x8000_0000, {
        our_func:
                lw      v1,0(a0)
                nop
                beq     v1,zero,L2
                li      v0,7

                li      a1,1
                beq     v1,a1,L2
                li      v0,13

                li      v0,17
        L2:
                jr      ra
                sw      v0,0(a0)
        }));
    }

    #[test]
    fn test_trace_1() {
        test_code(CODE);
    }

    #[test]
    fn test_trace_2() {
        test_code(&assemble!(0x8000_0000, {
            lw    t0,0(a0)
            beq   t0,zero,write
            move  t1,zero

            li    t1,7

        write:
            jr    ra
            sw    t1,4(a0)
        }));
    }

    #[test]
    fn test_trace_component() {
        test_code(&assemble!(0x8000_0000, {
            //nop // hack for the tracer
        start:
            beq     a0,a1,ret
            nop
            sw      zero,0(a0)
            b       start
            addiu   a0,a0,4
        ret:
            jr      ra
            nop
        }));
    }

    #[test]
    fn test_trace_unreachable_taken() {
        test_code(&assemble!(0x8000_0000, {
            beq     a0,a1,ret
            move    v0,zero

            // Branch not taken, so we know a0 != a1 == false, or that a0 == a1
            bne     a0,a1,ret
            nop

            // Meaning that the above branch will always be taken, and this will never be executed
            li      v0,1

        ret:
            jr      ra
            nop
        }));
    }

    #[test]
    fn test_trace_unreachable_not_taken() {
        test_code(&assemble!(0x8000_0000, {
            beq     a0,a1,ret
            move    v0,zero

            // Branch not taken, so we know a0 != a1 == false, or that a0 == a1
            beq     a0,a1,ret
            nop

            // Meaning that the above branch will always be taken, and this will never be executed
            li      v0,1

        ret:
            jr      ra
            nop
        }));
    }

    /*
    unsigned
    max (unsigned **z)
    {
        unsigned max = 0;

        for (; *z; z++)
            max = **z > max ? **z : max;

        return max;
    }
    */
    #[test]
    fn test_trace_function() {
        test_code(&assemble!(0x8000_0000, {
        max:
                move    v0,zero
        L2:
                lw      v1,0(a0)
                nop
                beq     v1,zero,L7
                nop

                lw      v1,0(v1)
                nop
                sltu    a1,v0,v1
                beq     a1,zero,L3
                nop

                move    v0,v1
        L3:
                b       L2
                addiu   a0,a0,4

        L7:
                jr      ra
                nop
        }));
    }

    #[test]
    fn test_global_vars() {
        test_code(&assemble!(0x8000_0000, {
            lui         t0,%hi(word_0)
            lw          t0,%lo(word_0)(t0)
            lui         t1,%hi(word_1)
            lw          t1,%lo(word_1)(t1)
            jr          ra
            addu        v0,t0,t1

        word_0:
            .word       0
        word_1:
            .word       0
        }));
    }

    fn test_code(x: &[u32]) {
        use super::trace_function;
        use crate::block;

        let map = x.to_be_bytes().map_to(Pc::MEM_START);

        let blocks = block::trace(Pc::MEM_START, &map).expect("Block trace");

        println!("{}", blocks.display(&map));

        let _whatever = trace_function(&map, &blocks).expect("Hmmm");
    }
}
