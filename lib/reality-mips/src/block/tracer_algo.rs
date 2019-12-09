use crate::block::BlockId;

use reality_util::graph::dfs;
use std::cmp;

use std::fmt::Debug;
use std::mem;

use std::usize;

pub trait Successor {
    fn succ(self) -> Self;
}

impl Successor for BlockId {
    fn succ(self) -> Self {
        self.next()
    }
}

#[derive(Debug, Clone)]
pub(super) struct AlgoState<V> {
    visited: bool,
    in_edges: u8,
    out_edges: u8,
    incoming: Vec<V>,
}

impl<G> Default for AlgoState<G> {
    fn default() -> Self {
        Self {
            visited: false,
            in_edges: 0,
            out_edges: 0,
            incoming: Vec::new(),
        }
    }
}

pub(super) fn tracer_algo<V, E, I, R>(
    start: V,
    node_to_index: &impl Fn(V) -> usize,
    successors: &mut impl FnMut(V) -> Result<I, R>,
    edge_builder: &mut impl FnMut(V, V, E),
    group_builder: &mut impl FnMut(V, Vec<V>),
) -> Result<(), R>
where
    V: Debug + Copy + PartialEq,
    E: Debug + Clone,
    I: Iterator<Item = (V, E)>,
{
    let mut state = Vec::new();

    let ensure_fits = |state: &mut Vec<AlgoState<V>>, max| {
        state.resize(cmp::max(state.len(), max + 1), AlgoState::default())
    };

    dfs(start, &mut |s| -> Result<_, R> {
        let s_idx = node_to_index(s);
        ensure_fits(&mut state, s_idx);

        let mut next = Vec::new();

        if state[s_idx].visited {
            return Ok(next.into_iter());
        }

        state[s_idx].visited = true;

        for (n, _) in successors(s)? {
            let n_idx = node_to_index(n);

            ensure_fits(&mut state, n_idx);
            state[s_idx].out_edges += 1;
            state[n_idx].in_edges += 1;

            next.push(n);
        }

        Ok(next.into_iter())
    })?;

    // First node has one invisible in edge, since it's being executed, after all
    state[0].in_edges += 1;

    for s in state.iter_mut() {
        s.visited = false;
    }

    //println!("block tracer algo state = {:#?}", state);

    dfs((0, start, Vec::new(), start), &mut |(
        depth,
        group_id,
        mut group_nodes,
        s,
    )|
     -> Result<_, R> {
        let s_idx = node_to_index(s);
        let mut next = Vec::new();

        if state[s_idx].visited {
            return Ok(next.into_iter());
        }

        state[s_idx].visited = true;
        group_nodes.push(s);

        for (n, e) in successors(s)? {
            let n_idx = node_to_index(n);

            next.push((
                depth + 1,
                // todo: what about a loop back to the start of the fn? it doesn't work
                // not even this helps:  || (state[s_idx].in_edges == 1 && s_idx == 0)
                if state[s_idx].out_edges > 1 || state[n_idx].in_edges > 1 {
                    edge_builder(group_id, n, e);

                    // nb: check for non-empty group_nodes here, otherwise empty groups replaced real ones
                    // that's because we're creating groups for node s, but we're iterating its successors.
                    if group_nodes.len() > 0 {
                        let mut nodes = Vec::new();

                        mem::swap(&mut nodes, &mut group_nodes);
                        group_builder(group_id, nodes);
                    }

                    n
                } else {
                    group_id
                },
                // TODO: Avoid the clone of group nodes here. Would need to recurse immediately here.
                group_nodes.clone(),
                n,
            ));
        }

        // For dead ends
        if state[s_idx].out_edges == 0 || (depth == 0 && group_nodes.len() > 0) {
            group_builder(group_id, group_nodes);
        }

        Ok(next.into_iter())
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    #[test]
    fn test_no_forks() {
        let mut edges = BTreeMap::new();
        let mut groups = BTreeMap::new();

        let out: Result<(), ()> = tracer_algo(
            1,
            &|v| v,
            &mut |v| Ok(((v + 1)..9).take(1).map(|v| (v, ()))),
            &mut |s, d, e| {
                edges.insert((s, d), e);
            },
            &mut |g, n| {
                groups.insert(g, n);
            },
        );

        assert!(out.is_ok());
        assert_eq!(1, groups.len());
        assert!(groups.contains_key(&1));
        assert_eq!(groups[&1], &[1, 2, 3, 4, 5, 6, 7, 8][..]);
    }
}
