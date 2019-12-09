use std::cmp::{self, min};
use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;

#[allow(non_snake_case)]
struct SccState {
    index: i32,
    S: Vec<usize>,
    index_map: Vec<i32>,
    low_link_map: Vec<i32>,
    on_stack_map: Vec<bool>,
}

impl SccState {
    fn new(num_verts: usize) -> SccState {
        SccState {
            index: 0,
            S: Vec::new(),
            index_map: vec![-1; num_verts],
            low_link_map: vec![-1; num_verts],
            on_stack_map: vec![false; num_verts],
        }
    }
}

pub use scc as tarjan_strongly_connected_components;

/// Returns a topologically sorted list of components.
/// The components are returned in reverse order (3, 2, 1).
pub fn scc<T, I>(
    verts: impl Iterator<Item = T>,
    upper: T,
    get_successors: impl Fn(T) -> I,
) -> Vec<Vec<T>>
where
    usize: From<T>,
    T: TryFrom<usize> + Copy,
    T::Error: Debug,
    I: Iterator<Item = T>,
{
    let mut state = SccState::new(upper.into());
    let mut rval = Vec::new();

    for (orig_v, v) in verts.map(|v| (v, usize::from(v))) {
        if state.index_map[v] != -1 {
            continue;
        }

        scc_internal(orig_v, &mut state, &get_successors, &mut rval);
    }

    rval
}

fn scc_internal<T, I>(
    v: T,
    state: &mut SccState,
    get_successors: &impl Fn(T) -> I,
    component_list: &mut Vec<Vec<T>>,
) where
    usize: From<T>,
    T: TryFrom<usize> + Copy,
    T::Error: Debug,
    I: Iterator<Item = T>,
{
    let orig_v = v;
    let v: usize = v.into();

    state.index_map[v] = state.index;
    state.low_link_map[v] = state.index;

    state.index = state.index + 1;

    state.S.push(v);

    state.on_stack_map[v] = true;

    for (orig_w, w) in get_successors(orig_v).map(|w| (w, usize::from(w))) {
        if state.index_map[w] == -1 {
            scc_internal(orig_w, state, get_successors, component_list);
            state.low_link_map[v] = min(state.low_link_map[v], state.low_link_map[w]);
        } else if state.on_stack_map[w] {
            state.low_link_map[v] = min(state.low_link_map[v], state.index_map[w]);
        }
    }

    // If v is a root node, pop the stack and generate an SCC
    if state.low_link_map[v] == state.index_map[v] {
        let mut component = Vec::new();

        while let Some(w) = state.S.pop() {
            state.on_stack_map[w] = false;
            component.push(w.try_into().expect("Overflow")); // was unshift

            if w == v {
                break;
            }
        }

        component_list.push(component);
    }
}

#[allow(non_snake_case)]
struct State<V> {
    index: i32,
    S: Vec<V>,
    index_map: Vec<i32>,
    low_link_map: Vec<i32>,
    on_stack_map: Vec<bool>,
}

impl<V> State<V> {
    pub fn resize_to_fit(&mut self, index: usize) {
        let new_size = cmp::max(self.index_map.len(), index + 1);

        self.index_map.resize(new_size, -1);
        self.low_link_map.resize(new_size, -1);
        self.on_stack_map.resize(new_size, false);
    }
}

pub fn scc_enhanced<V, E, I>(
    vertices: impl Iterator<Item = V>,
    node_to_idx: &impl Fn(V) -> Result<usize, E>,
    successors: &impl Fn(V) -> I,
) -> Result<Vec<Vec<V>>, E>
where
    V: Copy,
    I: Iterator<Item = V>,
{
    let mut result = Vec::new();
    let mut state = State {
        index: 0,
        S: Vec::new(),
        index_map: Vec::new(),
        low_link_map: Vec::new(),
        on_stack_map: Vec::new(),
    };

    for v_node in vertices {
        let v = node_to_idx(v_node)?;

        state.resize_to_fit(v);

        if state.index_map[v] != -1 {
            continue;
        }

        scc_enhanced_internal(v_node, &mut state, successors, node_to_idx, &mut result)?;
    }

    Ok(result)
}

fn scc_enhanced_internal<V, E, I>(
    v_node: V,
    state: &mut State<V>,
    successors: &impl Fn(V) -> I,
    node_to_idx: &impl Fn(V) -> Result<usize, E>,
    result: &mut Vec<Vec<V>>,
) -> Result<(), E>
where
    V: Copy,
    I: Iterator<Item = V>,
{
    let v = node_to_idx(v_node)?;

    state.index_map[v] = state.index;
    state.low_link_map[v] = state.index;
    state.index += 1;
    state.on_stack_map[v] = true;
    state.S.push(v_node);

    state.resize_to_fit(v);

    for w_node in successors(v_node) {
        let w = node_to_idx(w_node)?;

        state.resize_to_fit(w);

        if state.index_map[w] == -1 {
            scc_enhanced_internal(w_node, state, successors, node_to_idx, result)?;
            state.low_link_map[v] = min(state.low_link_map[v], state.low_link_map[w]);
        } else if state.on_stack_map[w] {
            state.low_link_map[v] = min(state.low_link_map[v], state.index_map[w]);
        }
    }

    // If v is a root node, pop the stack and generate an SCC
    if state.low_link_map[v] == state.index_map[v] {
        let mut component = Vec::new();

        while let Some(w_node) = state.S.pop() {
            let w = node_to_idx(w_node)?;

            state.on_stack_map[w] = false;
            component.push(w_node);

            if w == v {
                break;
            }
        }

        result.push(component);
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use std::iter;

    #[test]
    fn test_scc() {
        let components = tarjan_strongly_connected_components(0..10, 10, |v| {
            let mut count = 0;

            let edges: &[&[usize]] = &[
                &[1usize, 2],
                &[3],
                &[3],
                &[0, 4],
                &[5],
                &[6, 7],
                &[8],
                &[8],
                &[5, 9],
                &[],
            ];

            iter::from_fn(move || {
                count += 1;

                edges.get(v).and_then(|e| e.get(count - 1)).cloned()
            })
        });

        let correct: &[&[usize]] = &[&[9], &[7, 8, 6, 5], &[4], &[2, 3, 1, 0]];

        assert_eq!(components, correct);
    }

    #[test]
    fn test_scc_enhanced() {
        let components = scc_enhanced(0..10, &|v| -> Result<_, ()> { Ok(v) }, &|v| {
            let mut count = 0;

            let edges: &[&[usize]] = &[
                &[1usize, 2],
                &[3],
                &[3],
                &[0, 4],
                &[5],
                &[6, 7],
                &[8],
                &[8],
                &[5, 9],
                &[],
            ];

            iter::from_fn(move || {
                count += 1;

                edges.get(v).and_then(|e| e.get(count - 1)).cloned()
            })
        });

        let correct: &[&[usize]] = &[&[9], &[7, 8, 6, 5], &[4], &[2, 3, 1, 0]];

        assert_eq!(
            components,
            Ok(correct
                .iter()
                .map(|c| c.iter().cloned().collect::<Vec<_>>())
                .collect())
        );
    }
}
