use std::fmt::Write;

pub fn dot<V, I>(
    start: V,
    to_idx: &impl Fn(V) -> usize,
    attr_for: &impl Fn(V, V) -> String,
    successors: &impl Fn(V) -> I,
) -> String
where
    I: Iterator<Item = V>,
    V: Copy,
{
    pub fn dot_inner<V, I>(
        v: V,
        output: &mut String,
        visited: &mut Vec<bool>,
        to_idx: &impl Fn(V) -> usize,
        attr_for: &impl Fn(V, V) -> String,
        successors: &impl Fn(V) -> I,
    ) where
        I: Iterator<Item = V>,
        V: Copy,
    {
        let v_idx = to_idx(v);

        visited.resize(std::cmp::max(visited.len(), v_idx + 1), false);

        if visited[v_idx] {
            return;
        }

        visited[v_idx] = true;

        for s in successors(v) {
            writeln!(
                output,
                "    {} -> {}{};",
                v_idx,
                to_idx(s),
                match attr_for(v, s) {
                    x if x.len() > 0 => format!(" [label=\"{}\"]", x),
                    x => x,
                }
            )
            .unwrap();

            dot_inner(s, output, visited, to_idx, attr_for, successors);
        }
    }

    let mut output = String::new();

    writeln!(output, "digraph G {{").unwrap();

    dot_inner(
        start,
        &mut output,
        &mut Vec::new(),
        to_idx,
        attr_for,
        successors,
    );

    writeln!(output, "}}").unwrap();

    output
}
