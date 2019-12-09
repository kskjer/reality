pub fn lowest_common_ancestor<V, I>(
    want: &[V],
    pred: impl Fn(V) -> I,
    to_index: impl Fn(V) -> usize,
) -> Option<V>
where
    I: Iterator<Item = V>,
    V: Copy,
{
    let mut todo = want.to_vec();
    let mut hits = Vec::new();

    while let Some((next, v_next)) = todo.pop().map(|n| (to_index(n), n)) {
        hits.resize(next + 1, 0);
        hits[next] += 1;

        if hits[next] == want.len() {
            return Some(v_next);
        }

        for v in pred(v_next) {
            todo.push(v);
        }
    }

    None
}
