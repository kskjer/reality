pub fn dfs<N, I, R>(start: N, successors: &mut impl FnMut(N) -> Result<I, R>) -> Result<(), R>
where
    I: Iterator<Item = N>,
{
    for n in successors(start)? {
        dfs(n, successors)?;
    }

    Ok(())
}
