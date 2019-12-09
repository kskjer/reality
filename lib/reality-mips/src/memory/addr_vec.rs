use crate::isa::Pc;
use crate::memory::Addr;
use std::cmp;
use std::convert::{TryFrom, TryInto};
use std::ops;

pub trait ResizeToFit<Idx>: ops::IndexMut<Idx> {
    fn resize_to_fit(&mut self, upper_incl: Idx, fill: Self::Output);
}

#[derive(Debug)]
pub struct AddrVec<A, T>(A, Vec<T>);

impl<A, T> AddrVec<A, T>
where
    A: Into<Addr>,
{
    pub fn new(base: A) -> Self {
        Self(base, Vec::new())
    }
}

impl<T> AddrVec<Pc, T>
where
    T: Clone,
{
    pub fn new_from(base: Pc, upper: Pc, val: T) -> Self {
        Self(base, vec![val; (upper - base).insns().try_into().unwrap()])
    }
}

macro_rules! access {
    ($delta:ident, $self:ident, $getter:tt) => {{
        let len = ($self.1).len();
        let base = $self.0;

        usize::try_from($delta)
            .ok()
            .and_then(move |i| ($self.1).$getter(i))
            .unwrap_or_else(|| {
                panic!(
                    "Tried to access index {} in AddrVec with length {}. Base is {:?}.",
                    $delta, len, base
                )
            })
    }};
}

impl<A, T> ops::Deref for AddrVec<A, T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl<A, T> ops::DerefMut for AddrVec<A, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.1
    }
}

impl<T> ops::Index<Addr> for AddrVec<Addr, T> {
    type Output = T;

    fn index(&self, index: Addr) -> &Self::Output {
        let delta = index - self.0;

        access!(delta, self, get)
    }
}

impl<T> ops::IndexMut<Addr> for AddrVec<Addr, T> {
    fn index_mut(&mut self, index: Addr) -> &mut Self::Output {
        let delta = index - self.0;

        access!(delta, self, get_mut)
    }
}

impl<T> ops::Index<Pc> for AddrVec<Pc, T> {
    type Output = T;

    fn index(&self, index: Pc) -> &Self::Output {
        let delta = (index - self.0).insns();

        access!(delta, self, get)
    }
}

impl<T> ops::IndexMut<Pc> for AddrVec<Pc, T> {
    fn index_mut(&mut self, index: Pc) -> &mut Self::Output {
        let delta = (index - self.0).insns();

        access!(delta, self, get_mut)
    }
}

impl<T> ResizeToFit<Pc> for AddrVec<Pc, T>
where
    T: Clone,
{
    fn resize_to_fit(&mut self, upper_incl: Pc, fill: Self::Output) {
        self.1.resize(
            cmp::max(
                usize::try_from((upper_incl - self.0).insns()).unwrap() + 1,
                self.len(),
            ),
            fill,
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::isa::Pc;

    #[test]
    fn test_pc() {
        let start = Pc::MEM_START;
        let mut test = AddrVec::new(start);

        test.push(0);
        test.push(1);
        test.push(2);
        test.push(3);

        for (i, pc) in start.iter().enumerate().take(4) {
            assert_eq!(start + i as u32 * 4, pc.into());
            assert_eq!(test.1[i], test[pc]);
            assert_eq!(test[pc], i);
        }
    }

    #[test]
    fn test_addr() {
        let start: Addr = Pc::MEM_START.into();
        let mut test = AddrVec::new(start);

        test.push(0);
        test.push(1);
        test.push(2);
        test.push(3);

        for (i, pc) in start.iter().enumerate().take(4) {
            assert_eq!(pc, (start + i as u32).into());
            assert_eq!(test.1[i], test[pc]);
            assert_eq!(test[pc], i);
        }
    }
}
