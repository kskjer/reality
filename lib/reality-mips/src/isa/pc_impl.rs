use crate::isa::Pc;
use crate::memory::Addr;
use std::cmp::Ordering;
use std::convert::TryFrom;
use std::{iter, ops, u32};

#[derive(Copy, Clone, Debug)]
pub struct PcDelta(i32);

impl Pc {
    pub const MEM_START: Pc = Pc(0x8000_0000);
    pub const STEP: PcDelta = PcDelta(4);
    pub const SIZE: usize = 4;
    pub const MIN: Pc = Pc(0);
    pub const MAX: Pc = Pc(u32::MAX);

    /// Enumerate succeeding instruction addresses. An enumerator that yields
    /// the starting PC, and every successive PC + 4.
    pub fn iter(mut self) -> impl Iterator<Item = Pc> {
        iter::from_fn(move || {
            let cur = self;

            self = Pc(cur.0.wrapping_add(4));

            Some(cur)
        })
    }

    pub fn iter_backwards(mut self) -> impl Iterator<Item = Pc> {
        iter::from_fn(move || {
            let cur = self;

            self = Pc(cur.0.wrapping_sub(4));

            Some(cur)
        })
    }

    pub fn next(self) -> Pc {
        Pc(self.0 + 4)
    }

    pub fn next_n(self, n: usize) -> Pc {
        Pc(self.0 + n as u32 * 4)
    }

    pub fn prev(self) -> Pc {
        Pc(self.0 - 4)
    }

    pub fn until(self, until: Pc) -> impl Iterator<Item = Pc> {
        (0..(until - self).bytes())
            .step_by(4)
            .map(move |b| Pc(self.0 + u32::try_from(b).unwrap()))
    }
}

impl PcDelta {
    pub fn bytes(self) -> i32 {
        self.0
    }

    pub fn insns(self) -> i32 {
        self.0 / 4
    }
}

impl ops::Add<u32> for Pc {
    type Output = Addr;

    fn add(self, rhs: u32) -> Self::Output {
        (self.0 + rhs).into()
    }
}

impl ops::Sub for Pc {
    type Output = PcDelta;

    fn sub(self, rhs: Self) -> Self::Output {
        PcDelta(self.0 as i32 - rhs.0 as i32)
    }
}

impl ops::Add<PcDelta> for Pc {
    type Output = Pc;

    fn add(self, rhs: PcDelta) -> Self::Output {
        Pc((self.0 as i32 + rhs.0) as u32)
    }
}

impl ops::Sub<PcDelta> for Pc {
    type Output = Pc;

    fn sub(self, rhs: PcDelta) -> Self::Output {
        Pc((self.0 as i32 - rhs.0) as u32)
    }
}

impl PartialEq<u32> for Pc {
    fn eq(&self, other: &u32) -> bool {
        self.0 == *other
    }
}

impl PartialOrd<u32> for Pc {
    fn partial_cmp(&self, other: &u32) -> Option<Ordering> {
        self.0.partial_cmp(other)
    }
}

#[cfg(test)]
mod tests {
    use crate::isa::Pc;

    #[test]
    fn test_iter() {
        assert_eq!(
            &[0, 4, 8, 12].iter().map(|p| Pc(*p)).collect::<Vec<_>>()[..],
            &Pc(0).iter().take(4).collect::<Vec<_>>()[..]
        );
    }

    #[test]
    fn test_backwards_iter() {
        assert_eq!(
            &[0, 4, 8, 12]
                .iter()
                .map(|p| Pc(*p))
                .rev()
                .collect::<Vec<_>>()[..],
            &Pc(12).iter_backwards().take(4).collect::<Vec<_>>()[..]
        );
    }
}
