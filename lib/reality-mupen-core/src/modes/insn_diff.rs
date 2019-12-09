use reality_mips::isa::Pc;
use std::convert::TryInto;

const NUM_ENTRIES: usize = 8 * 1024 * 1024 / 4;

pub struct InsnDiff {
    serial: Serial,
    snaps: Snapshots,
}

struct SnapId(usize);
struct Snapshots(Vec<Snap>);
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
struct Serial(u64);

#[derive(Clone)]
struct Snap {
    hits: u32,
    touched: Vec<Serial>,
}

impl Serial {
    pub fn next(self) -> Self {
        Serial(self.0 + 1)
    }

    pub fn touched(self) -> bool {
        self.0 != 0
    }
}

impl Snapshots {
    pub fn new() -> Self {
        Snapshots(vec![
            Snap {
                hits: 0,
                touched: vec![Serial(0); NUM_ENTRIES]
            };
            1
        ])
    }

    pub fn pop(&mut self) -> Option<(SnapId, Snap)> {
        if self.0.len() <= 1 {
            return None;
        }

        Some((SnapId(self.0.len() - 1), self.0.pop().unwrap()))
    }

    pub fn push(&mut self) -> SnapId {
        let id = SnapId(self.0.len());

        self.0.push(self.0.last().unwrap().clone());

        id
    }

    pub fn hit(&mut self, pc: Pc, serial: Serial) {
        let idx: usize = (pc - Pc::MEM_START).insns().try_into().unwrap();
        let snap = self.0.last_mut().unwrap();
        let state = snap
            .touched
            .get_mut(idx)
            .unwrap_or_else(|| panic!("Couldn't get snap idx {} at {:?}", idx, pc));

        if !state.touched() {
            snap.hits += 1;
            *state = serial;
        }
    }

    pub fn last_two(&self) -> Option<(&Snap, &Snap)> {
        self.0
            .split_last()
            .and_then(|(l, other)| other.last().map(|r| (r, l)))
    }
}

impl InsnDiff {
    pub fn insn_pre(&mut self, pc: Pc) {
        self.snaps.hit(pc, self.serial);
        self.serial = self.serial.next();
    }

    pub fn push(&mut self) -> usize {
        self.snaps.push().0
    }

    pub fn pop(&mut self) -> Option<usize> {
        self.snaps.pop().map(|(id, _)| id.0)
    }

    pub fn delta(&self) -> Option<impl Iterator<Item = Pc>> {
        let (prev, cur) = self.snaps.last_two()?;

        let mut touched: Vec<_> = prev
            .touched
            .iter()
            .zip(cur.touched.iter())
            .enumerate()
            .filter(|(_, (p, c))| !p.touched() && c.touched())
            .map(|(i, (_, c))| (Pc::MEM_START.next_n(i), *c))
            .collect();

        touched.sort_unstable_by_key(|(_, serial)| *serial);

        Some(touched.into_iter().map(|(pc, _)| pc))
    }
}

impl Default for InsnDiff {
    fn default() -> Self {
        InsnDiff {
            snaps: Snapshots::new(),
            serial: Serial(1),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_delta() {
        let mut x: InsnDiff = Default::default();

        x.snaps.push();
        x.insn_pre(Pc::MEM_START);

        let which: Vec<_> = x.delta().unwrap().collect();

        assert_eq!(&which[..], &[Pc::MEM_START]);
    }
}
