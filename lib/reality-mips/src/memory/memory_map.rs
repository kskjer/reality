use crate::memory::Addr;
use reality_util::U8SliceUtils;
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::convert::TryInto;

pub trait MemoryMap {
    fn lookup<A: Into<Addr>>(&self, addr: A) -> Option<&[u8]>;
    fn lookup_exact<A: Into<Addr>>(&self, addr: A, size: usize) -> Result<&[u8], LookupError>;

    fn read_u32<A: Into<Addr>>(&self, addr: A) -> Result<u32, LookupError> {
        self.lookup_exact(addr, 4).map(|s| s.read_u32())
    }
}

pub trait MemoryMapFrom {
    type Output;

    fn map_to<A: Into<Addr>>(self, pc: A) -> Self::Output;
}

#[derive(Debug)]
pub enum LookupError {
    NotFound,
    OutOfBounds(Addr, usize, MemoryRange),
}

#[derive(Debug)]
pub struct RangeAlreadyMapped(MemoryRange, MemoryRange);

#[derive(Debug)]
pub struct MemoryRange(Addr, Addr);

struct Entry<T>(Addr, T);

impl<'a, T> From<&'a Entry<T>> for MemoryRange
where
    T: Borrow<[u8]>,
{
    fn from(x: &'a Entry<T>) -> Self {
        MemoryRange(x.0, (u32::from(x.0) + x.1.borrow().len() as u32).into())
    }
}

impl<T> Entry<T>
where
    T: Borrow<[u8]>,
{
    #[allow(dead_code)] // used in tests
    pub fn overlaps(&self, range: MemoryRange) -> bool {
        let MemoryRange(start, end) = range;
        let MemoryRange(_, our_end) = self.into();

        (our_end <= end && our_end > start) || (self.0 >= start && self.0 < end)
    }

    pub fn contains(&self, addr: Addr) -> bool {
        let delta = addr - self.0;

        delta >= 0 && delta < self.1.borrow().len() as i32
    }
}

pub struct VecMemoryMap<T> {
    entries: Vec<Entry<T>>,
}

impl<T> MemoryMapFrom for T
where
    T: Borrow<[u8]>,
{
    type Output = VecMemoryMap<T>;

    fn map_to<A: Into<Addr>>(self, pc: A) -> Self::Output {
        VecMemoryMap {
            entries: vec![Entry(pc.into(), self)],
        }
    }
}

impl<T> VecMemoryMap<T>
where
    T: Borrow<[u8]>,
{
    pub fn also_map<A: Into<Addr>>(mut self, pc: A, data: T) -> Self {
        self.entries.push(Entry(pc.into(), data));
        self.entries.sort_by_key(|e| e.0);

        VecMemoryMap {
            entries: self.entries,
        }
    }
}

impl<T> MemoryMap for VecMemoryMap<T>
where
    T: Borrow<[u8]>,
{
    fn lookup<A: Into<Addr>>(&self, addr: A) -> Option<&[u8]> {
        let addr: Addr = addr.into();

        self.entries
            .binary_search_by(|e| {
                if e.contains(addr) {
                    Ordering::Equal
                } else {
                    e.0.cmp(&addr)
                }
            })
            .map(|i| {
                let entry = &self.entries[i];
                let index: usize = (addr - entry.0)
                    .try_into()
                    .expect("Unable to convert to slice index");

                &entry.1.borrow()[index as usize..]
            })
            .ok()
    }

    fn lookup_exact<A: Into<Addr>>(&self, addr: A, size: usize) -> Result<&[u8], LookupError> {
        let addr: Addr = addr.into();

        self.entries
            .binary_search_by(|e| {
                if e.contains(addr) {
                    Ordering::Equal
                } else {
                    e.0.cmp(&addr)
                }
            })
            .map_err(|_| LookupError::NotFound)
            .and_then(|i| {
                let entry = &self.entries[i];
                let index: usize = (addr - entry.0)
                    .try_into()
                    .expect("Unable to convert to slice index");

                let slice = &entry.1.borrow()[index as usize..];

                if slice.len() < size {
                    Err(LookupError::OutOfBounds(addr, size, entry.into()))
                } else {
                    Ok(slice)
                }
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_map() {
        let map = (&[1, 2, 3, 4, 5, 6, 7, 8][..])
            .map_to(0x8000_0000)
            .also_map(0x8000_1000, &[0, 0, 0, 0]);

        assert_eq!(map.lookup(0x8000_0004), Some(&[5, 6, 7, 8][..]));
    }

    #[test]
    fn test_entry_overlap() {
        let entry = Entry(0x8000_0000.into(), &[0u8, 0, 0, 0][..]);

        assert_eq!(
            false,
            entry.overlaps(MemoryRange((0x8000_0000 - 4).into(), 0x8000_0000.into()))
        );
        assert_eq!(
            true,
            entry.overlaps(MemoryRange(0x8000_0000.into(), 0x8000_0004.into()))
        );
        assert_eq!(
            true,
            entry.overlaps(MemoryRange(0x8000_0002.into(), 0x8000_0004.into()))
        );
        assert_eq!(
            true,
            entry.overlaps(MemoryRange(0x8000_0003.into(), 0x8000_0004.into()))
        );
        assert_eq!(
            false,
            entry.overlaps(MemoryRange(0x8000_0004.into(), 0x8000_0008.into()))
        );
    }
}
