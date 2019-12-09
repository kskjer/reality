mod debug;
pub mod graph;
mod iter;

pub use debug::*;
pub use iter::*;

pub trait U8SliceUtils {
    fn read_u16(&self) -> u16;
    fn read_u32(&self) -> u32;
    fn read_u64(&self) -> u64;
    fn try_read_u16(&self) -> Option<u16>;
    fn try_read_u32(&self) -> Option<u32>;
    fn try_read_u64(&self) -> Option<u64>;
    fn write_u32(&mut self, w: u32);
}

impl U8SliceUtils for [u8] {
    #[inline(always)]
    fn read_u16(&self) -> u16 {
        (self[0] as u16) << 8 | self[1] as u16
    }

    #[inline(always)]
    fn read_u32(&self) -> u32 {
        (self.read_u16() as u32) << 16 | (&self[2..]).read_u16() as u32
    }

    #[inline(always)]
    fn read_u64(&self) -> u64 {
        (self.read_u32() as u64) << 32 | (&self[4..]).read_u32() as u64
    }

    #[inline(always)]
    fn try_read_u16(&self) -> Option<u16> {
        if self.len() < 2 {
            None
        } else {
            Some(self.read_u16())
        }
    }

    #[inline(always)]
    fn try_read_u32(&self) -> Option<u32> {
        if self.len() < 4 {
            None
        } else {
            Some(self.read_u32())
        }
    }

    #[inline(always)]
    fn try_read_u64(&self) -> Option<u64> {
        if self.len() < 8 {
            None
        } else {
            Some(self.read_u64())
        }
    }

    #[inline(always)]
    fn write_u32(&mut self, w: u32) {
        for (i, b) in w.to_be_bytes().iter().enumerate() {
            self[i] = *b;
        }
    }
}

#[macro_export]
macro_rules! to_u8_array {
    ([$($word:expr),*]) => {
        [
            $(
                ($word as u32 >> 24) as u8,
                ($word as u32 >> 16) as u8,
                ($word as u32 >>  8) as u8,
                ($word as u32 >>  0) as u8,
            )*
        ]
    };
}
