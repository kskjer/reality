mod crc_calc;
mod endian;
pub mod rom;

pub mod crc {
    pub use super::crc_calc::*;
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
