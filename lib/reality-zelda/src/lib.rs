pub mod files;
pub mod overlay;
mod yaz0;
mod zelda_rom;

pub use zelda_rom::{ZeldaRom, ZeldaRomLoadError};

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
