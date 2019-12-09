use reality_ultra::crc;
use std::{env, fs};

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let mut rom = fs::read(&args[1]).expect("read");

    crc::update_rom_crc(&mut rom).expect("CRC calc");

    fs::write(&args[1], rom).expect("ROM write");
}
