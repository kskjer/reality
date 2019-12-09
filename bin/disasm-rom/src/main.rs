use reality_mips::block;
use reality_mips::isa::Pc;
use reality_ultra::rom::Rom;
use std::convert::TryFrom;
use std::{env, fs};

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let rom = Rom::new(fs::read(&args[1]).expect("read file")).expect("bad ROM");
    let map = rom.map();
    let start = args
        .get(2)
        .map(|a| u32::from_str_radix(a, 16).expect("bad PC (number)"))
        .map(|w| Pc::try_from(w).expect("bad PC"));

    for f in block::trace_all(start.unwrap_or(rom.entry()), &map) {
        println!("-----------------------------------------------------");
        println!(
            "Function @ {} - {} ({} bytes)",
            f.start(),
            f.end(),
            (f.end() - f.start()).bytes()
        );
        println!("-----------------------------------------------------");
        println!();
        println!("{}", f.display(&map));
        println!();
    }
}
