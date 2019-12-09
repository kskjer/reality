use reality_ultra::rom::Rom;
use reality_zelda::ZeldaRom;
use std::{env, fs};

fn main() {
    let args = env::args().collect::<Vec<_>>();

    let zrom = ZeldaRom::load(
        Rom::new(fs::read(&args[1]).expect("File read")).expect("Couldn't read N64 ROM"),
    )
    .expect("Couldn't read Zelda ROM");

    for (i, f) in zrom.files().enumerate() {
        let f = f.entry();

        println!(
            "{:>4}  {:08X} {:08X}  {:08X} {:08X}  {:>7.2} KiB  {:>7.2} KiB",
            i,
            f.virt_start(),
            f.virt_end(),
            f.phys_start(),
            f.phys_end(),
            (f.virt_end() - f.virt_start()) as f64 / 1024.0,
            (f.phys_end() - f.phys_start()) as f64 / 1024.0
        );
    }
}
