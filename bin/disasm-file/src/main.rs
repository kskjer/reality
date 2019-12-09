use reality_mips::isa;
use reality_mips::isa::Pc;
use std::convert::TryFrom;
use std::env;
use std::fs;
use std::io::{self, BufWriter, Write};

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let file = fs::read(&args[1]).expect("File read");
    let start = args
        .get(2)
        .map(|a| u32::from_str_radix(a, 16).expect("bad PC (number)"))
        .map(|w| Pc::try_from(w).expect("bad PC"))
        .unwrap_or(Pc::MEM_START);

    let stdout = io::stdout();
    let mut stdout = BufWriter::new(stdout.lock());

    for i in (0..file.len()).step_by(4) {
        let word = unsafe {
            (*file.get_unchecked(i) as u32) << 24
                | (*file.get_unchecked(i + 1) as u32) << 16
                | (*file.get_unchecked(i + 2) as u32) << 8
                | (*file.get_unchecked(i + 3) as u32)
        };

        let pc = start.next_n(i / 4);

        writeln!(
            stdout,
            "{}    {:08X}    {}",
            pc,
            word,
            isa::disassemble(word).at(pc)
        )
        .unwrap();
    }
}
