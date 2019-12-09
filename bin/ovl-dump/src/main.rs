use reality_mips::block;
use reality_mips::flow::trace_function;
use reality_mips::isa::Pc;
use reality_mips::memory::MemoryMapFrom;
use reality_ultra::rom::Rom;
use reality_zelda::files::{FileInfo, OverlayInfo};
use reality_zelda::overlay::Overlay;
use reality_zelda::ZeldaRom;
use std::convert::{TryFrom, TryInto};
use std::{env, fs};

enum Mode {
    Invalid,
    ListOverlays(ZeldaRom),
    DumpOverlay(ZeldaRom, u16),
    DumpOverlayFunction(ZeldaRom, u16, Pc),
}

fn main() {
    let mode = env::args()
        .enumerate()
        .fold(Mode::Invalid, |acc, (i, cur)| match (i, acc) {
            (1, Mode::Invalid) => Mode::ListOverlays(
                ZeldaRom::load(
                    Rom::new(fs::read(cur).expect("File read")).expect("Couldn't read N64 ROM"),
                )
                .expect("Couldn't read Zelda ROM"),
            ),
            (2, Mode::ListOverlays(rom)) => {
                Mode::DumpOverlay(rom, cur.parse().expect("Couldn't parse overlay id"))
            }
            (3, Mode::DumpOverlay(rom, id)) => Mode::DumpOverlayFunction(
                rom,
                id,
                u32::from_str_radix(&cur, 16)
                    .ok()
                    .and_then(|x| Pc::try_from(x).ok())
                    .expect("Couldn't parse function address"),
            ),
            (_, acc) => acc,
        });

    match mode {
        Mode::Invalid => {
            println!("No mode, don't know what to do.");
        }
        Mode::ListOverlays(rom) => {
            for (f, o) in rom.overlays() {
                print_ovl_file(&f, &o);
            }
        }
        Mode::DumpOverlay(rom, id) => {
            let (file, ovl) = rom
                .overlays()
                .filter(|(f, _)| f.id() == id)
                .nth(0)
                .expect("Overlay not found");

            let start: Pc = ovl
                .image_start()
                .try_into()
                .expect("Image start address borked");
            let parsed = Overlay::read(file.data()).expect("OVL parsing failed");
            let memory = file.data().map_to(start);

            print_ovl_file(&file, &ovl);
            println!();
            println!("{}", parsed.display(start));

            for f in block::trace_all(start, &memory) {
                println!("-----------------------------------------------------");
                println!(
                    "Function @ {} - {} ({} bytes)",
                    f.start(),
                    f.end(),
                    (f.end() - f.start()).bytes()
                );
                println!("-----------------------------------------------------");
                println!();
                println!("{}", f.display(&memory));
                println!();
            }
        }
        Mode::DumpOverlayFunction(rom, id, which) => {
            let (file, ovl) = rom
                .overlays()
                .filter(|(f, _)| f.id() == id)
                .nth(0)
                .expect("Overlay not found");

            let start: Pc = ovl
                .image_start()
                .try_into()
                .expect("Image start address borked");
            let parsed = Overlay::read(file.data()).expect("OVL parsing failed");
            let memory = file.data().map_to(start);

            print_ovl_file(&file, &ovl);
            println!();
            println!("{}", parsed.display(start));

            for f in block::trace_all(start, &memory) {
                if f.start() != which {
                    continue;
                }

                println!("-----------------------------------------------------");
                println!(
                    "Function @ {} - {} ({} bytes)",
                    f.start(),
                    f.end(),
                    (f.end() - f.start()).bytes()
                );
                println!("-----------------------------------------------------");
                println!();
                println!("{}", f.display(&memory));
                println!();

                trace_function(&memory, &f).expect("Trace error");

                break;
            }
        }
    }
}

fn print_ovl_file(f: &FileInfo, o: &OverlayInfo) {
    println!(
        "{:>4}  {:08X} {:08X}  {:08X} {:08X}  {:>6.2} KiB",
        f.id(),
        f.entry().virt_start(),
        f.entry().virt_end(),
        o.image_start(),
        o.image_end(),
        (o.image_end() - o.image_start()) as f64 / 1024.0
    );
}
