use reality_mips::block;
use reality_mips::isa::Pc;
use reality_mips::memory::{MemoryMap, MemoryMapFrom};
use std::collections::{BTreeMap, BTreeSet};
use std::convert::TryInto;
use std::{env, fs};

struct ConfigBuilder<'a> {
    ignore: BTreeSet<Pc>,
    super_dots: BTreeSet<Pc>,
    input: Option<&'a str>,
    entry: Pc,
    start: Option<Pc>,
}

struct Config<'a> {
    ignore: BTreeSet<Pc>,
    super_dots: BTreeSet<Pc>,
    input: &'a str,
    entry: Pc,
}

enum Flag<'a> {
    NoFlag,
    Flag(&'a str),
}

fn main() {
    let args = env::args().collect::<Vec<_>>();

    let (_, builder) = args[1..].iter().fold(
        (
            Flag::NoFlag,
            ConfigBuilder {
                ignore: BTreeSet::new(),
                super_dots: BTreeSet::new(),
                input: None,
                entry: Pc::MEM_START,
                start: None,
            },
        ),
        |(flag, mut cfg), cur| {
            match (flag, cur) {
                (Flag::Flag("e"), w) => {
                    cfg.entry = w.parse().expect("EP parse failed");

                    eprintln!("Entry point set to {}", cfg.entry);
                }
                (Flag::Flag("s"), w) => {
                    cfg.start = Some(w.parse().expect("Start PC parse failed"));

                    eprintln!("Start set to {}", cfg.start.unwrap());
                }
                (Flag::Flag("S"), w) => {
                    cfg.super_dots
                        .insert(w.parse().expect("PC parse for superdot failed"));

                    eprintln!("Making superdot of {}", w);
                }
                (Flag::Flag("i"), w) => {
                    let parts = w
                        .split('-')
                        .map(|w| -> Pc { w.parse().expect("PC parse") })
                        .collect::<Vec<_>>();

                    let start = parts.get(0).copied().expect("Range has no lower bound");
                    let upper = parts.get(1).copied().unwrap_or(start.next());

                    for p in start.until(upper) {
                        cfg.ignore.insert(p);
                    }

                    eprintln!("Ignoring {} locations", cfg.ignore.len());
                }
                (Flag::Flag(f), w) => {
                    panic!("Don't know what to do with flag '{}' with arg '{}'.", f, w);
                }
                (Flag::NoFlag, w) if w.starts_with('-') => {
                    return (Flag::Flag(&w[1..]), cfg);
                }
                (Flag::NoFlag, w) if cfg.input.is_none() => {
                    cfg.input = Some(w);

                    eprintln!("Input set to {}", w);
                }
                (Flag::NoFlag, w) => {
                    panic!("Don't know what to do with arg '{}'.", w);
                }
            }

            (Flag::NoFlag, cfg)
        },
    );

    let mut config = Config {
        input: builder.input.expect("No input file"),
        entry: builder.entry,
        ignore: builder.ignore,
        super_dots: builder.super_dots,
    };

    let mut data = fs::read(config.input).expect("Failed to read file");

    for &p in &config.ignore {
        let w_idx: usize = (p - config.entry).bytes().try_into().unwrap();

        for i in 0..4 {
            data[w_idx + i] = 0;
        }
    }

    let map = data.map_to(config.entry);
    let mut call_graph = BTreeSet::new();
    let mut names = BTreeMap::new();

    // Skip preceding nops
    while map.read_u32(config.entry).expect("Read") == 0 {
        config.entry = config.entry.next();
    }

    for b in block::trace_all(config.entry, &map) {
        for &c in &b.calls {
            call_graph.insert((b.start(), c));
        }

        names.insert(
            b.start(),
            format!("{}\\n{} bytes", b.start(), (b.end() - b.start()).bytes()),
        );

        println!("-----------------------------------------------------");
        println!(
            "Function @ {} - {} ({} bytes)",
            b.start(),
            b.end(),
            (b.end() - b.start()).bytes()
        );
        println!("-----------------------------------------------------");
        println!();
        println!("{}", b.display(&map));
        println!();

        if config.super_dots.contains(&b.start()) {
            let out = format!("super_{}.dot", b.start());

            eprintln!("Writing superdot for {} to '{}'.", b.start(), out);

            fs::write(out, block::create_super_dot(&b, &map)).expect("Couldn't write super dot");
        }
    }

    println!("===========================================");
    println!("Complete call graph");
    println!("===========================================");

    println!("digraph G {{");
    for (v, n) in &names {
        println!("    \"{}\"[label=\"{}\"];", v, n);
    }
    for (a, b) in &call_graph {
        println!("    \"{}\" -> \"{}\";", a, b);
    }
    println!("}}");
}
