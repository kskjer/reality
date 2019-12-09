use byteorder::{BigEndian, ReadBytesExt};
use std::collections::BTreeSet;
use std::convert::TryInto;
use std::io::Cursor;
use std::{env, fs, io, iter, mem};

fn main() -> io::Result<()> {
    let args = env::args().collect::<Vec<_>>();
    let file = fs::read(&args[1])?;

    dump_banks(&file);
    println!();
    dump_overlays(&file);
    println!();
    dump_compressed_files(&file);

    Ok(())
}

fn dump_overlays(file: &Vec<u8>) {
    let reslts = (0..file.len() - 16)
        .step_by(4)
        .map(|i| Cursor::new(&file[i..i + 4 * 4]))
        .map(|mut c| {
            (
                c.read_u32::<BigEndian>().unwrap(),
                c.read_u32::<BigEndian>().unwrap(),
                c.read_u32::<BigEndian>().unwrap(),
                c.read_u32::<BigEndian>().unwrap(),
            )
        })
        .filter(|&(rs, re, vs, ve)| {
            re.wrapping_sub(rs) == ve.wrapping_sub(vs)
                && vs >= 0x8040_0000
                && ve >= 0x8040_0000
                && ve > vs
                && re > rs
                && vs < 0x8080_0000
                && ve < 0x80A0_0000
                && re < file.len() as u32
                && rs < file.len() as u32
        })
        .collect::<BTreeSet<_>>();

    println!("  ID   PHYS. RANGE           VIRT. RANGE                  SIZE");

    let mut rsum = 0.0;

    for (i, (rs, re, vs, ve)) in reslts.iter().enumerate() {
        let rsize = (re - rs) as f64 / 1024.0;

        println!(
            "{:4}   {:08X} - {:08X}   {:08X} - {:08X}   {:7.2} KiB",
            i, rs, re, vs, ve, rsize,
        );

        rsum += rsize;
    }

    println!();
    println!("Sum overlays: {:.2} KiB", rsum);
}

fn dump_banks(file: &Vec<u8>) {
    let map = (0xA79D4..)
        .step_by(4)
        .take(16)
        .map(|i| Cursor::new(&file[i..]).read_u32::<BigEndian>().unwrap())
        .collect::<Vec<_>>();

    println!("BANK   BASE ADDRESS");

    for (i, b) in map.iter().enumerate() {
        println!(
            "  {:2}   {}",
            i,
            if *b != 0 {
                format!("{:08X}", b)
            } else {
                "-".to_owned()
            }
        );
    }

    /*println!();

    let files = (0x000A1C90..)
        .step_by(12)
        .take(0x62e)
        .map(|i| Cursor::new(&file[i..i + 12]))
        .map(|mut c| (
            c.read_u32::<BigEndian>().unwrap(),
            c.read_u32::<BigEndian>().unwrap(),
            c.read_u32::<BigEndian>().unwrap(),
        ))
        .map(|(_, b, c)| (b, c))
        .filter(|(s, e)| e > s)
        .collect::<BTreeSet<_>>();

    for (i, (s, e)) in files.iter().enumerate() {
        println!(
            "{:4}   {:08X} - {:08X}   {:08X} - {:08X}   {:6.2} KiB",
            i,
            s,
            e,
            (s & 0x00FF_FFFF) + map[3],
            (e & 0x00FF_FFFF) + map[3],
            (e - s) as f64 / 1024.0
        );
    }*/
}

fn dump_compressed_files(file: &Vec<u8>) {
    let files = (0..file.len())
        .step_by(4)
        .take_while(|i| i + mem::size_of::<CmprFile>() < file.len())
        .filter_map(|i| read_header(&file[i..]).map(|h| (i, h)));

    let mut csum = 0.0;
    let mut dsum = 0.0;

    println!("  ID   PHYS. RANGE           COMP. SIZE    DEC. SIZE");

    for (i, (addr, f)) in files.enumerate() {
        let csize = f.comp_size as f64 / 1024.0;
        let dsize = f.dec_size as f64 / 1024.0;

        println!(
            "{:4}   {:08X} - {:08X}   {:6.2} KiB   {:6.2} KiB",
            i,
            addr,
            addr + f.comp_size as usize,
            csize,
            dsize
        );

        let dec = decode(
            &file[addr + 0x20..addr + 0x20 + f.data_offset as usize],
            &file[addr + 0x20 + f.data_offset as usize..addr + f.comp_size as usize],
        );

        println!("dec'd {:6} bytes, expected {:6}", dec.len(), f.dec_size);

        fs::write(format!("ys_dec_{:08X}.bin", addr), dec).expect("write");

        csum += csize;
        dsum += dsize;
    }

    println!();
    println!(
        "Compressed size: {:.2} KiB   Decompressed size: {:.2} KiB",
        csum, dsum
    );
}

/*
434D5052 000001AC 000001F8 00000000
534D5352 30300000 000001F8 0000007C
*/

#[repr(C)]
struct CmprFile {
    magic_a: u32,
    comp_size: u32,
    dec_size: u32,
    _blank: u32,
    magic_b: u32,
    magic_c: u32,
    dec_size_b: u32,
    data_offset: u32,
}

fn read_header(data: &[u8]) -> Option<CmprFile> {
    unsafe {
        let tmp: [u8; 32] = data[0..32].try_into().expect("Not enough data.");
        let tmp: CmprFile = mem::transmute(tmp);

        let x = CmprFile {
            magic_a: tmp.magic_a.to_be(),
            comp_size: tmp.comp_size.to_be(),
            dec_size: tmp.dec_size.to_be(),
            _blank: tmp._blank.to_be(),
            magic_b: tmp.magic_b.to_be(),
            magic_c: tmp.magic_c.to_be(),
            dec_size_b: tmp.dec_size_b.to_be(),
            data_offset: tmp.data_offset.to_be(),
        };

        if (x.magic_a, x.magic_b, x.magic_c) == (0x434D5052, 0x534D5352, 0x30300000) {
            Some(x)
        } else {
            None
        }
    }
}

fn decode(flags: &[u8], data: &[u8]) -> Vec<u8> {
    #[derive(PartialEq)]
    enum State {
        NeedFlag(usize, usize),
        HaveFlag(usize, usize, i32, u8),
        Done,
    }

    let mut dst = Vec::new();

    macro_rules! flags_at {
        ($ptr:expr) => {
            ((flags[$ptr + 0] as u16) << 8 | (flags[$ptr + 1] as u16)) as i16
        };
    }

    let _ = iter::successors(Some(State::NeedFlag(0, 0)), |state| {
        Some(match state {
            State::Done => State::Done,
            &State::HaveFlag(fptr, dptr, _, _) if fptr == flags.len() || dptr == data.len() => {
                State::Done
            }
            &State::NeedFlag(fptr, dptr) | &State::HaveFlag(fptr, dptr, _, 0) => {
                State::HaveFlag(fptr + 2, dptr, (flags_at!(fptr) as i32) << 16, 0x10)
            }
            &State::HaveFlag(mut fptr, mut dptr, flags, remain) => {
                if flags < 0 {
                    dst.push(data[dptr]);
                    dptr += 1;
                } else {
                    let flag_word = flags_at!(fptr);
                    let window_ptr = dst.len() - (flag_word & 0xFFF) as usize - 1;
                    let copy_count = ((flag_word >> 12) & 0xF) as usize + 3;

                    for x in window_ptr..(window_ptr + copy_count) {
                        dst.push(dst[x]);
                    }

                    fptr += 2;
                }

                State::HaveFlag(fptr, dptr, flags << 1, remain - 1)
            }
        })
    })
    .take_while(|x| x != &State::Done)
    .last();

    dst
}
