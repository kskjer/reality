use reality_mips::isa::Pc;
use reality_util::U8SliceUtils;
use std::borrow::Borrow;
use std::convert::TryInto;
use std::fmt;

fn check_align<T>(ptr: u32, map: fn(u32) -> T) -> Result<u32, T> {
    if ptr % 4 != 0 {
        Err(map(ptr))
    } else {
        Ok(ptr)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Section {
    Text,
    Data,
    Rodata,
}

impl Section {
    fn to_index(self) -> usize {
        match self {
            Section::Text => 0,
            Section::Data => 1,
            Section::Rodata => 2,
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct OverlaySection {
    pstart: u32,
    psize: u32,
    vstart: u32,
    vsize: u32,
}

#[derive(Copy, Clone, Debug)]
pub enum Relocation {
    R32,
    R26,
    Hi16,
    Lo16,
}

#[derive(Copy, Clone, Debug)]
pub struct OverlayRelocation {
    section: Section,
    reloc: Relocation,
    relative_target: u32,
}

pub struct Overlay<T> {
    data: T,
    sections: [OverlaySection; 5],
}

pub struct OverlayDisplay<'a, T>(Pc, &'a Overlay<T>);

#[derive(Debug)]
pub enum OverlayError {
    InvalidSize(u32),
    HeaderPointerUnaligned(u32),
    BadHeaderPointer(u32),
    InvalidHeaderPointer(u32),
    UnexpectedHeaderEnd(u32),
    OverlaySizeMismatch(u32, u32),
}

#[derive(Debug)]
pub enum RelocationError {
    InvalidSection(u8),
    UnknownRelocationType(u8),
    RelocationTargetOutOfBounds(u32, u32),
}

impl<T> Overlay<T>
where
    T: Borrow<[u8]>,
{
    pub fn read(data_in: T) -> Result<Overlay<T>, OverlayError> {
        use OverlayError::*;

        let data = data_in.borrow();
        let len: u32 = data.len().try_into().unwrap();

        if len < 4 {
            Err(InvalidSize(len))?
        }

        let header_ptr = data[(len as usize - 4)..].read_u32();
        let header_ptr = len
            .checked_sub(check_align(header_ptr, HeaderPointerUnaligned)?)
            .ok_or(OverlayError::BadHeaderPointer(header_ptr))?;

        let sections = data
            .get(header_ptr as usize..)
            .ok_or(InvalidHeaderPointer(header_ptr))
            .and_then(|header| Ok(header.get(0..20).ok_or(UnexpectedHeaderEnd(header_ptr))?))
            .and_then(|header| {
                let words = (0..header.len())
                    .step_by(4)
                    .map(|i| header[i..].read_u32())
                    .collect::<Vec<_>>();

                let actual_size = words
                    .iter()
                    .copied()
                    .enumerate()
                    .map(|(i, n)| match i {
                        0 | 1 | 2 => n,
                        3 => 0,     // bss
                        4 => n * 4, // relocation count,
                        _ => unreachable!(),
                    })
                    .sum::<u32>()
                    + 20;

                let actual_size = (actual_size / 16 + 1) * 16;

                if actual_size != len {
                    Err(OverlaySizeMismatch(len, actual_size))?;
                }

                // Doing things in a bit of a strange order here since we consider 3 and 4 to be
                // the BSS and relocs section, respectively. However, in the memory layout BSS comes
                // AFTER the relocs, and hence when we are calculating the full section start addr
                // we swap the two.
                let mut tmp = [0, 1, 2, 4, 3]
                    .iter()
                    .map(|&i| (i, words[i]))
                    .fold((Vec::new(), 0, 0), |(mut out, pstart, vstart), (i, n)| {
                        let (psize, vsize, pskip) = match i {
                            2 => (n, n, 0),
                            0 | 1 => (n, n, 0),
                            3 => (0, n, 0),
                            4 => (20 + n * 4, 20 + n * 4, 0),
                            _ => unreachable!(),
                        };

                        out.push((
                            i,
                            OverlaySection {
                                pstart,
                                vstart,
                                psize,
                                vsize,
                            },
                        ));

                        (out, pstart + psize + pskip, vstart + vsize + pskip)
                    })
                    .0;

                tmp.sort_by_key(|(i, _)| *i);

                Ok(tmp.into_iter().map(|(_, x)| x).collect::<Vec<_>>()[..]
                    .try_into()
                    .expect("Section count; shouldn't happen"))
            });

        sections.map(|s| Overlay {
            data: data_in,
            sections: s,
        })
    }

    pub fn display(&self, at: Pc) -> OverlayDisplay<T> {
        OverlayDisplay(at, self)
    }

    pub fn section(&self, section: Section) -> &[u8] {
        let info = self.sections[section.to_index()];
        let data = self.data.borrow();

        &data[info.pstart as usize..(info.pstart + info.psize) as usize]
    }

    pub fn relocations(
        &self,
    ) -> impl Iterator<Item = Result<OverlayRelocation, (u32, RelocationError)>> + '_ {
        let data = self.data.borrow();
        let section = self.sections[4];

        (section.pstart..(section.pstart + section.psize))
            .step_by(4)
            .map(move |i| {
                let w = data[i as usize..].read_u32();

                let reloc = OverlayRelocation {
                    section: match w >> 30 {
                        1 => Section::Text,
                        2 => Section::Data,
                        3 => Section::Rodata,
                        x => Err((i, RelocationError::InvalidSection(x as u8)))?,
                    },
                    relative_target: w & 0x00FF_FFFF,
                    reloc: match w >> 24 & 0x3F {
                        2 => Relocation::R32,
                        4 => Relocation::R26,
                        5 => Relocation::Hi16,
                        6 => Relocation::Lo16,
                        x => Err((i, RelocationError::UnknownRelocationType(x as u8)))?,
                    },
                };

                match (
                    reloc.relative_target,
                    self.section(reloc.section).len() as u32,
                ) {
                    (x, y) if x >= y => {
                        Err((i, RelocationError::RelocationTargetOutOfBounds(x, y)))?
                    }
                    _ => {}
                }

                Ok(reloc)
            })
    }
}

struct OvlAddressPair(u32, u32);

impl fmt::Display for OvlAddressPair {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let OvlAddressPair(start, end) = self;
        let size = end - start;

        if size == 0 {
            write!(f, "{:<31}", "-")
        } else {
            write!(
                f,
                "{:08X} - {:08X}  {:>6.2} KiB",
                start,
                end,
                size as f64 / 1024.0
            )
        }
    }
}

impl<'a, T> fmt::Display for OverlayDisplay<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let secs = [
            (0, ".text"),
            (1, ".data"),
            (2, ".rodata"),
            (4, ".rel.*"),
            (3, ".bss"),
        ];

        let base: u32 = self.0.into();

        /*
        SECTION    VIRTUAL                            PHYSICAL
        .text      80813820 - 8082A070   90.08 KiB    00000000 - 00016850   90.08 KiB
        .data      8082A070 - 8082F420   20.92 KiB    00016850 - 0001BC00   20.92 KiB
        .rodata    8082F420 - 80830690    4.61 KiB    0001BC00 - 0001CE70    4.61 KiB
        .bss       80830690 - 80830700    0.11 KiB    0001CE70 - 0001CE70    0.00 KiB
        .rel.*     80830700 - 808321EC    6.73 KiB    0001CE84 - 0001E970    6.73 KiB
        */

        writeln!(f, "SECTION    VIRTUAL                            PHYSICAL")?;

        for &(id, name) in secs.iter() {
            let sec = self.1.sections[id];

            writeln!(
                f,
                "{:<7}    {}    {}",
                name,
                OvlAddressPair(base + sec.vstart, base + sec.vstart + sec.vsize),
                OvlAddressPair(sec.pstart, sec.pstart + sec.psize)
            )?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const OVL_VASE: &[u8] = &[
        0x27, 0xBD, 0xFF, 0xE8, 0xAF, 0xA5, 0x00, 0x1C, 0xAF, 0xBF, 0x00, 0x14, 0x3C, 0x05, 0x3C,
        0x23, 0xAF, 0xA4, 0x00, 0x18, 0x0C, 0x00, 0xB5, 0x8B, 0x34, 0xA5, 0xD7, 0x0A, 0x8F, 0xA2,
        0x00, 0x18, 0x3C, 0x06, 0x80, 0x03, 0x24, 0xC6, 0xB5, 0xEC, 0x8C, 0x4F, 0x00, 0x24, 0x8C,
        0x4E, 0x00, 0x28, 0x24, 0x05, 0x00, 0x00, 0xAC, 0x4F, 0x00, 0x38, 0x8C, 0x4F, 0x00, 0x2C,
        0x3C, 0x07, 0x40, 0xC0, 0x24, 0x44, 0x00, 0xB4, 0xAC, 0x4E, 0x00, 0x3C, 0x0C, 0x00, 0xAC,
        0x78, 0xAC, 0x4F, 0x00, 0x40, 0x8F, 0xBF, 0x00, 0x14, 0x27, 0xBD, 0x00, 0x18, 0x03, 0xE0,
        0x00, 0x08, 0x00, 0x00, 0x00, 0x00, 0xAF, 0xA4, 0x00, 0x00, 0x03, 0xE0, 0x00, 0x08, 0xAF,
        0xA5, 0x00, 0x04, 0x27, 0xBD, 0xFF, 0xE8, 0xAF, 0xA4, 0x00, 0x18, 0x00, 0xA0, 0x20, 0x25,
        0xAF, 0xBF, 0x00, 0x14, 0x3C, 0x05, 0x06, 0x00, 0x0C, 0x00, 0xD4, 0x98, 0x24, 0xA5, 0x00,
        0x00, 0x8F, 0xBF, 0x00, 0x14, 0x27, 0xBD, 0x00, 0x18, 0x03, 0xE0, 0x00, 0x08, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x82, 0x06, 0x00, 0x00,
        0x00, 0x00, 0x10, 0x00, 0x86, 0x00, 0x00, 0x00, 0x00, 0x01, 0x4C, 0x80, 0xB2, 0x8E, 0xB0,
        0x80, 0xB2, 0x8F, 0x10, 0x80, 0x03, 0x51, 0x18, 0x80, 0xB2, 0x8F, 0x1C, 0x00, 0x00, 0x00,
        0xA0, 0x00, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x03, 0x82, 0x00, 0x00, 0x10, 0x82, 0x00, 0x00, 0x14, 0x82, 0x00, 0x00, 0x1C, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x30,
    ];

    #[test]
    fn test_load() {
        Overlay::read(OVL_VASE).expect("Couldn't parse overlay");
    }

    #[test]
    fn test_sections() {
        let ovl = Overlay::read(OVL_VASE).expect("Couldn't parse overlay");

        assert_eq!(ovl.sections[Section::Text.to_index()].vstart, 0);
        assert_eq!(ovl.sections[Section::Text.to_index()].vsize, 0xA0);
        assert_eq!(ovl.sections[Section::Data.to_index()].vstart, 0xA0);
        assert_eq!(ovl.sections[Section::Data.to_index()].vsize, 0x20);
        assert_eq!(ovl.sections[Section::Rodata.to_index()].vstart, 0xA0 + 0x20);
        assert_eq!(ovl.sections[Section::Rodata.to_index()].vsize, 0x20);
    }
}
