use crate::files::FileInfo;
use reality_ultra::rom::Rom;
use reality_util::U8SliceUtils;

pub struct ZeldaRom {
    pub(crate) rom: Rom,
    pub(crate) file_table: Vec<FileEntry>,
}

#[derive(Copy, Clone, Debug)]
pub enum ZeldaRomLoadError {
    FileTableDiscovery(FileTableDiscoveryError),
    FileTableRead(usize, FileTableReadError),
}

impl ZeldaRom {
    pub fn load(rom: Rom) -> Result<ZeldaRom, ZeldaRomLoadError> {
        let table = find_file_table(rom.data())
            .map_err(|e| ZeldaRomLoadError::FileTableDiscovery(e))
            .and_then(|t| {
                read_file_table(rom.data(), t)
                    .collect::<Result<Vec<_>, _>>()
                    .map_err(|(i, e)| ZeldaRomLoadError::FileTableRead(i, e))
            })?;

        Ok(ZeldaRom {
            file_table: table,
            rom,
        })
    }

    pub fn files<'a>(&'a self) -> impl Iterator<Item = FileInfo<'a>> + '_ {
        let mut have_code = false;

        self.file_table
            .iter()
            .enumerate()
            .map(move |(i, e)| FileInfo::new(self.rom.data(), i, None, *e, &mut have_code))
    }
}

#[derive(Copy, Clone, Debug)]
pub struct AddressRange(u32, u32);

#[derive(Copy, Clone, Debug)]
pub struct FileEntry(AddressRange, AddressRange);

impl FileEntry {
    pub fn phys_start(&self) -> u32 {
        (self.1).0
    }

    pub fn phys_end(&self) -> u32 {
        match (self.1).1 {
            0 => self.phys_start() + ((self.0).1 - (self.0).0),
            x => x,
        }
    }

    pub fn virt_start(&self) -> u32 {
        (self.0).0
    }

    pub fn virt_end(&self) -> u32 {
        (self.0).1
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CompressionFormat {
    Yaz0,
    Deflate,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FileTableDiscoveryError {
    FileTableNotFound,
    EndOfFileTableNotFound(usize),
}

fn find_file_table(
    data: &[u8],
) -> Result<(CompressionFormat, usize, usize), FileTableDiscoveryError> {
    const N64_START: u64 = 0x1060;
    const IQUE_START: u64 = 0x1050;

    (0..data.len())
        .step_by(16)
        .take_while(|x| x + 32 <= data.len())
        .map(|i| {
            (
                i,
                data[(i + 0)..].read_u64(),
                data[(i + 8)..].read_u64(),
                data[(i + 16)..].read_u32(),
                data[(i + 24)..].read_u64(),
            )
        })
        .map(|(i, a, b, c, d)| {
            (
                i,
                a == N64_START && b == 0 && c == N64_START as u32 && d == (N64_START << 32),
                a == IQUE_START && b == 0 && c == IQUE_START as u32 && d == (IQUE_START << 32),
            )
        })
        .filter(|(_, is_n64, is_ique)| *is_n64 || *is_ique)
        .nth(0)
        .ok_or(FileTableDiscoveryError::FileTableNotFound)
        .and_then(|(i, is_n64, is_ique)| {
            Ok((
                if is_n64 {
                    CompressionFormat::Yaz0
                } else if is_ique {
                    CompressionFormat::Deflate
                } else {
                    unreachable!()
                },
                i,
                ((i + 16)..data.len())
                    .step_by(16)
                    .map(|i| (i, data[i..].read_u64(), data[(i + 8)..].read_u64()))
                    .filter(|(_, a, b)| *a == 0 && *b == 0)
                    .map(|(i, _, _)| i)
                    .nth(0)
                    .ok_or(FileTableDiscoveryError::EndOfFileTableNotFound(i))?,
            ))
        })
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FileTableReadError {
    PointerNotAligned,
    VirtEndNotGt,
    PhysEndNotGtOrZero,
    PhysStartNotLteVirtStart,
}

fn read_file_table(
    data: &[u8],
    ft: (CompressionFormat, usize, usize),
) -> impl Iterator<Item = Result<FileEntry, (usize, FileTableReadError)>> + '_ {
    let (_, start, end) = ft;

    (start..end).step_by(16).map(move |i| {
        let (vs, ve, ps, pe) = (
            data[i + 0..].read_u32(),
            data[i + 4..].read_u32(),
            data[i + 8..].read_u32(),
            data[i + 12..].read_u32(),
        );

        let id = (i - start) / 16;

        Err((
            id,
            match (vs, ve, ps, pe) {
                (vs, ve, ps, pe) if vs % 4 != 0 || ve % 4 != 0 || ps % 4 != 0 || pe % 4 != 0 => {
                    FileTableReadError::PointerNotAligned
                }
                (vs, ve, _, _) if ve <= vs => FileTableReadError::VirtEndNotGt,
                (_, _, ps, pe) if pe != 0 && pe <= ps => FileTableReadError::PhysEndNotGtOrZero,
                (vs, _, ps, _) if ps > vs => FileTableReadError::PhysStartNotLteVirtStart,
                (vs, ve, ps, pe) => {
                    return Ok(FileEntry(AddressRange(vs, ve), AddressRange(ps, pe)))
                }
            },
        ))?
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    static FT_DUMMY: &[u8] = &[
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x60, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x10, 0x60, 0x00, 0x01, 0xAB, 0x50, 0x00, 0x00, 0x10, 0x60, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x01, 0xAB, 0x50, 0x00, 0x02, 0x0D, 0x70, 0x00, 0x01, 0xAB, 0x50,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x0D, 0x70, 0x00, 0x04, 0x71, 0x60, 0x00, 0x02, 0x0D,
        0x70, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x71, 0x60, 0x00, 0x09, 0x85, 0xE0, 0x00, 0x04,
        0x71, 0x60, 0x00, 0x00, 0x00, 0x00, 0x00, 0x09, 0x85, 0xE0, 0x00, 0x5E, 0x0D, 0x50, 0x00,
        0x09, 0x85, 0xE0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x5E, 0x0D, 0x50, 0x00, 0x65, 0xD0, 0x50,
        0x00, 0x5E, 0x0D, 0x50, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    ];

    static FT_DUMMY_IQUE: &[u8] = &[
        0x74, 0x79, 0x75, 0x40, 0x6C, 0x69, 0x6E, 0x75, 0x78, 0x64, 0x65, 0x76, 0x33, 0x00, 0x00,
        0x00, 0x30, 0x36, 0x2D, 0x31, 0x30, 0x2D, 0x31, 0x33, 0x20, 0x31, 0x34, 0x3A, 0x31, 0x37,
        0x3A, 0x34, 0x33, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x50, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x50, 0x00, 0x00, 0xB2, 0x40, 0x00, 0x00, 0x10,
        0x50, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xB2, 0x40, 0x00, 0x01, 0x11, 0x90, 0x00, 0x00,
        0xB2, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x11, 0x90, 0x00, 0x03, 0xCF, 0x50, 0x00,
        0x01, 0x11, 0x90, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0xCF, 0x50, 0x00, 0x08, 0xC9, 0xD0,
        0x00, 0x03, 0xCF, 0x50, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0xC9, 0xD0, 0x00, 0x4D, 0xDD,
        0x60, 0x00, 0x08, 0xC9, 0xD0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x4D, 0xDD, 0x60, 0x00, 0x55,
        0xA0, 0x60, 0x00, 0x4D, 0xDD, 0x60, 0x00, 0x00, 0x00, 0x00, 0x00, 0x55, 0xB0, 0x00, 0x00,
        0x7C, 0x0C, 0x30, 0x00, 0x55, 0xA0, 0x60, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    ];

    #[test]
    fn test_n64_file_table() {
        let loc = find_file_table(&FT_DUMMY);

        assert_eq!(Ok((CompressionFormat::Yaz0, 16, 128)), loc);

        let out = read_file_table(&FT_DUMMY, loc.unwrap()).collect::<Result<Vec<_>, _>>();

        assert!(out.is_ok());
    }

    #[test]
    fn test_ique_file_table() {
        let loc = find_file_table(&FT_DUMMY_IQUE);

        assert_eq!(Ok((CompressionFormat::Deflate, 48, 176)), loc);

        let out = read_file_table(&FT_DUMMY_IQUE, loc.unwrap()).collect::<Result<Vec<_>, _>>();

        assert!(out.is_ok());
    }
}
