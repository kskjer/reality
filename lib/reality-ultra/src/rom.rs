use crate::endian::{DataNotWordAligned, Endian, UnrecognizedIdent};
use reality_mips::isa::{Pc, PcNotWordAligned};
use reality_mips::memory::{MemoryMapFrom, VecMemoryMap};
use reality_util::U8SliceUtils;
use std::convert::TryInto;
use std::str::Utf8Error;
use std::{cmp, str};

pub struct RomCrc(u32, u32);

pub struct Rom {
    data: Vec<u8>,
    pub crc: RomCrc,
    pub name: String,
    pub endian: Endian,
    entry: Pc,
}

#[derive(Debug)]
pub enum RomError {
    UnknownEndian(UnrecognizedIdent),
    BadSize(DataNotWordAligned),
    BadName(Utf8Error),
    BadEntry(PcNotWordAligned),
}

impl Rom {
    pub fn new(mut data: Vec<u8>) -> Result<Rom, RomError> {
        let endian = Endian::from_ident(data.read_u32()).map_err(|e| RomError::UnknownEndian(e))?;

        endian.to_big(&mut data).map_err(|e| RomError::BadSize(e))?;

        Ok(Rom {
            endian,
            entry: data[8..]
                .read_u32()
                .try_into()
                .map_err(|e| RomError::BadEntry(e))?,
            crc: RomCrc(data[16..].read_u32(), data[20..].read_u32()),
            name: str::from_utf8(&data[32..52])
                .map(|n| n.trim_end().to_string())
                .map_err(|e| RomError::BadName(e))?,
            data,
        })
    }

    pub fn data(&self) -> &[u8] {
        &self.data[..]
    }

    pub fn entry(&self) -> Pc {
        self.entry
    }

    pub fn map(&self) -> VecMemoryMap<&[u8]> {
        (&self.data[0x1000..cmp::min(self.data.len(), 1024 * 1024 + 0x1000)]).map_to(self.entry)
    }
}
