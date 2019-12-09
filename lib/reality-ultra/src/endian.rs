use reality_util::U8SliceUtils;

#[derive(Debug)]
pub struct UnrecognizedIdent(u32);
#[derive(Debug, PartialEq)]
pub struct DataNotWordAligned(usize);

impl UnrecognizedIdent {
    pub fn ident(self) -> u32 {
        self.0
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Endian {
    Big,
    Little16,
    Little32,
}

impl Endian {
    pub fn from_ident(ident: u32) -> Result<Endian, UnrecognizedIdent> {
        Ok(match ident {
            0x80371240 => Endian::Big,
            0x37804012 => Endian::Little16,
            0x40123780 => Endian::Little32,
            x => Err(UnrecognizedIdent(x))?,
        })
    }

    pub fn from_ident_bytes(ident: &[u8]) -> Result<Endian, UnrecognizedIdent> {
        Endian::from_ident(ident.read_u32())
    }

    pub fn to_big(self, data: &mut [u8]) -> Result<(), DataNotWordAligned> {
        if data.len() % 4 != 0 {
            Err(DataNotWordAligned(data.len()))?;
        }

        if self == Endian::Big {
            return Ok(());
        }

        for i in (0..data.len()).step_by(4) {
            let mut copy = data[i..].read_u32();

            copy = match self {
                Endian::Big => copy,
                Endian::Little32 => copy.swap_bytes(),
                Endian::Little16 => ((copy << 8) & 0xFF00FF00) | ((copy >> 8) & 0x00FF00FF),
            };

            data[i + 0] = (copy >> 24) as u8;
            data[i + 1] = (copy >> 16) as u8;
            data[i + 2] = (copy >> 8) as u8;
            data[i + 3] = (copy >> 0) as u8;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn swap_from_le16_to_be() {
        let mut input = [0xB, 0xA, 0xD, 0xC];

        let _ = Endian::Little16.to_big(&mut input);

        assert_eq!(input, [0xA, 0xB, 0xC, 0xD]);
    }

    #[test]
    fn swap_from_le_to_be() {
        let mut input = [0xB, 0xA, 0xD, 0xC];

        let _ = Endian::Little32.to_big(&mut input);

        assert_eq!(input, [0xC, 0xD, 0xA, 0xB]);
    }

    #[test]
    fn swap_from_be_to_be() {
        let mut input = [0xB, 0xA, 0xD, 0xC];

        let _ = Endian::Big.to_big(&mut input);

        assert_eq!(input, [0xB, 0xA, 0xD, 0xC]);
    }

    #[test]
    fn swap_unaligned() {
        assert_eq!(
            Endian::Little16.to_big(&mut [1, 2, 3]),
            Err(DataNotWordAligned(3))
        );
    }
}
