use crate::overlay::Overlay;
use crate::yaz0::yaz0_decode;
use crate::zelda_rom::FileEntry;
use crate::ZeldaRom;
use reality_mips::isa;
use reality_util::U8SliceUtils;
use std::borrow::{Borrow, Cow};
use std::convert::TryInto;
use std::fmt;
use subslice::SubsliceExt;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FileType {
    Code,
    Overlay,
}

pub struct FileInfo<'a> {
    id: u16,
    data: Cow<'a, [u8]>,
    entry: FileEntry,
    // todo: filenames
    #[allow(dead_code)]
    name: Option<&'a str>,
    file_type: Option<FileType>,
}

fn detect_code(data: &[u8]) -> bool {
    let num_insns = (0..data.len())
        .step_by(4)
        .map(|i| data[i..].read_u32())
        .map(|w| isa::decode(w).is_ok())
        .take_while(|i| *i)
        .take(16)
        .count();

    num_insns == 16 && data.find(b"Yoshitaka Yasumoto").is_some()
}

impl<'a> FileInfo<'a> {
    pub fn new(
        rom_data: &'a [u8],
        id: usize,
        name: Option<&'a str>,
        entry: FileEntry,
        have_code: &mut bool,
    ) -> Self {
        let our_file = &rom_data[entry.phys_start() as usize..entry.phys_end() as usize];
        let content = if our_file[0..4] != b"Yaz0"[..] {
            Cow::Borrowed(our_file)
        } else {
            let mut tmp = Vec::new();

            yaz0_decode(our_file, &mut tmp, our_file[4..].read_u32() as usize);

            Cow::Owned(tmp)
        };

        let file_type = match content.borrow() {
            x if Overlay::read(x).is_ok() => Some(FileType::Overlay),
            x if !*have_code && detect_code(x) => {
                *have_code = true;

                Some(FileType::Code)
            }
            _ => None,
        };

        FileInfo {
            id: id.try_into().expect("File ID overflowed u16"),
            data: content,
            entry,
            name,
            file_type,
        }
    }

    pub fn file_type(&self) -> Option<FileType> {
        self.file_type
    }

    pub fn id(&self) -> u16 {
        self.id
    }

    pub fn data(&self) -> &[u8] {
        self.data.borrow()
    }

    pub fn entry(&self) -> &FileEntry {
        &self.entry
    }
}

pub struct OverlayInfo {
    file_vma_start: u32,
    file_vma_end: u32,
    image_vma_start: u32,
    image_vma_end: u32,
}

impl fmt::Debug for OverlayInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "{:08X}-{:08X} {:08X}-{:08X}",
            self.file_vma_start, self.file_vma_end, self.image_vma_start, self.image_vma_end
        )
    }
}

impl OverlayInfo {
    fn from(data: &[u8], swap: bool) -> Option<OverlayInfo> {
        let (file_part, image_part) = if !swap { (0, 8) } else { (8, 0) };

        let (file_start, file_end, vma_start, vma_end) = (
            data[(file_part + 0)..].read_u32(),
            data[(file_part + 4)..].read_u32(),
            data[(image_part + 0)..].read_u32(),
            data[(image_part + 4)..].read_u32(),
        );

        // Some sanity checks...
        let no_match = file_start % 16 != 0
            || file_end % 16 != 0
            || vma_start % 16 != 0
            || vma_end % 16 != 0
            || file_start >= file_end
            || vma_start >= vma_end
            || vma_start < 0x80800000
            || vma_end <= 0x80800000
            || vma_start >= 0x81000000
            || vma_end >= 0x81000000
            || file_start >= 0x0400_0000
            || file_end >= 0x0400_0000;

        if no_match {
            return None;
        }

        Some(OverlayInfo {
            file_vma_start: file_start,
            file_vma_end: file_end,
            image_vma_start: vma_start,
            image_vma_end: vma_end,
        })
    }

    pub fn file_start(&self) -> u32 {
        self.file_vma_start
    }
    pub fn file_end(&self) -> u32 {
        self.file_vma_end
    }
    pub fn image_start(&self) -> u32 {
        self.image_vma_start
    }
    pub fn image_end(&self) -> u32 {
        self.image_vma_end
    }
}

impl ZeldaRom {
    pub fn overlays<'a>(&'a self) -> impl Iterator<Item = (FileInfo<'a>, OverlayInfo)> + '_ {
        let mut have_code = false;
        let mut code_index = None;

        let mut full_files = self
            .file_table
            .iter()
            .enumerate()
            .map(|(i, e)| {
                let had_code = have_code;
                let result = FileInfo::new(self.rom.data(), i, None, *e, &mut have_code);

                if !had_code && have_code {
                    code_index = Some(i);
                }

                result
            })
            .collect::<Vec<_>>();

        let code_index = match code_index {
            Some(x) => x,
            None => panic!("Code file not found while iterating overlays"),
        };

        let mut overlays: Vec<OverlayInfo> = {
            let code: &[u8] = full_files[code_index].data.borrow();

            (0..code.len())
                .step_by(4)
                .take_while(|i| i + 16 <= code.len())
                .filter_map(move |i| {
                    OverlayInfo::from(&code[i..], false)
                        .or_else(|| OverlayInfo::from(&code[i..], true))
                })
                .collect::<Vec<_>>()
        };

        full_files.sort_by_key(|f| f.entry.virt_start());
        overlays.sort_by(|a, b| a.file_vma_start.cmp(&b.file_vma_start).reverse());

        full_files.into_iter().filter_map(move |f| {
            overlays
                .last()
                .map(|o| o.file_vma_start == f.entry.virt_start())
                .and_then(|found| if found { overlays.pop() } else { None })
                .map(|o| (f, o))
        })
    }
}
