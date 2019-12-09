use std::{env, fs, io};

fn main() -> io::Result<()> {
    let args = env::args().collect::<Vec<_>>();
    let file = fs::read(&args[1])?;

    let result = match (file[0] as u32) << 24
        | (file[1] as u32) << 16
        | (file[2] as u32) << 8
        | (file[3] as u32)
    {
        0x40123780 => file,
        0x80371240 => {
            let mut new_file = vec![0u8; file.len()];

            for i in 0..new_file.len() {
                new_file[i] = file[i ^ 1];
            }

            new_file
        }
        x => panic!("Don't know endian {:08X}", x),
    };

    fs::write(&args[2], result)
}
