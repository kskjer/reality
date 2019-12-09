pub fn yaz0_decode(src: &[u8], dst: &mut Vec<u8>, length: usize) {
    trait PlusPlus {
        fn plusplus(&mut self) -> Self;
    }

    impl PlusPlus for usize {
        fn plusplus(&mut self) -> Self {
            let cur = *self;

            *self += 1;

            cur
        }
    }

    let mut src_pos = 16;

    loop {
        let mut header = src[src_pos.plusplus()];

        for _ in 0..8 {
            if (header & 0x80) != 0 {
                dst.push(src[src_pos.plusplus()]);
            } else {
                let b = src[src_pos.plusplus()];
                let offs = (((b & 0xF) as usize) << 8 | (src[src_pos.plusplus()] as usize)) + 1;

                let mut rle_len = (b >> 4) as usize + 2;

                if rle_len == 2 {
                    rle_len = src[src_pos.plusplus()] as usize + 0x12;
                }

                for _ in 0..rle_len {
                    dst.push(dst[dst.len() - offs]);
                }
            }

            if dst.len() >= length {
                return;
            }

            header <<= 1;
        }
    }
}
