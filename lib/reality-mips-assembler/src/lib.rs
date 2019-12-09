include!(concat!(env!("OUT_DIR"), "/mma.rs"));

#[macro_export]
macro_rules! __impl_assembler_terminal {
    (
        $base_pc:expr;
        $pc:expr;
        [$( ($addr:tt, $label:tt), )*];
        [$( ($word:expr, $reloc:tt), )*];
        [$( $exports:ident ),*];
    ) => {{
        $(
            #[allow(non_upper_case_globals)]
            const $label: u32 = $base_pc + $addr;
        )*

        ([
            $(
                __impl_assembler_terminal!(RELOC; $base_pc, $word, $reloc),
            )*
        ], $($exports),*)
    }};

    (RELOC; $base_pc:expr, $word:expr, (lo, $label:ident)) => {
        $word | ($label & 0xFFFF)
    };

    (RELOC; $base_pc:expr, $word:expr, (hi, $label:ident)) => {{
        let lower = $label & 0xFFFF;
        let upper = $label >> 16;

        $word | (upper + (lower >> 15))
    }};

    (RELOC; $base_pc:expr, $word:expr, (16, $label:ident, $pc:expr)) => {{
        let delta = $label as i32 - ($base_pc + $pc + 4) as i32;
        let delta = (delta >> 2) as u16;

        $word | delta as u32
    }};

    (RELOC; $base_pc:expr, $word:expr, (26, $label:ident)) => {
        $word | (($label >> 2) & ((1 << 26) - 1))
    };

    (RELOC; $base_pc:expr, $word:expr, (32, $label:ident)) => {
        $label
    };

    (RELOC; $base_pc:expr, $word:expr, None) => {
        $word
    };
}

/// PC defaults to 0x8000_0000 when not specified
#[macro_export]
macro_rules! assemble_mips {
    ({$($insns:tt)*}) => {
        assemble_mips!(0x8000_0000, {$($insns)*});
    };

    ($base_pc:expr, {$($insns:tt)*}) => {
        __impl_assemble_mips!($base_pc; 0u32; []; []; []; $($insns)*).0
    };

    ($base_pc:expr, [$($exports:ident),*], {$($insns:tt)*}) => {
        __impl_assemble_mips!($base_pc; 0u32; []; []; [$($exports),*]; $($insns)*)
    };
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_negatives() {
        let words = assemble_mips!(0x8000_0000, {
            lw       gp,-4(s0)
            addiu    gp,gp,-8
        });

        assert_eq!([0x8E1C_FFFC, 0x279C_FFF8], words);
    }

    #[test]
    fn test_labels() {
        let words = assemble_mips!(0x8000_0000, {
            lw          gp,-4(s0)
        L0:
            addiu       gp,gp,-8
            bne         gp,zero,L0
            nop

            jal         L0
            nop

            .word_sym   L0
        });

        assert_eq!(
            [0x8E1CFFFC, 0x279CFFF8, 0x1780FFFE, 0x00000000, 0x0C000001, 0x00000000, 0x80000004],
            words
        );
    }

    #[test]
    fn test_pseudo_move() {
        assert_eq!(
            [0x00001025],
            assemble_mips!(0x8000_0000, {
                move v0,zero
            })
        );
    }

    #[test]
    fn test_pseudo_li() {
        assert_eq!(
            [0x24020011],
            assemble_mips!(0x8000_0000, {
                li v0,17
            })
        );
    }

    #[test]
    fn test_pseudo_li_neg() {
        assert_eq!(
            [0x2402FFFD],
            assemble_mips!(0x8000_0000, {
                li v0,-3
            })
        );
    }

    #[test]
    fn test_pseudo_b() {
        assert_eq!(
            0x10000001,
            assemble_mips!(0x8000_0000, {
                b test
                nop
            test:
                nop
            })[0]
        );
    }

    #[test]
    fn test_word_directive_with_sym() {
        assert_eq!(
            0x8000_0004,
            assemble_mips!(0x8000_0000, {
                .word test
            test:
                nop
            })[0]
        );
    }

    #[test]
    fn test_word_directive() {
        assert_eq!(
            0xFFFF_FFFF,
            assemble_mips!(0x8000_0000, {
                .word 0xFFFF_FFFFu32
                nop
            })[0]
        );
    }

    #[test]
    fn test_word_expr() {
        assert_eq!(
            [1234, 4321, 4],
            assemble_mips!({
            label_a:
                .word 1234
            label_b:
                .word 4321
            _size:
                .word (label_b - label_a)
            })
        );
    }

    #[test]
    fn test_exports() {
        assert_eq!(
            ([1234, 4321, 4], 0x8000_0004u32),
            assemble_mips!(0x8000_0000, [label_b], {
            label_a:
                .word 1234
            label_b:
                .word 4321
            _size:
                .word (label_b - label_a)
            })
        );
    }
}
