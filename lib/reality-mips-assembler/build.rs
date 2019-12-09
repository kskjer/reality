use reality_mips_isa_specification::*;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::{env, io};

fn make_mma_reg_name(input: &str) -> String {
    input.replace("$", "").to_lowercase()
}

fn make_mma_insn_name(input: &str) -> String {
    input.replace("_", ".").to_lowercase()
}

macro_rules! cloned_const {
    ($data:expr) => {
        $data.iter().map(|v| v.to_string())
    };
}

macro_rules! to_lookup {
	([$($array:expr),+]) => {{
		use std::collections::HashSet;

		[$($array,)*].iter().collect::<HashSet<_>>()
	}};

	($single:expr) => {
		to_lookup!([$single]);
	};
}

fn main() -> Result<(), io::Error> {
    const MACRO_NAME: &str = "__impl_assemble_mips";
    const SHARED_ARGS: &str =
        "$base_pc:expr; $pc:expr; [$($labels:tt)*]; [$($words:tt)*]; [$($exports:ident),*];";

    let out_dir = env::var("OUT_DIR").expect("Couldn't get output dir");
    let dest_path = Path::new(&out_dir).join("mma.rs");

    let mut f = File::create(&dest_path).expect("Couldn't create output file");

    let (opcodes, reg_args, insns, regs) = &SPEC;

    let get_reg_arg_kind = |arg_name: &str| -> Option<&'static str> {
        reg_args
            .iter()
            .flat_map(|(name, args)| args.iter().map(move |(arg, _shift)| (name, arg)))
            .filter(|(_kind, arg)| &arg_name == *arg)
            .nth(0)
            .map(|(kind, _)| *kind)
    };

    let regs_kinds_and_values = regs
        .iter()
        .flat_map(|(k, _enum_names, asm_names)| {
            asm_names.iter().enumerate().map(move |(i, r)| (k, i, r))
        })
        .collect::<Vec<_>>();

    let reg_value_macro = ["#[macro_export] macro_rules! reg_value {".to_string()]
        .iter()
        .cloned()
        .chain(regs_kinds_and_values.iter().map(|(kind, val, asm_name)| {
            format!(
                "    ({}, {}) => {{ {} }};",
                kind,
                make_mma_reg_name(asm_name),
                val
            )
        }))
        .chain(["}".to_string()].iter().cloned())
        .collect::<Vec<_>>()
        .join("\n");

    let reg_arg_kinds = reg_args
        .iter()
        .flat_map(|(_name, kinds)| kinds.iter())
        .collect::<Vec<_>>();

    let construct_reg_value_macro = ["#[macro_export] macro_rules! reg_part {".to_string()]
        .iter()
        .cloned()
        .chain(reg_arg_kinds.iter().map(|(name, shift)| {
            format!("    ({}, $value:expr) => {{ $value << {} }};", name, shift)
        }))
        .chain(["}".to_string()].iter().cloned())
        .collect::<Vec<_>>()
        .join("\n");

    let make_op_macro = cloned_const!(["#[macro_export] macro_rules! make_op {"])
        .chain(opcodes.iter().map(|(name, shift, _size, parent)| {
            if let Some((parent, parent_val)) = parent {
                format!(
                    "    ({}, $value:expr) => {{ ($value << {}) | make_op!({}, {}) }};",
                    name, shift, parent, parent_val
                )
            } else {
                format!("    ({}, $value:expr) => {{ $value << {} }};", name, shift)
            }
        }))
        .chain(cloned_const!(["}"]))
        .collect::<Vec<_>>()
        .join("\n");

    let individual_insn_macro = [format!("#[macro_export] macro_rules! {} {{", MACRO_NAME)]
        .iter()
        .cloned()
        .chain(insns.iter().flat_map(|(kind, name, opcode, args)| {
            let param_sets = match args.get(1) {
                Some(&"Displace") => vec![
                    (format!("${}:tt,-$Offset:tt($Base:tt)", args[0]), None, true),
                    (format!("${}:tt,$Offset:tt($Base:tt)", args[0]), None, false),
                    (
                        format!("${}:tt,%lo($sym:tt)($Base:tt)", args[0]),
                        Some(("lo", to_lookup!("Offset"))),
                        false,
                    ),
                ],
                _ if *name == "LUI" => vec![
                    (format!("${}:tt,${}:tt", args[0], args[1]), None, false),
                    (
                        format!("${}:tt,%hi($sym:tt)", args[0]),
                        Some(("hi", to_lookup!("VerbatimImm"))),
                        false,
                    ),
                ],
                _ if *name == "ADDIU" => vec![
                    (
                        format!("${}:tt,${}:tt,${}:tt", args[0], args[1], args[2]),
                        None,
                        false,
                    ),
                    (
                        format!("${}:tt,${}:tt,-${}:tt", args[0], args[1], args[2]),
                        None,
                        true,
                    ),
                    (
                        format!("${}:tt,${}:tt,%lo($sym:tt)", args[0], args[1]),
                        Some(("lo", to_lookup!("Imm"))),
                        false,
                    ),
                ],
                _ if *name == "J" || *name == "JAL" => vec![(
                    format!("$sym:tt",),
                    Some(("26", to_lookup!("JumpTarget"))),
                    false,
                )],
                _ => vec![if args.iter().any(|a| *a == "BranchTarget") {
                    (
                        args.iter()
                            .map(|a| format!("${}:tt", a.replace("BranchTarget", "sym")))
                            .collect::<Vec<_>>()
                            .join(","),
                        Some(("16", to_lookup!("BranchTarget"))),
                        false,
                    )
                } else {
                    (
                        args.iter()
                            .map(|v| format!("${}:tt", v))
                            .collect::<Vec<_>>()
                            .join(", "),
                        None,
                        false,
                    )
                }],
            };

            param_sets
                .iter()
                .enumerate()
                .map(|(_i, (set, relo, negate))| {
                    let insn_param = format!("{:<12} {}", make_mma_insn_name(name), set);

                    let insn_word = format!(
                        "make_op!({}, {}) | 0 {}",
                        kind,
                        opcode,
                        // Little workaround since we combined the Offset and Base args
                        // into "Displace"
                        match args.get(1) {
                            Some(&"Displace") => [args[0], "Offset", "Base"].to_vec(),
                            _ => args.to_vec(),
                        }
                        .iter()
                        .filter_map(|v| get_reg_arg_kind(v)
                            .map(|kind| format!("| reg_part!({}, reg_value!({}, ${}))", v, kind, v))
                            .or_else(|| match relo {
                                Some((_, lookup)) if lookup.contains(v) => None,
                                _ if *v == "ShiftAmount" =>
                                    Some(format!("| ((${} as u32 & 0x1F) << 6)", v)),
                                _ => Some(if *negate {
                                    format!("| -(${} as i16) as u16 as u32", v)
                                } else {
                                    format!("| ${}", v)
                                }),
                            }))
                        .collect::<Vec<_>>()
                        .join(" ")
                    );

                    format!(
                        "    ({shared_args} {:<46} $($insns:tt)*) => \
                         {{ {macro_name}!( \
                         $base_pc; \
                         $pc + 4; \
                         [$($labels)*]; \
                         [$($words)* ({}, {relocs}),]; \
                         [$($exports),*]; \
                         $($insns)* \
                         ) }};",
                        insn_param,
                        insn_word,
                        shared_args = SHARED_ARGS,
                        macro_name = MACRO_NAME,
                        relocs = if let Some((kind, _)) = relo {
                            format!(
                                "({}, $sym{})",
                                kind,
                                if *kind == "16" { ", $pc" } else { "" }
                            )
                        } else {
                            "None".to_string()
                        }
                    )
                })
                .rev()
                .collect::<Vec<_>>()
        }))
        .chain(PSEUDO_SPEC.iter().map(|&(p_op, p_args, op, args)| {
            let p_arg_vars = p_args
                .iter()
                .map(|a| format!("${}:tt", a))
                .collect::<Vec<_>>();

            format!(
                "    ({shared_args} {p_op} {p_args} $($insns:tt)*) => {{ \
                 {macro_name}!(     \
                 $base_pc;      \
                 $pc;           \
                 [$($labels)*]; \
                 [$($words)*];  \
                 [$($exports),*]; \
                 {op} {args}    \
                 $($insns)*     \
                 );                 \
                 }};",
                shared_args = SHARED_ARGS,
                macro_name = MACRO_NAME,
                p_op = make_mma_insn_name(p_op),
                p_args = p_arg_vars.join(","),
                op = make_mma_insn_name(op),
                args = args
                    .iter()
                    .map(|&a| {
                        p_args
                            .iter()
                            .enumerate()
                            .filter(|(_, &x)| x == a)
                            .nth(0)
                            .map(|(i, _)| p_arg_vars[i].split(":").nth(0).unwrap())
                            .unwrap_or(a)
                    })
                    .collect::<Vec<_>>()
                    .join(",")
            )
        }))
        .chain(
            [
                format!(
                    "    ({shared_args} $label:tt: $($insns:tt)*) => {{ {macro_name}!( \
                     $base_pc; \
                     $pc;\
                     [$($labels)* ($pc, $label),]; \
                     [$($words)*]; \
                     [$($exports),*]; \
                     $($insns)* \
                     ); }};",
                    shared_args = SHARED_ARGS,
                    macro_name = MACRO_NAME
                ),
                format!(
                    "    ({shared_args} nop $($insns:tt)*) => {{ {macro_name}!( \
                     $base_pc; \
                     $pc + 4;\
                     [$($labels)*]; \
                     [$($words)* (0, None),]; \
                     [$($exports),*]; \
                     $($insns)* \
                     ); }};",
                    shared_args = SHARED_ARGS,
                    macro_name = MACRO_NAME
                ),
                format!(
                    "    ({shared_args} .word_sym $sym:ident $($insns:tt)*) => {{ {macro_name}!( \
                     $base_pc; \
                     $pc + 4;\
                     [$($labels)*]; \
                     [$($words)* (0, (32, $sym)),]; \
                     [$($exports),*]; \
                     $($insns)* \
                     ); }};",
                    shared_args = SHARED_ARGS,
                    macro_name = MACRO_NAME
                ),
                format!(
                    "    ({shared_args} .word $val:tt $($insns:tt)*) => {{ {macro_name}!( \
                     $base_pc; \
                     $pc + 4;\
                     [$($labels)*]; \
                     [$($words)* ($val, None),]; \
                     [$($exports),*]; \
                     $($insns)* \
                     ); }};",
                    shared_args = SHARED_ARGS,
                    macro_name = MACRO_NAME
                ),
                format!(
                    "    ({shared_args}) => {{ __impl_assembler_terminal!( \
                     $base_pc; \
                     $pc;\
                     [$($labels)*]; \
                     [$($words)*]; \
                     [$($exports),*]; \
                     ); }};",
                    shared_args = SHARED_ARGS
                ),
            ]
            .iter()
            .cloned(),
        )
        .chain(["}".to_string()].iter().cloned())
        .collect::<Vec<_>>()
        .join("\n");

    writeln!(f, "{}", reg_value_macro)?;
    writeln!(f, "{}", construct_reg_value_macro)?;
    writeln!(f, "{}", make_op_macro)?;
    writeln!(f, "{}", individual_insn_macro)?;

    Ok(())
}
