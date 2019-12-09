use reality_mips_isa_specification::*;
use std::collections::BTreeMap;
use std::env;

use std::fmt;
use std::fmt::Write;
use std::fs::File;
use std::io;
use std::io::Write as OtherWrite;
use std::path::Path;

trait JoinHelper {
    fn join(self, sep: &str) -> String;
}

impl<T, S> JoinHelper for T
where
    T: Iterator<Item = S>,
    String: From<S>,
{
    fn join(self, sep: &str) -> String {
        self.map(String::from).collect::<Vec<_>>().join(sep)
    }
}

#[derive(Debug)]
enum AppError {
    Io(io::Error),
    Fmt(fmt::Error),
}

impl From<fmt::Error> for AppError {
    fn from(e: fmt::Error) -> Self {
        AppError::Fmt(e)
    }
}

impl From<io::Error> for AppError {
    fn from(e: io::Error) -> Self {
        AppError::Io(e)
    }
}

pub enum Op<T> {
    Data(T, u32),
    EndScope(&'static str),
}

fn generate_instruction_match<'a>(
    opcodes: &[OpcodeSpec],
    insns: &'a [InsnSpec],
    word_var: &str,
    prelude: &str,
    worker: impl Fn(usize, &&(&str, &str, u8, &[&str])) -> String,
) -> Result<(BTreeMap<&'static str, (u32, &'a InsnSpec<'a>)>, String), fmt::Error> {
    let mut insns_by_kind = insns.iter().fold(BTreeMap::new(), |mut acc, cur| {
        acc.entry(cur.0).or_insert_with(|| Vec::new()).push(cur);

        acc
    });

    for (_, v) in insns_by_kind.iter_mut() {
        v.sort_by_key(|i| i.2);
    }

    let mut output = prelude.to_string();
    let mut todo = vec![(1, Op::Data(&opcodes[0], 0))];
    let mut encodings = BTreeMap::new();

    while let Some((depth, x)) = todo.pop() {
        match x {
            Op::Data(x, base) => {
                if let Some((_, val)) = x.3 {
                    write!(
                        &mut output,
                        "{:width$}0x{:02X} => ",
                        "",
                        val,
                        width = depth * 4
                    )?;
                } else {
                    write!(&mut output, "{:width$}", "", width = depth * 4)?;
                }

                writeln!(
                    &mut output,
                    "{}match ({} >> {}) as u8 & 0x{:02X} {{",
                    if x.3.is_none() { "Ok(" } else { "" },
                    word_var,
                    x.1,
                    (1 << x.2) - 1
                )?;

                for i in insns_by_kind.get(x.0).iter().flat_map(|x| x.iter()) {
                    write!(
                        &mut output,
                        "{:width$}0x{:02X} => ",
                        "",
                        i.2,
                        width = (depth + 1) * 4
                    )?;
                    // name = i.1

                    writeln!(&mut output, "{},", worker(depth, i))?;

                    encodings.insert(i.1, (base | ((i.2 as u32) << x.1), *i));
                }

                todo.push((depth, Op::EndScope(x.0)));

                for child in opcodes.iter().filter(|o| match o.3 {
                    Some((name, _)) => name == x.0,
                    _ => false,
                }) {
                    todo.push((
                        depth + 1,
                        Op::Data(child, base | ((child.3.unwrap().1 as u32) << x.1)),
                    ));
                }
            }
            Op::EndScope(name) => {
                writeln!(
                    &mut output,
                    "{:width$}x => Err(Unknown{}Opcode(x))?,",
                    "",
                    name,
                    width = (depth + 1) * 4
                )?;
                writeln!(
                    &mut output,
                    "{:width$}}}{}",
                    "",
                    if todo.len() > 0 { "," } else { ")" },
                    width = depth * 4
                )?;
            }
        }
    }

    output.push_str(
        "
}",
    );

    Ok((encodings, output))
}

fn just_return_id(_depth: usize, i: &&(&str, &str, u8, &[&str])) -> String {
    format!("{}", i.1)
}

fn make_macro(_depth: usize, i: &&(&str, &str, u8, &[&str])) -> String {
    format!("{}::$action()", i.1)
}

fn magic_beans(depth: usize, i: &&(&str, &str, u8, &[&str])) -> String {
    let args = i.3;

    let to_remove = args.last().and_then(|v| match *v {
        "Base" => Some((2, "write_offset_and_base")),
        "BranchTarget" => Some((1, "write_branch_target")),
        "JumpTarget" => Some((1, "write_jump_target")),
        _ if i.1 == "LUI" => Some((1, "write_upper_immediate")),
        _ if i.1 == "ADDIU" => Some((1, "write_potential_lo16")),
        _ => None,
    });

    let base_args = &args[0..(args.len() - to_remove.unwrap_or((0, "")).0)];

    let base_write = format!(
        "write!(f, \"{{:<width$}}{args_fmt}{sep}\", {args_val})?",
        sep = if to_remove.is_some() && base_args.len() > 0 {
            ","
        } else {
            ""
        },
        args_fmt = base_args.iter().map(|_| "{}").collect::<Vec<_>>().join(","),
        args_val = [if i.0 == "Pseudo" {
            format!("{:?}", i.1)
        } else {
            format!("{}::name()", i.1)
        }]
        .iter()
        .cloned()
        .chain(base_args.iter().map(|v| format!("{}::parse(word)", v)))
        .chain(["width = width".to_string()].iter().cloned())
        .collect::<Vec<_>>()
        .join(", "),
    );

    let pseudo_op: Option<(_, _, &'static [&'static str])> = match i.1 {
        "SLL" => Some(("word == 0", "nop", &[])),
        "ADDIU" => Some(("Rs::parse(word) == Gp::Zero", "li", &["Rt", "Imm"])),
        "BEQ" => Some(("Rs::parse(word) == Rt::parse(word)", "b", &["BranchTarget"])),
        "OR" => Some(("Rt::parse(word) == Gp::Zero", "move", &["Rd", "Rs"])),
        _ => None,
    };

    let actual_op = if let Some((_to_remove, kind)) = to_remove {
        //let excess = &args[0..args.len() - to_remove];

        format!(
            "{{\n\
             {0:width$}{base};\n\
             {0:width$}{kind}(f, context, word)?\n\
             {0:width2$}}}",
            "",
            base = base_write,
            kind = kind,
            width = (depth + 2) * 4,
            width2 = (depth + 1) * 4
        )
    } else {
        format!("{}", base_write)
    };

    if let Some((condition, opcode, args)) = pseudo_op {
        format!(
            "{{
                if {condition} {{
                    {pseudo}
                }} else {{
                    {actual}
                }}
            }}",
            condition = condition,
            pseudo = magic_beans(depth, &&("Pseudo", opcode, 0xFF, args)),
            actual = actual_op
        )
    } else {
        format!("{}", actual_op)
    }
}

fn main() -> Result<(), AppError> {
    let spec = &SPEC;

    let (map, str) = generate_instruction_match(&spec.0, &spec.2, "word", "
pub fn disassemble<T: SymbolResolver>(word:    u32,
                                      width:   usize,
                                      context: &DisassemblyContext<T>,
                                      f:       &mut impl io::Write) -> Result<(), DisassembleError> {
    use self::DisassembleError::*;

", magic_beans)?;

    let (_, get_insn_id) = generate_instruction_match(
        &spec.0,
        &spec.2,
        "word",
        "
pub fn decode(word: u32) -> Result<Instruction, DisassembleError> {
    use self::DisassembleError::*;
    use self::Instruction::*;

",
        just_return_id,
    )?;

    let (_, mut match_macro) = generate_instruction_match(
        &spec.0,
        &spec.2,
        "$word",
        "
macro_rules! for_instruction {
    ($word:expr, $action:ident) => {
",
        make_macro,
    )?;

    match_macro.push_str("}");

    let _insns = map
        .iter()
        .map(|(name, (base, info))| {
            let nice_name = name.to_ascii_lowercase().replace("_", ".");

            format!(
                "#[allow(non_camel_case_types)]
struct {};

impl Instruction for {} {{
    type Arguments = ({});

    fn construct() -> u32          {{ 0x{:08X} }}
    fn name()      -> &'static str {{ {:?} }}
}}
",
                name,
                name,
                info.3.iter().cloned().collect::<Vec<_>>().join(", "),
                base,
                nice_name
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    let _regs = spec
        .1
        .iter()
        .flat_map(|x| x.1.iter().map(move |y| (x.0, y)))
        .map(|(kind, (arg_name, shift))| {
            format!(
                "pub struct {};

impl InstructionArgument for {} {{
    type Value = {};

    fn construct(val: Self::Value) -> u32         {{ (val.as_u8() as u32) << {} }}
    fn parse    (word: u32)        -> Self::Value {{ {}::from_u8((word >> {}) as u8) }}
}}",
                arg_name, arg_name, kind, shift, kind, shift
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    let error_kinds = ["IoError(io::Error)".to_string()]
        .iter()
        .cloned()
        .chain(spec.0.iter().map(|k| format!("Unknown{}Opcode(u8)", k.0)))
        .map(|v| format!("    {}", v))
        .collect::<Vec<_>>()
        .join(",\n");

    let _error_kinds = format!(
        "#[derive(Debug)]
pub enum DisassembleError {{\n{}\n}}

impl From<io::Error> for DisassembleError {{
    fn from(val: io::Error) -> Self {{
        DisassembleError::IoError(val)
    }}
}}
",
        error_kinds
    );

    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("isa.rs");
    let mut f = File::create(&dest_path).unwrap();

    writeln!(&mut f, "{}\n", str)?;
    //write!(&mut f, "{}\n", insns)?;
    //write!(&mut f, "{}\n", regs)?;
    //write!(&mut f, "{}\n", error_kinds)?;

    /*write!(&mut f, "
    macro_rules! impl_as_u8 {{
        ($name:ident) => {{
            impl $name {{
                pub fn as_u8(self) -> u8 {{
                    unsafe {{ std::mem::transmute(self) }}
                }}

                pub fn from_u8(val: u8) -> Self {{
                    unsafe {{ std::mem::transmute(val & 0x1F) }}
                }}
            }}
        }}
    }}
    ")?;*/

    //write!(&mut f, "{}\n", match_macro);
    writeln!(&mut f, "{}\n", get_insn_id)?;

    dump_to_file("regs.rs", generate_registers());
    dump_to_file("reg_operands.rs", generate_register_operands());
    dump_to_file(
        "instruction.rs",
        [
            generate_decode_error_enum(),
            generate_instruction_enum(),
            generate_decode(),
            generate_opcodes(),
        ]
        .iter()
        .join("\n"),
    );
    dump_to_file("disassemble.rs", generate_disassembler());

    //part_two()
    Ok(())
}

fn dump_to_file(filename: &str, contents: String) {
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join(filename);
    let mut f = File::create(&dest_path).unwrap();

    writeln!(&mut f, "{}", contents).unwrap();
}

fn generate_decode_error_enum() -> String {
    format!(
        r"
#[derive(Copy, Clone, Debug)]
pub enum DecodeError {{
    {}
}}
",
        SPEC.0
            .iter()
            .map(|k| format!("Unknown{}Opcode(u8)", k.0))
            .join(",\n    ")
    )
}

fn generate_instruction_enum() -> String {
    format!(
        "#[allow(non_camel_case_types)]\n\
         #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]\n\
         pub enum Instruction {{\n{}\n}}",
        SPEC.2
            .iter()
            .map(|(_, name, _, args)| format!("    {}{}", name, {
                if args.len() > 0 {
                    format!(
                        "({})",
                        args.iter()
                            .map(|&a| match a {
                                "Rd" | "Rs" | "Rt" | "Base" => "Gpr",
                                "Cop0Rd" => "Mmu",
                                "FcrRd" => "Fcr",
                                "Imm" => "i16",
                                "VerbatimImm" => "u16",
                                "Fd" | "Fs" | "Ft" => "Fpr",
                                "Displace" | "Offset" | "CacheOp" | "ShiftAmount"
                                | "SyscallCode" | "BreakCode" | "JumpTarget" | "BranchTarget" => a,
                                x => panic!("{} is unimplemented", x),
                            })
                            .join(", ")
                    )
                } else {
                    "".into()
                }
            }))
            .join(",\n")
    )
}

fn generate_register_operands() -> String {
    SPEC.1
        .iter()
        .flat_map(|(series, ops)| ops.iter().map(move |o| (series, o)))
        .map(|(series, (name, shift))| {
            format!(
                r"
pub struct {name};

impl InstructionOperand for {name} {{
    type Output = {};

    const SHIFT: u32 = {};
    const WIDTH: u32 = {};
}}",
                match *series {
                    "Gp" => "Gpr",
                    "Fp" => "Fpr",
                    "Fc" => "Fcr",
                    "C0" => "Mmu",
                    x => panic!("{} is unhandled", x),
                },
                shift,
                5,
                name = name
            )
        })
        .join("\n")
}

fn generate_registers() -> String {
    SPEC.3
        .iter()
        .map(|(series, names, display_names)| {
            format!(
                r"
#[allow(non_camel_case_types)]
#[derive(Debug, Copy, Clone, PartialOrd, PartialEq, Ord, Eq)]
pub enum {type_name} {{
    {}
}}

impl From<RawInstructionOperand> for {type_name} {{
    fn from(src: RawInstructionOperand) -> {type_name} {{
        use {type_name}::*;

        match src.0 {{
            {}
            _ => unreachable!(),
        }}
    }}
}}

impl From<{type_name}> for RawInstructionOperand {{
    fn from(src: {type_name}) -> RawInstructionOperand {{
        use {type_name}::*;

        RawInstructionOperand(match src {{
            {var_to_int}
        }})
    }}
}}

impl From<{type_name}> for u8 {{
    fn from(src: {type_name}) -> u8 {{
        use {type_name}::*;

        match src {{
            {var_to_int}
        }}
    }}
}}

impl Display for {type_name} {{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {{
        use {type_name}::*;

        <str as Display>::fmt(match self {{
            {}
        }}, f)
    }}
}}
",
                names.iter().map(|n| format!("{},", n)).join("\n    "),
                names
                    .iter()
                    .enumerate()
                    .map(|(i, n)| format!("{} => {},", i, n))
                    .join("\n            "),
                names
                    .iter()
                    .zip(display_names)
                    .map(|(name, display)| format!("{} => \"{}\",", name, display))
                    .join("\n            "),
                type_name = match *series {
                    "Gp" => "Gpr",
                    "Fp" => "Fpr",
                    "Fc" => "Fcr",
                    "C0" => "Mmu",
                    x => panic!("unsupported reg series: {}", x),
                },
                var_to_int = names
                    .iter()
                    .enumerate()
                    .map(|(i, n)| format!("{} => {},", n, i))
                    .join("\n            "),
            )
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn generate_decode() -> String {
    let (_, text) = generate_instruction_match(
        &SPEC.0,
        &SPEC.2,
        "word",
        "
pub fn decode(word: u32) -> Result<Instruction, DecodeError> {
    let w = InstructionWord(word);

",
        |_id, (_series, opcode, _val, args)| {
            format!("{}{}", opcode, {
                if args.len() == 0 {
                    return (*opcode).into();
                }

                format!(
                    "({})",
                    args.iter()
                        .map(|a| match *a {
                            "Imm" => "(word & 0xFFFF) as i16".into(),
                            "VerbatimImm" => "word as u16".into(),
                            "Displace" => "Displace(Offset::read(w), Base::read(w))".into(),
                            _ => format!("{}::read(w)", a),
                        })
                        .join(", ")
                )
            })
        },
    )
    .unwrap();

    text
}

fn generate_opcodes() -> String {
    let lines = SPEC
        .2
        .iter()
        .map(|(_series, op, _v, args)| {
            format!(
                "{}{} => Opcode::{},",
                op,
                if args.len() > 0 { "(..)" } else { "" },
                op
            )
        })
        .join("\n            ");

    format!(
        r"
impl Instruction {{
    pub fn op(&self) -> Opcode {{
        use Instruction::*;

        match self {{
            {}
        }}
    }}
}}

#[allow(non_camel_case_types)]
#[derive(Debug, Copy, Clone, PartialOrd, PartialEq, Ord, Eq)]
pub enum Opcode {{
    {}
}}",
        lines,
        SPEC.2.iter().map(|(_, op, _, _)| *op).join(",\n    ")
    )
}

fn generate_disassembler() -> String {
    format!("
pub fn write_disassembly<S, R, P>(f: &mut Formatter, word: u32, width: usize, pc: P, syms: S, relocs: R) -> Result<(), fmt::Error>
where S: Symbols + Copy,
      R: Relocations + Copy,
      P: Copy,
      Wrapper<S, P, JumpTarget>: Display,
      Wrapper<S, P, BranchTarget>: Display,
      Wrapper<R, P, Displace>: Display,
      Wrapper<R, P, u16>: Display
{{
    let insn = match decode(word) {{
        Err(e) => return write!(f, \"/* {{:?}} */\", e),
        Ok(i) => i
    }};

    match (insn, insn.pseudo()) {{
        {}
    }}
}}",
    SPEC.2.iter()
        .map(|(_, op, _, args)| (false, op, args))
        .chain(PSEUDO_SPEC.iter()
            .map(|(op, args, _, _)| (true, op, args)))
        .map(|(is_pseudo, op, args)| {
            let readable_op = op.replace("_", ".").to_lowercase();
            let names = ["a", "b", "c"];

            let mk_match = |part| {
                if is_pseudo {
                    format!("(_, Some({}))", part)
                } else {
                    format!("({}, _)", part)
                }
            };

            format!(
                "{} => write!(f, \"{{{width_param}}}{}\", \"{}\", {}{width_arg})",
                mk_match(format!(
                    "{}{}",
                    op,
                    if args.len() == 0 {
                        "".into()
                    } else {
                        format!("({})", args.iter()
                            .enumerate()
                            .map(|(i, _)| names[i])
                            .join(", "))
                    }
                )),
                args.iter().map(|x| match *x {
                    "VerbatimImm" => "0x{:04x}",
                    _ => "{}"
                }).join(","),
                readable_op,
                if args.len() == 0 {
                    "".into()
                } else {
                    [args.iter()
                        .enumerate()
                        .map(|(i, a)| {
                            match *a {
                                "JumpTarget" | "BranchTarget" => format!("Wrapper(syms, pc, {})", names[i]),
                                "Displace" => format!("Wrapper(relocs, pc, {})", names[i]),
                                _ => format!("{}", names[i]),
                            }
                        })
                        .join(", "), "".into()].iter().join(", ")

                },
                width_param = if args.len() > 0 { ":<width$" } else { "" },
                width_arg = if args.len() > 0 { "width = width" } else { "" }
            )
        })
        .chain(vec!["_ if word == 0 => write!(f, \"nop\")".to_owned()])
        .rev()
        .join(",\n        ")
    )
}
