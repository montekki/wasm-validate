use super::context::ModuleContext;
use super::{Error, FunctionValidator};
use parity_wasm::elements::{Func, FuncBody, Instruction};

pub struct DenyFloatingPointValidator;

impl<'a> FunctionValidator<'a> for DenyFloatingPointValidator {
    fn begin_function(&mut self, _: &ModuleContext, _: &Func, _: &FuncBody) -> Result<(), Error> {
        Ok(())
    }
    fn next_instruction(
        &mut self,
        _: &ModuleContext,
        instruction: &Instruction,
        _: usize,
    ) -> Result<(), Error> {
        self.read_instruction(instruction)?;

        Ok(())
    }

    fn end_function(&mut self) {}
}

impl DenyFloatingPointValidator {
    fn read_instruction(&mut self, instruction: &Instruction) -> Result<(), Error> {
        use parity_wasm::elements::Instruction::*;

        macro_rules! match_eq {
            ($pattern:pat) => {
                |val| if let $pattern = *val { true } else { false }
            };
        }

        const DENIED: &[fn(&Instruction) -> bool] = &[
            match_eq!(F32Load(_, _)),
            match_eq!(F64Load(_, _)),
            match_eq!(F32Store(_, _)),
            match_eq!(F64Store(_, _)),
            match_eq!(F32Const(_)),
            match_eq!(F64Const(_)),
            match_eq!(F32Eq),
            match_eq!(F32Ne),
            match_eq!(F32Lt),
            match_eq!(F32Gt),
            match_eq!(F32Le),
            match_eq!(F32Ge),
            match_eq!(F64Eq),
            match_eq!(F64Ne),
            match_eq!(F64Lt),
            match_eq!(F64Gt),
            match_eq!(F64Le),
            match_eq!(F64Ge),
            match_eq!(F32Abs),
            match_eq!(F32Neg),
            match_eq!(F32Ceil),
            match_eq!(F32Floor),
            match_eq!(F32Trunc),
            match_eq!(F32Nearest),
            match_eq!(F32Sqrt),
            match_eq!(F32Add),
            match_eq!(F32Sub),
            match_eq!(F32Mul),
            match_eq!(F32Div),
            match_eq!(F32Min),
            match_eq!(F32Max),
            match_eq!(F32Copysign),
            match_eq!(F64Abs),
            match_eq!(F64Neg),
            match_eq!(F64Ceil),
            match_eq!(F64Floor),
            match_eq!(F64Trunc),
            match_eq!(F64Nearest),
            match_eq!(F64Sqrt),
            match_eq!(F64Add),
            match_eq!(F64Sub),
            match_eq!(F64Mul),
            match_eq!(F64Div),
            match_eq!(F64Min),
            match_eq!(F64Max),
            match_eq!(F64Copysign),
            match_eq!(F32ConvertSI32),
            match_eq!(F32ConvertUI32),
            match_eq!(F32ConvertSI64),
            match_eq!(F32ConvertUI64),
            match_eq!(F32DemoteF64),
            match_eq!(F64ConvertSI32),
            match_eq!(F64ConvertUI32),
            match_eq!(F64ConvertSI64),
            match_eq!(F64ConvertUI64),
            match_eq!(F64PromoteF32),
            match_eq!(F32ReinterpretI32),
            match_eq!(F64ReinterpretI64),
            match_eq!(I32TruncSF32),
            match_eq!(I32TruncUF32),
            match_eq!(I32TruncSF64),
            match_eq!(I32TruncUF64),
            match_eq!(I64TruncSF32),
            match_eq!(I64TruncUF32),
            match_eq!(I64TruncSF64),
            match_eq!(I64TruncUF64),
            match_eq!(I32ReinterpretF32),
            match_eq!(I64ReinterpretF64),
        ];

        if DENIED.iter().any(|is_denied| is_denied(instruction)) {
            return Err(Error(format!(
                "Floating point operation denied: {:?}",
                instruction
            )));
        }

        Ok(())
    }
}
