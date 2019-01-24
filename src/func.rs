use super::common::stack::StackWithLimit;
use super::common::{DEFAULT_MEMORY_INDEX, DEFAULT_TABLE_INDEX};
use super::{Error, FunctionValidator, Outcome};
use core::u32;
use parity_wasm::elements::{BlockType, Func, FuncBody, Instruction, TableElementType, ValueType};

use super::context::ModuleContext;
use super::util::Locals;

/// Maximum number of entries in value stack per function.
const DEFAULT_VALUE_STACK_LIMIT: usize = 16384;
/// Maximum number of entries in frame stack per function.
const DEFAULT_FRAME_STACK_LIMIT: usize = 16384;

/// Control stack frame.
#[derive(Debug, Clone)]
struct BlockFrame {
    /// Frame type.
    frame_type: BlockFrameType,
    /// A signature, which is a block signature type indicating the number and types of result values of the region.
    block_type: BlockType,
    /// A label for reference to block instruction.
    begin_position: usize,
    /// A limit integer value, which is an index into the value stack indicating where to reset it to on a branch to that label.
    value_stack_len: usize,
    /// Boolean which signals whether value stack became polymorphic. Value stack starts in non-polymorphic state and
    /// becomes polymorphic only after an instruction that never passes control further is executed,
    /// i.e. `unreachable`, `br` (but not `br_if`!), etc.
    polymorphic_stack: bool,
}

/// The target of a branch instruction.
///
/// It references a `LabelId` instead of exact instruction address. This is handy
/// for emitting code right away with labels resolved later.
#[derive(Clone)]
struct Target {}

/// Identifier of a label.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct LabelId(usize);

/// Type of block frame.
#[derive(Debug, Clone, Copy, PartialEq)]
enum BlockFrameType {
    /// Usual block frame.
    ///
    /// Can be used for an implicit function block.
    Block,
    /// Loop frame (branching to the beginning of block).
    Loop,
    /// True-subblock of if expression.
    IfTrue,
    /// False-subblock of if expression.
    IfFalse,
}

impl BlockFrameType {
    fn is_loop(&self) -> bool {
        match *self {
            BlockFrameType::Loop { .. } => true,
            _ => false,
        }
    }
}

/// Value type on the stack.
#[derive(Debug, Clone, Copy)]
enum StackValueType {
    /// Any value type.
    Any,
    /// Concrete value type.
    Specific(ValueType),
}

impl StackValueType {
    fn is_any(&self) -> bool {
        match self {
            &StackValueType::Any => true,
            _ => false,
        }
    }

    fn value_type(&self) -> ValueType {
        match self {
            &StackValueType::Any => unreachable!("must be checked by caller"),
            &StackValueType::Specific(value_type) => value_type,
        }
    }
}

impl From<ValueType> for StackValueType {
    fn from(value_type: ValueType) -> Self {
        StackValueType::Specific(value_type)
    }
}

impl PartialEq<StackValueType> for StackValueType {
    fn eq(&self, other: &StackValueType) -> bool {
        if self.is_any() || other.is_any() {
            true
        } else {
            self.value_type() == other.value_type()
        }
    }
}

impl PartialEq<ValueType> for StackValueType {
    fn eq(&self, other: &ValueType) -> bool {
        if self.is_any() {
            true
        } else {
            self.value_type() == *other
        }
    }
}

impl PartialEq<StackValueType> for ValueType {
    fn eq(&self, other: &StackValueType) -> bool {
        other == self
    }
}

struct FunctionContext<'a> {
    /// Local variables.
    locals: Locals<'a>,
    /// Value stack.
    /// Value stack.
    value_stack: StackWithLimit<StackValueType>,
    /// Frame stack.
    frame_stack: StackWithLimit<BlockFrame>,
    /// Function return type.
    return_type: BlockType,
}

impl<'a> FunctionContext<'a> {
    fn new(
        locals: Locals<'a>,
        value_stack_limit: usize,
        frame_stack_limit: usize,
        return_type: BlockType,
    ) -> Self {
        FunctionContext {
            locals,
            value_stack: StackWithLimit::with_limit(value_stack_limit),
            frame_stack: StackWithLimit::with_limit(frame_stack_limit),
            return_type,
        }
    }
}

pub struct DefaultFuncValidator<'a> {
    /// Wasm module
    module: Option<&'a ModuleContext>,
    /// Current function context
    context: Option<FunctionContext<'a>>,
}

impl<'a> FunctionValidator<'a> for DefaultFuncValidator<'a> {
    fn begin_function(
        &mut self,
        module: &ModuleContext,
        func: &'a Func,
        body: &'a FuncBody,
    ) -> Result<(), Error> {
        let (params, result_ty) = module.require_function_type(func.type_ref())?;
        let p = params.to_vec();

        let context = FunctionContext::new(
            Locals::new(p, body.locals())?,
            DEFAULT_VALUE_STACK_LIMIT,
            DEFAULT_FRAME_STACK_LIMIT,
            result_ty,
        );

        self.context = Some(context);
        println!("begin_function");
        Ok(())
    }
    fn next_instruction(
        &mut self,
        _context: &ModuleContext,
        instruction: &Instruction,
        position: usize,
    ) -> Result<(), Error> {
        println!("next_instruction");
        let outcome = self.read_instruction(instruction, position)?;
        if let Some(context) = &mut self.context {
            match outcome {
                Outcome::NextInstruction => (),
                Outcome::Unreachable => {
                    make_top_frame_polymorphic(&mut context.value_stack, &mut context.frame_stack)
                }
            }
            return Ok(());
        } else {
            return Err(Error("".into()));
        }
    }

    fn end_function(&mut self) {}
}

impl<'a> DefaultFuncValidator<'a> {
    pub fn new() -> Self {
        DefaultFuncValidator {
            module: None,
            context: None,
        }
    }

    pub fn set_module(&mut self, module: &'a ModuleContext) {
        self.module = Some(module);
    }
    fn get_context(&mut self) -> Result<&mut FunctionContext<'a>, Error> {
        match self.context {
            Some(ref mut ctx) => return Ok(ctx),
            None => return Err(Error("".into())),
        }
    }

    fn get_module(&self) -> Result<&'a ModuleContext, Error> {
        match self.module {
            Some(ref m) => return Ok(m),
            None => return Err(Error("".into())),
        }
    }

    fn read_instruction(
        &mut self,
        instruction: &Instruction,
        position: usize,
    ) -> Result<Outcome, Error> {
        use self::Instruction::*;
        let context = self.get_context()?;

        match *instruction {
            // Nop instruction doesn't do anything. It is safe to just skip it.
            Nop => {}

            Unreachable => {
                return Ok(Outcome::Unreachable);
            }

            Block(block_type) => {
                push_label(
                    BlockFrameType::Block,
                    block_type,
                    position,
                    &context.value_stack,
                    &mut context.frame_stack,
                )?;
            }
            Loop(block_type) => {
                push_label(
                    BlockFrameType::Loop,
                    block_type,
                    position,
                    &context.value_stack,
                    &mut context.frame_stack,
                )?;
            }
            If(block_type) => {
                pop_value(
                    &mut context.value_stack,
                    &context.frame_stack,
                    ValueType::I32.into(),
                )?;
                push_label(
                    BlockFrameType::IfTrue,
                    block_type,
                    position,
                    &context.value_stack,
                    &mut context.frame_stack,
                )?;
            }
            Else => {
                let block_type = {
                    let top_frame = top_label(&context.frame_stack);

                    match top_frame.frame_type {
                        BlockFrameType::IfTrue => (),
                        _ => return Err(Error("Misplaced else instruction".into())),
                    };
                    top_frame.block_type
                };
                pop_label(&mut context.value_stack, &mut context.frame_stack)?;
                push_label(
                    BlockFrameType::IfFalse,
                    block_type,
                    position,
                    &context.value_stack,
                    &mut context.frame_stack,
                )?;
            }
            End => {
                let (frame_type, block_type) = {
                    let top = top_label(&context.frame_stack);
                    (top.frame_type, top.block_type)
                };

                if let BlockFrameType::IfTrue = frame_type {
                    // A `if` without an `else` can't return a result.
                    if block_type != BlockType::NoResult {
                        return Err(Error(format!(
                                    "If block without else required to have NoResult block type. But it has {:?} type",
                                    block_type
                        )));
                    }
                }

                if context.frame_stack.len() == 1 {
                    // We are about to close the last frame. Insert
                    // an explicit return.

                    // Check the return type.
                    if let BlockType::Value(value_type) = context.return_type {
                        tee_value(
                            &mut context.value_stack,
                            &context.frame_stack,
                            value_type.into(),
                        )?;
                    }
                }

                pop_label(&mut context.value_stack, &mut context.frame_stack)?;

                // Push the result value.
                if let BlockType::Value(value_type) = block_type {
                    push_value(&mut context.value_stack, value_type.into())?;
                }
            }
            Br(depth) => {
                self.validate_br(depth)?;

                return Ok(Outcome::Unreachable);
            }
            BrIf(depth) => {
                self.validate_br_if(depth)?;
            }
            BrTable(ref table, default) => {
                self.validate_br_table(table, default)?;

                return Ok(Outcome::Unreachable);
            }
            Return => {
                if let BlockType::Value(value_type) = context.return_type {
                    tee_value(
                        &mut context.value_stack,
                        &context.frame_stack,
                        value_type.into(),
                    )?;
                }

                return Ok(Outcome::Unreachable);
            }

            Call(index) => {
                self.validate_call(index)?;
            }
            CallIndirect(index, _reserved) => {
                self.validate_call_indirect(index)?;
            }

            Drop => {
                self.validate_drop()?;
            }
            Select => {
                self.validate_select()?;
            }

            GetLocal(index) => {
                // We need to calculate relative depth before validation since
                // it will change the value stack size.
                self.validate_get_local(index)?;
            }
            SetLocal(index) => {
                self.validate_set_local(index)?;
            }
            TeeLocal(index) => {
                self.validate_tee_local(index)?;
            }
            GetGlobal(index) => {
                self.validate_get_global(index)?;
            }
            SetGlobal(index) => {
                self.validate_set_global(index)?;
            }

            I32Load(align, _) => {
                self.validate_load(align, 4, ValueType::I32)?;
            }
            I64Load(align, _) => {
                self.validate_load(align, 8, ValueType::I64)?;
            }
            F32Load(align, _) => {
                self.validate_load(align, 4, ValueType::F32)?;
            }
            F64Load(align, _) => {
                self.validate_load(align, 8, ValueType::F64)?;
            }
            I32Load8S(align, _) => {
                self.validate_load(align, 1, ValueType::I32)?;
            }
            I32Load8U(align, _) => {
                self.validate_load(align, 1, ValueType::I32)?;
            }
            I32Load16S(align, _) => {
                self.validate_load(align, 2, ValueType::I32)?;
            }
            I32Load16U(align, _) => {
                self.validate_load(align, 2, ValueType::I32)?;
            }
            I64Load8S(align, _) => {
                self.validate_load(align, 1, ValueType::I64)?;
            }
            I64Load8U(align, _) => {
                self.validate_load(align, 1, ValueType::I64)?;
            }
            I64Load16S(align, _) => {
                self.validate_load(align, 2, ValueType::I64)?;
            }
            I64Load16U(align, _) => {
                self.validate_load(align, 2, ValueType::I64)?;
            }
            I64Load32S(align, _) => {
                self.validate_load(align, 4, ValueType::I64)?;
            }
            I64Load32U(align, _) => {
                self.validate_load(align, 4, ValueType::I64)?;
            }

            I32Store(align, _) => {
                self.validate_store(align, 4, ValueType::I32)?;
            }
            I64Store(align, _) => {
                self.validate_store(align, 8, ValueType::I64)?;
            }
            F32Store(align, _) => {
                self.validate_store(align, 4, ValueType::F32)?;
            }
            F64Store(align, _) => {
                self.validate_store(align, 8, ValueType::F64)?;
            }
            I32Store8(align, _) => {
                self.validate_store(align, 1, ValueType::I32)?;
            }
            I32Store16(align, _) => {
                self.validate_store(align, 2, ValueType::I32)?;
            }
            I64Store8(align, _) => {
                self.validate_store(align, 1, ValueType::I64)?;
            }
            I64Store16(align, _) => {
                self.validate_store(align, 2, ValueType::I64)?;
            }
            I64Store32(align, _) => {
                self.validate_store(align, 4, ValueType::I64)?;
            }

            CurrentMemory(_) => {
                self.validate_current_memory()?;
            }
            GrowMemory(_) => {
                self.validate_grow_memory()?;
            }

            I32Const(_) => {
                self.validate_const(ValueType::I32)?;
            }
            I64Const(_) => {
                self.validate_const(ValueType::I64)?;
            }
            F32Const(_) => {
                self.validate_const(ValueType::F32)?;
            }
            F64Const(_) => {
                self.validate_const(ValueType::F64)?;
            }

            I32Eqz => {
                self.validate_testop(ValueType::I32)?;
            }
            I32Eq => {
                self.validate_relop(ValueType::I32)?;
            }
            I32Ne => {
                self.validate_relop(ValueType::I32)?;
            }
            I32LtS => {
                self.validate_relop(ValueType::I32)?;
            }
            I32LtU => {
                self.validate_relop(ValueType::I32)?;
            }
            I32GtS => {
                self.validate_relop(ValueType::I32)?;
            }
            I32GtU => {
                self.validate_relop(ValueType::I32)?;
            }
            I32LeS => {
                self.validate_relop(ValueType::I32)?;
            }
            I32LeU => {
                self.validate_relop(ValueType::I32)?;
            }
            I32GeS => {
                self.validate_relop(ValueType::I32)?;
            }
            I32GeU => {
                self.validate_relop(ValueType::I32)?;
            }

            I64Eqz => {
                self.validate_testop(ValueType::I64)?;
            }
            I64Eq => {
                self.validate_relop(ValueType::I64)?;
            }
            I64Ne => {
                self.validate_relop(ValueType::I64)?;
            }
            I64LtS => {
                self.validate_relop(ValueType::I64)?;
            }
            I64LtU => {
                self.validate_relop(ValueType::I64)?;
            }
            I64GtS => {
                self.validate_relop(ValueType::I64)?;
            }
            I64GtU => {
                self.validate_relop(ValueType::I64)?;
            }
            I64LeS => {
                self.validate_relop(ValueType::I64)?;
            }
            I64LeU => {
                self.validate_relop(ValueType::I64)?;
            }
            I64GeS => {
                self.validate_relop(ValueType::I64)?;
            }
            I64GeU => {
                self.validate_relop(ValueType::I64)?;
            }

            F32Eq => {
                self.validate_relop(ValueType::F32)?;
            }
            F32Ne => {
                self.validate_relop(ValueType::F32)?;
            }
            F32Lt => {
                self.validate_relop(ValueType::F32)?;
            }
            F32Gt => {
                self.validate_relop(ValueType::F32)?;
            }
            F32Le => {
                self.validate_relop(ValueType::F32)?;
            }
            F32Ge => {
                self.validate_relop(ValueType::F32)?;
            }

            F64Eq => {
                self.validate_relop(ValueType::F64)?;
            }
            F64Ne => {
                self.validate_relop(ValueType::F64)?;
            }
            F64Lt => {
                self.validate_relop(ValueType::F64)?;
            }
            F64Gt => {
                self.validate_relop(ValueType::F64)?;
            }
            F64Le => {
                self.validate_relop(ValueType::F64)?;
            }
            F64Ge => {
                self.validate_relop(ValueType::F64)?;
            }

            I32Clz => {
                self.validate_unop(ValueType::I32)?;
            }
            I32Ctz => {
                self.validate_unop(ValueType::I32)?;
            }
            I32Popcnt => {
                self.validate_unop(ValueType::I32)?;
            }
            I32Add => {
                self.validate_binop(ValueType::I32)?;
            }
            I32Sub => {
                self.validate_binop(ValueType::I32)?;
            }
            I32Mul => {
                self.validate_binop(ValueType::I32)?;
            }
            I32DivS => {
                self.validate_binop(ValueType::I32)?;
            }
            I32DivU => {
                self.validate_binop(ValueType::I32)?;
            }
            I32RemS => {
                self.validate_binop(ValueType::I32)?;
            }
            I32RemU => {
                self.validate_binop(ValueType::I32)?;
            }
            I32And => {
                self.validate_binop(ValueType::I32)?;
            }
            I32Or => {
                self.validate_binop(ValueType::I32)?;
            }
            I32Xor => {
                self.validate_binop(ValueType::I32)?;
            }
            I32Shl => {
                self.validate_binop(ValueType::I32)?;
            }
            I32ShrS => {
                self.validate_binop(ValueType::I32)?;
            }
            I32ShrU => {
                self.validate_binop(ValueType::I32)?;
            }
            I32Rotl => {
                self.validate_binop(ValueType::I32)?;
            }
            I32Rotr => {
                self.validate_binop(ValueType::I32)?;
            }

            I64Clz => {
                self.validate_unop(ValueType::I64)?;
            }
            I64Ctz => {
                self.validate_unop(ValueType::I64)?;
            }
            I64Popcnt => {
                self.validate_unop(ValueType::I64)?;
            }
            I64Add => {
                self.validate_binop(ValueType::I64)?;
            }
            I64Sub => {
                self.validate_binop(ValueType::I64)?;
            }
            I64Mul => {
                self.validate_binop(ValueType::I64)?;
            }
            I64DivS => {
                self.validate_binop(ValueType::I64)?;
            }
            I64DivU => {
                self.validate_binop(ValueType::I64)?;
            }
            I64RemS => {
                self.validate_binop(ValueType::I64)?;
            }
            I64RemU => {
                self.validate_binop(ValueType::I64)?;
            }
            I64And => {
                self.validate_binop(ValueType::I64)?;
            }
            I64Or => {
                self.validate_binop(ValueType::I64)?;
            }
            I64Xor => {
                self.validate_binop(ValueType::I64)?;
            }
            I64Shl => {
                self.validate_binop(ValueType::I64)?;
            }
            I64ShrS => {
                self.validate_binop(ValueType::I64)?;
            }
            I64ShrU => {
                self.validate_binop(ValueType::I64)?;
            }
            I64Rotl => {
                self.validate_binop(ValueType::I64)?;
            }
            I64Rotr => {
                self.validate_binop(ValueType::I64)?;
            }

            F32Abs => {
                self.validate_unop(ValueType::F32)?;
            }
            F32Neg => {
                self.validate_unop(ValueType::F32)?;
            }
            F32Ceil => {
                self.validate_unop(ValueType::F32)?;
            }
            F32Floor => {
                self.validate_unop(ValueType::F32)?;
            }
            F32Trunc => {
                self.validate_unop(ValueType::F32)?;
            }
            F32Nearest => {
                self.validate_unop(ValueType::F32)?;
            }
            F32Sqrt => {
                self.validate_unop(ValueType::F32)?;
            }
            F32Add => {
                self.validate_binop(ValueType::F32)?;
            }
            F32Sub => {
                self.validate_binop(ValueType::F32)?;
            }
            F32Mul => {
                self.validate_binop(ValueType::F32)?;
            }
            F32Div => {
                self.validate_binop(ValueType::F32)?;
            }
            F32Min => {
                self.validate_binop(ValueType::F32)?;
            }
            F32Max => {
                self.validate_binop(ValueType::F32)?;
            }
            F32Copysign => {
                self.validate_binop(ValueType::F32)?;
            }

            F64Abs => {
                self.validate_unop(ValueType::F64)?;
            }
            F64Neg => {
                self.validate_unop(ValueType::F64)?;
            }
            F64Ceil => {
                self.validate_unop(ValueType::F64)?;
            }
            F64Floor => {
                self.validate_unop(ValueType::F64)?;
            }
            F64Trunc => {
                self.validate_unop(ValueType::F64)?;
            }
            F64Nearest => {
                self.validate_unop(ValueType::F64)?;
            }
            F64Sqrt => {
                self.validate_unop(ValueType::F64)?;
            }
            F64Add => {
                self.validate_binop(ValueType::F64)?;
            }
            F64Sub => {
                self.validate_binop(ValueType::F64)?;
            }
            F64Mul => {
                self.validate_binop(ValueType::F64)?;
            }
            F64Div => {
                self.validate_binop(ValueType::F64)?;
            }
            F64Min => {
                self.validate_binop(ValueType::F64)?;
            }
            F64Max => {
                self.validate_binop(ValueType::F64)?;
            }
            F64Copysign => {
                self.validate_binop(ValueType::F64)?;
            }

            I32WrapI64 => {
                self.validate_cvtop(ValueType::I64, ValueType::I32)?;
            }
            I32TruncSF32 => {
                self.validate_cvtop(ValueType::F32, ValueType::I32)?;
            }
            I32TruncUF32 => {
                self.validate_cvtop(ValueType::F32, ValueType::I32)?;
            }
            I32TruncSF64 => {
                self.validate_cvtop(ValueType::F64, ValueType::I32)?;
            }
            I32TruncUF64 => {
                self.validate_cvtop(ValueType::F64, ValueType::I32)?;
            }
            I64ExtendSI32 => {
                self.validate_cvtop(ValueType::I32, ValueType::I64)?;
            }
            I64ExtendUI32 => {
                self.validate_cvtop(ValueType::I32, ValueType::I64)?;
            }
            I64TruncSF32 => {
                self.validate_cvtop(ValueType::F32, ValueType::I64)?;
            }
            I64TruncUF32 => {
                self.validate_cvtop(ValueType::F32, ValueType::I64)?;
            }
            I64TruncSF64 => {
                self.validate_cvtop(ValueType::F64, ValueType::I64)?;
            }
            I64TruncUF64 => {
                self.validate_cvtop(ValueType::F64, ValueType::I64)?;
            }
            F32ConvertSI32 => {
                self.validate_cvtop(ValueType::I32, ValueType::F32)?;
            }
            F32ConvertUI32 => {
                self.validate_cvtop(ValueType::I32, ValueType::F32)?;
            }
            F32ConvertSI64 => {
                self.validate_cvtop(ValueType::I64, ValueType::F32)?;
            }
            F32ConvertUI64 => {
                self.validate_cvtop(ValueType::I64, ValueType::F32)?;
            }
            F32DemoteF64 => {
                self.validate_cvtop(ValueType::F64, ValueType::F32)?;
            }
            F64ConvertSI32 => {
                self.validate_cvtop(ValueType::I32, ValueType::F64)?;
            }
            F64ConvertUI32 => {
                self.validate_cvtop(ValueType::I32, ValueType::F64)?;
            }
            F64ConvertSI64 => {
                self.validate_cvtop(ValueType::I64, ValueType::F64)?;
            }
            F64ConvertUI64 => {
                self.validate_cvtop(ValueType::I64, ValueType::F64)?;
            }
            F64PromoteF32 => {
                self.validate_cvtop(ValueType::F32, ValueType::F64)?;
            }

            I32ReinterpretF32 => {
                self.validate_cvtop(ValueType::F32, ValueType::I32)?;
            }
            I64ReinterpretF64 => {
                self.validate_cvtop(ValueType::F64, ValueType::I64)?;
            }
            F32ReinterpretI32 => {
                self.validate_cvtop(ValueType::I32, ValueType::F32)?;
            }
            F64ReinterpretI64 => {
                self.validate_cvtop(ValueType::I64, ValueType::F64)?;
            }
        }

        Ok(Outcome::NextInstruction)
    }

    fn validate_const(&mut self, value_type: ValueType) -> Result<(), Error> {
        if let Some(context) = &mut self.context {
            push_value(&mut context.value_stack, value_type.into())?;
            return Ok(());
        }
        Err(Error("".into()))
    }

    fn validate_unop(&mut self, value_type: ValueType) -> Result<(), Error> {
        if let Some(context) = &mut self.context {
            pop_value(
                &mut context.value_stack,
                &context.frame_stack,
                value_type.into(),
            )?;
            push_value(&mut context.value_stack, value_type.into())?;
            return Ok(());
        }
        Err(Error("".into()))
    }

    fn validate_binop(&mut self, value_type: ValueType) -> Result<(), Error> {
        if let Some(context) = &mut self.context {
            pop_value(
                &mut context.value_stack,
                &context.frame_stack,
                value_type.into(),
            )?;
            pop_value(
                &mut context.value_stack,
                &context.frame_stack,
                value_type.into(),
            )?;
            push_value(&mut context.value_stack, value_type.into())?;
            return Ok(());
        }
        Err(Error("".into()))
    }

    fn validate_testop(&mut self, value_type: ValueType) -> Result<(), Error> {
        if let Some(context) = &mut self.context {
            pop_value(
                &mut context.value_stack,
                &context.frame_stack,
                value_type.into(),
            )?;
            push_value(&mut context.value_stack, ValueType::I32.into())?;
            return Ok(());
        }
        Err(Error("".into()))
    }
    fn validate_relop(&mut self, value_type: ValueType) -> Result<(), Error> {
        if let Some(context) = &mut self.context {
            pop_value(
                &mut context.value_stack,
                &context.frame_stack,
                value_type.into(),
            )?;
            pop_value(
                &mut context.value_stack,
                &context.frame_stack,
                value_type.into(),
            )?;
            push_value(&mut context.value_stack, ValueType::I32.into())?;
            return Ok(());
        }
        Err(Error("".into()))
    }

    fn validate_cvtop(
        &mut self,
        value_type1: ValueType,
        value_type2: ValueType,
    ) -> Result<(), Error> {
        if let Some(context) = &mut self.context {
            pop_value(
                &mut context.value_stack,
                &context.frame_stack,
                value_type1.into(),
            )?;
            push_value(&mut context.value_stack, value_type2.into())?;
            return Ok(());
        }
        Err(Error("".into()))
    }

    fn validate_drop(&mut self) -> Result<(), Error> {
        if let Some(context) = &mut self.context {
            pop_value(
                &mut context.value_stack,
                &context.frame_stack,
                StackValueType::Any,
            )?;
            return Ok(());
        }
        Err(Error("".into()))
    }
    fn validate_select(&mut self) -> Result<(), Error> {
        if let Some(context) = &mut self.context {
            pop_value(
                &mut context.value_stack,
                &context.frame_stack,
                ValueType::I32.into(),
            )?;
            let select_type = pop_value(
                &mut context.value_stack,
                &context.frame_stack,
                StackValueType::Any,
            )?;
            pop_value(&mut context.value_stack, &context.frame_stack, select_type)?;
            push_value(&mut context.value_stack, select_type)?;
            return Ok(());
        }
        Err(Error("".into()))
    }

    fn validate_get_local(&mut self, index: u32) -> Result<(), Error> {
        if let Some(context) = &mut self.context {
            let local_type = require_local(&context.locals, index)?;
            push_value(&mut context.value_stack, local_type.into())?;
            return Ok(());
        }
        Err(Error("".into()))
    }

    fn validate_set_local(&mut self, index: u32) -> Result<(), Error> {
        if let Some(context) = &mut self.context {
            let local_type = require_local(&context.locals, index)?;
            let value_type = pop_value(
                &mut context.value_stack,
                &context.frame_stack,
                StackValueType::Any,
            )?;
            if StackValueType::from(local_type) != value_type {
                return Err(Error(format!(
                    "Trying to update local {} of type {:?} with value of type {:?}",
                    index, local_type, value_type
                )));
            }
            return Ok(());
        }

        Err(Error("".into()))
    }

    fn validate_tee_local(&mut self, index: u32) -> Result<(), Error> {
        if let Some(context) = &mut self.context {
            let local_type = require_local(&context.locals, index)?;
            tee_value(
                &mut context.value_stack,
                &context.frame_stack,
                local_type.into(),
            )?;
            return Ok(());
        }
        Err(Error("".into()))
    }

    fn validate_get_global(&mut self, index: u32) -> Result<(), Error> {
        let global_type: StackValueType = {
            let global = self.get_module()?.require_global(index, None)?;
            global.content_type().into()
        };
        push_value(&mut self.get_context()?.value_stack, global_type)?;
        return Ok(());
    }
    fn validate_set_global(&mut self, index: u32) -> Result<(), Error> {
        let global_type: StackValueType = {
            let global = self.get_module()?.require_global(index, Some(true))?;
            global.content_type().into()
        };
        let context = self.get_context()?;
        let value_type = pop_value(
            &mut context.value_stack,
            &context.frame_stack,
            StackValueType::Any,
        )?;
        if global_type != value_type {
            return Err(Error(format!(
                "Trying to update global {} of type {:?} with value of type {:?}",
                index, global_type, value_type
            )));
        }
        return Ok(());
    }

    fn validate_load(
        &mut self,
        align: u32,
        max_align: u32,
        value_type: ValueType,
    ) -> Result<(), Error> {
        if 1u32.checked_shl(align).unwrap_or(u32::MAX) > max_align {
            return Err(Error(format!(
                "Too large memory alignment 2^{} (expected at most {})",
                align, max_align
            )));
        }

        {
            let context = self.get_context()?;
            pop_value(
                &mut context.value_stack,
                &context.frame_stack,
                ValueType::I32.into(),
            )?;
        }
        self.get_module()?.require_memory(DEFAULT_MEMORY_INDEX)?;

        push_value(&mut self.get_context()?.value_stack, value_type.into())?;
        return Ok(());
    }

    fn validate_store(
        &mut self,
        align: u32,
        max_align: u32,
        value_type: ValueType,
    ) -> Result<(), Error> {
        if 1u32.checked_shl(align).unwrap_or(u32::MAX) > max_align {
            return Err(Error(format!(
                "Too large memory alignment 2^{} (expected at most {})",
                align, max_align
            )));
        }

        let module = self.get_module()?;
        module.require_memory(DEFAULT_MEMORY_INDEX)?;
        if let Some(context) = &mut self.context {
            pop_value(
                &mut context.value_stack,
                &context.frame_stack,
                value_type.into(),
            )?;
            pop_value(
                &mut context.value_stack,
                &context.frame_stack,
                ValueType::I32.into(),
            )?;
            return Ok(());
        }
        Err(Error("".into()))
    }
    fn validate_br(&mut self, depth: u32) -> Result<(), Error> {
        if let Some(context) = &mut self.context {
            let (frame_type, frame_block_type) = {
                let frame = require_label(depth, &context.frame_stack)?;
                (frame.frame_type, frame.block_type)
            };
            if !frame_type.is_loop() {
                if let BlockType::Value(value_type) = frame_block_type {
                    tee_value(
                        &mut context.value_stack,
                        &context.frame_stack,
                        value_type.into(),
                    )?;
                }
            }
            return Ok(());
        }
        Err(Error("".into()))
    }

    fn validate_br_if(&mut self, depth: u32) -> Result<(), Error> {
        if let Some(context) = &mut self.context {
            pop_value(
                &mut context.value_stack,
                &context.frame_stack,
                ValueType::I32.into(),
            )?;

            let (frame_type, frame_block_type) = {
                let frame = require_label(depth, &context.frame_stack)?;
                (frame.frame_type, frame.block_type)
            };
            if !frame_type.is_loop() {
                if let BlockType::Value(value_type) = frame_block_type {
                    tee_value(
                        &mut context.value_stack,
                        &context.frame_stack,
                        value_type.into(),
                    )?;
                }
            }
            return Ok(());
        }
        Err(Error("".into()))
    }

    fn validate_br_table(&mut self, table: &[u32], default: u32) -> Result<(), Error> {
        if let Some(context) = &mut self.context {
            let required_block_type: BlockType = {
                let default_block = require_label(default, &context.frame_stack)?;
                let required_block_type = if !default_block.frame_type.is_loop() {
                    default_block.block_type
                } else {
                    BlockType::NoResult
                };

                for label in table {
                    let label_block = require_label(*label, &context.frame_stack)?;
                    let label_block_type = if !label_block.frame_type.is_loop() {
                        label_block.block_type
                    } else {
                        BlockType::NoResult
                    };
                    if required_block_type != label_block_type {
                        return Err(Error(format!(
                            "Labels in br_table points to block of different types: {:?} and {:?}",
                            required_block_type, label_block.block_type
                        )));
                    }
                }
                required_block_type
            };

            pop_value(
                &mut context.value_stack,
                &context.frame_stack,
                ValueType::I32.into(),
            )?;
            if let BlockType::Value(value_type) = required_block_type {
                tee_value(
                    &mut context.value_stack,
                    &context.frame_stack,
                    value_type.into(),
                )?;
            }

            return Ok(());
        }
        Err(Error("".into()))
    }
    fn validate_call(&mut self, idx: u32) -> Result<(), Error> {
        let (argument_types, return_type) = self.get_module()?.require_function(idx)?;
        for argument_type in argument_types.iter().rev() {
            let context = self.get_context()?;
            pop_value(
                &mut context.value_stack,
                &context.frame_stack,
                (*argument_type).into(),
            )?;
        }
        if let BlockType::Value(value_type) = return_type {
            push_value(&mut self.get_context()?.value_stack, value_type.into())?;
        }
        return Ok(());
    }

    fn validate_call_indirect(&mut self, idx: u32) -> Result<(), Error> {
        {
            let table = self.get_module()?.require_table(DEFAULT_TABLE_INDEX)?;
            if table.elem_type() != TableElementType::AnyFunc {
                return Err(Error(format!(
                    "Table {} has element type {:?} while `anyfunc` expected",
                    idx,
                    table.elem_type()
                )));
            }
        }

        let context = self.get_context()?;
        pop_value(
            &mut context.value_stack,
            &context.frame_stack,
            ValueType::I32.into(),
        )?;
        let (argument_types, return_type) = self.get_module()?.require_function_type(idx)?;
        for argument_type in argument_types.iter().rev() {
            let context = self.get_context()?;
            pop_value(
                &mut context.value_stack,
                &context.frame_stack,
                (*argument_type).into(),
            )?;
        }
        if let BlockType::Value(value_type) = return_type {
            push_value(&mut self.get_context()?.value_stack, value_type.into())?;
        }
        return Ok(());
    }

    fn validate_current_memory(&mut self) -> Result<(), Error> {
        self.get_module()?.require_memory(DEFAULT_MEMORY_INDEX)?;
        push_value(&mut self.get_context()?.value_stack, ValueType::I32.into())?;
        return Ok(());
    }

    fn validate_grow_memory(&mut self) -> Result<(), Error> {
        self.get_module()?.require_memory(DEFAULT_MEMORY_INDEX)?;
        let context = self.get_context()?;
        pop_value(
            &mut context.value_stack,
            &context.frame_stack,
            ValueType::I32.into(),
        )?;
        push_value(&mut self.get_context()?.value_stack, ValueType::I32.into())?;
        return Ok(());
    }
}

fn make_top_frame_polymorphic(
    value_stack: &mut StackWithLimit<StackValueType>,
    frame_stack: &mut StackWithLimit<BlockFrame>,
) {
    let frame = frame_stack
        .top_mut()
        .expect("make_top_frame_polymorphic is called with empty frame stack");
    value_stack.resize(frame.value_stack_len, StackValueType::Any);
    frame.polymorphic_stack = true;
}

fn push_value(
    value_stack: &mut StackWithLimit<StackValueType>,
    value_type: StackValueType,
) -> Result<(), Error> {
    Ok(value_stack.push(value_type.into())?)
}

// TODO: Rename value_type -> expected_value_ty
fn pop_value(
    value_stack: &mut StackWithLimit<StackValueType>,
    frame_stack: &StackWithLimit<BlockFrame>,
    value_type: StackValueType,
) -> Result<StackValueType, Error> {
    let (is_stack_polymorphic, label_value_stack_len) = {
        let frame = top_label(frame_stack);
        (frame.polymorphic_stack, frame.value_stack_len)
    };
    let stack_is_empty = value_stack.len() == label_value_stack_len;
    let actual_value = if stack_is_empty && is_stack_polymorphic {
        StackValueType::Any
    } else {
        let value_stack_min = frame_stack
            .top()
            .expect("at least 1 topmost block")
            .value_stack_len;
        if value_stack.len() <= value_stack_min {
            return Err(Error("Trying to access parent frame stack values.".into()));
        }
        value_stack.pop()?
    };
    match actual_value {
        StackValueType::Specific(stack_value_type) if stack_value_type == value_type => {
            Ok(actual_value)
        }
        StackValueType::Any => Ok(actual_value),
        stack_value_type @ _ => Err(Error(format!(
            "Expected value of type {:?} on top of stack. Got {:?}",
            value_type, stack_value_type
        ))),
    }
}

fn tee_value(
    value_stack: &mut StackWithLimit<StackValueType>,
    frame_stack: &StackWithLimit<BlockFrame>,
    value_type: StackValueType,
) -> Result<(), Error> {
    let _ = pop_value(value_stack, frame_stack, value_type)?;
    push_value(value_stack, value_type)?;
    Ok(())
}

fn push_label(
    frame_type: BlockFrameType,
    block_type: BlockType,
    position: usize,
    value_stack: &StackWithLimit<StackValueType>,
    frame_stack: &mut StackWithLimit<BlockFrame>,
) -> Result<(), Error> {
    Ok(frame_stack.push(BlockFrame {
        frame_type: frame_type,
        block_type: block_type,
        begin_position: position,
        value_stack_len: value_stack.len(),
        polymorphic_stack: false,
    })?)
}

// TODO: Refactor
fn pop_label(
    value_stack: &mut StackWithLimit<StackValueType>,
    frame_stack: &mut StackWithLimit<BlockFrame>,
) -> Result<(), Error> {
    // Don't pop frame yet. This is essential since we still might pop values from the value stack
    // and this in turn requires current frame to check whether or not we've reached
    // unreachable.
    let block_type = frame_stack.top()?.block_type;
    match block_type {
        BlockType::NoResult => (),
        BlockType::Value(required_value_type) => {
            let _ = pop_value(
                value_stack,
                frame_stack,
                StackValueType::Specific(required_value_type),
            )?;
        }
    }

    let frame = frame_stack.pop()?;
    if value_stack.len() != frame.value_stack_len {
        return Err(Error(format!(
            "Unexpected stack height {}, expected {}",
            value_stack.len(),
            frame.value_stack_len
        )));
    }

    Ok(())
}

fn top_label(frame_stack: &StackWithLimit<BlockFrame>) -> &BlockFrame {
    frame_stack
        .top()
        .expect("this function can't be called with empty frame stack")
}

fn require_label(
    depth: u32,
    frame_stack: &StackWithLimit<BlockFrame>,
) -> Result<&BlockFrame, Error> {
    Ok(frame_stack.get(depth as usize)?)
}

fn require_local(locals: &Locals, idx: u32) -> Result<ValueType, Error> {
    Ok(locals.type_of_local(idx)?)
}
