use parity_wasm::builder::module;
use parity_wasm::elements::*;

use super::Validator;

#[test]
fn empty_valid() {
    let mut validator = Validator::new();
    let module = module().build();
    assert!(validator.validate_module(&module).is_ok());
}

#[test]
fn limits() {
    let test_cases = vec![
        // min > max
        (10, Some(9), false),
        // min = max
        (10, Some(10), true),
        // table/memory is always valid without max
        (10, None, true),
    ];

    for (min, max, is_valid) in test_cases {
        // defined table
        let m = module().table().with_min(min).with_max(max).build().build();
        let mut v = Validator::new();
        assert_eq!(v.validate_module(&m).is_ok(), is_valid);

        // imported table
        let m = module()
            .with_import(ImportEntry::new(
                "core".into(),
                "table".into(),
                External::Table(TableType::new(min, max)),
            ))
            .build();
        assert_eq!(v.validate_module(&m).is_ok(), is_valid);

        // defined memory
        let m = module()
            .memory()
            .with_min(min)
            .with_max(max)
            .build()
            .build();
        assert_eq!(v.validate_module(&m).is_ok(), is_valid);

        // imported table
        let m = module()
            .with_import(ImportEntry::new(
                "core".into(),
                "memory".into(),
                External::Memory(MemoryType::new(min, max)),
            ))
            .build();
        assert_eq!(v.validate_module(&m).is_ok(), is_valid);
    }
}

#[test]
fn global_init_const() {
    let mut v = Validator::new();
    let m = module()
        .with_global(GlobalEntry::new(
            GlobalType::new(ValueType::I32, true),
            InitExpr::new(vec![Instruction::I32Const(42), Instruction::End]),
        ))
        .build();
    assert!(v.validate_module(&m).is_ok());

    // init expr type differs from declared global type
    let m = module()
        .with_global(GlobalEntry::new(
            GlobalType::new(ValueType::I64, true),
            InitExpr::new(vec![Instruction::I32Const(42), Instruction::End]),
        ))
        .build();
    assert!(v.validate_module(&m).is_err());
}

#[test]
fn global_init_global() {
    let mut v = Validator::new();
    let m = module()
        .with_import(ImportEntry::new(
            "env".into(),
            "ext_global".into(),
            External::Global(GlobalType::new(ValueType::I32, false)),
        ))
        .with_global(GlobalEntry::new(
            GlobalType::new(ValueType::I32, true),
            InitExpr::new(vec![Instruction::GetGlobal(0), Instruction::End]),
        ))
        .build();
    assert!(v.validate_module(&m).is_ok());

    // get_global can reference only previously defined globals
    let m = module()
        .with_global(GlobalEntry::new(
            GlobalType::new(ValueType::I32, true),
            InitExpr::new(vec![Instruction::GetGlobal(0), Instruction::End]),
        ))
        .build();
    assert!(v.validate_module(&m).is_err());

    // get_global can reference only const globals
    let m = module()
        .with_import(ImportEntry::new(
            "env".into(),
            "ext_global".into(),
            External::Global(GlobalType::new(ValueType::I32, true)),
        ))
        .with_global(GlobalEntry::new(
            GlobalType::new(ValueType::I32, true),
            InitExpr::new(vec![Instruction::GetGlobal(0), Instruction::End]),
        ))
        .build();
    assert!(v.validate_module(&m).is_err());

    // get_global in init_expr can only refer to imported globals.
    let m = module()
        .with_global(GlobalEntry::new(
            GlobalType::new(ValueType::I32, false),
            InitExpr::new(vec![Instruction::I32Const(0), Instruction::End]),
        ))
        .with_global(GlobalEntry::new(
            GlobalType::new(ValueType::I32, true),
            InitExpr::new(vec![Instruction::GetGlobal(0), Instruction::End]),
        ))
        .build();
    assert!(v.validate_module(&m).is_err());
}

#[test]
fn global_init_misc() {
    let mut v = Validator::new();
    // without delimiting End opcode
    let m = module()
        .with_global(GlobalEntry::new(
            GlobalType::new(ValueType::I32, true),
            InitExpr::new(vec![Instruction::I32Const(42)]),
        ))
        .build();
    assert!(v.validate_module(&m).is_err());

    // empty init expr
    let m = module()
        .with_global(GlobalEntry::new(
            GlobalType::new(ValueType::I32, true),
            InitExpr::new(vec![Instruction::End]),
        ))
        .build();
    assert!(v.validate_module(&m).is_err());

    // not an constant opcode used
    let m = module()
        .with_global(GlobalEntry::new(
            GlobalType::new(ValueType::I32, true),
            InitExpr::new(vec![Instruction::Unreachable, Instruction::End]),
        ))
        .build();
    assert!(v.validate_module(&m).is_err());
}

#[test]
fn module_limits_validity() {
    let mut v = Validator::new();
    // module cannot contain more than 1 memory atm.
    let m = module()
        .with_import(ImportEntry::new(
            "core".into(),
            "memory".into(),
            External::Memory(MemoryType::new(10, None)),
        ))
        .memory()
        .with_min(10)
        .build()
        .build();
    assert!(v.validate_module(&m).is_err());

    // module cannot contain more than 1 table atm.
    let m = module()
        .with_import(ImportEntry::new(
            "core".into(),
            "table".into(),
            External::Table(TableType::new(10, None)),
        ))
        .table()
        .with_min(10)
        .build()
        .build();
    assert!(v.validate_module(&m).is_err());
}

#[test]
fn funcs() {
    let mut v = Validator::new();
    // recursive function calls is legal.
    let m = module()
        .function()
        .signature()
        .return_type()
        .i32()
        .build()
        .body()
        .with_instructions(Instructions::new(vec![
            Instruction::Call(1),
            Instruction::End,
        ]))
        .build()
        .build()
        .function()
        .signature()
        .return_type()
        .i32()
        .build()
        .body()
        .with_instructions(Instructions::new(vec![
            Instruction::Call(0),
            Instruction::End,
        ]))
        .build()
        .build()
        .build();
    assert!(v.validate_module(&m).is_ok());
}

#[test]
fn globals() {
    let mut v = Validator::new();
    // import immutable global is legal.
    let m = module()
        .with_import(ImportEntry::new(
            "env".into(),
            "ext_global".into(),
            External::Global(GlobalType::new(ValueType::I32, false)),
        ))
        .build();
    assert!(v.validate_module(&m).is_ok());

    // import mutable global is invalid.
    let m = module()
        .with_import(ImportEntry::new(
            "env".into(),
            "ext_global".into(),
            External::Global(GlobalType::new(ValueType::I32, true)),
        ))
        .build();
    assert!(v.validate_module(&m).is_err());
}

#[test]
fn if_else_with_return_type_validation() {
    let mut v = Validator::new();
    let m = module()
        .function()
        .signature()
        .build()
        .body()
        .with_instructions(Instructions::new(vec![
            Instruction::I32Const(1),
            Instruction::If(BlockType::NoResult),
            Instruction::I32Const(1),
            Instruction::If(BlockType::Value(ValueType::I32)),
            Instruction::I32Const(1),
            Instruction::Else,
            Instruction::I32Const(2),
            Instruction::End,
            Instruction::Drop,
            Instruction::End,
            Instruction::End,
        ]))
        .build()
        .build()
        .build();
    v.validate_module(&m).unwrap();
}
