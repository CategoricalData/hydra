-- Note: this is an automatically generated file. Do not edit.

-- | A WebAssembly text format (WAT) syntax model, based on the WebAssembly spec (https://webassembly.github.io/spec/core/), retrieved 2026-04-06

module Hydra.Wasm.Syntax where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Int as I

-- | A WebAssembly module, the top-level container
data Module =
  Module {
    -- | Optional module name
    moduleName :: (Maybe String),
    -- | The module's fields (types, functions, memories, tables, globals, imports, exports, etc.)
    moduleFields :: [ModuleField]}
  deriving (Eq, Ord, Read, Show)

_Module = Core.Name "hydra.wasm.syntax.Module"

_Module_name = Core.Name "name"

_Module_fields = Core.Name "fields"

-- | A field within a WebAssembly module
data ModuleField =
  -- | A type definition
  ModuleFieldType TypeDef |
  -- | A function definition
  ModuleFieldFunc Func |
  -- | A memory definition
  ModuleFieldMemory MemoryDef |
  -- | A table definition
  ModuleFieldTable TableDef |
  -- | A global variable definition
  ModuleFieldGlobal GlobalDef |
  -- | An import declaration
  ModuleFieldImport ImportDef |
  -- | An export declaration
  ModuleFieldExport ExportDef |
  -- | A data segment
  ModuleFieldData DataSegment |
  -- | An element segment
  ModuleFieldElem ElemSegment |
  -- | A start function index
  ModuleFieldStart String
  deriving (Eq, Ord, Read, Show)

_ModuleField = Core.Name "hydra.wasm.syntax.ModuleField"

_ModuleField_type = Core.Name "type"

_ModuleField_func = Core.Name "func"

_ModuleField_memory = Core.Name "memory"

_ModuleField_table = Core.Name "table"

_ModuleField_global = Core.Name "global"

_ModuleField_import = Core.Name "import"

_ModuleField_export = Core.Name "export"

_ModuleField_data = Core.Name "data"

_ModuleField_elem = Core.Name "elem"

_ModuleField_start = Core.Name "start"

-- | A WebAssembly value type
data ValType =
  -- | 32-bit integer
  ValTypeI32  |
  -- | 64-bit integer
  ValTypeI64  |
  -- | 32-bit float
  ValTypeF32  |
  -- | 64-bit float
  ValTypeF64  |
  -- | Function reference
  ValTypeFuncref  |
  -- | External reference
  ValTypeExternref
  deriving (Eq, Ord, Read, Show)

_ValType = Core.Name "hydra.wasm.syntax.ValType"

_ValType_i32 = Core.Name "i32"

_ValType_i64 = Core.Name "i64"

_ValType_f32 = Core.Name "f32"

_ValType_f64 = Core.Name "f64"

_ValType_funcref = Core.Name "funcref"

_ValType_externref = Core.Name "externref"

-- | A function type signature
data FuncType =
  FuncType {
    -- | Parameter types
    funcTypeParams :: [ValType],
    -- | Result types
    funcTypeResults :: [ValType]}
  deriving (Eq, Ord, Read, Show)

_FuncType = Core.Name "hydra.wasm.syntax.FuncType"

_FuncType_params = Core.Name "params"

_FuncType_results = Core.Name "results"

-- | A reference to a function type, either by index or inline
data FuncTypeRef =
  -- | Reference by type index
  FuncTypeRefIndex String |
  -- | Inline function type
  FuncTypeRefInline FuncType
  deriving (Eq, Ord, Read, Show)

_FuncTypeRef = Core.Name "hydra.wasm.syntax.FuncTypeRef"

_FuncTypeRef_index = Core.Name "index"

_FuncTypeRef_inline = Core.Name "inline"

-- | A type use clause, referencing a type definition
data TypeUse =
  TypeUse {
    -- | Optional type index reference
    typeUseIndex :: (Maybe String),
    -- | Explicit parameter declarations (may augment or override type index)
    typeUseParams :: [Param],
    -- | Explicit result types
    typeUseResults :: [ValType]}
  deriving (Eq, Ord, Read, Show)

_TypeUse = Core.Name "hydra.wasm.syntax.TypeUse"

_TypeUse_index = Core.Name "index"

_TypeUse_params = Core.Name "params"

_TypeUse_results = Core.Name "results"

-- | A named type definition in the type section
data TypeDef =
  TypeDef {
    -- | Optional type name
    typeDefName :: (Maybe String),
    -- | The function type
    typeDefType :: FuncType}
  deriving (Eq, Ord, Read, Show)

_TypeDef = Core.Name "hydra.wasm.syntax.TypeDef"

_TypeDef_name = Core.Name "name"

_TypeDef_type = Core.Name "type"

-- | A function definition
data Func =
  Func {
    -- | Optional function name
    funcName :: (Maybe String),
    -- | The function's type signature
    funcTypeUse :: TypeUse,
    -- | Local variable declarations
    funcLocals :: [FuncLocal],
    -- | The function body (a sequence of instructions)
    funcBody :: [Instruction]}
  deriving (Eq, Ord, Read, Show)

_Func = Core.Name "hydra.wasm.syntax.Func"

_Func_name = Core.Name "name"

_Func_typeUse = Core.Name "typeUse"

_Func_locals = Core.Name "locals"

_Func_body = Core.Name "body"

-- | A local variable declaration within a function
data FuncLocal =
  FuncLocal {
    -- | Optional local name
    funcLocalName :: (Maybe String),
    -- | The local's value type
    funcLocalType :: ValType}
  deriving (Eq, Ord, Read, Show)

_FuncLocal = Core.Name "hydra.wasm.syntax.FuncLocal"

_FuncLocal_name = Core.Name "name"

_FuncLocal_type = Core.Name "type"

-- | A function parameter declaration
data Param =
  Param {
    -- | Optional parameter name
    paramName :: (Maybe String),
    -- | The parameter's value type
    paramType :: ValType}
  deriving (Eq, Ord, Read, Show)

_Param = Core.Name "hydra.wasm.syntax.Param"

_Param_name = Core.Name "name"

_Param_type = Core.Name "type"

-- | Arguments for a branch table instruction
data BrTableArgs =
  BrTableArgs {
    -- | Branch target labels
    brTableArgsLabels :: [String],
    -- | Default branch target
    brTableArgsDefault :: String}
  deriving (Eq, Ord, Read, Show)

_BrTableArgs = Core.Name "hydra.wasm.syntax.BrTableArgs"

_BrTableArgs_labels = Core.Name "labels"

_BrTableArgs_default = Core.Name "default"

-- | A WebAssembly instruction
data Instruction =
  -- | Push a constant value
  InstructionConst ConstValue |
  -- | Get local variable by name or index
  InstructionLocalGet String |
  -- | Set local variable by name or index
  InstructionLocalSet String |
  -- | Set local variable and keep value on stack
  InstructionLocalTee String |
  -- | Get global variable by name or index
  InstructionGlobalGet String |
  -- | Set global variable by name or index
  InstructionGlobalSet String |
  -- | Load from memory (e.g., i32.load, f64.load)
  InstructionLoad MemoryInstruction |
  -- | Store to memory (e.g., i32.store, f64.store)
  InstructionStore MemoryInstruction |
  -- | Unary numeric operation (e.g., i32.clz, f64.neg)
  InstructionUnop NumericOp |
  -- | Binary numeric operation (e.g., i32.add, f64.mul)
  InstructionBinop NumericOp |
  -- | Test operation (e.g., i32.eqz)
  InstructionTestop NumericOp |
  -- | Comparison operation (e.g., i32.eq, f64.lt)
  InstructionRelop NumericOp |
  -- | Type conversion (e.g., i32.wrap_i64, f64.convert_i32_s)
  InstructionConvert String |
  -- | Call a function by name or index
  InstructionCall String |
  -- | Indirect function call through a table
  InstructionCallIndirect TypeUse |
  -- | A block instruction
  InstructionBlock BlockInstruction |
  -- | A loop instruction
  InstructionLoop BlockInstruction |
  -- | An if-then-else instruction
  InstructionIf IfInstruction |
  -- | Branch to a label
  InstructionBr String |
  -- | Conditional branch to a label
  InstructionBrIf String |
  -- | Branch table
  InstructionBrTable BrTableArgs |
  -- | Return from the current function
  InstructionReturn  |
  -- | Drop the top value from the stack
  InstructionDrop  |
  -- | Select one of two values based on a condition
  InstructionSelect  |
  -- | Trap immediately
  InstructionUnreachable  |
  -- | No operation
  InstructionNop  |
  -- | Get current memory size in pages
  InstructionMemorySize  |
  -- | Grow memory by a number of pages
  InstructionMemoryGrow  |
  -- | Null reference of the given type
  InstructionRefNull RefType |
  -- | Test if a reference is null
  InstructionRefIsNull  |
  -- | A raw WAT instruction string (escape hatch)
  InstructionRaw String
  deriving (Eq, Ord, Read, Show)

_Instruction = Core.Name "hydra.wasm.syntax.Instruction"

_Instruction_const = Core.Name "const"

_Instruction_localGet = Core.Name "localGet"

_Instruction_localSet = Core.Name "localSet"

_Instruction_localTee = Core.Name "localTee"

_Instruction_globalGet = Core.Name "globalGet"

_Instruction_globalSet = Core.Name "globalSet"

_Instruction_load = Core.Name "load"

_Instruction_store = Core.Name "store"

_Instruction_unop = Core.Name "unop"

_Instruction_binop = Core.Name "binop"

_Instruction_testop = Core.Name "testop"

_Instruction_relop = Core.Name "relop"

_Instruction_convert = Core.Name "convert"

_Instruction_call = Core.Name "call"

_Instruction_callIndirect = Core.Name "callIndirect"

_Instruction_block = Core.Name "block"

_Instruction_loop = Core.Name "loop"

_Instruction_if = Core.Name "if"

_Instruction_br = Core.Name "br"

_Instruction_brIf = Core.Name "brIf"

_Instruction_brTable = Core.Name "brTable"

_Instruction_return = Core.Name "return"

_Instruction_drop = Core.Name "drop"

_Instruction_select = Core.Name "select"

_Instruction_unreachable = Core.Name "unreachable"

_Instruction_nop = Core.Name "nop"

_Instruction_memorySize = Core.Name "memorySize"

_Instruction_memoryGrow = Core.Name "memoryGrow"

_Instruction_refNull = Core.Name "refNull"

_Instruction_refIsNull = Core.Name "refIsNull"

_Instruction_raw = Core.Name "raw"

-- | A block or loop instruction with a label and body
data BlockInstruction =
  BlockInstruction {
    -- | Optional block label
    blockInstructionLabel :: (Maybe String),
    -- | The block's result type
    blockInstructionBlockType :: BlockType,
    -- | The instructions in the block
    blockInstructionBody :: [Instruction]}
  deriving (Eq, Ord, Read, Show)

_BlockInstruction = Core.Name "hydra.wasm.syntax.BlockInstruction"

_BlockInstruction_label = Core.Name "label"

_BlockInstruction_blockType = Core.Name "blockType"

_BlockInstruction_body = Core.Name "body"

-- | The type of a block, loop, or if instruction
data BlockType =
  -- | No result
  BlockTypeEmpty  |
  -- | Single result value type
  BlockTypeValue ValType |
  -- | Full function type signature
  BlockTypeTypeUse TypeUse
  deriving (Eq, Ord, Read, Show)

_BlockType = Core.Name "hydra.wasm.syntax.BlockType"

_BlockType_empty = Core.Name "empty"

_BlockType_value = Core.Name "value"

_BlockType_typeUse = Core.Name "typeUse"

-- | Memory access arguments
data MemArg =
  MemArg {
    -- | Memory offset in bytes
    memArgOffset :: Int,
    -- | Alignment hint (power of 2)
    memArgAlign :: Int}
  deriving (Eq, Ord, Read, Show)

_MemArg = Core.Name "hydra.wasm.syntax.MemArg"

_MemArg_offset = Core.Name "offset"

_MemArg_align = Core.Name "align"

-- | A memory load or store instruction
data MemoryInstruction =
  MemoryInstruction {
    -- | Value type to load or store
    memoryInstructionType :: ValType,
    -- | Memory argument (offset and alignment)
    memoryInstructionMemArg :: MemArg}
  deriving (Eq, Ord, Read, Show)

_MemoryInstruction = Core.Name "hydra.wasm.syntax.MemoryInstruction"

_MemoryInstruction_type = Core.Name "type"

_MemoryInstruction_memArg = Core.Name "memArg"

-- | An if-then-else instruction
data IfInstruction =
  IfInstruction {
    -- | Optional label for the if block
    ifInstructionLabel :: (Maybe String),
    -- | The result type of the if
    ifInstructionBlockType :: BlockType,
    -- | The 'then' branch instructions
    ifInstructionThen :: [Instruction],
    -- | The 'else' branch instructions (may be empty)
    ifInstructionElse :: [Instruction]}
  deriving (Eq, Ord, Read, Show)

_IfInstruction = Core.Name "hydra.wasm.syntax.IfInstruction"

_IfInstruction_label = Core.Name "label"

_IfInstruction_blockType = Core.Name "blockType"

_IfInstruction_then = Core.Name "then"

_IfInstruction_else = Core.Name "else"

-- | A typed numeric operation (used for unop, binop, testop, relop)
data NumericOp =
  NumericOp {
    -- | Operand value type
    numericOpType :: ValType,
    -- | Operation name (e.g., add, sub, mul, eqz, lt_s)
    numericOpName :: String}
  deriving (Eq, Ord, Read, Show)

_NumericOp = Core.Name "hydra.wasm.syntax.NumericOp"

_NumericOp_type = Core.Name "type"

_NumericOp_name = Core.Name "name"

-- | A memory definition
data MemoryDef =
  MemoryDef {
    -- | Optional memory name
    memoryDefName :: (Maybe String),
    -- | Memory size limits (in pages of 64KB)
    memoryDefLimits :: Limits}
  deriving (Eq, Ord, Read, Show)

_MemoryDef = Core.Name "hydra.wasm.syntax.MemoryDef"

_MemoryDef_name = Core.Name "name"

_MemoryDef_limits = Core.Name "limits"

-- | A data segment for initializing memory
data DataSegment =
  DataSegment {
    -- | Optional data segment name
    dataSegmentName :: (Maybe String),
    -- | Whether this is an active or passive data segment
    dataSegmentMode :: DataMode,
    -- | The data bytes (as a string of escaped bytes)
    dataSegmentBytes :: String}
  deriving (Eq, Ord, Read, Show)

_DataSegment = Core.Name "hydra.wasm.syntax.DataSegment"

_DataSegment_name = Core.Name "name"

_DataSegment_mode = Core.Name "mode"

_DataSegment_bytes = Core.Name "bytes"

-- | The mode of a data segment
data DataMode =
  -- | Active segment: loaded at the given offset expression
  DataModeActive [Instruction] |
  -- | Passive segment: must be explicitly loaded via memory.init
  DataModePassive
  deriving (Eq, Ord, Read, Show)

_DataMode = Core.Name "hydra.wasm.syntax.DataMode"

_DataMode_active = Core.Name "active"

_DataMode_passive = Core.Name "passive"

-- | Size limits for memories and tables
data Limits =
  Limits {
    -- | Minimum size
    limitsMin :: Int,
    -- | Optional maximum size
    limitsMax :: (Maybe Int)}
  deriving (Eq, Ord, Read, Show)

_Limits = Core.Name "hydra.wasm.syntax.Limits"

_Limits_min = Core.Name "min"

_Limits_max = Core.Name "max"

-- | A global variable definition
data GlobalDef =
  GlobalDef {
    -- | Optional global name
    globalDefName :: (Maybe String),
    -- | The global's type (value type and mutability)
    globalDefType :: GlobalType,
    -- | Initialization expression
    globalDefInit :: [Instruction]}
  deriving (Eq, Ord, Read, Show)

_GlobalDef = Core.Name "hydra.wasm.syntax.GlobalDef"

_GlobalDef_name = Core.Name "name"

_GlobalDef_type = Core.Name "type"

_GlobalDef_init = Core.Name "init"

-- | A global variable type
data GlobalType =
  GlobalType {
    -- | The value type
    globalTypeValType :: ValType,
    -- | Whether the global is mutable
    globalTypeMutable :: Bool}
  deriving (Eq, Ord, Read, Show)

_GlobalType = Core.Name "hydra.wasm.syntax.GlobalType"

_GlobalType_valType = Core.Name "valType"

_GlobalType_mutable = Core.Name "mutable"

-- | A table definition
data TableDef =
  TableDef {
    -- | Optional table name
    tableDefName :: (Maybe String),
    -- | The reference type of table elements
    tableDefRefType :: RefType,
    -- | Table size limits
    tableDefLimits :: Limits}
  deriving (Eq, Ord, Read, Show)

_TableDef = Core.Name "hydra.wasm.syntax.TableDef"

_TableDef_name = Core.Name "name"

_TableDef_refType = Core.Name "refType"

_TableDef_limits = Core.Name "limits"

-- | Active element segment parameters
data ElemActive =
  ElemActive {
    -- | Table index
    elemActiveTable :: String,
    -- | Offset expression
    elemActiveOffset :: [Instruction]}
  deriving (Eq, Ord, Read, Show)

_ElemActive = Core.Name "hydra.wasm.syntax.ElemActive"

_ElemActive_table = Core.Name "table"

_ElemActive_offset = Core.Name "offset"

-- | An element segment for initializing tables
data ElemSegment =
  ElemSegment {
    -- | Optional element segment name
    elemSegmentName :: (Maybe String),
    -- | Whether this is an active or passive element segment
    elemSegmentMode :: ElemMode,
    -- | The reference type of elements
    elemSegmentType :: RefType,
    -- | Initialization expressions (one per element)
    elemSegmentInit :: [[Instruction]]}
  deriving (Eq, Ord, Read, Show)

_ElemSegment = Core.Name "hydra.wasm.syntax.ElemSegment"

_ElemSegment_name = Core.Name "name"

_ElemSegment_mode = Core.Name "mode"

_ElemSegment_type = Core.Name "type"

_ElemSegment_init = Core.Name "init"

-- | The mode of an element segment
data ElemMode =
  -- | Active segment: loaded at the given table index and offset
  ElemModeActive ElemActive |
  -- | Passive segment: must be explicitly loaded via table.init
  ElemModePassive  |
  -- | Declarative segment: declares function references for ref.func
  ElemModeDeclarative
  deriving (Eq, Ord, Read, Show)

_ElemMode = Core.Name "hydra.wasm.syntax.ElemMode"

_ElemMode_active = Core.Name "active"

_ElemMode_passive = Core.Name "passive"

_ElemMode_declarative = Core.Name "declarative"

-- | A reference type for tables and reference instructions
data RefType =
  -- | Function reference
  RefTypeFuncref  |
  -- | External reference
  RefTypeExternref
  deriving (Eq, Ord, Read, Show)

_RefType = Core.Name "hydra.wasm.syntax.RefType"

_RefType_funcref = Core.Name "funcref"

_RefType_externref = Core.Name "externref"

-- | An import declaration
data ImportDef =
  ImportDef {
    -- | The module name to import from
    importDefModule :: String,
    -- | The name of the imported entity
    importDefName :: String,
    -- | The import descriptor (what kind of entity is imported)
    importDefDesc :: ImportDesc}
  deriving (Eq, Ord, Read, Show)

_ImportDef = Core.Name "hydra.wasm.syntax.ImportDef"

_ImportDef_module = Core.Name "module"

_ImportDef_name = Core.Name "name"

_ImportDef_desc = Core.Name "desc"

-- | An import descriptor specifying the kind of imported entity
data ImportDesc =
  -- | Import a function with the given type
  ImportDescFunc ImportFunc |
  -- | Import a memory with the given limits
  ImportDescMemory ImportMemory |
  -- | Import a table
  ImportDescTable ImportTable |
  -- | Import a global variable
  ImportDescGlobal ImportGlobal
  deriving (Eq, Ord, Read, Show)

_ImportDesc = Core.Name "hydra.wasm.syntax.ImportDesc"

_ImportDesc_func = Core.Name "func"

_ImportDesc_memory = Core.Name "memory"

_ImportDesc_table = Core.Name "table"

_ImportDesc_global = Core.Name "global"

-- | A function import descriptor
data ImportFunc =
  ImportFunc {
    -- | Optional local name for the imported function
    importFuncName :: (Maybe String),
    -- | The function's type
    importFuncTypeUse :: TypeUse}
  deriving (Eq, Ord, Read, Show)

_ImportFunc = Core.Name "hydra.wasm.syntax.ImportFunc"

_ImportFunc_name = Core.Name "name"

_ImportFunc_typeUse = Core.Name "typeUse"

-- | A global import descriptor
data ImportGlobal =
  ImportGlobal {
    -- | Optional local name
    importGlobalName :: (Maybe String),
    -- | Global type
    importGlobalType :: GlobalType}
  deriving (Eq, Ord, Read, Show)

_ImportGlobal = Core.Name "hydra.wasm.syntax.ImportGlobal"

_ImportGlobal_name = Core.Name "name"

_ImportGlobal_type = Core.Name "type"

-- | A memory import descriptor
data ImportMemory =
  ImportMemory {
    -- | Optional local name
    importMemoryName :: (Maybe String),
    -- | Memory limits
    importMemoryLimits :: Limits}
  deriving (Eq, Ord, Read, Show)

_ImportMemory = Core.Name "hydra.wasm.syntax.ImportMemory"

_ImportMemory_name = Core.Name "name"

_ImportMemory_limits = Core.Name "limits"

-- | A table import descriptor
data ImportTable =
  ImportTable {
    -- | Optional local name
    importTableName :: (Maybe String),
    -- | Reference type
    importTableRefType :: RefType,
    -- | Table limits
    importTableLimits :: Limits}
  deriving (Eq, Ord, Read, Show)

_ImportTable = Core.Name "hydra.wasm.syntax.ImportTable"

_ImportTable_name = Core.Name "name"

_ImportTable_refType = Core.Name "refType"

_ImportTable_limits = Core.Name "limits"

-- | An export declaration
data ExportDef =
  ExportDef {
    -- | The exported name
    exportDefName :: String,
    -- | What is being exported
    exportDefDesc :: ExportDesc}
  deriving (Eq, Ord, Read, Show)

_ExportDef = Core.Name "hydra.wasm.syntax.ExportDef"

_ExportDef_name = Core.Name "name"

_ExportDef_desc = Core.Name "desc"

-- | An export descriptor identifying what is being exported
data ExportDesc =
  -- | Export a function by name or index
  ExportDescFunc String |
  -- | Export a memory by name or index
  ExportDescMemory String |
  -- | Export a table by name or index
  ExportDescTable String |
  -- | Export a global by name or index
  ExportDescGlobal String
  deriving (Eq, Ord, Read, Show)

_ExportDesc = Core.Name "hydra.wasm.syntax.ExportDesc"

_ExportDesc_func = Core.Name "func"

_ExportDesc_memory = Core.Name "memory"

_ExportDesc_table = Core.Name "table"

_ExportDesc_global = Core.Name "global"

-- | A constant value instruction
data ConstValue =
  -- | A 32-bit integer constant
  ConstValueI32 Int |
  -- | A 64-bit integer constant
  ConstValueI64 I.Int64 |
  -- | A 32-bit float constant
  ConstValueF32 Float |
  -- | A 64-bit float constant
  ConstValueF64 Double
  deriving (Eq, Ord, Read, Show)

_ConstValue = Core.Name "hydra.wasm.syntax.ConstValue"

_ConstValue_i32 = Core.Name "i32"

_ConstValue_i64 = Core.Name "i64"

_ConstValue_f32 = Core.Name "f32"

_ConstValue_f64 = Core.Name "f64"
