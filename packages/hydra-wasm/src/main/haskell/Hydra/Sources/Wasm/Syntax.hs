-- | A WebAssembly text format (WAT) syntax model.
-- | See https://webassembly.github.io/spec/core/ (retrieved 2026-04-06)

module Hydra.Sources.Wasm.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


ns :: Namespace
ns = Namespace "hydra.wasm.syntax"

define :: String -> Type -> Binding
define = datatype ns

wasm :: String -> Type
wasm = typeref ns

module_ :: Module
module_ = Module ns (map toTypeDef definitions) [Core.ns] [Core.ns] $
    Just ("A WebAssembly text format (WAT) syntax model, based on the WebAssembly spec"
      ++ " (https://webassembly.github.io/spec/core/), retrieved 2026-04-06")
  where
    definitions = moduleLevel ++ types ++ functions ++ instructions ++ memory
      ++ globals ++ tables ++ imports ++ exports ++ literals

    -- Module-level constructs
    moduleLevel = [
      wasmModule,
      moduleField]

    -- Type system
    types = [
      valType,
      funcType,
      funcTypeRef,
      typeUse,
      typeDef]

    -- Function definitions
    functions = [
      func,
      funcLocal,
      param]

    -- Instructions
    instructions = [
      brTableArgs,
      instruction,
      blockInstruction,
      blockType,
      memArg,
      memoryInstruction,
      ifInstruction,
      numericOp]

    -- Memory
    memory = [
      memoryDef,
      dataSegment,
      dataMode,
      limits]

    -- Globals
    globals = [
      globalDef,
      globalType]

    -- Tables
    tables = [
      tableDef,
      elemActive,
      elemSegment,
      elemMode,
      refType]

    -- Imports and exports
    imports = [
      importDef,
      importDesc,
      importFunc,
      importGlobal,
      importMemory,
      importTable]

    exports = [
      exportDef,
      exportDesc]

    -- Literal values
    literals = [
      constValue]


-- ================================================================================================
-- Module-level constructs
-- ================================================================================================

wasmModule :: Binding
wasmModule = define "Module" $
  doc "A WebAssembly module, the top-level container" $
  T.record [
    "name">:
      doc "Optional module name" $
      T.maybe T.string,
    "fields">:
      doc "The module's fields (types, functions, memories, tables, globals, imports, exports, etc.)" $
      T.list moduleField]

moduleField :: Binding
moduleField = define "ModuleField" $
  doc "A field within a WebAssembly module" $
  T.union [
    "type">:
      doc "A type definition" $
      wasm "TypeDef",
    "func">:
      doc "A function definition" $
      wasm "Func",
    "memory">:
      doc "A memory definition" $
      wasm "MemoryDef",
    "table">:
      doc "A table definition" $
      wasm "TableDef",
    "global">:
      doc "A global variable definition" $
      wasm "GlobalDef",
    "import">:
      doc "An import declaration" $
      wasm "ImportDef",
    "export">:
      doc "An export declaration" $
      wasm "ExportDef",
    "data">:
      doc "A data segment" $
      wasm "DataSegment",
    "elem">:
      doc "An element segment" $
      wasm "ElemSegment",
    "start">:
      doc "A start function index" $
      T.string]


-- ================================================================================================
-- Type system
-- ================================================================================================

valType :: Binding
valType = define "ValType" $
  doc "A WebAssembly value type" $
  T.union [
    "i32">:
      doc "32-bit integer" $
      T.unit,
    "i64">:
      doc "64-bit integer" $
      T.unit,
    "f32">:
      doc "32-bit float" $
      T.unit,
    "f64">:
      doc "64-bit float" $
      T.unit,
    "funcref">:
      doc "Function reference" $
      T.unit,
    "externref">:
      doc "External reference" $
      T.unit]

funcType :: Binding
funcType = define "FuncType" $
  doc "A function type signature" $
  T.record [
    "params">:
      doc "Parameter types" $
      T.list (wasm "ValType"),
    "results">:
      doc "Result types" $
      T.list (wasm "ValType")]

funcTypeRef :: Binding
funcTypeRef = define "FuncTypeRef" $
  doc "A reference to a function type, either by index or inline" $
  T.union [
    "index">:
      doc "Reference by type index" $
      T.string,
    "inline">:
      doc "Inline function type" $
      wasm "FuncType"]

typeUse :: Binding
typeUse = define "TypeUse" $
  doc "A type use clause, referencing a type definition" $
  T.record [
    "index">:
      doc "Optional type index reference" $
      T.maybe T.string,
    "params">:
      doc "Explicit parameter declarations (may augment or override type index)" $
      T.list (wasm "Param"),
    "results">:
      doc "Explicit result types" $
      T.list (wasm "ValType")]

typeDef :: Binding
typeDef = define "TypeDef" $
  doc "A named type definition in the type section" $
  T.record [
    "name">:
      doc "Optional type name" $
      T.maybe T.string,
    "type">:
      doc "The function type" $
      wasm "FuncType"]


-- ================================================================================================
-- Function definitions
-- ================================================================================================

func :: Binding
func = define "Func" $
  doc "A function definition" $
  T.record [
    "name">:
      doc "Optional function name" $
      T.maybe T.string,
    "typeUse">:
      doc "The function's type signature" $
      wasm "TypeUse",
    "locals">:
      doc "Local variable declarations" $
      T.list (wasm "FuncLocal"),
    "body">:
      doc "The function body (a sequence of instructions)" $
      T.list (wasm "Instruction")]

funcLocal :: Binding
funcLocal = define "FuncLocal" $
  doc "A local variable declaration within a function" $
  T.record [
    "name">:
      doc "Optional local name" $
      T.maybe T.string,
    "type">:
      doc "The local's value type" $
      wasm "ValType"]

param :: Binding
param = define "Param" $
  doc "A function parameter declaration" $
  T.record [
    "name">:
      doc "Optional parameter name" $
      T.maybe T.string,
    "type">:
      doc "The parameter's value type" $
      wasm "ValType"]


-- ================================================================================================
-- Instructions
-- ================================================================================================

instruction :: Binding
instruction = define "Instruction" $
  doc "A WebAssembly instruction" $
  T.union [
    "const">:
      doc "Push a constant value" $
      wasm "ConstValue",
    "localGet">:
      doc "Get local variable by name or index" $
      T.string,
    "localSet">:
      doc "Set local variable by name or index" $
      T.string,
    "localTee">:
      doc "Set local variable and keep value on stack" $
      T.string,
    "globalGet">:
      doc "Get global variable by name or index" $
      T.string,
    "globalSet">:
      doc "Set global variable by name or index" $
      T.string,
    "load">:
      doc "Load from memory (e.g., i32.load, f64.load)" $
      wasm "MemoryInstruction",
    "store">:
      doc "Store to memory (e.g., i32.store, f64.store)" $
      wasm "MemoryInstruction",
    "unop">:
      doc "Unary numeric operation (e.g., i32.clz, f64.neg)" $
      wasm "NumericOp",
    "binop">:
      doc "Binary numeric operation (e.g., i32.add, f64.mul)" $
      wasm "NumericOp",
    "testop">:
      doc "Test operation (e.g., i32.eqz)" $
      wasm "NumericOp",
    "relop">:
      doc "Comparison operation (e.g., i32.eq, f64.lt)" $
      wasm "NumericOp",
    "convert">:
      doc "Type conversion (e.g., i32.wrap_i64, f64.convert_i32_s)" $
      T.string,
    "call">:
      doc "Call a function by name or index" $
      T.string,
    "callIndirect">:
      doc "Indirect function call through a table" $
      wasm "TypeUse",
    "block">:
      doc "A block instruction" $
      wasm "BlockInstruction",
    "loop">:
      doc "A loop instruction" $
      wasm "BlockInstruction",
    "if">:
      doc "An if-then-else instruction" $
      wasm "IfInstruction",
    "br">:
      doc "Branch to a label" $
      T.string,
    "brIf">:
      doc "Conditional branch to a label" $
      T.string,
    "brTable">:
      doc "Branch table" $
      wasm "BrTableArgs",
    "return">:
      doc "Return from the current function" $
      T.unit,
    "drop">:
      doc "Drop the top value from the stack" $
      T.unit,
    "select">:
      doc "Select one of two values based on a condition" $
      T.unit,
    "unreachable">:
      doc "Trap immediately" $
      T.unit,
    "nop">:
      doc "No operation" $
      T.unit,
    "memorySize">:
      doc "Get current memory size in pages" $
      T.unit,
    "memoryGrow">:
      doc "Grow memory by a number of pages" $
      T.unit,
    "refNull">:
      doc "Null reference of the given type" $
      wasm "RefType",
    "refIsNull">:
      doc "Test if a reference is null" $
      T.unit,
    "raw">:
      doc "A raw WAT instruction string (escape hatch)" $
      T.string]

brTableArgs :: Binding
brTableArgs = define "BrTableArgs" $
  doc "Arguments for a branch table instruction" $
  T.record [
    "labels">:
      doc "Branch target labels" $
      T.list T.string,
    "default">:
      doc "Default branch target" $
      T.string]

blockInstruction :: Binding
blockInstruction = define "BlockInstruction" $
  doc "A block or loop instruction with a label and body" $
  T.record [
    "label">:
      doc "Optional block label" $
      T.maybe T.string,
    "blockType">:
      doc "The block's result type" $
      wasm "BlockType",
    "body">:
      doc "The instructions in the block" $
      T.list (wasm "Instruction")]

blockType :: Binding
blockType = define "BlockType" $
  doc "The type of a block, loop, or if instruction" $
  T.union [
    "empty">:
      doc "No result" $
      T.unit,
    "value">:
      doc "Single result value type" $
      wasm "ValType",
    "typeUse">:
      doc "Full function type signature" $
      wasm "TypeUse"]

memoryInstruction :: Binding
memoryInstruction = define "MemoryInstruction" $
  doc "A memory load or store instruction" $
  T.record [
    "type">:
      doc "Value type to load or store" $
      wasm "ValType",
    "memArg">:
      doc "Memory argument (offset and alignment)" $
      wasm "MemArg"]

numericOp :: Binding
numericOp = define "NumericOp" $
  doc "A typed numeric operation (used for unop, binop, testop, relop)" $
  T.record [
    "type">:
      doc "Operand value type" $
      wasm "ValType",
    "name">:
      doc "Operation name (e.g., add, sub, mul, eqz, lt_s)" $
      T.string]

memArg :: Binding
memArg = define "MemArg" $
  doc "Memory access arguments" $
  T.record [
    "offset">:
      doc "Memory offset in bytes" $
      T.int32,
    "align">:
      doc "Alignment hint (power of 2)" $
      T.int32]

ifInstruction :: Binding
ifInstruction = define "IfInstruction" $
  doc "An if-then-else instruction" $
  T.record [
    "label">:
      doc "Optional label for the if block" $
      T.maybe T.string,
    "blockType">:
      doc "The result type of the if" $
      wasm "BlockType",
    "then">:
      doc "The 'then' branch instructions" $
      T.list (wasm "Instruction"),
    "else">:
      doc "The 'else' branch instructions (may be empty)" $
      T.list (wasm "Instruction")]


-- ================================================================================================
-- Memory
-- ================================================================================================

memoryDef :: Binding
memoryDef = define "MemoryDef" $
  doc "A memory definition" $
  T.record [
    "name">:
      doc "Optional memory name" $
      T.maybe T.string,
    "limits">:
      doc "Memory size limits (in pages of 64KB)" $
      wasm "Limits"]

dataSegment :: Binding
dataSegment = define "DataSegment" $
  doc "A data segment for initializing memory" $
  T.record [
    "name">:
      doc "Optional data segment name" $
      T.maybe T.string,
    "mode">:
      doc "Whether this is an active or passive data segment" $
      wasm "DataMode",
    "bytes">:
      doc "The data bytes (as a string of escaped bytes)" $
      T.string]

dataMode :: Binding
dataMode = define "DataMode" $
  doc "The mode of a data segment" $
  T.union [
    "active">:
      doc "Active segment: loaded at the given offset expression" $
      T.list (wasm "Instruction"),
    "passive">:
      doc "Passive segment: must be explicitly loaded via memory.init" $
      T.unit]

limits :: Binding
limits = define "Limits" $
  doc "Size limits for memories and tables" $
  T.record [
    "min">:
      doc "Minimum size" $
      T.int32,
    "max">:
      doc "Optional maximum size" $
      T.maybe T.int32]


-- ================================================================================================
-- Globals
-- ================================================================================================

globalDef :: Binding
globalDef = define "GlobalDef" $
  doc "A global variable definition" $
  T.record [
    "name">:
      doc "Optional global name" $
      T.maybe T.string,
    "type">:
      doc "The global's type (value type and mutability)" $
      wasm "GlobalType",
    "init">:
      doc "Initialization expression" $
      T.list (wasm "Instruction")]

globalType :: Binding
globalType = define "GlobalType" $
  doc "A global variable type" $
  T.record [
    "valType">:
      doc "The value type" $
      wasm "ValType",
    "mutable">:
      doc "Whether the global is mutable" $
      T.boolean]


-- ================================================================================================
-- Tables
-- ================================================================================================

tableDef :: Binding
tableDef = define "TableDef" $
  doc "A table definition" $
  T.record [
    "name">:
      doc "Optional table name" $
      T.maybe T.string,
    "refType">:
      doc "The reference type of table elements" $
      wasm "RefType",
    "limits">:
      doc "Table size limits" $
      wasm "Limits"]

elemSegment :: Binding
elemSegment = define "ElemSegment" $
  doc "An element segment for initializing tables" $
  T.record [
    "name">:
      doc "Optional element segment name" $
      T.maybe T.string,
    "mode">:
      doc "Whether this is an active or passive element segment" $
      wasm "ElemMode",
    "type">:
      doc "The reference type of elements" $
      wasm "RefType",
    "init">:
      doc "Initialization expressions (one per element)" $
      T.list (T.list (wasm "Instruction"))]

elemActive :: Binding
elemActive = define "ElemActive" $
  doc "Active element segment parameters" $
  T.record [
    "table">:
      doc "Table index" $
      T.string,
    "offset">:
      doc "Offset expression" $
      T.list (wasm "Instruction")]

elemMode :: Binding
elemMode = define "ElemMode" $
  doc "The mode of an element segment" $
  T.union [
    "active">:
      doc "Active segment: loaded at the given table index and offset" $
      wasm "ElemActive",
    "passive">:
      doc "Passive segment: must be explicitly loaded via table.init" $
      T.unit,
    "declarative">:
      doc "Declarative segment: declares function references for ref.func" $
      T.unit]

refType :: Binding
refType = define "RefType" $
  doc "A reference type for tables and reference instructions" $
  T.union [
    "funcref">:
      doc "Function reference" $
      T.unit,
    "externref">:
      doc "External reference" $
      T.unit]


-- ================================================================================================
-- Imports and exports
-- ================================================================================================

importDef :: Binding
importDef = define "ImportDef" $
  doc "An import declaration" $
  T.record [
    "module">:
      doc "The module name to import from" $
      T.string,
    "name">:
      doc "The name of the imported entity" $
      T.string,
    "desc">:
      doc "The import descriptor (what kind of entity is imported)" $
      wasm "ImportDesc"]

importDesc :: Binding
importDesc = define "ImportDesc" $
  doc "An import descriptor specifying the kind of imported entity" $
  T.union [
    "func">:
      doc "Import a function with the given type" $
      wasm "ImportFunc",
    "memory">:
      doc "Import a memory with the given limits" $
      wasm "ImportMemory",
    "table">:
      doc "Import a table" $
      wasm "ImportTable",
    "global">:
      doc "Import a global variable" $
      wasm "ImportGlobal"]

importFunc :: Binding
importFunc = define "ImportFunc" $
  doc "A function import descriptor" $
  T.record [
    "name">:
      doc "Optional local name for the imported function" $
      T.maybe T.string,
    "typeUse">:
      doc "The function's type" $
      wasm "TypeUse"]

importGlobal :: Binding
importGlobal = define "ImportGlobal" $
  doc "A global import descriptor" $
  T.record [
    "name">:
      doc "Optional local name" $
      T.maybe T.string,
    "type">:
      doc "Global type" $
      wasm "GlobalType"]

importMemory :: Binding
importMemory = define "ImportMemory" $
  doc "A memory import descriptor" $
  T.record [
    "name">:
      doc "Optional local name" $
      T.maybe T.string,
    "limits">:
      doc "Memory limits" $
      wasm "Limits"]

importTable :: Binding
importTable = define "ImportTable" $
  doc "A table import descriptor" $
  T.record [
    "name">:
      doc "Optional local name" $
      T.maybe T.string,
    "refType">:
      doc "Reference type" $
      wasm "RefType",
    "limits">:
      doc "Table limits" $
      wasm "Limits"]

exportDef :: Binding
exportDef = define "ExportDef" $
  doc "An export declaration" $
  T.record [
    "name">:
      doc "The exported name" $
      T.string,
    "desc">:
      doc "What is being exported" $
      wasm "ExportDesc"]

exportDesc :: Binding
exportDesc = define "ExportDesc" $
  doc "An export descriptor identifying what is being exported" $
  T.union [
    "func">:
      doc "Export a function by name or index" $
      T.string,
    "memory">:
      doc "Export a memory by name or index" $
      T.string,
    "table">:
      doc "Export a table by name or index" $
      T.string,
    "global">:
      doc "Export a global by name or index" $
      T.string]


-- ================================================================================================
-- Literal / constant values
-- ================================================================================================

constValue :: Binding
constValue = define "ConstValue" $
  doc "A constant value instruction" $
  T.union [
    "i32">:
      doc "A 32-bit integer constant" $
      T.int32,
    "i64">:
      doc "A 64-bit integer constant" $
      T.int64,
    "f32">:
      doc "A 32-bit float constant" $
      T.float32,
    "f64">:
      doc "A 64-bit float constant" $
      T.float64]
