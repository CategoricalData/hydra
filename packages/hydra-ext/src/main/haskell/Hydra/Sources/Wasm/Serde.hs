-- | WebAssembly serializer: converts WAT AST to concrete WAT text format.
-- Serializes the WASM syntax model (Hydra.Wasm.Syntax) into properly formatted WAT source code.

module Hydra.Sources.Wasm.Serde where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants  as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import Hydra.Ast
import qualified Hydra.Wasm.Syntax as W
import qualified Hydra.Sources.Wasm.Syntax as WasmSyntax


define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.wasm.serde"

module_ :: Module
module_ = Module ns definitions
    [Constants.ns, Serialization.ns]
    (WasmSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "WebAssembly serializer: converts WAT AST to concrete WAT text format"
  where
    definitions = [
      -- Top-level serialization
      toDefinition moduleToExpr,
      toDefinition moduleFieldToExpr,
      -- Types
      toDefinition valTypeToStr,
      toDefinition funcTypeToExpr,
      toDefinition typeUseToExpr,
      toDefinition typeDefToExpr,
      toDefinition paramToExpr,
      -- Functions
      toDefinition funcToExpr,
      toDefinition funcLocalToExpr,
      -- Instructions
      toDefinition instructionToExpr,
      toDefinition blockInstructionToExpr,
      toDefinition blockTypeToExpr,
      toDefinition ifInstructionToExpr,
      -- Memory
      toDefinition memoryDefToExpr,
      toDefinition dataSegmentToExpr,
      toDefinition limitsToExpr,
      -- Globals
      toDefinition globalDefToExpr,
      toDefinition globalTypeToExpr,
      -- Tables
      toDefinition tableDefToExpr,
      -- Imports and exports
      toDefinition importDefToExpr,
      toDefinition importDescToExpr,
      toDefinition exportDefToExpr,
      toDefinition exportDescToExpr,
      -- Constants
      toDefinition constValueToExpr,
      -- Helpers
      toDefinition refTypeToStr,
      toDefinition toWatComment]


-- =============================================================================
-- Top-level serialization
-- =============================================================================

moduleToExpr :: TTermDefinition (W.Module -> Expr)
moduleToExpr = define "moduleToExpr" $
  doc "Serialize a WebAssembly module to a WAT expression" $
  lambda "mod" $ lets [
    "name">: project W._Module W._Module_name @@ var "mod",
    "fields">: project W._Module W._Module_fields @@ var "mod",
    "nameStr">: Maybes.maybe (string "") (lambda "n" $ Strings.cat2 (string " $") (var "n")) (var "name"),
    "fieldExprs">: Lists.map moduleFieldToExpr (var "fields")] $
    Serialization.newlineSep @@ Lists.concat (list [
      list [Serialization.cst @@ Strings.cat2 (string "(module") (var "nameStr")],
      Lists.map (lambda "fe" $ Serialization.cst @@ Strings.cat2 (string "  ") (Serialization.printExpr @@ var "fe")) (var "fieldExprs"),
      list [Serialization.cst @@ string ")"]])

moduleFieldToExpr :: TTermDefinition (W.ModuleField -> Expr)
moduleFieldToExpr = define "moduleFieldToExpr" $
  doc "Serialize a module field to a WAT expression" $
  lambda "field" $
    cases W._ModuleField (var "field") Nothing [
      W._ModuleField_type>>: lambda "t" $ typeDefToExpr @@ var "t",
      W._ModuleField_func>>: lambda "f" $ funcToExpr @@ var "f",
      W._ModuleField_memory>>: lambda "m" $ memoryDefToExpr @@ var "m",
      W._ModuleField_table>>: lambda "t" $ tableDefToExpr @@ var "t",
      W._ModuleField_global>>: lambda "g" $ globalDefToExpr @@ var "g",
      W._ModuleField_import>>: lambda "i" $ importDefToExpr @@ var "i",
      W._ModuleField_export>>: lambda "e" $ exportDefToExpr @@ var "e",
      W._ModuleField_data>>: lambda "d" $ dataSegmentToExpr @@ var "d",
      W._ModuleField_elem>>: lambda "_" $ Serialization.cst @@ string ";; elem segment (not yet supported)",
      W._ModuleField_start>>: lambda "s" $
        Serialization.cst @@ Strings.cat (list [string "(start $", var "s", string ")"])]


-- =============================================================================
-- Types
-- =============================================================================

valTypeToStr :: TTermDefinition (W.ValType -> String)
valTypeToStr = define "valTypeToStr" $
  doc "Convert a value type to its WAT string representation" $
  lambda "vt" $
    cases W._ValType (var "vt") Nothing [
      W._ValType_i32>>: constant $ string "i32",
      W._ValType_i64>>: constant $ string "i64",
      W._ValType_f32>>: constant $ string "f32",
      W._ValType_f64>>: constant $ string "f64",
      W._ValType_funcref>>: constant $ string "funcref",
      W._ValType_externref>>: constant $ string "externref"]

funcTypeToExpr :: TTermDefinition (W.FuncType -> Expr)
funcTypeToExpr = define "funcTypeToExpr" $
  doc "Serialize a function type to WAT" $
  lambda "ft" $ lets [
    "params">: project W._FuncType W._FuncType_params @@ var "ft",
    "results">: project W._FuncType W._FuncType_results @@ var "ft",
    "paramParts">: Lists.map
      (lambda "p" $ Serialization.cst @@ Strings.cat (list [string "(param ", valTypeToStr @@ var "p", string ")"]))
      (var "params"),
    "resultParts">: Lists.map
      (lambda "r" $ Serialization.cst @@ Strings.cat (list [string "(result ", valTypeToStr @@ var "r", string ")"]))
      (var "results")] $
    Serialization.spaceSep @@ Maybes.cat (list [
      just (Serialization.cst @@ string "(func"),
      Logic.ifElse (Lists.null (var "paramParts")) nothing (just (Serialization.spaceSep @@ var "paramParts")),
      Logic.ifElse (Lists.null (var "resultParts")) nothing (just (Serialization.spaceSep @@ var "resultParts")),
      just (Serialization.cst @@ string ")")])

typeUseToExpr :: TTermDefinition (W.TypeUse -> Expr)
typeUseToExpr = define "typeUseToExpr" $
  doc "Serialize a type use clause to WAT" $
  lambda "tu" $ lets [
    "idx">: project W._TypeUse W._TypeUse_index @@ var "tu",
    "params">: project W._TypeUse W._TypeUse_params @@ var "tu",
    "results">: project W._TypeUse W._TypeUse_results @@ var "tu",
    "idxPart">: Maybes.map
      (lambda "i" $ Serialization.cst @@ Strings.cat (list [string "(type $", var "i", string ")"]))
      (var "idx"),
    "paramParts">: Lists.map (paramToExpr) (var "params"),
    "resultParts">: Lists.map
      (lambda "r" $ Serialization.cst @@ Strings.cat (list [string "(result ", valTypeToStr @@ var "r", string ")"]))
      (var "results")] $
    Serialization.spaceSep @@ Maybes.cat (Lists.concat (list [
      list [var "idxPart"],
      Lists.map (lambda "p" $ just (var "p")) (var "paramParts"),
      Lists.map (lambda "r" $ just (var "r")) (var "resultParts")]))

typeDefToExpr :: TTermDefinition (W.TypeDef -> Expr)
typeDefToExpr = define "typeDefToExpr" $
  doc "Serialize a type definition to WAT" $
  lambda "td" $ lets [
    "name">: project W._TypeDef W._TypeDef_name @@ var "td",
    "ft">: project W._TypeDef W._TypeDef_type @@ var "td",
    "nameStr">: Maybes.maybe (string "") (lambda "n" $ Strings.cat2 (string " $") (var "n")) (var "name")] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ Strings.cat2 (string "(type") (var "nameStr"),
      funcTypeToExpr @@ var "ft",
      Serialization.cst @@ string ")"]

paramToExpr :: TTermDefinition (W.Param -> Expr)
paramToExpr = define "paramToExpr" $
  doc "Serialize a function parameter to WAT" $
  lambda "p" $ lets [
    "name">: project W._Param W._Param_name @@ var "p",
    "typ">: project W._Param W._Param_type @@ var "p",
    "nameStr">: Maybes.maybe (string "") (lambda "n" $ Strings.cat2 (string " $") (var "n")) (var "name")] $
    Serialization.cst @@ Strings.cat (list [string "(param", var "nameStr", string " ", valTypeToStr @@ var "typ", string ")"])


-- =============================================================================
-- Functions
-- =============================================================================

funcToExpr :: TTermDefinition (W.Func -> Expr)
funcToExpr = define "funcToExpr" $
  doc "Serialize a function definition to WAT" $
  lambda "f" $ lets [
    "name">: project W._Func W._Func_name @@ var "f",
    "typeUse">: project W._Func W._Func_typeUse @@ var "f",
    "locals">: project W._Func W._Func_locals @@ var "f",
    "body">: project W._Func W._Func_body @@ var "f",
    "nameStr">: Maybes.maybe (string "") (lambda "n" $ Strings.cat2 (string " $") (var "n")) (var "name"),
    "headerStr">: Strings.cat2 (string "(func") (var "nameStr"),
    "typeUsePart">: typeUseToExpr @@ var "typeUse",
    "localParts">: Lists.map funcLocalToExpr (var "locals"),
    "bodyParts">: Lists.map instructionToExpr (var "body"),
    "innerParts">: Lists.concat2 (var "localParts") (var "bodyParts")] $
    Logic.ifElse (Lists.null (var "innerParts"))
      (Serialization.spaceSep @@ list [
        Serialization.cst @@ var "headerStr",
        var "typeUsePart",
        Serialization.cst @@ string ")"])
      (Serialization.newlineSep @@ Lists.concat (list [
        list [Serialization.spaceSep @@ list [Serialization.cst @@ var "headerStr", var "typeUsePart"]],
        Lists.map (lambda "p" $ Serialization.cst @@ Strings.cat2 (string "  ") (Serialization.printExpr @@ var "p")) (var "innerParts"),
        list [Serialization.cst @@ string ")"]]))

funcLocalToExpr :: TTermDefinition (W.FuncLocal -> Expr)
funcLocalToExpr = define "funcLocalToExpr" $
  doc "Serialize a local variable declaration to WAT" $
  lambda "l" $ lets [
    "name">: project W._FuncLocal W._FuncLocal_name @@ var "l",
    "typ">: project W._FuncLocal W._FuncLocal_type @@ var "l",
    "nameStr">: Maybes.maybe (string "") (lambda "n" $ Strings.cat2 (string " $") (var "n")) (var "name")] $
    Serialization.cst @@ Strings.cat (list [
      string "(local", var "nameStr", string " ", valTypeToStr @@ var "typ", string ")"])


-- =============================================================================
-- Instructions
-- =============================================================================

instructionToExpr :: TTermDefinition (W.Instruction -> Expr)
instructionToExpr = define "instructionToExpr" $
  doc "Serialize an instruction to WAT" $
  lambda "instr" $
    cases W._Instruction (var "instr") Nothing [
      W._Instruction_const>>: lambda "c" $ constValueToExpr @@ var "c",
      W._Instruction_localGet>>: lambda "v" $
        Serialization.cst @@ Strings.cat (list [string "local.get $", var "v"]),
      W._Instruction_localSet>>: lambda "v" $
        Serialization.cst @@ Strings.cat (list [string "local.set $", var "v"]),
      W._Instruction_localTee>>: lambda "v" $
        Serialization.cst @@ Strings.cat (list [string "local.tee $", var "v"]),
      W._Instruction_globalGet>>: lambda "v" $
        Serialization.cst @@ Strings.cat (list [string "global.get $", var "v"]),
      W._Instruction_globalSet>>: lambda "v" $
        Serialization.cst @@ Strings.cat (list [string "global.set $", var "v"]),
      W._Instruction_load>>: lambda "ld" $ lets [
        "vt">: project W._MemoryInstruction W._MemoryInstruction_type @@ var "ld"] $
        Serialization.cst @@ Strings.cat (list [
          valTypeToStr @@ var "vt", string ".load"]),
      W._Instruction_store>>: lambda "st" $ lets [
        "vt">: project W._MemoryInstruction W._MemoryInstruction_type @@ var "st"] $
        Serialization.cst @@ Strings.cat (list [
          valTypeToStr @@ var "vt", string ".store"]),
      W._Instruction_unop>>: lambda "op" $ lets [
        "vt">: project W._NumericOp W._NumericOp_type @@ var "op",
        "nm">: project W._NumericOp W._NumericOp_name @@ var "op"] $
        Serialization.cst @@ Strings.cat (list [
          valTypeToStr @@ var "vt", string ".", var "nm"]),
      W._Instruction_binop>>: lambda "op" $ lets [
        "vt">: project W._NumericOp W._NumericOp_type @@ var "op",
        "nm">: project W._NumericOp W._NumericOp_name @@ var "op"] $
        Serialization.cst @@ Strings.cat (list [
          valTypeToStr @@ var "vt", string ".", var "nm"]),
      W._Instruction_testop>>: lambda "op" $ lets [
        "vt">: project W._NumericOp W._NumericOp_type @@ var "op",
        "nm">: project W._NumericOp W._NumericOp_name @@ var "op"] $
        Serialization.cst @@ Strings.cat (list [
          valTypeToStr @@ var "vt", string ".", var "nm"]),
      W._Instruction_relop>>: lambda "op" $ lets [
        "vt">: project W._NumericOp W._NumericOp_type @@ var "op",
        "nm">: project W._NumericOp W._NumericOp_name @@ var "op"] $
        Serialization.cst @@ Strings.cat (list [
          valTypeToStr @@ var "vt", string ".", var "nm"]),
      W._Instruction_convert>>: lambda "c" $
        Serialization.cst @@ var "c",
      W._Instruction_call>>: lambda "f" $
        Serialization.cst @@ Strings.cat (list [string "call $", var "f"]),
      W._Instruction_callIndirect>>: lambda "tu" $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "call_indirect",
          typeUseToExpr @@ var "tu"],
      W._Instruction_block>>: lambda "b" $
        blockInstructionToExpr @@ string "block" @@ var "b",
      W._Instruction_loop>>: lambda "b" $
        blockInstructionToExpr @@ string "loop" @@ var "b",
      W._Instruction_if>>: lambda "i" $
        ifInstructionToExpr @@ var "i",
      W._Instruction_br>>: lambda "l" $
        Serialization.cst @@ Strings.cat (list [string "br $", var "l"]),
      W._Instruction_brIf>>: lambda "l" $
        Serialization.cst @@ Strings.cat (list [string "br_if $", var "l"]),
      W._Instruction_brTable>>: lambda "bt" $ lets [
        "labels">: project W._BrTableArgs W._BrTableArgs_labels @@ var "bt",
        "def">: project W._BrTableArgs W._BrTableArgs_default @@ var "bt"] $
        Serialization.cst @@ Strings.cat (list [
          string "br_table ",
          Strings.intercalate (string " ") (Lists.map (lambda "l" $ Strings.cat2 (string "$") (var "l")) (var "labels")),
          string " $", var "def"]),
      W._Instruction_return>>: constant $ Serialization.cst @@ string "return",
      W._Instruction_drop>>: constant $ Serialization.cst @@ string "drop",
      W._Instruction_select>>: constant $ Serialization.cst @@ string "select",
      W._Instruction_unreachable>>: constant $ Serialization.cst @@ string "unreachable",
      W._Instruction_nop>>: constant $ Serialization.cst @@ string "nop",
      W._Instruction_memorySize>>: constant $ Serialization.cst @@ string "memory.size",
      W._Instruction_memoryGrow>>: constant $ Serialization.cst @@ string "memory.grow",
      W._Instruction_refNull>>: lambda "rt" $
        Serialization.cst @@ Strings.cat (list [string "ref.null ", refTypeToStr @@ var "rt"]),
      W._Instruction_refIsNull>>: constant $ Serialization.cst @@ string "ref.is_null",
      W._Instruction_raw>>: lambda "s" $
        Serialization.cst @@ var "s"]

blockInstructionToExpr :: TTermDefinition (String -> W.BlockInstruction -> Expr)
blockInstructionToExpr = define "blockInstructionToExpr" $
  doc "Serialize a block or loop instruction to WAT" $
  lambda "keyword" $ lambda "b" $ lets [
    "label">: project W._BlockInstruction W._BlockInstruction_label @@ var "b",
    "bt">: project W._BlockInstruction W._BlockInstruction_blockType @@ var "b",
    "body">: project W._BlockInstruction W._BlockInstruction_body @@ var "b",
    "labelStr">: Maybes.maybe (string "") (lambda "l" $ Strings.cat2 (string " $") (var "l")) (var "label"),
    "btPart">: blockTypeToExpr @@ var "bt",
    "bodyParts">: Lists.map instructionToExpr (var "body"),
    "header">: Serialization.spaceSep @@ Maybes.cat (list [
      just (Serialization.cst @@ Strings.cat (list [string "(", var "keyword", var "labelStr"])),
      var "btPart"])] $
    Serialization.newlineSep @@ Lists.concat (list [
      list [var "header"],
      Lists.map (lambda "p" $ Serialization.cst @@ Strings.cat2 (string "  ") (Serialization.printExpr @@ var "p")) (var "bodyParts"),
      list [Serialization.cst @@ string ")"]])

blockTypeToExpr :: TTermDefinition (W.BlockType -> Maybe Expr)
blockTypeToExpr = define "blockTypeToExpr" $
  doc "Serialize a block type to WAT" $
  lambda "bt" $
    cases W._BlockType (var "bt") Nothing [
      W._BlockType_empty>>: constant nothing,
      W._BlockType_value>>: lambda "vt" $
        just (Serialization.cst @@ Strings.cat (list [string "(result ", valTypeToStr @@ var "vt", string ")"])),
      W._BlockType_typeUse>>: lambda "tu" $
        just (typeUseToExpr @@ var "tu")]

ifInstructionToExpr :: TTermDefinition (W.IfInstruction -> Expr)
ifInstructionToExpr = define "ifInstructionToExpr" $
  doc "Serialize an if instruction to WAT" $
  lambda "i" $ lets [
    "label">: project W._IfInstruction W._IfInstruction_label @@ var "i",
    "bt">: project W._IfInstruction W._IfInstruction_blockType @@ var "i",
    "thenBranch">: project W._IfInstruction W._IfInstruction_then @@ var "i",
    "elseBranch">: project W._IfInstruction W._IfInstruction_else @@ var "i",
    "labelStr">: Maybes.maybe (string "") (lambda "l" $ Strings.cat2 (string " $") (var "l")) (var "label"),
    "btPart">: blockTypeToExpr @@ var "bt",
    "thenParts">: Lists.map instructionToExpr (var "thenBranch"),
    "elseParts">: Lists.map instructionToExpr (var "elseBranch"),
    "header">: Serialization.spaceSep @@ Maybes.cat (list [
      just (Serialization.cst @@ Strings.cat (list [string "(if", var "labelStr"])),
      var "btPart"]),
    "thenBlock">: Serialization.newlineSep @@ Lists.concat (list [
      list [Serialization.cst @@ string "(then"],
      Lists.map (lambda "p" $ Serialization.cst @@ Strings.cat2 (string "  ") (Serialization.printExpr @@ var "p")) (var "thenParts"),
      list [Serialization.cst @@ string ")"]]),
    "elseBlock">: Serialization.newlineSep @@ Lists.concat (list [
      list [Serialization.cst @@ string "(else"],
      Lists.map (lambda "p" $ Serialization.cst @@ Strings.cat2 (string "  ") (Serialization.printExpr @@ var "p")) (var "elseParts"),
      list [Serialization.cst @@ string ")"]])] $
    Logic.ifElse (Lists.null (var "elseParts"))
      (Serialization.newlineSep @@ list [var "header", var "thenBlock", Serialization.cst @@ string ")"])
      (Serialization.newlineSep @@ list [var "header", var "thenBlock", var "elseBlock", Serialization.cst @@ string ")"])


-- =============================================================================
-- Memory
-- =============================================================================

memoryDefToExpr :: TTermDefinition (W.MemoryDef -> Expr)
memoryDefToExpr = define "memoryDefToExpr" $
  doc "Serialize a memory definition to WAT" $
  lambda "m" $ lets [
    "name">: project W._MemoryDef W._MemoryDef_name @@ var "m",
    "lim">: project W._MemoryDef W._MemoryDef_limits @@ var "m",
    "nameStr">: Maybes.maybe (string "") (lambda "n" $ Strings.cat2 (string " $") (var "n")) (var "name")] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ Strings.cat2 (string "(memory") (var "nameStr"),
      limitsToExpr @@ var "lim",
      Serialization.cst @@ string ")"]

dataSegmentToExpr :: TTermDefinition (W.DataSegment -> Expr)
dataSegmentToExpr = define "dataSegmentToExpr" $
  doc "Serialize a data segment to WAT" $
  lambda "d" $ lets [
    "name">: project W._DataSegment W._DataSegment_name @@ var "d",
    "mode">: project W._DataSegment W._DataSegment_mode @@ var "d",
    "bytes">: project W._DataSegment W._DataSegment_bytes @@ var "d",
    "nameStr">: Maybes.maybe (string "") (lambda "n" $ Strings.cat2 (string " $") (var "n")) (var "name")] $
    cases W._DataMode (var "mode") Nothing [
      W._DataMode_active>>: lambda "offset" $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ Strings.cat2 (string "(data") (var "nameStr"),
          Serialization.spaceSep @@ list [
            Serialization.cst @@ string "(offset",
            Serialization.spaceSep @@ Lists.map instructionToExpr (var "offset"),
            Serialization.cst @@ string ")"],
          Serialization.cst @@ Strings.cat (list [string "\"", var "bytes", string "\")"]) ],
      W._DataMode_passive>>: constant $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ Strings.cat2 (string "(data") (var "nameStr"),
          Serialization.cst @@ Strings.cat (list [string "\"", var "bytes", string "\")"])]]

limitsToExpr :: TTermDefinition (W.Limits -> Expr)
limitsToExpr = define "limitsToExpr" $
  doc "Serialize limits to WAT" $
  lambda "l" $ lets [
    "mn">: project W._Limits W._Limits_min @@ var "l",
    "mx">: project W._Limits W._Limits_max @@ var "l"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      just (Serialization.cst @@ Literals.showInt32 (var "mn")),
      Maybes.map (lambda "m" $ Serialization.cst @@ Literals.showInt32 (var "m")) (var "mx")])


-- =============================================================================
-- Globals
-- =============================================================================

globalDefToExpr :: TTermDefinition (W.GlobalDef -> Expr)
globalDefToExpr = define "globalDefToExpr" $
  doc "Serialize a global definition to WAT" $
  lambda "g" $ lets [
    "name">: project W._GlobalDef W._GlobalDef_name @@ var "g",
    "gt">: project W._GlobalDef W._GlobalDef_type @@ var "g",
    "init">: project W._GlobalDef W._GlobalDef_init @@ var "g",
    "nameStr">: Maybes.maybe (string "") (lambda "n" $ Strings.cat2 (string " $") (var "n")) (var "name")] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ Strings.cat2 (string "(global") (var "nameStr"),
      globalTypeToExpr @@ var "gt",
      Serialization.spaceSep @@ Lists.map instructionToExpr (var "init"),
      Serialization.cst @@ string ")"]

globalTypeToExpr :: TTermDefinition (W.GlobalType -> Expr)
globalTypeToExpr = define "globalTypeToExpr" $
  doc "Serialize a global type to WAT" $
  lambda "gt" $ lets [
    "vt">: project W._GlobalType W._GlobalType_valType @@ var "gt",
    "mut">: project W._GlobalType W._GlobalType_mutable @@ var "gt"] $
    Logic.ifElse (var "mut")
      (Serialization.cst @@ Strings.cat (list [string "(mut ", valTypeToStr @@ var "vt", string ")"]))
      (Serialization.cst @@ (valTypeToStr @@ var "vt"))


-- =============================================================================
-- Tables
-- =============================================================================

tableDefToExpr :: TTermDefinition (W.TableDef -> Expr)
tableDefToExpr = define "tableDefToExpr" $
  doc "Serialize a table definition to WAT" $
  lambda "t" $ lets [
    "name">: project W._TableDef W._TableDef_name @@ var "t",
    "rt">: project W._TableDef W._TableDef_refType @@ var "t",
    "lim">: project W._TableDef W._TableDef_limits @@ var "t",
    "nameStr">: Maybes.maybe (string "") (lambda "n" $ Strings.cat2 (string " $") (var "n")) (var "name")] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ Strings.cat2 (string "(table") (var "nameStr"),
      limitsToExpr @@ var "lim",
      Serialization.cst @@ (refTypeToStr @@ var "rt"),
      Serialization.cst @@ string ")"]


-- =============================================================================
-- Imports and exports
-- =============================================================================

importDefToExpr :: TTermDefinition (W.ImportDef -> Expr)
importDefToExpr = define "importDefToExpr" $
  doc "Serialize an import declaration to WAT" $
  lambda "i" $ lets [
    "modName">: project W._ImportDef W._ImportDef_module @@ var "i",
    "name">: project W._ImportDef W._ImportDef_name @@ var "i",
    "desc">: project W._ImportDef W._ImportDef_desc @@ var "i"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "(import",
      Serialization.cst @@ Strings.cat (list [string "\"", var "modName", string "\""]),
      Serialization.cst @@ Strings.cat (list [string "\"", var "name", string "\""]),
      importDescToExpr @@ var "desc",
      Serialization.cst @@ string ")"]

importDescToExpr :: TTermDefinition (W.ImportDesc -> Expr)
importDescToExpr = define "importDescToExpr" $
  doc "Serialize an import descriptor to WAT" $
  lambda "desc" $
    cases W._ImportDesc (var "desc") Nothing [
      W._ImportDesc_func>>: lambda "f" $ lets [
        "name">: project W._ImportFunc W._ImportFunc_name @@ var "f",
        "tu">: project W._ImportFunc W._ImportFunc_typeUse @@ var "f",
        "nameStr">: Maybes.maybe (string "") (lambda "n" $ Strings.cat2 (string " $") (var "n")) (var "name")] $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ Strings.cat2 (string "(func") (var "nameStr"),
          typeUseToExpr @@ var "tu",
          Serialization.cst @@ string ")"],
      W._ImportDesc_memory>>: lambda "m" $ lets [
        "name">: project W._ImportMemory W._ImportMemory_name @@ var "m",
        "lim">: project W._ImportMemory W._ImportMemory_limits @@ var "m",
        "nameStr">: Maybes.maybe (string "") (lambda "n" $ Strings.cat2 (string " $") (var "n")) (var "name")] $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ Strings.cat2 (string "(memory") (var "nameStr"),
          limitsToExpr @@ var "lim",
          Serialization.cst @@ string ")"],
      W._ImportDesc_table>>: lambda "t" $ lets [
        "name">: project W._ImportTable W._ImportTable_name @@ var "t",
        "rt">: project W._ImportTable W._ImportTable_refType @@ var "t",
        "lim">: project W._ImportTable W._ImportTable_limits @@ var "t",
        "nameStr">: Maybes.maybe (string "") (lambda "n" $ Strings.cat2 (string " $") (var "n")) (var "name")] $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ Strings.cat2 (string "(table") (var "nameStr"),
          limitsToExpr @@ var "lim",
          Serialization.cst @@ (refTypeToStr @@ var "rt"),
          Serialization.cst @@ string ")"],
      W._ImportDesc_global>>: lambda "g" $ lets [
        "name">: project W._ImportGlobal W._ImportGlobal_name @@ var "g",
        "gt">: project W._ImportGlobal W._ImportGlobal_type @@ var "g",
        "nameStr">: Maybes.maybe (string "") (lambda "n" $ Strings.cat2 (string " $") (var "n")) (var "name")] $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ Strings.cat2 (string "(global") (var "nameStr"),
          globalTypeToExpr @@ var "gt",
          Serialization.cst @@ string ")"]]

exportDefToExpr :: TTermDefinition (W.ExportDef -> Expr)
exportDefToExpr = define "exportDefToExpr" $
  doc "Serialize an export declaration to WAT" $
  lambda "e" $ lets [
    "name">: project W._ExportDef W._ExportDef_name @@ var "e",
    "desc">: project W._ExportDef W._ExportDef_desc @@ var "e"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "(export",
      Serialization.cst @@ Strings.cat (list [string "\"", var "name", string "\""]),
      exportDescToExpr @@ var "desc",
      Serialization.cst @@ string ")"]

exportDescToExpr :: TTermDefinition (W.ExportDesc -> Expr)
exportDescToExpr = define "exportDescToExpr" $
  doc "Serialize an export descriptor to WAT" $
  lambda "desc" $
    cases W._ExportDesc (var "desc") Nothing [
      W._ExportDesc_func>>: lambda "f" $
        Serialization.cst @@ Strings.cat (list [string "(func $", var "f", string ")"]),
      W._ExportDesc_memory>>: lambda "m" $
        Serialization.cst @@ Strings.cat (list [string "(memory $", var "m", string ")"]),
      W._ExportDesc_table>>: lambda "t" $
        Serialization.cst @@ Strings.cat (list [string "(table $", var "t", string ")"]),
      W._ExportDesc_global>>: lambda "g" $
        Serialization.cst @@ Strings.cat (list [string "(global $", var "g", string ")"])]


-- =============================================================================
-- Constants
-- =============================================================================

constValueToExpr :: TTermDefinition (W.ConstValue -> Expr)
constValueToExpr = define "constValueToExpr" $
  doc "Serialize a constant value instruction to WAT" $
  lambda "c" $
    cases W._ConstValue (var "c") Nothing [
      W._ConstValue_i32>>: lambda "v" $
        Serialization.cst @@ Strings.cat2 (string "i32.const ") (Literals.showInt32 (var "v")),
      W._ConstValue_i64>>: lambda "v" $
        Serialization.cst @@ Strings.cat2 (string "i64.const ") (Literals.showInt64 (var "v")),
      W._ConstValue_f32>>: lambda "v" $
        Serialization.cst @@ Strings.cat2 (string "f32.const ") (Literals.showFloat32 (var "v")),
      W._ConstValue_f64>>: lambda "v" $
        Serialization.cst @@ Strings.cat2 (string "f64.const ") (Literals.showFloat64 (var "v"))]


-- =============================================================================
-- Helpers
-- =============================================================================

refTypeToStr :: TTermDefinition (W.RefType -> String)
refTypeToStr = define "refTypeToStr" $
  doc "Convert a reference type to its WAT string" $
  lambda "rt" $
    cases W._RefType (var "rt") Nothing [
      W._RefType_funcref>>: constant $ string "funcref",
      W._RefType_externref>>: constant $ string "externref"]

toWatComment :: TTermDefinition (String -> String)
toWatComment = define "toWatComment" $
  doc "Convert a string to a WAT comment" $
  lambda "s" $
    Strings.cat (list [string ";; ", var "s"])
