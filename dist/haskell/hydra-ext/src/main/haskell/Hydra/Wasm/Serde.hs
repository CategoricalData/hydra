-- Note: this is an automatically generated file. Do not edit.

-- | WebAssembly serializer: converts WAT AST to concrete WAT text format

module Hydra.Wasm.Serde where

import qualified Hydra.Ast as Ast
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Wasm.Syntax as Syntax
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Serialize a block or loop instruction to WAT
blockInstructionToExpr :: String -> Syntax.BlockInstruction -> Ast.Expr
blockInstructionToExpr keyword b =

      let label = Syntax.blockInstructionLabel b
          bt = Syntax.blockInstructionBlockType b
          body = Syntax.blockInstructionBody b
          labelStr = Maybes.maybe "" (\l -> Strings.cat2 " $" l) label
          btPart = blockTypeToExpr bt
          bodyParts = Lists.map instructionToExpr body
          header =
                  Serialization.spaceSep (Maybes.cat [
                    Just (Serialization.cst (Strings.cat [
                      "(",
                      keyword,
                      labelStr])),
                    btPart])
      in (Serialization.newlineSep (Lists.concat [
        [
          header],
        (Lists.map (\p -> Serialization.cst (Strings.cat2 "  " (Serialization.printExpr p))) bodyParts),
        [
          Serialization.cst ")"]]))

-- | Serialize a block type to WAT
blockTypeToExpr :: Syntax.BlockType -> Maybe Ast.Expr
blockTypeToExpr bt =
    case bt of
      Syntax.BlockTypeEmpty -> Nothing
      Syntax.BlockTypeValue v0 -> Just (Serialization.cst (Strings.cat [
        "(result ",
        (valTypeToStr v0),
        ")"]))
      Syntax.BlockTypeTypeUse v0 -> Just (typeUseToExpr v0)

-- | Serialize a constant value instruction to WAT
constValueToExpr :: Syntax.ConstValue -> Ast.Expr
constValueToExpr c =
    case c of
      Syntax.ConstValueI32 v0 -> Serialization.cst (Strings.cat2 "i32.const " (Literals.showInt32 v0))
      Syntax.ConstValueI64 v0 -> Serialization.cst (Strings.cat2 "i64.const " (Literals.showInt64 v0))
      Syntax.ConstValueF32 v0 -> Serialization.cst (Strings.cat2 "f32.const " (Literals.showFloat32 v0))
      Syntax.ConstValueF64 v0 -> Serialization.cst (Strings.cat2 "f64.const " (Literals.showFloat64 v0))

-- | Serialize a data segment to WAT
dataSegmentToExpr :: Syntax.DataSegment -> Ast.Expr
dataSegmentToExpr d =

      let name = Syntax.dataSegmentName d
          mode = Syntax.dataSegmentMode d
          bytes = Syntax.dataSegmentBytes d
          nameStr = Maybes.maybe "" (\n -> Strings.cat2 " $" n) name
      in case mode of
        Syntax.DataModeActive v0 -> Serialization.spaceSep [
          Serialization.cst (Strings.cat2 "(data" nameStr),
          (Serialization.spaceSep [
            Serialization.cst "(offset",
            (Serialization.spaceSep (Lists.map instructionToExpr v0)),
            (Serialization.cst ")")]),
          (Serialization.cst (Strings.cat [
            "\"",
            bytes,
            "\")"]))]
        Syntax.DataModePassive -> Serialization.spaceSep [
          Serialization.cst (Strings.cat2 "(data" nameStr),
          (Serialization.cst (Strings.cat [
            "\"",
            bytes,
            "\")"]))]

-- | Serialize an export declaration to WAT
exportDefToExpr :: Syntax.ExportDef -> Ast.Expr
exportDefToExpr e =

      let name = Syntax.exportDefName e
          desc = Syntax.exportDefDesc e
      in (Serialization.spaceSep [
        Serialization.cst "(export",
        (Serialization.cst (Strings.cat [
          "\"",
          name,
          "\""])),
        (exportDescToExpr desc),
        (Serialization.cst ")")])

-- | Serialize an export descriptor to WAT
exportDescToExpr :: Syntax.ExportDesc -> Ast.Expr
exportDescToExpr desc =
    case desc of
      Syntax.ExportDescFunc v0 -> Serialization.cst (Strings.cat [
        "(func $",
        v0,
        ")"])
      Syntax.ExportDescMemory v0 -> Serialization.cst (Strings.cat [
        "(memory $",
        v0,
        ")"])
      Syntax.ExportDescTable v0 -> Serialization.cst (Strings.cat [
        "(table $",
        v0,
        ")"])
      Syntax.ExportDescGlobal v0 -> Serialization.cst (Strings.cat [
        "(global $",
        v0,
        ")"])

-- | Serialize a local variable declaration to WAT
funcLocalToExpr :: Syntax.FuncLocal -> Ast.Expr
funcLocalToExpr l =

      let name = Syntax.funcLocalName l
          typ = Syntax.funcLocalType l
          nameStr = Maybes.maybe "" (\n -> Strings.cat2 " $" n) name
      in (Serialization.cst (Strings.cat [
        "(local",
        nameStr,
        " ",
        (valTypeToStr typ),
        ")"]))

-- | Serialize a function definition to WAT
funcToExpr :: Syntax.Func -> Ast.Expr
funcToExpr f =

      let name = Syntax.funcName f
          typeUse = Syntax.funcTypeUse f
          locals = Syntax.funcLocals f
          body = Syntax.funcBody f
          nameStr = Maybes.maybe "" (\n -> Strings.cat2 " $" n) name
          headerStr = Strings.cat2 "(func" nameStr
          typeUsePart = typeUseToExpr typeUse
          localParts = Lists.map funcLocalToExpr locals
          bodyParts = Lists.map instructionToExpr body
          innerParts = Lists.concat2 localParts bodyParts
      in (Logic.ifElse (Lists.null innerParts) (Serialization.spaceSep [
        Serialization.cst headerStr,
        typeUsePart,
        (Serialization.cst ")")]) (Serialization.newlineSep (Lists.concat [
        [
          Serialization.spaceSep [
            Serialization.cst headerStr,
            typeUsePart]],
        (Lists.map (\p -> Serialization.cst (Strings.cat2 "  " (Serialization.printExpr p))) innerParts),
        [
          Serialization.cst ")"]])))

-- | Serialize a function type to WAT
funcTypeToExpr :: Syntax.FuncType -> Ast.Expr
funcTypeToExpr ft =

      let params = Syntax.funcTypeParams ft
          results = Syntax.funcTypeResults ft
          paramParts =
                  Lists.map (\p -> Serialization.cst (Strings.cat [
                    "(param ",
                    (valTypeToStr p),
                    ")"])) params
          resultParts =
                  Lists.map (\r -> Serialization.cst (Strings.cat [
                    "(result ",
                    (valTypeToStr r),
                    ")"])) results
      in (Serialization.spaceSep (Maybes.cat [
        Just (Serialization.cst "(func"),
        (Logic.ifElse (Lists.null paramParts) Nothing (Just (Serialization.spaceSep paramParts))),
        (Logic.ifElse (Lists.null resultParts) Nothing (Just (Serialization.spaceSep resultParts))),
        (Just (Serialization.cst ")"))]))

-- | Serialize a global definition to WAT
globalDefToExpr :: Syntax.GlobalDef -> Ast.Expr
globalDefToExpr g =

      let name = Syntax.globalDefName g
          gt = Syntax.globalDefType g
          init = Syntax.globalDefInit g
          nameStr = Maybes.maybe "" (\n -> Strings.cat2 " $" n) name
      in (Serialization.spaceSep [
        Serialization.cst (Strings.cat2 "(global" nameStr),
        (globalTypeToExpr gt),
        (Serialization.spaceSep (Lists.map instructionToExpr init)),
        (Serialization.cst ")")])

-- | Serialize a global type to WAT
globalTypeToExpr :: Syntax.GlobalType -> Ast.Expr
globalTypeToExpr gt =

      let vt = Syntax.globalTypeValType gt
          mut = Syntax.globalTypeMutable gt
      in (Logic.ifElse mut (Serialization.cst (Strings.cat [
        "(mut ",
        (valTypeToStr vt),
        ")"])) (Serialization.cst (valTypeToStr vt)))

-- | Serialize an if instruction to WAT
ifInstructionToExpr :: Syntax.IfInstruction -> Ast.Expr
ifInstructionToExpr i =

      let label = Syntax.ifInstructionLabel i
          bt = Syntax.ifInstructionBlockType i
          thenBranch = Syntax.ifInstructionThen i
          elseBranch = Syntax.ifInstructionElse i
          labelStr = Maybes.maybe "" (\l -> Strings.cat2 " $" l) label
          btPart = blockTypeToExpr bt
          thenParts = Lists.map instructionToExpr thenBranch
          elseParts = Lists.map instructionToExpr elseBranch
          header =
                  Serialization.spaceSep (Maybes.cat [
                    Just (Serialization.cst (Strings.cat [
                      "(if",
                      labelStr])),
                    btPart])
          thenBlock =
                  Serialization.newlineSep (Lists.concat [
                    [
                      Serialization.cst "(then"],
                    (Lists.map (\p -> Serialization.cst (Strings.cat2 "  " (Serialization.printExpr p))) thenParts),
                    [
                      Serialization.cst ")"]])
          elseBlock =
                  Serialization.newlineSep (Lists.concat [
                    [
                      Serialization.cst "(else"],
                    (Lists.map (\p -> Serialization.cst (Strings.cat2 "  " (Serialization.printExpr p))) elseParts),
                    [
                      Serialization.cst ")"]])
      in (Logic.ifElse (Lists.null elseParts) (Serialization.newlineSep [
        header,
        thenBlock,
        (Serialization.cst ")")]) (Serialization.newlineSep [
        header,
        thenBlock,
        elseBlock,
        (Serialization.cst ")")]))

-- | Serialize an import declaration to WAT
importDefToExpr :: Syntax.ImportDef -> Ast.Expr
importDefToExpr i =

      let modName = Syntax.importDefModule i
          name = Syntax.importDefName i
          desc = Syntax.importDefDesc i
      in (Serialization.spaceSep [
        Serialization.cst "(import",
        (Serialization.cst (Strings.cat [
          "\"",
          modName,
          "\""])),
        (Serialization.cst (Strings.cat [
          "\"",
          name,
          "\""])),
        (importDescToExpr desc),
        (Serialization.cst ")")])

-- | Serialize an import descriptor to WAT
importDescToExpr :: Syntax.ImportDesc -> Ast.Expr
importDescToExpr desc =
    case desc of
      Syntax.ImportDescFunc v0 ->
        let name = Syntax.importFuncName v0
            tu = Syntax.importFuncTypeUse v0
            nameStr = Maybes.maybe "" (\n -> Strings.cat2 " $" n) name
        in (Serialization.spaceSep [
          Serialization.cst (Strings.cat2 "(func" nameStr),
          (typeUseToExpr tu),
          (Serialization.cst ")")])
      Syntax.ImportDescMemory v0 ->
        let name = Syntax.importMemoryName v0
            lim = Syntax.importMemoryLimits v0
            nameStr = Maybes.maybe "" (\n -> Strings.cat2 " $" n) name
        in (Serialization.spaceSep [
          Serialization.cst (Strings.cat2 "(memory" nameStr),
          (limitsToExpr lim),
          (Serialization.cst ")")])
      Syntax.ImportDescTable v0 ->
        let name = Syntax.importTableName v0
            rt = Syntax.importTableRefType v0
            lim = Syntax.importTableLimits v0
            nameStr = Maybes.maybe "" (\n -> Strings.cat2 " $" n) name
        in (Serialization.spaceSep [
          Serialization.cst (Strings.cat2 "(table" nameStr),
          (limitsToExpr lim),
          (Serialization.cst (refTypeToStr rt)),
          (Serialization.cst ")")])
      Syntax.ImportDescGlobal v0 ->
        let name = Syntax.importGlobalName v0
            gt = Syntax.importGlobalType v0
            nameStr = Maybes.maybe "" (\n -> Strings.cat2 " $" n) name
        in (Serialization.spaceSep [
          Serialization.cst (Strings.cat2 "(global" nameStr),
          (globalTypeToExpr gt),
          (Serialization.cst ")")])

-- | Serialize an instruction to WAT
instructionToExpr :: Syntax.Instruction -> Ast.Expr
instructionToExpr instr =
    case instr of
      Syntax.InstructionConst v0 -> constValueToExpr v0
      Syntax.InstructionLocalGet v0 -> Serialization.cst (Strings.cat [
        "local.get $",
        v0])
      Syntax.InstructionLocalSet v0 -> Serialization.cst (Strings.cat [
        "local.set $",
        v0])
      Syntax.InstructionLocalTee v0 -> Serialization.cst (Strings.cat [
        "local.tee $",
        v0])
      Syntax.InstructionGlobalGet v0 -> Serialization.cst (Strings.cat [
        "global.get $",
        v0])
      Syntax.InstructionGlobalSet v0 -> Serialization.cst (Strings.cat [
        "global.set $",
        v0])
      Syntax.InstructionLoad v0 ->
        let vt = Syntax.memoryInstructionType v0
            ma = Syntax.memoryInstructionMemArg v0
            off = Syntax.memArgOffset ma
            offStr = Logic.ifElse (Equality.equal off 0) "" (Strings.cat2 " offset=" (Literals.showInt32 off))
        in (Serialization.cst (Strings.cat [
          valTypeToStr vt,
          ".load",
          offStr]))
      Syntax.InstructionStore v0 ->
        let vt = Syntax.memoryInstructionType v0
            ma = Syntax.memoryInstructionMemArg v0
            off = Syntax.memArgOffset ma
            offStr = Logic.ifElse (Equality.equal off 0) "" (Strings.cat2 " offset=" (Literals.showInt32 off))
        in (Serialization.cst (Strings.cat [
          valTypeToStr vt,
          ".store",
          offStr]))
      Syntax.InstructionUnop v0 ->
        let vt = Syntax.numericOpType v0
            nm = Syntax.numericOpName v0
        in (Serialization.cst (Strings.cat [
          valTypeToStr vt,
          ".",
          nm]))
      Syntax.InstructionBinop v0 ->
        let vt = Syntax.numericOpType v0
            nm = Syntax.numericOpName v0
        in (Serialization.cst (Strings.cat [
          valTypeToStr vt,
          ".",
          nm]))
      Syntax.InstructionTestop v0 ->
        let vt = Syntax.numericOpType v0
            nm = Syntax.numericOpName v0
        in (Serialization.cst (Strings.cat [
          valTypeToStr vt,
          ".",
          nm]))
      Syntax.InstructionRelop v0 ->
        let vt = Syntax.numericOpType v0
            nm = Syntax.numericOpName v0
        in (Serialization.cst (Strings.cat [
          valTypeToStr vt,
          ".",
          nm]))
      Syntax.InstructionConvert v0 -> Serialization.cst v0
      Syntax.InstructionCall v0 -> Serialization.cst (Strings.cat [
        "call $",
        v0])
      Syntax.InstructionCallIndirect v0 -> Serialization.spaceSep [
        Serialization.cst "call_indirect",
        (typeUseToExpr v0)]
      Syntax.InstructionBlock v0 -> blockInstructionToExpr "block" v0
      Syntax.InstructionLoop v0 -> blockInstructionToExpr "loop" v0
      Syntax.InstructionIf v0 -> ifInstructionToExpr v0
      Syntax.InstructionBr v0 -> Serialization.cst (Strings.cat [
        "br $",
        v0])
      Syntax.InstructionBrIf v0 -> Serialization.cst (Strings.cat [
        "br_if $",
        v0])
      Syntax.InstructionBrTable v0 ->
        let labels = Syntax.brTableArgsLabels v0
            def = Syntax.brTableArgsDefault v0
        in (Serialization.cst (Strings.cat [
          "br_table ",
          (Strings.intercalate " " (Lists.map (\l -> Strings.cat2 "$" l) labels)),
          " $",
          def]))
      Syntax.InstructionReturn -> Serialization.cst "return"
      Syntax.InstructionDrop -> Serialization.cst "drop"
      Syntax.InstructionSelect -> Serialization.cst "select"
      Syntax.InstructionUnreachable -> Serialization.cst "unreachable"
      Syntax.InstructionNop -> Serialization.cst "nop"
      Syntax.InstructionMemorySize -> Serialization.cst "memory.size"
      Syntax.InstructionMemoryGrow -> Serialization.cst "memory.grow"
      Syntax.InstructionRefNull v0 -> Serialization.cst (Strings.cat [
        "ref.null ",
        (refTypeToStr v0)])
      Syntax.InstructionRefIsNull -> Serialization.cst "ref.is_null"
      Syntax.InstructionRaw v0 -> Serialization.cst v0

-- | Serialize limits to WAT
limitsToExpr :: Syntax.Limits -> Ast.Expr
limitsToExpr l =

      let mn = Syntax.limitsMin l
          mx = Syntax.limitsMax l
      in (Serialization.spaceSep (Maybes.cat [
        Just (Serialization.cst (Literals.showInt32 mn)),
        (Maybes.map (\m -> Serialization.cst (Literals.showInt32 m)) mx)]))

-- | Serialize a memory definition to WAT
memoryDefToExpr :: Syntax.MemoryDef -> Ast.Expr
memoryDefToExpr m =

      let name = Syntax.memoryDefName m
          lim = Syntax.memoryDefLimits m
          nameStr = Maybes.maybe "" (\n -> Strings.cat2 " $" n) name
      in (Serialization.spaceSep [
        Serialization.cst (Strings.cat2 "(memory" nameStr),
        (limitsToExpr lim),
        (Serialization.cst ")")])

-- | Serialize a module field to a WAT expression
moduleFieldToExpr :: Syntax.ModuleField -> Ast.Expr
moduleFieldToExpr field =
    case field of
      Syntax.ModuleFieldType v0 -> typeDefToExpr v0
      Syntax.ModuleFieldFunc v0 -> funcToExpr v0
      Syntax.ModuleFieldMemory v0 -> memoryDefToExpr v0
      Syntax.ModuleFieldTable v0 -> tableDefToExpr v0
      Syntax.ModuleFieldGlobal v0 -> globalDefToExpr v0
      Syntax.ModuleFieldImport v0 -> importDefToExpr v0
      Syntax.ModuleFieldExport v0 -> exportDefToExpr v0
      Syntax.ModuleFieldData v0 -> dataSegmentToExpr v0
      Syntax.ModuleFieldElem _ -> Serialization.cst ";; elem segment (not yet supported)"
      Syntax.ModuleFieldStart v0 -> Serialization.cst (Strings.cat [
        "(start $",
        v0,
        ")"])

-- | Serialize a WebAssembly module to a WAT expression
moduleToExpr :: Syntax.Module -> Ast.Expr
moduleToExpr mod =

      let name = Syntax.moduleName mod
          fields = Syntax.moduleFields mod
          nameStr = Maybes.maybe "" (\n -> Strings.cat2 " $" n) name
          fieldExprs = Lists.map moduleFieldToExpr fields
      in (Serialization.newlineSep (Lists.concat [
        [
          Serialization.cst (Strings.cat2 "(module" nameStr)],
        (Lists.map (\fe -> Serialization.cst (Strings.cat2 "  " (Serialization.printExpr fe))) fieldExprs),
        [
          Serialization.cst ")"]]))

-- | Serialize a function parameter to WAT
paramToExpr :: Syntax.Param -> Ast.Expr
paramToExpr p =

      let name = Syntax.paramName p
          typ = Syntax.paramType p
          nameStr = Maybes.maybe "" (\n -> Strings.cat2 " $" n) name
      in (Serialization.cst (Strings.cat [
        "(param",
        nameStr,
        " ",
        (valTypeToStr typ),
        ")"]))

-- | Convert a reference type to its WAT string
refTypeToStr :: Syntax.RefType -> String
refTypeToStr rt =
    case rt of
      Syntax.RefTypeFuncref -> "funcref"
      Syntax.RefTypeExternref -> "externref"

-- | Serialize a table definition to WAT
tableDefToExpr :: Syntax.TableDef -> Ast.Expr
tableDefToExpr t =

      let name = Syntax.tableDefName t
          rt = Syntax.tableDefRefType t
          lim = Syntax.tableDefLimits t
          nameStr = Maybes.maybe "" (\n -> Strings.cat2 " $" n) name
      in (Serialization.spaceSep [
        Serialization.cst (Strings.cat2 "(table" nameStr),
        (limitsToExpr lim),
        (Serialization.cst (refTypeToStr rt)),
        (Serialization.cst ")")])

-- | Convert a string to a WAT comment
toWatComment :: String -> String
toWatComment s =
    Strings.cat [
      ";; ",
      s]

-- | Serialize a type definition to WAT
typeDefToExpr :: Syntax.TypeDef -> Ast.Expr
typeDefToExpr td =

      let name = Syntax.typeDefName td
          ft = Syntax.typeDefType td
          nameStr = Maybes.maybe "" (\n -> Strings.cat2 " $" n) name
      in (Serialization.spaceSep [
        Serialization.cst (Strings.cat2 "(type" nameStr),
        (funcTypeToExpr ft),
        (Serialization.cst ")")])

-- | Serialize a type use clause to WAT
typeUseToExpr :: Syntax.TypeUse -> Ast.Expr
typeUseToExpr tu =

      let idx = Syntax.typeUseIndex tu
          params = Syntax.typeUseParams tu
          results = Syntax.typeUseResults tu
          idxPart =
                  Maybes.map (\i -> Serialization.cst (Strings.cat [
                    "(type $",
                    i,
                    ")"])) idx
          paramParts = Lists.map paramToExpr params
          resultParts =
                  Lists.map (\r -> Serialization.cst (Strings.cat [
                    "(result ",
                    (valTypeToStr r),
                    ")"])) results
      in (Serialization.spaceSep (Maybes.cat (Lists.concat [
        [
          idxPart],
        (Lists.map (\p -> Just p) paramParts),
        (Lists.map (\r -> Just r) resultParts)])))

-- | Convert a value type to its WAT string representation
valTypeToStr :: Syntax.ValType -> String
valTypeToStr vt =
    case vt of
      Syntax.ValTypeI32 -> "i32"
      Syntax.ValTypeI64 -> "i64"
      Syntax.ValTypeF32 -> "f32"
      Syntax.ValTypeF64 -> "f64"
      Syntax.ValTypeFuncref -> "funcref"
      Syntax.ValTypeExternref -> "externref"
