-- Note: this is an automatically generated file. Do not edit.

-- | WebAssembly code generator: converts Hydra type and term modules to WAT source code

module Hydra.Wasm.Coder where

import qualified Hydra.Analysis as Analysis
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Environment as Environment
import qualified Hydra.Errors as Errors
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Strip as Strip
import qualified Hydra.Util as Util
import qualified Hydra.Wasm.Serde as Serde
import qualified Hydra.Wasm.Syntax as Syntax
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S

buildFieldOffsets :: Graph.Graph -> M.Map Core.Name [(Core.Name, Int)]
buildFieldOffsets g =

      let recordFieldsOf =
              \t ->
                let stripped = Strip.deannotateType t
                in case stripped of
                  Core.TypeRecord v0 -> Just v0
                  Core.TypeForall v0 ->
                    let innerStripped = Strip.deannotateType (Core.forallTypeBody v0)
                    in case innerStripped of
                      Core.TypeRecord v1 -> Just v1
                      _ -> Nothing
                  _ -> Nothing
          entryFor =
                  \nameSchemePair ->
                    let tname = Pairs.first nameSchemePair
                        tscheme = Pairs.second nameSchemePair
                        tbody = Core.typeSchemeType tscheme
                        mfields = recordFieldsOf tbody
                    in (Maybes.cases mfields Nothing (\fts ->
                      let namedOffsets =
                              Lists.map (\p ->
                                let i = Pairs.first p
                                    ft = Pairs.second p
                                in (Core.fieldTypeName ft, (Math.mul i 4))) (Lists.zip (Lists.foldl (\acc -> \_f -> Lists.concat2 acc [
                                Lists.length acc]) [] fts) fts)
                      in (Just (tname, namedOffsets))))
          schemaTypesList = Maps.toList (Graph.graphSchemaTypes g)
          entries = Maybes.cat (Lists.map entryFor schemaTypesList)
      in (Maps.fromList entries)

buildFunctionSignatures :: t0 -> Graph.Graph -> [Packaging.TermDefinition] -> M.Map String ([Syntax.ValType], [Syntax.ValType])
buildFunctionSignatures cx g termDefs =

      let toSigEntry =
              \nameAndScheme ->
                let nm = Pairs.first nameAndScheme
                    ts = Pairs.second nameAndScheme
                    snakeName = Formatting.convertCaseCamelToLowerSnake (Core.unName nm)
                    sigEither = extractSignature cx g (Core.typeSchemeType ts)
                in (Eithers.either (\_err -> Nothing) (\sig -> Just (snakeName, sig)) sigEither)
          primEntries =
                  Maybes.cat (Lists.map (\kv -> toSigEntry (Pairs.first kv, (Graph.primitiveType (Pairs.second kv)))) (Maps.toList (Graph.graphPrimitives g)))
          boundEntries = Maybes.cat (Lists.map (\kv -> toSigEntry kv) (Maps.toList (Graph.graphBoundTypes g)))
          localEntries =
                  Maybes.cat (Lists.map (\td -> Maybes.bind (Packaging.termDefinitionType td) (\ts -> toSigEntry (Packaging.termDefinitionName td, ts))) termDefs)
      in (Maps.fromList (Lists.concat [
        primEntries,
        boundEntries,
        localEntries]))

buildStringOffsets :: [String] -> (M.Map String Int, Int)
buildStringOffsets strs =

      let step =
              \acc -> \s ->
                let m = Pairs.first acc
                    off = Pairs.second acc
                    len = Strings.length s
                in (Maps.insert s off m, (Math.add off (Math.add 4 len)))
          final = Lists.foldl step (Maps.empty, 1024) strs
          rawEnd = Pairs.second final
          aligned = Math.mul (Maybes.fromMaybe 0 (Math.maybeDiv (Math.add rawEnd 15) 16)) 16
      in (Pairs.first final, aligned)

buildVariantIndexes :: Graph.Graph -> M.Map Core.Name [(Core.Name, Int)]
buildVariantIndexes g =

      let unionFieldsOf =
              \t ->
                let stripped = Strip.deannotateType t
                in case stripped of
                  Core.TypeUnion v0 -> Just v0
                  Core.TypeForall v0 ->
                    let innerStripped = Strip.deannotateType (Core.forallTypeBody v0)
                    in case innerStripped of
                      Core.TypeUnion v1 -> Just v1
                      _ -> Nothing
                  _ -> Nothing
          entryFor =
                  \nameSchemePair ->
                    let tname = Pairs.first nameSchemePair
                        tscheme = Pairs.second nameSchemePair
                        tbody = Core.typeSchemeType tscheme
                        mfields = unionFieldsOf tbody
                    in (Maybes.cases mfields Nothing (\fts ->
                      let namedIndexes =
                              Lists.map (\p ->
                                let i = Pairs.first p
                                    ft = Pairs.second p
                                in (Core.fieldTypeName ft, i)) (Lists.zip (Lists.foldl (\acc -> \_f -> Lists.concat2 acc [
                                Lists.length acc]) [] fts) fts)
                      in (Just (tname, namedIndexes))))
          schemaTypesList = Maps.toList (Graph.graphSchemaTypes g)
          entries = Maybes.cat (Lists.map entryFor schemaTypesList)
      in (Maps.fromList entries)

clampValTypesToI32 :: [t0] -> [Syntax.ValType]
clampValTypesToI32 vts = Lists.map (\_vt -> Syntax.ValTypeI32) vts

collectCallTargets :: [Syntax.Instruction] -> S.Set String
collectCallTargets instrs =
    Lists.foldl (\acc -> \instr -> case instr of
      Syntax.InstructionCall v0 -> Sets.insert v0 acc
      Syntax.InstructionBlock v0 -> Sets.union acc (collectCallTargets (Syntax.blockInstructionBody v0))
      Syntax.InstructionLoop v0 -> Sets.union acc (collectCallTargets (Syntax.blockInstructionBody v0))
      Syntax.InstructionIf v0 -> Sets.union (Sets.union acc (collectCallTargets (Syntax.ifInstructionThen v0))) (collectCallTargets (Syntax.ifInstructionElse v0))
      _ -> acc) Sets.empty instrs

collectInstructionLocals :: [Syntax.Instruction] -> S.Set String
collectInstructionLocals instrs =
    Lists.foldl (\acc -> \instr -> case instr of
      Syntax.InstructionLocalGet v0 -> Sets.insert v0 acc
      Syntax.InstructionLocalSet v0 -> Sets.insert v0 acc
      Syntax.InstructionLocalTee v0 -> Sets.insert v0 acc
      Syntax.InstructionBlock v0 -> Sets.union acc (collectInstructionLocals (Syntax.blockInstructionBody v0))
      Syntax.InstructionLoop v0 -> Sets.union acc (collectInstructionLocals (Syntax.blockInstructionBody v0))
      Syntax.InstructionIf v0 -> Sets.union (Sets.union acc (collectInstructionLocals (Syntax.ifInstructionThen v0))) (collectInstructionLocals (Syntax.ifInstructionElse v0))
      _ -> acc) Sets.empty instrs

collectStrings :: [Packaging.TermDefinition] -> [String]
collectStrings termDefs =

      let collectOne =
              \acc -> \t -> case t of
                Core.TermLiteral v0 -> case v0 of
                  Core.LiteralString v1 -> Sets.insert v1 acc
                  _ -> acc
                _ -> acc
          allStrings =
                  Lists.foldl (\acc -> \td -> Rewriting.foldOverTerm Coders.TraversalOrderPre collectOne acc (Packaging.termDefinitionTerm td)) Sets.empty termDefs
      in (Sets.toList allStrings)

encodeApplication :: t0 -> t1 -> M.Map String Int -> M.Map Core.Name [(Core.Name, Int)] -> M.Map Core.Name [(Core.Name, Int)] -> M.Map String ([t2], t3) -> Core.Term -> Either Errors.Error [Syntax.Instruction]
encodeApplication cx g stringOffsets fieldOffsets variantIndexes funcSigs term =

      let gathered = Analysis.gatherArgs term []
          fun = Pairs.first gathered
          args = Pairs.second gathered
          dfun = Strip.deannotateTerm fun
      in (Eithers.bind (Eithers.mapList (\a -> encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs a) args) (\realArgInstrs ->
        let flatRealArgInstrs = Lists.concat realArgInstrs
            droppedArgInstrs = Lists.concat (Lists.map (\ai -> Lists.concat2 ai [
                  Syntax.InstructionDrop]) realArgInstrs)
        in case dfun of
          Core.TermVariable v0 ->
            let rawName = Core.unName v0
                lname = Formatting.convertCaseCamelToLowerSnake rawName
            in (Logic.ifElse (Lists.null (Maybes.fromMaybe [] (Lists.maybeTail (Strings.splitOn "." rawName)))) (Right (Lists.concat [
              droppedArgInstrs,
              [
                Syntax.InstructionLocalGet lname,
                Syntax.InstructionDrop,
                (Syntax.InstructionConst (Syntax.ConstValueI32 0))]])) (
              let mSig = Maps.lookup lname funcSigs
                  callerArgCount = Lists.length args
                  calleeParamCount = Maybes.maybe callerArgCount (\sig -> Lists.length (Pairs.first sig)) mSig
                  padCount = Math.sub calleeParamCount callerArgCount
                  padInstrs =
                          Lists.concat (Lists.replicate (Logic.ifElse (Equality.gt padCount 0) padCount 0) [
                            Syntax.InstructionConst (Syntax.ConstValueI32 0)])
              in (Right (Lists.concat [
                flatRealArgInstrs,
                padInstrs,
                [
                  Syntax.InstructionCall lname]]))))
          Core.TermProject v0 -> Maybes.cases (Lists.maybeHead args) (Right [
            Syntax.InstructionConst (Syntax.ConstValueI32 0)]) (\firstArg -> Eithers.bind (encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs firstArg) (\firstArgInstrs -> encodeProjection cx g fieldOffsets v0 firstArgInstrs))
          Core.TermCases v0 -> Maybes.cases (Lists.maybeHead args) (Right [
            Syntax.InstructionConst (Syntax.ConstValueI32 0)]) (\firstArg -> Eithers.bind (encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs firstArg) (\firstArgInstrs -> encodeCases cx g stringOffsets fieldOffsets variantIndexes funcSigs v0 firstArgInstrs))
          Core.TermLambda v0 ->
            let peeled = peelLambdaApp (Core.TermLambda v0) args
                paramNames = Pairs.first peeled
                innerBody = Pairs.second peeled
                bindInstrs =
                      Lists.concat (Lists.map (\np ->
                        let pname = Formatting.convertCaseCamelToLowerSnake (Core.unName (Pairs.first np))
                            argInstrs = Pairs.second np
                        in Lists.concat2 argInstrs [Syntax.InstructionLocalSet pname])
                        (Lists.zip paramNames realArgInstrs))
                extraArgInstrs =
                      Lists.concat (Lists.map (\ai ->
                        Lists.concat2 ai [Syntax.InstructionDrop])
                        (Lists.drop (Lists.length paramNames) realArgInstrs))
            in Eithers.bind (encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs innerBody) (\bodyInstrs ->
              Right (Lists.concat [bindInstrs, extraArgInstrs, bodyInstrs]))
          _ -> Eithers.bind (encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs fun) (\funInstrs -> Right (Lists.concat [
            droppedArgInstrs,
            funInstrs,
            [
              Syntax.InstructionDrop,
              (Syntax.InstructionConst (Syntax.ConstValueI32 0))]]))))

encodeCases :: t0 -> t1 -> M.Map String Int -> M.Map Core.Name [(Core.Name, Int)] -> M.Map Core.Name [(Core.Name, Int)] -> M.Map String ([t2], t3) -> Core.CaseStatement -> [Syntax.Instruction] -> Either Errors.Error [Syntax.Instruction]
encodeCases cx g stringOffsets fieldOffsets variantIndexes funcSigs cs scrutineeInstrsRaw =

      let tname = Formatting.convertCaseCamelToLowerSnake (Names.localNameOf (Core.caseStatementTypeName cs))
          caseFields = Core.caseStatementCases cs
          scrutineeInstrs =
                  Logic.ifElse (Lists.null scrutineeInstrsRaw) [
                    Syntax.InstructionConst (Syntax.ConstValueI32 0)] scrutineeInstrsRaw
          prologue =
                  Lists.concat [
                    scrutineeInstrs,
                    [
                      Syntax.InstructionLocalSet "__rec_ptr",
                      (Syntax.InstructionLocalGet "__rec_ptr"),
                      (Syntax.InstructionLoad (Syntax.MemoryInstruction {
                        Syntax.memoryInstructionType = Syntax.ValTypeI32,
                        Syntax.memoryInstructionMemArg = Syntax.MemArg {
                          Syntax.memArgOffset = 4,
                          Syntax.memArgAlign = 2}})),
                      (Syntax.InstructionLocalSet "v"),
                      (Syntax.InstructionLocalGet "__rec_ptr"),
                      (Syntax.InstructionLoad (Syntax.MemoryInstruction {
                        Syntax.memoryInstructionType = Syntax.ValTypeI32,
                        Syntax.memoryInstructionMemArg = Syntax.MemArg {
                          Syntax.memArgOffset = 0,
                          Syntax.memArgAlign = 2}}))]]
      in (Eithers.bind (Eithers.mapList (\cf ->
        let cfname = Formatting.convertCaseCamelToLowerSnake (Core.unName (Core.fieldName cf))
            cfterm = Core.fieldTerm cf
        in (Eithers.bind (encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs (Core.TermApplication (Core.Application {
          Core.applicationFunction = cfterm,
          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))) (\armBody -> Right (cfname, armBody)))) caseFields) (\explicitArms ->
        let defaultArmLabel = "_default"
            mDefault = Core.caseStatementDefault cs
        in (Eithers.bind
          (Maybes.cases mDefault
            (Right [Syntax.InstructionConst (Syntax.ConstValueI32 0)])
            (\defTerm -> encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs defTerm))
          (\defaultArmBody ->
        let arms = Lists.concat2 explicitArms [(defaultArmLabel, defaultArmBody)]
            explicitLabelForName fname =
              Maybes.fromMaybe defaultArmLabel
                (Maybes.map Pairs.first
                  (Lists.find (\arm -> Equality.equal (Pairs.first arm) fname) explicitArms))
            typeName = Core.caseStatementTypeName cs
            mUnionVariants = Maps.lookup typeName variantIndexes
            brTableLabels =
              Maybes.cases mUnionVariants
                (Lists.map Pairs.first explicitArms)
                (\variantPairs ->
                  let sorted = Lists.sortOn Pairs.second variantPairs
                  in Lists.map (\np ->
                       let fieldName = Formatting.convertCaseCamelToLowerSnake (Core.unName (Pairs.first np))
                       in explicitLabelForName fieldName) sorted)
            endLabel = Strings.cat2 "end_" tname
            innerDispatch =
                    Lists.concat2 prologue [
                      Syntax.InstructionBrTable (Syntax.BrTableArgs {
                        Syntax.brTableArgsLabels = brTableLabels,
                        Syntax.brTableArgsDefault = defaultArmLabel})]
            dispatch =
                    Lists.foldl (\acc -> \arm ->
                      let label = Pairs.first arm
                          body = Pairs.second arm
                      in (Lists.concat [
                        [
                          Syntax.InstructionBlock (Syntax.BlockInstruction {
                            Syntax.blockInstructionLabel = (Just label),
                            Syntax.blockInstructionBlockType = Syntax.BlockTypeEmpty,
                            Syntax.blockInstructionBody = acc})],
                        body,
                        [
                          Syntax.InstructionBr endLabel]])) innerDispatch arms
        in Right [
          Syntax.InstructionBlock (Syntax.BlockInstruction {
            Syntax.blockInstructionLabel = (Just endLabel),
            Syntax.blockInstructionBlockType = (Syntax.BlockTypeValue Syntax.ValTypeI32),
            Syntax.blockInstructionBody = dispatch})]))))

encodeLiteral :: Core.Literal -> Syntax.Instruction
encodeLiteral lit =
    case lit of
      Core.LiteralBoolean v0 -> Syntax.InstructionConst (Syntax.ConstValueI32 (Logic.ifElse v0 1 0))
      Core.LiteralString _ -> Syntax.InstructionConst (Syntax.ConstValueI32 0)
      Core.LiteralFloat _ -> Syntax.InstructionConst (Syntax.ConstValueI32 0)
      Core.LiteralInteger v0 -> case v0 of
        Core.IntegerValueInt8 v1 -> Syntax.InstructionConst (Syntax.ConstValueI32 (Literals.bigintToInt32 (Literals.int8ToBigint v1)))
        Core.IntegerValueInt16 v1 -> Syntax.InstructionConst (Syntax.ConstValueI32 (Literals.bigintToInt32 (Literals.int16ToBigint v1)))
        Core.IntegerValueInt32 v1 -> Syntax.InstructionConst (Syntax.ConstValueI32 v1)
        Core.IntegerValueInt64 _ -> Syntax.InstructionConst (Syntax.ConstValueI32 0)
        Core.IntegerValueUint8 v1 -> Syntax.InstructionConst (Syntax.ConstValueI32 (Literals.bigintToInt32 (Literals.uint8ToBigint v1)))
        Core.IntegerValueUint16 v1 -> Syntax.InstructionConst (Syntax.ConstValueI32 (Literals.bigintToInt32 (Literals.uint16ToBigint v1)))
        Core.IntegerValueUint32 v1 -> Syntax.InstructionConst (Syntax.ConstValueI32 (Literals.bigintToInt32 (Literals.uint32ToBigint v1)))
        Core.IntegerValueUint64 _ -> Syntax.InstructionConst (Syntax.ConstValueI32 0)
        Core.IntegerValueBigint _ -> Syntax.InstructionConst (Syntax.ConstValueI32 0)

encodeLiteralType :: Core.LiteralType -> Syntax.ValType
encodeLiteralType lt =
    case lt of
      Core.LiteralTypeBinary -> Syntax.ValTypeI32
      Core.LiteralTypeBoolean -> Syntax.ValTypeI32
      Core.LiteralTypeFloat v0 -> case v0 of
        Core.FloatTypeBigfloat -> Syntax.ValTypeF64
        Core.FloatTypeFloat32 -> Syntax.ValTypeF32
        Core.FloatTypeFloat64 -> Syntax.ValTypeF64
      Core.LiteralTypeInteger v0 -> case v0 of
        Core.IntegerTypeBigint -> Syntax.ValTypeI64
        Core.IntegerTypeInt8 -> Syntax.ValTypeI32
        Core.IntegerTypeInt16 -> Syntax.ValTypeI32
        Core.IntegerTypeInt32 -> Syntax.ValTypeI32
        Core.IntegerTypeInt64 -> Syntax.ValTypeI64
        Core.IntegerTypeUint8 -> Syntax.ValTypeI32
        Core.IntegerTypeUint16 -> Syntax.ValTypeI32
        Core.IntegerTypeUint32 -> Syntax.ValTypeI32
        Core.IntegerTypeUint64 -> Syntax.ValTypeI64
      Core.LiteralTypeString -> Syntax.ValTypeI32

encodeProjection :: t0 -> t1 -> M.Map Core.Name [(Core.Name, Int)] -> Core.Projection -> [Syntax.Instruction] -> Either t2 [Syntax.Instruction]
encodeProjection cx g fieldOffsets proj scrutineeInstrs =

      let typeName = Core.projectionTypeName proj
          fieldName = Core.projectionField proj
          mFields = Maps.lookup typeName fieldOffsets
          mOffset =
                  Maybes.cases mFields Nothing (\pairs ->
                    let matching = Lists.filter (\p -> Equality.equal (Pairs.first p) fieldName) pairs
                    in (Maybes.map Pairs.second (Lists.maybeHead matching)))
      in (Maybes.cases mOffset (Right (Lists.concat [
        scrutineeInstrs,
        (Logic.ifElse (Lists.null scrutineeInstrs) [] [
          Syntax.InstructionDrop]),
        [
          Syntax.InstructionConst (Syntax.ConstValueI32 0)]])) (\off -> Right (Lists.concat [
        Logic.ifElse (Lists.null scrutineeInstrs) [
          Syntax.InstructionConst (Syntax.ConstValueI32 0)] scrutineeInstrs,
        [
          Syntax.InstructionLoad (Syntax.MemoryInstruction {
            Syntax.memoryInstructionType = Syntax.ValTypeI32,
            Syntax.memoryInstructionMemArg = Syntax.MemArg {
              Syntax.memArgOffset = off,
              Syntax.memArgAlign = 2}})]])))

encodeTerm :: t0 -> t1 -> M.Map String Int -> M.Map Core.Name [(Core.Name, Int)] -> M.Map Core.Name [(Core.Name, Int)] -> M.Map String ([t2], t3) -> Core.Term -> Either Errors.Error [Syntax.Instruction]
encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs term =
    case term of
      Core.TermAnnotated v0 -> encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs (Core.annotatedTermBody v0)
      Core.TermApplication v0 -> encodeApplication cx g stringOffsets fieldOffsets variantIndexes funcSigs (Core.TermApplication v0)
      Core.TermCases _ -> Right [
        Syntax.InstructionConst (Syntax.ConstValueI32 0)]
      Core.TermEither v0 ->
        let eitherTag = Eithers.either (\_lv -> 0) (\_rv -> 1) v0
            innerTerm = Eithers.either (\lv -> lv) (\rv -> rv) v0
        in (Eithers.bind (encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs innerTerm) (\payloadInstrs -> Right (Lists.concat [
          [
            Syntax.InstructionConst (Syntax.ConstValueI32 eitherTag)],
          payloadInstrs,
          [
            Syntax.InstructionConst (Syntax.ConstValueI32 8),
            (Syntax.InstructionCall "__alloc"),
            (Syntax.InstructionLocalSet "__rec_ptr"),
            (Syntax.InstructionLocalSet "__rec_scratch"),
            (Syntax.InstructionLocalGet "__rec_ptr"),
            (Syntax.InstructionLocalGet "__rec_scratch"),
            (Syntax.InstructionStore (Syntax.MemoryInstruction {
              Syntax.memoryInstructionType = Syntax.ValTypeI32,
              Syntax.memoryInstructionMemArg = Syntax.MemArg {
                Syntax.memArgOffset = 4,
                Syntax.memArgAlign = 2}})),
            (Syntax.InstructionLocalSet "__rec_scratch"),
            (Syntax.InstructionLocalGet "__rec_ptr"),
            (Syntax.InstructionLocalGet "__rec_scratch"),
            (Syntax.InstructionStore (Syntax.MemoryInstruction {
              Syntax.memoryInstructionType = Syntax.ValTypeI32,
              Syntax.memoryInstructionMemArg = Syntax.MemArg {
                Syntax.memArgOffset = 0,
                Syntax.memArgAlign = 2}})),
            (Syntax.InstructionLocalGet "__rec_ptr")]])))
      Core.TermInject v0 ->
        let typeName = Core.injectionTypeName v0
            field = Core.injectionField v0
            fieldName = Core.fieldName field
            fterm = Core.fieldTerm field
            dterm = Strip.deannotateTerm fterm
            isUnit =
                    case dterm of
                      Core.TermUnit -> True
                      Core.TermRecord v1 -> Lists.null (Core.recordFields v1)
                      _ -> False
            mVariants = Maps.lookup typeName variantIndexes
            tag =
                    Maybes.cases mVariants 0 (\pairs ->
                      let matching = Lists.filter (\p -> Equality.equal (Pairs.first p) fieldName) pairs
                      in (Maybes.cases (Lists.maybeHead matching) 0 (\p -> Pairs.second p)))
        in (Eithers.bind (Logic.ifElse isUnit (Right [
          Syntax.InstructionConst (Syntax.ConstValueI32 0)]) (encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs fterm)) (\payloadInstrs -> Right (Lists.concat [
          [
            Syntax.InstructionConst (Syntax.ConstValueI32 tag)],
          payloadInstrs,
          [
            Syntax.InstructionConst (Syntax.ConstValueI32 8),
            (Syntax.InstructionCall "__alloc"),
            (Syntax.InstructionLocalSet "__rec_ptr"),
            (Syntax.InstructionLocalSet "__rec_scratch"),
            (Syntax.InstructionLocalGet "__rec_ptr"),
            (Syntax.InstructionLocalGet "__rec_scratch"),
            (Syntax.InstructionStore (Syntax.MemoryInstruction {
              Syntax.memoryInstructionType = Syntax.ValTypeI32,
              Syntax.memoryInstructionMemArg = Syntax.MemArg {
                Syntax.memArgOffset = 4,
                Syntax.memArgAlign = 2}})),
            (Syntax.InstructionLocalSet "__rec_scratch"),
            (Syntax.InstructionLocalGet "__rec_ptr"),
            (Syntax.InstructionLocalGet "__rec_scratch"),
            (Syntax.InstructionStore (Syntax.MemoryInstruction {
              Syntax.memoryInstructionType = Syntax.ValTypeI32,
              Syntax.memoryInstructionMemArg = Syntax.MemArg {
                Syntax.memArgOffset = 0,
                Syntax.memArgAlign = 2}})),
            (Syntax.InstructionLocalGet "__rec_ptr")]])))
      Core.TermLambda v0 -> encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs (Core.lambdaBody v0)
      Core.TermLet v0 ->
        let bindings = Core.letBindings v0
            body = Core.letBody v0
        in (Eithers.bind (Eithers.mapList (\b ->
          let bname = Formatting.convertCaseCamelToLowerSnake (Core.unName (Core.bindingName b))
          in (Eithers.bind (encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs (Core.bindingTerm b)) (\bval -> Right (Lists.concat2 bval [
            Syntax.InstructionLocalSet bname])))) bindings) (\bindInstrs -> Eithers.bind (encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs body) (\bodyInstrs -> Right (Lists.concat2 (Lists.concat bindInstrs) bodyInstrs))))
      Core.TermList v0 ->
        let numElems = Lists.length v0
            listSize = Math.mul (Math.add numElems 1) 4
        in (Eithers.bind (Eithers.mapList (\el -> encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs el) v0) (\encodedElems ->
          let allElemInstrs = Lists.concat encodedElems
              allocInstrs =
                      [
                        Syntax.InstructionConst (Syntax.ConstValueI32 listSize),
                        (Syntax.InstructionCall "__alloc"),
                        (Syntax.InstructionLocalSet "__rec_ptr")]
              lengthStoreInstrs =
                      [
                        Syntax.InstructionLocalGet "__rec_ptr",
                        (Syntax.InstructionConst (Syntax.ConstValueI32 numElems)),
                        (Syntax.InstructionStore (Syntax.MemoryInstruction {
                          Syntax.memoryInstructionType = Syntax.ValTypeI32,
                          Syntax.memoryInstructionMemArg = Syntax.MemArg {
                            Syntax.memArgOffset = 0,
                            Syntax.memArgAlign = 2}}))]
              storeInstrs =
                      Lists.concat (Lists.map (\i -> [
                        Syntax.InstructionLocalSet "__rec_scratch",
                        (Syntax.InstructionLocalGet "__rec_ptr"),
                        (Syntax.InstructionLocalGet "__rec_scratch"),
                        (Syntax.InstructionStore (Syntax.MemoryInstruction {
                          Syntax.memoryInstructionType = Syntax.ValTypeI32,
                          Syntax.memoryInstructionMemArg = Syntax.MemArg {
                            Syntax.memArgOffset = (Math.mul (Math.add i 1) 4),
                            Syntax.memArgAlign = 2}}))]) (Lists.foldl (\acc -> \_e -> Lists.cons (Lists.length acc) acc) [] v0))
              finalInstrs = [
                    Syntax.InstructionLocalGet "__rec_ptr"]
          in (Right (Lists.concat [
            allElemInstrs,
            allocInstrs,
            lengthStoreInstrs,
            storeInstrs,
            finalInstrs]))))
      Core.TermLiteral v0 -> case v0 of
        Core.LiteralString v1 -> Maybes.maybe (Right [
          Syntax.InstructionConst (Syntax.ConstValueI32 0)]) (\off -> Right [
          Syntax.InstructionConst (Syntax.ConstValueI32 off)]) (Maps.lookup v1 stringOffsets)
        _ -> Right [
          encodeLiteral v0]
      Core.TermMap v0 ->
        let mapEntries = Maps.toList v0
            numMapEntries = Lists.length mapEntries
            mapWordCount = Math.add (Math.mul numMapEntries 2) 1
            mapSize = Math.mul mapWordCount 4
        in (Eithers.bind (Eithers.mapList (\kv -> Eithers.bind (encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs (Pairs.first kv)) (\kInstrs -> Eithers.map (\vInstrs -> Lists.concat2 kInstrs vInstrs) (encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs (Pairs.second kv)))) mapEntries) (\encodedMapKVs ->
          let allMapKVInstrs = Lists.concat encodedMapKVs
              mapAllocInstrs =
                      [
                        Syntax.InstructionConst (Syntax.ConstValueI32 mapSize),
                        (Syntax.InstructionCall "__alloc"),
                        (Syntax.InstructionLocalSet "__rec_ptr")]
              mapLengthStoreInstrs =
                      [
                        Syntax.InstructionLocalGet "__rec_ptr",
                        (Syntax.InstructionConst (Syntax.ConstValueI32 numMapEntries)),
                        (Syntax.InstructionStore (Syntax.MemoryInstruction {
                          Syntax.memoryInstructionType = Syntax.ValTypeI32,
                          Syntax.memoryInstructionMemArg = Syntax.MemArg {
                            Syntax.memArgOffset = 0,
                            Syntax.memArgAlign = 2}}))]
              numMapWords = Math.mul numMapEntries 2
              mapReverseIndices = Lists.foldl (\acc -> \_i -> Lists.cons (Lists.length acc) acc) [] (Lists.replicate numMapWords ())
              mapStoreInstrs =
                      Lists.concat (Lists.map (\j -> [
                        Syntax.InstructionLocalSet "__rec_scratch",
                        (Syntax.InstructionLocalGet "__rec_ptr"),
                        (Syntax.InstructionLocalGet "__rec_scratch"),
                        (Syntax.InstructionStore (Syntax.MemoryInstruction {
                          Syntax.memoryInstructionType = Syntax.ValTypeI32,
                          Syntax.memoryInstructionMemArg = Syntax.MemArg {
                            Syntax.memArgOffset = (Math.mul (Math.add j 1) 4),
                            Syntax.memArgAlign = 2}}))]) mapReverseIndices)
              mapFinalInstrs = [
                    Syntax.InstructionLocalGet "__rec_ptr"]
          in (Right (Lists.concat [
            allMapKVInstrs,
            mapAllocInstrs,
            mapLengthStoreInstrs,
            mapStoreInstrs,
            mapFinalInstrs]))))
      Core.TermMaybe v0 -> Maybes.cases v0 (Right [
        Syntax.InstructionConst (Syntax.ConstValueI32 0)]) (\val -> encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs val)
      Core.TermPair v0 -> Eithers.bind (encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs (Pairs.first v0)) (\firstInstrs -> Eithers.bind (encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs (Pairs.second v0)) (\secondInstrs -> Right (Lists.concat [
        firstInstrs,
        secondInstrs,
        [
          Syntax.InstructionConst (Syntax.ConstValueI32 8),
          (Syntax.InstructionCall "__alloc"),
          (Syntax.InstructionLocalSet "__rec_ptr"),
          (Syntax.InstructionLocalSet "__rec_scratch"),
          (Syntax.InstructionLocalGet "__rec_ptr"),
          (Syntax.InstructionLocalGet "__rec_scratch"),
          (Syntax.InstructionStore (Syntax.MemoryInstruction {
            Syntax.memoryInstructionType = Syntax.ValTypeI32,
            Syntax.memoryInstructionMemArg = Syntax.MemArg {
              Syntax.memArgOffset = 4,
              Syntax.memArgAlign = 2}})),
          (Syntax.InstructionLocalSet "__rec_scratch"),
          (Syntax.InstructionLocalGet "__rec_ptr"),
          (Syntax.InstructionLocalGet "__rec_scratch"),
          (Syntax.InstructionStore (Syntax.MemoryInstruction {
            Syntax.memoryInstructionType = Syntax.ValTypeI32,
            Syntax.memoryInstructionMemArg = Syntax.MemArg {
              Syntax.memArgOffset = 0,
              Syntax.memArgAlign = 2}})),
          (Syntax.InstructionLocalGet "__rec_ptr")]])))
      Core.TermProject _ -> Right [
        Syntax.InstructionConst (Syntax.ConstValueI32 0)]
      Core.TermRecord v0 ->
        let recFields = Core.recordFields v0
            numFields = Lists.length recFields
            recSize = Math.mul numFields 4
        in (Eithers.bind (Eithers.mapList (\fld -> encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs (Core.fieldTerm fld)) recFields) (\encodedFields ->
          let allFieldInstrs = Lists.concat encodedFields
              allocInstrs =
                      [
                        Syntax.InstructionConst (Syntax.ConstValueI32 recSize),
                        (Syntax.InstructionCall "__alloc"),
                        (Syntax.InstructionLocalSet "__rec_ptr")]
              storeInstrs =
                      Lists.concat (Lists.map (\i -> [
                        Syntax.InstructionLocalSet "__rec_scratch",
                        (Syntax.InstructionLocalGet "__rec_ptr"),
                        (Syntax.InstructionLocalGet "__rec_scratch"),
                        (Syntax.InstructionStore (Syntax.MemoryInstruction {
                          Syntax.memoryInstructionType = Syntax.ValTypeI32,
                          Syntax.memoryInstructionMemArg = Syntax.MemArg {
                            Syntax.memArgOffset = (Math.mul i 4),
                            Syntax.memArgAlign = 2}}))]) (Lists.foldl (\acc -> \_f -> Lists.cons (Lists.length acc) acc) [] recFields))
              finalInstrs = [
                    Syntax.InstructionLocalGet "__rec_ptr"]
          in (Right (Lists.concat [
            allFieldInstrs,
            allocInstrs,
            storeInstrs,
            finalInstrs]))))
      Core.TermSet v0 ->
        let setElems = Sets.toList v0
            numSetElems = Lists.length setElems
            setSize = Math.mul (Math.add numSetElems 1) 4
        in (Eithers.bind (Eithers.mapList (\el -> encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs el) setElems) (\encodedSetElems ->
          let allSetElemInstrs = Lists.concat encodedSetElems
              setAllocInstrs =
                      [
                        Syntax.InstructionConst (Syntax.ConstValueI32 setSize),
                        (Syntax.InstructionCall "__alloc"),
                        (Syntax.InstructionLocalSet "__rec_ptr")]
              setLengthStoreInstrs =
                      [
                        Syntax.InstructionLocalGet "__rec_ptr",
                        (Syntax.InstructionConst (Syntax.ConstValueI32 numSetElems)),
                        (Syntax.InstructionStore (Syntax.MemoryInstruction {
                          Syntax.memoryInstructionType = Syntax.ValTypeI32,
                          Syntax.memoryInstructionMemArg = Syntax.MemArg {
                            Syntax.memArgOffset = 0,
                            Syntax.memArgAlign = 2}}))]
              setStoreInstrs =
                      Lists.concat (Lists.map (\i -> [
                        Syntax.InstructionLocalSet "__rec_scratch",
                        (Syntax.InstructionLocalGet "__rec_ptr"),
                        (Syntax.InstructionLocalGet "__rec_scratch"),
                        (Syntax.InstructionStore (Syntax.MemoryInstruction {
                          Syntax.memoryInstructionType = Syntax.ValTypeI32,
                          Syntax.memoryInstructionMemArg = Syntax.MemArg {
                            Syntax.memArgOffset = (Math.mul (Math.add i 1) 4),
                            Syntax.memArgAlign = 2}}))]) (Lists.foldl (\acc -> \_e -> Lists.cons (Lists.length acc) acc) [] setElems))
              setFinalInstrs = [
                    Syntax.InstructionLocalGet "__rec_ptr"]
          in (Right (Lists.concat [
            allSetElemInstrs,
            setAllocInstrs,
            setLengthStoreInstrs,
            setStoreInstrs,
            setFinalInstrs]))))
      Core.TermTypeApplication v0 -> encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs (Core.typeApplicationTermBody v0)
      Core.TermTypeLambda v0 -> encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs (Core.typeLambdaBody v0)
      Core.TermUnit -> Right [
        Syntax.InstructionConst (Syntax.ConstValueI32 0)]
      Core.TermUnwrap _ -> Right [
        Syntax.InstructionConst (Syntax.ConstValueI32 0)]
      Core.TermVariable v0 ->
        let rawName = Core.unName v0
            lname = Formatting.convertCaseCamelToLowerSnake rawName
        in (Logic.ifElse (Lists.null (Maybes.fromMaybe [] (Lists.maybeTail (Strings.splitOn "." rawName)))) (Right [
          Syntax.InstructionLocalGet lname]) (Right [
          Syntax.InstructionConst (Syntax.ConstValueI32 0)]))
      Core.TermWrap v0 -> encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs (Core.wrappedTermBody v0)
      _ -> Left (Errors.ErrorOther (Errors.OtherError "unexpected term variant in WASM encoding"))

encodeTermDefinition :: t0 -> t1 -> M.Map String Int -> M.Map Core.Name [(Core.Name, Int)] -> M.Map Core.Name [(Core.Name, Int)] -> M.Map String ([t2], t3) -> Packaging.TermDefinition -> Either Errors.Error Syntax.ModuleField
encodeTermDefinition cx g stringOffsets fieldOffsets variantIndexes funcSigs tdef =

      let name = Packaging.termDefinitionName tdef
          term = Packaging.termDefinitionTerm tdef
          lname = Formatting.convertCaseCamelToLowerSnake (Core.unName name)
          typ = Maybes.maybe Core.TypeUnit Core.typeSchemeType (Packaging.termDefinitionType tdef)
          extracted = extractLambdaParams term
          paramNames = Pairs.first extracted
          innerBody = Pairs.second extracted
          lambdaParamNameStrs = Lists.map (\pn -> Formatting.convertCaseCamelToLowerSnake (Core.unName pn)) paramNames
      in (Eithers.bind (extractParamTypes cx g typ) (\typeParams ->
        let typeParamCount = Lists.length typeParams
            lambdaParamCount = Lists.length lambdaParamNameStrs
            syntheticCount = if typeParamCount > lambdaParamCount then typeParamCount - lambdaParamCount else 0
            syntheticParamNames = Lists.map (\i -> Strings.cat2 "arg_synth_" (Literals.showInt32 i)) [0..syntheticCount - 1]
            paramNameStrs = Lists.concat2 lambdaParamNameStrs syntheticParamNames
            wasmParams =
                    Lists.map (\pn -> Syntax.Param {
                      Syntax.paramName = (Just pn),
                      Syntax.paramType = Syntax.ValTypeI32}) paramNameStrs
            initPrologue = Lists.concat (Lists.map (\sn -> [
                    Syntax.InstructionLocalGet sn,
                    Syntax.InstructionDrop]) syntheticParamNames)
            resultTypes = [
                  Syntax.ValTypeI32]
            dBody = Strip.deannotateTerm innerBody
            scrutineeInstrs = Maybes.cases (Lists.maybeHead paramNameStrs) [] (\p0 -> [
                  Syntax.InstructionLocalGet p0])
        in (Eithers.bind (case dBody of
          Core.TermProject v0 -> encodeProjection cx g fieldOffsets v0 scrutineeInstrs
          Core.TermCases v0 -> encodeCases cx g stringOffsets fieldOffsets variantIndexes funcSigs v0 scrutineeInstrs
          Core.TermUnwrap _ -> Right [
            Syntax.InstructionConst (Syntax.ConstValueI32 0)]
          _ -> encodeTerm cx g stringOffsets fieldOffsets variantIndexes funcSigs innerBody) (\rawBodyInstrs ->
          let bodyInstrs = Lists.concat2 initPrologue rawBodyInstrs
              referencedLocals = collectInstructionLocals bodyInstrs
              allLocalNames = Sets.toList (Sets.difference referencedLocals (Sets.fromList paramNameStrs))
              wasmLocals =
                      Lists.map (\ln -> Syntax.FuncLocal {
                        Syntax.funcLocalName = (Just ln),
                        Syntax.funcLocalType = Syntax.ValTypeI32}) allLocalNames
          in (Right (Syntax.ModuleFieldFunc (Syntax.Func {
            Syntax.funcName = (Just lname),
            Syntax.funcTypeUse = Syntax.TypeUse {
              Syntax.typeUseIndex = Nothing,
              Syntax.typeUseParams = wasmParams,
              Syntax.typeUseResults = resultTypes},
            Syntax.funcLocals = wasmLocals,
            Syntax.funcBody = bodyInstrs})))))))

encodeType :: t0 -> t1 -> Core.Type -> Either t2 [Syntax.ValType]
encodeType cx g t =

      let typ = Strip.deannotateType t
      in case typ of
        Core.TypeAnnotated v0 -> encodeType cx g (Core.annotatedTypeBody v0)
        Core.TypeFunction v0 -> encodeType cx g (Core.functionTypeCodomain v0)
        Core.TypeUnit -> Right []
        Core.TypeVoid -> Right []
        Core.TypeLiteral v0 -> Right [
          encodeLiteralType v0]
        Core.TypeForall v0 -> encodeType cx g (Core.forallTypeBody v0)
        _ -> Right [
          Syntax.ValTypeI32]

encodeTypeDefinition :: t0 -> t1 -> Packaging.TypeDefinition -> Either t2 [Syntax.ModuleField]
encodeTypeDefinition cx g tdef =

      let name = Packaging.typeDefinitionName tdef
          lname = Formatting.convertCaseCamelToLowerSnake (Names.localNameOf name)
          typ = Core.typeSchemeType (Packaging.typeDefinitionType tdef)
          dtyp = Strip.deannotateType typ
      in case dtyp of
        Core.TypeFunction _ -> Eithers.bind (extractParamTypes cx g typ) (\paramTypes -> Eithers.bind (encodeType cx g typ) (\resultTypes -> Right [
          Syntax.ModuleFieldType (Syntax.TypeDef {
            Syntax.typeDefName = (Just lname),
            Syntax.typeDefType = Syntax.FuncType {
              Syntax.funcTypeParams = paramTypes,
              Syntax.funcTypeResults = resultTypes}})]))
        _ -> Right []

encodeValType :: t0 -> t1 -> Core.Type -> Either t2 Syntax.ValType
encodeValType cx g t =

      let typ = Strip.deannotateType t
      in case typ of
        Core.TypeAnnotated v0 -> encodeValType cx g (Core.annotatedTypeBody v0)
        Core.TypeApplication v0 -> encodeValType cx g (Core.applicationTypeFunction v0)
        Core.TypeLiteral v0 -> Right (encodeLiteralType v0)
        Core.TypeUnit -> Right Syntax.ValTypeI32
        Core.TypeVoid -> Right Syntax.ValTypeI32
        Core.TypeForall v0 -> encodeValType cx g (Core.forallTypeBody v0)
        _ -> Right Syntax.ValTypeI32

peelLambdaApp :: Core.Term -> [Core.Term] -> ([Core.Name], Core.Term)
peelLambdaApp term args =
    if Lists.null args
      then ([], term)
      else
        let stripped = Strip.deannotateTerm term
        in case stripped of
          Core.TermLambda v0 ->
            let paramName = Core.lambdaParameter v0
                body = Core.lambdaBody v0
                restArgs = Maybes.fromMaybe [] (Lists.maybeTail args)
                inner = peelLambdaApp body restArgs
            in (Lists.cons paramName (Pairs.first inner), Pairs.second inner)
          _ -> ([], term)

extractLambdaParams :: Core.Term -> ([Core.Name], Core.Term)
extractLambdaParams term =

      let stripped = Strip.deannotateTerm term
      in case stripped of
        Core.TermLambda v0 ->
          let paramName = Core.lambdaParameter v0
              body = Core.lambdaBody v0
              inner = extractLambdaParams body
          in (Lists.cons paramName (Pairs.first inner), (Pairs.second inner))
        Core.TermCases _ -> ([
          Core.Name "arg_0"], term)
        Core.TermProject _ -> ([
          Core.Name "arg_0"], term)
        Core.TermUnwrap _ -> ([
          Core.Name "arg_0"], term)
        Core.TermTypeLambda v0 -> extractLambdaParams (Core.typeLambdaBody v0)
        Core.TermTypeApplication v0 -> extractLambdaParams (Core.typeApplicationTermBody v0)
        _ -> ([], term)

extractParamTypes :: t0 -> t1 -> Core.Type -> Either t2 [Syntax.ValType]
extractParamTypes cx g t =

      let typ = Strip.deannotateType t
      in case typ of
        Core.TypeFunction v0 -> Eithers.bind (encodeValType cx g (Core.functionTypeDomain v0)) (\domType -> Eithers.bind (extractParamTypes cx g (Core.functionTypeCodomain v0)) (\rest -> Right (Lists.cons domType rest)))
        Core.TypeForall v0 -> extractParamTypes cx g (Core.forallTypeBody v0)
        _ -> Right []

extractSignature :: t0 -> t1 -> Core.Type -> Either t2 ([Syntax.ValType], [Syntax.ValType])
extractSignature cx g t =
    Eithers.bind (extractParamTypes cx g t) (\params -> Eithers.bind (encodeType cx g t) (\results -> Right (clampValTypesToI32 params, (clampValTypesToI32 results))))

hexEscapeString :: Int -> String
hexEscapeString b =

      let byte = Maybes.fromMaybe 0 (Math.maybeMod b 256)
          digitToHex =
                  \d -> Logic.ifElse (Equality.lt d 10) (Strings.fromList [
                    Math.add d 48]) (Strings.fromList [
                    Math.add d 87])
      in (Strings.cat [
        "\\",
        (digitToHex (Maybes.fromMaybe 0 (Math.maybeDiv byte 16))),
        (digitToHex (Maybes.fromMaybe 0 (Math.maybeMod byte 16)))])

moduleToWasm :: Packaging.Module -> [Packaging.Definition] -> t0 -> Graph.Graph -> Either Errors.Error (M.Map String String)
moduleToWasm mod defs cx g =

      let partitioned = Environment.partitionDefinitions defs
          typeDefs = Pairs.first partitioned
          termDefs = Pairs.second partitioned
          stringList = collectStrings termDefs
          stringOffsetsAndEnd = buildStringOffsets stringList
          stringOffsets = Pairs.first stringOffsetsAndEnd
          stringEnd = Pairs.second stringOffsetsAndEnd
          dataField = stringDataSegment stringOffsets
          fieldOffsets = buildFieldOffsets g
          variantIndexes = buildVariantIndexes g
          funcSigs = buildFunctionSignatures cx g termDefs
      in (Eithers.bind (Eithers.mapList (encodeTypeDefinition cx g) typeDefs) (\typeFields -> Eithers.bind (Eithers.mapList (encodeTermDefinition cx g stringOffsets fieldOffsets variantIndexes funcSigs) termDefs) (\termFields ->
        let allFields = Lists.concat2 (Lists.concat typeFields) termFields
            bumpGlobal =
                    Syntax.ModuleFieldGlobal (Syntax.GlobalDef {
                      Syntax.globalDefName = (Just "__bump_ptr"),
                      Syntax.globalDefType = Syntax.GlobalType {
                        Syntax.globalTypeValType = Syntax.ValTypeI32,
                        Syntax.globalTypeMutable = True},
                      Syntax.globalDefInit = [
                        Syntax.InstructionConst (Syntax.ConstValueI32 stringEnd)]})
            allocFunc =
                    Syntax.ModuleFieldFunc (Syntax.Func {
                      Syntax.funcName = (Just "__alloc"),
                      Syntax.funcTypeUse = Syntax.TypeUse {
                        Syntax.typeUseIndex = Nothing,
                        Syntax.typeUseParams = [
                          Syntax.Param {
                            Syntax.paramName = (Just "sz"),
                            Syntax.paramType = Syntax.ValTypeI32}],
                        Syntax.typeUseResults = [
                          Syntax.ValTypeI32]},
                      Syntax.funcLocals = [],
                      Syntax.funcBody = [
                        Syntax.InstructionGlobalGet "__bump_ptr",
                        (Syntax.InstructionGlobalGet "__bump_ptr"),
                        (Syntax.InstructionLocalGet "sz"),
                        (Syntax.InstructionBinop (Syntax.NumericOp {
                          Syntax.numericOpType = Syntax.ValTypeI32,
                          Syntax.numericOpName = "add"})),
                        (Syntax.InstructionGlobalSet "__bump_ptr")]})
            memField =
                    Syntax.ModuleFieldMemory (Syntax.MemoryDef {
                      Syntax.memoryDefName = (Just "memory"),
                      Syntax.memoryDefLimits = Syntax.Limits {
                        Syntax.limitsMin = 2,
                        Syntax.limitsMax = Nothing}})
            memExport =
                    Syntax.ModuleFieldExport (Syntax.ExportDef {
                      Syntax.exportDefName = "memory",
                      Syntax.exportDefDesc = (Syntax.ExportDescMemory "memory")})
            bumpExport =
                    Syntax.ModuleFieldExport (Syntax.ExportDef {
                      Syntax.exportDefName = "__bump_ptr",
                      Syntax.exportDefDesc = (Syntax.ExportDescGlobal "__bump_ptr")})
            allocExport =
                    Syntax.ModuleFieldExport (Syntax.ExportDef {
                      Syntax.exportDefName = "__alloc",
                      Syntax.exportDefDesc = (Syntax.ExportDescFunc "__alloc")})
            funcExports =
                    Lists.map (\td ->
                      let ename = Formatting.convertCaseCamelToLowerSnake (Core.unName (Packaging.termDefinitionName td))
                      in (Syntax.ModuleFieldExport (Syntax.ExportDef {
                        Syntax.exportDefName = ename,
                        Syntax.exportDefDesc = (Syntax.ExportDescFunc ename)}))) termDefs
            allBodyInstrs =
                    Lists.concat (Lists.map (\tf -> case tf of
                      Syntax.ModuleFieldFunc v0 -> Syntax.funcBody v0
                      _ -> []) termFields)
            allCallTargets = collectCallTargets allBodyInstrs
            localFuncNames =
                    Sets.insert "__alloc" (Sets.fromList (Lists.map (\td -> Formatting.convertCaseCamelToLowerSnake (Core.unName (Packaging.termDefinitionName td))) termDefs))
            externalCalls = Sets.toList (Sets.difference allCallTargets localFuncNames)
            defaultSig = ([
                  Syntax.ValTypeI32], [
                  Syntax.ValTypeI32])
            importFields =
                    Lists.map (\fname ->
                      let parts = Strings.splitOn "." fname
                          modName = Strings.intercalate "." (Lists.reverse (Maybes.fromMaybe [] (Lists.maybeTail (Lists.reverse parts))))
                          sig = Maybes.fromMaybe defaultSig (Maps.lookup fname funcSigs)
                          sigParams = Pairs.first sig
                          sigResults = Pairs.second sig
                          wasmImportParams =
                                  Lists.map (\vt -> Syntax.Param {
                                    Syntax.paramName = Nothing,
                                    Syntax.paramType = vt}) sigParams
                      in (Syntax.ModuleFieldImport (Syntax.ImportDef {
                        Syntax.importDefModule = modName,
                        Syntax.importDefName = fname,
                        Syntax.importDefDesc = (Syntax.ImportDescFunc (Syntax.ImportFunc {
                          Syntax.importFuncName = (Just fname),
                          Syntax.importFuncTypeUse = Syntax.TypeUse {
                            Syntax.typeUseIndex = Nothing,
                            Syntax.typeUseParams = wasmImportParams,
                            Syntax.typeUseResults = sigResults}}))}))) externalCalls
            wasmMod =
                    Syntax.Module {
                      Syntax.moduleName = Nothing,
                      Syntax.moduleFields = (Lists.concat [
                        importFields,
                        [
                          memField,
                          memExport,
                          dataField,
                          bumpGlobal,
                          bumpExport,
                          allocFunc,
                          allocExport],
                        funcExports,
                        allFields])}
            code = Serialization.printExpr (Serialization.parenthesize (Serde.moduleToExpr wasmMod))
            filePath =
                    Names.namespaceToFilePath Util.CaseConventionLowerSnake (Packaging.FileExtension "wat") (Packaging.moduleNamespace mod)
        in (Right (Maps.singleton filePath code)))))

stringDataSegment :: Ord t0 => (M.Map String t0 -> Syntax.ModuleField)
stringDataSegment offsets =

      let entries = Lists.sortOn Pairs.second (Maps.toList offsets)
          bytesForEntry =
                  \entry ->
                    let s = Pairs.first entry
                        len = Strings.length s
                        lenBytes =
                                [
                                  Maybes.fromMaybe 0 (Math.maybeMod len 256),
                                  (Maybes.fromMaybe 0 (Math.maybeMod (Maybes.fromMaybe 0 (Math.maybeDiv len 256)) 256)),
                                  (Maybes.fromMaybe 0 (Math.maybeMod (Maybes.fromMaybe 0 (Math.maybeDiv len 65536)) 256)),
                                  (Maybes.fromMaybe 0 (Math.maybeMod (Maybes.fromMaybe 0 (Math.maybeDiv len 16777216)) 256))]
                        contentBytes = Strings.toList s
                        allBytes = Lists.concat2 lenBytes contentBytes
                    in (Strings.cat (Lists.map (\b -> hexEscapeString b) allBytes))
          allHex = Strings.cat (Lists.map bytesForEntry entries)
      in (Syntax.ModuleFieldData (Syntax.DataSegment {
        Syntax.dataSegmentName = Nothing,
        Syntax.dataSegmentMode = (Syntax.DataModeActive [
          Syntax.InstructionConst (Syntax.ConstValueI32 1024)]),
        Syntax.dataSegmentBytes = allHex}))
