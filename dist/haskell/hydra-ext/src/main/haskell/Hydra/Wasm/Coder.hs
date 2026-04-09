-- Note: this is an automatically generated file. Do not edit.

-- | WebAssembly code generator: converts Hydra type and term modules to WAT source code

module Hydra.Wasm.Coder where

import qualified Hydra.Analysis as Analysis
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Environment as Environment
import qualified Hydra.Errors as Errors
import qualified Hydra.Wasm.Serde as Serde
import qualified Hydra.Wasm.Syntax as Syntax
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Strip as Strip
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M
import qualified Data.Set as S

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

encodeApplication :: Context.Context -> t0 -> Core.Term -> Either (Context.InContext Errors.Error) [Syntax.Instruction]
encodeApplication cx g term =

      let gathered = Analysis.gatherArgs term []
          fun = Pairs.first gathered
          args = Pairs.second gathered
          dfun = Strip.deannotateTerm fun
      in (Eithers.bind (Eithers.mapList (encodeTerm cx g) args) (\encodedArgs ->
        let argInstrs = Lists.concat encodedArgs
        in case dfun of
          Core.TermVariable v0 ->
            let rawName = Core.unName v0
                lname = Formatting.convertCaseCamelToLowerSnake rawName
            in (Logic.ifElse (Lists.null (Lists.tail (Strings.splitOn "." rawName))) (Right (Lists.concat2 argInstrs [
              Syntax.InstructionLocalGet lname])) (Right (Lists.concat2 argInstrs [
              Syntax.InstructionCall lname])))
          Core.TermFunction v0 -> case v0 of
            Core.FunctionElimination v1 -> Logic.ifElse (Lists.null args) (encodeElimination cx g v1 []) (encodeElimination cx g v1 argInstrs)
            Core.FunctionLambda v1 -> encodeTerm cx g (Core.lambdaBody v1)
            _ -> Eithers.bind (encodeTerm cx g fun) (\funInstrs -> Right (Lists.concat2 argInstrs funInstrs))
          _ -> Eithers.bind (encodeTerm cx g fun) (\funInstrs -> Right (Lists.concat2 argInstrs funInstrs))))

encodeElimination :: Context.Context -> t0 -> Core.Elimination -> [Syntax.Instruction] -> Either (Context.InContext Errors.Error) [Syntax.Instruction]
encodeElimination cx g elim scrutineeInstrs =
    case elim of
      Core.EliminationRecord v0 ->
        let fname = Formatting.convertCaseCamelToLowerSnake (Core.unName (Core.projectionField v0))
        in (Right (Lists.concat2 scrutineeInstrs [
          Syntax.InstructionRaw (Strings.cat [
            ";; project field: ",
            fname])]))
      Core.EliminationUnion v0 ->
        let tname = Formatting.convertCaseCamelToLowerSnake (Names.localNameOf (Core.caseStatementTypeName v0))
            caseFields = Core.caseStatementCases v0
        in (Eithers.bind (Eithers.mapList (\cf ->
          let cfname = Formatting.convertCaseCamelToLowerSnake (Core.unName (Core.fieldName cf))
              cfterm = Core.fieldTerm cf
          in (Eithers.bind (encodeTerm cx g (Core.TermApplication (Core.Application {
            Core.applicationFunction = cfterm,
            Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))) (\armBody -> Right (cfname, armBody)))) caseFields) (\arms ->
          let armLabels = Lists.map Pairs.first arms
              endLabel = Strings.cat2 "end_" tname
              defaultLabel = Logic.ifElse (Lists.null armLabels) endLabel (Lists.last armLabels)
              innerDispatch =
                      Lists.concat2 scrutineeInstrs [
                        Syntax.InstructionBrTable (Syntax.BrTableArgs {
                          Syntax.brTableArgsLabels = armLabels,
                          Syntax.brTableArgsDefault = defaultLabel})]
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
          in (Right [
            Syntax.InstructionBlock (Syntax.BlockInstruction {
              Syntax.blockInstructionLabel = (Just endLabel),
              Syntax.blockInstructionBlockType = (Syntax.BlockTypeValue Syntax.ValTypeI32),
              Syntax.blockInstructionBody = dispatch})])))
      Core.EliminationWrap _ -> Right [
        Syntax.InstructionNop]

encodeFunction :: Context.Context -> t0 -> Core.Function -> Either (Context.InContext Errors.Error) [Syntax.Instruction]
encodeFunction cx g fun =
    case fun of
      Core.FunctionLambda v0 -> encodeTerm cx g (Core.lambdaBody v0)
      Core.FunctionElimination v0 -> encodeElimination cx g v0 []

encodeLiteral :: Core.Literal -> Syntax.Instruction
encodeLiteral lit =
    case lit of
      Core.LiteralBoolean v0 -> Syntax.InstructionConst (Syntax.ConstValueI32 (Logic.ifElse v0 1 0))
      Core.LiteralString v0 -> Syntax.InstructionRaw (Strings.cat [
        "i32.const 0 ;; string: \"",
        v0,
        "\""])
      Core.LiteralFloat v0 -> case v0 of
        Core.FloatValueFloat32 v1 -> Syntax.InstructionConst (Syntax.ConstValueF32 v1)
        Core.FloatValueFloat64 v1 -> Syntax.InstructionConst (Syntax.ConstValueF64 v1)
        Core.FloatValueBigfloat v1 -> Syntax.InstructionConst (Syntax.ConstValueF64 (Literals.bigfloatToFloat64 v1))
      Core.LiteralInteger v0 -> case v0 of
        Core.IntegerValueInt8 v1 -> Syntax.InstructionConst (Syntax.ConstValueI32 (Literals.bigintToInt32 (Literals.int8ToBigint v1)))
        Core.IntegerValueInt16 v1 -> Syntax.InstructionConst (Syntax.ConstValueI32 (Literals.bigintToInt32 (Literals.int16ToBigint v1)))
        Core.IntegerValueInt32 v1 -> Syntax.InstructionConst (Syntax.ConstValueI32 v1)
        Core.IntegerValueInt64 v1 -> Syntax.InstructionConst (Syntax.ConstValueI64 v1)
        Core.IntegerValueUint8 v1 -> Syntax.InstructionConst (Syntax.ConstValueI32 (Literals.bigintToInt32 (Literals.uint8ToBigint v1)))
        Core.IntegerValueUint16 v1 -> Syntax.InstructionConst (Syntax.ConstValueI32 (Literals.bigintToInt32 (Literals.uint16ToBigint v1)))
        Core.IntegerValueUint32 v1 -> Syntax.InstructionConst (Syntax.ConstValueI32 (Literals.bigintToInt32 (Literals.uint32ToBigint v1)))
        Core.IntegerValueUint64 v1 -> Syntax.InstructionConst (Syntax.ConstValueI64 (Literals.bigintToInt64 (Literals.uint64ToBigint v1)))
        Core.IntegerValueBigint v1 -> Syntax.InstructionConst (Syntax.ConstValueI64 (Literals.bigintToInt64 v1))

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

encodeTerm :: Context.Context -> t0 -> Core.Term -> Either (Context.InContext Errors.Error) [Syntax.Instruction]
encodeTerm cx g term =
    case term of
      Core.TermAnnotated v0 -> encodeTerm cx g (Core.annotatedTermBody v0)
      Core.TermApplication v0 -> encodeApplication cx g (Core.TermApplication v0)
      Core.TermEither v0 -> Eithers.either (\l -> Eithers.bind (encodeTerm cx g l) (\sl -> Right (Lists.concat2 [
        Syntax.InstructionConst (Syntax.ConstValueI32 0)] sl))) (\r -> Eithers.bind (encodeTerm cx g r) (\sr -> Right (Lists.concat2 [
        Syntax.InstructionConst (Syntax.ConstValueI32 1)] sr))) v0
      Core.TermFunction v0 -> encodeFunction cx g v0
      Core.TermLet v0 ->
        let bindings = Core.letBindings v0
            body = Core.letBody v0
        in (Eithers.bind (Eithers.mapList (\b ->
          let bname = Formatting.convertCaseCamelToLowerSnake (Core.unName (Core.bindingName b))
          in (Eithers.bind (encodeTerm cx g (Core.bindingTerm b)) (\bval -> Right (Lists.concat2 bval [
            Syntax.InstructionLocalSet bname])))) bindings) (\bindInstrs -> Eithers.bind (encodeTerm cx g body) (\bodyInstrs -> Right (Lists.concat2 (Lists.concat bindInstrs) bodyInstrs))))
      Core.TermList v0 -> Eithers.bind (Eithers.mapList (encodeTerm cx g) v0) (\sels -> Right (Lists.concat2 [
        Syntax.InstructionConst (Syntax.ConstValueI32 (Lists.length v0)),
        (Syntax.InstructionRaw ";; list elements follow")] (Lists.concat sels)))
      Core.TermLiteral v0 -> Right [
        encodeLiteral v0]
      Core.TermMap v0 -> Eithers.bind (Eithers.mapList (\entry -> Eithers.bind (encodeTerm cx g (Pairs.first entry)) (\k -> Eithers.bind (encodeTerm cx g (Pairs.second entry)) (\v -> Right (Lists.concat2 k v)))) (Maps.toList v0)) (\pairs -> Right (Lists.concat2 [
        Syntax.InstructionConst (Syntax.ConstValueI32 (Lists.length (Maps.toList v0))),
        (Syntax.InstructionRaw ";; map entries follow")] (Lists.concat pairs)))
      Core.TermMaybe v0 -> Maybes.cases v0 (Right [
        Syntax.InstructionConst (Syntax.ConstValueI32 0)]) (\val -> encodeTerm cx g val)
      Core.TermPair v0 -> Eithers.bind (encodeTerm cx g (Pairs.first v0)) (\fInstrs -> Eithers.bind (encodeTerm cx g (Pairs.second v0)) (\sInstrs -> Right (Lists.concat2 fInstrs sInstrs)))
      Core.TermRecord v0 ->
        let fields = Core.recordFields v0
        in (Eithers.bind (Eithers.mapList (\f -> encodeTerm cx g (Core.fieldTerm f)) fields) (\fieldInstrs -> Right (Lists.concat fieldInstrs)))
      Core.TermSet v0 -> Eithers.bind (Eithers.mapList (encodeTerm cx g) (Sets.toList v0)) (\sels -> Right (Lists.concat2 [
        Syntax.InstructionConst (Syntax.ConstValueI32 (Lists.length (Sets.toList v0))),
        (Syntax.InstructionRaw ";; set elements follow")] (Lists.concat sels)))
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fterm = Core.fieldTerm field
            dterm = Strip.deannotateTerm fterm
            isUnit =
                    case dterm of
                      Core.TermUnit -> True
                      Core.TermRecord v1 -> Lists.null (Core.recordFields v1)
                      _ -> False
        in (Logic.ifElse isUnit (Right [
          Syntax.InstructionConst (Syntax.ConstValueI32 0)]) (encodeTerm cx g fterm))
      Core.TermUnit -> Right []
      Core.TermTypeApplication v0 -> encodeTerm cx g (Core.typeApplicationTermBody v0)
      Core.TermTypeLambda v0 -> encodeTerm cx g (Core.typeLambdaBody v0)
      Core.TermVariable v0 ->
        let rawName = Core.unName v0
            lname = Formatting.convertCaseCamelToLowerSnake rawName
        in (Logic.ifElse (Lists.null (Lists.tail (Strings.splitOn "." rawName))) (Right [
          Syntax.InstructionLocalGet lname]) (Right [
          Syntax.InstructionCall lname]))
      Core.TermWrap v0 -> encodeTerm cx g (Core.wrappedTermBody v0)
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "unexpected term variant in WASM encoding")),
        Context.inContextContext = cx})

encodeTermDefinition :: Context.Context -> t0 -> Packaging.TermDefinition -> Either (Context.InContext Errors.Error) Syntax.ModuleField
encodeTermDefinition cx g tdef =

      let name = Packaging.termDefinitionName tdef
          term = Packaging.termDefinitionTerm tdef
          lname = Formatting.convertCaseCamelToLowerSnake (Core.unName name)
          typ = Maybes.maybe Core.TypeUnit Core.typeSchemeType (Packaging.termDefinitionType tdef)
          extracted = extractLambdaParams term
          paramNames = Pairs.first extracted
          innerBody = Pairs.second extracted
      in (Eithers.bind (extractParamTypes cx g typ) (\paramTypes ->
        let effectiveParamTypes =
                Logic.ifElse (Logic.and (Logic.not (Lists.null paramNames)) (Lists.null paramTypes)) (Lists.map (\_ -> Syntax.ValTypeI32) paramNames) paramTypes
            wasmParams =
                    Lists.map (\pair -> Syntax.Param {
                      Syntax.paramName = (Just (Formatting.convertCaseCamelToLowerSnake (Core.unName (Pairs.first pair)))),
                      Syntax.paramType = (Pairs.second pair)}) (Lists.zip paramNames effectiveParamTypes)
        in (Eithers.bind (encodeType cx g typ) (\resultTypes ->
          let dBody = Strip.deannotateTerm innerBody
          in (Eithers.bind (case dBody of
            Core.TermFunction v0 -> case v0 of
              Core.FunctionElimination v1 -> Logic.ifElse (Lists.null paramNames) (encodeElimination cx g v1 []) (encodeElimination cx g v1 [
                Syntax.InstructionLocalGet (Formatting.convertCaseCamelToLowerSnake (Core.unName (Lists.head paramNames)))])
              _ -> encodeTerm cx g innerBody
            _ -> encodeTerm cx g innerBody) (\bodyInstrs ->
            let referencedLocals = collectInstructionLocals bodyInstrs
                paramNameStrs = Lists.map (\pn -> Formatting.convertCaseCamelToLowerSnake (Core.unName pn)) paramNames
                undeclaredLocals = Sets.toList (Sets.difference referencedLocals (Sets.fromList paramNameStrs))
                wasmLocals =
                        Lists.map (\ln -> Syntax.FuncLocal {
                          Syntax.funcLocalName = (Just ln),
                          Syntax.funcLocalType = Syntax.ValTypeI32}) undeclaredLocals
            in (Right (Syntax.ModuleFieldFunc (Syntax.Func {
              Syntax.funcName = (Just lname),
              Syntax.funcTypeUse = Syntax.TypeUse {
                Syntax.typeUseIndex = Nothing,
                Syntax.typeUseParams = wasmParams,
                Syntax.typeUseResults = resultTypes},
              Syntax.funcLocals = wasmLocals,
              Syntax.funcBody = bodyInstrs})))))))))

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

extractLambdaParams :: Core.Term -> ([Core.Name], Core.Term)
extractLambdaParams term =

      let stripped = Strip.deannotateTerm term
      in case stripped of
        Core.TermFunction v0 -> case v0 of
          Core.FunctionLambda v1 ->
            let paramName = Core.lambdaParameter v1
                body = Core.lambdaBody v1
                inner = extractLambdaParams body
            in (Lists.cons paramName (Pairs.first inner), (Pairs.second inner))
          Core.FunctionElimination _ -> ([
            Core.Name "arg_0"], term)
          _ -> ([], term)
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

moduleToWasm :: Packaging.Module -> [Packaging.Definition] -> Context.Context -> t0 -> Either (Context.InContext Errors.Error) (M.Map String String)
moduleToWasm mod defs cx g =

      let partitioned = Environment.partitionDefinitions defs
          typeDefs = Pairs.first partitioned
          termDefs = Pairs.second partitioned
      in (Eithers.bind (Eithers.mapList (encodeTypeDefinition cx g) typeDefs) (\typeFields -> Eithers.bind (Eithers.mapList (encodeTermDefinition cx g) termDefs) (\termFields ->
        let allFields = Lists.concat2 (Lists.concat typeFields) termFields
            memField =
                    Syntax.ModuleFieldMemory (Syntax.MemoryDef {
                      Syntax.memoryDefName = (Just "memory"),
                      Syntax.memoryDefLimits = Syntax.Limits {
                        Syntax.limitsMin = 1,
                        Syntax.limitsMax = Nothing}})
            memExport =
                    Syntax.ModuleFieldExport (Syntax.ExportDef {
                      Syntax.exportDefName = "memory",
                      Syntax.exportDefDesc = (Syntax.ExportDescMemory "memory")})
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
                    Sets.fromList (Lists.map (\td -> Formatting.convertCaseCamelToLowerSnake (Core.unName (Packaging.termDefinitionName td))) termDefs)
            externalCalls = Sets.toList (Sets.difference allCallTargets localFuncNames)
            importFields =
                    Lists.map (\fname ->
                      let parts = Strings.splitOn "." fname
                          modName = Strings.intercalate "." (Lists.reverse (Lists.tail (Lists.reverse parts)))
                      in (Syntax.ModuleFieldImport (Syntax.ImportDef {
                        Syntax.importDefModule = modName,
                        Syntax.importDefName = fname,
                        Syntax.importDefDesc = (Syntax.ImportDescFunc (Syntax.ImportFunc {
                          Syntax.importFuncName = (Just fname),
                          Syntax.importFuncTypeUse = Syntax.TypeUse {
                            Syntax.typeUseIndex = Nothing,
                            Syntax.typeUseParams = [
                              Syntax.Param {
                                Syntax.paramName = Nothing,
                                Syntax.paramType = Syntax.ValTypeI32}],
                            Syntax.typeUseResults = [
                              Syntax.ValTypeI32]}}))}))) externalCalls
            wasmMod =
                    Syntax.Module {
                      Syntax.moduleName = Nothing,
                      Syntax.moduleFields = (Lists.concat [
                        importFields,
                        [
                          memField,
                          memExport],
                        funcExports,
                        allFields])}
            code = Serialization.printExpr (Serialization.parenthesize (Serde.moduleToExpr wasmMod))
            filePath =
                    Names.namespaceToFilePath Util.CaseConventionLowerSnake (Packaging.FileExtension "wat") (Packaging.moduleNamespace mod)
        in (Right (Maps.singleton filePath code)))))
