-- Note: this is an automatically generated file. Do not edit.

-- | Rust code generator: converts Hydra type and term modules to Rust source code

module Hydra.Ext.Rust.Coder where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Ext.Rust.Language as Language
import qualified Hydra.Ext.Rust.Serde as Serde
import qualified Hydra.Ext.Rust.Syntax as Syntax
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Monads as Monads
import qualified Hydra.Names as Names
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

standardDerives :: [String]
standardDerives = [
  "Clone",
  "Debug",
  "PartialEq",
  "Eq",
  "PartialOrd",
  "Ord"]

rustPath :: (String -> Syntax.Type)
rustPath name = (Syntax.TypePath_ (Syntax.TypePath {
  Syntax.typePathGlobal = False,
  Syntax.typePathSegments = [
    Syntax.PathSegment {
      Syntax.pathSegmentName = name,
      Syntax.pathSegmentArguments = Syntax.GenericArgumentsNone}]}))

rustPathSegmented :: ([String] -> Syntax.Type)
rustPathSegmented segs = (Syntax.TypePath_ (Syntax.TypePath {
  Syntax.typePathGlobal = False,
  Syntax.typePathSegments = (Lists.map (\s -> Syntax.PathSegment {
    Syntax.pathSegmentName = s,
    Syntax.pathSegmentArguments = Syntax.GenericArgumentsNone}) segs)}))

rustApply1 :: (String -> Syntax.Type -> Syntax.Type)
rustApply1 name arg = (Syntax.TypePath_ (Syntax.TypePath {
  Syntax.typePathGlobal = False,
  Syntax.typePathSegments = [
    Syntax.PathSegment {
      Syntax.pathSegmentName = name,
      Syntax.pathSegmentArguments = (Syntax.GenericArgumentsAngleBracketed (Syntax.AngleBracketedArgs {
        Syntax.angleBracketedArgsArgs = [
          Syntax.GenericArgType arg]}))}]}))

rustApply2 :: (String -> Syntax.Type -> Syntax.Type -> Syntax.Type)
rustApply2 name arg1 arg2 = (Syntax.TypePath_ (Syntax.TypePath {
  Syntax.typePathGlobal = False,
  Syntax.typePathSegments = [
    Syntax.PathSegment {
      Syntax.pathSegmentName = name,
      Syntax.pathSegmentArguments = (Syntax.GenericArgumentsAngleBracketed (Syntax.AngleBracketedArgs {
        Syntax.angleBracketedArgsArgs = [
          Syntax.GenericArgType arg1,
          (Syntax.GenericArgType arg2)]}))}]}))

rustUnit :: Syntax.Type
rustUnit = Syntax.TypeUnit

rustExprPath :: (String -> Syntax.Expression)
rustExprPath name = (Syntax.ExpressionPath (Syntax.ExprPath {
  Syntax.exprPathGlobal = False,
  Syntax.exprPathSegments = [
    Syntax.PathSegment {
      Syntax.pathSegmentName = name,
      Syntax.pathSegmentArguments = Syntax.GenericArgumentsNone}]}))

rustCall :: (Syntax.Expression -> [Syntax.Expression] -> Syntax.Expression)
rustCall fun args = (Syntax.ExpressionCall (Syntax.CallExpr {
  Syntax.callExprFunction = fun,
  Syntax.callExprArgs = args}))

rustBlock :: ([Syntax.Statement] -> Syntax.Expression -> Syntax.Expression)
rustBlock stmts expr = (Syntax.ExpressionBlock (Syntax.Block {
  Syntax.blockStatements = stmts,
  Syntax.blockExpression = (Just expr)}))

rustLetStmt :: (String -> Syntax.Expression -> Syntax.Statement)
rustLetStmt name expr = (Syntax.StatementLet (Syntax.LetStatement {
  Syntax.letStatementPattern = (Syntax.PatternIdentifier (Syntax.IdentifierPattern {
    Syntax.identifierPatternName = name,
    Syntax.identifierPatternMutable = False,
    Syntax.identifierPatternAtPattern = Nothing})),
  Syntax.letStatementMutable = False,
  Syntax.letStatementType = Nothing,
  Syntax.letStatementInit = (Just expr)}))

rustClosure :: ([String] -> Syntax.Expression -> Syntax.Expression)
rustClosure params body = (Syntax.ExpressionClosure (Syntax.ClosureExpr {
  Syntax.closureExprMove = False,
  Syntax.closureExprParams = (Lists.map (\p -> Syntax.ClosureParam {
    Syntax.closureParamPattern = (Syntax.PatternIdentifier (Syntax.IdentifierPattern {
      Syntax.identifierPatternName = p,
      Syntax.identifierPatternMutable = False,
      Syntax.identifierPatternAtPattern = Nothing})),
    Syntax.closureParamType = Nothing}) params),
  Syntax.closureExprReturnType = Nothing,
  Syntax.closureExprBody = body}))

encodeLiteralType :: (Core.LiteralType -> Syntax.Type)
encodeLiteralType lt = ((\x -> case x of
  Core.LiteralTypeBinary -> (rustApply1 "Vec" (rustPath "u8"))
  Core.LiteralTypeBoolean -> (rustPath "bool")
  Core.LiteralTypeFloat v1 -> ((\x -> case x of
    Core.FloatTypeBigfloat -> (rustPath "f64")
    Core.FloatTypeFloat32 -> (rustPath "f32")
    Core.FloatTypeFloat64 -> (rustPath "f64")) v1)
  Core.LiteralTypeInteger v1 -> ((\x -> case x of
    Core.IntegerTypeBigint -> (rustPathSegmented [
      "num",
      "BigInt"])
    Core.IntegerTypeInt8 -> (rustPath "i8")
    Core.IntegerTypeInt16 -> (rustPath "i16")
    Core.IntegerTypeInt32 -> (rustPath "i32")
    Core.IntegerTypeInt64 -> (rustPath "i64")
    Core.IntegerTypeUint8 -> (rustPath "u8")
    Core.IntegerTypeUint16 -> (rustPath "u16")
    Core.IntegerTypeUint32 -> (rustPath "u32")
    Core.IntegerTypeUint64 -> (rustPath "u64")) v1)
  Core.LiteralTypeString -> (rustPath "String")) lt)

encodeLiteral :: (Core.Literal -> Syntax.Expression)
encodeLiteral lit = ((\x -> case x of
  Core.LiteralBoolean v1 -> (Syntax.ExpressionLiteral (Syntax.LiteralBool v1))
  Core.LiteralString v1 -> (Syntax.ExpressionLiteral (Syntax.LiteralString v1))
  Core.LiteralFloat v1 -> ((\x -> case x of
    Core.FloatValueFloat32 v2 -> (Syntax.ExpressionLiteral (Syntax.LiteralFloat (Syntax.FloatLiteral {
      Syntax.floatLiteralValue = (Literals.bigfloatToFloat64 (Literals.float32ToBigfloat v2)),
      Syntax.floatLiteralSuffix = (Just "f32")})))
    Core.FloatValueFloat64 v2 -> (Syntax.ExpressionLiteral (Syntax.LiteralFloat (Syntax.FloatLiteral {
      Syntax.floatLiteralValue = v2,
      Syntax.floatLiteralSuffix = Nothing})))
    Core.FloatValueBigfloat v2 -> (Syntax.ExpressionLiteral (Syntax.LiteralFloat (Syntax.FloatLiteral {
      Syntax.floatLiteralValue = (Literals.bigfloatToFloat64 v2),
      Syntax.floatLiteralSuffix = Nothing})))) v1)
  Core.LiteralInteger v1 -> ((\x -> case x of
    Core.IntegerValueInt8 v2 -> (Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
      Syntax.integerLiteralValue = (Literals.int8ToBigint v2),
      Syntax.integerLiteralSuffix = (Just "i8")})))
    Core.IntegerValueInt16 v2 -> (Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
      Syntax.integerLiteralValue = (Literals.int16ToBigint v2),
      Syntax.integerLiteralSuffix = (Just "i16")})))
    Core.IntegerValueInt32 v2 -> (Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
      Syntax.integerLiteralValue = (Literals.int32ToBigint v2),
      Syntax.integerLiteralSuffix = (Just "i32")})))
    Core.IntegerValueInt64 v2 -> (Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
      Syntax.integerLiteralValue = (Literals.int64ToBigint v2),
      Syntax.integerLiteralSuffix = (Just "i64")})))
    Core.IntegerValueUint8 v2 -> (Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
      Syntax.integerLiteralValue = (Literals.uint8ToBigint v2),
      Syntax.integerLiteralSuffix = (Just "u8")})))
    Core.IntegerValueUint16 v2 -> (Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
      Syntax.integerLiteralValue = (Literals.uint16ToBigint v2),
      Syntax.integerLiteralSuffix = (Just "u16")})))
    Core.IntegerValueUint32 v2 -> (Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
      Syntax.integerLiteralValue = (Literals.uint32ToBigint v2),
      Syntax.integerLiteralSuffix = (Just "u32")})))
    Core.IntegerValueUint64 v2 -> (Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
      Syntax.integerLiteralValue = (Literals.uint64ToBigint v2),
      Syntax.integerLiteralSuffix = (Just "u64")})))
    Core.IntegerValueBigint v2 -> (Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
      Syntax.integerLiteralValue = v2,
      Syntax.integerLiteralSuffix = Nothing})))) v1)) lit)

encodeType :: (Core.Type -> Compute.Flow t0 Syntax.Type)
encodeType t =  
  let typ = (Rewriting.deannotateType t)
  in ((\x -> case x of
    Core.TypeAnnotated v1 -> (encodeType (Core.annotatedTypeBody v1))
    Core.TypeApplication v1 -> (encodeType (Core.applicationTypeFunction v1))
    Core.TypeUnit -> (Flows.pure rustUnit)
    Core.TypeLiteral v1 -> (Flows.pure (encodeLiteralType v1))
    Core.TypeList v1 -> (Flows.map (\enc -> rustApply1 "Vec" enc) (encodeType v1))
    Core.TypeSet v1 -> (Flows.map (\enc -> rustApply1 "BTreeSet" enc) (encodeType v1))
    Core.TypeMap v1 -> (Flows.bind (encodeType (Core.mapTypeKeys v1)) (\kt -> Flows.bind (encodeType (Core.mapTypeValues v1)) (\vt -> Flows.pure (rustApply2 "BTreeMap" kt vt))))
    Core.TypeMaybe v1 -> (Flows.map (\enc -> rustApply1 "Option" enc) (encodeType v1))
    Core.TypeEither v1 -> (Flows.bind (encodeType (Core.eitherTypeLeft v1)) (\lt -> Flows.bind (encodeType (Core.eitherTypeRight v1)) (\rt -> Flows.pure (rustApply2 "Either" lt rt))))
    Core.TypePair v1 -> (Flows.bind (encodeType (Core.pairTypeFirst v1)) (\ft -> Flows.bind (encodeType (Core.pairTypeSecond v1)) (\st -> Flows.pure (Syntax.TypeTuple [
      ft,
      st]))))
    Core.TypeFunction v1 -> (Flows.bind (encodeType (Core.functionTypeDomain v1)) (\dom -> Flows.bind (encodeType (Core.functionTypeCodomain v1)) (\cod -> Flows.pure (rustApply1 "Box" (Syntax.TypeDynTrait [
      Syntax.TypeParamBoundTrait (Syntax.TypePath {
        Syntax.typePathGlobal = False,
        Syntax.typePathSegments = [
          Syntax.PathSegment {
            Syntax.pathSegmentName = "Fn",
            Syntax.pathSegmentArguments = (Syntax.GenericArgumentsParenthesized (Syntax.ParenthesizedArgs {
              Syntax.parenthesizedArgsInputs = [
                dom],
              Syntax.parenthesizedArgsOutput = (Just cod)}))}]})])))))
    Core.TypeRecord v1 -> (Flows.pure (rustPath (Formatting.capitalize (Names.localNameOf (Core.rowTypeTypeName v1)))))
    Core.TypeUnion v1 -> (Flows.pure (rustPath (Formatting.capitalize (Names.localNameOf (Core.rowTypeTypeName v1)))))
    Core.TypeWrap v1 -> (Flows.pure (rustPath (Formatting.capitalize (Names.localNameOf (Core.wrappedTypeTypeName v1)))))
    Core.TypeVariable v1 -> (Flows.pure (rustPath (Formatting.capitalize (Core.unName v1))))
    Core.TypeForall v1 -> (encodeType (Core.forallTypeBody v1))) typ)

encodeTerm :: (Core.Term -> Compute.Flow t0 Syntax.Expression)
encodeTerm term = ((\x -> case x of
  Core.TermAnnotated v1 -> (encodeTerm (Core.annotatedTermBody v1))
  Core.TermApplication v1 -> (Flows.bind (encodeTerm (Core.applicationFunction v1)) (\fun -> Flows.bind (encodeTerm (Core.applicationArgument v1)) (\arg -> Flows.pure (rustCall fun [
    arg]))))
  Core.TermEither v1 -> (Eithers.either (\l -> Flows.bind (encodeTerm l) (\sl -> Flows.pure (rustCall (rustExprPath "Left") [
    sl]))) (\r -> Flows.bind (encodeTerm r) (\sr -> Flows.pure (rustCall (rustExprPath "Right") [
    sr]))) v1)
  Core.TermFunction v1 -> (encodeFunction v1)
  Core.TermLet v1 ->  
    let bindings = (Core.letBindings v1)
    in  
      let body = (Core.letBody v1)
      in (Flows.bind (Flows.mapList (\b ->  
        let bname = (Formatting.convertCaseCamelToLowerSnake (Core.unName (Core.bindingName b)))
        in (Flows.bind (encodeTerm (Core.bindingTerm b)) (\bval -> Flows.pure (rustLetStmt bname bval)))) bindings) (\stmts -> Flows.bind (encodeTerm body) (\bodyExpr -> Flows.pure (rustBlock stmts bodyExpr))))
  Core.TermList v1 -> (Flows.bind (Flows.mapList encodeTerm v1) (\sels -> Flows.pure (rustCall (rustExprPath "Vec::from") [
    Syntax.ExpressionArray (Syntax.ArrayExprElements sels)])))
  Core.TermLiteral v1 -> (Flows.pure (encodeLiteral v1))
  Core.TermMap v1 -> (Flows.bind (Flows.mapList (\entry -> Flows.bind (encodeTerm (Pairs.first entry)) (\k -> Flows.bind (encodeTerm (Pairs.second entry)) (\v -> Flows.pure (Syntax.ExpressionTuple [
    k,
    v])))) (Maps.toList v1)) (\pairs -> Flows.pure (rustCall (rustExprPath "BTreeMap::from") [
    Syntax.ExpressionArray (Syntax.ArrayExprElements pairs)])))
  Core.TermMaybe v1 -> (Maybes.cases v1 (Flows.pure (rustExprPath "None")) (\val -> Flows.bind (encodeTerm val) (\sval -> Flows.pure (rustCall (rustExprPath "Some") [
    sval]))))
  Core.TermPair v1 -> (Flows.bind (encodeTerm (Pairs.first v1)) (\f -> Flows.bind (encodeTerm (Pairs.second v1)) (\s -> Flows.pure (Syntax.ExpressionTuple [
    f,
    s]))))
  Core.TermRecord v1 ->  
    let rname = (Core.recordTypeName v1)
    in  
      let fields = (Core.recordFields v1)
      in (Flows.bind (Flows.mapList (\f ->  
        let fname = (Formatting.convertCaseCamelToLowerSnake (Core.unName (Core.fieldName f)))
        in (Flows.bind (encodeTerm (Core.fieldTerm f)) (\fval -> Flows.pure (Syntax.FieldValue {
          Syntax.fieldValueName = fname,
          Syntax.fieldValueValue = (Just fval)})))) fields) (\sfields -> Flows.pure (Syntax.ExpressionStruct (Syntax.StructExpr {
        Syntax.structExprPath = Syntax.ExprPath {
          Syntax.exprPathGlobal = False,
          Syntax.exprPathSegments = [
            Syntax.PathSegment {
              Syntax.pathSegmentName = (Formatting.capitalize (Names.localNameOf rname)),
              Syntax.pathSegmentArguments = Syntax.GenericArgumentsNone}]},
        Syntax.structExprFields = sfields,
        Syntax.structExprRest = Nothing}))))
  Core.TermSet v1 -> (Flows.bind (Flows.mapList encodeTerm (Sets.toList v1)) (\sels -> Flows.pure (rustCall (rustExprPath "BTreeSet::from") [
    Syntax.ExpressionArray (Syntax.ArrayExprElements sels)])))
  Core.TermUnion v1 ->  
    let tname = (Formatting.capitalize (Names.localNameOf (Core.injectionTypeName v1)))
    in  
      let field = (Core.injectionField v1)
      in  
        let fname = (Formatting.capitalize (Core.unName (Core.fieldName field)))
        in  
          let fterm = (Core.fieldTerm field)
          in  
            let dterm = (Rewriting.deannotateTerm fterm)
            in  
              let isUnit = ((\x -> case x of
                      Core.TermUnit -> True
                      Core.TermRecord v2 -> (Lists.null (Core.recordFields v2))
                      _ -> False) dterm)
              in (Logic.ifElse isUnit (Flows.pure (rustExprPath (Strings.cat2 (Strings.cat2 tname "::") fname))) (Flows.bind (encodeTerm fterm) (\sval -> Flows.pure (rustCall (rustExprPath (Strings.cat2 (Strings.cat2 tname "::") fname)) [
                sval]))))
  Core.TermUnit -> (Flows.pure (Syntax.ExpressionTuple []))
  Core.TermVariable v1 -> (Flows.pure (rustExprPath (Formatting.convertCaseCamelToLowerSnake (Formatting.sanitizeWithUnderscores Language.rustReservedWords (Core.unName v1)))))
  Core.TermWrap v1 ->  
    let tname = (Formatting.capitalize (Names.localNameOf (Core.wrappedTermTypeName v1)))
    in (Flows.bind (encodeTerm (Core.wrappedTermBody v1)) (\inner -> Flows.pure (rustCall (rustExprPath tname) [
      inner])))
  _ -> (Flows.fail "unexpected term variant")) term)

encodeFunction :: (Core.Function -> Compute.Flow t0 Syntax.Expression)
encodeFunction fun = ((\x -> case x of
  Core.FunctionLambda v1 ->  
    let param = (Formatting.convertCaseCamelToLowerSnake (Core.unName (Core.lambdaParameter v1)))
    in (Flows.bind (encodeTerm (Core.lambdaBody v1)) (\body -> Flows.pure (rustClosure [
      param] body)))
  Core.FunctionPrimitive v1 -> (Flows.pure (rustExprPath (Core.unName v1)))
  Core.FunctionElimination v1 -> (encodeElimination v1 Nothing)) fun)

encodeElimination :: (Core.Elimination -> Maybe Core.Term -> Compute.Flow t0 Syntax.Expression)
encodeElimination elim marg = ((\x -> case x of
  Core.EliminationRecord v1 ->  
    let fname = (Formatting.convertCaseCamelToLowerSnake (Core.unName (Core.projectionField v1)))
    in (Maybes.cases marg (Flows.pure (rustClosure [
      "v"] (Syntax.ExpressionFieldAccess (Syntax.FieldAccessExpr {
      Syntax.fieldAccessExprObject = (rustExprPath "v"),
      Syntax.fieldAccessExprField = fname})))) (\arg -> Flows.bind (encodeTerm arg) (\sarg -> Flows.pure (Syntax.ExpressionFieldAccess (Syntax.FieldAccessExpr {
      Syntax.fieldAccessExprObject = sarg,
      Syntax.fieldAccessExprField = fname})))))
  Core.EliminationUnion v1 ->  
    let tname = (Formatting.capitalize (Names.localNameOf (Core.caseStatementTypeName v1)))
    in  
      let caseFields = (Core.caseStatementCases v1)
      in  
        let defCase = (Core.caseStatementDefault v1)
        in (Flows.bind (Flows.mapList (\cf ->  
          let cfname = (Formatting.capitalize (Core.unName (Core.fieldName cf)))
          in  
            let cfterm = (Core.fieldTerm cf)
            in (Flows.bind (encodeTerm (Core.TermApplication (Core.Application {
              Core.applicationFunction = cfterm,
              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))) (\armBody -> Flows.pure (Syntax.MatchArm {
              Syntax.matchArmPattern = (Syntax.PatternTupleStruct (Syntax.TupleStructPattern {
                Syntax.tupleStructPatternPath = Syntax.ExprPath {
                  Syntax.exprPathGlobal = False,
                  Syntax.exprPathSegments = [
                    Syntax.PathSegment {
                      Syntax.pathSegmentName = (Strings.cat2 (Strings.cat2 tname "::") cfname),
                      Syntax.pathSegmentArguments = Syntax.GenericArgumentsNone}]},
                Syntax.tupleStructPatternElements = [
                  Syntax.PatternIdentifier (Syntax.IdentifierPattern {
                    Syntax.identifierPatternName = "v",
                    Syntax.identifierPatternMutable = False,
                    Syntax.identifierPatternAtPattern = Nothing})]})),
              Syntax.matchArmGuard = Nothing,
              Syntax.matchArmBody = armBody})))) caseFields) (\arms -> Flows.bind (Maybes.cases defCase (Flows.pure arms) (\dt -> Flows.bind (encodeTerm (Core.TermApplication (Core.Application {
          Core.applicationFunction = dt,
          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))) (\defBody -> Flows.pure (Lists.concat2 arms [
          Syntax.MatchArm {
            Syntax.matchArmPattern = Syntax.PatternWildcard,
            Syntax.matchArmGuard = Nothing,
            Syntax.matchArmBody = defBody}])))) (\allArms -> Maybes.cases marg (Flows.pure (rustClosure [
          "v"] (Syntax.ExpressionMatch (Syntax.MatchExpr {
          Syntax.matchExprScrutinee = (rustExprPath "v"),
          Syntax.matchExprArms = allArms})))) (\arg -> Flows.bind (encodeTerm arg) (\sarg -> Flows.pure (Syntax.ExpressionMatch (Syntax.MatchExpr {
          Syntax.matchExprScrutinee = sarg,
          Syntax.matchExprArms = allArms})))))))
  Core.EliminationWrap _ -> (Maybes.cases marg (Flows.pure (rustClosure [
    "v"] (Syntax.ExpressionTupleIndex (Syntax.TupleIndexExpr {
    Syntax.tupleIndexExprTuple = (rustExprPath "v"),
    Syntax.tupleIndexExprIndex = 0})))) (\arg -> Flows.bind (encodeTerm arg) (\sarg -> Flows.pure (Syntax.ExpressionTupleIndex (Syntax.TupleIndexExpr {
    Syntax.tupleIndexExprTuple = sarg,
    Syntax.tupleIndexExprIndex = 0})))))) elim)

encodeStructField :: (Core.FieldType -> Compute.Flow t0 Syntax.StructField)
encodeStructField ft =  
  let fname = (Core.unName (Core.fieldTypeName ft))
  in  
    let ftyp = (Core.fieldTypeType ft)
    in (Flows.bind (encodeType ftyp) (\sftyp -> Flows.pure (Syntax.StructField {
      Syntax.structFieldName = (Formatting.convertCaseCamelToLowerSnake (Formatting.sanitizeWithUnderscores Language.rustReservedWords fname)),
      Syntax.structFieldType = sftyp,
      Syntax.structFieldPublic = True,
      Syntax.structFieldDoc = Nothing})))

encodeEnumVariant :: (Core.FieldType -> Compute.Flow t0 Syntax.EnumVariant)
encodeEnumVariant ft =  
  let fname = (Core.unName (Core.fieldTypeName ft))
  in  
    let ftyp = (Core.fieldTypeType ft)
    in  
      let dtyp = (Rewriting.deannotateType ftyp)
      in  
        let isUnit = ((\x -> case x of
                Core.TypeUnit -> True
                Core.TypeRecord v1 -> (Lists.null (Core.rowTypeFields v1))
                _ -> False) dtyp)
        in (Logic.ifElse isUnit (Flows.pure (Syntax.EnumVariant {
          Syntax.enumVariantName = (Formatting.capitalize fname),
          Syntax.enumVariantBody = Syntax.EnumVariantBodyUnit,
          Syntax.enumVariantDoc = Nothing})) ((\x -> case x of
          Core.TypeRecord v1 -> (Flows.bind (Flows.mapList encodeStructField (Core.rowTypeFields v1)) (\sfields -> Flows.pure (Syntax.EnumVariant {
            Syntax.enumVariantName = (Formatting.capitalize fname),
            Syntax.enumVariantBody = (Syntax.EnumVariantBodyStruct sfields),
            Syntax.enumVariantDoc = Nothing})))
          _ -> (Flows.bind (encodeType ftyp) (\sftyp -> Flows.pure (Syntax.EnumVariant {
            Syntax.enumVariantName = (Formatting.capitalize fname),
            Syntax.enumVariantBody = (Syntax.EnumVariantBodyTuple [
              sftyp]),
            Syntax.enumVariantDoc = Nothing})))) dtyp))

encodeTypeDefinition :: (Module.TypeDefinition -> Compute.Flow t0 Syntax.ItemWithComments)
encodeTypeDefinition tdef =  
  let name = (Module.typeDefinitionName tdef)
  in  
    let typ = (Module.typeDefinitionType tdef)
    in  
      let lname = (Formatting.capitalize (Names.localNameOf name))
      in  
        let freeVars = (Lists.filter (\v -> Lists.null (Lists.tail (Strings.splitOn "." (Core.unName v)))) (Sets.toList (Rewriting.freeVariablesInType typ)))
        in  
          let generics = (Lists.map (\v -> Syntax.GenericParam {
                  Syntax.genericParamName = (Formatting.capitalize (Core.unName v)),
                  Syntax.genericParamBounds = []}) freeVars)
          in  
            let dtyp = (Rewriting.deannotateType typ)
            in (Monads.withTrace (Strings.cat2 "type " (Core.unName name)) (Flows.bind ((\x -> case x of
              Core.TypeRecord v1 -> (Flows.bind (Flows.mapList encodeStructField (Core.rowTypeFields v1)) (\sfields -> Flows.pure (Syntax.ItemStruct (Syntax.StructDef {
                Syntax.structDefName = lname,
                Syntax.structDefGenerics = generics,
                Syntax.structDefWhereClause = Nothing,
                Syntax.structDefBody = (Syntax.StructBodyNamed sfields),
                Syntax.structDefDerives = standardDerives,
                Syntax.structDefPublic = True,
                Syntax.structDefDoc = Nothing}))))
              Core.TypeUnion v1 -> (Flows.bind (Flows.mapList encodeEnumVariant (Core.rowTypeFields v1)) (\variants -> Flows.pure (Syntax.ItemEnum (Syntax.EnumDef {
                Syntax.enumDefName = lname,
                Syntax.enumDefGenerics = generics,
                Syntax.enumDefWhereClause = Nothing,
                Syntax.enumDefVariants = variants,
                Syntax.enumDefDerives = standardDerives,
                Syntax.enumDefPublic = True,
                Syntax.enumDefDoc = Nothing}))))
              Core.TypeWrap v1 -> (Flows.bind (encodeType (Core.wrappedTypeBody v1)) (\styp -> Flows.pure (Syntax.ItemStruct (Syntax.StructDef {
                Syntax.structDefName = lname,
                Syntax.structDefGenerics = generics,
                Syntax.structDefWhereClause = Nothing,
                Syntax.structDefBody = (Syntax.StructBodyTuple [
                  Syntax.TupleField {
                    Syntax.tupleFieldType = styp,
                    Syntax.tupleFieldPublic = True}]),
                Syntax.structDefDerives = standardDerives,
                Syntax.structDefPublic = True,
                Syntax.structDefDoc = Nothing}))))
              _ -> (Flows.bind (encodeType typ) (\styp -> Flows.pure (Syntax.ItemTypeAlias (Syntax.TypeAlias {
                Syntax.typeAliasName = lname,
                Syntax.typeAliasGenerics = generics,
                Syntax.typeAliasType = styp,
                Syntax.typeAliasPublic = True,
                Syntax.typeAliasDoc = Nothing}))))) dtyp) (\item -> Flows.pure (Syntax.ItemWithComments {
              Syntax.itemWithCommentsDoc = Nothing,
              Syntax.itemWithCommentsVisibility = Syntax.VisibilityPublic,
              Syntax.itemWithCommentsItem = item}))))

encodeTermDefinition :: (Module.TermDefinition -> Compute.Flow t0 Syntax.ItemWithComments)
encodeTermDefinition tdef =  
  let name = (Module.termDefinitionName tdef)
  in  
    let term = (Module.termDefinitionTerm tdef)
    in  
      let tscheme = (Module.termDefinitionType tdef)
      in  
        let lname = (Formatting.convertCaseCamelToLowerSnake (Names.localNameOf name))
        in  
          let typ = (Core.typeSchemeType tscheme)
          in (Monads.withTrace (Strings.cat2 "term " (Core.unName name)) (Flows.bind (encodeTerm term) (\body -> Flows.bind (encodeType typ) (\retType -> Flows.pure (Syntax.ItemWithComments {
            Syntax.itemWithCommentsDoc = Nothing,
            Syntax.itemWithCommentsVisibility = Syntax.VisibilityPublic,
            Syntax.itemWithCommentsItem = (Syntax.ItemFn (Syntax.FnDef {
              Syntax.fnDefName = lname,
              Syntax.fnDefGenerics = [],
              Syntax.fnDefWhereClause = Nothing,
              Syntax.fnDefParams = [],
              Syntax.fnDefReturnType = (Just retType),
              Syntax.fnDefBody = Syntax.Block {
                Syntax.blockStatements = [],
                Syntax.blockExpression = (Just body)},
              Syntax.fnDefPublic = True,
              Syntax.fnDefAsync = False,
              Syntax.fnDefConst = False,
              Syntax.fnDefUnsafe = False,
              Syntax.fnDefDoc = Nothing}))})))))

moduleToRust :: (Module.Module -> [Module.Definition] -> Compute.Flow t0 (M.Map String String))
moduleToRust mod defs = (Monads.withTrace (Strings.cat2 "encode Rust module: " (Module.unNamespace (Module.moduleNamespace mod))) ( 
  let partitioned = (Schemas.partitionDefinitions defs)
  in  
    let typeDefs = (Pairs.first partitioned)
    in  
      let termDefs = (Pairs.second partitioned)
      in (Flows.bind (Flows.mapList encodeTypeDefinition typeDefs) (\typeItems -> Flows.bind (Flows.mapList encodeTermDefinition termDefs) (\termItems ->  
        let allItems = (Lists.concat2 typeItems termItems)
        in  
          let crate = Syntax.Crate {
                  Syntax.crateItems = allItems}
          in  
            let code = (Serialization.printExpr (Serialization.parenthesize (Serde.crateToExpr crate)))
            in  
              let filePath = (Names.namespaceToFilePath Util.CaseConventionLowerSnake (Module.FileExtension "rs") (Module.moduleNamespace mod))
              in (Flows.pure (Maps.singleton filePath code)))))))
