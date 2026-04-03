-- Note: this is an automatically generated file. Do not edit.

-- | Rust code generator: converts Hydra type and term modules to Rust source code

module Hydra.Ext.Rust.Coder where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Environment as Environment
import qualified Hydra.Errors as Errors
import qualified Hydra.Ext.Rust.Language as Language
import qualified Hydra.Ext.Rust.Serde as Serde
import qualified Hydra.Ext.Rust.Syntax as Syntax
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
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Strip as Strip
import qualified Hydra.Util as Util
import qualified Hydra.Variables as Variables
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

encodeElimination :: Context.Context -> t0 -> Core.Elimination -> Maybe Core.Term -> Either (Context.InContext Errors.Error) Syntax.Expression
encodeElimination cx g elim marg =
    case elim of
      Core.EliminationRecord v0 ->
        let fname = Formatting.convertCaseCamelToLowerSnake (Core.unName (Core.projectionField v0))
        in (Maybes.cases marg (Right (rustClosure [
          "v"] (Syntax.ExpressionFieldAccess (Syntax.FieldAccessExpr {
          Syntax.fieldAccessExprObject = (rustExprPath "v"),
          Syntax.fieldAccessExprField = fname})))) (\arg -> Eithers.bind (encodeTerm cx g arg) (\sarg -> Right (Syntax.ExpressionFieldAccess (Syntax.FieldAccessExpr {
          Syntax.fieldAccessExprObject = sarg,
          Syntax.fieldAccessExprField = fname})))))
      Core.EliminationUnion v0 ->
        let tname = Formatting.capitalize (Names.localNameOf (Core.caseStatementTypeName v0))
            caseFields = Core.caseStatementCases v0
            defCase = Core.caseStatementDefault v0
        in (Eithers.bind (Eithers.mapList (\cf ->
          let cfname = Formatting.capitalize (Core.unName (Core.fieldName cf))
              cfterm = Core.fieldTerm cf
          in (Eithers.bind (encodeTerm cx g (Core.TermApplication (Core.Application {
            Core.applicationFunction = cfterm,
            Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))) (\armBody -> Right (Syntax.MatchArm {
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
            Syntax.matchArmBody = armBody})))) caseFields) (\arms -> Eithers.bind (Maybes.cases defCase (Right arms) (\dt -> Eithers.bind (encodeTerm cx g (Core.TermApplication (Core.Application {
          Core.applicationFunction = dt,
          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))) (\defBody -> Right (Lists.concat2 arms [
          Syntax.MatchArm {
            Syntax.matchArmPattern = Syntax.PatternWildcard,
            Syntax.matchArmGuard = Nothing,
            Syntax.matchArmBody = defBody}])))) (\allArms -> Maybes.cases marg (Right (rustClosure [
          "v"] (Syntax.ExpressionMatch (Syntax.MatchExpr {
          Syntax.matchExprScrutinee = (rustExprPath "v"),
          Syntax.matchExprArms = allArms})))) (\arg -> Eithers.bind (encodeTerm cx g arg) (\sarg -> Right (Syntax.ExpressionMatch (Syntax.MatchExpr {
          Syntax.matchExprScrutinee = sarg,
          Syntax.matchExprArms = allArms})))))))
      Core.EliminationWrap _ -> Maybes.cases marg (Right (rustClosure [
        "v"] (Syntax.ExpressionTupleIndex (Syntax.TupleIndexExpr {
        Syntax.tupleIndexExprTuple = (rustExprPath "v"),
        Syntax.tupleIndexExprIndex = 0})))) (\arg -> Eithers.bind (encodeTerm cx g arg) (\sarg -> Right (Syntax.ExpressionTupleIndex (Syntax.TupleIndexExpr {
        Syntax.tupleIndexExprTuple = sarg,
        Syntax.tupleIndexExprIndex = 0}))))

encodeEnumVariant :: Context.Context -> t0 -> Core.FieldType -> Either (Context.InContext Errors.Error) Syntax.EnumVariant
encodeEnumVariant cx g ft =

      let fname = Core.unName (Core.fieldTypeName ft)
          ftyp = Core.fieldTypeType ft
          dtyp = Strip.deannotateType ftyp
          isUnit =
                  case dtyp of
                    Core.TypeUnit -> True
                    Core.TypeRecord v0 -> Lists.null v0
                    _ -> False
      in (Logic.ifElse isUnit (Right (Syntax.EnumVariant {
        Syntax.enumVariantName = (Formatting.capitalize fname),
        Syntax.enumVariantBody = Syntax.EnumVariantBodyUnit,
        Syntax.enumVariantDoc = Nothing})) (case dtyp of
        Core.TypeRecord v0 -> Eithers.bind (Eithers.mapList (encodeStructField cx g) v0) (\sfields -> Right (Syntax.EnumVariant {
          Syntax.enumVariantName = (Formatting.capitalize fname),
          Syntax.enumVariantBody = (Syntax.EnumVariantBodyStruct sfields),
          Syntax.enumVariantDoc = Nothing}))
        _ -> Eithers.bind (encodeType cx g ftyp) (\sftyp -> Right (Syntax.EnumVariant {
          Syntax.enumVariantName = (Formatting.capitalize fname),
          Syntax.enumVariantBody = (Syntax.EnumVariantBodyTuple [
            sftyp]),
          Syntax.enumVariantDoc = Nothing}))))

encodeFunction :: Context.Context -> t0 -> Core.Function -> Either (Context.InContext Errors.Error) Syntax.Expression
encodeFunction cx g fun =
    case fun of
      Core.FunctionLambda v0 ->
        let param = Formatting.convertCaseCamelToLowerSnake (Core.unName (Core.lambdaParameter v0))
        in (Eithers.bind (encodeTerm cx g (Core.lambdaBody v0)) (\body -> Right (rustClosure [
          param] body)))
      Core.FunctionPrimitive v0 -> Right (rustExprPath (Core.unName v0))
      Core.FunctionElimination v0 -> encodeElimination cx g v0 Nothing

encodeLiteral :: Core.Literal -> Syntax.Expression
encodeLiteral lit =
    case lit of
      Core.LiteralBoolean v0 -> Syntax.ExpressionLiteral (Syntax.LiteralBool v0)
      Core.LiteralString v0 -> Syntax.ExpressionLiteral (Syntax.LiteralString v0)
      Core.LiteralFloat v0 -> case v0 of
        Core.FloatValueFloat32 v1 -> Syntax.ExpressionLiteral (Syntax.LiteralFloat (Syntax.FloatLiteral {
          Syntax.floatLiteralValue = (Literals.bigfloatToFloat64 (Literals.float32ToBigfloat v1)),
          Syntax.floatLiteralSuffix = (Just "f32")}))
        Core.FloatValueFloat64 v1 -> Syntax.ExpressionLiteral (Syntax.LiteralFloat (Syntax.FloatLiteral {
          Syntax.floatLiteralValue = v1,
          Syntax.floatLiteralSuffix = Nothing}))
        Core.FloatValueBigfloat v1 -> Syntax.ExpressionLiteral (Syntax.LiteralFloat (Syntax.FloatLiteral {
          Syntax.floatLiteralValue = (Literals.bigfloatToFloat64 v1),
          Syntax.floatLiteralSuffix = Nothing}))
      Core.LiteralInteger v0 -> case v0 of
        Core.IntegerValueInt8 v1 -> Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
          Syntax.integerLiteralValue = (Literals.int8ToBigint v1),
          Syntax.integerLiteralSuffix = (Just "i8")}))
        Core.IntegerValueInt16 v1 -> Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
          Syntax.integerLiteralValue = (Literals.int16ToBigint v1),
          Syntax.integerLiteralSuffix = (Just "i16")}))
        Core.IntegerValueInt32 v1 -> Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
          Syntax.integerLiteralValue = (Literals.int32ToBigint v1),
          Syntax.integerLiteralSuffix = (Just "i32")}))
        Core.IntegerValueInt64 v1 -> Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
          Syntax.integerLiteralValue = (Literals.int64ToBigint v1),
          Syntax.integerLiteralSuffix = (Just "i64")}))
        Core.IntegerValueUint8 v1 -> Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
          Syntax.integerLiteralValue = (Literals.uint8ToBigint v1),
          Syntax.integerLiteralSuffix = (Just "u8")}))
        Core.IntegerValueUint16 v1 -> Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
          Syntax.integerLiteralValue = (Literals.uint16ToBigint v1),
          Syntax.integerLiteralSuffix = (Just "u16")}))
        Core.IntegerValueUint32 v1 -> Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
          Syntax.integerLiteralValue = (Literals.uint32ToBigint v1),
          Syntax.integerLiteralSuffix = (Just "u32")}))
        Core.IntegerValueUint64 v1 -> Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
          Syntax.integerLiteralValue = (Literals.uint64ToBigint v1),
          Syntax.integerLiteralSuffix = (Just "u64")}))
        Core.IntegerValueBigint v1 -> Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
          Syntax.integerLiteralValue = v1,
          Syntax.integerLiteralSuffix = Nothing}))

encodeLiteralType :: Core.LiteralType -> Syntax.Type
encodeLiteralType lt =
    case lt of
      Core.LiteralTypeBinary -> rustApply1 "Vec" (rustPath "u8")
      Core.LiteralTypeBoolean -> rustPath "bool"
      Core.LiteralTypeFloat v0 -> case v0 of
        Core.FloatTypeBigfloat -> rustPath "f64"
        Core.FloatTypeFloat32 -> rustPath "f32"
        Core.FloatTypeFloat64 -> rustPath "f64"
      Core.LiteralTypeInteger v0 -> case v0 of
        Core.IntegerTypeBigint -> rustPathSegmented [
          "num",
          "BigInt"]
        Core.IntegerTypeInt8 -> rustPath "i8"
        Core.IntegerTypeInt16 -> rustPath "i16"
        Core.IntegerTypeInt32 -> rustPath "i32"
        Core.IntegerTypeInt64 -> rustPath "i64"
        Core.IntegerTypeUint8 -> rustPath "u8"
        Core.IntegerTypeUint16 -> rustPath "u16"
        Core.IntegerTypeUint32 -> rustPath "u32"
        Core.IntegerTypeUint64 -> rustPath "u64"
      Core.LiteralTypeString -> rustPath "String"

encodeStructField :: Context.Context -> t0 -> Core.FieldType -> Either (Context.InContext Errors.Error) Syntax.StructField
encodeStructField cx g ft =

      let fname = Core.unName (Core.fieldTypeName ft)
          ftyp = Core.fieldTypeType ft
      in (Eithers.bind (encodeType cx g ftyp) (\sftyp -> Right (Syntax.StructField {
        Syntax.structFieldName = (Formatting.convertCaseCamelToLowerSnake (Formatting.sanitizeWithUnderscores Language.rustReservedWords fname)),
        Syntax.structFieldType = sftyp,
        Syntax.structFieldPublic = True,
        Syntax.structFieldDoc = Nothing})))

encodeTerm :: Context.Context -> t0 -> Core.Term -> Either (Context.InContext Errors.Error) Syntax.Expression
encodeTerm cx g term =
    case term of
      Core.TermAnnotated v0 -> encodeTerm cx g (Core.annotatedTermBody v0)
      Core.TermApplication v0 -> Eithers.bind (encodeTerm cx g (Core.applicationFunction v0)) (\fun -> Eithers.bind (encodeTerm cx g (Core.applicationArgument v0)) (\arg -> Right (rustCall fun [
        arg])))
      Core.TermEither v0 -> Eithers.either (\l -> Eithers.bind (encodeTerm cx g l) (\sl -> Right (rustCall (rustExprPath "Left") [
        sl]))) (\r -> Eithers.bind (encodeTerm cx g r) (\sr -> Right (rustCall (rustExprPath "Right") [
        sr]))) v0
      Core.TermFunction v0 -> encodeFunction cx g v0
      Core.TermLet v0 ->
        let bindings = Core.letBindings v0
            body = Core.letBody v0
        in (Eithers.bind (Eithers.mapList (\b ->
          let bname = Formatting.convertCaseCamelToLowerSnake (Core.unName (Core.bindingName b))
          in (Eithers.bind (encodeTerm cx g (Core.bindingTerm b)) (\bval -> Right (rustLetStmt bname bval)))) bindings) (\stmts -> Eithers.bind (encodeTerm cx g body) (\bodyExpr -> Right (rustBlock stmts bodyExpr))))
      Core.TermList v0 -> Eithers.bind (Eithers.mapList (encodeTerm cx g) v0) (\sels -> Right (rustCall (rustExprPath "Vec::from") [
        Syntax.ExpressionArray (Syntax.ArrayExprElements sels)]))
      Core.TermLiteral v0 -> Right (encodeLiteral v0)
      Core.TermMap v0 -> Eithers.bind (Eithers.mapList (\entry -> Eithers.bind (encodeTerm cx g (Pairs.first entry)) (\k -> Eithers.bind (encodeTerm cx g (Pairs.second entry)) (\v -> Right (Syntax.ExpressionTuple [
        k,
        v])))) (Maps.toList v0)) (\pairs -> Right (rustCall (rustExprPath "BTreeMap::from") [
        Syntax.ExpressionArray (Syntax.ArrayExprElements pairs)]))
      Core.TermMaybe v0 -> Maybes.cases v0 (Right (rustExprPath "None")) (\val -> Eithers.bind (encodeTerm cx g val) (\sval -> Right (rustCall (rustExprPath "Some") [
        sval])))
      Core.TermPair v0 -> Eithers.bind (encodeTerm cx g (Pairs.first v0)) (\f -> Eithers.bind (encodeTerm cx g (Pairs.second v0)) (\s -> Right (Syntax.ExpressionTuple [
        f,
        s])))
      Core.TermRecord v0 ->
        let rname = Core.recordTypeName v0
            fields = Core.recordFields v0
        in (Eithers.bind (Eithers.mapList (\f ->
          let fname = Formatting.convertCaseCamelToLowerSnake (Core.unName (Core.fieldName f))
          in (Eithers.bind (encodeTerm cx g (Core.fieldTerm f)) (\fval -> Right (Syntax.FieldValue {
            Syntax.fieldValueName = fname,
            Syntax.fieldValueValue = (Just fval)})))) fields) (\sfields -> Right (Syntax.ExpressionStruct (Syntax.StructExpr {
          Syntax.structExprPath = Syntax.ExprPath {
            Syntax.exprPathGlobal = False,
            Syntax.exprPathSegments = [
              Syntax.PathSegment {
                Syntax.pathSegmentName = (Formatting.capitalize (Names.localNameOf rname)),
                Syntax.pathSegmentArguments = Syntax.GenericArgumentsNone}]},
          Syntax.structExprFields = sfields,
          Syntax.structExprRest = Nothing}))))
      Core.TermSet v0 -> Eithers.bind (Eithers.mapList (encodeTerm cx g) (Sets.toList v0)) (\sels -> Right (rustCall (rustExprPath "BTreeSet::from") [
        Syntax.ExpressionArray (Syntax.ArrayExprElements sels)]))
      Core.TermUnion v0 ->
        let tname = Formatting.capitalize (Names.localNameOf (Core.injectionTypeName v0))
            field = Core.injectionField v0
            fname = Formatting.capitalize (Core.unName (Core.fieldName field))
            fterm = Core.fieldTerm field
            dterm = Strip.deannotateTerm fterm
            isUnit =
                    case dterm of
                      Core.TermUnit -> True
                      Core.TermRecord v1 -> Lists.null (Core.recordFields v1)
                      _ -> False
        in (Logic.ifElse isUnit (Right (rustExprPath (Strings.cat2 (Strings.cat2 tname "::") fname))) (Eithers.bind (encodeTerm cx g fterm) (\sval -> Right (rustCall (rustExprPath (Strings.cat2 (Strings.cat2 tname "::") fname)) [
          sval]))))
      Core.TermUnit -> Right (Syntax.ExpressionTuple [])
      Core.TermVariable v0 -> Right (rustExprPath (Formatting.convertCaseCamelToLowerSnake (Formatting.sanitizeWithUnderscores Language.rustReservedWords (Core.unName v0))))
      Core.TermWrap v0 ->
        let tname = Formatting.capitalize (Names.localNameOf (Core.wrappedTermTypeName v0))
        in (Eithers.bind (encodeTerm cx g (Core.wrappedTermBody v0)) (\inner -> Right (rustCall (rustExprPath tname) [
          inner])))
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "unexpected term variant")),
        Context.inContextContext = cx})

encodeTermDefinition :: Context.Context -> t0 -> Module.TermDefinition -> Either (Context.InContext Errors.Error) Syntax.ItemWithComments
encodeTermDefinition cx g tdef =

      let name = Module.termDefinitionName tdef
          term = Module.termDefinitionTerm tdef
          lname = Formatting.convertCaseCamelToLowerSnake (Names.localNameOf name)
          typ = Maybes.maybe (Core.TypeVariable (Core.Name "hydra.core.Unit")) Core.typeSchemeType (Module.termDefinitionType tdef)
      in (Eithers.bind (encodeTerm cx g term) (\body -> Eithers.bind (encodeType cx g typ) (\retType -> Right (Syntax.ItemWithComments {
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
          Syntax.fnDefDoc = Nothing}))}))))

encodeType :: Context.Context -> t0 -> Core.Type -> Either (Context.InContext Errors.Error) Syntax.Type
encodeType cx g t =

      let typ = Strip.deannotateType t
      in case typ of
        Core.TypeAnnotated v0 -> encodeType cx g (Core.annotatedTypeBody v0)
        Core.TypeApplication v0 -> encodeType cx g (Core.applicationTypeFunction v0)
        Core.TypeUnit -> Right rustUnit
        Core.TypeVoid -> Right rustUnit
        Core.TypeLiteral v0 -> Right (encodeLiteralType v0)
        Core.TypeList v0 -> Eithers.map (\enc -> rustApply1 "Vec" enc) (encodeType cx g v0)
        Core.TypeSet v0 -> Eithers.map (\enc -> rustApply1 "BTreeSet" enc) (encodeType cx g v0)
        Core.TypeMap v0 -> Eithers.bind (encodeType cx g (Core.mapTypeKeys v0)) (\kt -> Eithers.bind (encodeType cx g (Core.mapTypeValues v0)) (\vt -> Right (rustApply2 "BTreeMap" kt vt)))
        Core.TypeMaybe v0 -> Eithers.map (\enc -> rustApply1 "Option" enc) (encodeType cx g v0)
        Core.TypeEither v0 -> Eithers.bind (encodeType cx g (Core.eitherTypeLeft v0)) (\lt -> Eithers.bind (encodeType cx g (Core.eitherTypeRight v0)) (\rt -> Right (rustApply2 "Either" lt rt)))
        Core.TypePair v0 -> Eithers.bind (encodeType cx g (Core.pairTypeFirst v0)) (\ft -> Eithers.bind (encodeType cx g (Core.pairTypeSecond v0)) (\st -> Right (Syntax.TypeTuple [
          ft,
          st])))
        Core.TypeFunction v0 -> Eithers.bind (encodeType cx g (Core.functionTypeDomain v0)) (\dom -> Eithers.bind (encodeType cx g (Core.functionTypeCodomain v0)) (\cod -> Right (rustApply1 "Box" (Syntax.TypeDynTrait [
          Syntax.TypeParamBoundTrait (Syntax.TypePath {
            Syntax.typePathGlobal = False,
            Syntax.typePathSegments = [
              Syntax.PathSegment {
                Syntax.pathSegmentName = "Fn",
                Syntax.pathSegmentArguments = (Syntax.GenericArgumentsParenthesized (Syntax.ParenthesizedArgs {
                  Syntax.parenthesizedArgsInputs = [
                    dom],
                  Syntax.parenthesizedArgsOutput = (Just cod)}))}]})]))))
        Core.TypeRecord _ -> Left (Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "unexpected anonymous record type")),
          Context.inContextContext = cx})
        Core.TypeUnion _ -> Left (Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "unexpected anonymous union type")),
          Context.inContextContext = cx})
        Core.TypeWrap _ -> Left (Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "unexpected anonymous wrap type")),
          Context.inContextContext = cx})
        Core.TypeVariable v0 -> Right (rustPath (Formatting.capitalize (Core.unName v0)))
        Core.TypeForall v0 -> encodeType cx g (Core.forallTypeBody v0)

encodeTypeDefinition :: Context.Context -> t0 -> Module.TypeDefinition -> Either (Context.InContext Errors.Error) Syntax.ItemWithComments
encodeTypeDefinition cx g tdef =

      let name = Module.typeDefinitionName tdef
          typ = Module.typeDefinitionType tdef
          lname = Formatting.capitalize (Names.localNameOf name)
          freeVars =
                  Lists.filter (\v -> Lists.null (Lists.tail (Strings.splitOn "." (Core.unName v)))) (Sets.toList (Variables.freeVariablesInType typ))
          generics =
                  Lists.map (\v -> Syntax.GenericParam {
                    Syntax.genericParamName = (Formatting.capitalize (Core.unName v)),
                    Syntax.genericParamBounds = []}) freeVars
          dtyp = Strip.deannotateType typ
      in (Eithers.bind (case dtyp of
        Core.TypeRecord v0 -> Eithers.bind (Eithers.mapList (encodeStructField cx g) v0) (\sfields -> Right (Syntax.ItemStruct (Syntax.StructDef {
          Syntax.structDefName = lname,
          Syntax.structDefGenerics = generics,
          Syntax.structDefWhereClause = Nothing,
          Syntax.structDefBody = (Syntax.StructBodyNamed sfields),
          Syntax.structDefDerives = standardDerives,
          Syntax.structDefPublic = True,
          Syntax.structDefDoc = Nothing})))
        Core.TypeUnion v0 -> Eithers.bind (Eithers.mapList (encodeEnumVariant cx g) v0) (\variants -> Right (Syntax.ItemEnum (Syntax.EnumDef {
          Syntax.enumDefName = lname,
          Syntax.enumDefGenerics = generics,
          Syntax.enumDefWhereClause = Nothing,
          Syntax.enumDefVariants = variants,
          Syntax.enumDefDerives = standardDerives,
          Syntax.enumDefPublic = True,
          Syntax.enumDefDoc = Nothing})))
        Core.TypeWrap v0 -> Eithers.bind (encodeType cx g v0) (\styp -> Right (Syntax.ItemStruct (Syntax.StructDef {
          Syntax.structDefName = lname,
          Syntax.structDefGenerics = generics,
          Syntax.structDefWhereClause = Nothing,
          Syntax.structDefBody = (Syntax.StructBodyTuple [
            Syntax.TupleField {
              Syntax.tupleFieldType = styp,
              Syntax.tupleFieldPublic = True}]),
          Syntax.structDefDerives = standardDerives,
          Syntax.structDefPublic = True,
          Syntax.structDefDoc = Nothing})))
        _ -> Eithers.bind (encodeType cx g typ) (\styp -> Right (Syntax.ItemTypeAlias (Syntax.TypeAlias {
          Syntax.typeAliasName = lname,
          Syntax.typeAliasGenerics = generics,
          Syntax.typeAliasType = styp,
          Syntax.typeAliasPublic = True,
          Syntax.typeAliasDoc = Nothing})))) (\item -> Right (Syntax.ItemWithComments {
        Syntax.itemWithCommentsDoc = Nothing,
        Syntax.itemWithCommentsVisibility = Syntax.VisibilityPublic,
        Syntax.itemWithCommentsItem = item})))

moduleToRust :: Module.Module -> [Module.Definition] -> Context.Context -> t0 -> Either (Context.InContext Errors.Error) (M.Map String String)
moduleToRust mod defs cx g =

      let partitioned = Environment.partitionDefinitions defs
          typeDefs = Pairs.first partitioned
          termDefs = Pairs.second partitioned
      in (Eithers.bind (Eithers.mapList (encodeTypeDefinition cx g) typeDefs) (\typeItems -> Eithers.bind (Eithers.mapList (encodeTermDefinition cx g) termDefs) (\termItems ->
        let allItems = Lists.concat2 typeItems termItems
            crate = Syntax.Crate {
                  Syntax.crateItems = allItems}
            code = Serialization.printExpr (Serialization.parenthesize (Serde.crateToExpr crate))
            filePath = Names.namespaceToFilePath Util.CaseConventionLowerSnake (Module.FileExtension "rs") (Module.moduleNamespace mod)
        in (Right (Maps.singleton filePath code)))))

rustApply1 :: String -> Syntax.Type -> Syntax.Type
rustApply1 name arg =
    Syntax.TypePath_ (Syntax.TypePath {
      Syntax.typePathGlobal = False,
      Syntax.typePathSegments = [
        Syntax.PathSegment {
          Syntax.pathSegmentName = name,
          Syntax.pathSegmentArguments = (Syntax.GenericArgumentsAngleBracketed (Syntax.AngleBracketedArgs {
            Syntax.angleBracketedArgsArgs = [
              Syntax.GenericArgType arg]}))}]})

rustApply2 :: String -> Syntax.Type -> Syntax.Type -> Syntax.Type
rustApply2 name arg1 arg2 =
    Syntax.TypePath_ (Syntax.TypePath {
      Syntax.typePathGlobal = False,
      Syntax.typePathSegments = [
        Syntax.PathSegment {
          Syntax.pathSegmentName = name,
          Syntax.pathSegmentArguments = (Syntax.GenericArgumentsAngleBracketed (Syntax.AngleBracketedArgs {
            Syntax.angleBracketedArgsArgs = [
              Syntax.GenericArgType arg1,
              (Syntax.GenericArgType arg2)]}))}]})

rustBlock :: [Syntax.Statement] -> Syntax.Expression -> Syntax.Expression
rustBlock stmts expr =
    Syntax.ExpressionBlock (Syntax.Block {
      Syntax.blockStatements = stmts,
      Syntax.blockExpression = (Just expr)})

rustCall :: Syntax.Expression -> [Syntax.Expression] -> Syntax.Expression
rustCall fun args =
    Syntax.ExpressionCall (Syntax.CallExpr {
      Syntax.callExprFunction = fun,
      Syntax.callExprArgs = args})

rustClosure :: [String] -> Syntax.Expression -> Syntax.Expression
rustClosure params body =
    Syntax.ExpressionClosure (Syntax.ClosureExpr {
      Syntax.closureExprMove = False,
      Syntax.closureExprParams = (Lists.map (\p -> Syntax.ClosureParam {
        Syntax.closureParamPattern = (Syntax.PatternIdentifier (Syntax.IdentifierPattern {
          Syntax.identifierPatternName = p,
          Syntax.identifierPatternMutable = False,
          Syntax.identifierPatternAtPattern = Nothing})),
        Syntax.closureParamType = Nothing}) params),
      Syntax.closureExprReturnType = Nothing,
      Syntax.closureExprBody = body})

rustExprPath :: String -> Syntax.Expression
rustExprPath name =
    Syntax.ExpressionPath (Syntax.ExprPath {
      Syntax.exprPathGlobal = False,
      Syntax.exprPathSegments = [
        Syntax.PathSegment {
          Syntax.pathSegmentName = name,
          Syntax.pathSegmentArguments = Syntax.GenericArgumentsNone}]})

rustLetStmt :: String -> Syntax.Expression -> Syntax.Statement
rustLetStmt name expr =
    Syntax.StatementLet (Syntax.LetStatement {
      Syntax.letStatementPattern = (Syntax.PatternIdentifier (Syntax.IdentifierPattern {
        Syntax.identifierPatternName = name,
        Syntax.identifierPatternMutable = False,
        Syntax.identifierPatternAtPattern = Nothing})),
      Syntax.letStatementMutable = False,
      Syntax.letStatementType = Nothing,
      Syntax.letStatementInit = (Just expr)})

rustPath :: String -> Syntax.Type
rustPath name =
    Syntax.TypePath_ (Syntax.TypePath {
      Syntax.typePathGlobal = False,
      Syntax.typePathSegments = [
        Syntax.PathSegment {
          Syntax.pathSegmentName = name,
          Syntax.pathSegmentArguments = Syntax.GenericArgumentsNone}]})

rustPathSegmented :: [String] -> Syntax.Type
rustPathSegmented segs =
    Syntax.TypePath_ (Syntax.TypePath {
      Syntax.typePathGlobal = False,
      Syntax.typePathSegments = (Lists.map (\s -> Syntax.PathSegment {
        Syntax.pathSegmentName = s,
        Syntax.pathSegmentArguments = Syntax.GenericArgumentsNone}) segs)})

rustUnit :: Syntax.Type
rustUnit = Syntax.TypeUnit

standardDerives :: [String]
standardDerives =
    [
      "Clone",
      "Debug",
      "PartialEq",
      "Eq",
      "PartialOrd",
      "Ord"]
