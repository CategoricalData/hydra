-- Note: this is an automatically generated file. Do not edit.

-- | Functions for encoding Hydra modules as Haskell modules

module Hydra.Haskell.Coder where

import qualified Hydra.Adapt as Adapt
import qualified Hydra.Analysis as Analysis
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Classes as Classes
import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Dependencies as Dependencies
import qualified Hydra.Encode.Core as Core_
import qualified Hydra.Errors as Errors
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Haskell.Environment as Environment
import qualified Hydra.Haskell.Language as Language
import qualified Hydra.Haskell.Serde as Serde
import qualified Hydra.Haskell.Syntax as Syntax
import qualified Hydra.Haskell.Utils as Utils
import qualified Hydra.Lexical as Lexical
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
import qualified Hydra.Predicates as Predicates
import qualified Hydra.Resolution as Resolution
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Show.Core as Core__
import qualified Hydra.Strip as Strip
import qualified Hydra.Util as Util
import qualified Hydra.Variables as Variables
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M
import qualified Data.Set as S

-- | Adapt a Hydra type to Haskell's type system and encode it
adaptTypeToHaskellAndEncode :: Packaging.Namespaces Syntax.ModuleName -> Core.Type -> t0 -> t1 -> Either Errors.Error Syntax.Type
adaptTypeToHaskellAndEncode namespaces typ cx g =

      let enc = \t -> encodeType namespaces t cx g
      in case (Strip.deannotateType typ) of
        Core.TypeVariable _ -> enc typ
        _ -> Eithers.bind (Adapt.adaptTypeForLanguage Language.haskellLanguage typ) (\adaptedType -> enc adaptedType)

-- | Generate a constant name for a field (e.g., '_TypeName_fieldName')
constantForFieldName :: Core.Name -> Core.Name -> String
constantForFieldName tname fname =
    Strings.cat [
      "_",
      (Names.localNameOf tname),
      "_",
      (Core.unName fname)]

-- | Generate a constant name for a type (e.g., '_TypeName')
constantForTypeName :: Core.Name -> String
constantForTypeName tname = Strings.cat2 "_" (Names.localNameOf tname)

-- | Construct a Haskell module from a Hydra module and its definitions
constructModule :: Packaging.Namespaces Syntax.ModuleName -> Packaging.Module -> [Packaging.Definition] -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.Module
constructModule namespaces mod defs cx g =

      let h = \namespace -> Packaging.unNamespace namespace
          createDeclarations =
                  \def -> case def of
                    Packaging.DefinitionType v0 ->
                      let name = Packaging.typeDefinitionName v0
                          typ = Core.typeSchemeType (Packaging.typeDefinitionType v0)
                      in (toTypeDeclarationsFrom namespaces name typ cx g)
                    Packaging.DefinitionTerm v0 -> Eithers.bind (toDataDeclaration namespaces v0 cx g) (\d -> Right [
                      d])
          importName =
                  \name -> Syntax.ModuleName (Strings.intercalate "." (Lists.map Formatting.capitalize (Strings.splitOn "." name)))
          imports = Lists.concat2 domainImports standardImports
          domainImports =

                    let toImport =
                            \pair ->
                              let namespace = Pairs.first pair
                                  alias = Pairs.second pair
                                  name = h namespace
                              in Syntax.Import {
                                Syntax.importQualified = True,
                                Syntax.importModule = (importName name),
                                Syntax.importAs = (Just alias),
                                Syntax.importSpec = Nothing}
                    in (Lists.map toImport (Maps.toList (Packaging.namespacesMapping namespaces)))
          meta = gatherMetadata defs
          condImport = \flag -> \triple -> Logic.ifElse flag [
                triple] []
          standardImports =

                    let toImport =
                            \triple ->
                              let name = Pairs.first (Pairs.first triple)
                                  malias = Pairs.second (Pairs.first triple)
                                  hidden = Pairs.second triple
                                  spec =
                                          Logic.ifElse (Lists.null hidden) Nothing (Just (Syntax.SpecImportHiding (Lists.map (\n -> Syntax.ImportExportSpec {
                                            Syntax.importExportSpecModifier = Nothing,
                                            Syntax.importExportSpecName = (Utils.simpleName n),
                                            Syntax.importExportSpecSubspec = Nothing}) hidden)))
                              in Syntax.Import {
                                Syntax.importQualified = (Maybes.isJust malias),
                                Syntax.importModule = (Syntax.ModuleName name),
                                Syntax.importAs = (Maybes.map (\x -> Syntax.ModuleName x) malias),
                                Syntax.importSpec = spec}
                    in (Lists.map toImport (Lists.concat [
                      [
                        (("Prelude", Nothing), [
                          "Enum",
                          "Ordering",
                          "decodeFloat",
                          "encodeFloat",
                          "fail",
                          "map",
                          "pure",
                          "sum"])],
                      (condImport (Environment.haskellModuleMetadataUsesByteString meta) (("Data.ByteString", (Just "B")), [])),
                      (condImport (Environment.haskellModuleMetadataUsesInt meta) (("Data.Int", (Just "I")), [])),
                      (condImport (Environment.haskellModuleMetadataUsesMap meta) (("Data.Map", (Just "M")), [])),
                      (condImport (Environment.haskellModuleMetadataUsesSet meta) (("Data.Set", (Just "S")), [])),
                      (Logic.ifElse (Analysis.moduleContainsBinaryLiterals mod) [
                        (("Hydra.Lib.Literals", (Just "Literals")), [])] [])]))
      in (Eithers.bind (Eithers.mapList createDeclarations defs) (\declLists ->
        let decls = Lists.concat declLists
            mc = Packaging.moduleDescription mod
        in (Right (Syntax.Module {
          Syntax.moduleHead = (Just (Syntax.ModuleHead {
            Syntax.moduleHeadComments = mc,
            Syntax.moduleHeadName = (importName (h (Packaging.moduleNamespace mod))),
            Syntax.moduleHeadExports = []})),
          Syntax.moduleImports = imports,
          Syntax.moduleDeclarations = decls}))))

-- | Create an initial empty metadata record with all flags set to false
emptyMetadata :: Environment.HaskellModuleMetadata
emptyMetadata =
    Environment.HaskellModuleMetadata {
      Environment.haskellModuleMetadataUsesByteString = False,
      Environment.haskellModuleMetadataUsesInt = False,
      Environment.haskellModuleMetadataUsesMap = False,
      Environment.haskellModuleMetadataUsesSet = False}

-- | Encode a Hydra case statement as a Haskell case expression with a given scrutinee
encodeCaseExpression :: Int -> Packaging.Namespaces Syntax.ModuleName -> Core.CaseStatement -> Syntax.Expression -> t0 -> Graph.Graph -> Either Errors.Error Syntax.Expression
encodeCaseExpression depth namespaces stmt scrutinee cx g =

      let dn = Core.caseStatementTypeName stmt
          def = Core.caseStatementDefault stmt
          fields = Core.caseStatementCases stmt
          toAlt =
                  \fieldMap -> \field ->
                    let fn = Core.fieldName field
                        fun_ = Core.fieldTerm field
                        v0 = Strings.cat2 "v" (Literals.showInt32 depth)
                        raw =
                                Core.TermApplication (Core.Application {
                                  Core.applicationFunction = fun_,
                                  Core.applicationArgument = (Core.TermVariable (Core.Name v0))})
                        rhsTerm = Dependencies.simplifyTerm raw
                        v1 = Logic.ifElse (Variables.isFreeVariableInTerm (Core.Name v0) rhsTerm) Constants.ignoredVariable v0
                        hname =
                                Utils.unionFieldReference (Sets.union (Sets.fromList (Maps.keys (Graph.graphBoundTerms g))) (Sets.fromList (Maps.keys (Graph.graphSchemaTypes g)))) namespaces dn fn
                    in (Eithers.bind (Maybes.cases (Maps.lookup fn fieldMap) (Left (Errors.ErrorResolution (Errors.ResolutionErrorNoMatchingField (Errors.NoMatchingFieldError {
                      Errors.noMatchingFieldErrorFieldName = fn})))) (\fieldType ->
                      let ft = Core.fieldTypeType fieldType
                          noArgs = []
                          singleArg = [
                                Syntax.PatternName (Utils.rawName v1)]
                      in case (Strip.deannotateType ft) of
                        Core.TypeUnit -> Right noArgs
                        _ -> Right singleArg)) (\args ->
                      let lhs = Utils.applicationPattern hname args
                      in (Eithers.bind (Eithers.map (\x -> Syntax.CaseRhs x) (encodeTerm (Math.add depth 1) namespaces rhsTerm cx g)) (\rhs -> Right (Syntax.Alternative {
                        Syntax.alternativePattern = lhs,
                        Syntax.alternativeRhs = rhs,
                        Syntax.alternativeBinds = Nothing})))))
      in (Eithers.bind (Resolution.requireUnionType cx g dn) (\rt ->
        let toFieldMapEntry = \f -> (Core.fieldTypeName f, f)
            fieldMap = Maps.fromList (Lists.map toFieldMapEntry rt)
        in (Eithers.bind (Eithers.mapList (toAlt fieldMap) fields) (\ecases -> Eithers.bind (Maybes.cases def (Right []) (\d -> Eithers.bind (Eithers.map (\x -> Syntax.CaseRhs x) (encodeTerm depth namespaces d cx g)) (\cs ->
          let lhs = Syntax.PatternName (Utils.rawName Constants.ignoredVariable)
              alt =
                      Syntax.Alternative {
                        Syntax.alternativePattern = lhs,
                        Syntax.alternativeRhs = cs,
                        Syntax.alternativeBinds = Nothing}
          in (Right [
            alt])))) (\dcases -> Right (Syntax.ExpressionCase (Syntax.CaseExpression {
          Syntax.caseExpressionCase = scrutinee,
          Syntax.caseExpressionAlternatives = (Lists.concat2 ecases dcases)})))))))

-- | Encode a Hydra lambda as a Haskell expression
encodeLambdaTerm :: Int -> Packaging.Namespaces Syntax.ModuleName -> Core.Lambda -> t0 -> Graph.Graph -> Either Errors.Error Syntax.Expression
encodeLambdaTerm depth namespaces lam cx g =

      let v = Core.lambdaParameter lam
          body = Core.lambdaBody lam
      in (Eithers.bind (encodeTerm depth namespaces body cx g) (\hbody -> Right (Utils.hslambda (Utils.elementReference namespaces v) hbody)))

-- | Encode a Hydra literal as a Haskell expression
encodeLiteral :: Core.Literal -> t0 -> Either Errors.Error Syntax.Expression
encodeLiteral l cx =
    case l of
      Core.LiteralBinary v0 -> Right (Utils.hsapp (Utils.hsvar "Literals.stringToBinary") (Utils.hslit (Syntax.LiteralString (Literals.binaryToString v0))))
      Core.LiteralBoolean v0 -> Right (Utils.hsvar (Logic.ifElse v0 "True" "False"))
      Core.LiteralFloat v0 -> case v0 of
        Core.FloatValueFloat32 v1 -> Right (Utils.hslit (Syntax.LiteralFloat v1))
        Core.FloatValueFloat64 v1 -> Right (Utils.hslit (Syntax.LiteralDouble v1))
        Core.FloatValueBigfloat v1 -> Right (Utils.hslit (Syntax.LiteralDouble (Literals.bigfloatToFloat64 v1)))
      Core.LiteralInteger v0 -> case v0 of
        Core.IntegerValueBigint v1 -> Right (Utils.hslit (Syntax.LiteralInteger v1))
        Core.IntegerValueInt8 v1 -> Right (Utils.hslit (Syntax.LiteralInteger (Literals.int8ToBigint v1)))
        Core.IntegerValueInt16 v1 -> Right (Utils.hslit (Syntax.LiteralInteger (Literals.int16ToBigint v1)))
        Core.IntegerValueInt32 v1 -> Right (Utils.hslit (Syntax.LiteralInt v1))
        Core.IntegerValueInt64 v1 -> Right (Utils.hslit (Syntax.LiteralInteger (Literals.int64ToBigint v1)))
        Core.IntegerValueUint8 v1 -> Right (Utils.hslit (Syntax.LiteralInteger (Literals.uint8ToBigint v1)))
        Core.IntegerValueUint16 v1 -> Right (Utils.hslit (Syntax.LiteralInteger (Literals.uint16ToBigint v1)))
        Core.IntegerValueUint32 v1 -> Right (Utils.hslit (Syntax.LiteralInteger (Literals.uint32ToBigint v1)))
        Core.IntegerValueUint64 v1 -> Right (Utils.hslit (Syntax.LiteralInteger (Literals.uint64ToBigint v1)))
      Core.LiteralString v0 -> Right (Utils.hslit (Syntax.LiteralString v0))
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "supported literal",
        Errors.unexpectedShapeErrorActual = (Core__.literal l)})))

-- | Encode a record projection as a Haskell expression
encodeProjection :: Packaging.Namespaces Syntax.ModuleName -> Core.Projection -> Either t0 Syntax.Expression
encodeProjection namespaces proj =

      let dn = Core.projectionTypeName proj
          fname = Core.projectionField proj
      in (Right (Syntax.ExpressionVariable (Utils.recordFieldReference namespaces dn fname)))

-- | Encode a standalone (un-applied) case statement as a Haskell lambda over a case expression
encodeStandaloneCases :: Int -> Packaging.Namespaces Syntax.ModuleName -> Core.CaseStatement -> t0 -> Graph.Graph -> Either Errors.Error Syntax.Expression
encodeStandaloneCases depth namespaces stmt cx g =
    Eithers.map (Utils.hslambda (Utils.rawName "x")) (encodeCaseExpression depth namespaces stmt (Utils.hsvar "x") cx g)

-- | Encode a Hydra term as a Haskell expression
encodeTerm :: Int -> Packaging.Namespaces Syntax.ModuleName -> Core.Term -> t0 -> Graph.Graph -> Either Errors.Error Syntax.Expression
encodeTerm depth namespaces term cx g =

      let encode = \t -> encodeTerm depth namespaces t cx g
          nonemptyMap =
                  \m ->
                    let lhs = Utils.hsvar "M.fromList"
                        encodePair =
                                \pair ->
                                  let k = Pairs.first pair
                                      v = Pairs.second pair
                                  in (Eithers.bind (encode k) (\hk -> Eithers.bind (encode v) (\hv -> Right (Syntax.ExpressionTuple [
                                    hk,
                                    hv]))))
                    in (Eithers.bind (Eithers.map (\x -> Syntax.ExpressionList x) (Eithers.mapList encodePair (Maps.toList m))) (\rhs -> Right (Utils.hsapp lhs rhs)))
          nonemptySet =
                  \s ->
                    let lhs = Utils.hsvar "S.fromList"
                    in (Eithers.bind (encodeTerm depth namespaces (Core.TermList (Sets.toList s)) cx g) (\rhs -> Right (Utils.hsapp lhs rhs)))
      in case (Strip.deannotateTerm term) of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
              deannotatedFun = Strip.deannotateTerm fun
          in case deannotatedFun of
            Core.TermCases v1 -> Eithers.bind (encode arg) (\harg -> encodeCaseExpression depth namespaces v1 harg cx g)
            _ -> Eithers.bind (encode fun) (\hfun -> Eithers.bind (encode arg) (\harg -> Right (Utils.hsapp hfun harg)))
        Core.TermCases v0 -> encodeStandaloneCases depth namespaces v0 cx g
        Core.TermEither v0 -> Eithers.either (\l -> Eithers.bind (encode l) (\hl -> Right (Utils.hsapp (Utils.hsvar "Left") hl))) (\r -> Eithers.bind (encode r) (\hr -> Right (Utils.hsapp (Utils.hsvar "Right") hr))) v0
        Core.TermLambda v0 -> encodeLambdaTerm depth namespaces v0 cx g
        Core.TermProject v0 -> encodeProjection namespaces v0
        Core.TermUnwrap v0 -> encodeUnwrap namespaces v0
        Core.TermLet v0 ->
          let collectBindings =
                  \lt ->
                    let bs = Core.letBindings lt
                        body = Core.letBody lt
                    in case (Strip.deannotateTerm body) of
                      Core.TermLet v1 ->
                        let innerResult = collectBindings v1
                        in (Lists.concat2 bs (Pairs.first innerResult), (Pairs.second innerResult))
                      _ -> (bs, body)
              collected = collectBindings v0
              allBindings = Pairs.first collected
              finalBody = Pairs.second collected
              encodeBinding =
                      \binding ->
                        let name = Core.bindingName binding
                            term_ = Core.bindingTerm binding
                            hname = Utils.simpleName (Core.unName name)
                        in (Eithers.bind (encode term_) (\hexpr -> Right (Syntax.LocalBindingValue (Utils.simpleValueBinding hname hexpr Nothing))))
          in (Eithers.bind (Eithers.mapList encodeBinding allBindings) (\hbindings -> Eithers.bind (encode finalBody) (\hinner -> Right (Syntax.ExpressionLet (Syntax.LetExpression {
            Syntax.letExpressionBindings = hbindings,
            Syntax.letExpressionInner = hinner})))))
        Core.TermList v0 -> Eithers.bind (Eithers.mapList encode v0) (\helems -> Right (Syntax.ExpressionList helems))
        Core.TermLiteral v0 -> encodeLiteral v0 cx
        Core.TermMap v0 -> Logic.ifElse (Maps.null v0) (Right (Utils.hsvar "M.empty")) (nonemptyMap v0)
        Core.TermMaybe v0 -> Maybes.cases v0 (Right (Utils.hsvar "Nothing")) (\t -> Eithers.bind (encode t) (\ht -> Right (Utils.hsapp (Utils.hsvar "Just") ht)))
        Core.TermPair v0 -> Eithers.bind (encode (Pairs.first v0)) (\f -> Eithers.bind (encode (Pairs.second v0)) (\s -> Right (Syntax.ExpressionTuple [
          f,
          s])))
        Core.TermRecord v0 ->
          let sname = Core.recordTypeName v0
              fields = Core.recordFields v0
              toFieldUpdate =
                      \field ->
                        let fn = Core.fieldName field
                            ft = Core.fieldTerm field
                            fieldRef = Utils.recordFieldReference namespaces sname fn
                        in (Eithers.bind (encode ft) (\hft -> Right (Syntax.FieldUpdate {
                          Syntax.fieldUpdateName = fieldRef,
                          Syntax.fieldUpdateValue = hft})))
              typeName = Utils.elementReference namespaces sname
          in (Eithers.bind (Eithers.mapList toFieldUpdate fields) (\updates -> Right (Syntax.ExpressionConstructRecord (Syntax.ConstructRecordExpression {
            Syntax.constructRecordExpressionName = typeName,
            Syntax.constructRecordExpressionFields = updates}))))
        Core.TermSet v0 -> Logic.ifElse (Sets.null v0) (Right (Utils.hsvar "S.empty")) (nonemptySet v0)
        Core.TermTypeLambda v0 ->
          let term1 = Core.typeLambdaBody v0
          in (encode term1)
        Core.TermTypeApplication v0 ->
          let term1 = Core.typeApplicationTermBody v0
          in (encode term1)
        Core.TermUnion v0 ->
          let sname = Core.injectionTypeName v0
              field = Core.injectionField v0
              fn = Core.fieldName field
              ft = Core.fieldTerm field
              lhs =
                      Syntax.ExpressionVariable (Utils.unionFieldReference (Sets.union (Sets.fromList (Maps.keys (Graph.graphBoundTerms g))) (Sets.fromList (Maps.keys (Graph.graphSchemaTypes g)))) namespaces sname fn)
              dflt = Eithers.map (Utils.hsapp lhs) (encode ft)
          in (Eithers.bind (Resolution.requireUnionField cx g sname fn) (\ftyp -> case (Strip.deannotateType ftyp) of
            Core.TypeUnit -> Right lhs
            _ -> dflt))
        Core.TermUnit -> Right (Syntax.ExpressionTuple [])
        Core.TermVariable v0 -> Right (Syntax.ExpressionVariable (Utils.elementReference namespaces v0))
        Core.TermWrap v0 ->
          let tname = Core.wrappedTermTypeName v0
              term_ = Core.wrappedTermBody v0
              lhs = Syntax.ExpressionVariable (Utils.elementReference namespaces tname)
          in (Eithers.bind (encode term_) (\rhs -> Right (Utils.hsapp lhs rhs)))
        _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "supported term",
          Errors.unexpectedShapeErrorActual = (Core__.term term)})))

-- | Encode a Hydra type as a Haskell type
encodeType :: Packaging.Namespaces Syntax.ModuleName -> Core.Type -> t0 -> t1 -> Either Errors.Error Syntax.Type
encodeType namespaces typ cx g =

      let encode = \t -> encodeType namespaces t cx g
          ref = \name -> Right (Syntax.TypeVariable (Utils.elementReference namespaces name))
          unitTuple = Syntax.TypeTuple []
      in case (Strip.deannotateType typ) of
        Core.TypeApplication v0 ->
          let lhs = Core.applicationTypeFunction v0
              rhs = Core.applicationTypeArgument v0
          in (Eithers.bind (encode lhs) (\hlhs -> Eithers.bind (encode rhs) (\hrhs -> Right (Utils.toTypeApplication [
            hlhs,
            hrhs]))))
        Core.TypeEither v0 ->
          let left_ = Core.eitherTypeLeft v0
              right_ = Core.eitherTypeRight v0
          in (Eithers.bind (encode left_) (\hleft -> Eithers.bind (encode right_) (\hright -> Right (Utils.toTypeApplication [
            Syntax.TypeVariable (Utils.rawName "Either"),
            hleft,
            hright]))))
        Core.TypeFunction v0 ->
          let dom = Core.functionTypeDomain v0
              cod = Core.functionTypeCodomain v0
          in (Eithers.bind (encode dom) (\hdom -> Eithers.bind (encode cod) (\hcod -> Right (Syntax.TypeFunction (Syntax.FunctionType {
            Syntax.functionTypeDomain = hdom,
            Syntax.functionTypeCodomain = hcod})))))
        Core.TypeForall v0 ->
          let v = Core.forallTypeParameter v0
              body = Core.forallTypeBody v0
          in (encode body)
        Core.TypeList v0 -> Eithers.bind (encode v0) (\hlt -> Right (Syntax.TypeList hlt))
        Core.TypeLiteral v0 -> case v0 of
          Core.LiteralTypeBinary -> Right (Syntax.TypeVariable (Utils.rawName "B.ByteString"))
          Core.LiteralTypeBoolean -> Right (Syntax.TypeVariable (Utils.rawName "Bool"))
          Core.LiteralTypeFloat v1 -> case v1 of
            Core.FloatTypeFloat32 -> Right (Syntax.TypeVariable (Utils.rawName "Float"))
            Core.FloatTypeFloat64 -> Right (Syntax.TypeVariable (Utils.rawName "Double"))
            Core.FloatTypeBigfloat -> Right (Syntax.TypeVariable (Utils.rawName "Double"))
          Core.LiteralTypeInteger v1 -> case v1 of
            Core.IntegerTypeBigint -> Right (Syntax.TypeVariable (Utils.rawName "Integer"))
            Core.IntegerTypeInt8 -> Right (Syntax.TypeVariable (Utils.rawName "I.Int8"))
            Core.IntegerTypeInt16 -> Right (Syntax.TypeVariable (Utils.rawName "I.Int16"))
            Core.IntegerTypeInt32 -> Right (Syntax.TypeVariable (Utils.rawName "Int"))
            Core.IntegerTypeInt64 -> Right (Syntax.TypeVariable (Utils.rawName "I.Int64"))
            _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
              Errors.unexpectedShapeErrorExpected = "supported integer type",
              Errors.unexpectedShapeErrorActual = (Core__.integerType v1)})))
          Core.LiteralTypeString -> Right (Syntax.TypeVariable (Utils.rawName "String"))
          _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
            Errors.unexpectedShapeErrorExpected = "supported literal type",
            Errors.unexpectedShapeErrorActual = (Core__.literalType v0)})))
        Core.TypeMap v0 ->
          let kt = Core.mapTypeKeys v0
              vt = Core.mapTypeValues v0
          in (Eithers.bind (encode kt) (\hkt -> Eithers.bind (encode vt) (\hvt -> Right (Utils.toTypeApplication [
            Syntax.TypeVariable (Utils.rawName "M.Map"),
            hkt,
            hvt]))))
        Core.TypeMaybe v0 -> Eithers.bind (encode v0) (\hot -> Right (Utils.toTypeApplication [
          Syntax.TypeVariable (Utils.rawName "Maybe"),
          hot]))
        Core.TypePair v0 -> Eithers.bind (encode (Core.pairTypeFirst v0)) (\f -> Eithers.bind (encode (Core.pairTypeSecond v0)) (\s -> Right (Syntax.TypeTuple [
          f,
          s])))
        Core.TypeRecord _ -> ref (Core.Name "placeholder")
        Core.TypeSet v0 -> Eithers.bind (encode v0) (\hst -> Right (Utils.toTypeApplication [
          Syntax.TypeVariable (Utils.rawName "S.Set"),
          hst]))
        Core.TypeUnion _ -> ref (Core.Name "placeholder")
        Core.TypeUnit -> Right unitTuple
        Core.TypeVariable v0 -> ref v0
        Core.TypeVoid -> Right (Syntax.TypeVariable (Utils.rawName "Void"))
        Core.TypeWrap _ -> ref (Core.Name "placeholder")
        _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "supported type",
          Errors.unexpectedShapeErrorActual = (Core__.type_ typ)})))

-- | Encode a Hydra type as a Haskell type with typeclass assertions
encodeTypeWithClassAssertions :: Packaging.Namespaces Syntax.ModuleName -> M.Map Core.Name (S.Set Classes.TypeClass) -> Core.Type -> t0 -> t1 -> Either Errors.Error Syntax.Type
encodeTypeWithClassAssertions namespaces explicitClasses typ cx g =

      let classes = Maps.union explicitClasses (getImplicitTypeClasses typ)
          implicitClasses = getImplicitTypeClasses typ
          encodeAssertion =
                  \pair ->
                    let name = Pairs.first pair
                        cls = Pairs.second pair
                        hname =
                                Utils.rawName (case cls of
                                  Classes.TypeClassEquality -> "Eq"
                                  Classes.TypeClassOrdering -> "Ord")
                        htype = Syntax.TypeVariable (Utils.rawName (Core.unName name))
                    in (Syntax.AssertionClass (Syntax.ClassAssertion {
                      Syntax.classAssertionName = hname,
                      Syntax.classAssertionTypes = [
                        htype]}))
          assertPairs = Lists.concat (Lists.map toPairs (Maps.toList classes))
          toPairs =
                  \mapEntry ->
                    let name = Pairs.first mapEntry
                        clsSet = Pairs.second mapEntry
                        toPair = \c -> (name, c)
                    in (Lists.map toPair (Sets.toList clsSet))
      in (Eithers.bind (adaptTypeToHaskellAndEncode namespaces typ cx g) (\htyp -> Logic.ifElse (Lists.null assertPairs) (Right htyp) (
        let encoded = Lists.map encodeAssertion assertPairs
            hassert = Logic.ifElse (Equality.equal (Lists.length encoded) 1) (Lists.head encoded) (Syntax.AssertionTuple encoded)
        in (Right (Syntax.TypeCtx (Syntax.ContextType {
          Syntax.contextTypeCtx = hassert,
          Syntax.contextTypeType = htyp}))))))

-- | Encode an unwrap term as a Haskell expression
encodeUnwrap :: Packaging.Namespaces Syntax.ModuleName -> Core.Name -> Either t0 Syntax.Expression
encodeUnwrap namespaces name =
    Right (Syntax.ExpressionVariable (Utils.elementReference namespaces (Names.qname (Maybes.fromJust (Names.namespaceOf name)) (Utils.newtypeAccessorName name))))

-- | Extend metadata by analyzing a term for standard import usage (bottom-up step function)
extendMetaForTerm :: Environment.HaskellModuleMetadata -> Core.Term -> Environment.HaskellModuleMetadata
extendMetaForTerm meta term =
    case term of
      Core.TermMap _ -> setMetaUsesMap True meta
      Core.TermSet _ -> setMetaUsesSet True meta
      _ -> meta

-- | Extend metadata by analyzing a type for standard import usage (bottom-up step function)
extendMetaForType :: Environment.HaskellModuleMetadata -> Core.Type -> Environment.HaskellModuleMetadata
extendMetaForType meta typ =
    case (Strip.deannotateType typ) of
      Core.TypeLiteral v0 -> case v0 of
        Core.LiteralTypeBinary -> setMetaUsesByteString True meta
        Core.LiteralTypeInteger v1 -> case v1 of
          Core.IntegerTypeInt8 -> setMetaUsesInt True meta
          Core.IntegerTypeInt16 -> setMetaUsesInt True meta
          Core.IntegerTypeInt64 -> setMetaUsesInt True meta
          _ -> meta
        _ -> meta
      Core.TypeMap _ -> setMetaUsesMap True meta
      Core.TypeSet _ -> setMetaUsesSet True meta
      _ -> meta

-- | Find type variables that require an Ord constraint (used in maps or sets)
findOrdVariables :: Core.Type -> S.Set Core.Name
findOrdVariables typ =

      let fold =
              \names -> \typ_ -> case typ_ of
                Core.TypeMap v0 ->
                  let kt = Core.mapTypeKeys v0
                  in (tryType names kt)
                Core.TypeSet v0 -> tryType names v0
                _ -> names
          isTypeVariable = \v -> Maybes.isNothing (Names.namespaceOf v)
          tryType =
                  \names -> \t -> case (Strip.deannotateType t) of
                    Core.TypeVariable v0 -> Logic.ifElse (isTypeVariable v0) (Sets.insert v0 names) names
                    _ -> names
      in (Rewriting.foldOverType Coders.TraversalOrderPre fold Sets.empty typ)

-- | Gather metadata from definitions by bottom-up traversal of all terms and types
gatherMetadata :: [Packaging.Definition] -> Environment.HaskellModuleMetadata
gatherMetadata defs =

      let addDef =
              \meta -> \def -> case def of
                Packaging.DefinitionTerm v0 ->
                  let term = Packaging.termDefinitionTerm v0
                      metaWithTerm = Rewriting.foldOverTerm Coders.TraversalOrderPre (\m -> \t -> extendMetaForTerm m t) meta term
                  in (Maybes.maybe metaWithTerm (\ts -> Rewriting.foldOverType Coders.TraversalOrderPre (\m -> \t -> extendMetaForType m t) metaWithTerm (Core.typeSchemeType ts)) (Packaging.termDefinitionType v0))
                Packaging.DefinitionType v0 ->
                  let typ = Core.typeSchemeType (Packaging.typeDefinitionType v0)
                  in (Rewriting.foldOverType Coders.TraversalOrderPre (\m -> \t -> extendMetaForType m t) meta typ)
      in (Lists.foldl addDef emptyMetadata defs)

-- | Get implicit typeclass constraints for type variables that need Ord
getImplicitTypeClasses :: Core.Type -> M.Map Core.Name (S.Set Classes.TypeClass)
getImplicitTypeClasses typ =

      let toPair = \name -> (name, (Sets.fromList [
            Classes.TypeClassOrdering]))
      in (Maps.fromList (Lists.map toPair (Sets.toList (findOrdVariables typ))))

-- | Whether to include type definitions in generated Haskell modules
includeTypeDefinitions :: Bool
includeTypeDefinitions = False

-- | The key used to track Haskell variable depth in annotations
keyHaskellVar :: Core.Name
keyHaskellVar = Core.Name "haskellVar"

-- | Convert a Hydra module to Haskell source code as a filepath-to-content map
moduleToHaskell :: Packaging.Module -> [Packaging.Definition] -> Context.Context -> Graph.Graph -> Either Errors.Error (M.Map String String)
moduleToHaskell mod defs cx g =
    Eithers.bind (moduleToHaskellModule mod defs cx g) (\hsmod ->
      let s = Serialization.printExpr (Serialization.parenthesize (Serde.moduleToExpr hsmod))
          filepath =
                  Names.namespaceToFilePath Util.CaseConventionPascal (Packaging.FileExtension "hs") (Packaging.moduleNamespace mod)
      in (Right (Maps.singleton filepath s)))

-- | Convert a Hydra module and definitions to a Haskell module AST
moduleToHaskellModule :: Packaging.Module -> [Packaging.Definition] -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.Module
moduleToHaskellModule mod defs cx g =
    Eithers.bind (Utils.namespacesForModule mod cx g) (\namespaces -> constructModule namespaces mod defs cx g)

-- | Generate Haskell declarations for type and field name constants
nameDecls :: Packaging.Namespaces Syntax.ModuleName -> Core.Name -> Core.Type -> [Syntax.DeclarationWithComments]
nameDecls namespaces name typ =

      let nm = Core.unName name
          toDecl =
                  \n -> \pair ->
                    let k = Pairs.first pair
                        v = Pairs.second pair
                        decl =
                                Syntax.DeclarationValueBinding (Syntax.ValueBindingSimple (Syntax.SimpleValueBinding {
                                  Syntax.simpleValueBindingPattern = (Utils.applicationPattern (Utils.simpleName k) []),
                                  Syntax.simpleValueBindingRhs = (Syntax.RightHandSide (Syntax.ExpressionApplication (Syntax.ApplicationExpression {
                                    Syntax.applicationExpressionFunction = (Syntax.ExpressionVariable (Utils.elementReference namespaces n)),
                                    Syntax.applicationExpressionArgument = (Syntax.ExpressionLiteral (Syntax.LiteralString v))}))),
                                  Syntax.simpleValueBindingLocalBindings = Nothing}))
                    in Syntax.DeclarationWithComments {
                      Syntax.declarationWithCommentsBody = decl,
                      Syntax.declarationWithCommentsComments = Nothing}
          nameDecl = (constantForTypeName name, nm)
          fieldDecls = Lists.map toConstant (Lexical.fieldsOf typ)
          toConstant =
                  \fieldType ->
                    let fname = Core.fieldTypeName fieldType
                    in (constantForFieldName name fname, (Core.unName fname))
      in (Logic.ifElse useCoreImport (Lists.cons (toDecl (Core.Name "hydra.core.Name") nameDecl) (Lists.map (toDecl (Core.Name "hydra.core.Name")) fieldDecls)) [])

setMetaUsesByteString :: Bool -> Environment.HaskellModuleMetadata -> Environment.HaskellModuleMetadata
setMetaUsesByteString b m =
    Environment.HaskellModuleMetadata {
      Environment.haskellModuleMetadataUsesByteString = b,
      Environment.haskellModuleMetadataUsesInt = (Environment.haskellModuleMetadataUsesInt m),
      Environment.haskellModuleMetadataUsesMap = (Environment.haskellModuleMetadataUsesMap m),
      Environment.haskellModuleMetadataUsesSet = (Environment.haskellModuleMetadataUsesSet m)}

setMetaUsesInt :: Bool -> Environment.HaskellModuleMetadata -> Environment.HaskellModuleMetadata
setMetaUsesInt b m =
    Environment.HaskellModuleMetadata {
      Environment.haskellModuleMetadataUsesByteString = (Environment.haskellModuleMetadataUsesByteString m),
      Environment.haskellModuleMetadataUsesInt = b,
      Environment.haskellModuleMetadataUsesMap = (Environment.haskellModuleMetadataUsesMap m),
      Environment.haskellModuleMetadataUsesSet = (Environment.haskellModuleMetadataUsesSet m)}

setMetaUsesMap :: Bool -> Environment.HaskellModuleMetadata -> Environment.HaskellModuleMetadata
setMetaUsesMap b m =
    Environment.HaskellModuleMetadata {
      Environment.haskellModuleMetadataUsesByteString = (Environment.haskellModuleMetadataUsesByteString m),
      Environment.haskellModuleMetadataUsesInt = (Environment.haskellModuleMetadataUsesInt m),
      Environment.haskellModuleMetadataUsesMap = b,
      Environment.haskellModuleMetadataUsesSet = (Environment.haskellModuleMetadataUsesSet m)}

setMetaUsesSet :: Bool -> Environment.HaskellModuleMetadata -> Environment.HaskellModuleMetadata
setMetaUsesSet b m =
    Environment.HaskellModuleMetadata {
      Environment.haskellModuleMetadataUsesByteString = (Environment.haskellModuleMetadataUsesByteString m),
      Environment.haskellModuleMetadataUsesInt = (Environment.haskellModuleMetadataUsesInt m),
      Environment.haskellModuleMetadataUsesMap = (Environment.haskellModuleMetadataUsesMap m),
      Environment.haskellModuleMetadataUsesSet = b}

-- | Convert a Hydra term definition to a Haskell declaration with comments
toDataDeclaration :: Packaging.Namespaces Syntax.ModuleName -> Packaging.TermDefinition -> t0 -> Graph.Graph -> Either Errors.Error Syntax.DeclarationWithComments
toDataDeclaration namespaces def cx g =

      let name = Packaging.termDefinitionName def
          term = Packaging.termDefinitionTerm def
          typ = Packaging.termDefinitionType def
          hname = Utils.simpleName (Names.localNameOf name)
          rewriteValueBinding =
                  \vb -> case vb of
                    Syntax.ValueBindingSimple v0 ->
                      let pattern_ = Syntax.simpleValueBindingPattern v0
                          rhs = Syntax.simpleValueBindingRhs v0
                          bindings = Syntax.simpleValueBindingLocalBindings v0
                      in case pattern_ of
                        Syntax.PatternApplication v1 ->
                          let name_ = Syntax.applicationPatternName v1
                              args = Syntax.applicationPatternArgs v1
                              rhsExpr = Syntax.unRightHandSide rhs
                          in case rhsExpr of
                            Syntax.ExpressionLambda v2 ->
                              let vars = Syntax.lambdaExpressionBindings v2
                                  body = Syntax.lambdaExpressionInner v2
                                  newPattern = Utils.applicationPattern name_ (Lists.concat2 args vars)
                                  newRhs = Syntax.RightHandSide body
                              in (rewriteValueBinding (Syntax.ValueBindingSimple (Syntax.SimpleValueBinding {
                                Syntax.simpleValueBindingPattern = newPattern,
                                Syntax.simpleValueBindingRhs = newRhs,
                                Syntax.simpleValueBindingLocalBindings = bindings})))
                            _ -> vb
                        _ -> vb
          toDecl =
                  \comments -> \hname_ -> \term_ -> \bindings -> case (Strip.deannotateTerm term_) of
                    Core.TermLet v0 ->
                      let lbindings = Core.letBindings v0
                          env = Core.letBody v0
                          toTermDefinition = \hname_ -> \hterm_ -> Syntax.LocalBindingValue (Utils.simpleValueBinding hname_ hterm_ Nothing)
                          hnames = Lists.map (\binding -> Utils.simpleName (Core.unName (Core.bindingName binding))) lbindings
                          terms = Lists.map Core.bindingTerm lbindings
                      in (Eithers.bind (Eithers.mapList (\t -> encodeTerm 0 namespaces t cx g) terms) (\hterms ->
                        let hbindings = Lists.zipWith toTermDefinition hnames hterms
                            prevBindings = Maybes.maybe [] (\lb -> Syntax.unLocalBindings lb) bindings
                            allBindings = Lists.concat2 prevBindings hbindings
                        in (toDecl comments hname_ env (Just (Syntax.LocalBindings allBindings)))))
                    _ -> Eithers.bind (encodeTerm 0 namespaces term_ cx g) (\hterm ->
                      let vb = Utils.simpleValueBinding hname_ hterm bindings
                          schemeConstraints = Maybes.maybe Nothing (\ts -> Core.typeSchemeConstraints ts) typ
                          schemeClasses = typeSchemeConstraintsToClassMap schemeConstraints
                      in (Eithers.bind (Annotations.getTypeClasses cx g (Strip.removeTypesFromTerm term)) (\explicitClasses ->
                        let combinedClasses = Maps.union schemeClasses explicitClasses
                            schemeType = Maybes.maybe Core.TypeUnit (\ts -> Core.typeSchemeType ts) typ
                        in (Eithers.bind (encodeTypeWithClassAssertions namespaces combinedClasses schemeType cx g) (\htype ->
                          let decl =
                                  Syntax.DeclarationTypedBinding (Syntax.TypedBinding {
                                    Syntax.typedBindingTypeSignature = Syntax.TypeSignature {
                                      Syntax.typeSignatureName = hname_,
                                      Syntax.typeSignatureType = htype},
                                    Syntax.typedBindingValueBinding = (rewriteValueBinding vb)})
                          in (Right (Syntax.DeclarationWithComments {
                            Syntax.declarationWithCommentsBody = decl,
                            Syntax.declarationWithCommentsComments = comments})))))))
      in (Eithers.bind (Annotations.getTermDescription cx g term) (\comments -> toDecl comments hname term Nothing))

-- | Convert a Hydra type definition to Haskell declarations
toTypeDeclarationsFrom :: Packaging.Namespaces Syntax.ModuleName -> Core.Name -> Core.Type -> Context.Context -> Graph.Graph -> Either Errors.Error [Syntax.DeclarationWithComments]
toTypeDeclarationsFrom namespaces elementName typ cx g =

      let lname = Names.localNameOf elementName
          hname = Utils.simpleName lname
          declHead =
                  \name -> \vars_ -> Logic.ifElse (Lists.null vars_) (Syntax.DeclarationHeadSimple name) (
                    let h = Lists.head vars_
                        rest = Lists.tail vars_
                        hvar = Syntax.Variable (Utils.simpleName (Core.unName h))
                    in (Syntax.DeclarationHeadApplication (Syntax.ApplicationDeclarationHead {
                      Syntax.applicationDeclarationHeadFunction = (declHead name rest),
                      Syntax.applicationDeclarationHeadOperand = hvar})))
          newtypeCons =
                  \tname -> \typ_ ->
                    let hname0 = Utils.simpleName (Utils.newtypeAccessorName tname)
                    in (Eithers.bind (adaptTypeToHaskellAndEncode namespaces typ_ cx g) (\htype ->
                      let hfield =
                              Syntax.FieldWithComments {
                                Syntax.fieldWithCommentsField = Syntax.Field {
                                  Syntax.fieldName = hname0,
                                  Syntax.fieldType = htype},
                                Syntax.fieldWithCommentsComments = Nothing}
                          constructorName = Utils.simpleName (Names.localNameOf tname)
                      in (Right (Syntax.ConstructorWithComments {
                        Syntax.constructorWithCommentsBody = (Syntax.ConstructorRecord (Syntax.RecordConstructor {
                          Syntax.recordConstructorName = constructorName,
                          Syntax.recordConstructorFields = [
                            hfield]})),
                        Syntax.constructorWithCommentsComments = Nothing}))))
          recordCons =
                  \lname_ -> \fields ->
                    let toField =
                            \fieldType ->
                              let fname = Core.fieldTypeName fieldType
                                  ftype = Core.fieldTypeType fieldType
                                  hname_ = Utils.simpleName (Strings.cat2 (Formatting.decapitalize lname_) (Formatting.capitalize (Core.unName fname)))
                              in (Eithers.bind (adaptTypeToHaskellAndEncode namespaces ftype cx g) (\htype -> Eithers.bind (Annotations.getTypeDescription cx g ftype) (\comments -> Right (Syntax.FieldWithComments {
                                Syntax.fieldWithCommentsField = Syntax.Field {
                                  Syntax.fieldName = hname_,
                                  Syntax.fieldType = htype},
                                Syntax.fieldWithCommentsComments = comments}))))
                    in (Eithers.bind (Eithers.mapList toField fields) (\hFields -> Right (Syntax.ConstructorWithComments {
                      Syntax.constructorWithCommentsBody = (Syntax.ConstructorRecord (Syntax.RecordConstructor {
                        Syntax.recordConstructorName = (Utils.simpleName lname_),
                        Syntax.recordConstructorFields = hFields})),
                      Syntax.constructorWithCommentsComments = Nothing})))
          unionCons =
                  \boundNames_ -> \lname_ -> \fieldType ->
                    let fname = Core.fieldTypeName fieldType
                        ftype = Core.fieldTypeType fieldType
                        deconflict =
                                \name ->
                                  let tname =
                                          Names.unqualifyName (Packaging.QualifiedName {
                                            Packaging.qualifiedNameNamespace = (Just (Pairs.first (Packaging.namespacesFocus namespaces))),
                                            Packaging.qualifiedNameLocal = name})
                                  in (Logic.ifElse (Sets.member tname boundNames_) (deconflict (Strings.cat2 name "_")) name)
                    in (Eithers.bind (Annotations.getTypeDescription cx g ftype) (\comments ->
                      let nm = deconflict (Strings.cat2 (Formatting.capitalize lname_) (Formatting.capitalize (Core.unName fname)))
                      in (Eithers.bind (Logic.ifElse (Equality.equal (Strip.deannotateType ftype) Core.TypeUnit) (Right []) (Eithers.bind (adaptTypeToHaskellAndEncode namespaces ftype cx g) (\htype -> Right [
                        htype]))) (\typeList -> Right (Syntax.ConstructorWithComments {
                        Syntax.constructorWithCommentsBody = (Syntax.ConstructorOrdinary (Syntax.OrdinaryConstructor {
                          Syntax.ordinaryConstructorName = (Utils.simpleName nm),
                          Syntax.ordinaryConstructorFields = typeList})),
                        Syntax.constructorWithCommentsComments = comments})))))
      in (Eithers.bind (Predicates.isSerializableByName cx g elementName) (\isSer ->
        let deriv =
                Syntax.Deriving (Logic.ifElse isSer (Lists.map Utils.rawName [
                  "Eq",
                  "Ord",
                  "Read",
                  "Show"]) [])
            unpackResult = Utils.unpackForallType typ
            vars = Pairs.first unpackResult
            t_ = Pairs.second unpackResult
            hd = declHead hname (Lists.reverse vars)
        in (Eithers.bind (case (Strip.deannotateType t_) of
          Core.TypeRecord v0 -> Eithers.bind (recordCons lname v0) (\cons -> Right (Syntax.DeclarationData (Syntax.DataDeclaration {
            Syntax.dataDeclarationKeyword = Syntax.DataOrNewtypeData,
            Syntax.dataDeclarationContext = [],
            Syntax.dataDeclarationHead = hd,
            Syntax.dataDeclarationConstructors = [
              cons],
            Syntax.dataDeclarationDeriving = [
              deriv]})))
          Core.TypeUnion v0 -> Eithers.bind (Eithers.mapList (unionCons (Sets.fromList (Maps.keys (Graph.graphBoundTerms g))) lname) v0) (\cons -> Right (Syntax.DeclarationData (Syntax.DataDeclaration {
            Syntax.dataDeclarationKeyword = Syntax.DataOrNewtypeData,
            Syntax.dataDeclarationContext = [],
            Syntax.dataDeclarationHead = hd,
            Syntax.dataDeclarationConstructors = cons,
            Syntax.dataDeclarationDeriving = [
              deriv]})))
          Core.TypeWrap v0 -> Eithers.bind (newtypeCons elementName v0) (\cons -> Right (Syntax.DeclarationData (Syntax.DataDeclaration {
            Syntax.dataDeclarationKeyword = Syntax.DataOrNewtypeNewtype,
            Syntax.dataDeclarationContext = [],
            Syntax.dataDeclarationHead = hd,
            Syntax.dataDeclarationConstructors = [
              cons],
            Syntax.dataDeclarationDeriving = [
              deriv]})))
          _ -> Eithers.bind (adaptTypeToHaskellAndEncode namespaces typ cx g) (\htype -> Right (Syntax.DeclarationType (Syntax.TypeDeclaration {
            Syntax.typeDeclarationName = hd,
            Syntax.typeDeclarationType = htype})))) (\decl -> Eithers.bind (Annotations.getTypeDescription cx g typ) (\comments -> Eithers.bind (Logic.ifElse includeTypeDefinitions (Eithers.bind (typeDecl namespaces elementName typ cx g) (\decl_ -> Right [
          decl_])) (Right [])) (\tdecls ->
          let mainDecl =
                  Syntax.DeclarationWithComments {
                    Syntax.declarationWithCommentsBody = decl,
                    Syntax.declarationWithCommentsComments = comments}
              nameDecls_ = nameDecls namespaces elementName typ
          in (Right (Lists.concat [
            [
              mainDecl],
            nameDecls_,
            tdecls]))))))))

-- | Generate a Haskell declaration for a type definition constant
typeDecl :: Packaging.Namespaces Syntax.ModuleName -> Core.Name -> Core.Type -> t0 -> Graph.Graph -> Either Errors.Error Syntax.DeclarationWithComments
typeDecl namespaces name typ cx g =

      let typeName = \ns -> \name_ -> Names.qname ns (typeNameLocal name_)
          typeNameLocal =
                  \name_ -> Strings.cat [
                    "_",
                    (Names.localNameOf name_),
                    "_type_"]
          rawTerm = Core_.type_ typ
          rewrite =
                  \recurse -> \term ->
                    let variantResult =
                            case (Strip.deannotateTerm term) of
                              Core.TermUnion v0 -> Logic.ifElse (Equality.equal (Core.injectionTypeName v0) (Core.Name "hydra.core.Type")) (Just (Core.injectionField v0)) Nothing
                              _ -> Nothing
                        decodeString =
                                \term2 -> case (Strip.deannotateTerm term2) of
                                  Core.TermLiteral v0 -> case v0 of
                                    Core.LiteralString v1 -> Just v1
                                    _ -> Nothing
                                  _ -> Nothing
                        decodeName =
                                \term2 -> case (Strip.deannotateTerm term2) of
                                  Core.TermWrap v0 -> Logic.ifElse (Equality.equal (Core.wrappedTermTypeName v0) (Core.Name "hydra.core.Name")) (Maybes.map (\x -> Core.Name x) (decodeString (Core.wrappedTermBody v0))) Nothing
                                  _ -> Nothing
                        forType =
                                \field ->
                                  let fname = Core.fieldName field
                                      fterm = Core.fieldTerm field
                                  in (Logic.ifElse (Equality.equal fname (Core.Name "record")) Nothing (Logic.ifElse (Equality.equal fname (Core.Name "variable")) (Maybes.bind (decodeName fterm) forVariableType) Nothing))
                        forVariableType =
                                \vname ->
                                  let qname = Names.qualifyName vname
                                      mns = Packaging.qualifiedNameNamespace qname
                                      local = Packaging.qualifiedNameLocal qname
                                  in (Maybes.map (\ns -> Core.TermVariable (Names.qname ns (Strings.cat [
                                    "_",
                                    local,
                                    "_type_"]))) mns)
                    in (Maybes.fromMaybe (recurse term) (Maybes.bind variantResult forType))
          finalTerm = Rewriting.rewriteTerm rewrite rawTerm
      in (Eithers.bind (encodeTerm 0 namespaces finalTerm cx g) (\expr ->
        let rhs = Syntax.RightHandSide expr
            hname = Utils.simpleName (typeNameLocal name)
            pat = Utils.applicationPattern hname []
            decl =
                    Syntax.DeclarationValueBinding (Syntax.ValueBindingSimple (Syntax.SimpleValueBinding {
                      Syntax.simpleValueBindingPattern = pat,
                      Syntax.simpleValueBindingRhs = rhs,
                      Syntax.simpleValueBindingLocalBindings = Nothing}))
        in (Right (Syntax.DeclarationWithComments {
          Syntax.declarationWithCommentsBody = decl,
          Syntax.declarationWithCommentsComments = Nothing}))))

-- | Convert type scheme constraints to a map of type variables to typeclasses
typeSchemeConstraintsToClassMap :: Ord t0 => (Maybe (M.Map t0 Core.TypeVariableMetadata) -> M.Map t0 (S.Set Classes.TypeClass))
typeSchemeConstraintsToClassMap maybeConstraints =

      let nameToTypeClass =
              \className ->
                let classNameStr = Core.unName className
                    isEq = Equality.equal classNameStr (Core.unName (Core.Name "equality"))
                    isOrd = Equality.equal classNameStr (Core.unName (Core.Name "ordering"))
                in (Logic.ifElse isEq (Just Classes.TypeClassEquality) (Logic.ifElse isOrd (Just Classes.TypeClassOrdering) Nothing))
      in (Maybes.maybe Maps.empty (\constraints -> Maps.map (\meta -> Sets.fromList (Maybes.cat (Lists.map nameToTypeClass (Sets.toList (Core.typeVariableMetadataClasses meta))))) constraints) maybeConstraints)

-- | Whether to use the Hydra core import in generated modules
useCoreImport :: Bool
useCoreImport = True
