-- Note: this is an automatically generated file. Do not edit.

-- | Functions for encoding Hydra modules as Haskell modules

module Hydra.Ext.Haskell.Coder where

import qualified Hydra.Adapt as Adapt
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Classes as Classes
import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as Core_
import qualified Hydra.Errors as Errors
import qualified Hydra.Ext.Haskell.Ast as Ast
import qualified Hydra.Ext.Haskell.Language as Language
import qualified Hydra.Ext.Haskell.Serde as Serde
import qualified Hydra.Ext.Haskell.Utils as Utils
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
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
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Show.Core as Core__
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Whether to include type definitions in generated Haskell modules
includeTypeDefinitions :: Bool
includeTypeDefinitions = False

-- | Whether to use the Hydra core import in generated modules
useCoreImport :: Bool
useCoreImport = True

-- | The key used to track Haskell variable depth in annotations
keyHaskellVar :: Core.Name
keyHaskellVar = Core.Name "haskellVar"

-- | Adapt a Hydra type to Haskell's type system and encode it
adaptTypeToHaskellAndEncode :: Module.Namespaces Ast.ModuleName -> Core.Type -> Context.Context -> t0 -> Either (Context.InContext Errors.Error) Ast.Type
adaptTypeToHaskellAndEncode namespaces typ cx g =

      let enc = \t -> encodeType namespaces t cx g
      in case (Rewriting.deannotateType typ) of
        Core.TypeVariable _ -> enc typ
        _ -> Eithers.bind (Eithers.bimap (\_s -> Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError _s)),
          Context.inContextContext = cx}) (\_x -> _x) (Adapt.adaptTypeForLanguage Language.haskellLanguage typ)) (\adaptedType -> enc adaptedType)

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
constructModule :: Module.Namespaces Ast.ModuleName -> Module.Module -> [Module.Definition] -> Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) Ast.Module
constructModule namespaces mod defs cx g =

      let h = \namespace -> Module.unNamespace namespace
          createDeclarations =
                  \def -> case def of
                    Module.DefinitionType v0 ->
                      let name = Module.typeDefinitionName v0
                          typ = Module.typeDefinitionType v0
                      in (toTypeDeclarationsFrom namespaces name typ cx g)
                    Module.DefinitionTerm v0 -> Eithers.bind (toDataDeclaration namespaces v0 cx g) (\d -> Right [
                      d])
          importName =
                  \name -> Ast.ModuleName (Strings.intercalate "." (Lists.map Formatting.capitalize (Strings.splitOn "." name)))
          imports = Lists.concat2 domainImports standardImports
          domainImports =

                    let toImport =
                            \pair ->
                              let namespace = Pairs.first pair
                                  alias = Pairs.second pair
                                  name = h namespace
                              in Ast.Import {
                                Ast.importQualified = True,
                                Ast.importModule = (importName name),
                                Ast.importAs = (Just alias),
                                Ast.importSpec = Nothing}
                    in (Lists.map toImport (Maps.toList (Module.namespacesMapping namespaces)))
          standardImports =

                    let toImport =
                            \triple ->
                              let name = Pairs.first (Pairs.first triple)
                                  malias = Pairs.second (Pairs.first triple)
                                  hidden = Pairs.second triple
                                  spec =
                                          Logic.ifElse (Lists.null hidden) Nothing (Just (Ast.SpecImportHiding (Lists.map (\n -> Ast.ImportExportSpec {
                                            Ast.importExportSpecModifier = Nothing,
                                            Ast.importExportSpecName = (Utils.simpleName n),
                                            Ast.importExportSpecSubspec = Nothing}) hidden)))
                              in Ast.Import {
                                Ast.importQualified = (Maybes.isJust malias),
                                Ast.importModule = (Ast.ModuleName name),
                                Ast.importAs = (Maybes.map (\x -> Ast.ModuleName x) malias),
                                Ast.importSpec = spec}
                    in (Lists.map toImport (Lists.concat2 [
                      (("Prelude", Nothing), [
                        "Enum",
                        "Ordering",
                        "decodeFloat",
                        "encodeFloat",
                        "fail",
                        "map",
                        "pure",
                        "sum"]),
                      (("Data.ByteString", (Just "B")), []),
                      (("Data.Int", (Just "I")), []),
                      (("Data.List", (Just "L")), []),
                      (("Data.Map", (Just "M")), []),
                      (("Data.Set", (Just "S")), [])] (Logic.ifElse (Schemas.moduleContainsBinaryLiterals mod) [
                      (("Hydra.Lib.Literals", (Just "Literals")), [])] [])))
      in (Eithers.bind (Eithers.mapList createDeclarations defs) (\declLists ->
        let decls = Lists.concat declLists
            mc = Module.moduleDescription mod
        in (Right (Ast.Module {
          Ast.moduleHead = (Just (Ast.ModuleHead {
            Ast.moduleHeadComments = mc,
            Ast.moduleHeadName = (importName (h (Module.moduleNamespace mod))),
            Ast.moduleHeadExports = []})),
          Ast.moduleImports = imports,
          Ast.moduleDeclarations = decls}))))

-- | Encode a Hydra case statement as a Haskell case expression with a given scrutinee
encodeCaseExpression :: Int -> Module.Namespaces Ast.ModuleName -> Core.CaseStatement -> Ast.Expression -> Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) Ast.Expression
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
                        rhsTerm = Rewriting.simplifyTerm raw
                        v1 = Logic.ifElse (Rewriting.isFreeVariableInTerm (Core.Name v0) rhsTerm) Constants.ignoredVariable v0
                        hname = Utils.unionFieldReference (Sets.fromList (Maps.keys (Graph.graphBoundTerms g))) namespaces dn fn
                    in (Eithers.bind (Maybes.cases (Maps.lookup fn fieldMap) (Left (Context.InContext {
                      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                        "field ",
                        (Literals.showString (Core.unName fn)),
                        " not found in ",
                        (Literals.showString (Core.unName dn))]))),
                      Context.inContextContext = cx})) (\fieldType ->
                      let ft = Core.fieldTypeType fieldType
                          noArgs = []
                          singleArg = [
                                Ast.PatternName (Utils.rawName v1)]
                      in case (Rewriting.deannotateType ft) of
                        Core.TypeUnit -> Right noArgs
                        _ -> Right singleArg)) (\args ->
                      let lhs = Utils.applicationPattern hname args
                      in (Eithers.bind (Eithers.map (\x -> Ast.CaseRhs x) (encodeTerm (Math.add depth 1) namespaces rhsTerm cx g)) (\rhs -> Right (Ast.Alternative {
                        Ast.alternativePattern = lhs,
                        Ast.alternativeRhs = rhs,
                        Ast.alternativeBinds = Nothing})))))
      in (Eithers.bind (Schemas.requireUnionType cx g dn) (\rt ->
        let toFieldMapEntry = \f -> (Core.fieldTypeName f, f)
            fieldMap = Maps.fromList (Lists.map toFieldMapEntry rt)
        in (Eithers.bind (Eithers.mapList (toAlt fieldMap) fields) (\ecases -> Eithers.bind (Maybes.cases def (Right []) (\d -> Eithers.bind (Eithers.map (\x -> Ast.CaseRhs x) (encodeTerm depth namespaces d cx g)) (\cs ->
          let lhs = Ast.PatternName (Utils.rawName Constants.ignoredVariable)
              alt =
                      Ast.Alternative {
                        Ast.alternativePattern = lhs,
                        Ast.alternativeRhs = cs,
                        Ast.alternativeBinds = Nothing}
          in (Right [
            alt])))) (\dcases -> Right (Ast.ExpressionCase (Ast.CaseExpression {
          Ast.caseExpressionCase = scrutinee,
          Ast.caseExpressionAlternatives = (Lists.concat2 ecases dcases)})))))))

-- | Encode a Hydra function as a Haskell expression
encodeFunction :: Int -> Module.Namespaces Ast.ModuleName -> Core.Function -> Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) Ast.Expression
encodeFunction depth namespaces fun cx g =
    case fun of
      Core.FunctionElimination v0 -> case v0 of
        Core.EliminationWrap v1 -> Right (Ast.ExpressionVariable (Utils.elementReference namespaces (Names.qname (Maybes.fromJust (Names.namespaceOf v1)) (Utils.newtypeAccessorName v1))))
        Core.EliminationRecord v1 ->
          let dn = Core.projectionTypeName v1
              fname = Core.projectionField v1
          in (Right (Ast.ExpressionVariable (Utils.recordFieldReference namespaces dn fname)))
        Core.EliminationUnion v1 -> Eithers.map (Utils.hslambda (Utils.rawName "x")) (encodeCaseExpression depth namespaces v1 (Utils.hsvar "x") cx g)
      Core.FunctionLambda v0 ->
        let v = Core.lambdaParameter v0
            body = Core.lambdaBody v0
        in (Eithers.bind (encodeTerm depth namespaces body cx g) (\hbody -> Right (Utils.hslambda (Utils.elementReference namespaces v) hbody)))
      Core.FunctionPrimitive v0 -> Right (Ast.ExpressionVariable (Utils.elementReference namespaces v0))

-- | Encode a Hydra literal as a Haskell expression
encodeLiteral :: Core.Literal -> Context.Context -> Either (Context.InContext Errors.Error) Ast.Expression
encodeLiteral l cx =
    case l of
      Core.LiteralBinary v0 -> Right (Utils.hsapp (Utils.hsvar "Literals.stringToBinary") (Utils.hslit (Ast.LiteralString (Literals.binaryToString v0))))
      Core.LiteralBoolean v0 -> Right (Utils.hsvar (Logic.ifElse v0 "True" "False"))
      Core.LiteralFloat v0 -> case v0 of
        Core.FloatValueFloat32 v1 -> Right (Utils.hslit (Ast.LiteralFloat v1))
        Core.FloatValueFloat64 v1 -> Right (Utils.hslit (Ast.LiteralDouble v1))
        Core.FloatValueBigfloat v1 -> Right (Utils.hslit (Ast.LiteralDouble (Literals.bigfloatToFloat64 v1)))
      Core.LiteralInteger v0 -> case v0 of
        Core.IntegerValueBigint v1 -> Right (Utils.hslit (Ast.LiteralInteger v1))
        Core.IntegerValueInt8 v1 -> Right (Utils.hslit (Ast.LiteralInteger (Literals.int8ToBigint v1)))
        Core.IntegerValueInt16 v1 -> Right (Utils.hslit (Ast.LiteralInteger (Literals.int16ToBigint v1)))
        Core.IntegerValueInt32 v1 -> Right (Utils.hslit (Ast.LiteralInt v1))
        Core.IntegerValueInt64 v1 -> Right (Utils.hslit (Ast.LiteralInteger (Literals.int64ToBigint v1)))
        Core.IntegerValueUint8 v1 -> Right (Utils.hslit (Ast.LiteralInteger (Literals.uint8ToBigint v1)))
        Core.IntegerValueUint16 v1 -> Right (Utils.hslit (Ast.LiteralInteger (Literals.uint16ToBigint v1)))
        Core.IntegerValueUint32 v1 -> Right (Utils.hslit (Ast.LiteralInteger (Literals.uint32ToBigint v1)))
        Core.IntegerValueUint64 v1 -> Right (Utils.hslit (Ast.LiteralInteger (Literals.uint64ToBigint v1)))
      Core.LiteralString v0 -> Right (Utils.hslit (Ast.LiteralString v0))
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "literal value " (Core__.literal l)))),
        Context.inContextContext = cx})

-- | Encode a Hydra term as a Haskell expression
encodeTerm :: Int -> Module.Namespaces Ast.ModuleName -> Core.Term -> Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) Ast.Expression
encodeTerm depth namespaces term cx g =

      let encode = \t -> encodeTerm depth namespaces t cx g
          nonemptyMap =
                  \m ->
                    let lhs = Utils.hsvar "M.fromList"
                        encodePair =
                                \pair ->
                                  let k = Pairs.first pair
                                      v = Pairs.second pair
                                  in (Eithers.bind (encode k) (\hk -> Eithers.bind (encode v) (\hv -> Right (Ast.ExpressionTuple [
                                    hk,
                                    hv]))))
                    in (Eithers.bind (Eithers.map (\x -> Ast.ExpressionList x) (Eithers.mapList encodePair (Maps.toList m))) (\rhs -> Right (Utils.hsapp lhs rhs)))
          nonemptySet =
                  \s ->
                    let lhs = Utils.hsvar "S.fromList"
                    in (Eithers.bind (encodeTerm depth namespaces (Core.TermList (Sets.toList s)) cx g) (\rhs -> Right (Utils.hsapp lhs rhs)))
      in case (Rewriting.deannotateTerm term) of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
              deannotatedFun = Rewriting.deannotateTerm fun
          in case deannotatedFun of
            Core.TermFunction v1 -> case v1 of
              Core.FunctionElimination v2 -> case v2 of
                Core.EliminationUnion v3 -> Eithers.bind (encode arg) (\harg -> encodeCaseExpression depth namespaces v3 harg cx g)
                _ -> Eithers.bind (encode fun) (\hfun -> Eithers.bind (encode arg) (\harg -> Right (Utils.hsapp hfun harg)))
              _ -> Eithers.bind (encode fun) (\hfun -> Eithers.bind (encode arg) (\harg -> Right (Utils.hsapp hfun harg)))
            _ -> Eithers.bind (encode fun) (\hfun -> Eithers.bind (encode arg) (\harg -> Right (Utils.hsapp hfun harg)))
        Core.TermEither v0 -> Eithers.either (\l -> Eithers.bind (encode l) (\hl -> Right (Utils.hsapp (Utils.hsvar "Left") hl))) (\r -> Eithers.bind (encode r) (\hr -> Right (Utils.hsapp (Utils.hsvar "Right") hr))) v0
        Core.TermFunction v0 -> encodeFunction depth namespaces v0 cx g
        Core.TermLet v0 ->
          let collectBindings =
                  \lt ->
                    let bs = Core.letBindings lt
                        body = Core.letBody lt
                    in case (Rewriting.deannotateTerm body) of
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
                        in (Eithers.bind (encode term_) (\hexpr -> Right (Ast.LocalBindingValue (Utils.simpleValueBinding hname hexpr Nothing))))
          in (Eithers.bind (Eithers.mapList encodeBinding allBindings) (\hbindings -> Eithers.bind (encode finalBody) (\hinner -> Right (Ast.ExpressionLet (Ast.LetExpression {
            Ast.letExpressionBindings = hbindings,
            Ast.letExpressionInner = hinner})))))
        Core.TermList v0 -> Eithers.bind (Eithers.mapList encode v0) (\helems -> Right (Ast.ExpressionList helems))
        Core.TermLiteral v0 -> encodeLiteral v0 cx
        Core.TermMap v0 -> Logic.ifElse (Maps.null v0) (Right (Utils.hsvar "M.empty")) (nonemptyMap v0)
        Core.TermMaybe v0 -> Maybes.cases v0 (Right (Utils.hsvar "Nothing")) (\t -> Eithers.bind (encode t) (\ht -> Right (Utils.hsapp (Utils.hsvar "Just") ht)))
        Core.TermPair v0 -> Eithers.bind (encode (Pairs.first v0)) (\f -> Eithers.bind (encode (Pairs.second v0)) (\s -> Right (Ast.ExpressionTuple [
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
                        in (Eithers.bind (encode ft) (\hft -> Right (Ast.FieldUpdate {
                          Ast.fieldUpdateName = fieldRef,
                          Ast.fieldUpdateValue = hft})))
              typeName = Utils.elementReference namespaces sname
          in (Eithers.bind (Eithers.mapList toFieldUpdate fields) (\updates -> Right (Ast.ExpressionConstructRecord (Ast.ConstructRecordExpression {
            Ast.constructRecordExpressionName = typeName,
            Ast.constructRecordExpressionFields = updates}))))
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
                      Ast.ExpressionVariable (Utils.unionFieldReference (Sets.fromList (Maps.keys (Graph.graphBoundTerms g))) namespaces sname fn)
              dflt = Eithers.map (Utils.hsapp lhs) (encode ft)
          in (Eithers.bind (Schemas.requireUnionField cx g sname fn) (\ftyp -> case (Rewriting.deannotateType ftyp) of
            Core.TypeUnit -> Right lhs
            _ -> dflt))
        Core.TermUnit -> Right (Ast.ExpressionTuple [])
        Core.TermVariable v0 -> Right (Ast.ExpressionVariable (Utils.elementReference namespaces v0))
        Core.TermWrap v0 ->
          let tname = Core.wrappedTermTypeName v0
              term_ = Core.wrappedTermBody v0
              lhs = Ast.ExpressionVariable (Utils.elementReference namespaces tname)
          in (Eithers.bind (encode term_) (\rhs -> Right (Utils.hsapp lhs rhs)))
        _ -> Left (Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "unexpected term: " (Core__.term term)))),
          Context.inContextContext = cx})

-- | Encode a Hydra type as a Haskell type
encodeType :: Module.Namespaces Ast.ModuleName -> Core.Type -> Context.Context -> t0 -> Either (Context.InContext Errors.Error) Ast.Type
encodeType namespaces typ cx g =

      let encode = \t -> encodeType namespaces t cx g
          ref = \name -> Right (Ast.TypeVariable (Utils.elementReference namespaces name))
          unitTuple = Ast.TypeTuple []
      in case (Rewriting.deannotateType typ) of
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
            Ast.TypeVariable (Utils.rawName "Either"),
            hleft,
            hright]))))
        Core.TypeFunction v0 ->
          let dom = Core.functionTypeDomain v0
              cod = Core.functionTypeCodomain v0
          in (Eithers.bind (encode dom) (\hdom -> Eithers.bind (encode cod) (\hcod -> Right (Ast.TypeFunction (Ast.FunctionType {
            Ast.functionTypeDomain = hdom,
            Ast.functionTypeCodomain = hcod})))))
        Core.TypeForall v0 ->
          let v = Core.forallTypeParameter v0
              body = Core.forallTypeBody v0
          in (encode body)
        Core.TypeList v0 -> Eithers.bind (encode v0) (\hlt -> Right (Ast.TypeList hlt))
        Core.TypeLiteral v0 -> case v0 of
          Core.LiteralTypeBinary -> Right (Ast.TypeVariable (Utils.rawName "B.ByteString"))
          Core.LiteralTypeBoolean -> Right (Ast.TypeVariable (Utils.rawName "Bool"))
          Core.LiteralTypeFloat v1 -> case v1 of
            Core.FloatTypeFloat32 -> Right (Ast.TypeVariable (Utils.rawName "Float"))
            Core.FloatTypeFloat64 -> Right (Ast.TypeVariable (Utils.rawName "Double"))
            Core.FloatTypeBigfloat -> Right (Ast.TypeVariable (Utils.rawName "Double"))
          Core.LiteralTypeInteger v1 -> case v1 of
            Core.IntegerTypeBigint -> Right (Ast.TypeVariable (Utils.rawName "Integer"))
            Core.IntegerTypeInt8 -> Right (Ast.TypeVariable (Utils.rawName "I.Int8"))
            Core.IntegerTypeInt16 -> Right (Ast.TypeVariable (Utils.rawName "I.Int16"))
            Core.IntegerTypeInt32 -> Right (Ast.TypeVariable (Utils.rawName "Int"))
            Core.IntegerTypeInt64 -> Right (Ast.TypeVariable (Utils.rawName "I.Int64"))
            _ -> Left (Context.InContext {
              Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "unexpected integer type: " (Core__.integerType v1)))),
              Context.inContextContext = cx})
          Core.LiteralTypeString -> Right (Ast.TypeVariable (Utils.rawName "String"))
          _ -> Left (Context.InContext {
            Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "unexpected literal type: " (Core__.literalType v0)))),
            Context.inContextContext = cx})
        Core.TypeMap v0 ->
          let kt = Core.mapTypeKeys v0
              vt = Core.mapTypeValues v0
          in (Eithers.bind (encode kt) (\hkt -> Eithers.bind (encode vt) (\hvt -> Right (Utils.toTypeApplication [
            Ast.TypeVariable (Utils.rawName "M.Map"),
            hkt,
            hvt]))))
        Core.TypeMaybe v0 -> Eithers.bind (encode v0) (\hot -> Right (Utils.toTypeApplication [
          Ast.TypeVariable (Utils.rawName "Maybe"),
          hot]))
        Core.TypePair v0 -> Eithers.bind (encode (Core.pairTypeFirst v0)) (\f -> Eithers.bind (encode (Core.pairTypeSecond v0)) (\s -> Right (Ast.TypeTuple [
          f,
          s])))
        Core.TypeRecord _ -> ref (Core.Name "placeholder")
        Core.TypeSet v0 -> Eithers.bind (encode v0) (\hst -> Right (Utils.toTypeApplication [
          Ast.TypeVariable (Utils.rawName "S.Set"),
          hst]))
        Core.TypeUnion _ -> ref (Core.Name "placeholder")
        Core.TypeUnit -> Right unitTuple
        Core.TypeVariable v0 -> ref v0
        Core.TypeWrap _ -> ref (Core.Name "placeholder")
        _ -> Left (Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "unexpected type: " (Core__.type_ typ)))),
          Context.inContextContext = cx})

-- | Encode a Hydra type as a Haskell type with typeclass assertions
encodeTypeWithClassAssertions :: Module.Namespaces Ast.ModuleName -> M.Map Core.Name (S.Set Classes.TypeClass) -> Core.Type -> Context.Context -> t0 -> Either (Context.InContext Errors.Error) Ast.Type
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
                        htype = Ast.TypeVariable (Utils.rawName (Core.unName name))
                    in (Ast.AssertionClass (Ast.ClassAssertion {
                      Ast.classAssertionName = hname,
                      Ast.classAssertionTypes = [
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
            hassert = Logic.ifElse (Equality.equal (Lists.length encoded) 1) (Lists.head encoded) (Ast.AssertionTuple encoded)
        in (Right (Ast.TypeCtx (Ast.ContextType {
          Ast.contextTypeCtx = hassert,
          Ast.contextTypeType = htyp}))))))

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
                  \names -> \t -> case (Rewriting.deannotateType t) of
                    Core.TypeVariable v0 -> Logic.ifElse (isTypeVariable v0) (Sets.insert v0 names) names
                    _ -> names
      in (Rewriting.foldOverType Coders.TraversalOrderPre fold Sets.empty typ)

-- | Get implicit typeclass constraints for type variables that need Ord
getImplicitTypeClasses :: Core.Type -> M.Map Core.Name (S.Set Classes.TypeClass)
getImplicitTypeClasses typ =

      let toPair = \name -> (name, (Sets.fromList [
            Classes.TypeClassOrdering]))
      in (Maps.fromList (Lists.map toPair (Sets.toList (findOrdVariables typ))))

-- | Convert a Hydra module and definitions to a Haskell module AST
moduleToHaskellModule :: Module.Module -> [Module.Definition] -> Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) Ast.Module
moduleToHaskellModule mod defs cx g =
    Eithers.bind (Utils.namespacesForModule mod cx g) (\namespaces -> constructModule namespaces mod defs cx g)

-- | Convert a Hydra module to Haskell source code as a filepath-to-content map
moduleToHaskell :: Module.Module -> [Module.Definition] -> Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) (M.Map String String)
moduleToHaskell mod defs cx g =
    Eithers.bind (moduleToHaskellModule mod defs cx g) (\hsmod ->
      let s = Serialization.printExpr (Serialization.parenthesize (Serde.moduleToExpr hsmod))
          filepath = Names.namespaceToFilePath Util.CaseConventionPascal (Module.FileExtension "hs") (Module.moduleNamespace mod)
      in (Right (Maps.singleton filepath s)))

-- | Generate Haskell declarations for type and field name constants
nameDecls :: Module.Namespaces Ast.ModuleName -> Core.Name -> Core.Type -> [Ast.DeclarationWithComments]
nameDecls namespaces name typ =

      let nm = Core.unName name
          toDecl =
                  \n -> \pair ->
                    let k = Pairs.first pair
                        v = Pairs.second pair
                        decl =
                                Ast.DeclarationValueBinding (Ast.ValueBindingSimple (Ast.SimpleValueBinding {
                                  Ast.simpleValueBindingPattern = (Utils.applicationPattern (Utils.simpleName k) []),
                                  Ast.simpleValueBindingRhs = (Ast.RightHandSide (Ast.ExpressionApplication (Ast.ApplicationExpression {
                                    Ast.applicationExpressionFunction = (Ast.ExpressionVariable (Utils.elementReference namespaces n)),
                                    Ast.applicationExpressionArgument = (Ast.ExpressionLiteral (Ast.LiteralString v))}))),
                                  Ast.simpleValueBindingLocalBindings = Nothing}))
                    in Ast.DeclarationWithComments {
                      Ast.declarationWithCommentsBody = decl,
                      Ast.declarationWithCommentsComments = Nothing}
          nameDecl = (constantForTypeName name, nm)
          fieldDecls = Lists.map toConstant (Lexical.fieldsOf typ)
          toConstant =
                  \fieldType ->
                    let fname = Core.fieldTypeName fieldType
                    in (constantForFieldName name fname, (Core.unName fname))
      in (Logic.ifElse useCoreImport (Lists.cons (toDecl (Core.Name "hydra.core.Name") nameDecl) (Lists.map (toDecl (Core.Name "hydra.core.Name")) fieldDecls)) [])

-- | Convert a Hydra term definition to a Haskell declaration with comments
toDataDeclaration :: Module.Namespaces Ast.ModuleName -> Module.TermDefinition -> Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) Ast.DeclarationWithComments
toDataDeclaration namespaces def cx g =

      let name = Module.termDefinitionName def
          term = Module.termDefinitionTerm def
          typ = Module.termDefinitionType def
          hname = Utils.simpleName (Names.localNameOf name)
          rewriteValueBinding =
                  \vb -> case vb of
                    Ast.ValueBindingSimple v0 ->
                      let pattern_ = Ast.simpleValueBindingPattern v0
                          rhs = Ast.simpleValueBindingRhs v0
                          bindings = Ast.simpleValueBindingLocalBindings v0
                      in case pattern_ of
                        Ast.PatternApplication v1 ->
                          let name_ = Ast.applicationPatternName v1
                              args = Ast.applicationPatternArgs v1
                              rhsExpr = Ast.unRightHandSide rhs
                          in case rhsExpr of
                            Ast.ExpressionLambda v2 ->
                              let vars = Ast.lambdaExpressionBindings v2
                                  body = Ast.lambdaExpressionInner v2
                                  newPattern = Utils.applicationPattern name_ (Lists.concat2 args vars)
                                  newRhs = Ast.RightHandSide body
                              in (rewriteValueBinding (Ast.ValueBindingSimple (Ast.SimpleValueBinding {
                                Ast.simpleValueBindingPattern = newPattern,
                                Ast.simpleValueBindingRhs = newRhs,
                                Ast.simpleValueBindingLocalBindings = bindings})))
                            _ -> vb
                        _ -> vb
          toDecl =
                  \comments -> \hname_ -> \term_ -> \bindings -> case (Rewriting.deannotateTerm term_) of
                    Core.TermLet v0 ->
                      let lbindings = Core.letBindings v0
                          env = Core.letBody v0
                          toBinding = \hname_ -> \hterm_ -> Ast.LocalBindingValue (Utils.simpleValueBinding hname_ hterm_ Nothing)
                          hnames = Lists.map (\binding -> Utils.simpleName (Core.unName (Core.bindingName binding))) lbindings
                          terms = Lists.map Core.bindingTerm lbindings
                      in (Eithers.bind (Eithers.mapList (\t -> encodeTerm 0 namespaces t cx g) terms) (\hterms ->
                        let hbindings = Lists.zipWith toBinding hnames hterms
                            prevBindings = Maybes.maybe [] (\lb -> Ast.unLocalBindings lb) bindings
                            allBindings = Lists.concat2 prevBindings hbindings
                        in (toDecl comments hname_ env (Just (Ast.LocalBindings allBindings)))))
                    _ -> Eithers.bind (encodeTerm 0 namespaces term_ cx g) (\hterm ->
                      let vb = Utils.simpleValueBinding hname_ hterm bindings
                          schemeConstraints = Core.typeSchemeConstraints typ
                          schemeClasses = typeSchemeConstraintsToClassMap schemeConstraints
                      in (Eithers.bind (Annotations.getTypeClasses cx g (Rewriting.removeTypesFromTerm term)) (\explicitClasses ->
                        let combinedClasses = Maps.union schemeClasses explicitClasses
                        in (Eithers.bind (encodeTypeWithClassAssertions namespaces combinedClasses (Core.typeSchemeType typ) cx g) (\htype ->
                          let decl =
                                  Ast.DeclarationTypedBinding (Ast.TypedBinding {
                                    Ast.typedBindingTypeSignature = Ast.TypeSignature {
                                      Ast.typeSignatureName = hname_,
                                      Ast.typeSignatureType = htype},
                                    Ast.typedBindingValueBinding = (rewriteValueBinding vb)})
                          in (Right (Ast.DeclarationWithComments {
                            Ast.declarationWithCommentsBody = decl,
                            Ast.declarationWithCommentsComments = comments})))))))
      in (Eithers.bind (Annotations.getTermDescription cx g term) (\comments -> toDecl comments hname term Nothing))

-- | Convert a Hydra type definition to Haskell declarations
toTypeDeclarationsFrom :: Module.Namespaces Ast.ModuleName -> Core.Name -> Core.Type -> Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) [Ast.DeclarationWithComments]
toTypeDeclarationsFrom namespaces elementName typ cx g =

      let lname = Names.localNameOf elementName
          hname = Utils.simpleName lname
          declHead =
                  \name -> \vars_ -> Logic.ifElse (Lists.null vars_) (Ast.DeclarationHeadSimple name) (
                    let h = Lists.head vars_
                        rest = Lists.tail vars_
                        hvar = Ast.Variable (Utils.simpleName (Core.unName h))
                    in (Ast.DeclarationHeadApplication (Ast.ApplicationDeclarationHead {
                      Ast.applicationDeclarationHeadFunction = (declHead name rest),
                      Ast.applicationDeclarationHeadOperand = hvar})))
          newtypeCons =
                  \tname -> \typ_ ->
                    let hname0 = Utils.simpleName (Utils.newtypeAccessorName tname)
                    in (Eithers.bind (adaptTypeToHaskellAndEncode namespaces typ_ cx g) (\htype ->
                      let hfield =
                              Ast.FieldWithComments {
                                Ast.fieldWithCommentsField = Ast.Field {
                                  Ast.fieldName = hname0,
                                  Ast.fieldType = htype},
                                Ast.fieldWithCommentsComments = Nothing}
                          constructorName = Utils.simpleName (Names.localNameOf tname)
                      in (Right (Ast.ConstructorWithComments {
                        Ast.constructorWithCommentsBody = (Ast.ConstructorRecord (Ast.RecordConstructor {
                          Ast.recordConstructorName = constructorName,
                          Ast.recordConstructorFields = [
                            hfield]})),
                        Ast.constructorWithCommentsComments = Nothing}))))
          recordCons =
                  \lname_ -> \fields ->
                    let toField =
                            \fieldType ->
                              let fname = Core.fieldTypeName fieldType
                                  ftype = Core.fieldTypeType fieldType
                                  hname_ = Utils.simpleName (Strings.cat2 (Formatting.decapitalize lname_) (Formatting.capitalize (Core.unName fname)))
                              in (Eithers.bind (adaptTypeToHaskellAndEncode namespaces ftype cx g) (\htype -> Eithers.bind (Annotations.getTypeDescription cx g ftype) (\comments -> Right (Ast.FieldWithComments {
                                Ast.fieldWithCommentsField = Ast.Field {
                                  Ast.fieldName = hname_,
                                  Ast.fieldType = htype},
                                Ast.fieldWithCommentsComments = comments}))))
                    in (Eithers.bind (Eithers.mapList toField fields) (\hFields -> Right (Ast.ConstructorWithComments {
                      Ast.constructorWithCommentsBody = (Ast.ConstructorRecord (Ast.RecordConstructor {
                        Ast.recordConstructorName = (Utils.simpleName lname_),
                        Ast.recordConstructorFields = hFields})),
                      Ast.constructorWithCommentsComments = Nothing})))
          unionCons =
                  \boundNames_ -> \lname_ -> \fieldType ->
                    let fname = Core.fieldTypeName fieldType
                        ftype = Core.fieldTypeType fieldType
                        deconflict =
                                \name ->
                                  let tname =
                                          Names.unqualifyName (Module.QualifiedName {
                                            Module.qualifiedNameNamespace = (Just (Pairs.first (Module.namespacesFocus namespaces))),
                                            Module.qualifiedNameLocal = name})
                                  in (Logic.ifElse (Sets.member tname boundNames_) (deconflict (Strings.cat2 name "_")) name)
                    in (Eithers.bind (Annotations.getTypeDescription cx g ftype) (\comments ->
                      let nm = deconflict (Strings.cat2 (Formatting.capitalize lname_) (Formatting.capitalize (Core.unName fname)))
                      in (Eithers.bind (Logic.ifElse (Equality.equal (Rewriting.deannotateType ftype) Core.TypeUnit) (Right []) (Eithers.bind (adaptTypeToHaskellAndEncode namespaces ftype cx g) (\htype -> Right [
                        htype]))) (\typeList -> Right (Ast.ConstructorWithComments {
                        Ast.constructorWithCommentsBody = (Ast.ConstructorOrdinary (Ast.OrdinaryConstructor {
                          Ast.ordinaryConstructorName = (Utils.simpleName nm),
                          Ast.ordinaryConstructorFields = typeList})),
                        Ast.constructorWithCommentsComments = comments})))))
      in (Eithers.bind (Schemas.isSerializableByName cx g elementName) (\isSer ->
        let deriv =
                Ast.Deriving (Logic.ifElse isSer (Lists.map Utils.rawName [
                  "Eq",
                  "Ord",
                  "Read",
                  "Show"]) [])
            unpackResult = Utils.unpackForallType typ
            vars = Pairs.first unpackResult
            t_ = Pairs.second unpackResult
            hd = declHead hname (Lists.reverse vars)
        in (Eithers.bind (case (Rewriting.deannotateType t_) of
          Core.TypeRecord v0 -> Eithers.bind (recordCons lname v0) (\cons -> Right (Ast.DeclarationData (Ast.DataDeclaration {
            Ast.dataDeclarationKeyword = Ast.DataOrNewtypeData,
            Ast.dataDeclarationContext = [],
            Ast.dataDeclarationHead = hd,
            Ast.dataDeclarationConstructors = [
              cons],
            Ast.dataDeclarationDeriving = [
              deriv]})))
          Core.TypeUnion v0 -> Eithers.bind (Eithers.mapList (unionCons (Sets.fromList (Maps.keys (Graph.graphBoundTerms g))) lname) v0) (\cons -> Right (Ast.DeclarationData (Ast.DataDeclaration {
            Ast.dataDeclarationKeyword = Ast.DataOrNewtypeData,
            Ast.dataDeclarationContext = [],
            Ast.dataDeclarationHead = hd,
            Ast.dataDeclarationConstructors = cons,
            Ast.dataDeclarationDeriving = [
              deriv]})))
          Core.TypeWrap v0 -> Eithers.bind (newtypeCons elementName v0) (\cons -> Right (Ast.DeclarationData (Ast.DataDeclaration {
            Ast.dataDeclarationKeyword = Ast.DataOrNewtypeNewtype,
            Ast.dataDeclarationContext = [],
            Ast.dataDeclarationHead = hd,
            Ast.dataDeclarationConstructors = [
              cons],
            Ast.dataDeclarationDeriving = [
              deriv]})))
          _ -> Eithers.bind (adaptTypeToHaskellAndEncode namespaces typ cx g) (\htype -> Right (Ast.DeclarationType (Ast.TypeDeclaration {
            Ast.typeDeclarationName = hd,
            Ast.typeDeclarationType = htype})))) (\decl -> Eithers.bind (Annotations.getTypeDescription cx g typ) (\comments -> Eithers.bind (Logic.ifElse includeTypeDefinitions (Eithers.bind (typeDecl namespaces elementName typ cx g) (\decl_ -> Right [
          decl_])) (Right [])) (\tdecls ->
          let mainDecl =
                  Ast.DeclarationWithComments {
                    Ast.declarationWithCommentsBody = decl,
                    Ast.declarationWithCommentsComments = comments}
              nameDecls_ = nameDecls namespaces elementName typ
          in (Right (Lists.concat [
            [
              mainDecl],
            nameDecls_,
            tdecls]))))))))

-- | Generate a Haskell declaration for a type definition constant
typeDecl :: Module.Namespaces Ast.ModuleName -> Core.Name -> Core.Type -> Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) Ast.DeclarationWithComments
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
                            case (Rewriting.deannotateTerm term) of
                              Core.TermUnion v0 -> Logic.ifElse (Equality.equal (Core.injectionTypeName v0) (Core.Name "hydra.core.Type")) (Just (Core.injectionField v0)) Nothing
                              _ -> Nothing
                        decodeString =
                                \term -> case (Rewriting.deannotateTerm term) of
                                  Core.TermLiteral v0 -> case v0 of
                                    Core.LiteralString v1 -> Just v1
                                    _ -> Nothing
                                  _ -> Nothing
                        decodeName =
                                \term -> case (Rewriting.deannotateTerm term) of
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
                                      mns = Module.qualifiedNameNamespace qname
                                      local = Module.qualifiedNameLocal qname
                                  in (Maybes.map (\ns -> Core.TermVariable (Names.qname ns (Strings.cat [
                                    "_",
                                    local,
                                    "_type_"]))) mns)
                    in (Maybes.fromMaybe (recurse term) (Maybes.bind variantResult forType))
          finalTerm = Rewriting.rewriteTerm rewrite rawTerm
      in (Eithers.bind (encodeTerm 0 namespaces finalTerm cx g) (\expr ->
        let rhs = Ast.RightHandSide expr
            hname = Utils.simpleName (typeNameLocal name)
            pat = Utils.applicationPattern hname []
            decl =
                    Ast.DeclarationValueBinding (Ast.ValueBindingSimple (Ast.SimpleValueBinding {
                      Ast.simpleValueBindingPattern = pat,
                      Ast.simpleValueBindingRhs = rhs,
                      Ast.simpleValueBindingLocalBindings = Nothing}))
        in (Right (Ast.DeclarationWithComments {
          Ast.declarationWithCommentsBody = decl,
          Ast.declarationWithCommentsComments = Nothing}))))

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
