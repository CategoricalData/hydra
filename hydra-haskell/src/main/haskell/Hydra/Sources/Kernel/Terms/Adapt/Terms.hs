
module Hydra.Sources.Kernel.Terms.Adapt.Terms where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  fieldAdapter, forTypeReference, functionProxyName, functionProxyType, functionToUnion,
  lambdaToMonotype, maybeToList, passApplication, passEither, passForall, passFunction, passList,
  passLiteral, passMap, passOptional, passRecord, passSet, passUnion, passUnit, passWrapped,
  setToList, simplifyApplication, unitToRecord, unionToRecord, wrapToUnwrapped,
  termAdapter)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors    as Accessors
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Meta.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Meta.Coders       as Coders
import qualified Hydra.Dsl.Meta.Compute      as Compute
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Grammar      as Grammar
import qualified Hydra.Dsl.Grammars          as Grammars
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Meta.Json         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Meta.Module       as Module
import qualified Hydra.Dsl.Meta.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Meta.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Meta.Typing       as Typing
import qualified Hydra.Dsl.Meta.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils    as AdaptUtils
import qualified Hydra.Sources.CoderUtils                  as CoderUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore

import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Meta.Error        as Error
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Error     as ShowError


ns :: Namespace
ns = Namespace "hydra.adapt.terms"

module_ :: Module
module_ = Module ns elements
    [AdaptLiterals.ns, AdaptUtils.ns, CoderUtils.ns, Annotations.ns, ExtractCore.ns,
      Reflect.ns, Rewriting.ns, Schemas.ns, ShowCore.ns, ShowError.ns]
    kernelTypesNamespaces $
    Just "Adapter framework for types and terms"
  where
    elements = [
      toBinding fieldAdapter,
      toBinding forTypeReference,
      toBinding functionProxyName,
      toBinding functionProxyType,
      toBinding functionToUnion,
      toBinding lambdaToMonotype,
      toBinding maybeToList,
      toBinding passApplication,
      toBinding passEither,
      toBinding passFunction,
      toBinding passForall,
      toBinding passLiteral,
      toBinding passList,
      toBinding passMap,
      toBinding passOptional,
      toBinding passRecord,
      toBinding passSet,
      toBinding passUnion,
      toBinding passUnit,
      toBinding passWrapped,
      toBinding setToList,
      toBinding simplifyApplication,
      toBinding termAdapter,
      toBinding unionToRecord,
      toBinding unitToRecord,
      toBinding wrapToUnwrapped]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

formatError :: TTerm (InContext Error -> String)
formatError = "ic" ~> ShowError.error_ @@ Ctx.inContextObject (var "ic")

fieldAdapter :: TBinding (AdapterContext -> FieldType -> Either String (SymmetricAdapter FieldType Field))
fieldAdapter = define "fieldAdapter" $
  doc "Create an adapter for field types" $
  "cx" ~> "ftyp" ~>
  "encdec" <~ ("ad" ~> "dir" ~> "cx" ~> "field" ~>
    "name" <~ Core.fieldName (var "field") $
    "term" <~ Core.fieldTerm (var "field") $
    "newTerm" <<~ AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "cx" @@ var "term" $
    right $ Core.field (var "name") (var "newTerm")) $
  "ad" <<~ termAdapter @@ var "cx" @@ (Core.fieldTypeType (var "ftyp")) $
  right (Compute.adapter
    (Compute.adapterIsLossy (var "ad"))
    (var "ftyp")
    (Core.fieldType (Core.fieldTypeName (var "ftyp")) (Compute.adapterTarget (var "ad")))
    (AdaptUtils.bidirectional @@ (var "encdec" @@ var "ad")))

forTypeReference :: TBinding (AdapterContext -> Name -> Either String (SymmetricAdapter Type Term))
forTypeReference = define "forTypeReference" $
  doc "This function accounts for recursive type definitions" $
  "cx" ~> "name" ~>
  "encdec" <~ ("name" ~> "adapters0" ~> "dir" ~> "cx" ~> "term" ~>
    Maybes.maybe
      (Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat2 (string "no adapter for reference type ") (unwrap _Name @@ var "name"))) (var "cx"))
      ("ad" ~> AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "cx" @@ var "term")
      (Maps.lookup (var "name") (var "adapters0"))) $
  "forType" <~ ("cx2" ~> "adapters0" ~> "t" ~>
    "actual" <<~ termAdapter @@ var "cx2" @@ var "t" $
    right (var "actual")) $
  "forMissingAdapter" <~ ("cx2" ~> "lossy" ~> "adapters0" ~> "placeholder" ~>
    "newAdapters" <~ Maps.insert (var "name") (var "placeholder") (var "adapters0") $
    "newCx" <~ Coders.adapterContext
      (Coders.adapterContextGraph (var "cx2"))
      (Coders.adapterContextLanguage (var "cx2"))
      (var "newAdapters") $
    "mt" <~ Schemas.resolveType @@ (Coders.adapterContextGraph (var "newCx")) @@ (Core.typeVariable (var "name")) $
    Maybes.maybe
      (right (Compute.adapter (var "lossy") (Core.typeVariable (var "name")) (Core.typeVariable (var "name"))
        (AdaptUtils.bidirectional @@ ("dir" ~> "_cx" ~> "term" ~> right (var "term")))))
      (var "forType" @@ var "cx2" @@ var "adapters0")
      (var "mt")) $
  "lossy" <~ false $
  "adapters" <~ Coders.adapterContextAdapters (var "cx") $
  "placeholder" <~ Compute.adapter (var "lossy") (Core.typeVariable (var "name")) (Core.typeVariable (var "name"))
    (AdaptUtils.bidirectional @@ (var "encdec" @@ var "name" @@ var "adapters")) $
  Maybes.maybe
    (var "forMissingAdapter" @@ var "cx" @@ var "lossy" @@ var "adapters" @@ var "placeholder")
    (unaryFunction right)
    (Maps.lookup (var "name") (var "adapters"))

functionProxyName :: TBinding Name
functionProxyName = define "functionProxyName" $
  Core.name (string "hydra.core.FunctionProxy")

functionProxyType :: TBinding (a -> Type)
functionProxyType = define "functionProxyType" $
  doc "Generate a function proxy type for a given domain type" $
  constant (Core.typeUnion (Core.rowType (functionProxyName) (list [
    Core.fieldType (Core.nameLift _Elimination_wrap) MetaTypes.string,
    Core.fieldType (Core.nameLift _Elimination_record) MetaTypes.string,
    Core.fieldType (Core.nameLift _Elimination_union) MetaTypes.string,
    Core.fieldType (Core.nameLift _Function_lambda) MetaTypes.string,
    Core.fieldType (Core.nameLift _Function_primitive) MetaTypes.string,
    Core.fieldType (Core.nameLift _Term_variable) MetaTypes.string])))

functionToUnion :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
functionToUnion = define "functionToUnion" $
  doc "Convert function types to union types" $
  "cx" ~> "t" ~>
  "encTerm" <~ ("term" ~> "strippedTerm" ~> cases _Term (var "strippedTerm")
    Nothing [
    _Term_function>>: "f" ~> cases _Function (var "f")
      Nothing [
      _Function_elimination>>: "e" ~> cases _Elimination (var "e")
        Nothing [
        _Elimination_wrap>>: "name" ~> Core.termUnion (Core.injection (functionProxyName)
          (Core.field (Core.nameLift _Elimination_wrap) (MetaTerms.stringLift (unwrap _Name @@ var "name")))),
        _Elimination_record>>: "r" ~> Core.termUnion (Core.injection (functionProxyName)
          (Core.field (Core.nameLift _Elimination_record) (MetaTerms.stringLift (ShowCore.term @@ var "term")))),
        _Elimination_union>>: "u" ~> Core.termUnion (Core.injection (functionProxyName)
          (Core.field (Core.nameLift _Elimination_union) (MetaTerms.stringLift (ShowCore.term @@ var "term"))))],
      _Function_lambda>>: "l" ~> Core.termUnion (Core.injection (functionProxyName)
        (Core.field (Core.nameLift _Function_lambda) (MetaTerms.stringLift (ShowCore.term @@ var "term")))),
      _Function_primitive>>: "name" ~> Core.termUnion (Core.injection (functionProxyName)
        (Core.field (Core.nameLift _Function_primitive) (MetaTerms.stringLift (unwrap _Name @@ var "name"))))],
    _Term_variable>>: "name" ~>
      Core.termUnion (Core.injection (functionProxyName) (Core.field (Core.nameLift _Term_variable) (MetaTerms.stringLift (unwrap _Name @@ var "name"))))]) $
  "encode" <~ ("ad" ~> "cx" ~> "term" ~>
    "strippedTerm" <~ Rewriting.deannotateTerm @@ var "term" $
    Compute.coderEncode (Compute.adapterCoder (var "ad")) @@ var "cx" @@ (var "encTerm" @@ var "term" @@ var "strippedTerm")) $
  "readFromString" <~ ("cx" ~> "graph" ~> "term" ~>
    "s" <<~ ExtractCore.string @@ var "cx" @@ var "graph" @@ var "term" $
    Maybes.maybe
      (Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat2 (string "failed to parse term: ") (var "s"))) (var "cx"))
      (unaryFunction right)
      (ShowCore.readTerm @@ var "s")) $
  "decode" <~ ("graph" ~> "ad" ~> "cx" ~> "term" ~>
    "notFound" <~ ("fname" ~> Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat2 (string "unexpected field: ") (unwrap _Name @@ var "fname"))) (var "cx")) $
    "forCases" <~ ("fterm" ~> var "readFromString" @@ var "cx" @@ var "graph" @@ var "fterm") $
    "forLambda" <~ ("fterm" ~> var "readFromString" @@ var "cx" @@ var "graph" @@ var "fterm") $
    "forWrapped" <~ ("fterm" ~>
      "s" <<~ ExtractCore.string @@ var "cx" @@ var "graph" @@ var "fterm" $
      right (MetaTerms.unwrap (Core.name (var "s")))) $
    "forPrimitive" <~ ("fterm" ~>
      "s" <<~ ExtractCore.string @@ var "cx" @@ var "graph" @@ var "fterm" $
      right (MetaTerms.primitiveLift (Core.name (var "s")))) $
    "forProjection" <~ ("fterm" ~> var "readFromString" @@ var "cx" @@ var "graph" @@ var "fterm") $
    "forVariable" <~ ("fterm" ~>
      "s" <<~ ExtractCore.string @@ var "cx" @@ var "graph" @@ var "fterm" $
      right (Core.termVariable (Core.name (var "s")))) $
    "injTerm" <<~ Compute.coderDecode (Compute.adapterCoder (var "ad")) @@ var "cx" @@ var "term" $
    "field" <<~ ExtractCore.injection @@ var "cx" @@ (functionProxyName) @@ var "graph" @@ var "injTerm" $
    "fname" <~ Core.fieldName (var "field") $
    "fterm" <~ Core.fieldTerm (var "field") $
    Maybes.fromMaybe (var "notFound" @@ var "fname") (Maps.lookup (var "fname") (Maps.fromList (list [
      pair (Core.nameLift _Elimination_wrap) (var "forWrapped" @@ var "fterm"),
      pair (Core.nameLift _Elimination_record) (var "forProjection" @@ var "fterm"),
      pair (Core.nameLift _Elimination_union) (var "forCases" @@ var "fterm"),
      pair (Core.nameLift _Function_lambda) (var "forLambda" @@ var "fterm"),
      pair (Core.nameLift _Function_primitive) (var "forPrimitive" @@ var "fterm"),
      pair (Core.nameLift _Term_variable) (var "forVariable" @@ var "fterm")])))) $
  cases _Type (var "t")
    Nothing [
    _Type_function>>: "ft" ~>
      "dom" <~ Core.functionTypeDomain (var "ft") $
      "cod" <~ Core.functionTypeCodomain (var "ft") $
      "unionType" <~ (
        "domAd" <<~ termAdapter @@ var "cx" @@ var "dom" $
        right (Core.typeUnion (Core.rowType (functionProxyName) (list [
          Core.fieldType (Core.nameLift _Elimination_wrap) MetaTypes.string,
          Core.fieldType (Core.nameLift _Elimination_record) MetaTypes.string,
          Core.fieldType (Core.nameLift _Elimination_union) MetaTypes.string,
          Core.fieldType (Core.nameLift _Function_lambda) MetaTypes.string,
          Core.fieldType (Core.nameLift _Function_primitive) MetaTypes.string,
          Core.fieldType (Core.nameLift _Term_variable) MetaTypes.string])))) $
      "ut" <<~ var "unionType" $
      "ad" <<~ termAdapter @@ var "cx" @@ var "ut" $
      "graph" <~ Coders.adapterContextGraph (var "cx") $
      right (Compute.adapter
        (Compute.adapterIsLossy (var "ad"))
        (var "t")
        (Compute.adapterTarget (var "ad"))
        (Compute.coder (var "encode" @@ var "ad") (var "decode" @@ var "graph" @@ var "ad")))]

lambdaToMonotype :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
lambdaToMonotype = define "lambdaToMonotype" $
  doc "Convert forall types to monotypes" $
  "cx" ~> "t" ~> cases _Type (var "t")
    Nothing [
    _Type_forall>>: "ft" ~>
      "body" <~ Core.forallTypeBody (var "ft") $
      "ad" <<~ termAdapter @@ var "cx" @@ var "body" $
      right (Compute.adapter
        (Compute.adapterIsLossy (var "ad"))
        (var "t")
        (Compute.adapterTarget (var "ad"))
        (Compute.adapterCoder (var "ad")))]

maybeToList :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
maybeToList = define "maybeToList" $
  doc "Convert optional types to list types" $
  "cx" ~> "t" ~>
  "encode" <~ ("ad" ~> "cx" ~> "term" ~> cases _Term (var "term")
    Nothing [
    _Term_maybe>>: "m" ~> Maybes.maybe
      (right (MetaTerms.list []))
      ("r" ~>
        "encoded" <<~ AdaptUtils.encodeDecode @@ Coders.coderDirectionEncode @@ (Compute.adapterCoder (var "ad")) @@ var "cx" @@ var "r" $
        right (Core.termList (list [var "encoded"])))
      (var "m")]) $
  "decode" <~ ("ad" ~> "cx" ~> "term" ~> cases _Term (var "term")
    Nothing [
    _Term_list>>: "l" ~> Eithers.map (unaryFunction Core.termMaybe) (Logic.ifElse (Lists.null (var "l"))
      (right nothing)
      ("decoded" <<~ AdaptUtils.encodeDecode @@ Coders.coderDirectionDecode @@ (Compute.adapterCoder (var "ad")) @@ var "cx" @@ (Lists.head (var "l")) $
       right (just (var "decoded"))))]) $
  cases _Type (var "t")
    Nothing [
    _Type_maybe>>: "ot" ~>
      "ad" <<~ termAdapter @@ var "cx" @@ var "ot" $
      right (Compute.adapter
        false
        (var "t")
        (Core.typeList (Compute.adapterTarget (var "ad")))
        (Compute.coder (var "encode" @@ var "ad") (var "decode" @@ var "ad")))]

passApplication :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
passApplication = define "passApplication" $
  doc "Pass through application types" $
  "cx" ~> "t" ~>
  "forApplicationType" <~ ("at" ~>
    "lhs" <~ Core.applicationTypeFunction (var "at") $
    "rhs" <~ Core.applicationTypeArgument (var "at") $
    "lhsAd" <<~ termAdapter @@ var "cx" @@ var "lhs" $
    "rhsAd" <<~ termAdapter @@ var "cx" @@ var "rhs" $
    right (Compute.adapter
      (Logic.or (Compute.adapterIsLossy (var "lhsAd")) (Compute.adapterIsLossy (var "rhsAd")))
      (var "t")
      (Core.typeApplication (Core.applicationType (Compute.adapterTarget (var "lhsAd")) (Compute.adapterTarget (var "rhsAd"))))
      (AdaptUtils.bidirectional @@
        ("dir" ~> "cx" ~> "term" ~> AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "lhsAd")) @@ var "cx" @@ var "term")))) $
  cases _Type (var "t")
    Nothing [
    _Type_application>>: "at" ~> var "forApplicationType" @@ var "at"]

passEither :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
passEither = define "passEither" $
  doc "Pass through either types" $
  "cx" ~> "t" ~>
  "forEitherType" <~ ("et" ~>
    "left" <~ Core.eitherTypeLeft (var "et") $
    "right" <~ Core.eitherTypeRight (var "et") $
    "leftAd" <<~ termAdapter @@ var "cx" @@ var "left" $
    "rightAd" <<~ termAdapter @@ var "cx" @@ var "right" $
    right (Compute.adapter
      (Logic.or (Compute.adapterIsLossy (var "leftAd")) (Compute.adapterIsLossy (var "rightAd")))
      (var "t")
      (Core.typeEither (Core.eitherType (Compute.adapterTarget (var "leftAd")) (Compute.adapterTarget (var "rightAd"))))
      (AdaptUtils.bidirectional @@
        ("dir" ~> "cx" ~> "term" ~> AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "leftAd")) @@ var "cx" @@ var "term")))) $
  cases _Type (var "t")
    Nothing [
    _Type_either>>: "et" ~> var "forEitherType" @@ var "et"]

passForall :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
passForall = define "passForall" $
  doc "Pass through forall types" $
  "cx" ~> "t" ~>
  "forForallType" <~ ("ft" ~>
    "v" <~ Core.forallTypeParameter (var "ft") $
    "body" <~ Core.forallTypeBody (var "ft") $
    "ad" <<~ termAdapter @@ var "cx" @@ var "body" $
    right (Compute.adapter
      (Compute.adapterIsLossy (var "ad"))
      (var "t")
      (Core.typeForall (Core.forallType (var "v") (Compute.adapterTarget (var "ad"))))
      (AdaptUtils.bidirectional @@ ("dir" ~> "cx" ~> "term" ~>
        AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "cx" @@ var "term")))) $
  cases _Type (var "t")
    Nothing [
    _Type_forall>>: "ft" ~> var "forForallType" @@ var "ft"]

passFunction :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
passFunction = define "passFunction" $
  doc "Pass through function types with adaptation" $
  "cx" ~> "t" ~>
  "toCaseAds" <~ ("dom" ~> "cod" ~> cases _Type (Rewriting.deannotateType @@ var "dom")
    (Just (right Maps.empty)) [
    _Type_union >>: "rt" ~>
      "pairs" <<~ Eithers.mapList
        ("f" ~>
          "ad" <<~ fieldAdapter @@ var "cx" @@ Core.fieldType
            (Core.fieldTypeName (var "f"))
            (Core.typeFunction (Core.functionType
              (Core.fieldTypeType (var "f"))
              (var "cod"))) $
          right (pair (Core.fieldTypeName (var "f")) (var "ad")))
        (Core.rowTypeFields (var "rt")) $
      right (Maps.fromList (var "pairs"))]) $
  "toOptionAd" <~ ("dom" ~> "cod" ~> cases _Type (Rewriting.deannotateType @@ var "dom")
    (Just (right nothing)) [
    _Type_maybe >>: "ot" ~>
      Eithers.map (unaryFunction just) (termAdapter @@ var "cx" @@ MetaTypes.function (var "ot") (var "cod"))]) $
  "getCoder" <~ ("caseAds" ~> "fname" ~> Maybes.maybe
    AdaptUtils.idCoder
    (unaryFunction Compute.adapterCoder)
    (Maps.lookup (var "fname") (var "caseAds"))) $
  "forElimination" <~ ("dir" ~> "cx" ~> "codAd" ~> "caseAds" ~> "e" ~> cases _Elimination (var "e")
    Nothing [
    _Elimination_union >>: "cs" ~>
      "n" <~ Core.caseStatementTypeName (var "cs") $
      "def" <~ Core.caseStatementDefault (var "cs") $
      "cases" <~ Core.caseStatementCases (var "cs") $
      "rcases" <<~ Eithers.mapList
        ("f" ~> AdaptUtils.encodeDecode @@ var "dir" @@ (var "getCoder" @@ var "caseAds" @@ Core.fieldName (var "f")) @@ var "cx" @@ var "f")
        (var "cases") $
      "rdef" <<~ Eithers.mapMaybe
        ("d" ~> AdaptUtils.encodeDecode @@ var "dir" @@ Compute.adapterCoder (var "codAd") @@ var "cx" @@ var "d")
        (var "def") $
      right (Core.eliminationUnion (Core.caseStatement (var "n") (var "rdef") (var "rcases")))]) $
  "forFunction" <~ ("dir" ~> "cx" ~> "codAd" ~> "caseAds" ~> "f" ~> cases _Function (var "f")
    Nothing [
    _Function_elimination >>: "e" ~>
      Eithers.map (unaryFunction Core.functionElimination)
        (var "forElimination" @@ var "dir" @@ var "cx" @@ var "codAd" @@ var "caseAds" @@ var "e"),
    _Function_lambda >>: "l" ~>
      "var" <~ Core.lambdaParameter (var "l") $
      "d" <~ Core.lambdaDomain (var "l") $
      "body" <~ Core.lambdaBody (var "l") $
      "newBody" <<~ AdaptUtils.encodeDecode @@ var "dir" @@ Compute.adapterCoder (var "codAd") @@ var "cx" @@ var "body" $
      right (Core.functionLambda (Core.lambda (var "var") (var "d") (var "newBody"))),
    _Function_primitive >>: "name" ~> right (Core.functionPrimitive (var "name"))]) $
  "encdec" <~ ("codAd" ~> "caseAds" ~> "dir" ~> "cx" ~> "term" ~> cases _Term (Rewriting.deannotateTerm @@ var "term")
    (Just (right (var "term"))) [
    _Term_function >>: "f" ~> Eithers.map (unaryFunction Core.termFunction)
      (var "forFunction" @@ var "dir" @@ var "cx" @@ var "codAd" @@ var "caseAds" @@ var "f")]) $
  "forFunctionType" <~ ("ft" ~>
    "dom" <~ Core.functionTypeDomain (var "ft") $
    "cod" <~ Core.functionTypeCodomain (var "ft") $
    "domAd" <<~ termAdapter @@ var "cx" @@ var "dom" $
    "codAd" <<~ termAdapter @@ var "cx" @@ var "cod" $
    "caseAds" <<~ var "toCaseAds" @@ var "dom" @@ var "cod" $
    "optionAd" <<~ var "toOptionAd" @@ var "dom" @@ var "cod" $
    "lossy" <~ Logic.or
      (Compute.adapterIsLossy (var "codAd"))
      (Logic.ors (Lists.map ("pair" ~> Compute.adapterIsLossy (Pairs.second (var "pair"))) (Maps.toList (var "caseAds")))) $
    "target" <~ MetaTypes.function (Compute.adapterTarget (var "domAd")) (Compute.adapterTarget (var "codAd")) $
    right $ Compute.adapter (var "lossy") (var "t") (var "target")
      (AdaptUtils.bidirectional @@ (var "encdec" @@ var "codAd" @@ var "caseAds"))) $
  cases _Type (var "t")
    Nothing [
    _Type_function >>: "ft" ~> var "forFunctionType" @@ var "ft"]

passList :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
passList = define "passList" $
  doc "Pass through list types" $
  "cx" ~> "t" ~>
  "encdec" <~ ("ad" ~> "dir" ~> "cx" ~> "term" ~> cases _Term (var "term")
    Nothing [
    _Term_list>>: "terms" ~>
      "newTerms" <<~ Eithers.mapList (AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "cx") (var "terms") $
      right (Core.termList (var "newTerms"))]) $
  "forListType" <~ ("lt" ~>
    "ad" <<~ termAdapter @@ var "cx" @@ var "lt" $
    right (Compute.adapter
      (Compute.adapterIsLossy (var "ad"))
      (var "t")
      (MetaTypes.list (Compute.adapterTarget (var "ad")))
      (AdaptUtils.bidirectional @@ (var "encdec" @@ var "ad")))) $
  cases _Type (var "t")
    Nothing [
    _Type_list>>: "lt" ~> var "forListType" @@ var "lt"]

passLiteral :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
passLiteral = define "passLiteral" $
  doc "Pass through literal types with literal adaptation" $
  "cx" ~> "t" ~>
  "encdec" <~ ("graph" ~> "ad" ~> "dir" ~> "cx" ~> "term" ~>
    "l" <<~ ExtractCore.literal @@ var "cx" @@ var "graph" @@ var "term" $
    "l2" <<~ AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "cx" @@ var "l" $
    right $ Core.termLiteral $ var "l2") $
  "forLiteral" <~ ("lt" ~>
    "ad" <<~ AdaptLiterals.literalAdapter @@ var "cx" @@ var "lt" $
    "step" <~ AdaptUtils.bidirectional @@ (var "encdec" @@ Coders.adapterContextGraph (var "cx") @@ var "ad") $
    right (Compute.adapter
      (Compute.adapterIsLossy (var "ad"))
      (Core.typeLiteral (Compute.adapterSource (var "ad")))
      (Core.typeLiteral (Compute.adapterTarget (var "ad")))
      (var "step"))) $
  cases _Type (var "t")
    Nothing [
    _Type_literal>>: "lt" ~> var "forLiteral" @@ var "lt"]

passMap :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
passMap = define "passMap" $
  doc "Pass through map types" $
  "cx" ~> "t" ~>
  "encdec" <~ ("kad" ~> "vad" ~> "dir" ~> "cx" ~> "term" ~> cases _Term (var "term")
    Nothing [
    _Term_map>>: "m" ~>
      "newPairs" <<~ Eithers.mapList
        ("pair" ~>
          "k" <~ Pairs.first (var "pair") $
          "v" <~ Pairs.second (var "pair") $
          "newK" <<~ AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "kad")) @@ var "cx" @@ var "k" $
          "newV" <<~ AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "vad")) @@ var "cx" @@ var "v" $
          right (pair (var "newK") (var "newV")))
        (Maps.toList (var "m")) $
      right (Core.termMap (Maps.fromList (var "newPairs")))]) $
  "forMapType" <~ ("mt" ~>
    "kt" <~ Core.mapTypeKeys (var "mt") $
    "vt" <~ Core.mapTypeValues (var "mt") $
    "kad" <<~ termAdapter @@ var "cx" @@ var "kt" $
    "vad" <<~ termAdapter @@ var "cx" @@ var "vt" $
    right (Compute.adapter
      (Logic.or (Compute.adapterIsLossy (var "kad")) (Compute.adapterIsLossy (var "vad")))
      (var "t")
      (MetaTypes.map (Compute.adapterTarget (var "kad")) (Compute.adapterTarget (var "vad")))
      (AdaptUtils.bidirectional @@ (var "encdec" @@ var "kad" @@ var "vad")))) $
  cases _Type (var "t")
    Nothing [
    _Type_map>>: "mt" ~> var "forMapType" @@ var "mt"]

passOptional :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
passOptional = define "passOptional" $
  doc "Pass through optional types" $
  "cx" ~> "t" ~>
  "mapTerm" <~ ("graph" ~> "coder" ~> "dir" ~> "cx" ~> "term" ~>
    "opt" <<~ ExtractCore.maybeTerm @@ var "cx" @@ unaryFunction right @@ var "graph" @@ var "term" $
    "newOpt" <<~ Eithers.mapMaybe (AdaptUtils.encodeDecode @@ var "dir" @@ var "coder" @@ var "cx") (var "opt") $
    right (Core.termMaybe (var "newOpt"))) $
  cases _Type (var "t")
    Nothing [
    _Type_maybe>>: "ot" ~>
      "adapter" <<~ termAdapter @@ var "cx" @@ var "ot" $
      right (Compute.adapter
        (Compute.adapterIsLossy (var "adapter"))
        (var "t")
        (Core.typeMaybe (Compute.adapterTarget (var "adapter")))
        (AdaptUtils.bidirectional @@ (var "mapTerm" @@ Coders.adapterContextGraph (var "cx") @@ (Compute.adapterCoder (var "adapter")))))]

passRecord :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
passRecord = define "passRecord" $
  doc "Pass through record types" $
  "cx" ~> "t" ~>
  "encdec" <~ ("rt" ~> "adapters" ~> "dir" ~> "cx" ~> "term" ~> cases _Term (var "term")
    Nothing [
    _Term_record>>: "rec" ~>
      "dfields" <~ Core.recordFields (var "rec") $
      "newFields" <<~ Eithers.mapList
        ("p" ~> AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (Pairs.first (var "p"))) @@ var "cx" @@ (Pairs.second (var "p")))
        (Lists.zip (var "adapters") (var "dfields")) $
      right (Core.termRecord (Core.record (Core.rowTypeTypeName (var "rt")) (var "newFields")))]) $
  "forRecordType" <~ ("rt" ~>
    "adapters" <<~ Eithers.mapList (fieldAdapter @@ var "cx") (Core.rowTypeFields (var "rt")) $
    "lossy" <~ Logic.ors (Lists.map (unaryFunction Compute.adapterIsLossy) (var "adapters")) $
    "sfields'" <~ Lists.map (unaryFunction Compute.adapterTarget) (var "adapters") $
    right (Compute.adapter
      (var "lossy")
      (var "t")
      (Core.typeRecord (Core.rowType (Core.rowTypeTypeName (var "rt")) (var "sfields'")))
      (AdaptUtils.bidirectional @@ (var "encdec" @@ var "rt" @@ var "adapters")))) $
  cases _Type (var "t")
    Nothing [
    _Type_record>>: "rt" ~> var "forRecordType" @@ var "rt"]

passSet :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
passSet = define "passSet" $
  doc "Pass through set types" $
  "cx" ~> "t" ~>
  "encdec" <~ ("ad" ~> "dir" ~> "cx" ~> "term" ~> cases _Term (var "term")
    Nothing [
    _Term_set>>: "terms" ~>
      "newTerms" <<~ Eithers.mapList (AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "cx") (Sets.toList (var "terms")) $
      right (Core.termSet (Sets.fromList (var "newTerms")))]) $
  cases _Type (var "t")
    Nothing [
    _Type_set>>: "st" ~>
      "ad" <<~ termAdapter @@ var "cx" @@ var "st" $
      right (Compute.adapter
        (Compute.adapterIsLossy (var "ad"))
        (var "t")
        (MetaTypes.set (Compute.adapterTarget (var "ad")))
        (AdaptUtils.bidirectional @@ (var "encdec" @@ var "ad")))]

passUnion :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
passUnion = define "passUnion" $
  doc "Pass through union types" $
  "cx" ~> "t" ~> cases _Type (var "t")
    Nothing [
    _Type_union>>: "rt" ~>
      "sfields" <~ Core.rowTypeFields (var "rt") $
      "tname" <~ Core.rowTypeTypeName (var "rt") $
      "adapters" <<~ Eithers.mapList
        ("f" ~>
          "ad" <<~ fieldAdapter @@ var "cx" @@ var "f" $
          right (pair (Core.fieldTypeName (var "f")) (var "ad")))
        (var "sfields") $
      "adaptersMap" <~ Maps.fromList (var "adapters") $
      "lossy" <~ Logic.ors (Lists.map ("pair" ~> Compute.adapterIsLossy (Pairs.second (var "pair"))) (var "adapters")) $
      "sfields'" <~ Lists.map ("pair" ~> Compute.adapterTarget (Pairs.second (var "pair"))) (var "adapters") $
      right (Compute.adapter
        (var "lossy")
        (var "t")
        (Core.typeUnion (Core.rowType (var "tname") (var "sfields'")))
        (AdaptUtils.bidirectional @@ ("dir" ~> "_cx" ~> "term" ~>
          -- Note: this is a shortcut, since we anticipate deprecating the current term adapter logic
          right (var "term"))))]

passUnit :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
passUnit = define "passUnit" $
  doc "Pass through unit types" $
  "_cx" ~> constant (right (Compute.adapter false Core.typeUnit Core.typeUnit
    (Compute.coder
      ("_cx" ~> constant (right Core.termUnit))
      ("_cx" ~> constant (right Core.termUnit)))))

passWrapped :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
passWrapped = define "passWrapped" $
  doc "Pass through wrapped types" $
  "cx" ~> "t" ~> cases _Type (var "t")
    Nothing [
    _Type_wrap>>: "wt" ~>
      "tname" <~ Core.wrappedTypeTypeName (var "wt") $
      "ot" <~ Core.wrappedTypeBody (var "wt") $
      "mapTerm" <~ ("graph" ~> "coder" ~> "dir" ~> "cx" ~> "term" ~>
        "unwrapped" <<~ ExtractCore.wrap @@ var "cx" @@ var "tname" @@ var "graph" @@ var "term" $
        "newTerm" <<~ AdaptUtils.encodeDecode @@ var "dir" @@ var "coder" @@ var "cx" @@ var "unwrapped" $
        right (Core.termWrap (Core.wrappedTerm (var "tname") (var "newTerm")))) $
      "adapter" <<~ termAdapter @@ var "cx" @@ var "ot" $
      right (Compute.adapter
        (Compute.adapterIsLossy (var "adapter"))
        (var "t")
        (Core.typeWrap (Core.wrappedType (var "tname") (Compute.adapterTarget (var "adapter"))))
        (AdaptUtils.bidirectional @@ (var "mapTerm" @@ Coders.adapterContextGraph (var "cx") @@ (Compute.adapterCoder (var "adapter")))))]

setToList :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
setToList = define "setToList" $
  doc "Convert set types to list types" $
  "cx" ~> "t" ~>
  "encode" <~ ("ad" ~> "cx" ~> "term" ~> cases _Term (var "term")
    Nothing [
    _Term_set>>: "s" ~> AdaptUtils.encodeDecode @@ Coders.coderDirectionEncode @@ (Compute.adapterCoder (var "ad")) @@ var "cx" @@ (Core.termList (Sets.toList (var "s")))]) $
  "forListTerm" <~ ("t" ~> cases _Term (var "t")
    Nothing [
    _Term_list>>: "l" ~> right (Core.termSet (Sets.fromList (var "l")))]) $
  "decode" <~ ("ad" ~> "cx" ~> "term" ~>
    "listTerm" <<~ AdaptUtils.encodeDecode @@ Coders.coderDirectionDecode @@ (Compute.adapterCoder (var "ad")) @@ var "cx" @@ var "term" $
    var "forListTerm" @@ var "listTerm") $
  "forSetType" <~ ("st" ~>
    "ad" <<~ termAdapter @@ var "cx" @@ (MetaTypes.list (var "st")) $
    right (Compute.adapter
      (Compute.adapterIsLossy (var "ad"))
      (var "t")
      (Compute.adapterTarget (var "ad"))
      (Compute.coder (var "encode" @@ var "ad") (var "decode" @@ var "ad")))) $
  cases _Type (var "t")
    Nothing [
    _Type_set>>: "st" ~> var "forSetType" @@ var "st"]

simplifyApplication :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
simplifyApplication = define "simplifyApplication" $
  doc "Simplify application types" $
  "cx" ~> "t" ~>
  "encdec" <~ ("ad" ~> "dir" ~> "cx" ~> "term" ~>
    AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "cx" @@ var "term") $
  "forApplicationType" <~ ("at" ~>
    "lhs" <~ Core.applicationTypeFunction (var "at") $
    "ad" <<~ termAdapter @@ var "cx" @@ var "lhs" $
    right (Compute.adapter
      false
      (var "t")
      (Compute.adapterTarget (var "ad"))
      (AdaptUtils.bidirectional @@ (var "encdec" @@ var "ad")))) $
  cases _Type (var "t")
    Nothing [
    _Type_application>>: "at" ~> var "forApplicationType" @@ var "at"]

unitToRecord :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
unitToRecord = define "unitToRecord" $
    doc "Convert unit terms to records" $
    "_cx" ~> constant (right
      (Compute.adapter false Core.typeUnit (Core.typeRecord (Core.rowType unitName (list ([] :: [TTerm FieldType]))))
        (Compute.coder
          ("_cx" ~> constant (right (Core.termRecord (Core.record unitName (list ([] :: [TTerm Field]))))))
          ("_cx" ~> constant (right Core.termUnit)))))
  where
    unitName = Core.name (string "_Unit")

unionToRecord :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
unionToRecord = define "unionToRecord" $
  doc "Convert union types to record types" $
  "cx" ~> "t" ~>
  "forField" <~ ("field" ~>
    "fn" <~ Core.fieldName (var "field") $
    "fterm" <~ Core.fieldTerm (var "field") $
    cases _Term (var "fterm")
      Nothing [
      _Term_maybe>>: "opt" ~> Maybes.bind (var "opt") ("t" ~>
        just (Core.field (var "fn") (var "t")))]) $
  "fromRecordFields" <~ ("cx" ~> "term" ~> "term'" ~> "t'" ~> "fields" ~>
    "matches" <~ Maybes.mapMaybe (var "forField") (var "fields") $
    Logic.ifElse (Lists.null (var "matches"))
      (Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat (list [
        string "cannot convert term back to union: ",
        ShowCore.term @@ var "term",
        string " where type = ",
        ShowCore.type_ @@ var "t",
        string "    and target type = ",
        ShowCore.type_ @@ var "t'"]))) (var "cx"))
      (right (Lists.head (var "matches")))) $
  "forRecTerm" <~ ("cx" ~> "nm" ~> "ad" ~> "term" ~> "recTerm" ~> cases _Term (var "recTerm")
    Nothing [
    _Term_record>>: "rec" ~>
      "fields" <~ Core.recordFields (var "rec") $
      "resultField" <<~
        (var "fromRecordFields"
          @@ var "cx"
          @@ var "term"
          @@ (Core.termRecord (Core.record (var "nm") (var "fields")))
          @@ (Compute.adapterTarget (var "ad"))
          @@ var "fields") $
      right (Core.termUnion (Core.injection (var "nm") (var "resultField")))]) $
  cases _Type (var "t")
    Nothing [
    _Type_union>>: "rt" ~>
      "nm" <~ Core.rowTypeTypeName (var "rt") $
      "sfields" <~ Core.rowTypeFields (var "rt") $
      "target" <~ Core.typeRecord (CoderUtils.unionTypeToRecordType @@ var "rt") $
      "toRecordField" <~ ("term" ~> "fn" ~> "f" ~>
        "fn'" <~ Core.fieldTypeName (var "f") $
        Core.field (var "fn'") (Core.termMaybe (Logic.ifElse
          (Equality.equal (var "fn'") (var "fn"))
          (just (var "term"))
          nothing))) $
      "ad" <<~ termAdapter @@ var "cx" @@ var "target" $
      "graph" <~ Coders.adapterContextGraph (var "cx") $
      right (Compute.adapter
        (Compute.adapterIsLossy (var "ad"))
        (var "t")
        (Compute.adapterTarget (var "ad"))
        (Compute.coder
          ("cx" ~> "term'" ~>
            "field" <<~ ExtractCore.injection @@ var "cx" @@ (Core.rowTypeTypeName (var "rt")) @@ var "graph" @@ var "term'" $
            "fn" <~ Core.fieldName (var "field") $
            "term" <~ Core.fieldTerm (var "field") $
            Compute.coderEncode (Compute.adapterCoder (var "ad")) @@ var "cx" @@
              (Core.termRecord (Core.record (var "nm") (Lists.map (var "toRecordField" @@ var "term" @@ var "fn") (var "sfields")))))
          ("cx" ~> "term" ~>
            "recTerm" <<~ Compute.coderDecode (Compute.adapterCoder (var "ad")) @@ var "cx" @@ var "term" $
            var "forRecTerm" @@ var "cx" @@ var "nm" @@ var "ad" @@ var "term" @@ var "recTerm")))]

wrapToUnwrapped :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
wrapToUnwrapped = define "wrapToUnwrapped" $
  doc "Convert wrapped types to unwrapped types" $
  "cx" ~> "t" ~> cases _Type (var "t")
    Nothing [
    _Type_wrap>>: "wt" ~>
      "tname" <~ Core.wrappedTypeTypeName (var "wt") $
      "typ" <~ Core.wrappedTypeBody (var "wt") $
      "encode" <~ ("graph" ~> "ad" ~> "cx" ~> "term" ~>
        "unwrapped" <<~ ExtractCore.wrap @@ var "cx" @@ var "tname" @@ var "graph" @@ var "term" $
        Compute.coderEncode (Compute.adapterCoder (var "ad")) @@ var "cx" @@ var "unwrapped") $
      "decode" <~ ("ad" ~> "cx" ~> "term" ~>
        "decoded" <<~ Compute.coderDecode (Compute.adapterCoder (var "ad")) @@ var "cx" @@ var "term" $
        right (Core.termWrap (Core.wrappedTerm (var "tname") (var "decoded")))) $
      "ad" <<~ termAdapter @@ var "cx" @@ var "typ" $
      right (Compute.adapter
        false
        (var "t")
        (Compute.adapterTarget (var "ad"))
        (Compute.coder (var "encode" @@ Coders.adapterContextGraph (var "cx") @@ var "ad") (var "decode" @@ var "ad")))]

-- Note: those constructors which cannot be mapped meaningfully at this time are simply
--       preserved as strings.
termAdapter :: TBinding (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))
termAdapter = define "termAdapter" $
  doc "Create an adapter for any type" $
  "cx" ~> "typ" ~>
  "constraints" <~ ("cx" ~> Coders.languageConstraintsProjection (Coders.adapterContextLanguage (var "cx"))) $
  "supported" <~ ("cx" ~> AdaptUtils.typeIsSupported @@ (var "constraints" @@ var "cx")) $
  "variantIsSupported" <~ ("cx" ~> "t" ~>
    Sets.member (Reflect.typeVariant @@ var "t") (Coders.languageConstraintsTypeVariants (var "constraints" @@ var "cx"))) $
  "supportedAtTopLevel" <~ ("cx" ~> "t" ~> Logic.and
    (var "variantIsSupported" @@ var "cx" @@ var "t")
    (Coders.languageConstraintsTypes (var "constraints" @@ var "cx") @@ var "t")) $
  "pass" <~ ("t" ~> cases _TypeVariant (Reflect.typeVariant @@ (Rewriting.deannotateType @@ var "t"))
    Nothing [
    _TypeVariant_annotated>>: constant (list ([] :: [TTerm (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))])),
    _TypeVariant_application>>: constant (list [passApplication]),
    _TypeVariant_either>>: constant (list [passEither]),
    _TypeVariant_forall>>: constant (list [passForall]),
    _TypeVariant_function>>: constant (list [passFunction]),
    _TypeVariant_list>>: constant (list [passList]),
    _TypeVariant_literal>>: constant (list [passLiteral]),
    _TypeVariant_map>>: constant (list [passMap]),
    _TypeVariant_maybe>>: constant (list [passOptional, maybeToList]),
    _TypeVariant_pair>>: constant (list ([] :: [TTerm (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))])),
    _TypeVariant_record>>: constant (list [passRecord]),
    _TypeVariant_set>>: constant (list [passSet]),
    _TypeVariant_union>>: constant (list [passUnion]),
    _TypeVariant_unit>>: constant (list [passUnit]),
    _TypeVariant_variable>>: constant (list ([] :: [TTerm (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))])),
    _TypeVariant_wrap>>: constant (list [passWrapped])]) $
  "trySubstitution" <~ ("t" ~> cases _TypeVariant (Reflect.typeVariant @@ var "t")
    Nothing [
    _TypeVariant_annotated>>: constant (list ([] :: [TTerm (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))])),
    _TypeVariant_application>>: constant (list [simplifyApplication]),
    _TypeVariant_either>>: constant (list ([] :: [TTerm (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))])),
    _TypeVariant_forall>>: constant (list [lambdaToMonotype]),
    _TypeVariant_function>>: constant (list [functionToUnion]),
    _TypeVariant_list>>: constant (list ([] :: [TTerm (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))])),
    _TypeVariant_literal>>: constant (list ([] :: [TTerm (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))])),
    _TypeVariant_map>>: constant (list ([] :: [TTerm (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))])),
    _TypeVariant_maybe>>: constant (list [maybeToList]),
    _TypeVariant_pair>>: constant (list ([] :: [TTerm (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))])),
    _TypeVariant_record>>: constant (list ([] :: [TTerm (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))])),
    _TypeVariant_set>>: constant (list [setToList]),
    _TypeVariant_union>>: constant (list [unionToRecord]),
    _TypeVariant_unit>>: constant (list [unitToRecord]),
    _TypeVariant_variable>>: constant (list ([] :: [TTerm (AdapterContext -> Type -> Either String (SymmetricAdapter Type Term))])),
    _TypeVariant_wrap>>: constant (list [wrapToUnwrapped])]) $
  "alts" <~ ("cx" ~> "t" ~> Eithers.mapList ("c" ~> var "c" @@ var "cx" @@ var "t")
     (Logic.ifElse (var "supportedAtTopLevel" @@ var "cx" @@ var "t")
       (var "pass" @@ var "t")
       (var "trySubstitution" @@ var "t"))) $
  "dflt" <~ (cases _Type (var "typ")
    (Just (
      AdaptUtils.chooseAdapter
        @@ (var "alts" @@ var "cx")
        @@ (var "supported" @@ var "cx")
        @@ ShowCore.type_
        @@ (ShowCore.type_)
        @@ (var "typ"))) [
    -- Account for let-bound variables
    _Type_variable>>: "name" ~> forTypeReference @@ var "cx" @@ var "name"]) $
  cases _Type (var "typ")
    (Just (var "dflt")) [
    _Type_annotated>>: "at" ~>
      "ad" <<~ termAdapter @@ var "cx" @@ Core.annotatedTypeBody (var "at") $
      right (Compute.adapterWithTarget (var "ad")
        (Core.typeAnnotated (Core.annotatedType (Compute.adapterTarget (var "ad")) (Core.annotatedTypeAnnotation (var "at")))))]

