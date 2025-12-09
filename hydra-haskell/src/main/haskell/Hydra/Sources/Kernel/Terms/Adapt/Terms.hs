
module Hydra.Sources.Kernel.Terms.Adapt.Terms where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  fieldAdapter, forTypeReference, functionProxyName, functionProxyType, functionToUnion,
  lambdaToMonotype, maybeToList, passApplication, passEither, passForall, passFunction, passList,
  passLiteral, passMap, passOptional, passRecord, passSet, passUnion, passUnit, passWrapped,
  setToList, simplifyApplication, unitToRecord, unionToRecord, unionTypeToRecordType, wrapToUnwrapped,
  termAdapter, withGraphContext)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils    as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Monads         as Monads
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore


module_ :: Module
module_ = Module (Namespace "hydra.adapt.terms") elements
    [AdaptLiterals.module_, AdaptUtils.module_, Annotations.module_, ExtractCore.module_, Monads.module_,
      Reflect.module_, Rewriting.module_, Schemas.module_, ShowCore.module_]
    kernelTypesModules $
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
      toBinding unionTypeToRecordType,
      toBinding unitToRecord,
      toBinding wrapToUnwrapped,
      toBinding withGraphContext]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

fieldAdapter :: TBinding (FieldType -> Flow AdapterContext (SymmetricAdapter AdapterContext FieldType Field))
fieldAdapter = define "fieldAdapter" $
  doc "Create an adapter for field types" $
  "ftyp" ~>
  "encdec" <~ ("ad" ~> "dir" ~> "field" ~>
    "name" <~ Core.fieldName (var "field") $
    "term" <~ Core.fieldTerm (var "field") $
    "newTerm" <<~ AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "term" $
    produce $ Core.field (var "name") (var "newTerm")) $
  "ad" <<~ termAdapter @@ (Core.fieldTypeType (var "ftyp")) $
  produce (Compute.adapter
    (Compute.adapterIsLossy (var "ad"))
    (var "ftyp")
    (Core.fieldType (Core.fieldTypeName (var "ftyp")) (Compute.adapterTarget (var "ad")))
    (AdaptUtils.bidirectional @@ (var "encdec" @@ var "ad")))

forTypeReference :: TBinding (Name -> Flow AdapterContext (SymmetricAdapter AdapterContext Type Term))
forTypeReference = define "forTypeReference" $
  doc "This function accounts for recursive type definitions" $
  "name" ~>
  "encdec" <~ ("name" ~> "dir" ~> "term" ~>
    "cx" <<~ Monads.getState $
    "adapters" <~ Coders.adapterContextAdapters (var "cx") $
    Maybes.maybe
      (Flows.fail (Strings.cat2 (string "no adapter for reference type ") (unwrap _Name @@ var "name")))
      ("ad" ~> AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "term")
      (Maps.lookup (var "name") (var "adapters"))) $
  "forType" <~ ("cx" ~> "adapters" ~> "t" ~>
    "actual" <<~ termAdapter @@ var "t" $
    "finalAdapters" <~ Maps.insert (var "name") (var "actual") (var "adapters") $
    "finalCx" <~ Coders.adapterContext
      (Coders.adapterContextGraph (var "cx"))
      (Coders.adapterContextLanguage (var "cx"))
      (var "finalAdapters") $
    "ignored2" <<~ Monads.putState @@ var "finalCx" $
    produce (var "actual")) $
  "forMissingAdapter" <~ ("cx" ~> "lossy" ~> "adapters" ~> "placeholder" ~>
    "newAdapters" <~ Maps.insert (var "name") (var "placeholder") (var "adapters") $
    "newCx" <~ Coders.adapterContext
      (Coders.adapterContextGraph (var "cx"))
      (Coders.adapterContextLanguage (var "cx"))
      (var "newAdapters") $
    "ignored" <<~ Monads.putState @@ var "newCx" $
    "mt" <<~ withGraphContext @@ (Schemas.resolveType @@ (Core.typeVariable (var "name"))) $
    Maybes.maybe
      (produce (Compute.adapter (var "lossy") (Core.typeVariable (var "name")) (Core.typeVariable (var "name"))
        (AdaptUtils.bidirectional @@ ("dir" ~> "term" ~> produce (var "term")))))
      (var "forType" @@ var "cx" @@ var "adapters")
      (var "mt")) $
  "flow" <~ (
    "lossy" <~ false $
    "placeholder" <~ Compute.adapter (var "lossy") (Core.typeVariable (var "name")) (Core.typeVariable (var "name"))
      (AdaptUtils.bidirectional @@ (var "encdec" @@ var "name")) $
    "cx" <<~ Monads.getState $
    "adapters" <~ Coders.adapterContextAdapters (var "cx") $
    Maybes.maybe
      (var "forMissingAdapter" @@ var "cx" @@ var "lossy" @@ var "adapters" @@ var "placeholder")
      (unaryFunction Flows.pure)
      (Maps.lookup (var "name") (var "adapters"))) $
  trace (Strings.cat2 (string "adapt named type ") (unwrap _Name @@ var "name")) $
  var "flow"

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

functionToUnion :: TBinding TypeAdapter
functionToUnion = define "functionToUnion" $
  doc "Convert function types to union types" $
  "t" ~>
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
  "encode" <~ ("ad" ~> "term" ~>
    "strippedTerm" <~ Rewriting.deannotateTerm @@ var "term" $
    Compute.coderEncode (Compute.adapterCoder (var "ad")) @@ (var "encTerm" @@ var "term" @@ var "strippedTerm")) $
  "readFromString" <~ ("term" ~>
    "s" <<~ ExtractCore.string @@ var "term" $
    Maybes.maybe
      (Flows.fail (Strings.cat2 (string "failed to parse term: ") (var "s")))
      (unaryFunction Flows.pure)
      (ShowCore.readTerm @@ var "s")) $
  "decode" <~ ("ad" ~> "term" ~>
    "notFound" <~ ("fname" ~> Flows.fail (Strings.cat2 (string "unexpected field: ") (unwrap _Name @@ var "fname"))) $
    "forCases" <~ ("fterm" ~> withGraphContext @@ (var "readFromString" @@ var "fterm")) $
    "forLambda" <~ ("fterm" ~> withGraphContext @@ (var "readFromString" @@ var "fterm")) $
    "forWrapped" <~ ("fterm" ~> withGraphContext @@ (Flows.map ("s" ~> MetaTerms.unwrap (Core.name (var "s"))) (ExtractCore.string @@ var "fterm"))) $
    "forPrimitive" <~ ("fterm" ~> withGraphContext @@ (Flows.map ("s" ~> MetaTerms.primitiveLift (Core.name (var "s"))) (ExtractCore.string @@ var "fterm"))) $
    "forProjection" <~ ("fterm" ~> withGraphContext @@ (var "readFromString" @@ var "fterm")) $
    "forVariable" <~ ("fterm" ~> withGraphContext @@ (Flows.map ("s" ~> Core.termVariable (Core.name (var "s"))) (ExtractCore.string @@ var "fterm"))) $
    "injTerm" <<~ Compute.coderDecode (Compute.adapterCoder (var "ad")) @@ var "term" $
    "field" <<~ withGraphContext @@ (ExtractCore.injection @@ (functionProxyName) @@ var "injTerm") $
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
        "domAd" <<~ termAdapter @@ var "dom" $
        produce (Core.typeUnion (Core.rowType (functionProxyName) (list [
          Core.fieldType (Core.nameLift _Elimination_wrap) MetaTypes.string,
          Core.fieldType (Core.nameLift _Elimination_record) MetaTypes.string,
          Core.fieldType (Core.nameLift _Elimination_union) MetaTypes.string,
          Core.fieldType (Core.nameLift _Function_lambda) MetaTypes.string,
          Core.fieldType (Core.nameLift _Function_primitive) MetaTypes.string,
          Core.fieldType (Core.nameLift _Term_variable) MetaTypes.string])))) $
      "ut" <<~ var "unionType" $
      "ad" <<~ termAdapter @@ var "ut" $
      produce (Compute.adapter
        (Compute.adapterIsLossy (var "ad"))
        (var "t")
        (Compute.adapterTarget (var "ad"))
        (Compute.coder (var "encode" @@ var "ad") (var "decode" @@ var "ad")))]

lambdaToMonotype :: TBinding TypeAdapter
lambdaToMonotype = define "lambdaToMonotype" $
  doc "Convert forall types to monotypes" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_forall>>: "ft" ~>
      "body" <~ Core.forallTypeBody (var "ft") $
      "ad" <<~ termAdapter @@ var "body" $
      produce (Compute.adapter
        (Compute.adapterIsLossy (var "ad"))
        (var "t")
        (Compute.adapterTarget (var "ad"))
        (Compute.adapterCoder (var "ad")))]

maybeToList :: TBinding TypeAdapter
maybeToList = define "maybeToList" $
  doc "Convert optional types to list types" $
  "t" ~>
  "encode" <~ ("ad" ~> "term" ~> cases _Term (var "term")
    Nothing [
    _Term_maybe>>: "m" ~> Maybes.maybe
      (produce (MetaTerms.list []))
      ("r" ~>
        "encoded" <<~ Compute.coderEncode (Compute.adapterCoder (var "ad")) @@ var "r" $
        produce (Core.termList (list [var "encoded"])))
      (var "m")]) $
  "decode" <~ ("ad" ~> "term" ~> cases _Term (var "term")
    Nothing [
    _Term_list>>: "l" ~> Flows.map (unaryFunction Core.termMaybe) (Logic.ifElse (Lists.null (var "l"))
      (produce nothing)
      ("decoded" <<~ Compute.coderDecode (Compute.adapterCoder (var "ad")) @@ (Lists.head (var "l")) $
       produce (just (var "decoded"))))]) $
  cases _Type (var "t")
    Nothing [
    _Type_maybe>>: "ot" ~>
      "ad" <<~ termAdapter @@ var "ot" $
      produce (Compute.adapter
        false
        (var "t")
        (Core.typeList (Compute.adapterTarget (var "ad")))
        (Compute.coder (var "encode" @@ var "ad") (var "decode" @@ var "ad")))]

passApplication :: TBinding TypeAdapter
passApplication = define "passApplication" $
  doc "Pass through application types" $
  "t" ~>
  "forApplicationType" <~ ("at" ~>
    "lhs" <~ Core.applicationTypeFunction (var "at") $
    "rhs" <~ Core.applicationTypeArgument (var "at") $
    "lhsAd" <<~ termAdapter @@ var "lhs" $
    "rhsAd" <<~ termAdapter @@ var "rhs" $
    produce (Compute.adapter
      (Logic.or (Compute.adapterIsLossy (var "lhsAd")) (Compute.adapterIsLossy (var "rhsAd")))
      (var "t")
      (Core.typeApplication (Core.applicationType (Compute.adapterTarget (var "lhsAd")) (Compute.adapterTarget (var "rhsAd"))))
      (AdaptUtils.bidirectional @@
        ("dir" ~> "term" ~> AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "lhsAd")) @@ var "term")))) $
  cases _Type (var "t")
    Nothing [
    _Type_application>>: "at" ~> var "forApplicationType" @@ var "at"]

passEither :: TBinding TypeAdapter
passEither = define "passEither" $
  doc "Pass through either types" $
  "t" ~>
  "forEitherType" <~ ("et" ~>
    "left" <~ Core.eitherTypeLeft (var "et") $
    "right" <~ Core.eitherTypeRight (var "et") $
    "leftAd" <<~ termAdapter @@ var "left" $
    "rightAd" <<~ termAdapter @@ var "right" $
    produce (Compute.adapter
      (Logic.or (Compute.adapterIsLossy (var "leftAd")) (Compute.adapterIsLossy (var "rightAd")))
      (var "t")
      (Core.typeEither (Core.eitherType (Compute.adapterTarget (var "leftAd")) (Compute.adapterTarget (var "rightAd"))))
      (AdaptUtils.bidirectional @@
        ("dir" ~> "term" ~> AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "leftAd")) @@ var "term")))) $
  cases _Type (var "t")
    Nothing [
    _Type_either>>: "et" ~> var "forEitherType" @@ var "et"]

passForall :: TBinding TypeAdapter
passForall = define "passForall" $
  doc "Pass through forall types" $
  "t" ~>
  "forForallType" <~ ("ft" ~>
    "v" <~ Core.forallTypeParameter (var "ft") $
    "body" <~ Core.forallTypeBody (var "ft") $
    "ad" <<~ termAdapter @@ var "body" $
    produce (Compute.adapter
      (Compute.adapterIsLossy (var "ad"))
      (var "t")
      (Core.typeForall (Core.forallType (var "v") (Compute.adapterTarget (var "ad"))))
      (AdaptUtils.bidirectional @@ ("dir" ~> "term" ~>
        AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "term")))) $
  cases _Type (var "t")
    Nothing [
    _Type_forall>>: "ft" ~> var "forForallType" @@ var "ft"]

passFunction :: TBinding TypeAdapter
passFunction = define "passFunction" $
  doc "Pass through function types with adaptation" $
  "t" ~>
  "toCaseAds" <~ ("dom" ~> "cod" ~> cases _Type (Rewriting.deannotateType @@ var "dom")
    (Just (produce Maps.empty)) [
    _Type_union >>: "rt" ~>
      "pairs" <<~ Flows.mapList
        ("f" ~>
          "ad" <<~ fieldAdapter @@ Core.fieldType
            (Core.fieldTypeName (var "f"))
            (Core.typeFunction (Core.functionType
              (Core.fieldTypeType (var "f"))
              (var "cod"))) $
          produce (pair (Core.fieldTypeName (var "f")) (var "ad")))
        (Core.rowTypeFields (var "rt")) $
      produce (Maps.fromList (var "pairs"))]) $
  "toOptionAd" <~ ("dom" ~> "cod" ~> cases _Type (Rewriting.deannotateType @@ var "dom")
    (Just (produce nothing)) [
    _Type_maybe >>: "ot" ~>
      Flows.map (unaryFunction just) (termAdapter @@ MetaTypes.function (var "ot") (var "cod"))]) $
  "getCoder" <~ ("caseAds" ~> "fname" ~> Maybes.maybe
    AdaptUtils.idCoder
    (unaryFunction Compute.adapterCoder)
    (Maps.lookup (var "fname") (var "caseAds"))) $
  "forElimination" <~ ("dir" ~> "codAd" ~> "caseAds" ~> "e" ~> cases _Elimination (var "e")
    Nothing [
    _Elimination_union >>: "cs" ~>
      "n" <~ Core.caseStatementTypeName (var "cs") $
      "def" <~ Core.caseStatementDefault (var "cs") $
      "cases" <~ Core.caseStatementCases (var "cs") $
      "rcases" <<~ Flows.mapList
        ("f" ~> AdaptUtils.encodeDecode @@ var "dir" @@ (var "getCoder" @@ var "caseAds" @@ Core.fieldName (var "f")) @@ var "f")
        (var "cases") $
      "rdef" <<~ Maybes.maybe
        (produce nothing)
        ("d" ~> Flows.map (unaryFunction just) (AdaptUtils.encodeDecode @@ var "dir" @@ Compute.adapterCoder (var "codAd") @@ var "d"))
        (var "def") $
      produce (Core.eliminationUnion (Core.caseStatement (var "n") (var "rdef") (var "rcases")))]) $
  "forFunction" <~ ("dir" ~> "codAd" ~> "caseAds" ~> "f" ~> cases _Function (var "f")
    Nothing [
    _Function_elimination >>: "e" ~>
      Flows.map (unaryFunction Core.functionElimination)
        (var "forElimination" @@ var "dir" @@ var "codAd" @@ var "caseAds" @@ var "e"),
    _Function_lambda >>: "l" ~>
      "var" <~ Core.lambdaParameter (var "l") $
      "d" <~ Core.lambdaDomain (var "l") $
      "body" <~ Core.lambdaBody (var "l") $
      "newBody" <<~ AdaptUtils.encodeDecode @@ var "dir" @@ Compute.adapterCoder (var "codAd") @@ var "body" $
      produce (Core.functionLambda (Core.lambda (var "var") (var "d") (var "newBody"))),
    _Function_primitive >>: "name" ~> produce (Core.functionPrimitive (var "name"))]) $
  "encdec" <~ ("codAd" ~> "caseAds" ~> "dir" ~> "term" ~> cases _Term (Rewriting.deannotateTerm @@ var "term")
    (Just (produce (var "term"))) [
    _Term_function >>: "f" ~> Flows.map (unaryFunction Core.termFunction)
      (var "forFunction" @@ var "dir" @@ var "codAd" @@ var "caseAds" @@ var "f")]) $
  "forFunctionType" <~ ("ft" ~>
    "dom" <~ Core.functionTypeDomain (var "ft") $
    "cod" <~ Core.functionTypeCodomain (var "ft") $
    "domAd" <<~ termAdapter @@ var "dom" $
    "codAd" <<~ termAdapter @@ var "cod" $
    "caseAds" <<~ var "toCaseAds" @@ var "dom" @@ var "cod" $
    "optionAd" <<~ var "toOptionAd" @@ var "dom" @@ var "cod" $
    "lossy" <~ Logic.or
      (Compute.adapterIsLossy (var "codAd"))
      (Logic.ors (Lists.map ("pair" ~> Compute.adapterIsLossy (Pairs.second (var "pair"))) (Maps.toList (var "caseAds")))) $
    "target" <~ MetaTypes.function (Compute.adapterTarget (var "domAd")) (Compute.adapterTarget (var "codAd")) $
    produce $ Compute.adapter (var "lossy") (var "t") (var "target")
      (AdaptUtils.bidirectional @@ (var "encdec" @@ var "codAd" @@ var "caseAds"))) $
  cases _Type (var "t")
    Nothing [
    _Type_function >>: "ft" ~> var "forFunctionType" @@ var "ft"]

passList :: TBinding TypeAdapter
passList = define "passList" $
  doc "Pass through list types" $
  "t" ~>
  "encdec" <~ ("ad" ~> "dir" ~> "term" ~> cases _Term (var "term")
    Nothing [
    _Term_list>>: "terms" ~>
      "newTerms" <<~ Flows.mapList (AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "ad"))) (var "terms") $
      produce (Core.termList (var "newTerms"))]) $
  "forListType" <~ ("lt" ~>
    "ad" <<~ termAdapter @@ var "lt" $
    produce (Compute.adapter
      (Compute.adapterIsLossy (var "ad"))
      (var "t")
      (MetaTypes.list (Compute.adapterTarget (var "ad")))
      (AdaptUtils.bidirectional @@ (var "encdec" @@ var "ad")))) $
  cases _Type (var "t")
    Nothing [
    _Type_list>>: "lt" ~> var "forListType" @@ var "lt"]

passLiteral :: TBinding TypeAdapter
passLiteral = define "passLiteral" $
  doc "Pass through literal types with literal adaptation" $
  "t" ~>
  "encdec" <~ ("ad" ~> "dir" ~> "term" ~>
    "l" <<~ withGraphContext @@ (ExtractCore.literal @@ var "term") $
    "l2" <<~ AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "l" $
    produce $ Core.termLiteral $ var "l2") $
  "forLiteral" <~ ("lt" ~>
    "ad" <<~ AdaptLiterals.literalAdapter @@ var "lt" $
    "step" <~ AdaptUtils.bidirectional @@ (var "encdec" @@ var "ad") $
    produce (Compute.adapter
      (Compute.adapterIsLossy (var "ad"))
      (Core.typeLiteral (Compute.adapterSource (var "ad")))
      (Core.typeLiteral (Compute.adapterTarget (var "ad")))
      (var "step"))) $
  cases _Type (var "t")
    Nothing [
    _Type_literal>>: "lt" ~> var "forLiteral" @@ var "lt"]

passMap :: TBinding TypeAdapter
passMap = define "passMap" $
  doc "Pass through map types" $
  "t" ~>
  "encdec" <~ ("kad" ~> "vad" ~> "dir" ~> "term" ~> cases _Term (var "term")
    Nothing [
    _Term_map>>: "m" ~>
      "newPairs" <<~ Flows.mapList
        ("pair" ~>
          "k" <~ Pairs.first (var "pair") $
          "v" <~ Pairs.second (var "pair") $
          "newK" <<~ AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "kad")) @@ var "k" $
          "newV" <<~ AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "vad")) @@ var "v" $
          produce (pair (var "newK") (var "newV")))
        (Maps.toList (var "m")) $
      produce (Core.termMap (Maps.fromList (var "newPairs")))]) $
  "forMapType" <~ ("mt" ~>
    "kt" <~ Core.mapTypeKeys (var "mt") $
    "vt" <~ Core.mapTypeValues (var "mt") $
    "kad" <<~ termAdapter @@ var "kt" $
    "vad" <<~ termAdapter @@ var "vt" $
    produce (Compute.adapter
      (Logic.or (Compute.adapterIsLossy (var "kad")) (Compute.adapterIsLossy (var "vad")))
      (var "t")
      (MetaTypes.map (Compute.adapterTarget (var "kad")) (Compute.adapterTarget (var "vad")))
      (AdaptUtils.bidirectional @@ (var "encdec" @@ var "kad" @@ var "vad")))) $
  cases _Type (var "t")
    Nothing [
    _Type_map>>: "mt" ~> var "forMapType" @@ var "mt"]

passOptional :: TBinding TypeAdapter
passOptional = define "passOptional" $
  doc "Pass through optional types" $
  "t" ~>
  "mapTerm" <~ ("coder" ~> "dir" ~> "term" ~>
    "opt" <<~ withGraphContext @@ (ExtractCore.maybeTerm @@ unaryFunction Flows.pure @@ var "term") $
    "newOpt" <<~ Flows.mapMaybe (AdaptUtils.encodeDecode @@ var "dir" @@ var "coder") (var "opt") $
    produce (Core.termMaybe (var "newOpt"))) $
  cases _Type (var "t")
    Nothing [
    _Type_maybe>>: "ot" ~>
      "adapter" <<~ termAdapter @@ var "ot" $
      produce (Compute.adapter
        (Compute.adapterIsLossy (var "adapter"))
        (var "t")
        (Core.typeMaybe (Compute.adapterTarget (var "adapter")))
        (AdaptUtils.bidirectional @@ (var "mapTerm" @@ (Compute.adapterCoder (var "adapter")))))]

passRecord :: TBinding TypeAdapter
passRecord = define "passRecord" $
  doc "Pass through record types" $
  "t" ~>
  "encdec" <~ ("rt" ~> "adapters" ~> "dir" ~> "term" ~> cases _Term (var "term")
    Nothing [
    _Term_record>>: "rec" ~>
      "dfields" <~ Core.recordFields (var "rec") $
      "newFields" <<~ Flows.sequence (Lists.zipWith
        ("ad" ~> "f" ~> AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "f")
        (var "adapters")
        (var "dfields")) $
      produce (Core.termRecord (Core.record (Core.rowTypeTypeName (var "rt")) (var "newFields")))]) $
  "forRecordType" <~ ("rt" ~>
    "adapters" <<~ Flows.mapList (fieldAdapter) (Core.rowTypeFields (var "rt")) $
    "lossy" <~ Logic.ors (Lists.map (unaryFunction Compute.adapterIsLossy) (var "adapters")) $
    "sfields'" <~ Lists.map (unaryFunction Compute.adapterTarget) (var "adapters") $
    produce (Compute.adapter
      (var "lossy")
      (var "t")
      (Core.typeRecord (Core.rowType (Core.rowTypeTypeName (var "rt")) (var "sfields'")))
      (AdaptUtils.bidirectional @@ (var "encdec" @@ var "rt" @@ var "adapters")))) $
  cases _Type (var "t")
    Nothing [
    _Type_record>>: "rt" ~> var "forRecordType" @@ var "rt"]

passSet :: TBinding TypeAdapter
passSet = define "passSet" $
  doc "Pass through set types" $
  "t" ~>
  "encdec" <~ ("ad" ~> "dir" ~> "term" ~> cases _Term (var "term")
    Nothing [
    _Term_set>>: "terms" ~>
      "newTerms" <<~ Flows.mapList (AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "ad"))) (Sets.toList (var "terms")) $
      produce (Core.termSet (Sets.fromList (var "newTerms")))]) $
  cases _Type (var "t")
    Nothing [
    _Type_set>>: "st" ~>
      "ad" <<~ termAdapter @@ var "st" $
      produce (Compute.adapter
        (Compute.adapterIsLossy (var "ad"))
        (var "t")
        (MetaTypes.set (Compute.adapterTarget (var "ad")))
        (AdaptUtils.bidirectional @@ (var "encdec" @@ var "ad")))]

passUnion :: TBinding TypeAdapter
passUnion = define "passUnion" $
  doc "Pass through union types" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_union>>: "rt" ~>
      "sfields" <~ Core.rowTypeFields (var "rt") $
      "tname" <~ Core.rowTypeTypeName (var "rt") $
      "getAdapter" <~ ("adaptersMap" ~> "f" ~>
        Maybes.maybe
          (Flows.fail (Strings.cat2 (string "no such field: ") (unwrap _Name @@ (Core.fieldName (var "f")))))
          (unaryFunction Flows.pure)
          (Maps.lookup (Core.fieldName (var "f")) (var "adaptersMap"))) $
      "adapters" <<~ Flows.mapList
        ("f" ~>
          "ad" <<~ fieldAdapter @@ var "f" $
          produce (pair (Core.fieldTypeName (var "f")) (var "ad")))
        (var "sfields") $
      "adaptersMap" <~ Maps.fromList (var "adapters") $
      "lossy" <~ Logic.ors (Lists.map ("pair" ~> Compute.adapterIsLossy (Pairs.second (var "pair"))) (var "adapters")) $
      "sfields'" <~ Lists.map ("pair" ~> Compute.adapterTarget (Pairs.second (var "pair"))) (var "adapters") $
      produce (Compute.adapter
        (var "lossy")
        (var "t")
        (Core.typeUnion (Core.rowType (var "tname") (var "sfields'")))
        (AdaptUtils.bidirectional @@ ("dir" ~> "term" ~>
          -- Note: this is a shortcut, since we anticipate deprecating the current term adapter logic
          produce (var "term"))))]
          -- TODO: consider restoring the following
--          (AdaptUtils.bidirectional @@ (lambdas ["dir", "term"] $
--            "dfield" <<~ withGraphContext @@ (ExtractCore.injectionDef @@ var "tname" @@ var "term") $
--            "ad" <<~ var "getAdapter" @@ var "adaptersMap" @@ var "dfield" $
--            "newField" <<~ AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder $ var "ad") @@ var "dfield" $
--            produce $ Core.termUnion $ Core.injection (var "tname") (var "newField")))]

passUnit :: TBinding TypeAdapter
passUnit = define "passUnit" $
  doc "Pass through unit types" $
  constant (produce (Compute.adapter false Core.typeUnit Core.typeUnit
    (Compute.coder
      (constant (produce Core.termUnit))
      (constant (produce Core.termUnit)))))

passWrapped :: TBinding TypeAdapter
passWrapped = define "passWrapped" $
  doc "Pass through wrapped types" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_wrap>>: "wt" ~>
      "tname" <~ Core.wrappedTypeTypeName (var "wt") $
      "ot" <~ Core.wrappedTypeBody (var "wt") $
      "mapTerm" <~ ("coder" ~> "dir" ~> "term" ~>
        "unwrapped" <<~ withGraphContext @@ (ExtractCore.wrap @@ var "tname" @@ var "term") $
        "newTerm" <<~ AdaptUtils.encodeDecode @@ var "dir" @@ var "coder" @@ var "unwrapped" $
        produce (Core.termWrap (Core.wrappedTerm (var "tname") (var "newTerm")))) $
      "adapter" <<~ termAdapter @@ var "ot" $
      produce (Compute.adapter
        (Compute.adapterIsLossy (var "adapter"))
        (var "t")
        (Core.typeWrap (Core.wrappedType (var "tname") (Compute.adapterTarget (var "adapter"))))
        (AdaptUtils.bidirectional @@ (var "mapTerm" @@ (Compute.adapterCoder (var "adapter")))))]

setToList :: TBinding TypeAdapter
setToList = define "setToList" $
  doc "Convert set types to list types" $
  "t" ~>
  "encode" <~ ("ad" ~> "term" ~> cases _Term (var "term")
    Nothing [
    _Term_set>>: "s" ~> Compute.coderEncode (Compute.adapterCoder (var "ad")) @@ (Core.termList (Sets.toList (var "s")))]) $
  "forListTerm" <~ ("t" ~> cases _Term (var "t")
    Nothing [
    _Term_list>>: "l" ~> produce (Core.termSet (Sets.fromList (var "l")))]) $
  "decode" <~ ("ad" ~> "term" ~>
    "listTerm" <<~ Compute.coderDecode (Compute.adapterCoder (var "ad")) @@ var "term" $
    var "forListTerm" @@ var "listTerm") $
  "forSetType" <~ ("st" ~>
    "ad" <<~ termAdapter @@ (MetaTypes.list (var "st")) $
    produce (Compute.adapter
      (Compute.adapterIsLossy (var "ad"))
      (var "t")
      (Compute.adapterTarget (var "ad"))
      (Compute.coder (var "encode" @@ var "ad") (var "decode" @@ var "ad")))) $
  cases _Type (var "t")
    Nothing [
    _Type_set>>: "st" ~> var "forSetType" @@ var "st"]

simplifyApplication :: TBinding TypeAdapter
simplifyApplication = define "simplifyApplication" $
  doc "Simplify application types" $
  "t" ~>
  "encdec" <~ ("ad" ~> "dir" ~> "term" ~>
    AdaptUtils.encodeDecode @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "term") $
  "forApplicationType" <~ ("at" ~>
    "lhs" <~ Core.applicationTypeFunction (var "at") $
    "ad" <<~ termAdapter @@ var "lhs" $
    produce (Compute.adapter
      false
      (var "t")
      (Compute.adapterTarget (var "ad"))
      (AdaptUtils.bidirectional @@ (var "encdec" @@ var "ad")))) $
  cases _Type (var "t")
    Nothing [
    _Type_application>>: "at" ~> var "forApplicationType" @@ var "at"]

unitToRecord :: TBinding TypeAdapter
unitToRecord = define "unitToRecord" $
    doc "Convert unit terms to records" $
    constant (produce
      (Compute.adapter false Core.typeUnit (Core.typeRecord (Core.rowType unitName (list ([] :: [TTerm FieldType]))))
        (Compute.coder
          (constant (produce (Core.termRecord (Core.record unitName (list ([] :: [TTerm Field]))))))
          (constant (produce Core.termUnit)))))
  where
    unitName = Core.name (string "_Unit")

unionToRecord :: TBinding TypeAdapter
unionToRecord = define "unionToRecord" $
  doc "Convert union types to record types" $
  "t" ~>
  "forField" <~ ("field" ~>
    "fn" <~ Core.fieldName (var "field") $
    "fterm" <~ Core.fieldTerm (var "field") $
    cases _Term (var "fterm")
      Nothing [
      _Term_maybe>>: "opt" ~> Maybes.bind (var "opt") ("t" ~>
        just (Core.field (var "fn") (var "t")))]) $
  "fromRecordFields" <~ ("term" ~> "term'" ~> "t'" ~> "fields" ~>
    "matches" <~ Maybes.mapMaybe (var "forField") (var "fields") $
    Logic.ifElse (Lists.null (var "matches"))
      (Flows.fail (Strings.cat (list [
        string "cannot convert term back to union: ",
        ShowCore.term @@ var "term",
        string " where type = ",
        ShowCore.type_ @@ var "t",
        string "    and target type = ",
        ShowCore.type_ @@ var "t'"])))
      (produce (Lists.head (var "matches")))) $
  "forRecTerm" <~ ("nm" ~> "ad" ~> "term" ~> "recTerm" ~> cases _Term (var "recTerm")
    Nothing [
    _Term_record>>: "rec" ~>
      "fields" <~ Core.recordFields (var "rec") $
      "resultField" <<~
        (var "fromRecordFields"
          @@ var "term"
          @@ (Core.termRecord (Core.record (var "nm") (var "fields")))
          @@ (Compute.adapterTarget (var "ad"))
          @@ var "fields") $
      produce (Core.termUnion (Core.injection (var "nm") (var "resultField")))]) $
  cases _Type (var "t")
    Nothing [
    _Type_union>>: "rt" ~>
      "nm" <~ Core.rowTypeTypeName (var "rt") $
      "sfields" <~ Core.rowTypeFields (var "rt") $
      "target" <~ Core.typeRecord (unionTypeToRecordType @@ var "rt") $
      "toRecordField" <~ ("term" ~> "fn" ~> "f" ~>
        "fn'" <~ Core.fieldTypeName (var "f") $
        Core.field (var "fn'") (Core.termMaybe (Logic.ifElse
          (Equality.equal (var "fn'") (var "fn"))
          (just (var "term"))
          nothing))) $
      "ad" <<~ termAdapter @@ var "target" $
      produce (Compute.adapter
        (Compute.adapterIsLossy (var "ad"))
        (var "t")
        (Compute.adapterTarget (var "ad"))
        (Compute.coder
          ("term'" ~>
            "field" <<~ withGraphContext @@ (ExtractCore.injection @@ (Core.rowTypeTypeName (var "rt")) @@ var "term'") $
            "fn" <~ Core.fieldName (var "field") $
            "term" <~ Core.fieldTerm (var "field") $
            Compute.coderEncode (Compute.adapterCoder (var "ad")) @@
              (Core.termRecord (Core.record (var "nm") (Lists.map (var "toRecordField" @@ var "term" @@ var "fn") (var "sfields")))))
          ("term" ~>
            "recTerm" <<~ Compute.coderDecode (Compute.adapterCoder (var "ad")) @@ var "term" $
            var "forRecTerm" @@ var "nm" @@ var "ad" @@ var "term" @@ var "recTerm")))]

unionTypeToRecordType :: TBinding (RowType -> RowType)
unionTypeToRecordType = define "unionTypeToRecordType" $
  doc "Convert a union row type to a record row type" $
  "rt" ~>
  "makeOptional" <~ ("f" ~>
    "fn" <~ Core.fieldTypeName (var "f") $
    "ft" <~ Core.fieldTypeType (var "f") $
    Core.fieldType (var "fn") (Rewriting.mapBeneathTypeAnnotations @@ unaryFunction Core.typeMaybe @@ var "ft")) $
  Core.rowType (Core.rowTypeTypeName (var "rt")) (Lists.map (var "makeOptional") (Core.rowTypeFields (var "rt")))

wrapToUnwrapped :: TBinding TypeAdapter
wrapToUnwrapped = define "wrapToUnwrapped" $
  doc "Convert wrapped types to unwrapped types" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_wrap>>: "wt" ~>
      "tname" <~ Core.wrappedTypeTypeName (var "wt") $
      "typ" <~ Core.wrappedTypeBody (var "wt") $
      "encode" <~ ("ad" ~> "term" ~>
        "unwrapped" <<~ withGraphContext @@ (ExtractCore.wrap @@ var "tname" @@ var "term") $
        Compute.coderEncode (Compute.adapterCoder (var "ad")) @@ var "unwrapped") $
      "decode" <~ ("ad" ~> "term" ~>
        "decoded" <<~ Compute.coderDecode (Compute.adapterCoder (var "ad")) @@ var "term" $
        produce (Core.termWrap (Core.wrappedTerm (var "tname") (var "decoded")))) $
      "ad" <<~ termAdapter @@ var "typ" $
      produce (Compute.adapter
        false
        (var "t")
        (Compute.adapterTarget (var "ad"))
        (Compute.coder (var "encode" @@ var "ad") (var "decode" @@ var "ad")))]

-- Note: those constructors which cannot be mapped meaningfully at this time are simply
--       preserved as strings.
termAdapter :: TBinding TypeAdapter
termAdapter = define "termAdapter" $
  doc "Create an adapter for any type" $
  "typ" ~>
  "constraints" <~ ("cx" ~> Coders.languageConstraintsProjection (Coders.adapterContextLanguage (var "cx"))) $
  "supported" <~ ("cx" ~> AdaptUtils.typeIsSupported @@ (var "constraints" @@ var "cx")) $
  "variantIsSupported" <~ ("cx" ~> "t" ~>
    Sets.member (Reflect.typeVariant @@ var "t") (Coders.languageConstraintsTypeVariants (var "constraints" @@ var "cx"))) $
  "supportedAtTopLevel" <~ ("cx" ~> "t" ~> Logic.and
    (var "variantIsSupported" @@ var "cx" @@ var "t")
    (Coders.languageConstraintsTypes (var "constraints" @@ var "cx") @@ var "t")) $
  "pass" <~ ("t" ~> cases _TypeVariant (Reflect.typeVariant @@ (Rewriting.deannotateType @@ var "t"))
    Nothing [
    _TypeVariant_annotated>>: constant (list ([] :: [TTerm TypeAdapter])),
    _TypeVariant_application>>: constant (list [passApplication]),
    _TypeVariant_either>>: constant (list [passEither]),
    _TypeVariant_forall>>: constant (list [passForall]),
    _TypeVariant_function>>: constant (list [passFunction]),
    _TypeVariant_list>>: constant (list [passList]),
    _TypeVariant_literal>>: constant (list [passLiteral]),
    _TypeVariant_map>>: constant (list [passMap]),
    _TypeVariant_maybe>>: constant (list [passOptional, maybeToList]),
    _TypeVariant_pair>>: constant (list ([] :: [TTerm TypeAdapter])),
    _TypeVariant_record>>: constant (list [passRecord]),
    _TypeVariant_set>>: constant (list [passSet]),
    _TypeVariant_union>>: constant (list [passUnion]),
    _TypeVariant_unit>>: constant (list [passUnit]),
    _TypeVariant_variable>>: constant (list ([] :: [TTerm TypeAdapter])),
    _TypeVariant_wrap>>: constant (list [passWrapped])]) $
  "trySubstitution" <~ ("t" ~> cases _TypeVariant (Reflect.typeVariant @@ var "t")
    Nothing [
    _TypeVariant_annotated>>: constant (list ([] :: [TTerm TypeAdapter])),
    _TypeVariant_application>>: constant (list [simplifyApplication]),
    _TypeVariant_either>>: constant (list ([] :: [TTerm TypeAdapter])),
    _TypeVariant_forall>>: constant (list [lambdaToMonotype]),
    _TypeVariant_function>>: constant (list [functionToUnion]),
    _TypeVariant_list>>: constant (list ([] :: [TTerm TypeAdapter])),
    _TypeVariant_literal>>: constant (list ([] :: [TTerm TypeAdapter])),
    _TypeVariant_map>>: constant (list ([] :: [TTerm TypeAdapter])),
    _TypeVariant_maybe>>: constant (list [maybeToList]),
    _TypeVariant_pair>>: constant (list ([] :: [TTerm TypeAdapter])),
    _TypeVariant_record>>: constant (list ([] :: [TTerm TypeAdapter])),
    _TypeVariant_set>>: constant (list [setToList]),
    _TypeVariant_union>>: constant (list [unionToRecord]),
    _TypeVariant_unit>>: constant (list [unitToRecord]),
    _TypeVariant_variable>>: constant (list ([] :: [TTerm TypeAdapter])),
    _TypeVariant_wrap>>: constant (list [wrapToUnwrapped])]) $
  "alts" <~ ("cx" ~> "t" ~> Flows.mapList ("c" ~> var "c" @@ var "t")
     (Logic.ifElse (var "supportedAtTopLevel" @@ var "cx" @@ var "t")
       (var "pass" @@ var "t")
       (var "trySubstitution" @@ var "t"))) $
  "dflt" <~ (cases _Type (var "typ")
    (Just (
      "cx" <<~ Monads.getState $
      AdaptUtils.chooseAdapter
        @@ (var "alts" @@ var "cx")
        @@ (var "supported" @@ var "cx")
        @@ ShowCore.type_
        @@ (ShowCore.type_)
        @@ (var "typ"))) [
    -- Account for let-bound variables
    _Type_variable>>: "name" ~> forTypeReference @@ var "name"]) $
  cases _Type (var "typ")
    (Just (
      trace (Strings.cat2 (string "adapter for ") (ShowCore.type_ @@ var "typ"))
      (var "dflt"))) [
    _Type_annotated>>: "at" ~>
      "ad" <<~ termAdapter @@ Core.annotatedTypeBody (var "at") $
      produce (Compute.adapterWithTarget (var "ad")
        (Core.typeAnnotated (Core.annotatedType (Compute.adapterTarget (var "ad")) (Core.annotatedTypeAnnotation (var "at")))))]

withGraphContext :: TBinding (Flow Graph a -> Flow AdapterContext a)
withGraphContext = define "withGraphContext" $
  doc "Execute a flow with graph context" $
  "f" ~>
  "cx" <<~ Monads.getState $
  Monads.withState @@ (Coders.adapterContextGraph (var "cx")) @@ var "f"
