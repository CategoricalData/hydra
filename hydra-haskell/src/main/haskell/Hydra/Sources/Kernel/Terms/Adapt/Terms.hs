{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Adapt.Terms where

-- Standard imports for kernel terms modules
import Hydra.Kernel
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
      el fieldAdapterDef,
      el forTypeReferenceDef,
      el functionProxyNameDef,
      el functionProxyTypeDef,
      el functionToUnionDef,
      el lambdaToMonotypeDef,
      el maybeToListDef,
      el passApplicationDef,
      el passEitherDef,
      el passFunctionDef,
      el passForallDef,
      el passLiteralDef,
      el passListDef,
      el passMapDef,
      el passOptionalDef,
      el passProductDef,
      el passRecordDef,
      el passSetDef,
      el passUnionDef,
      el passUnitDef,
      el passWrappedDef,
      el setToListDef,
      el simplifyApplicationDef,
      el termAdapterDef,
      el unionToRecordDef,
      el unionTypeToRecordTypeDef,
      el unitToRecordDef,
      el wrapToUnwrappedDef,
      el withGraphContextDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

fieldAdapterDef :: TBinding (FieldType -> Flow AdapterContext (SymmetricAdapter AdapterContext FieldType Field))
fieldAdapterDef = define "fieldAdapter" $
  doc "Create an adapter for field types" $
  "ftyp" ~>
  "encdec" <~ ("ad" ~> "dir" ~> "field" ~>
    "name" <~ Core.fieldName (var "field") $
    "term" <~ Core.fieldTerm (var "field") $
    "newTerm" <<~ ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "term" $
    produce $ Core.field (var "name") (var "newTerm")) $
  "ad" <<~ ref termAdapterDef @@ (Core.fieldTypeType (var "ftyp")) $
  produce (Compute.adapter
    (Compute.adapterIsLossy (var "ad"))
    (var "ftyp")
    (Core.fieldType (Core.fieldTypeName (var "ftyp")) (Compute.adapterTarget (var "ad")))
    (ref AdaptUtils.bidirectionalDef @@ (var "encdec" @@ var "ad")))

forTypeReferenceDef :: TBinding (Name -> Flow AdapterContext (SymmetricAdapter AdapterContext Type Term))
forTypeReferenceDef = define "forTypeReference" $
  doc "This function accounts for recursive type definitions" $
  "name" ~>
  "encdec" <~ ("name" ~> "dir" ~> "term" ~>
    "cx" <<~ ref Monads.getStateDef $
    "adapters" <~ Coders.adapterContextAdapters (var "cx") $
    Maybes.maybe
      (Flows.fail (Strings.cat2 "no adapter for reference type " (unwrap _Name @@ var "name")))
      ("ad" ~> ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "term")
      (Maps.lookup (var "name") (var "adapters"))) $
  "forType" <~ ("cx" ~> "adapters" ~> "t" ~>
    "actual" <<~ ref termAdapterDef @@ var "t" $
    "finalAdapters" <~ Maps.insert (var "name") (var "actual") (var "adapters") $
    "finalCx" <~ Coders.adapterContext
      (Coders.adapterContextGraph (var "cx"))
      (Coders.adapterContextLanguage (var "cx"))
      (var "finalAdapters") $
    "ignored2" <<~ ref Monads.putStateDef @@ var "finalCx" $
    produce (var "actual")) $
  "forMissingAdapter" <~ ("cx" ~> "lossy" ~> "adapters" ~> "placeholder" ~>
    "newAdapters" <~ Maps.insert (var "name") (var "placeholder") (var "adapters") $
    "newCx" <~ Coders.adapterContext
      (Coders.adapterContextGraph (var "cx"))
      (Coders.adapterContextLanguage (var "cx"))
      (var "newAdapters") $
    "ignored" <<~ ref Monads.putStateDef @@ var "newCx" $
    "mt" <<~ ref withGraphContextDef @@ (ref Schemas.resolveTypeDef @@ (Core.typeVariable (var "name"))) $
    Maybes.maybe
      (produce (Compute.adapter (var "lossy") (Core.typeVariable (var "name")) (Core.typeVariable (var "name"))
        (ref AdaptUtils.bidirectionalDef @@ ("dir" ~> "term" ~> produce (var "term")))))
      (var "forType" @@ var "cx" @@ var "adapters")
      (var "mt")) $
  "flow" <~ (
    "lossy" <~ false $
    "placeholder" <~ Compute.adapter (var "lossy") (Core.typeVariable (var "name")) (Core.typeVariable (var "name"))
      (ref AdaptUtils.bidirectionalDef @@ (var "encdec" @@ var "name")) $
    "cx" <<~ ref Monads.getStateDef $
    "adapters" <~ Coders.adapterContextAdapters (var "cx") $
    Maybes.maybe
      (var "forMissingAdapter" @@ var "cx" @@ var "lossy" @@ var "adapters" @@ var "placeholder")
      (unaryFunction Flows.pure)
      (Maps.lookup (var "name") (var "adapters"))) $
  trace (Strings.cat2 "adapt named type " (unwrap _Name @@ var "name")) $
  var "flow"

functionProxyNameDef :: TBinding Name
functionProxyNameDef = define "functionProxyName" $
  Core.name "hydra.core.FunctionProxy"

functionProxyTypeDef :: TBinding (a -> Type)
functionProxyTypeDef = define "functionProxyType" $
  doc "Generate a function proxy type for a given domain type" $
  constant (Core.typeUnion (Core.rowType (ref functionProxyNameDef) (list [
    Core.fieldType (Core.nameLift _Elimination_wrap) MetaTypes.string,
    Core.fieldType (Core.nameLift _Elimination_record) MetaTypes.string,
    Core.fieldType (Core.nameLift _Elimination_union) MetaTypes.string,
    Core.fieldType (Core.nameLift _Function_lambda) MetaTypes.string,
    Core.fieldType (Core.nameLift _Function_primitive) MetaTypes.string,
    Core.fieldType (Core.nameLift _Term_variable) MetaTypes.string])))

functionToUnionDef :: TBinding TypeAdapter
functionToUnionDef = define "functionToUnion" $
  doc "Convert function types to union types" $
  "t" ~>
  "encTerm" <~ ("term" ~> "strippedTerm" ~> cases _Term (var "strippedTerm")
    Nothing [
    _Term_function>>: "f" ~> cases _Function (var "f")
      Nothing [
      _Function_elimination>>: "e" ~> cases _Elimination (var "e")
        Nothing [
        _Elimination_wrap>>: "name" ~> Core.termUnion (Core.injection (ref functionProxyNameDef)
          (Core.field (Core.nameLift _Elimination_wrap) (MetaTerms.stringLift (unwrap _Name @@ var "name")))),
        _Elimination_record>>: "r" ~> Core.termUnion (Core.injection (ref functionProxyNameDef)
          (Core.field (Core.nameLift _Elimination_record) (MetaTerms.stringLift (ref ShowCore.termDef @@ var "term")))),
        _Elimination_union>>: "u" ~> Core.termUnion (Core.injection (ref functionProxyNameDef)
          (Core.field (Core.nameLift _Elimination_union) (MetaTerms.stringLift (ref ShowCore.termDef @@ var "term"))))],
      _Function_lambda>>: "l" ~> Core.termUnion (Core.injection (ref functionProxyNameDef)
        (Core.field (Core.nameLift _Function_lambda) (MetaTerms.stringLift (ref ShowCore.termDef @@ var "term")))),
      _Function_primitive>>: "name" ~> Core.termUnion (Core.injection (ref functionProxyNameDef)
        (Core.field (Core.nameLift _Function_primitive) (MetaTerms.stringLift (unwrap _Name @@ var "name"))))],
    _Term_variable>>: "name" ~>
      Core.termUnion (Core.injection (ref functionProxyNameDef) (Core.field (Core.nameLift _Term_variable) (MetaTerms.stringLift (unwrap _Name @@ var "name"))))]) $
  "encode" <~ ("ad" ~> "term" ~>
    "strippedTerm" <~ ref Rewriting.deannotateTermDef @@ var "term" $
    Compute.coderEncode (Compute.adapterCoder (var "ad")) @@ (var "encTerm" @@ var "term" @@ var "strippedTerm")) $
  "readFromString" <~ ("term" ~>
    "s" <<~ ref ExtractCore.stringDef @@ var "term" $
    Maybes.maybe
      (Flows.fail (Strings.cat2 "failed to parse term: " (var "s")))
      (unaryFunction Flows.pure)
      (ref ShowCore.readTermDef @@ var "s")) $
  "decode" <~ ("ad" ~> "term" ~>
    "notFound" <~ ("fname" ~> Flows.fail (Strings.cat2 "unexpected field: " (unwrap _Name @@ var "fname"))) $
    "forCases" <~ ("fterm" ~> ref withGraphContextDef @@ (var "readFromString" @@ var "fterm")) $
    "forLambda" <~ ("fterm" ~> ref withGraphContextDef @@ (var "readFromString" @@ var "fterm")) $
    "forWrapped" <~ ("fterm" ~> ref withGraphContextDef @@ (Flows.map ("s" ~> MetaTerms.unwrap (Core.name (var "s"))) (ref ExtractCore.stringDef @@ var "fterm"))) $
    "forPrimitive" <~ ("fterm" ~> ref withGraphContextDef @@ (Flows.map ("s" ~> MetaTerms.primitiveLift (Core.name (var "s"))) (ref ExtractCore.stringDef @@ var "fterm"))) $
    "forProjection" <~ ("fterm" ~> ref withGraphContextDef @@ (var "readFromString" @@ var "fterm")) $
    "forVariable" <~ ("fterm" ~> ref withGraphContextDef @@ (Flows.map ("s" ~> Core.termVariable (Core.name (var "s"))) (ref ExtractCore.stringDef @@ var "fterm"))) $
    "injTerm" <<~ Compute.coderDecode (Compute.adapterCoder (var "ad")) @@ var "term" $
    "field" <<~ ref withGraphContextDef @@ (ref ExtractCore.injectionDef @@ (ref functionProxyNameDef) @@ var "injTerm") $
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
        "domAd" <<~ ref termAdapterDef @@ var "dom" $
        produce (Core.typeUnion (Core.rowType (ref functionProxyNameDef) (list [
          Core.fieldType (Core.nameLift _Elimination_wrap) MetaTypes.string,
          Core.fieldType (Core.nameLift _Elimination_record) MetaTypes.string,
          Core.fieldType (Core.nameLift _Elimination_union) MetaTypes.string,
          Core.fieldType (Core.nameLift _Function_lambda) MetaTypes.string,
          Core.fieldType (Core.nameLift _Function_primitive) MetaTypes.string,
          Core.fieldType (Core.nameLift _Term_variable) MetaTypes.string])))) $
      "ut" <<~ var "unionType" $
      "ad" <<~ ref termAdapterDef @@ var "ut" $
      produce (Compute.adapter
        (Compute.adapterIsLossy (var "ad"))
        (var "t")
        (Compute.adapterTarget (var "ad"))
        (Compute.coder (var "encode" @@ var "ad") (var "decode" @@ var "ad")))]

lambdaToMonotypeDef :: TBinding TypeAdapter
lambdaToMonotypeDef = define "lambdaToMonotype" $
  doc "Convert forall types to monotypes" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_forall>>: "ft" ~>
      "body" <~ Core.forallTypeBody (var "ft") $
      "ad" <<~ ref termAdapterDef @@ var "body" $
      produce (Compute.adapter
        (Compute.adapterIsLossy (var "ad"))
        (var "t")
        (Compute.adapterTarget (var "ad"))
        (Compute.adapterCoder (var "ad")))]

maybeToListDef :: TBinding TypeAdapter
maybeToListDef = define "maybeToList" $
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
      "ad" <<~ ref termAdapterDef @@ var "ot" $
      produce (Compute.adapter
        false
        (var "t")
        (Core.typeList (Compute.adapterTarget (var "ad")))
        (Compute.coder (var "encode" @@ var "ad") (var "decode" @@ var "ad")))]

passApplicationDef :: TBinding TypeAdapter
passApplicationDef = define "passApplication" $
  doc "Pass through application types" $
  "t" ~>
  "forApplicationType" <~ ("at" ~>
    "lhs" <~ Core.applicationTypeFunction (var "at") $
    "rhs" <~ Core.applicationTypeArgument (var "at") $
    "lhsAd" <<~ ref termAdapterDef @@ var "lhs" $
    "rhsAd" <<~ ref termAdapterDef @@ var "rhs" $
    produce (Compute.adapter
      (Logic.or (Compute.adapterIsLossy (var "lhsAd")) (Compute.adapterIsLossy (var "rhsAd")))
      (var "t")
      (Core.typeApplication (Core.applicationType (Compute.adapterTarget (var "lhsAd")) (Compute.adapterTarget (var "rhsAd"))))
      (ref AdaptUtils.bidirectionalDef @@
        ("dir" ~> "term" ~> ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "lhsAd")) @@ var "term")))) $
  cases _Type (var "t")
    Nothing [
    _Type_application>>: "at" ~> var "forApplicationType" @@ var "at"]

passEitherDef :: TBinding TypeAdapter
passEitherDef = define "passEither" $
  doc "Pass through either types" $
  "t" ~>
  "forEitherType" <~ ("et" ~>
    "left" <~ Core.eitherTypeLeft (var "et") $
    "right" <~ Core.eitherTypeRight (var "et") $
    "leftAd" <<~ ref termAdapterDef @@ var "left" $
    "rightAd" <<~ ref termAdapterDef @@ var "right" $
    produce (Compute.adapter
      (Logic.or (Compute.adapterIsLossy (var "leftAd")) (Compute.adapterIsLossy (var "rightAd")))
      (var "t")
      (Core.typeEither (Core.eitherType (Compute.adapterTarget (var "leftAd")) (Compute.adapterTarget (var "rightAd"))))
      (ref AdaptUtils.bidirectionalDef @@
        ("dir" ~> "term" ~> ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "leftAd")) @@ var "term")))) $
  cases _Type (var "t")
    Nothing [
    _Type_either>>: "et" ~> var "forEitherType" @@ var "et"]

passForallDef :: TBinding TypeAdapter
passForallDef = define "passForall" $
  doc "Pass through forall types" $
  "t" ~>
  "forForallType" <~ ("ft" ~>
    "v" <~ Core.forallTypeParameter (var "ft") $
    "body" <~ Core.forallTypeBody (var "ft") $
    "ad" <<~ ref termAdapterDef @@ var "body" $
    produce (Compute.adapter
      (Compute.adapterIsLossy (var "ad"))
      (var "t")
      (Core.typeForall (Core.forallType (var "v") (Compute.adapterTarget (var "ad"))))
      (ref AdaptUtils.bidirectionalDef @@ ("dir" ~> "term" ~>
        ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "term")))) $
  cases _Type (var "t")
    Nothing [
    _Type_forall>>: "ft" ~> var "forForallType" @@ var "ft"]

passFunctionDef :: TBinding TypeAdapter
passFunctionDef = define "passFunction" $
  doc "Pass through function types with adaptation" $
  "t" ~>
  "toCaseAds" <~ ("dom" ~> "cod" ~> cases _Type (ref Rewriting.deannotateTypeDef @@ var "dom")
    (Just (produce Maps.empty)) [
    _Type_union >>: "rt" ~>
      "pairs" <<~ Flows.mapList
        ("f" ~>
          "ad" <<~ ref fieldAdapterDef @@ Core.fieldType
            (Core.fieldTypeName (var "f"))
            (Core.typeFunction (Core.functionType
              (Core.fieldTypeType (var "f"))
              (var "cod"))) $
          produce (pair (Core.fieldTypeName (var "f")) (var "ad")))
        (Core.rowTypeFields (var "rt")) $
      produce (Maps.fromList (var "pairs"))]) $
  "toOptionAd" <~ ("dom" ~> "cod" ~> cases _Type (ref Rewriting.deannotateTypeDef @@ var "dom")
    (Just (produce nothing)) [
    _Type_maybe >>: "ot" ~>
      Flows.map (unaryFunction just) (ref termAdapterDef @@ MetaTypes.function (var "ot") (var "cod"))]) $
  "getCoder" <~ ("caseAds" ~> "fname" ~> Maybes.maybe
    (ref AdaptUtils.idCoderDef)
    (unaryFunction Compute.adapterCoder)
    (Maps.lookup (var "fname") (var "caseAds"))) $
  "forElimination" <~ ("dir" ~> "codAd" ~> "caseAds" ~> "e" ~> cases _Elimination (var "e")
    Nothing [
    _Elimination_union >>: "cs" ~>
      "n" <~ Core.caseStatementTypeName (var "cs") $
      "def" <~ Core.caseStatementDefault (var "cs") $
      "cases" <~ Core.caseStatementCases (var "cs") $
      "rcases" <<~ Flows.mapList
        ("f" ~> ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (var "getCoder" @@ var "caseAds" @@ Core.fieldName (var "f")) @@ var "f")
        (var "cases") $
      "rdef" <<~ Maybes.maybe
        (produce nothing)
        ("d" ~> Flows.map (unaryFunction just) (ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ Compute.adapterCoder (var "codAd") @@ var "d"))
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
      "newBody" <<~ ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ Compute.adapterCoder (var "codAd") @@ var "body" $
      produce (Core.functionLambda (Core.lambda (var "var") (var "d") (var "newBody"))),
    _Function_primitive >>: "name" ~> produce (Core.functionPrimitive (var "name"))]) $
  "encdec" <~ ("codAd" ~> "caseAds" ~> "dir" ~> "term" ~> cases _Term (ref Rewriting.deannotateTermDef @@ var "term")
    (Just (produce (var "term"))) [
    _Term_function >>: "f" ~> Flows.map (unaryFunction Core.termFunction)
      (var "forFunction" @@ var "dir" @@ var "codAd" @@ var "caseAds" @@ var "f")]) $
  "forFunctionType" <~ ("ft" ~>
    "dom" <~ Core.functionTypeDomain (var "ft") $
    "cod" <~ Core.functionTypeCodomain (var "ft") $
    "domAd" <<~ ref termAdapterDef @@ var "dom" $
    "codAd" <<~ ref termAdapterDef @@ var "cod" $
    "caseAds" <<~ var "toCaseAds" @@ var "dom" @@ var "cod" $
    "optionAd" <<~ var "toOptionAd" @@ var "dom" @@ var "cod" $
    "lossy" <~ Logic.or
      (Compute.adapterIsLossy (var "codAd"))
      (Logic.ors (Lists.map ("pair" ~> Compute.adapterIsLossy (Pairs.second (var "pair"))) (Maps.toList (var "caseAds")))) $
    "target" <~ MetaTypes.function (Compute.adapterTarget (var "domAd")) (Compute.adapterTarget (var "codAd")) $
    produce $ Compute.adapter (var "lossy") (var "t") (var "target")
      (ref AdaptUtils.bidirectionalDef @@ (var "encdec" @@ var "codAd" @@ var "caseAds"))) $
  cases _Type (var "t")
    Nothing [
    _Type_function >>: "ft" ~> var "forFunctionType" @@ var "ft"]

passListDef :: TBinding TypeAdapter
passListDef = define "passList" $
  doc "Pass through list types" $
  "t" ~>
  "encdec" <~ ("ad" ~> "dir" ~> "term" ~> cases _Term (var "term")
    Nothing [
    _Term_list>>: "terms" ~>
      "newTerms" <<~ Flows.mapList (ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "ad"))) (var "terms") $
      produce (Core.termList (var "newTerms"))]) $
  "forListType" <~ ("lt" ~>
    "ad" <<~ ref termAdapterDef @@ var "lt" $
    produce (Compute.adapter
      (Compute.adapterIsLossy (var "ad"))
      (var "t")
      (MetaTypes.list (Compute.adapterTarget (var "ad")))
      (ref AdaptUtils.bidirectionalDef @@ (var "encdec" @@ var "ad")))) $
  cases _Type (var "t")
    Nothing [
    _Type_list>>: "lt" ~> var "forListType" @@ var "lt"]

passLiteralDef :: TBinding TypeAdapter
passLiteralDef = define "passLiteral" $
  doc "Pass through literal types with literal adaptation" $
  "t" ~>
  "encdec" <~ ("ad" ~> "dir" ~> "term" ~>
    "l" <<~ ref withGraphContextDef @@ (ref ExtractCore.literalDef @@ var "term") $
    "l2" <<~ ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "l" $
    produce $ Core.termLiteral $ var "l2") $
  "forLiteral" <~ ("lt" ~>
    "ad" <<~ ref AdaptLiterals.literalAdapterDef @@ var "lt" $
    "step" <~ ref AdaptUtils.bidirectionalDef @@ (var "encdec" @@ var "ad") $
    produce (Compute.adapter
      (Compute.adapterIsLossy (var "ad"))
      (Core.typeLiteral (Compute.adapterSource (var "ad")))
      (Core.typeLiteral (Compute.adapterTarget (var "ad")))
      (var "step"))) $
  cases _Type (var "t")
    Nothing [
    _Type_literal>>: "lt" ~> var "forLiteral" @@ var "lt"]

passMapDef :: TBinding TypeAdapter
passMapDef = define "passMap" $
  doc "Pass through map types" $
  "t" ~>
  "encdec" <~ ("kad" ~> "vad" ~> "dir" ~> "term" ~> cases _Term (var "term")
    Nothing [
    _Term_map>>: "m" ~>
      "newPairs" <<~ Flows.mapList
        ("pair" ~>
          "k" <~ Pairs.first (var "pair") $
          "v" <~ Pairs.second (var "pair") $
          "newK" <<~ ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "kad")) @@ var "k" $
          "newV" <<~ ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "vad")) @@ var "v" $
          produce (pair (var "newK") (var "newV")))
        (Maps.toList (var "m")) $
      produce (Core.termMap (Maps.fromList (var "newPairs")))]) $
  "forMapType" <~ ("mt" ~>
    "kt" <~ Core.mapTypeKeys (var "mt") $
    "vt" <~ Core.mapTypeValues (var "mt") $
    "kad" <<~ ref termAdapterDef @@ var "kt" $
    "vad" <<~ ref termAdapterDef @@ var "vt" $
    produce (Compute.adapter
      (Logic.or (Compute.adapterIsLossy (var "kad")) (Compute.adapterIsLossy (var "vad")))
      (var "t")
      (MetaTypes.map (Compute.adapterTarget (var "kad")) (Compute.adapterTarget (var "vad")))
      (ref AdaptUtils.bidirectionalDef @@ (var "encdec" @@ var "kad" @@ var "vad")))) $
  cases _Type (var "t")
    Nothing [
    _Type_map>>: "mt" ~> var "forMapType" @@ var "mt"]

passOptionalDef :: TBinding TypeAdapter
passOptionalDef = define "passOptional" $
  doc "Pass through optional types" $
  "t" ~>
  "mapTerm" <~ ("coder" ~> "dir" ~> "term" ~>
    "opt" <<~ ref withGraphContextDef @@ (ref ExtractCore.maybeTermDef @@ unaryFunction Flows.pure @@ var "term") $
    "newOpt" <<~ Flows.mapMaybe (ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ var "coder") (var "opt") $
    produce (Core.termMaybe (var "newOpt"))) $
  cases _Type (var "t")
    Nothing [
    _Type_maybe>>: "ot" ~>
      "adapter" <<~ ref termAdapterDef @@ var "ot" $
      produce (Compute.adapter
        (Compute.adapterIsLossy (var "adapter"))
        (var "t")
        (Core.typeMaybe (Compute.adapterTarget (var "adapter")))
        (ref AdaptUtils.bidirectionalDef @@ (var "mapTerm" @@ (Compute.adapterCoder (var "adapter")))))]

passProductDef :: TBinding TypeAdapter
passProductDef = define "passProduct" $
  doc "Pass through product types" $
  "t" ~>
  "encdec" <~ ("ads" ~> "dir" ~> "term" ~> cases _Term (var "term")
    Nothing [
    _Term_product>>: "tuple" ~>
      "newTuple" <<~ Flows.sequence (Lists.zipWith
        ("term" ~> "ad" ~> ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "term")
        (var "tuple")
        (var "ads")) $
      produce (Core.termProduct (var "newTuple"))]) $
  cases _Type (var "t")
    Nothing [
    _Type_product>>: "types" ~>
      "ads" <<~ Flows.mapList (ref termAdapterDef) (var "types") $
      "lossy" <~ Logic.ors (Lists.map (unaryFunction Compute.adapterIsLossy) (var "ads")) $
      produce (Compute.adapter
        (var "lossy")
        (var "t")
        (Core.typeProduct (Lists.map (unaryFunction Compute.adapterTarget) (var "ads")))
        (ref AdaptUtils.bidirectionalDef @@ (var "encdec" @@ var "ads")))]

passRecordDef :: TBinding TypeAdapter
passRecordDef = define "passRecord" $
  doc "Pass through record types" $
  "t" ~>
  "encdec" <~ ("rt" ~> "adapters" ~> "dir" ~> "term" ~> cases _Term (var "term")
    Nothing [
    _Term_record>>: "rec" ~>
      "dfields" <~ Core.recordFields (var "rec") $
      "newFields" <<~ Flows.sequence (Lists.zipWith
        ("ad" ~> "f" ~> ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "f")
        (var "adapters")
        (var "dfields")) $
      produce (Core.termRecord (Core.record (Core.rowTypeTypeName (var "rt")) (var "newFields")))]) $
  "forRecordType" <~ ("rt" ~>
    "adapters" <<~ Flows.mapList (ref fieldAdapterDef) (Core.rowTypeFields (var "rt")) $
    "lossy" <~ Logic.ors (Lists.map (unaryFunction Compute.adapterIsLossy) (var "adapters")) $
    "sfields'" <~ Lists.map (unaryFunction Compute.adapterTarget) (var "adapters") $
    produce (Compute.adapter
      (var "lossy")
      (var "t")
      (Core.typeRecord (Core.rowType (Core.rowTypeTypeName (var "rt")) (var "sfields'")))
      (ref AdaptUtils.bidirectionalDef @@ (var "encdec" @@ var "rt" @@ var "adapters")))) $
  cases _Type (var "t")
    Nothing [
    _Type_record>>: "rt" ~> var "forRecordType" @@ var "rt"]

passSetDef :: TBinding TypeAdapter
passSetDef = define "passSet" $
  doc "Pass through set types" $
  "t" ~>
  "encdec" <~ ("ad" ~> "dir" ~> "term" ~> cases _Term (var "term")
    Nothing [
    _Term_set>>: "terms" ~>
      "newTerms" <<~ Flows.mapList (ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "ad"))) (Sets.toList (var "terms")) $
      produce (Core.termSet (Sets.fromList (var "newTerms")))]) $
  cases _Type (var "t")
    Nothing [
    _Type_set>>: "st" ~>
      "ad" <<~ ref termAdapterDef @@ var "st" $
      produce (Compute.adapter
        (Compute.adapterIsLossy (var "ad"))
        (var "t")
        (MetaTypes.set (Compute.adapterTarget (var "ad")))
        (ref AdaptUtils.bidirectionalDef @@ (var "encdec" @@ var "ad")))]

passUnionDef :: TBinding TypeAdapter
passUnionDef = define "passUnion" $
  doc "Pass through union types" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_union>>: "rt" ~>
      "sfields" <~ Core.rowTypeFields (var "rt") $
      "tname" <~ Core.rowTypeTypeName (var "rt") $
      "getAdapter" <~ ("adaptersMap" ~> "f" ~>
        Maybes.maybe
          (Flows.fail (Strings.cat2 "no such field: " (unwrap _Name @@ (Core.fieldName (var "f")))))
          (unaryFunction Flows.pure)
          (Maps.lookup (Core.fieldName (var "f")) (var "adaptersMap"))) $
      "adapters" <<~ Flows.mapList
        ("f" ~>
          "ad" <<~ ref fieldAdapterDef @@ var "f" $
          produce (pair (Core.fieldTypeName (var "f")) (var "ad")))
        (var "sfields") $
      "adaptersMap" <~ Maps.fromList (var "adapters") $
      "lossy" <~ Logic.ors (Lists.map ("pair" ~> Compute.adapterIsLossy (Pairs.second (var "pair"))) (var "adapters")) $
      "sfields'" <~ Lists.map ("pair" ~> Compute.adapterTarget (Pairs.second (var "pair"))) (var "adapters") $
      produce (Compute.adapter
        (var "lossy")
        (var "t")
        (Core.typeUnion (Core.rowType (var "tname") (var "sfields'")))
        (ref AdaptUtils.bidirectionalDef @@ ("dir" ~> "term" ~>
          -- Note: this is a shortcut, since we anticipate deprecating the current term adapter logic
          produce (var "term"))))]
          -- TODO: consider restoring the following
--          (ref AdaptUtils.bidirectionalDef @@ (lambdas ["dir", "term"] $
--            "dfield" <<~ ref withGraphContextDef @@ (ref ExtractCore.injectionDef @@ var "tname" @@ var "term") $
--            "ad" <<~ var "getAdapter" @@ var "adaptersMap" @@ var "dfield" $
--            "newField" <<~ ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder $ var "ad") @@ var "dfield" $
--            produce $ Core.termUnion $ Core.injection (var "tname") (var "newField")))]

passUnitDef :: TBinding TypeAdapter
passUnitDef = define "passUnit" $
  doc "Pass through unit types" $
  constant (produce (Compute.adapter false Core.typeUnit Core.typeUnit
    (Compute.coder
      (constant (produce Core.termUnit))
      (constant (produce Core.termUnit)))))

passWrappedDef :: TBinding TypeAdapter
passWrappedDef = define "passWrapped" $
  doc "Pass through wrapped types" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_wrap>>: "wt" ~>
      "tname" <~ Core.wrappedTypeTypeName (var "wt") $
      "ot" <~ Core.wrappedTypeBody (var "wt") $
      "mapTerm" <~ ("coder" ~> "dir" ~> "term" ~>
        "unwrapped" <<~ ref withGraphContextDef @@ (ref ExtractCore.wrapDef @@ var "tname" @@ var "term") $
        "newTerm" <<~ ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ var "coder" @@ var "unwrapped" $
        produce (Core.termWrap (Core.wrappedTerm (var "tname") (var "newTerm")))) $
      "adapter" <<~ ref termAdapterDef @@ var "ot" $
      produce (Compute.adapter
        (Compute.adapterIsLossy (var "adapter"))
        (var "t")
        (Core.typeWrap (Core.wrappedType (var "tname") (Compute.adapterTarget (var "adapter"))))
        (ref AdaptUtils.bidirectionalDef @@ (var "mapTerm" @@ (Compute.adapterCoder (var "adapter")))))]

setToListDef :: TBinding TypeAdapter
setToListDef = define "setToList" $
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
    "ad" <<~ ref termAdapterDef @@ (MetaTypes.list (var "st")) $
    produce (Compute.adapter
      (Compute.adapterIsLossy (var "ad"))
      (var "t")
      (Compute.adapterTarget (var "ad"))
      (Compute.coder (var "encode" @@ var "ad") (var "decode" @@ var "ad")))) $
  cases _Type (var "t")
    Nothing [
    _Type_set>>: "st" ~> var "forSetType" @@ var "st"]

simplifyApplicationDef :: TBinding TypeAdapter
simplifyApplicationDef = define "simplifyApplication" $
  doc "Simplify application types" $
  "t" ~>
  "encdec" <~ ("ad" ~> "dir" ~> "term" ~>
    ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "term") $
  "forApplicationType" <~ ("at" ~>
    "lhs" <~ Core.applicationTypeFunction (var "at") $
    "ad" <<~ ref termAdapterDef @@ var "lhs" $
    produce (Compute.adapter
      false
      (var "t")
      (Compute.adapterTarget (var "ad"))
      (ref AdaptUtils.bidirectionalDef @@ (var "encdec" @@ var "ad")))) $
  cases _Type (var "t")
    Nothing [
    _Type_application>>: "at" ~> var "forApplicationType" @@ var "at"]

unitToRecordDef :: TBinding TypeAdapter
unitToRecordDef = define "unitToRecord" $
    doc "Convert unit terms to records" $
    constant (produce
      (Compute.adapter false Core.typeUnit (Core.typeRecord (Core.rowType unitName (list [])))
        (Compute.coder
          (constant (produce (Core.termRecord (Core.record unitName (list [])))))
          (constant (produce Core.termUnit)))))
  where
    unitName = Core.name "_Unit"

unionToRecordDef :: TBinding TypeAdapter
unionToRecordDef = define "unionToRecord" $
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
        "cannot convert term back to union: ",
        ref ShowCore.termDef @@ var "term",
        " where type = ",
        ref ShowCore.typeDef @@ var "t",
        "    and target type = ",
        ref ShowCore.typeDef @@ var "t'"])))
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
      "target" <~ Core.typeRecord (ref unionTypeToRecordTypeDef @@ var "rt") $
      "toRecordField" <~ ("term" ~> "fn" ~> "f" ~>
        "fn'" <~ Core.fieldTypeName (var "f") $
        Core.field (var "fn'") (Core.termMaybe (Logic.ifElse
          (Equality.equal (var "fn'") (var "fn"))
          (just (var "term"))
          nothing))) $
      "ad" <<~ ref termAdapterDef @@ var "target" $
      produce (Compute.adapter
        (Compute.adapterIsLossy (var "ad"))
        (var "t")
        (Compute.adapterTarget (var "ad"))
        (Compute.coder
          ("term'" ~>
            "field" <<~ ref withGraphContextDef @@ (ref ExtractCore.injectionDef @@ (Core.rowTypeTypeName (var "rt")) @@ var "term'") $
            "fn" <~ Core.fieldName (var "field") $
            "term" <~ Core.fieldTerm (var "field") $
            Compute.coderEncode (Compute.adapterCoder (var "ad")) @@
              (Core.termRecord (Core.record (var "nm") (Lists.map (var "toRecordField" @@ var "term" @@ var "fn") (var "sfields")))))
          ("term" ~>
            "recTerm" <<~ Compute.coderDecode (Compute.adapterCoder (var "ad")) @@ var "term" $
            var "forRecTerm" @@ var "nm" @@ var "ad" @@ var "term" @@ var "recTerm")))]

unionTypeToRecordTypeDef :: TBinding (RowType -> RowType)
unionTypeToRecordTypeDef = define "unionTypeToRecordType" $
  doc "Convert a union row type to a record row type" $
  "rt" ~>
  "makeOptional" <~ ("f" ~>
    "fn" <~ Core.fieldTypeName (var "f") $
    "ft" <~ Core.fieldTypeType (var "f") $
    Core.fieldType (var "fn") (ref Rewriting.mapBeneathTypeAnnotationsDef @@ unaryFunction Core.typeMaybe @@ var "ft")) $
  Core.rowType (Core.rowTypeTypeName (var "rt")) (Lists.map (var "makeOptional") (Core.rowTypeFields (var "rt")))

wrapToUnwrappedDef :: TBinding TypeAdapter
wrapToUnwrappedDef = define "wrapToUnwrapped" $
  doc "Convert wrapped types to unwrapped types" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_wrap>>: "wt" ~>
      "tname" <~ Core.wrappedTypeTypeName (var "wt") $
      "typ" <~ Core.wrappedTypeBody (var "wt") $
      "encode" <~ ("ad" ~> "term" ~>
        "unwrapped" <<~ ref withGraphContextDef @@ (ref ExtractCore.wrapDef @@ var "tname" @@ var "term") $
        Compute.coderEncode (Compute.adapterCoder (var "ad")) @@ var "unwrapped") $
      "decode" <~ ("ad" ~> "term" ~>
        "decoded" <<~ Compute.coderDecode (Compute.adapterCoder (var "ad")) @@ var "term" $
        produce (Core.termWrap (Core.wrappedTerm (var "tname") (var "decoded")))) $
      "ad" <<~ ref termAdapterDef @@ var "typ" $
      produce (Compute.adapter
        false
        (var "t")
        (Compute.adapterTarget (var "ad"))
        (Compute.coder (var "encode" @@ var "ad") (var "decode" @@ var "ad")))]

-- Note: those constructors which cannot be mapped meaningfully at this time are simply
--       preserved as strings.
termAdapterDef :: TBinding TypeAdapter
termAdapterDef = define "termAdapter" $
  doc "Create an adapter for any type" $
  "typ" ~>
  "constraints" <~ ("cx" ~> Coders.languageConstraintsProjection (Coders.adapterContextLanguage (var "cx"))) $
  "supported" <~ ("cx" ~> ref AdaptUtils.typeIsSupportedDef @@ (var "constraints" @@ var "cx")) $
  "variantIsSupported" <~ ("cx" ~> "t" ~>
    Sets.member (ref Reflect.typeVariantDef @@ var "t") (Coders.languageConstraintsTypeVariants (var "constraints" @@ var "cx"))) $
  "supportedAtTopLevel" <~ ("cx" ~> "t" ~> Logic.and
    (var "variantIsSupported" @@ var "cx" @@ var "t")
    (Coders.languageConstraintsTypes (var "constraints" @@ var "cx") @@ var "t")) $
  "pass" <~ ("t" ~> cases _TypeVariant (ref Reflect.typeVariantDef @@ (ref Rewriting.deannotateTypeDef @@ var "t"))
    Nothing [
    _TypeVariant_annotated>>: constant (list []),
    _TypeVariant_application>>: constant (list [ref passApplicationDef]),
    _TypeVariant_either>>: constant (list [ref passEitherDef]),
    _TypeVariant_forall>>: constant (list [ref passForallDef]),
    _TypeVariant_function>>: constant (list [ref passFunctionDef]),
    _TypeVariant_list>>: constant (list [ref passListDef]),
    _TypeVariant_literal>>: constant (list [ref passLiteralDef]),
    _TypeVariant_map>>: constant (list [ref passMapDef]),
    _TypeVariant_maybe>>: constant (list [ref passOptionalDef, ref maybeToListDef]),
    _TypeVariant_pair>>: constant (list []),
    _TypeVariant_product>>: constant (list [ref passProductDef]),
    _TypeVariant_record>>: constant (list [ref passRecordDef]),
    _TypeVariant_set>>: constant (list [ref passSetDef]),
    _TypeVariant_union>>: constant (list [ref passUnionDef]),
    _TypeVariant_unit>>: constant (list [ref passUnitDef]),
    _TypeVariant_variable>>: constant (list []),
    _TypeVariant_wrap>>: constant (list [ref passWrappedDef])]) $
  "trySubstitution" <~ ("t" ~> cases _TypeVariant (ref Reflect.typeVariantDef @@ var "t")
    Nothing [
    _TypeVariant_annotated>>: constant (list []),
    _TypeVariant_application>>: constant (list [ref simplifyApplicationDef]),
    _TypeVariant_either>>: constant (list []),
    _TypeVariant_forall>>: constant (list [ref lambdaToMonotypeDef]),
    _TypeVariant_function>>: constant (list [ref functionToUnionDef]),
    _TypeVariant_list>>: constant (list []),
    _TypeVariant_literal>>: constant (list []),
    _TypeVariant_map>>: constant (list []),
    _TypeVariant_maybe>>: constant (list [ref maybeToListDef]),
    _TypeVariant_pair>>: constant (list []),
    _TypeVariant_product>>: constant (list []),
    _TypeVariant_record>>: constant (list []),
    _TypeVariant_set>>: constant (list [ref setToListDef]),
    _TypeVariant_union>>: constant (list [ref unionToRecordDef]),
    _TypeVariant_unit>>: constant (list [ref unitToRecordDef]),
    _TypeVariant_variable>>: constant (list []),
    _TypeVariant_wrap>>: constant (list [ref wrapToUnwrappedDef])]) $
  "alts" <~ ("cx" ~> "t" ~> Flows.mapList ("c" ~> var "c" @@ var "t")
     (Logic.ifElse (var "supportedAtTopLevel" @@ var "cx" @@ var "t")
       (var "pass" @@ var "t")
       (var "trySubstitution" @@ var "t"))) $
  "dflt" <~ (cases _Type (var "typ")
    (Just (
      "cx" <<~ ref Monads.getStateDef $
      ref AdaptUtils.chooseAdapterDef
        @@ (var "alts" @@ var "cx")
        @@ (var "supported" @@ var "cx")
        @@ ref ShowCore.typeDef
        @@ (ref ShowCore.typeDef)
        @@ (var "typ"))) [
    -- Account for let-bound variables
    _Type_variable>>: "name" ~> ref forTypeReferenceDef @@ var "name"]) $
  cases _Type (var "typ")
    (Just (
      trace (Strings.cat2 "adapter for " (ref ShowCore.typeDef @@ var "typ"))
      (var "dflt"))) [
    _Type_annotated>>: "at" ~>
      "ad" <<~ ref termAdapterDef @@ Core.annotatedTypeBody (var "at") $
      produce (Compute.adapterWithTarget (var "ad")
        (Core.typeAnnotated (Core.annotatedType (Compute.adapterTarget (var "ad")) (Core.annotatedTypeAnnotation (var "at")))))]

withGraphContextDef :: TBinding (Flow Graph a -> Flow AdapterContext a)
withGraphContextDef = define "withGraphContext" $
  doc "Execute a flow with graph context" $
  "f" ~>
  "cx" <<~ ref Monads.getStateDef $
  ref Monads.withStateDef @@ (Coders.adapterContextGraph (var "cx")) @@ var "f"
