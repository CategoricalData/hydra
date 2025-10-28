{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Adapt.Terms where

-- Standard imports for term-level kernel modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Json          as Json
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Mantle        as Mantle
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Describe.Core as DescribeCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Literals as Lits
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas as Schemas
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Kernel.Terms.Variants as Variants


module_ :: Module
module_ = Module (Namespace "hydra.adapt.terms") elements
    [ExtractCore.module_, AdaptLiterals.module_, Lits.module_, Rewriting.module_,
      Schemas.module_, ShowCore.module_]
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
      el optionalToListDef,
      el passApplicationDef,
      el passFunctionDef,
      el passForallDef,
      el passLiteralDef,
      el passListDef,
      el passMapDef,
      el passOptionalDef,
      el passProductDef,
      el passRecordDef,
      el passSetDef,
      el passSumDef,
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
  "ad" <<~ ref termAdapterDef @@ (Core.fieldTypeType (var "ftyp")) $
  produce (Compute.adapter
    (Compute.adapterIsLossy (var "ad"))
    (var "ftyp")
    (Core.fieldType (Core.fieldTypeName (var "ftyp")) (Compute.adapterTarget (var "ad")))
    (ref AdaptUtils.bidirectionalDef @@ ("dir" ~> "field" ~>
      "name" <~ Core.fieldName (var "field") $
      "term" <~ Core.fieldTerm (var "field") $
      Flows.map ("newTerm" ~> Core.field (var "name") (var "newTerm"))
        (ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "term"))))

forTypeReferenceDef :: TBinding (Name -> Flow AdapterContext (SymmetricAdapter AdapterContext Type Term))
forTypeReferenceDef = define "forTypeReference" $
  doc "This function accounts for recursive type definitions" $
  "name" ~>
  trace (Strings.cat2 "adapt named type " (unwrap _Name @@ var "name")) (
  "lossy" <~ false $
  "placeholder" <~ Compute.adapter (var "lossy") (Core.typeVariable (var "name")) (Core.typeVariable (var "name"))
    (ref AdaptUtils.bidirectionalDef @@ ("dir" ~> "term" ~>
      "cx" <<~ ref Monads.getStateDef $
      "adapters" <~ Coders.adapterContextAdapters (var "cx") $
      Optionals.maybe
        (Flows.fail (Strings.cat2 "no adapter for reference type " (unwrap _Name @@ var "name")))
        ("ad" ~> ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "term")
        (Maps.lookup (var "name") (var "adapters")))) $
  "cx" <<~ ref Monads.getStateDef $
  "adapters" <~ Coders.adapterContextAdapters (var "cx") $
  Optionals.maybe
    ("newAdapters" <~ Maps.insert (var "name") (var "placeholder") (var "adapters") $
     "newCx" <~ Coders.adapterContext
       (Coders.adapterContextGraph (var "cx"))
       (Coders.adapterContextLanguage (var "cx"))
       (var "newAdapters") $
     "ignored" <<~ ref Monads.putStateDef @@ var "newCx" $
     "mt" <<~ ref withGraphContextDef @@ (ref Schemas.resolveTypeDef @@ (Core.typeVariable (var "name"))) $
     Optionals.maybe
       (produce (Compute.adapter (var "lossy") (Core.typeVariable (var "name")) (Core.typeVariable (var "name"))
         (ref AdaptUtils.bidirectionalDef @@ ("dir" ~> "term" ~> produce (var "term")))))
       ("t" ~>
         "actual" <<~ ref termAdapterDef @@ var "t" $
         "finalAdapters" <~ Maps.insert (var "name") (var "actual") (var "adapters") $
         "finalCx" <~ Coders.adapterContext
           (Coders.adapterContextGraph (var "cx"))
           (Coders.adapterContextLanguage (var "cx"))
           (var "finalAdapters") $
         "ignored2" <<~ ref Monads.putStateDef @@ var "finalCx" $
         produce (var "actual"))
       (var "mt"))
    (unaryFunction Flows.pure)
    (Maps.lookup (var "name") (var "adapters")))

functionProxyNameDef :: TBinding Name
functionProxyNameDef = define "functionProxyName" $
  Core.name "hydra.core.FunctionProxy"

functionProxyTypeDef :: TBinding (a -> Type)
functionProxyTypeDef = define "functionProxyType" $
  doc "Generate a function proxy type for a given domain type" $
  constant (Core.typeUnion (Core.rowType (ref functionProxyNameDef) (list [
    Core.fieldType (Core.nameLift _Elimination_wrap) TTypes.string,
    Core.fieldType (Core.nameLift _Elimination_record) TTypes.string,
    Core.fieldType (Core.nameLift _Elimination_union) TTypes.string,
    Core.fieldType (Core.nameLift _Function_lambda) TTypes.string,
    Core.fieldType (Core.nameLift _Function_primitive) TTypes.string,
    Core.fieldType (Core.nameLift _Term_variable) TTypes.string])))

functionToUnionDef :: TBinding TypeAdapter
functionToUnionDef = define "functionToUnion" $
  doc "Convert function types to union types" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_function>>: "ft" ~>
      "dom" <~ Core.functionTypeDomain (var "ft") $
      "cod" <~ Core.functionTypeCodomain (var "ft") $
      "unionType" <~ (
        "domAd" <<~ ref termAdapterDef @@ var "dom" $
        produce (Core.typeUnion (Core.rowType (ref functionProxyNameDef) (list [
          Core.fieldType (Core.nameLift _Elimination_wrap) TTypes.string,
          Core.fieldType (Core.nameLift _Elimination_record) TTypes.string,
          Core.fieldType (Core.nameLift _Elimination_union) TTypes.string,
          Core.fieldType (Core.nameLift _Function_lambda) TTypes.string,
          Core.fieldType (Core.nameLift _Function_primitive) TTypes.string,
          Core.fieldType (Core.nameLift _Term_variable) TTypes.string])))) $
      "encode" <~ ("ad" ~> "term" ~>
        "strippedTerm" <~ ref Rewriting.deannotateTermDef @@ var "term" $
        Compute.coderEncode (Compute.adapterCoder (var "ad")) @@ (cases _Term (var "strippedTerm")
          Nothing [
          _Term_function>>: "f" ~> cases _Function (var "f")
            Nothing [
            _Function_elimination>>: "e" ~> cases _Elimination (var "e")
              Nothing [
              _Elimination_wrap>>: "name" ~> Core.termUnion (Core.injection (ref functionProxyNameDef)
                (Core.field (Core.nameLift _Elimination_wrap) (TTerms.stringLift (unwrap _Name @@ var "name")))),
              _Elimination_record>>: "r" ~> Core.termUnion (Core.injection (ref functionProxyNameDef)
                (Core.field (Core.nameLift _Elimination_record) (TTerms.stringLift (ref ShowCore.termDef @@ var "term")))),
              _Elimination_union>>: "u" ~> Core.termUnion (Core.injection (ref functionProxyNameDef)
                (Core.field (Core.nameLift _Elimination_union) (TTerms.stringLift (ref ShowCore.termDef @@ var "term"))))],
            _Function_lambda>>: "l" ~> Core.termUnion (Core.injection (ref functionProxyNameDef)
              (Core.field (Core.nameLift _Function_lambda) (TTerms.stringLift (ref ShowCore.termDef @@ var "term")))),
            _Function_primitive>>: "name" ~> Core.termUnion (Core.injection (ref functionProxyNameDef)
              (Core.field (Core.nameLift _Function_primitive) (TTerms.stringLift (unwrap _Name @@ var "name"))))],
          _Term_variable>>: "name" ~>
            Core.termUnion (Core.injection (ref functionProxyNameDef) (Core.field (Core.nameLift _Term_variable) (TTerms.stringLift (unwrap _Name @@ var "name"))))])) $
      "decode" <~ ("ad" ~> "term" ~>
        "readFromString" <~ ("term" ~>
          "s" <<~ ref ExtractCore.stringDef @@ var "term" $
          Optionals.maybe
            (Flows.fail (Strings.cat2 "failed to parse term: " (var "s")))
            (unaryFunction Flows.pure)
            (ref ShowCore.readTermDef @@ var "s")) $
        "notFound" <~ ("fname" ~> Flows.fail (Strings.cat2 "unexpected field: " (unwrap _Name @@ var "fname"))) $
        "forCases" <~ ("fterm" ~> ref withGraphContextDef @@ (var "readFromString" @@ var "fterm")) $
        "forLambda" <~ ("fterm" ~> ref withGraphContextDef @@ (var "readFromString" @@ var "fterm")) $
        "forWrapped" <~ ("fterm" ~> ref withGraphContextDef @@ (Flows.map ("s" ~> TTerms.unwrap (Core.name (var "s"))) (ref ExtractCore.stringDef @@ var "fterm"))) $
        "forPrimitive" <~ ("fterm" ~> ref withGraphContextDef @@ (Flows.map ("s" ~> TTerms.primitiveLift (Core.name (var "s"))) (ref ExtractCore.stringDef @@ var "fterm"))) $
        "forProjection" <~ ("fterm" ~> ref withGraphContextDef @@ (var "readFromString" @@ var "fterm")) $
        "forVariable" <~ ("fterm" ~> ref withGraphContextDef @@ (Flows.map ("s" ~> Core.termVariable (Core.name (var "s"))) (ref ExtractCore.stringDef @@ var "fterm"))) $
        "injTerm" <<~ Compute.coderDecode (Compute.adapterCoder (var "ad")) @@ var "term" $
        "field" <<~ ref withGraphContextDef @@ (ref ExtractCore.injectionDef @@ (ref functionProxyNameDef) @@ var "injTerm") $
        "fname" <~ Core.fieldName (var "field") $
        "fterm" <~ Core.fieldTerm (var "field") $
        Optionals.fromMaybe (var "notFound" @@ var "fname") (Maps.lookup (var "fname") (Maps.fromList (list [
          pair (Core.nameLift _Elimination_wrap) (var "forWrapped" @@ var "fterm"),
          pair (Core.nameLift _Elimination_record) (var "forProjection" @@ var "fterm"),
          pair (Core.nameLift _Elimination_union) (var "forCases" @@ var "fterm"),
          pair (Core.nameLift _Function_lambda) (var "forLambda" @@ var "fterm"),
          pair (Core.nameLift _Function_primitive) (var "forPrimitive" @@ var "fterm"),
          pair (Core.nameLift _Term_variable) (var "forVariable" @@ var "fterm")])))) $
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

optionalToListDef :: TBinding TypeAdapter
optionalToListDef = define "optionalToList" $
  doc "Convert optional types to list types" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_optional>>: "ot" ~>
      "ad" <<~ ref termAdapterDef @@ var "ot" $
      "encode" <~ ("term" ~> cases _Term (var "term")
        Nothing [
        _Term_optional>>: "m" ~> Optionals.maybe
          (produce (TTerms.list []))
          ("r" ~>
            "encoded" <<~ Compute.coderEncode (Compute.adapterCoder (var "ad")) @@ var "r" $
            produce (Core.termList (list [var "encoded"])))
          (var "m")]) $
      "decode" <~ ("term" ~> cases _Term (var "term")
        Nothing [
        _Term_list>>: "l" ~> Flows.map (unaryFunction Core.termOptional) (Logic.ifElse (Lists.null (var "l"))
          (produce nothing)
          ("decoded" <<~ Compute.coderDecode (Compute.adapterCoder (var "ad")) @@ (Lists.head (var "l")) $
           produce (just (var "decoded"))))]) $
      produce (Compute.adapter
        false
        (var "t")
        (Core.typeList (Compute.adapterTarget (var "ad")))
        (Compute.coder (var "encode") (var "decode")))]

passApplicationDef :: TBinding TypeAdapter
passApplicationDef = define "passApplication" $
  doc "Pass through application types" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_application>>: "at" ~>
      "lhs" <~ Core.applicationTypeFunction (var "at") $
      "rhs" <~ Core.applicationTypeArgument (var "at") $
      "lhsAd" <<~ ref termAdapterDef @@ var "lhs" $
      "rhsAd" <<~ ref termAdapterDef @@ var "rhs" $
      produce (Compute.adapter
        (Logic.or (Compute.adapterIsLossy (var "lhsAd")) (Compute.adapterIsLossy (var "rhsAd")))
        (var "t")
        (Core.typeApplication (Core.applicationType (Compute.adapterTarget (var "lhsAd")) (Compute.adapterTarget (var "rhsAd"))))
        (ref AdaptUtils.bidirectionalDef @@
          ("dir" ~> "term" ~> ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "lhsAd")) @@ var "term")))]

passFunctionDef :: TBinding TypeAdapter
passFunctionDef = define "passFunction" $
  doc "Pass through function types with adaptation" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_function >>: "ft" ~>
      "dom" <~ Core.functionTypeDomain (var "ft") $
      "cod" <~ Core.functionTypeCodomain (var "ft") $
      "domAd" <<~ ref termAdapterDef @@ var "dom" $
      "codAd" <<~ ref termAdapterDef @@ var "cod" $
      "caseAds" <<~ cases _Type (ref Rewriting.deannotateTypeDef @@ var "dom")
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
          produce (Maps.fromList (var "pairs"))] $
      "optionAd" <<~ cases _Type (ref Rewriting.deannotateTypeDef @@ var "dom")
        (Just (produce nothing)) [
        _Type_optional >>: "ot" ~>
          Flows.map (unaryFunction just) (ref termAdapterDef @@ TTypes.function (var "ot") (var "cod"))] $
      "lossy" <~ Logic.or
        (Compute.adapterIsLossy (var "codAd"))
        (Logic.ors (Lists.map ("pair" ~> Compute.adapterIsLossy (second (var "pair"))) (Maps.toList (var "caseAds")))) $
      "target" <~ TTypes.function (Compute.adapterTarget (var "domAd")) (Compute.adapterTarget (var "codAd")) $
      "getCoder" <~ ("fname" ~> Optionals.maybe
        (ref AdaptUtils.idCoderDef)
        (unaryFunction Compute.adapterCoder)
        (Maps.lookup (var "fname") (var "caseAds"))) $
      produce $ Compute.adapter (var "lossy") (var "t") (var "target")
        (ref AdaptUtils.bidirectionalDef @@ ("dir" ~> "term" ~>
          cases _Term (ref Rewriting.deannotateTermDef @@ var "term")
            (Just (produce (var "term"))) [
            _Term_function >>: "f" ~>
              Flows.map (unaryFunction Core.termFunction)
                (cases _Function (var "f")
                  Nothing [
                  _Function_elimination >>: "e" ~>
                    Flows.map (unaryFunction Core.functionElimination)
                      (cases _Elimination (var "e")
                        Nothing [
                        _Elimination_union >>: "cs" ~>
                          "n" <~ Core.caseStatementTypeName (var "cs") $
                          "def" <~ Core.caseStatementDefault (var "cs") $
                          "cases" <~ Core.caseStatementCases (var "cs") $
                          "rcases" <<~ Flows.mapList
                            ("f" ~> ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (var "getCoder" @@ Core.fieldName (var "f")) @@ var "f")
                            (var "cases") $
                          "rdef" <<~ Optionals.maybe
                            (produce nothing)
                            ("d" ~> Flows.map (unaryFunction just) (ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ Compute.adapterCoder (var "codAd") @@ var "d"))
                            (var "def") $
                          produce (Core.eliminationUnion (Core.caseStatement (var "n") (var "rdef") (var "rcases")))]),
                  _Function_lambda >>: "l" ~>
                    "var" <~ Core.lambdaParameter (var "l") $
                    "d" <~ Core.lambdaDomain (var "l") $
                    "body" <~ Core.lambdaBody (var "l") $
                    "newBody" <<~ ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ Compute.adapterCoder (var "codAd") @@ var "body" $
                    produce (Core.functionLambda (Core.lambda (var "var") (var "d") (var "newBody"))),
                  _Function_primitive >>: "name" ~> produce (Core.functionPrimitive (var "name"))])]))]

passForallDef :: TBinding TypeAdapter
passForallDef = define "passForall" $
  doc "Pass through forall types" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_forall>>: "ft" ~>
      "v" <~ Core.forallTypeParameter (var "ft") $
      "body" <~ Core.forallTypeBody (var "ft") $
      "ad" <<~ ref termAdapterDef @@ var "body" $
      produce (Compute.adapter
        (Compute.adapterIsLossy (var "ad"))
        (var "t")
        (Core.typeForall (Core.forallType (var "v") (Compute.adapterTarget (var "ad"))))
        (ref AdaptUtils.bidirectionalDef @@ ("dir" ~> "term" ~>
          ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "term")))]

passLiteralDef :: TBinding TypeAdapter
passLiteralDef = define "passLiteral" $
  doc "Pass through literal types with literal adaptation" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_literal>>: "lt" ~>
      "ad" <<~ ref AdaptLiterals.literalAdapterDef @@ var "lt" $
      "step" <~ ref AdaptUtils.bidirectionalDef @@ ("dir" ~> "term" ~>
        "l" <<~ ref withGraphContextDef @@ (ref ExtractCore.literalDef @@ var "term") $
        Flows.map (unaryFunction Core.termLiteral) (ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "l")) $
      produce (Compute.adapter
        (Compute.adapterIsLossy (var "ad"))
        (Core.typeLiteral (Compute.adapterSource (var "ad")))
        (Core.typeLiteral (Compute.adapterTarget (var "ad")))
        (var "step"))]

passListDef :: TBinding TypeAdapter
passListDef = define "passList" $
  doc "Pass through list types" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_list>>: "lt" ~>
      "ad" <<~ ref termAdapterDef @@ var "lt" $
      produce (Compute.adapter
        (Compute.adapterIsLossy (var "ad"))
        (var "t")
        (TTypes.list (Compute.adapterTarget (var "ad")))
        (ref AdaptUtils.bidirectionalDef @@ ("dir" ~> "term" ~> cases _Term (var "term")
          Nothing [
          _Term_list>>: "terms" ~>
            "newTerms" <<~ Flows.mapList (ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "ad"))) (var "terms") $
            produce (Core.termList (var "newTerms"))])))]

passMapDef :: TBinding TypeAdapter
passMapDef = define "passMap" $
  doc "Pass through map types" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_map>>: "mt" ~>
      "kt" <~ Core.mapTypeKeys (var "mt") $
      "vt" <~ Core.mapTypeValues (var "mt") $
      "kad" <<~ ref termAdapterDef @@ var "kt" $
      "vad" <<~ ref termAdapterDef @@ var "vt" $
      produce (Compute.adapter
        (Logic.or (Compute.adapterIsLossy (var "kad")) (Compute.adapterIsLossy (var "vad")))
        (var "t")
        (TTypes.map (Compute.adapterTarget (var "kad")) (Compute.adapterTarget (var "vad")))
        (ref AdaptUtils.bidirectionalDef @@ ("dir" ~> "term" ~> cases _Term (var "term")
          Nothing [
          _Term_map>>: "m" ~>
            "newPairs" <<~ Flows.mapList
              ("pair" ~>
                "k" <~ first (var "pair") $
                "v" <~ second (var "pair") $
                "newK" <<~ ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "kad")) @@ var "k" $
                "newV" <<~ ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "vad")) @@ var "v" $
                produce (pair (var "newK") (var "newV")))
              (Maps.toList (var "m")) $
            produce (Core.termMap (Maps.fromList (var "newPairs")))])))]

passOptionalDef :: TBinding TypeAdapter
passOptionalDef = define "passOptional" $
  doc "Pass through optional types" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_optional>>: "ot" ~>
      "mapTerm" <~ ("coder" ~> "dir" ~> "term" ~>
        "opt" <<~ ref withGraphContextDef @@ (ref ExtractCore.optionalDef @@ unaryFunction Flows.pure @@ var "term") $
        "newOpt" <<~ Flows.mapOptional (ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ var "coder") (var "opt") $
        produce (Core.termOptional (var "newOpt"))) $
      "adapter" <<~ ref termAdapterDef @@ var "ot" $
      produce (Compute.adapter
        (Compute.adapterIsLossy (var "adapter"))
        (var "t")
        (Core.typeOptional (Compute.adapterTarget (var "adapter")))
        (ref AdaptUtils.bidirectionalDef @@ (var "mapTerm" @@ (Compute.adapterCoder (var "adapter")))))]

passProductDef :: TBinding TypeAdapter
passProductDef = define "passProduct" $
  doc "Pass through product types" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_product>>: "types" ~>
      "ads" <<~ Flows.mapList (ref termAdapterDef) (var "types") $
      "lossy" <~ Logic.ors (Lists.map (unaryFunction Compute.adapterIsLossy) (var "ads")) $
      produce (Compute.adapter
        (var "lossy")
        (var "t")
        (Core.typeProduct (Lists.map (unaryFunction Compute.adapterTarget) (var "ads")))
        (ref AdaptUtils.bidirectionalDef @@ ("dir" ~> "term" ~> cases _Term (var "term")
          Nothing [
          _Term_product>>: "tuple" ~>
            "newTuple" <<~ Flows.sequence (Lists.zipWith
              ("term" ~> "ad" ~> ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "term")
              (var "tuple")
              (var "ads")) $
            produce (Core.termProduct (var "newTuple"))])))]

passRecordDef :: TBinding TypeAdapter
passRecordDef = define "passRecord" $
  doc "Pass through record types" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_record>>: "rt" ~>
      "adapters" <<~ Flows.mapList (ref fieldAdapterDef) (Core.rowTypeFields (var "rt")) $
      "lossy" <~ Logic.ors (Lists.map (unaryFunction Compute.adapterIsLossy) (var "adapters")) $
      "sfields'" <~ Lists.map (unaryFunction Compute.adapterTarget) (var "adapters") $
      produce (Compute.adapter
        (var "lossy")
        (var "t")
        (Core.typeRecord (Core.rowType (Core.rowTypeTypeName (var "rt")) (var "sfields'")))
        (ref AdaptUtils.bidirectionalDef @@ ("dir" ~> "term" ~> cases _Term (var "term")
          Nothing [
          _Term_record>>: "rec" ~>
            "dfields" <~ Core.recordFields (var "rec") $
            "newFields" <<~ Flows.sequence (Lists.zipWith
              ("ad" ~> "f" ~> ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "f")
              (var "adapters")
              (var "dfields")) $
            produce (Core.termRecord (Core.record (Core.rowTypeTypeName (var "rt")) (var "newFields")))])))]

passSetDef :: TBinding TypeAdapter
passSetDef = define "passSet" $
  doc "Pass through set types" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_set>>: "st" ~>
      "ad" <<~ ref termAdapterDef @@ var "st" $
      produce (Compute.adapter
        (Compute.adapterIsLossy (var "ad"))
        (var "t")
        (TTypes.set (Compute.adapterTarget (var "ad")))
        (ref AdaptUtils.bidirectionalDef @@ ("dir" ~> "term" ~> cases _Term (var "term")
          Nothing [
          _Term_set>>: "terms" ~>
            "newTerms" <<~ Flows.mapList (ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "ad"))) (Sets.toList (var "terms")) $
            produce (Core.termSet (Sets.fromList (var "newTerms")))])))]

passSumDef :: TBinding TypeAdapter
passSumDef = define "passSum" $
  doc "Pass through sum types" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_sum>>: "types" ~>
      "ads" <<~ Flows.mapList (ref termAdapterDef) (var "types") $
      "lossy" <~ Logic.ors (Lists.map (unaryFunction Compute.adapterIsLossy) (var "ads")) $
      produce (Compute.adapter
        (var "lossy")
        (var "t")
        (Core.typeSum (Lists.map (unaryFunction Compute.adapterTarget) (var "ads")))
        (ref AdaptUtils.bidirectionalDef @@ ("dir" ~> "term" ~> cases _Term (var "term")
          Nothing [
          _Term_sum>>: "s" ~>
            "i" <~ Core.sumIndex (var "s") $
            "n" <~ Core.sumSize (var "s") $
            "term" <~ Core.sumTerm (var "s") $
            "newTerm" <<~ ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (Lists.at (var "i") (var "ads"))) @@ var "term" $
            produce (Core.termSum (Core.sum (var "i") (var "n") (var "newTerm")))])))]

passUnionDef :: TBinding TypeAdapter
passUnionDef = define "passUnion" $
  doc "Pass through union types" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_union>>: "rt" ~>
      "sfields" <~ Core.rowTypeFields (var "rt") $
      "tname" <~ Core.rowTypeTypeName (var "rt") $
      "getAdapter" <~ ("adaptersMap" ~> "f" ~>
        Optionals.maybe
          (Flows.fail (Strings.cat2 "no such field: " (unwrap _Name @@ (Core.fieldName (var "f")))))
          (unaryFunction Flows.pure)
          (Maps.lookup (Core.fieldName (var "f")) (var "adaptersMap"))) $
      "adapters" <<~ Flows.mapList
        ("f" ~>
          "ad" <<~ ref fieldAdapterDef @@ var "f" $
          produce (pair (Core.fieldTypeName (var "f")) (var "ad")))
        (var "sfields") $
      "adaptersMap" <~ Maps.fromList (var "adapters") $
      "lossy" <~ Logic.ors (Lists.map ("pair" ~> Compute.adapterIsLossy (second (var "pair"))) (var "adapters")) $
      "sfields'" <~ Lists.map ("pair" ~> Compute.adapterTarget (second (var "pair"))) (var "adapters") $
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
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_set>>: "st" ~>
      "encode" <~ ("ad" ~> "term" ~> cases _Term (var "term")
        Nothing [
        _Term_set>>: "s" ~> Compute.coderEncode (Compute.adapterCoder (var "ad")) @@ (Core.termList (Sets.toList (var "s")))]) $
      "decode" <~ ("ad" ~> "term" ~>
        "listTerm" <<~ Compute.coderDecode (Compute.adapterCoder (var "ad")) @@ var "term" $
        cases _Term (var "listTerm")
          Nothing [
          _Term_list>>: "l" ~> produce (Core.termSet (Sets.fromList (var "l")))]) $
      "ad" <<~ ref termAdapterDef @@ (TTypes.list (var "st")) $
      produce (Compute.adapter
        (Compute.adapterIsLossy (var "ad"))
        (var "t")
        (Compute.adapterTarget (var "ad"))
        (Compute.coder (var "encode" @@ var "ad") (var "decode" @@ var "ad")))]

simplifyApplicationDef :: TBinding TypeAdapter
simplifyApplicationDef = define "simplifyApplication" $
  doc "Simplify application types" $
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_application>>: "at" ~>
      "lhs" <~ Core.applicationTypeFunction (var "at") $
      "ad" <<~ ref termAdapterDef @@ var "lhs" $
      produce (Compute.adapter
        false
        (var "t")
        (Compute.adapterTarget (var "ad"))
        (ref AdaptUtils.bidirectionalDef @@ ("dir" ~> "term" ~>
          ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "ad")) @@ var "term")))]

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
  "t" ~> cases _Type (var "t")
    Nothing [
    _Type_union>>: "rt" ~>
      "nm" <~ Core.rowTypeTypeName (var "rt") $
      "sfields" <~ Core.rowTypeFields (var "rt") $
      "target" <~ Core.typeRecord (ref unionTypeToRecordTypeDef @@ var "rt") $
      "toRecordField" <~ ("term" ~> "fn" ~> "f" ~>
        "fn'" <~ Core.fieldTypeName (var "f") $
        Core.field (var "fn'") (Core.termOptional (Logic.ifElse
          (Equality.equal (var "fn'") (var "fn"))
          (just (var "term"))
          nothing))) $
      "fromRecordFields" <~ ("term" ~> "term'" ~> "t'" ~> "fields" ~>
        "matches" <~ Optionals.mapMaybe
          ("field" ~>
            "fn" <~ Core.fieldName (var "field") $
            "fterm" <~ Core.fieldTerm (var "field") $
            cases _Term (var "fterm")
              Nothing [
              _Term_optional>>: "opt" ~> Optionals.bind (var "opt") ("t" ~>
                just (Core.field (var "fn") (var "t")))])
          (var "fields") $
        Logic.ifElse (Lists.null (var "matches"))
          (Flows.fail (Strings.cat (list [
            "cannot convert term back to union: ",
            ref ShowCore.termDef @@ var "term",
            " where type = ",
            ref ShowCore.typeDef @@ var "t",
            "    and target type = ",
            ref ShowCore.typeDef @@ var "t'"])))
          (produce (Lists.head (var "matches")))) $
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
            cases _Term (var "recTerm")
              Nothing [
              _Term_record>>: "rec" ~>
                "fields" <~ Core.recordFields (var "rec") $
                "resultField" <<~
                  (var "fromRecordFields"
                    @@ var "term"
                    @@ (Core.termRecord (Core.record (var "nm") (var "fields")))
                    @@ (Compute.adapterTarget (var "ad"))
                    @@ var "fields") $
                produce (Core.termUnion (Core.injection (var "nm") (var "resultField")))])))]

unionTypeToRecordTypeDef :: TBinding (RowType -> RowType)
unionTypeToRecordTypeDef = define "unionTypeToRecordType" $
  doc "Convert a union row type to a record row type" $
  "rt" ~>
  "makeOptional" <~ ("f" ~>
    "fn" <~ Core.fieldTypeName (var "f") $
    "ft" <~ Core.fieldTypeType (var "f") $
    Core.fieldType (var "fn") (ref Rewriting.mapBeneathTypeAnnotationsDef @@ unaryFunction Core.typeOptional @@ var "ft")) $
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
    Sets.member (ref Variants.typeVariantDef @@ var "t") (Coders.languageConstraintsTypeVariants (var "constraints" @@ var "cx"))) $
  "supportedAtTopLevel" <~ ("cx" ~> "t" ~> Logic.and
    (var "variantIsSupported" @@ var "cx" @@ var "t")
    (Coders.languageConstraintsTypes (var "constraints" @@ var "cx") @@ var "t")) $
  "pass" <~ ("t" ~> cases _TypeVariant (ref Variants.typeVariantDef @@ (ref Rewriting.deannotateTypeDef @@ var "t"))
    Nothing [
    _TypeVariant_application>>: constant (list [ref passApplicationDef]),
    _TypeVariant_forall>>: constant (list [ref passForallDef]),
    _TypeVariant_function>>: constant (list [ref passFunctionDef]),
    _TypeVariant_list>>: constant (list [ref passListDef]),
    _TypeVariant_literal>>: constant (list [ref passLiteralDef]),
    _TypeVariant_map>>: constant (list [ref passMapDef]),
    _TypeVariant_optional>>: constant (list [ref passOptionalDef, ref optionalToListDef]),
    _TypeVariant_product>>: constant (list [ref passProductDef]),
    _TypeVariant_record>>: constant (list [ref passRecordDef]),
    _TypeVariant_set>>: constant (list [ref passSetDef]),
    _TypeVariant_sum>>: constant (list [ref passSumDef]),
    _TypeVariant_union>>: constant (list [ref passUnionDef]),
    _TypeVariant_unit>>: constant (list [ref passUnitDef]),
    _TypeVariant_wrap>>: constant (list [ref passWrappedDef])]) $
  "trySubstitution" <~ ("t" ~> cases _TypeVariant (ref Variants.typeVariantDef @@ var "t")
    Nothing [
    _TypeVariant_application>>: constant (list [ref simplifyApplicationDef]),
    _TypeVariant_function>>: constant (list [ref functionToUnionDef]),
    _TypeVariant_forall>>: constant (list [ref lambdaToMonotypeDef]),
    _TypeVariant_optional>>: constant (list [ref optionalToListDef]),
    _TypeVariant_set>>: constant (list [ref setToListDef]),
    _TypeVariant_union>>: constant (list [ref unionToRecordDef]),
    _TypeVariant_unit>>: constant (list [ref unitToRecordDef]),
    _TypeVariant_wrap>>: constant (list [ref wrapToUnwrappedDef])]) $
  "alts" <~ ("cx" ~> "t" ~> Flows.mapList ("c" ~> var "c" @@ var "t")
     (Logic.ifElse (var "supportedAtTopLevel" @@ var "cx" @@ var "t")
       (var "pass" @@ var "t")
       (var "trySubstitution" @@ var "t"))) $
  cases _Type (var "typ")
    (Just (
      trace (Strings.cat2 "adapter for " (ref DescribeCore.typeDef @@ var "typ"))
      (cases _Type (var "typ")
        (Just (
          "cx" <<~ ref Monads.getStateDef $
          ref AdaptUtils.chooseAdapterDef
            @@ (var "alts" @@ var "cx")
            @@ (var "supported" @@ var "cx")
            @@ ref ShowCore.typeDef
            @@ (ref DescribeCore.typeDef)
            @@ (var "typ"))) [
        -- Account for let-bound variables
        _Type_variable>>: "name" ~> ref forTypeReferenceDef @@ var "name"]))) [
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
