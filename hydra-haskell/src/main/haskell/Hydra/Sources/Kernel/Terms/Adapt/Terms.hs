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
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas as Schemas
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Kernel.Terms.Variants as Variants


module_ :: Module
module_ = Module (Namespace "hydra.adapt.terms") elements
    [ExtractCore.module_, AdaptLiterals.module_,
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
     el listToSetDef,
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
     el simplifyApplicationDef,
     el termAdapterDef,
     el unionToRecordDef,
     el unionTypeToRecordTypeDef,
     el unitToRecordDef,
     el wrapToUnwrappedDef,
     el withGraphContextDef]

define :: String -> TTerm a -> TElement a
define = definitionInModule module_

fieldAdapterDef :: TElement (FieldType -> Flow AdapterContext (SymmetricAdapter AdapterContext FieldType Field))
fieldAdapterDef = define "fieldAdapter" $
  doc "Create an adapter for field types" $
  lambda "ftyp" $
    Flows.bind (ref termAdapterDef @@ (Core.fieldTypeType $ var "ftyp")) $ lambda "ad" $
      Flows.pure $ Compute.adapter
        (Compute.adapterIsLossy $ var "ad")
        (var "ftyp")
        (Core.fieldType (Core.fieldTypeName $ var "ftyp") (Compute.adapterTarget $ var "ad"))
        (ref AdaptUtils.bidirectionalDef @@ (lambdas ["dir", "field"] $ lets [
          "name">: Core.fieldName $ var "field",
          "term">: Core.fieldTerm $ var "field"] $
          Flows.map (lambda "newTerm" $ Core.field (var "name") (var "newTerm")) $
            ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder $ var "ad") @@ var "term"))

forTypeReferenceDef :: TElement (Name -> Flow AdapterContext (SymmetricAdapter AdapterContext Type Term))
forTypeReferenceDef = define "forTypeReference" $
  doc "This function accounts for recursive type definitions" $
  lambda "name" $
    ref Monads.withTraceDef
      @@ (Strings.cat2 (string "adapt named type ") (unwrap _Name @@ var "name"))
      @@ (lets [
        "lossy">: false,
        "placeholder">: Compute.adapter (var "lossy") (Core.typeVariable $ var "name") (Core.typeVariable $ var "name") $
          ref AdaptUtils.bidirectionalDef @@ (lambdas ["dir", "term"] $
            bind "cx" (ref Monads.getStateDef) $ lets [
              "adapters">: Coders.adapterContextAdapters $ var "cx"] $
              Optionals.maybe
                (Flows.fail $ Strings.cat2 (string "no adapter for reference type ") (unwrap _Name @@ var "name"))
                (lambda "ad" $ ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder $ var "ad") @@ var "term")
                (Maps.lookup (var "name") (var "adapters")))] $
      bind "cx" (ref Monads.getStateDef) $ lets [
        "adapters">: Coders.adapterContextAdapters $ var "cx"] $
        Optionals.maybe
          (lets [
            "newAdapters">: Maps.insert (var "name") (var "placeholder") (var "adapters"),
            "newCx">: Coders.adapterContext
              (Coders.adapterContextGraph $ var "cx")
              (Coders.adapterContextLanguage $ var "cx")
              (var "newAdapters")] $
            Flows.bind (ref Monads.putStateDef @@ var "newCx") $ constant $
              bind "mt" (ref withGraphContextDef @@ (ref Schemas.resolveTypeDef @@ (Core.typeVariable $ var "name"))) $
                Optionals.maybe
                  (Flows.pure $ Compute.adapter (var "lossy") (Core.typeVariable $ var "name") (Core.typeVariable $ var "name") $
                    ref AdaptUtils.bidirectionalDef @@ (lambdas ["dir", "term"] $ Flows.pure $ var "term"))
                  (lambda "t" $
                    bind "actual" (ref termAdapterDef @@ var "t") $ lets [
                      "finalAdapters">: Maps.insert (var "name") (var "actual") (var "adapters"),
                      "finalCx">: Coders.adapterContext
                        (Coders.adapterContextGraph $ var "cx")
                        (Coders.adapterContextLanguage $ var "cx")
                        (var "finalAdapters")] $
                    Flows.bind (ref Monads.putStateDef @@ var "finalCx") $ constant $ Flows.pure $ var "actual")
                  (var "mt"))
          (unaryFunction Flows.pure)
          (Maps.lookup (var "name") (var "adapters")))

functionProxyNameDef :: TElement Name
functionProxyNameDef = define "functionProxyName" $
  Core.name "hydra.core.FunctionProxy"

functionProxyTypeDef :: TElement (a -> Type)
functionProxyTypeDef = define "functionProxyType" $
  doc "Generate a function proxy type for a given domain type" $
  constant $ Core.typeUnion $ Core.rowType (ref functionProxyNameDef) $ list [
    Core.fieldType (Core.nameLift _Elimination_wrap) TTypes.string,
    Core.fieldType (Core.nameLift _Elimination_record) TTypes.string,
    Core.fieldType (Core.nameLift _Elimination_union) TTypes.string,
    Core.fieldType (Core.nameLift _Function_lambda) TTypes.string,
    Core.fieldType (Core.nameLift _Function_primitive) TTypes.string,
    Core.fieldType (Core.nameLift _Term_variable) TTypes.string]

functionToUnionDef :: TElement TypeAdapter
functionToUnionDef = define "functionToUnion" $
  doc "Convert function types to union types" $
  lambda "t" $ cases _Type (var "t") Nothing [
    _Type_function>>: lambda "ft" $ lets [
      "dom">: Core.functionTypeDomain $ var "ft",
      "cod">: Core.functionTypeCodomain $ var "ft",
      "unionType">:
        bind "domAd" (ref termAdapterDef @@ var "dom") $
        Flows.pure $ Core.typeUnion $ Core.rowType (ref functionProxyNameDef) $ list [
          Core.fieldType (Core.nameLift _Elimination_wrap) TTypes.string,
          Core.fieldType (Core.nameLift _Elimination_record) TTypes.string,
          Core.fieldType (Core.nameLift _Elimination_union) TTypes.string,
          Core.fieldType (Core.nameLift _Function_lambda) TTypes.string,
          Core.fieldType (Core.nameLift _Function_primitive) TTypes.string,
          Core.fieldType (Core.nameLift _Term_variable) TTypes.string],
      "encode">: lambdas ["ad", "term"] $ lets [
        "strippedTerm">: ref Rewriting.deannotateTermDef @@ var "term"] $
        Compute.coderEncode (Compute.adapterCoder $ var "ad") @@ (cases _Term (var "strippedTerm") Nothing [
          _Term_function>>: lambda "f" $ cases _Function (var "f") Nothing [
            _Function_elimination>>: lambda "e" $ cases _Elimination (var "e") Nothing [
              _Elimination_wrap>>: lambda "name" $ Core.termUnion $ Core.injection (ref functionProxyNameDef) $
                Core.field (Core.nameLift _Elimination_wrap) $ TTerms.stringLift $ unwrap _Name @@ var "name",
              _Elimination_record>>: lambda "r" $ Core.termUnion $ Core.injection (ref functionProxyNameDef) $
                Core.field (Core.nameLift _Elimination_record) $ TTerms.stringLift (ref ShowCore.termDef @@ var "term"),
              _Elimination_union>>: lambda "u" $ Core.termUnion $ Core.injection (ref functionProxyNameDef) $
                Core.field (Core.nameLift _Elimination_union) $ TTerms.stringLift (ref ShowCore.termDef @@ var "term")],
            _Function_lambda>>: lambda "l" $ Core.termUnion $ Core.injection (ref functionProxyNameDef) $
              Core.field (Core.nameLift _Function_lambda) $ TTerms.stringLift (ref ShowCore.termDef @@ var "term"),
            _Function_primitive>>: lambda "name" $ Core.termUnion $ Core.injection (ref functionProxyNameDef) $
              Core.field (Core.nameLift _Function_primitive) $ TTerms.stringLift $ unwrap _Name @@ var "name"],
          _Term_variable>>: lambda "name" $
            Core.termUnion $ Core.injection (ref functionProxyNameDef) $ Core.field (Core.nameLift _Term_variable) $ TTerms.stringLift $ unwrap _Name @@ var "name"]),
      "decode">: lambdas ["ad", "term"] $ lets [
        "readFromString">: lambda "term" $
          bind "s" (ref ExtractCore.stringDef @@ var "term") $
            Optionals.maybe
              (Flows.fail $ Strings.cat2 ("failed to parse term: ") (var "s"))
              (unaryFunction Flows.pure)
              (ref ShowCore.readTermDef @@ var "s"),
        "notFound">: lambda "fname" $ Flows.fail $ Strings.cat2 (string "unexpected field: ") (unwrap _Name @@ var "fname"),
        "forCases">: lambda "fterm" $ ref withGraphContextDef @@ (var "readFromString" @@ var "fterm"),
        "forLambda">: lambda "fterm" $ ref withGraphContextDef @@ (var "readFromString" @@ var "fterm"),
        "forWrapped">: lambda "fterm" $ ref withGraphContextDef @@ (Flows.map (lambda "s" $ TTerms.unwrap $ Core.name $ var "s") (ref ExtractCore.stringDef @@ var "fterm")),
        "forPrimitive">: lambda "fterm" $ ref withGraphContextDef @@ (Flows.map (lambda "s" $ TTerms.primitiveLift $ Core.name $ var "s") (ref ExtractCore.stringDef @@ var "fterm")),
        "forProjection">: lambda "fterm" $ ref withGraphContextDef @@ (var "readFromString" @@ var "fterm"),
        "forVariable">: lambda "fterm" $ ref withGraphContextDef @@ (Flows.map (lambda "s" $ Core.termVariable $ Core.name $ var "s") (ref ExtractCore.stringDef @@ var "fterm"))] $
        bind "injTerm" (Compute.coderDecode (Compute.adapterCoder $ var "ad") @@ var "term") $
        bind "field" (ref withGraphContextDef @@ (ref ExtractCore.injectionDef @@ (ref functionProxyNameDef) @@ var "injTerm")) $ lets [
            "fname">: Core.fieldName $ var "field",
            "fterm">: Core.fieldTerm $ var "field"] $
            Optionals.fromMaybe (var "notFound" @@ var "fname") $ Maps.lookup (var "fname") $ Maps.fromList $ list [
              pair (Core.nameLift _Elimination_wrap) (var "forWrapped" @@ var "fterm"),
              pair (Core.nameLift _Elimination_record) (var "forProjection" @@ var "fterm"),
              pair (Core.nameLift _Elimination_union) (var "forCases" @@ var "fterm"),
              pair (Core.nameLift _Function_lambda) (var "forLambda" @@ var "fterm"),
              pair (Core.nameLift _Function_primitive) (var "forPrimitive" @@ var "fterm"),
              pair (Core.nameLift _Term_variable) (var "forVariable" @@ var "fterm")]] $
    bind "ut" (var "unionType") $
    bind "ad" (ref termAdapterDef @@ var "ut") $
    Flows.pure $ Compute.adapter
      (Compute.adapterIsLossy $ var "ad")
      (var "t")
      (Compute.adapterTarget $ var "ad")
      (Compute.coder (var "encode" @@ var "ad") (var "decode" @@ var "ad"))]

lambdaToMonotypeDef :: TElement TypeAdapter
lambdaToMonotypeDef = define "lambdaToMonotype" $
  doc "Convert forall types to monotypes" $
  lambda "t" $ cases _Type (var "t") Nothing [
    _Type_forall>>: lambda "ft" $ lets [
        "body">: Core.forallTypeBody $ var "ft"] $
        bind "ad" (ref termAdapterDef @@ var "body") $
        Flows.pure $ Compute.adapter
          (Compute.adapterIsLossy $ var "ad")
          (var "t")
          (Compute.adapterTarget $ var "ad")
          (Compute.adapterCoder $ var "ad")]

listToSetDef :: TElement TypeAdapter
listToSetDef = define "listToSet" $
  doc "Convert set types to list types" $
  lambda "t" $ cases _Type (var "t") Nothing [
    _Type_set>>: lambda "st" $ lets [
      "encode">: lambda "ad" $ lambda "term" $ cases _Term (var "term") Nothing [
        _Term_set>>: lambda "s" $ Compute.coderEncode (Compute.adapterCoder $ var "ad") @@ (Core.termList $ Sets.toList $ var "s")],
      "decode">:
        lambdas ["ad", "term"] $
        bind "listTerm" (Compute.coderDecode (Compute.adapterCoder $ var "ad") @@ var "term") $
          cases _Term (var "listTerm") Nothing [
            _Term_list>>: lambda "l" $ Flows.pure $ Core.termSet $ Sets.fromList $ var "l"]] $
      bind "ad" (ref termAdapterDef @@ (TTypes.list $ var "st")) $
      Flows.pure $ Compute.adapter
        (Compute.adapterIsLossy $ var "ad")
        (var "t")
        (Compute.adapterTarget $ var "ad")
        (Compute.coder (var "encode" @@ var "ad") (var "decode" @@ var "ad"))]

optionalToListDef :: TElement TypeAdapter
optionalToListDef = define "optionalToList" $
  doc "Convert optional types to list types" $
  lambda "t" $ cases _Type (var "t") Nothing [
    _Type_optional>>: lambda "ot" $
      bind "ad" (ref termAdapterDef @@ var "ot") $ lets [
        "encode">: lambda "term" $ cases _Term (var "term") Nothing [
          _Term_optional>>: lambda "m" $ Optionals.maybe
            (Flows.pure $ TTerms.list [])
            (lambda "r" $
              bind "encoded" (Compute.coderEncode (Compute.adapterCoder $ var "ad") @@ var "r") $
              Flows.pure $ Core.termList $ list [var "encoded"])
            (var "m")],
        "decode">: lambda "term" $ cases _Term (var "term") Nothing [
          _Term_list>>: lambda "l" $ Flows.map (unaryFunction Core.termOptional) $ Logic.ifElse (Lists.null $ var "l")
            (Flows.pure $ nothing)
            (bind "decoded" (Compute.coderDecode (Compute.adapterCoder $ var "ad") @@ (Lists.head $ var "l")) $
              Flows.pure $ just $ var "decoded")]] $
      Flows.pure $ Compute.adapter
        false
        (var "t")
        (Core.typeList $ Compute.adapterTarget $ var "ad")
        (Compute.coder (var "encode") (var "decode"))]

passApplicationDef :: TElement TypeAdapter
passApplicationDef = define "passApplication" $
  doc "Pass through application types" $
  lambda "t" $ cases _Type (var "t") Nothing [
    _Type_application>>: lambda "at" $ lets [
        "lhs">: Core.applicationTypeFunction $ var "at",
        "rhs">: Core.applicationTypeArgument $ var "at"] $
        bind "lhsAd" (ref termAdapterDef @@ var "lhs") $
        bind "rhsAd" (ref termAdapterDef @@ var "rhs") $
        Flows.pure $ Compute.adapter
          (Logic.or (Compute.adapterIsLossy $ var "lhsAd") (Compute.adapterIsLossy $ var "rhsAd"))
          (var "t")
          (Core.typeApplication $ Core.applicationType (Compute.adapterTarget $ var "lhsAd") (Compute.adapterTarget $ var "rhsAd"))
          (ref AdaptUtils.bidirectionalDef @@
            (lambdas ["dir", "term"] $ ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder $ var "lhsAd") @@ var "term"))]

passFunctionDef :: TElement TypeAdapter
passFunctionDef = define "passFunction" $
  doc "Pass through function types with adaptation" $
  lambda "t" $ cases _Type (var "t") Nothing [
    _Type_function >>: lambda "ft" $ lets [
      "dom">: Core.functionTypeDomain $ var "ft",
      "cod">: Core.functionTypeCodomain $ var "ft"] $
      bind "domAd" (ref termAdapterDef @@ var "dom") $
      bind "codAd" (ref termAdapterDef @@ var "cod") $
      bind "caseAds" (cases _Type (ref Rewriting.deannotateTypeDef @@ var "dom") (Just $ Flows.pure $ Maps.empty) [
        _Type_union >>: lambda "rt" $
          bind "pairs" (Flows.mapList
            (lambda "f" $
              bind "ad" (ref fieldAdapterDef @@ Core.fieldType
                (Core.fieldTypeName $ var "f")
                (Core.typeFunction $ Core.functionType
                  (Core.fieldTypeType $ var "f")
                  (var "cod")))
              $ Flows.pure $ pair (Core.fieldTypeName $ var "f") (var "ad"))
            (Core.rowTypeFields $ var "rt")) $
          Flows.pure $ Maps.fromList $ var "pairs"]) $
      bind "optionAd" (cases _Type (ref Rewriting.deannotateTypeDef @@ var "dom") (Just $ Flows.pure nothing) [
        _Type_optional >>: lambda "ot" $
          Flows.map (unaryFunction just) $ ref termAdapterDef @@ TTypes.function (var "ot") (var "cod")]) $ lets [
      "lossy">: Logic.or
        (Compute.adapterIsLossy $ var "codAd")
        (Logic.ors $ Lists.map (lambda "pair" $ Compute.adapterIsLossy $ second $ var "pair") $ Maps.toList $ var "caseAds"),
      "target">: TTypes.function (Compute.adapterTarget $ var "domAd") (Compute.adapterTarget $ var "codAd"),
      "getCoder">: lambda "fname" $ Optionals.maybe
        (ref AdaptUtils.idCoderDef)
        (unaryFunction Compute.adapterCoder)
        (Maps.lookup (var "fname") (var "caseAds"))] $
      Flows.pure $ Compute.adapter (var "lossy") (var "t") (var "target") $
        ref AdaptUtils.bidirectionalDef @@ (lambdas ["dir", "term"] $
          cases _Term (ref Rewriting.deannotateTermDef @@ var "term") (Just $ Flows.pure $ var "term") [
            _Term_function >>: lambda "f" $
              Flows.map (unaryFunction Core.termFunction) $
                cases _Function (var "f") Nothing [
                  _Function_elimination >>: lambda "e" $
                    Flows.map (unaryFunction Core.functionElimination) $
                      cases _Elimination (var "e") Nothing [
                        _Elimination_union >>: lambda "cs" $ lets [
                          "n">: Core.caseStatementTypeName $ var "cs",
                          "def">: Core.caseStatementDefault $ var "cs",
                          "cases">: Core.caseStatementCases $ var "cs"] $
                          bind "rcases" (Flows.mapList
                            (lambda "f" $ ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (var "getCoder" @@ Core.fieldName (var "f")) @@ var "f")
                            (var "cases")) $
                          bind "rdef" (Optionals.maybe
                            (Flows.pure nothing)
                            (lambda "d" $ Flows.map (unaryFunction just) $ ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ Compute.adapterCoder (var "codAd") @@ var "d")
                            (var "def")) $
                          Flows.pure $ Core.eliminationUnion $ Core.caseStatement (var "n") (var "rdef") (var "rcases")],
                  _Function_lambda >>: lambda "l" $ lets [
                    "var">: Core.lambdaParameter $ var "l",
                    "d" >: Core.lambdaDomain $ var "l",
                    "body">: Core.lambdaBody $ var "l"] $
                    bind "newBody" (ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ Compute.adapterCoder (var "codAd") @@ var "body") $
                    Flows.pure $ Core.functionLambda $ Core.lambda (var "var") (var "d") (var "newBody"),
                  _Function_primitive >>: lambda "name" $ Flows.pure $ Core.functionPrimitive $ var "name"]]
         )]

passForallDef :: TElement TypeAdapter
passForallDef = define "passForall" $
  doc "Pass through forall types" $
  lambda "t" $ cases _Type (var "t") Nothing [
    _Type_forall>>: lambda "ft" $ lets [
        "v">: Core.forallTypeParameter $ var "ft",
        "body">: Core.forallTypeBody $ var "ft"] $
        Flows.bind (ref termAdapterDef @@ var "body") $ lambda "ad" $
          Flows.pure $ Compute.adapter
            (Compute.adapterIsLossy $ var "ad")
            (var "t")
            (Core.typeForall $ Core.forallType (var "v") (Compute.adapterTarget $ var "ad"))
            (ref AdaptUtils.bidirectionalDef @@ (lambdas ["dir", "term"] $
              ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder $ var "ad") @@ var "term"))]

passLiteralDef :: TElement TypeAdapter
passLiteralDef = define "passLiteral" $
  doc "Pass through literal types with literal adaptation" $
  lambda "t" $ cases _Type (var "t") Nothing [
    _Type_literal>>: lambda "lt" $
      bind "ad" (ref AdaptLiterals.literalAdapterDef @@ var "lt") $ lets [
      "step">: ref AdaptUtils.bidirectionalDef @@ (lambdas ["dir", "term"] $
        bind "l" (ref withGraphContextDef @@ (ref ExtractCore.literalDef @@ var "term")) $
        Flows.map (unaryFunction $ Core.termLiteral) (ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder $ var "ad") @@ var "l"))] $
      Flows.pure $ Compute.adapter
        (Compute.adapterIsLossy $ var "ad")
        (Core.typeLiteral $ Compute.adapterSource $ var "ad")
        (Core.typeLiteral $ Compute.adapterTarget $ var "ad")
        (var "step")]

passListDef :: TElement TypeAdapter
passListDef = define "passList" $
  doc "Pass through list types" $
  lambda "t" $ cases _Type (var "t") Nothing [
    _Type_list>>: lambda "lt" $
      bind "ad" (ref termAdapterDef @@ var "lt") $
      Flows.pure $ Compute.adapter
        (Compute.adapterIsLossy $ var "ad")
        (var "t")
        (TTypes.list $ Compute.adapterTarget $ var "ad")
        (ref AdaptUtils.bidirectionalDef @@ (lambdas ["dir", "term"] $ cases _Term (var "term") Nothing [
          _Term_list>>: lambda "terms" $
            bind "newTerms" (Flows.mapList (ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder $ var "ad")) (var "terms")) $
            Flows.pure $ Core.termList $ var "newTerms"]))]

passMapDef :: TElement TypeAdapter
passMapDef = define "passMap" $
  doc "Pass through map types" $
  lambda "t" $ cases _Type (var "t") Nothing [
    _Type_map>>: lambda "mt" $ lets [
        "kt">: Core.mapTypeKeys $ var "mt",
        "vt">: Core.mapTypeValues $ var "mt"] $
          bind "kad" (ref termAdapterDef @@ var "kt") $
          bind "vad" (ref termAdapterDef @@ var "vt") $
          Flows.pure $ Compute.adapter
            (Logic.or (Compute.adapterIsLossy $ var "kad") (Compute.adapterIsLossy $ var "vad"))
            (var "t")
            (TTypes.map (Compute.adapterTarget $ var "kad") (Compute.adapterTarget $ var "vad"))
            (ref AdaptUtils.bidirectionalDef @@ (lambdas ["dir", "term"] $ cases _Term (var "term") Nothing [
              _Term_map>>: lambda "m" $
                bind "newPairs" (Flows.mapList
                  (lambda "pair" $ lets [
                    "k">: first $ var "pair",
                    "v">: second $ var "pair"] $
                      bind "newK" (ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder $ var "kad") @@ var "k") $
                      bind "newV" (ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder $ var "vad") @@ var "v") $
                      Flows.pure $ pair (var "newK") (var "newV"))
                  (Maps.toList $ var "m")) $
                Flows.pure $ Core.termMap $ Maps.fromList $ var "newPairs"]))]

passOptionalDef :: TElement TypeAdapter
passOptionalDef = define "passOptional" $
  doc "Pass through optional types" $
  lambda "t" $ cases _Type (var "t") Nothing [
    _Type_optional>>: lambda "ot" $ lets [
      "mapTerm">: lambdas ["coder", "dir", "term"] $
        bind "opt" (ref withGraphContextDef @@ (ref ExtractCore.optionalDef @@ unaryFunction Flows.pure @@ var "term")) $
        bind "newOpt" (Flows.traverseOptional (ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ var "coder") (var "opt")) $
        Flows.pure $ Core.termOptional $ var "newOpt"] $
      bind "adapter" (ref termAdapterDef @@ var "ot") $
        Flows.pure $ Compute.adapter
          (Compute.adapterIsLossy $ var "adapter")
          (var "t")
          (Core.typeOptional $ Compute.adapterTarget $ var "adapter")
          (ref AdaptUtils.bidirectionalDef @@ (var "mapTerm" @@ (Compute.adapterCoder $ var "adapter")))]

passProductDef :: TElement TypeAdapter
passProductDef = define "passProduct" $
  doc "Pass through product types" $
  lambda "t" $ cases _Type (var "t") Nothing [
    _Type_product>>: lambda "types" $
      Flows.bind (Flows.mapList (ref termAdapterDef) (var "types")) $ lambda "ads" $ lets [
        "lossy">: Logic.ors $ Lists.map (unaryFunction Compute.adapterIsLossy) (var "ads")] $
        Flows.pure $ Compute.adapter
          (var "lossy")
          (var "t")
          (Core.typeProduct $ Lists.map (unaryFunction Compute.adapterTarget) (var "ads"))
          (ref AdaptUtils.bidirectionalDef @@ (lambdas ["dir", "term"] $ cases _Term (var "term") Nothing [
            _Term_product>>: lambda "tuple" $
              bind "newTuple" (Flows.sequence $ Lists.zipWith
                (lambdas ["term", "ad"] $ ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder $ var "ad") @@ var "term")
                (var "tuple")
                (var "ads")) $ Flows.pure $ Core.termProduct $ var "newTuple"]))]

passRecordDef :: TElement TypeAdapter
passRecordDef = define "passRecord" $
  doc "Pass through record types" $
  lambda "t" $ cases _Type (var "t") Nothing [
    _Type_record>>: lambda "rt" $
      bind "adapters" (Flows.mapList (ref fieldAdapterDef) (Core.rowTypeFields $ var "rt")) $ lets [
        "lossy">: Logic.ors $ Lists.map (unaryFunction Compute.adapterIsLossy) (var "adapters"),
        "sfields'">: Lists.map (unaryFunction Compute.adapterTarget) (var "adapters")] $
        Flows.pure $ Compute.adapter
          (var "lossy")
          (var "t")
          (Core.typeRecord $ Core.rowType (Core.rowTypeTypeName $ var "rt") (var "sfields'"))
          (ref AdaptUtils.bidirectionalDef @@ (lambdas ["dir", "term"] $ cases _Term (var "term") Nothing [
            _Term_record>>: lambda "rec" $ lets [
              "dfields">: Core.recordFields $ var "rec"] $
              bind "newFields" (Flows.sequence $ Lists.zipWith
                (lambdas ["ad", "f"] $ ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder $ var "ad") @@ var "f" )
                (var "adapters")
                (var "dfields")) $
                Flows.pure $ Core.termRecord $ Core.record (Core.rowTypeTypeName $ var "rt") (var "newFields")]))]

passSetDef :: TElement TypeAdapter
passSetDef = define "passSet" $
  doc "Pass through set types" $
  lambda "t" $ cases _Type (var "t") Nothing [
    _Type_set>>: lambda "st" $
      Flows.bind (ref termAdapterDef @@ var "st") $ lambda "ad" $
        Flows.pure $ Compute.adapter
          (Compute.adapterIsLossy $ var "ad")
          (var "t")
          (TTypes.set $ Compute.adapterTarget $ var "ad")
          (ref AdaptUtils.bidirectionalDef @@ (lambdas ["dir", "term"] $ cases _Term (var "term") Nothing [
            _Term_set>>: lambda "terms" $
              bind "newTerms" (Flows.mapList (ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder $ var "ad")) (Sets.toList $ var "terms")) $
              Flows.pure $ Core.termSet $ Sets.fromList $ var "newTerms"]))]

passSumDef :: TElement TypeAdapter
passSumDef = define "passSum" $
  doc "Pass through sum types" $
  lambda "t" $ cases _Type (var "t") Nothing [
    _Type_sum>>: lambda "types" $
      Flows.bind (Flows.mapList (ref termAdapterDef) (var "types")) $ lambda "ads" $ lets [
        "lossy">: Logic.ors $ Lists.map (unaryFunction Compute.adapterIsLossy) (var "ads")] $
        Flows.pure $ Compute.adapter
          (var "lossy")
          (var "t")
          (Core.typeSum $ Lists.map (unaryFunction Compute.adapterTarget) (var "ads"))
          (ref AdaptUtils.bidirectionalDef @@ (lambdas ["dir", "term"] $ cases _Term (var "term") Nothing [
            _Term_sum>>: lambda "s" $ lets [
                "i">: Core.sumIndex $ var "s",
                "n">: Core.sumSize $ var "s",
                "term">: Core.sumTerm $ var "s"] $
                  bind "newTerm" (ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder $ Lists.at (var "i") (var "ads")) @@ var "term") $
                    Flows.pure $ Core.termSum $ Core.sum (var "i") (var "n") (var "newTerm")]))]

passUnionDef :: TElement TypeAdapter
passUnionDef = define "passUnion" $
  doc "Pass through union types" $
  lambda "t" $ cases _Type (var "t") Nothing [
    _Type_union>>: lambda "rt" $ lets [
      "sfields">: Core.rowTypeFields $ var "rt",
      "tname">: Core.rowTypeTypeName $ var "rt",
      "getAdapter">: lambdas ["adaptersMap", "f"] $
        Optionals.maybe
          (Flows.fail $ Strings.cat2 (string "no such field: ") (unwrap _Name @@ (Core.fieldName $ var "f")))
          (unaryFunction Flows.pure)
          (Maps.lookup (Core.fieldName $ var "f") (var "adaptersMap"))] $
      bind "adapters" (Flows.mapList
          (lambda "f" $ Flows.bind (ref fieldAdapterDef @@ var "f") $ lambda "ad" $
            Flows.pure $ pair (Core.fieldTypeName $ var "f") (var "ad"))
          (var "sfields")) $ lets [
          "adaptersMap">: Maps.fromList $ var "adapters",
          "lossy">: Logic.ors $ Lists.map (lambda "pair" $ Compute.adapterIsLossy $ second $ var "pair") (var "adapters"),
          "sfields'">: Lists.map (lambda "pair" $ Compute.adapterTarget $ second $ var "pair") (var "adapters")] $
        Flows.pure $ Compute.adapter
          (var "lossy")
          (var "t")
          (Core.typeUnion $ Core.rowType (var "tname") (var "sfields'"))
          (ref AdaptUtils.bidirectionalDef @@ (lambdas ["dir", "term"] $
            bind "dfield" (ref withGraphContextDef @@ (ref ExtractCore.injectionDef @@ var "tname" @@ var "term")) $
            bind "ad" (var "getAdapter" @@ var "adaptersMap" @@ var "dfield") $
            bind "newField" (ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder $ var "ad") @@ var "dfield") $
            Flows.pure $ Core.termUnion $ Core.injection (var "tname") (var "newField")))]

passUnitDef :: TElement TypeAdapter
passUnitDef = define "passUnit" $
  doc "Pass through unit types" $
  constant $ Flows.pure $ Compute.adapter false Core.typeUnit Core.typeUnit $
    Compute.coder
      (constant $ Flows.pure Core.termUnit)
      (constant $ Flows.pure Core.termUnit)

passWrappedDef :: TElement TypeAdapter
passWrappedDef = define "passWrapped" $
  doc "Pass through wrapped types" $
  lambda "t" $ cases _Type (var "t") Nothing [
    _Type_wrap>>: lambda "wt" $ lets [
        "tname">: Core.wrappedTypeTypeName $ var "wt",
        "ot">: Core.wrappedTypeObject $ var "wt",
        "mapTerm">: lambdas ["coder", "dir", "term"] $
          bind "unwrapped" (ref withGraphContextDef @@ (ref ExtractCore.wrapDef @@ var "tname" @@ var "term")) $
          bind "newTerm" (ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ var "coder" @@ var "unwrapped") $
          Flows.pure $ Core.termWrap $ Core.wrappedTerm (var "tname") (var "newTerm")] $
        bind "adapter" (ref termAdapterDef @@ var "ot") $
          Flows.pure $ Compute.adapter
            (Compute.adapterIsLossy $ var "adapter")
            (var "t")
            (Core.typeWrap $ Core.wrappedType (var "tname") (Compute.adapterTarget $ var "adapter"))
            (ref AdaptUtils.bidirectionalDef @@ (var "mapTerm" @@ (Compute.adapterCoder $ var "adapter")))]

simplifyApplicationDef :: TElement TypeAdapter
simplifyApplicationDef = define "simplifyApplication" $
  doc "Simplify application types" $
  lambda "t" $ cases _Type (var "t") Nothing [
    _Type_application>>: lambda "at" $ lets [
        "lhs">: Core.applicationTypeFunction $ var "at"] $
        bind "ad" (ref termAdapterDef @@ var "lhs") $
          Flows.pure $ Compute.adapter
            false
            (var "t")
            (Compute.adapterTarget $ var "ad")
            (ref AdaptUtils.bidirectionalDef @@ (lambdas ["dir", "term"] $
              ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder $ var "ad") @@ var "term"))]

unitToRecordDef :: TElement TypeAdapter
unitToRecordDef = define "unitToRecord" $
    doc "Convert unit terms to records" $
    constant $ Flows.pure $
      Compute.adapter false Core.typeUnit (Core.typeRecord $ Core.rowType unitName $ list []) $
        Compute.coder
          (constant $ Flows.pure $ Core.termRecord $ Core.record unitName $ list [])
          (constant $ Flows.pure Core.termUnit)
  where
    unitName = Core.name $ string "_Unit"

unionToRecordDef :: TElement TypeAdapter
unionToRecordDef = define "unionToRecord" $
  doc "Convert union types to record types" $
  lambda "t" $ cases _Type (var "t") Nothing [
    _Type_union>>: lambda "rt" $ lets [
      "nm">: Core.rowTypeTypeName $ var "rt",
      "sfields">: Core.rowTypeFields $ var "rt",
      "target">: Core.typeRecord $ ref unionTypeToRecordTypeDef @@ var "rt",
      "toRecordField">: lambdas ["term", "fn", "f"] $ lets [
          "fn'">: Core.fieldTypeName $ var "f"] $
          Core.field (var "fn'") $ Core.termOptional $ Logic.ifElse
            (Equality.equal (var "fn'") (var "fn"))
            (just $ var "term")
            nothing,
      "fromRecordFields">: lambdas ["term", "term'", "t'", "fields"] $ lets [
        "matches">: Optionals.mapMaybe
          (lambda "field" $ lets [
              "fn">: Core.fieldName $ var "field",
              "fterm">: Core.fieldTerm $ var "field"] $
              cases _Term (var "fterm") Nothing [
                _Term_optional>>: lambda "opt" $ Optionals.bind (var "opt") $ lambda "t" $
                  just $ Core.field (var "fn") (var "t")])
          (var "fields")] $
        Logic.ifElse (Lists.null $ var "matches")
          (Flows.fail $ Strings.cat $ list [
            string "cannot convert term back to union: ",
            ref ShowCore.termDef @@ var "term",
            string " where type = ",
            ref ShowCore.typeDef @@ var "t",
            string "    and target type = ",
            ref ShowCore.typeDef @@ var "t'"])
          (Flows.pure $ Lists.head $ var "matches")] $
      bind "ad" (ref termAdapterDef @@ var "target") $
      Flows.pure $ Compute.adapter
        (Compute.adapterIsLossy $ var "ad")
        (var "t")
        (Compute.adapterTarget $ var "ad")
        (Compute.coder
          (lambda "term'" $
            bind "field" (ref withGraphContextDef @@ (ref ExtractCore.injectionDef @@ (Core.rowTypeTypeName $ var "rt") @@ var "term'")) $ lets [
              "fn">: Core.fieldName $ var "field",
              "term">: Core.fieldTerm $ var "field"] $
            Compute.coderEncode (Compute.adapterCoder $ var "ad") @@
              (Core.termRecord $ Core.record (var "nm") $ Lists.map (var "toRecordField" @@ var "term" @@ var "fn") (var "sfields")))
          (lambda "term" $
            bind "recTerm" (Compute.coderDecode (Compute.adapterCoder $ var "ad") @@ var "term") $
              cases _Term (var "recTerm") Nothing [
                _Term_record>>: lambda "rec" $ lets [
                  "fields">: Core.recordFields $ var "rec"] $
                  bind "resultField"
                    (var "fromRecordFields"
                      @@ var "term"
                      @@ (Core.termRecord $ Core.record (var "nm") (var "fields"))
                      @@ (Compute.adapterTarget $ var "ad")
                      @@ var "fields") $
                  Flows.pure $ Core.termUnion $ Core.injection (var "nm") (var "resultField")]))]

unionTypeToRecordTypeDef :: TElement (RowType -> RowType)
unionTypeToRecordTypeDef = define "unionTypeToRecordType" $
  doc "Convert a union row type to a record row type" $
  lambda "rt" $ lets [
    "makeOptional">: lambda "f" $ lets [
        "fn">: Core.fieldTypeName $ var "f",
        "ft">: Core.fieldTypeType $ var "f"] $
        Core.fieldType (var "fn") $ ref Rewriting.mapBeneathTypeAnnotationsDef @@ unaryFunction Core.typeOptional @@ var "ft"] $
    Core.rowType (Core.rowTypeTypeName $ var "rt") $ Lists.map (var "makeOptional") (Core.rowTypeFields $ var "rt")

wrapToUnwrappedDef :: TElement TypeAdapter
wrapToUnwrappedDef = define "wrapToUnwrapped" $
  doc "Convert wrapped types to unwrapped types" $
  lambda "t" $ cases _Type (var "t") Nothing [
    _Type_wrap>>: lambda "wt" $ lets [
        "tname">: Core.wrappedTypeTypeName $ var "wt",
        "typ">: Core.wrappedTypeObject $ var "wt",
        "encode">: lambda "ad" $ lambda "term" $
          bind "unwrapped" (ref withGraphContextDef @@ (ref ExtractCore.wrapDef @@ var "tname" @@ var "term")) $
          Compute.coderEncode (Compute.adapterCoder $ var "ad") @@ var "unwrapped",
        "decode">: lambda "ad" $ lambda "term" $
          bind "decoded" (Compute.coderDecode (Compute.adapterCoder $ var "ad") @@ var "term") $
          Flows.pure $ Core.termWrap $ Core.wrappedTerm (var "tname") (var "decoded")] $
        bind "ad" (ref termAdapterDef @@ var "typ") $
          Flows.pure $ Compute.adapter
            false
            (var "t")
            (Compute.adapterTarget $ var "ad")
            (Compute.coder (var "encode" @@ var "ad") (var "decode" @@ var "ad"))]

-- Note: those constructors which cannot be mapped meaningfully at this time are simply
--       preserved as strings.
termAdapterDef :: TElement TypeAdapter
termAdapterDef = define "termAdapter" $
  doc "Create an adapter for any type" $
  lambda "typ" $ lets [
    "constraints">: lambda "cx" $ Coders.languageConstraintsProjection $ Coders.adapterContextLanguage $ var "cx",
    "supported">: lambda "cx" $ ref AdaptUtils.typeIsSupportedDef @@ (var "constraints" @@ var "cx"),
    "variantIsSupported">: lambdas ["cx", "t"] $ Sets.member (ref Variants.typeVariantDef @@ var "t") $ Coders.languageConstraintsTypeVariants $ var "constraints" @@ var "cx",
    "supportedAtTopLevel">: lambdas ["cx", "t"] $ Logic.and
      (var "variantIsSupported" @@ var "cx" @@ var "t")
      (Coders.languageConstraintsTypes (var "constraints" @@ var "cx") @@ var "t"),
    "pass">: lambda "t" $ cases _TypeVariant (ref Variants.typeVariantDef @@ (ref Rewriting.deannotateTypeDef @@ var "t")) Nothing [
      _TypeVariant_application>>: constant $ list [ref passApplicationDef],
      _TypeVariant_forall>>: constant $ list [ref passForallDef],
      _TypeVariant_function>>: constant $ list [ref passFunctionDef],
      _TypeVariant_list>>: constant $ list [ref passListDef],
      _TypeVariant_literal>>: constant $ list [ref passLiteralDef],
      _TypeVariant_map>>: constant $ list [ref passMapDef],
      _TypeVariant_optional>>: constant $ list [ref passOptionalDef, ref optionalToListDef],
      _TypeVariant_product>>: constant $ list [ref passProductDef],
      _TypeVariant_record>>: constant $ list [ref passRecordDef],
      _TypeVariant_set>>: constant $ list [ref passSetDef],
      _TypeVariant_sum>>: constant $ list [ref passSumDef],
      _TypeVariant_union>>: constant $ list [ref passUnionDef],
      _TypeVariant_unit>>: constant $ list [ref passUnitDef],
      _TypeVariant_wrap>>: constant $ list [ref passWrappedDef]],
    "trySubstitution">: lambda "t" $ cases _TypeVariant (ref Variants.typeVariantDef @@ var "t") Nothing [
      _TypeVariant_application>>: constant $ list [ref simplifyApplicationDef],
      _TypeVariant_function>>: constant $ list [ref functionToUnionDef],
      _TypeVariant_forall>>: constant $ list [ref lambdaToMonotypeDef],
      _TypeVariant_optional>>: constant $ list [ref optionalToListDef],
      _TypeVariant_set>>: constant $ list [ref listToSetDef],
      _TypeVariant_union>>: constant $ list [ref unionToRecordDef],
      _TypeVariant_unit>>: constant $ list [ref unitToRecordDef],
      _TypeVariant_wrap>>: constant $ list [ref wrapToUnwrappedDef]],
    "alts">: lambdas ["cx", "t"] $
       Flows.mapList (lambda "c" $ var "c" @@ var "t") $
         Logic.ifElse (var "supportedAtTopLevel" @@ var "cx" @@ var "t")
           (var "pass" @@ var "t")
           (var "trySubstitution" @@ var "t")] $
    cases _Type (var "typ")
      (Just $ ref Monads.withTraceDef
        @@ (Strings.cat2 (string "adapter for ") (ref DescribeCore.typeDef @@ var "typ"))
        @@ (cases _Type (var "typ")
          (Just $
            bind "cx" (ref Monads.getStateDef) $
            ref AdaptUtils.chooseAdapterDef
              @@ (var "alts" @@ var "cx")
              @@ (var "supported" @@ var "cx")
              @@ ref ShowCore.typeDef
              @@ (ref DescribeCore.typeDef)
              @@ (var "typ")) [
          -- Account for let-bound variables
          _Type_variable>>: lambda "name" $ ref forTypeReferenceDef @@ var "name"])) [
      _Type_annotated>>: lambda "at" $
        bind "ad" (ref termAdapterDef @@ Core.annotatedTypeSubject (var "at")) $
        Flows.pure (Compute.adapterWithTarget (var "ad") $
          Core.typeAnnotated $ Core.annotatedType (Compute.adapterTarget $ var "ad") (Core.annotatedTypeAnnotation $ var "at"))]

withGraphContextDef :: TElement (Flow Graph a -> Flow AdapterContext a)
withGraphContextDef = define "withGraphContext" $
  doc "Execute a flow with graph context" $
  lambda "f" $
    Flows.bind (ref Monads.getStateDef) $ lambda "cx" $
      ref Monads.withStateDef @@ (Coders.adapterContextGraph $ var "cx") @@ var "f"
