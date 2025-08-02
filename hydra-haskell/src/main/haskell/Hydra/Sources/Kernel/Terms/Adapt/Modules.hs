{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Adapt.Modules where

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

import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Kernel.Terms.Decode.Core as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Describe.Core as DescribeCore
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas as Schemas


module_ :: Module
module_ = Module (Namespace "hydra.adapt.modules") elements
    [AdaptTerms.module_, AdaptUtils.module_, Annotations.module_, DecodeCore.module_, DescribeCore.module_,
      Lexical.module_, Monads.module_, Rewriting.module_, Schemas.module_]
    kernelTypesModules $
    Just "Entry point for Hydra's adapter (type/term rewriting) framework"
  where
   elements = [
     el adaptAndEncodeTypeDef,
     el adaptTypeDef,
     el adaptedModuleDefinitionsDef,
     el constructCoderDef,
     el languageAdapterDef,
     el transformModuleDef]

define :: String -> TTerm a -> TElement a
define = definitionInModule module_

adaptAndEncodeTypeDef :: TElement (Language -> (Type -> Flow Graph t) -> Type -> Flow Graph t)
adaptAndEncodeTypeDef = define "adaptAndEncodeType" $
  doc "Given a target language, an encoding function, and a type, adapt and encode the type" $
  lambdas ["lang", "enc", "typ"] $
    cases _Type (ref Rewriting.deannotateTypeDef @@ var "typ")
      (Just $ bind "adaptedType" (ref adaptTypeDef @@ var "lang" @@ var "typ") $
        var "enc" @@ var "adaptedType") [
      _Type_variable>>: constant (var "enc" @@ var "typ")]

adaptTypeDef :: TElement (Language -> Type -> Flow Graph Type)
adaptTypeDef = define "adaptType" $
  doc "Given a target language and a source type, find the target type to which the latter will be adapted" $
  lambdas ["lang", "typ"] $
    bind "adapter" (ref languageAdapterDef @@ var "lang" @@ var "typ") $
      Flows.pure $ Compute.adapterTarget $ var "adapter"

constructCoderDef :: TElement (Language -> (Term -> Flow Graph c) -> Type -> Flow Graph (Coder Graph Graph Term c))
constructCoderDef = define "constructCoder" $
  doc "Given a target language, a unidirectional last-mile encoding, and a source type, construct a unidirectional adapting coder for terms of that type" $
  lambdas ["lang", "encodeTerm", "typ"] $
  trace (Strings.cat2 (string "coder for ") (ref DescribeCore.typeDef @@ var "typ")) $
  bind "adapter" (ref languageAdapterDef @@ var "lang" @@ var "typ") $
  Flows.pure $ ref AdaptUtils.composeCodersDef
    @@ (Compute.adapterCoder $ var "adapter")
    @@ (ref AdaptUtils.unidirectionalCoderDef @@ var "encodeTerm")

languageAdapterDef :: TElement (Language -> Type -> Flow Graph (SymmetricAdapter Graph Type Term))
languageAdapterDef = define "languageAdapter" $
  doc "Given a target language and a source type, produce an adapter, which rewrites the type and its terms according to the language's constraints" $
  lambdas ["lang", "typ"] $
    bind "g" (ref Monads.getStateDef) $ lets [
      "cx0">: Coders.adapterContext (var "g") (var "lang") Maps.empty] $
    bind "result" (ref Monads.withStateDef @@ var "cx0" @@
      (bind "ad" (ref AdaptTerms.termAdapterDef @@ var "typ") $
       bind "cx" (ref Monads.getStateDef) $
       Flows.pure $ pair (var "ad") (var "cx"))) $ lets [
      "adapter">: first $ var "result",
      "cx">: second $ var "result",
      "encode">: lambda "term" $ ref Monads.withStateDef @@ var "cx" @@
        (Compute.coderEncode (Compute.adapterCoder $ var "adapter") @@ var "term"),
      "decode">: lambda "term" $ ref Monads.withStateDef @@ var "cx" @@
        (Compute.coderDecode (Compute.adapterCoder $ var "adapter") @@ var "term"),
      "ac">: Compute.coder (var "encode") (var "decode")] $
    Flows.pure $ Compute.adapterWithCoder (var "adapter") (var "ac")

transformModuleDef :: TElement (Language -> (Term -> Flow Graph e) -> (Module -> M.Map Type (Coder Graph Graph Term e) -> [(Element, TypedTerm)] -> Flow Graph d) -> Module -> Flow Graph d)
transformModuleDef = define "transformModule" $
  doc "Given a target language, a unidirectional last mile encoding, and an intermediate helper function, transform a given module into a target representation" $
  lambdas ["lang", "encodeTerm", "createModule", "mod"] $
  trace (Strings.cat2 (string "transform module ") (unwrap _Namespace @@ (Module.moduleNamespace $ var "mod"))) $ lets [
  "els">: Module.moduleElements $ var "mod",
  "codersFor">: lambda "types" $
    bind "cdrs" (Flows.mapList (ref constructCoderDef @@ var "lang" @@ var "encodeTerm") (var "types")) $
    Flows.pure $ Maps.fromList $ Lists.zip (var "types") (var "cdrs")] $
  bind "tterms" (ref Lexical.withSchemaContextDef @@ (Flows.mapList (ref Schemas.elementAsTypedTermDef) (var "els"))) $ lets [
  "types">: Lists.nub $ Lists.map (unaryFunction Core.typedTermType) (var "tterms")] $
  bind "coders" (var "codersFor" @@ var "types") $
  var "createModule" @@ var "mod" @@ var "coders" @@ (Lists.zip (var "els") (var "tterms"))

adaptedModuleDefinitionsDef :: TElement (Language -> Module -> Flow Graph [Definition])
adaptedModuleDefinitionsDef = define "adaptedModuleDefinitions" $
  doc "Map a Hydra module to a list of type and/or term definitions which have been adapted to the target language" $
  lambdas ["lang", "mod"] $ lets [
    "els">: Module.moduleElements $ var "mod",
    "adaptersFor">: lambda "types" $
      bind "adapters" (Flows.mapList (ref languageAdapterDef @@ var "lang") (var "types")) $
      Flows.pure $ Maps.fromList $ Lists.zip (var "types") (var "adapters"),
    "classify">: lambdas ["adapters", "pair"] $ lets [
      "el">: first $ var "pair",
      "tt">: second $ var "pair",
      "term">: Core.typedTermTerm $ var "tt",
      "typ">: Core.typedTermType $ var "tt",
      "name">: Graph.elementName $ var "el"] $
      Logic.ifElse (ref Annotations.isNativeTypeDef @@ var "el")
        (bind "adaptedTyp" (bind "coreTyp" (ref DecodeCore.typeDef @@ var "term") $ ref adaptTypeDef @@ var "lang" @@ var "coreTyp") $
         Flows.pure $ Module.definitionType $ Module.typeDefinition (var "name") (var "adaptedTyp"))
        (Optionals.maybe
          (Flows.fail $ Strings.cat2 (string "no adapter for element ") (unwrap _Name @@ var "name"))
          (lambda "adapter" $
            bind "adapted" (Compute.coderEncode (Compute.adapterCoder $ var "adapter") @@ var "term") $
            Flows.pure $ Module.definitionTerm $ Module.termDefinition (var "name") (var "adapted") (Compute.adapterTarget $ var "adapter"))
          (Maps.lookup (var "typ") (var "adapters")))] $
  bind "tterms" (ref Lexical.withSchemaContextDef @@ (Flows.mapList (ref Schemas.elementAsTypedTermDef) (var "els"))) $
  lets ["types">: Sets.toList $ Sets.fromList $ Lists.map (ref Rewriting.deannotateTypeDef <.> unaryFunction Core.typedTermType) (var "tterms")] $
  bind "adapters" (var "adaptersFor" @@ var "types") $
  Flows.mapList (var "classify" @@ var "adapters") $ Lists.zip (var "els") (var "tterms")
