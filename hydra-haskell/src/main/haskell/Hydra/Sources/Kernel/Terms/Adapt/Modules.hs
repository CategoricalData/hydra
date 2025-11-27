{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Adapt.Modules where

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

import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Kernel.Terms.Decode.Core as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas as Schemas
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore


module_ :: Module
module_ = Module (Namespace "hydra.adapt.modules") elements
    [AdaptTerms.module_, AdaptUtils.module_, Annotations.module_, DecodeCore.module_,
      Lexical.module_, Monads.module_, Rewriting.module_, Schemas.module_]
    kernelTypesModules $
    Just "Entry point for Hydra's adapter (type/term rewriting) framework"
  where
   elements = [
     el adaptTypeToLanguageAndEncodeDef,
     el adaptTypeToLanguageDef,
     el adaptedModuleDefinitionsDef,
     el constructCoderDef,
     el languageAdapterDef,
     el transformModuleDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_


adaptTypeToLanguageAndEncodeDef :: TBinding (Language -> (Type -> Flow Graph t) -> Type -> Flow Graph t)
adaptTypeToLanguageAndEncodeDef = define "adaptTypeToLanguageAndEncode" $
  doc "Given a target language, an encoding function, and a type, adapt and encode the type" $
  "lang" ~> "enc" ~> "typ" ~>
  "dflt" <~ (
    "adaptedType" <<~ ref adaptTypeToLanguageDef @@ var "lang" @@ var "typ" $
    var "enc" @@ var "adaptedType") $
  cases _Type (ref Rewriting.deannotateTypeDef @@ var "typ")
    (Just $ var "dflt") [
    _Type_variable>>: constant (var "enc" @@ var "typ")]

adaptTypeToLanguageDef :: TBinding (Language -> Type -> Flow Graph Type)
adaptTypeToLanguageDef = define "adaptTypeToLanguage" $
  doc "Given a target language and a source type, find the target type to which the latter will be adapted" $
  "lang" ~> "typ" ~>
  "adapter" <<~ ref languageAdapterDef @@ var "lang" @@ var "typ" $
  produce $ Compute.adapterTarget $ var "adapter"

constructCoderDef :: TBinding (Language -> (Term -> Flow Graph c) -> Type -> Flow Graph (Coder Graph Graph Term c))
constructCoderDef = define "constructCoder" $
  doc "Given a target language, a unidirectional last-mile encoding, and a source type, construct a unidirectional adapting coder for terms of that type" $
  "lang" ~> "encodeTerm" ~> "typ" ~>
  trace (Strings.cat2 (string "coder for ") (ref ShowCore.typeDef @@ var "typ")) $
  "adapter" <<~ ref languageAdapterDef @@ var "lang" @@ var "typ" $
  produce $ ref AdaptUtils.composeCodersDef
    @@ (Compute.adapterCoder $ var "adapter")
    @@ (ref AdaptUtils.unidirectionalCoderDef @@ var "encodeTerm")

languageAdapterDef :: TBinding (Language -> Type -> Flow Graph (SymmetricAdapter Graph Type Term))
languageAdapterDef = define "languageAdapter" $
  doc "Given a target language and a source type, produce an adapter, which rewrites the type and its terms according to the language's constraints" $
  "lang" ~> "typ" ~>
  "getPair" <~ ("typ" ~>
    "ad" <<~ ref AdaptTerms.termAdapterDef @@ var "typ" $
    "cx" <<~ ref Monads.getStateDef $
    produce $ pair (var "ad") (var "cx")) $
  "g" <<~ ref Monads.getStateDef $
  "cx0" <~ Coders.adapterContext (var "g") (var "lang") Maps.empty $
  "result" <<~ ref Monads.withStateDef @@ var "cx0" @@ (var "getPair" @@ var "typ") $
  "adapter" <~ Pairs.first (var "result") $
  "cx" <~ Pairs.second (var "result") $
  "encode" <~ ("term" ~> ref Monads.withStateDef @@ var "cx" @@
    (Compute.coderEncode (Compute.adapterCoder $ var "adapter") @@ var "term")) $
  "decode" <~ ("term" ~> ref Monads.withStateDef @@ var "cx" @@
    (Compute.coderDecode (Compute.adapterCoder $ var "adapter") @@ var "term")) $
  produce $ Compute.adapterWithCoder (var "adapter") (Compute.coder (var "encode") (var "decode"))

transformModuleDef :: TBinding (Language -> (Term -> Flow Graph e) -> (Module -> M.Map Type (Coder Graph Graph Term e) -> [(Binding, TypeApplicationTerm)] -> Flow Graph d) -> Module -> Flow Graph d)
transformModuleDef = define "transformModule" $
  doc "Given a target language, a unidirectional last mile encoding, and an intermediate helper function, transform a given module into a target representation" $
  "lang" ~> "encodeTerm" ~> "createModule" ~> "mod" ~>
  "els" <~ Module.moduleElements (var "mod") $
  "transform" <~ (
    "tterms" <<~ ref Lexical.withSchemaContextDef @@ (Flows.mapList (ref Schemas.elementAsTypeApplicationTermDef) (var "els")) $
    "types" <~ Lists.nub (Lists.map (unaryFunction Core.typeApplicationTermType) (var "tterms")) $
    "cdrs" <<~ Flows.mapList (ref constructCoderDef @@ var "lang" @@ var "encodeTerm") (var "types") $
    "coders" <~ Maps.fromList (Lists.zip (var "types") (var "cdrs")) $
    var "createModule" @@ var "mod" @@ var "coders" @@ (Lists.zip (var "els") (var "tterms"))) $
  trace (Strings.cat2 (string "transform module ") (unwrap _Namespace @@ (Module.moduleNamespace $ var "mod"))) $
  var "transform"

adaptedModuleDefinitionsDef :: TBinding (Language -> Module -> Flow Graph [Definition])
adaptedModuleDefinitionsDef = define "adaptedModuleDefinitions" $
  doc "Map a Hydra module to a list of type and/or term definitions which have been adapted to the target language" $
  "lang" ~> "mod" ~>
  "els" <~ Module.moduleElements (var "mod") $
  "adaptersFor" <~ lambda "types" (
    "adapters" <<~ Flows.mapList (ref languageAdapterDef @@ var "lang") (var "types") $
    produce $ Maps.fromList $ Lists.zip (var "types") (var "adapters")) $
  "classify" <~ ("adapters" ~> "pair" ~>
    "el" <~ Pairs.first (var "pair") $
    "tt" <~ Pairs.second (var "pair") $
    "term" <~ Core.typeApplicationTermBody (var "tt") $
    "typ" <~ Core.typeApplicationTermType (var "tt") $
    "name" <~ Core.bindingName (var "el") $
    Logic.ifElse (ref Annotations.isNativeTypeDef @@ var "el")
      ("adaptedTyp" <<~ (
        "coreTyp" <<~ (trace "adapt module definitions" $ ref DecodeCore.typeDef @@ var "term") $
        ref adaptTypeToLanguageDef @@ var "lang" @@ var "coreTyp") $
       produce $ Module.definitionType $ Module.typeDefinition (var "name") (var "adaptedTyp"))
      (Maybes.maybe
        (Flows.fail $ Strings.cat2 (string "no adapter for element ") (unwrap _Name @@ var "name"))
        (lambda "adapter" (
          "adapted" <<~ Compute.coderEncode (Compute.adapterCoder $ var "adapter") @@ var "term" $
          produce $ Module.definitionTerm $ Module.termDefinition (var "name") (var "adapted") (Compute.adapterTarget $ var "adapter")))
        (Maps.lookup (var "typ") (var "adapters")))) $
  "tterms" <<~ ref Lexical.withSchemaContextDef @@ (Flows.mapList (ref Schemas.elementAsTypeApplicationTermDef) (var "els")) $
  "types" <~ Sets.toList (Sets.fromList (Lists.map (ref Rewriting.deannotateTypeDef <.> unaryFunction Core.typeApplicationTermType) (var "tterms"))) $
  "adapters" <<~ var "adaptersFor" @@ var "types" $
  Flows.mapList (var "classify" @@ var "adapters") (Lists.zip (var "els") (var "tterms"))
