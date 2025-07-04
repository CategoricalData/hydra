{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Adapt.Modules where

-- Standard Tier-2 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors              as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Coders                 as Coders
import qualified Hydra.Dsl.Compute                as Compute
import qualified Hydra.Dsl.Core                   as Core
import qualified Hydra.Dsl.Graph                  as Graph
import qualified Hydra.Dsl.Lib.Chars              as Chars
import qualified Hydra.Dsl.Lib.Equality           as Equality
import qualified Hydra.Dsl.Lib.Flows              as Flows
import qualified Hydra.Dsl.Lib.Lists              as Lists
import qualified Hydra.Dsl.Lib.Literals           as Literals
import qualified Hydra.Dsl.Lib.Logic              as Logic
import qualified Hydra.Dsl.Lib.Maps               as Maps
import qualified Hydra.Dsl.Lib.Math               as Math
import qualified Hydra.Dsl.Lib.Optionals          as Optionals
import           Hydra.Dsl.Phantoms               as Phantoms
import qualified Hydra.Dsl.Lib.Sets               as Sets
import           Hydra.Dsl.Lib.Strings            as Strings
import qualified Hydra.Dsl.Mantle                 as Mantle
import qualified Hydra.Dsl.Module                 as Module
import qualified Hydra.Dsl.TTerms                 as TTerms
import qualified Hydra.Dsl.TTypes                 as TTypes
import qualified Hydra.Dsl.Terms                  as Terms
import qualified Hydra.Dsl.Topology               as Topology
import qualified Hydra.Dsl.Types                  as Types
import qualified Hydra.Dsl.Typing                 as Typing
import qualified Hydra.Sources.Tier1.All          as Tier1
import qualified Hydra.Sources.Tier1.Constants    as Constants
import qualified Hydra.Sources.Tier1.Encode.Core as EncodeCore
import qualified Hydra.Sources.Tier1.Decode       as Decode
import qualified Hydra.Sources.Tier1.Formatting   as Formatting
import qualified Hydra.Sources.Tier1.Literals     as Literals
import qualified Hydra.Sources.Tier1.Strip        as Strip
import           Prelude hiding ((++))
import qualified Data.Int                  as I
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

-- Uncomment tier-2 sources as needed
import qualified Hydra.Sources.Tier2.Adapt.Utils as AdaptUtils
--import qualified Hydra.Sources.Tier2.Adapt.Modules as AdaptModules
import qualified Hydra.Sources.Tier2.Annotations as Annotations
--import qualified Hydra.Sources.Tier2.Arity as Arity
import qualified Hydra.Sources.Tier2.Decode.Core as DecodeCore
--import qualified Hydra.Sources.Tier2.Languages as Languages
--import qualified Hydra.Sources.Tier2.Errors as Errors
--import qualified Hydra.Sources.Tier2.Extract.Core as ExtractCore
import qualified Hydra.Sources.Tier2.Monads as Monads
--import qualified Hydra.Sources.Tier2.Grammars as Grammars
--import qualified Hydra.Sources.Tier2.Inference as Inference
import qualified Hydra.Sources.Tier2.Lexical as Lexical
--import qualified Hydra.Sources.Tier2.Adapt.Literals as AdaptLiterals
import qualified Hydra.Sources.Tier2.Describe.Core as DescribeCore
--import qualified Hydra.Sources.Tier2.Qnames as Qnames
--import qualified Hydra.Sources.Tier2.Reduction as Reduction
--import qualified Hydra.Sources.Tier2.Rewriting as Rewriting
import qualified Hydra.Sources.Tier2.Schemas as Schemas
--import qualified Hydra.Sources.Tier2.Serialization as Serialization
--import qualified Hydra.Sources.Tier2.Show.Accessors as ShowAccessors
--import qualified Hydra.Sources.Tier2.Show.Core as ShowCore
--import qualified Hydra.Sources.Tier2.Sorting as Sorting
--import qualified Hydra.Sources.Tier2.Substitution as Substitution
--import qualified Hydra.Sources.Tier2.Tarjan as Tarjan
--import qualified Hydra.Sources.Tier2.Templating as Templating
import qualified Hydra.Sources.Tier2.Adapt.Terms as AdaptTerms
--import qualified Hydra.Sources.Tier2.Unification as Unification
--import qualified Hydra.Sources.Tier2.Variants as Variants


adaptModulesModule :: Module
adaptModulesModule = Module (Namespace "hydra.adapt.modules") elements
    [Annotations.hydraAnnotationsModule, AdaptTerms.adaptTermsModule]
    [Tier1.hydraCodersModule, Tier1.hydraComputeModule, Tier1.hydraModuleModule, Tier1.hydraTopologyModule] $
    Just "Entry point for Hydra's adapter (type/term rewriting) framework"
  where
   elements = [
     el adaptAndEncodeTypeDef,
     el adaptTypeDef,
     el adaptedModuleDefinitionsDef,
     el constructCoderDef,
     el languageAdapterDef,
     el transformModuleDef]

adaptModulesDefinition :: String -> TTerm a -> TElement a
adaptModulesDefinition = definitionInModule adaptModulesModule

adaptAndEncodeTypeDef :: TElement (Language -> (Type -> Flow Graph t) -> Type -> Flow Graph t)
adaptAndEncodeTypeDef = adaptModulesDefinition "adaptAndEncodeType" $
  doc "Given a target language, an encoding function, and a type, adapt and encode the type" $
  lambdas ["lang", "enc", "typ"] $
    cases _Type (ref Strip.stripTypeDef @@ var "typ")
      (Just $ withVar "adaptedType" (ref adaptTypeDef @@ var "lang" @@ var "typ") $
        var "enc" @@ var "adaptedType") [
      _Type_variable>>: constant (var "enc" @@ var "typ")]

adaptTypeDef :: TElement (Language -> Type -> Flow Graph Type)
adaptTypeDef = adaptModulesDefinition "adaptType" $
  doc "Given a target language and a source type, find the target type to which the latter will be adapted" $
  lambdas ["lang", "typ"] $
    withVar "adapter" (ref languageAdapterDef @@ var "lang" @@ var "typ") $
      Flows.pure $ Compute.adapterTarget $ var "adapter"

constructCoderDef :: TElement (Language -> (Term -> Flow Graph c) -> Type -> Flow Graph (Coder Graph Graph Term c))
constructCoderDef = adaptModulesDefinition "constructCoder" $
  doc "Given a target language, a unidirectional last-mile encoding, and a source type, construct a unidirectional adapting coder for terms of that type" $
  lambdas ["lang", "encodeTerm", "typ"] $
    ref Monads.withTraceDef
      @@ (Strings.cat2 (string "coder for ") (ref DescribeCore.typeDef @@ var "typ"))
      @@ (withVar "adapter" (ref languageAdapterDef @@ var "lang" @@ var "typ") $
          Flows.pure $ ref AdaptUtils.composeCodersDef
            @@ (Compute.adapterCoder $ var "adapter")
            @@ (ref AdaptUtils.unidirectionalCoderDef @@ var "encodeTerm"))

languageAdapterDef :: TElement (Language -> Type -> Flow Graph (SymmetricAdapter Graph Type Term))
languageAdapterDef = adaptModulesDefinition "languageAdapter" $
  doc "Given a target language and a source type, produce an adapter, which rewrites the type and its terms according to the language's constraints" $
  lambdas ["lang", "typ"] $
    withVar "g" (ref Monads.getStateDef) $ lets [
      "cx0">: Coders.adapterContext (var "g") (var "lang") Maps.empty] $
    withVar "result" (ref Monads.withStateDef @@ var "cx0" @@
      (withVar "ad" (ref AdaptTerms.termAdapterDef @@ var "typ") $
       withVar "cx" (ref Monads.getStateDef) $
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
transformModuleDef = adaptModulesDefinition "transformModule" $
  doc "Given a target language, a unidirectional last mile encoding, and an intermediate helper function, transform a given module into a target representation" $
  lambdas ["lang", "encodeTerm", "createModule", "mod"] $
    ref Monads.withTraceDef
      @@ (Strings.cat2 (string "transform module ") (unwrap _Namespace @@ (Module.moduleNamespace $ var "mod")))
      @@ (lets [
        "els">: Module.moduleElements $ var "mod",
        "codersFor">: lambda "types" $
          withVar "cdrs" (Flows.mapList (ref constructCoderDef @@ var "lang" @@ var "encodeTerm") (var "types")) $
          Flows.pure $ Maps.fromList $ Lists.zip (var "types") (var "cdrs")] $
      withVar "tterms" (ref Lexical.withSchemaContextDef @@ (Flows.mapList (ref Schemas.elementAsTypedTermDef) (var "els"))) $
      lets ["types">: Lists.nub $ Lists.map (unaryFunction Core.typedTermType) (var "tterms")] $
      withVar "coders" (var "codersFor" @@ var "types") $
      var "createModule" @@ var "mod" @@ var "coders" @@ (Lists.zip (var "els") (var "tterms")))

adaptedModuleDefinitionsDef :: TElement (Language -> Module -> Flow Graph [Definition])
adaptedModuleDefinitionsDef = adaptModulesDefinition "adaptedModuleDefinitions" $
  doc "Map a Hydra module to a list of type and/or term definitions which have been adapted to the target language" $
  lambdas ["lang", "mod"] $ lets [
    "els">: Module.moduleElements $ var "mod",
    "adaptersFor">: lambda "types" $
      withVar "adapters" (Flows.mapList (ref languageAdapterDef @@ var "lang") (var "types")) $
      Flows.pure $ Maps.fromList $ Lists.zip (var "types") (var "adapters"),
    "classify">: lambdas ["adapters", "pair"] $ lets [
      "el">: first $ var "pair",
      "tt">: second $ var "pair",
      "term">: Core.typedTermTerm $ var "tt",
      "typ">: Core.typedTermType $ var "tt",
      "name">: Graph.elementName $ var "el"] $
      Logic.ifElse (ref Annotations.isNativeTypeDef @@ var "el")
        (withVar "adaptedTyp" (withVar "coreTyp" (ref DecodeCore.typeDef @@ var "term") $ ref adaptTypeDef @@ var "lang" @@ var "coreTyp") $
         Flows.pure $ Module.definitionType $ Module.typeDefinition (var "name") (var "adaptedTyp"))
        (Optionals.maybe
          (Flows.fail $ Strings.cat2 (string "no adapter for element ") (unwrap _Name @@ var "name"))
          (lambda "adapter" $
            withVar "adapted" (Compute.coderEncode (Compute.adapterCoder $ var "adapter") @@ var "term") $
            Flows.pure $ Module.definitionTerm $ Module.termDefinition (var "name") (var "adapted") (Compute.adapterTarget $ var "adapter"))
          (Maps.lookup (var "typ") (var "adapters")))] $
  withVar "tterms" (ref Lexical.withSchemaContextDef @@ (Flows.mapList (ref Schemas.elementAsTypedTermDef) (var "els"))) $
  lets ["types">: Sets.toList $ Sets.fromList $ Lists.map (ref Strip.stripTypeDef <.> unaryFunction Core.typedTermType) (var "tterms")] $
  withVar "adapters" (var "adaptersFor" @@ var "types") $
  Flows.mapList (var "classify" @@ var "adapters") $ Lists.zip (var "els") (var "tterms")
