
module Hydra.Sources.Kernel.Terms.Adapt.Modules where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  adaptTypeToLanguageAndEncode, adaptTypeToLanguage, adaptedModuleDefinitions, constructCoder,
  languageAdapter, transformModule)
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
import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Meta.Error        as Error
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Decode.Core as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical

import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas as Schemas
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Error as ShowError


ns :: Namespace
ns = Namespace "hydra.adapt.modules"

module_ :: Module
module_ = Module ns elements
    [AdaptTerms.ns, AdaptUtils.ns, Annotations.ns, moduleNamespace DecodeCore.module_,
      Lexical.ns, Rewriting.ns, Schemas.ns, ShowError.ns]
    kernelTypesNamespaces $
    Just "Entry point for Hydra's adapter (type/term rewriting) framework"
  where
   elements = [
     toBinding adaptTypeToLanguageAndEncode,
     toBinding adaptTypeToLanguage,
     toBinding adaptedModuleDefinitions,
     toBinding constructCoder,
     toBinding languageAdapter,
     toBinding transformModule]

formatError :: TTerm (InContext Error -> String)
formatError = "ic" ~> ShowError.error_ @@ Ctx.inContextObject (var "ic")

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_


adaptTypeToLanguageAndEncode :: TBinding (Language -> (Type -> Either String t) -> Context -> Graph -> Type -> Either String t)
adaptTypeToLanguageAndEncode = define "adaptTypeToLanguageAndEncode" $
  doc "Given a target language, an encoding function, and a type, adapt and encode the type" $
  "lang" ~> "enc" ~> "cx" ~> "g" ~> "typ" ~>
  "dflt" <~ (
    "adaptedType" <<~ adaptTypeToLanguage @@ var "lang" @@ var "cx" @@ var "g" @@ var "typ" $
    var "enc" @@ var "adaptedType") $
  cases _Type (Rewriting.deannotateType @@ var "typ")
    (Just $ var "dflt") [
    _Type_variable>>: constant (var "enc" @@ var "typ")]

adaptTypeToLanguage :: TBinding (Language -> Context -> Graph -> Type -> Either String Type)
adaptTypeToLanguage = define "adaptTypeToLanguage" $
  doc "Given a target language and a source type, find the target type to which the latter will be adapted" $
  "lang" ~> "cx" ~> "g" ~> "typ" ~>
  Eithers.map (unaryFunction Compute.adapterTarget) (languageAdapter @@ var "lang" @@ var "cx" @@ var "g" @@ var "typ")

constructCoder :: TBinding (Language -> (Context -> Term -> Either (InContext Error) c) -> Context -> Graph -> Type -> Either String (Coder Term c))
constructCoder = define "constructCoder" $
  doc "Given a target language, a unidirectional last-mile encoding, and a source type, construct a unidirectional adapting coder for terms of that type" $
  "lang" ~> "encodeTerm" ~> "cx" ~> "g" ~> "typ" ~>
  Eithers.map
    ("adapter" ~> AdaptUtils.composeCoders
      @@ (Compute.adapterCoder $ var "adapter")
      @@ (AdaptUtils.unidirectionalCoder @@ var "encodeTerm"))
    (languageAdapter @@ var "lang" @@ var "cx" @@ var "g" @@ var "typ")

languageAdapter :: TBinding (Language -> Context -> Graph -> Type -> Either String (SymmetricAdapter Type Term))
languageAdapter = define "languageAdapter" $
  doc "Given a target language and a source type, produce an adapter, which rewrites the type and its terms according to the language's constraints" $
  "lang" ~> "_cx" ~> "g" ~> "typ" ~>
  "cx0" <~ Coders.adapterContext (var "g") (var "lang") Maps.empty $
  AdaptTerms.termAdapter @@ var "cx0" @@ var "typ"

transformModule :: TBinding (Language -> (Context -> Term -> Either (InContext Error) e) -> (Module -> M.Map Type (Coder Term e) -> [(Binding, TypeApplicationTerm)] -> Either String d) -> Context -> Graph -> Module -> Either String d)
transformModule = define "transformModule" $
  doc "Given a target language, a unidirectional last mile encoding, and an intermediate helper function, transform a given module into a target representation" $
  "lang" ~> "encodeTerm" ~> "createModule" ~> "cx" ~> "g" ~> "mod" ~>
  "els" <~ Module.moduleElements (var "mod") $
  "tterms" <<~ Eithers.mapList ("_el" ~> Eithers.bimap ("ic" ~> ShowError.error_ @@ Ctx.inContextObject (var "ic")) ("x" ~> var "x") (Schemas.elementAsTypeApplicationTerm @@ var "cx" @@ var "_el")) (var "els") $
  "types" <~ Lists.nub (Lists.map (unaryFunction Core.typeApplicationTermType) (var "tterms")) $
  "cdrs" <<~ Eithers.mapList (constructCoder @@ var "lang" @@ var "encodeTerm" @@ var "cx" @@ var "g") (var "types") $
  "coders" <~ Maps.fromList (Lists.zip (var "types") (var "cdrs")) $
  var "createModule" @@ var "mod" @@ var "coders" @@ (Lists.zip (var "els") (var "tterms"))

adaptedModuleDefinitions :: TBinding (Language -> Context -> Graph -> Module -> Either String [Definition])
adaptedModuleDefinitions = define "adaptedModuleDefinitions" $
  doc "Map a Hydra module to a list of type and/or term definitions which have been adapted to the target language" $
  "lang" ~> "cx" ~> "graph" ~> "mod" ~>
  "els" <~ Module.moduleElements (var "mod") $
  "adaptersFor" <~ lambda "types" (
    Eithers.map ("adapters" ~> Maps.fromList (Lists.zip (var "types") (var "adapters")))
      (Eithers.mapList (languageAdapter @@ var "lang" @@ var "cx" @@ var "graph") (var "types"))) $
  "classify" <~ ("adapters" ~> "pair" ~>
    "el" <~ Pairs.first (var "pair") $
    "tt" <~ Pairs.second (var "pair") $
    "term" <~ Core.typeApplicationTermBody (var "tt") $
    "typ" <~ Core.typeApplicationTermType (var "tt") $
    "name" <~ Core.bindingName (var "el") $
    Logic.ifElse (Annotations.isNativeType @@ var "el")
      ("coreTyp" <<~ Eithers.bimap ("e" ~> Error.unDecodingError @@ var "e") ("x" ~> var "x") (decoderFor _Type @@ var "graph" @@ var "term") $
       "adaptedTyp" <<~ adaptTypeToLanguage @@ var "lang" @@ var "cx" @@ var "graph" @@ var "coreTyp" $
       right $ Module.definitionType $ Module.typeDefinition (var "name") (var "adaptedTyp"))
      (Maybes.maybe
        (left $ Strings.cat2 (string "no adapter for element ") (unwrap _Name @@ var "name"))
        (lambda "adapter" (
          "adapted" <<~ Eithers.bimap ("ic" ~> ShowError.error_ @@ Ctx.inContextObject (var "ic")) ("x" ~> var "x") (Compute.coderEncode (Compute.adapterCoder $ var "adapter") @@ var "cx" @@ var "term") $
          right $ Module.definitionTerm $ Module.termDefinition (var "name") (var "adapted") (Schemas.typeToTypeScheme @@ (Compute.adapterTarget $ var "adapter"))))
        (Maps.lookup (var "typ") (var "adapters")))) $
  "tterms" <<~ Eithers.mapList ("_el" ~> Eithers.bimap ("ic" ~> ShowError.error_ @@ Ctx.inContextObject (var "ic")) ("x" ~> var "x") (Schemas.elementAsTypeApplicationTerm @@ var "cx" @@ var "_el")) (var "els") $
  "types" <~ Sets.toList (Sets.fromList (Lists.map (Rewriting.deannotateType <.> unaryFunction Core.typeApplicationTermType) (var "tterms"))) $
  "adapters" <<~ var "adaptersFor" @@ var "types" $
  Eithers.mapList (var "classify" @@ var "adapters") (Lists.zip (var "els") (var "tterms"))
