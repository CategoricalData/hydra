
module Hydra.Sources.Kernel.Terms.Analysis where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  addNamesToNamespaces,
  definitionDependencyNamespaces,
  dependencyNamespaces,
  moduleContainsBinaryLiterals,
  moduleDependencyNamespaces,
  namespacesForDefinitions)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
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
import qualified Hydra.Dsl.Module       as Module
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Errors       as Error
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Annotations  as Annotations
import qualified Hydra.Sources.Kernel.Terms.Dependencies  as Dependencies
import qualified Hydra.Sources.Decode.Core  as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Lexical      as Lexical
import qualified Hydra.Sources.Kernel.Terms.Names        as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting    as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Strip        as Strip

import qualified Hydra.Sources.Kernel.Terms.Predicates   as Predicates


ns :: Namespace
ns = Namespace "hydra.analysis"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [Annotations.ns, Dependencies.ns, moduleNamespace DecodeCore.module_, Lexical.ns, Names.ns, Predicates.ns, Rewriting.ns, Strip.ns]
    kernelTypesNamespaces $
    Just ("Module dependency namespace analysis")
  where
    elements = [
      toDefinition addNamesToNamespaces,
      toDefinition definitionDependencyNamespaces,
      toDefinition dependencyNamespaces,
      toDefinition moduleContainsBinaryLiterals,
      toDefinition moduleDependencyNamespaces,
      toDefinition namespacesForDefinitions]

addNamesToNamespaces :: TTermDefinition ((Namespace -> a) -> S.Set Name -> Namespaces a -> Namespaces a)
addNamesToNamespaces = define "addNamesToNamespaces" $
  doc "Add names to existing namespaces mapping" $
  "encodeNamespace" ~> "names" ~> "ns0" ~>
--  "nss" <~ Sets.empty $
  "nss" <~ Sets.fromList (Maybes.cat $ Lists.map (Names.namespaceOf) $ Sets.toList $ var "names") $
  "toPair" <~ ("ns" ~> pair (var "ns") (var "encodeNamespace" @@ var "ns")) $
  Module.namespacesWithMapping (var "ns0") $ Maps.union
    (Module.namespacesMapping $ var "ns0")
    (Maps.fromList $ Lists.map (var "toPair") $ Sets.toList $ var "nss")

definitionDependencyNamespaces :: TTermDefinition ([Definition] -> S.Set Namespace)
definitionDependencyNamespaces = define "definitionDependencyNamespaces" $
  doc "Get dependency namespaces from definitions" $
  "defs" ~>
  "defNames" <~ ("def" ~> cases _Definition (var "def")
    Nothing [
    _Definition_type>>: "typeDef" ~>
      Dependencies.typeDependencyNames @@ true @@ Module.typeDefinitionType (var "typeDef"),
    _Definition_term>>: "termDef" ~>
      Dependencies.termDependencyNames @@ true @@ true @@ true @@ Module.termDefinitionTerm (var "termDef")]) $
  "allNames" <~ Sets.unions (Lists.map (var "defNames") (var "defs")) $
  Sets.fromList (Maybes.cat (Lists.map (Names.namespaceOf) (Sets.toList (var "allNames"))))

dependencyNamespaces :: TTermDefinition (Context -> Graph -> Bool -> Bool -> Bool -> Bool -> [Binding] -> Either (InContext Error) (S.Set Namespace))
dependencyNamespaces = define "dependencyNamespaces" $
  doc "Find dependency namespaces in all of a set of terms (Either version)" $
  "cx" ~> "graph" ~> "binds" ~> "withPrims" ~> "withNoms" ~> "withSchema" ~> "els" ~>
  "depNames" <~ ("el" ~>
    "term" <~ Core.bindingTerm (var "el") $
    "deannotatedTerm" <~ Strip.deannotateTerm @@ var "term" $
    "dataNames" <~ Dependencies.termDependencyNames @@ var "binds" @@ var "withPrims" @@ var "withNoms" @@ var "term" $
    "schemaNames" <~ Logic.ifElse (var "withSchema")
      (Maybes.maybe Sets.empty
        ("ts" ~> Dependencies.typeDependencyNames @@ true @@ Core.typeSchemeType (var "ts"))
        (Core.bindingType (var "el")))
      Sets.empty $
    -- Handle encoded types: decode as Type and extract type dependency names
    Logic.ifElse (Predicates.isEncodedType @@ var "deannotatedTerm")
      (Eithers.map ("typ" ~> Sets.unions (list [
          var "dataNames", var "schemaNames",
          Dependencies.typeDependencyNames @@ true @@ var "typ"]))
        (Ctx.withContext (Ctx.pushTrace (string "dependency namespace (type)") (var "cx"))
          (Eithers.bimap ("_e" ~> Error.errorOther $ Error.otherError (unwrap _DecodingError @@ var "_e")) ("_a" ~> var "_a")
            (decoderFor _Type @@ var "graph" @@ var "term"))))
      -- Handle encoded terms: decode as Term and extract term dependency names
      (Logic.ifElse (Predicates.isEncodedTerm @@ var "deannotatedTerm")
        (Eithers.map ("decodedTerm" ~> Sets.unions (list [
            var "dataNames", var "schemaNames",
            Dependencies.termDependencyNames @@ var "binds" @@ var "withPrims" @@ var "withNoms" @@ var "decodedTerm"]))
          (Ctx.withContext (Ctx.pushTrace (string "dependency namespace (term)") (var "cx"))
            (Eithers.bimap ("_e" ~> Error.errorOther $ Error.otherError (unwrap _DecodingError @@ var "_e")) ("_a" ~> var "_a")
              (decoderFor _Term @@ var "graph" @@ var "term"))))
        (right (Sets.unions (list [var "dataNames", var "schemaNames"]))))) $
  Eithers.map ("namesList" ~> Sets.fromList (Maybes.cat (Lists.map (Names.namespaceOf) (
      Sets.toList (Sets.unions (var "namesList"))))))
    (Eithers.mapList (var "depNames") (var "els"))

moduleContainsBinaryLiterals :: TTermDefinition (Module -> Bool)
moduleContainsBinaryLiterals = define "moduleContainsBinaryLiterals" $
  doc "Check whether a module contains any binary literal values" $
  "mod" ~>
  "checkTerm" <~ ("found" ~> "term" ~> Logic.or (var "found") $
    cases _Term (var "term") (Just false) [
      _Term_literal>>: "lit" ~>
        cases _Literal (var "lit") (Just false) [
          _Literal_binary>>: constant true]]) $
  "termContainsBinary" <~ ("term" ~>
    Rewriting.foldOverTerm @@ Coders.traversalOrderPre @@ var "checkTerm" @@ false @@ var "term") $
  "defTerms" <~ Maybes.cat (Lists.map
    ("d" ~> cases _Definition (var "d") (Just nothing) [
      _Definition_term>>: "td" ~> just (Module.termDefinitionTerm $ var "td")])
    (Module.moduleDefinitions (var "mod"))) $
  Lists.foldl
    ("acc" ~> "t" ~> Logic.or (var "acc") (var "termContainsBinary" @@ var "t"))
    false
    (var "defTerms")

moduleDependencyNamespaces :: TTermDefinition (Context -> Graph -> Bool -> Bool -> Bool -> Bool -> Module -> Either (InContext Error) (S.Set Namespace))
moduleDependencyNamespaces = define "moduleDependencyNamespaces" $
  doc "Find dependency namespaces in all elements of a module, excluding the module's own namespace (Either version)" $
  "cx" ~> "graph" ~> "binds" ~> "withPrims" ~> "withNoms" ~> "withSchema" ~> "mod" ~>
  "allBindings" <~ Maybes.cat (Lists.map
    ("d" ~> cases _Definition (var "d") (Just nothing) [
      _Definition_type>>: "td" ~>
        just (Annotations.typeBinding @@ (Module.typeDefinitionName $ var "td") @@ (Module.typeDefinitionType $ var "td")),
      _Definition_term>>: "td" ~>
        just (Core.binding (Module.termDefinitionName $ var "td") (Module.termDefinitionTerm $ var "td")
          (Module.termDefinitionType $ var "td"))])
    (Module.moduleDefinitions (var "mod"))) $
  Eithers.map
    ("deps" ~> Sets.delete (Module.moduleNamespace (var "mod")) (var "deps"))
    (dependencyNamespaces @@ var "cx" @@ var "graph" @@ var "binds" @@ var "withPrims" @@ var "withNoms" @@ var "withSchema" @@
      (var "allBindings"))

namespacesForDefinitions :: TTermDefinition ((Namespace -> a) -> Namespace -> [Definition] -> Namespaces a)
namespacesForDefinitions = define "namespacesForDefinitions" $
  doc "Create namespaces mapping for definitions" $
  "encodeNamespace" ~> "focusNs" ~> "defs" ~>
  "nss" <~ Sets.delete (var "focusNs") (definitionDependencyNamespaces @@ var "defs") $
  "toPair" <~ ("ns" ~> pair (var "ns") (var "encodeNamespace" @@ var "ns")) $
  Module.namespaces (var "toPair" @@ var "focusNs") (Maps.fromList (Lists.map (var "toPair") (Sets.toList (var "nss"))))

