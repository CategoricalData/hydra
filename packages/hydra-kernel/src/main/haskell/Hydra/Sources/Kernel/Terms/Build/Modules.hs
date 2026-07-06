module Hydra.Sources.Kernel.Terms.Build.Modules where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import qualified Hydra.Dsl.Paths        as Paths
import qualified Hydra.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Lib.Chars    as Chars
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Math     as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Sets     as Sets
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base         as MetaBase
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms        as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular           as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing      as Testing
import qualified Hydra.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants     as Variants
import qualified Hydra.Dsl.Errors       as Error
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Strip as Strip


-- | Pure module-list utilities shared by the code-generation drivers (#416 Phase 2).
--
-- A DSL-level, translingual home for a family of small, pure helpers that the
-- Java and Python generator drivers previously each duplicated (see
-- @Generation.java@ / @generation.py@). Everything here is pure: no primitives,
-- no I/O, no routing map. Path helpers (@secondLevelDir@) operate on
-- already-forward-slash-separated relative paths; OS-separator normalization
-- stays in the drivers.
ns :: ModuleName
ns = ModuleName "hydra.build.modules"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Strip.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Pure module-list utilities shared by the code-generation drivers")}
  where
   definitions = [
     toDefinition dedupPreservingOrder,
     toDefinition filterKernelModules,
     toDefinition filterTypeModules,
     toDefinition secondLevelDir,
     toDefinition stripAllTermTypes,
     toDefinition stripTermTypes]

-- | Deduplicate a list of strings, preserving first-occurrence order.
--
-- Matches @Data.List.nub@ semantics: the first occurrence of each distinct
-- element is kept, in order; later duplicates are dropped. Replaces the ad-hoc
-- HashSet/set accumulation loops in the drivers.
dedupPreservingOrder :: TypedTermDefinition ([String] -> [String])
dedupPreservingOrder = define "dedupPreservingOrder" $
  doc "Deduplicate a list of strings, preserving first-occurrence order" $
  "xs" ~> Lists.nub (var "xs")

-- | Keep only non-kernel modules: those whose name does NOT start with "hydra."
-- and does NOT start with "hydra.json.yaml.".
--
-- Polarity note: this KEEPS the modules that are not part of the kernel
-- namespace. A module named e.g. "example.foo" is kept; "hydra.core" and
-- "hydra.json.yaml.model" are dropped.
filterKernelModules :: TypedTermDefinition ([Module] -> [Module])
filterKernelModules = define "filterKernelModules" $
  doc "Keep only modules outside the hydra.* and hydra.json.yaml.* namespaces" $
  "mods" ~>
  -- startsWith prefix s: s begins with prefix iff splitting s on prefix yields an
  -- empty first segment and at least one further segment (guards the empty-string case).
  "startsWith" <~ ("prefix" ~> "s" ~>
    "parts" <~ Strings.splitOn (var "prefix") (var "s") $
    Optionals.cases (Lists.uncons $ var "parts")
      false
      ("uc" ~> Logic.and
        (Strings.null $ Pairs.first $ var "uc")
        (Logic.not $ Lists.null $ Pairs.second $ var "uc"))) $
  Lists.filter
    ("m" ~>
      "name" <~ Packaging.unModuleName (Packaging.moduleName $ var "m") $
      Logic.and
        (Logic.not $ var "startsWith" @@ string "hydra." @@ var "name")
        (Logic.not $ var "startsWith" @@ string "hydra.json.yaml." @@ var "name"))
    (var "mods")

-- | Keep only modules that contain at least one type-defining binding.
--
-- A module with only term or primitive definitions (or no definitions) is
-- dropped.
filterTypeModules :: TypedTermDefinition ([Module] -> [Module])
filterTypeModules = define "filterTypeModules" $
  doc "Keep only modules containing at least one type-defining binding" $
  "mods" ~> Lists.filter
    ("m" ~> Lists.foldl
      ("acc" ~> "d" ~> Logic.or (var "acc")
        (cases _Definition (var "d") (Just false) [
          _Definition_type>>: "_" ~> true]))
      false
      (Packaging.moduleDefinitions $ var "m"))
    (var "mods")

-- | The first two forward-slash-separated segments of a relative path, joined by
-- "/", or nothing if the path has fewer than two segments.
--
-- @secondLevelDir "hydra/java/coder.json" = Just "hydra/java"@. The input is
-- expected to already use forward slashes; OS-separator normalization is the
-- driver's responsibility.
--
-- Trailing/repeated-separator behavior follows the Python reference: a trailing
-- separator yields an empty final segment, so @secondLevelDir "hydra/" = Just "hydra/"@
-- (two segments: "hydra" and ""). The Java reference differs (String.split drops
-- trailing empties, giving one segment -> none); exact dual-match is impossible, and
-- this port intentionally matches Python.
secondLevelDir :: TypedTermDefinition (String -> Maybe String)
secondLevelDir = define "secondLevelDir" $
  doc "The first two forward-slash-separated segments of a relative path, joined by /" $
  "rel" ~>
  "parts" <~ Strings.splitOn (string "/") (var "rel") $
  Logic.ifElse (Equality.lt (Lists.length $ var "parts") (int32 2))
    nothing
    (just $ Strings.intercalate (string "/") (Lists.take (int32 2) (var "parts")))

-- | Strip System F type annotations from every module in a list.
stripAllTermTypes :: TypedTermDefinition ([Module] -> [Module])
stripAllTermTypes = define "stripAllTermTypes" $
  doc "Strip System F type annotations from every module in a list" $
  "mods" ~> Lists.map ("m" ~> stripTermTypes @@ var "m") (var "mods")

-- | Strip System F type annotations from all term bodies in a module.
--
-- For each term definition: strip types from the body via
-- @Strip.removeTypesFromTerm@ and clear the (module-level) signature, to avoid
-- type conflicts. Type and primitive definitions pass through unchanged.
-- Preserves the module's name, metadata, and dependencies.
stripTermTypes :: TypedTermDefinition (Module -> Module)
stripTermTypes = define "stripTermTypes" $
  doc "Strip System F type annotations from all term bodies in a module" $
  "m" ~>
  Packaging.module_
    (Packaging.moduleName $ var "m")
    (Packaging.moduleMetadata $ var "m")
    (Packaging.moduleDependencies $ var "m")
    (Lists.map
      ("d" ~> cases _Definition (var "d") Nothing [
        _Definition_type>>: "td" ~> Packaging.definitionType (var "td"),
        _Definition_primitive>>: "pd" ~> Packaging.definitionPrimitive (var "pd"),
        _Definition_term>>: "td" ~> Packaging.definitionTerm (Packaging.termDefinition
          (Packaging.termDefinitionName $ var "td")
          (Packaging.termDefinitionMetadata $ var "td")
          nothing
          (Strip.removeTypesFromTerm @@ (Packaging.termDefinitionBody $ var "td")))])
      (Packaging.moduleDefinitions $ var "m"))
