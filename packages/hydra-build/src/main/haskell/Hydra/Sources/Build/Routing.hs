
module Hydra.Sources.Build.Routing where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (buildRoutingMap, groupByPackageIn, namespaceToPackageIn)
import qualified Hydra.Core.Dsl.Paths        as Paths
import qualified Hydra.Core.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Core.Dsl.Ast          as Ast
import qualified Hydra.Core.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Core.Dsl.Coders       as Coders
import qualified Hydra.Core.Dsl.Util      as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core         as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph        as Graph
import qualified Hydra.Core.Dsl.Json.Model         as Json
import qualified Hydra.Core.Dsl.Lib.Chars    as Chars
import qualified Hydra.Core.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Core.Dsl.Lib.Equality as Equality
import qualified Hydra.Core.Dsl.Lib.Lists    as Lists
import qualified Hydra.Core.Dsl.Lib.Literals as Literals
import qualified Hydra.Core.Dsl.Lib.Logic    as Logic
import qualified Hydra.Core.Dsl.Lib.Maps     as Maps
import qualified Hydra.Core.Dsl.Lib.Math     as Math
import qualified Hydra.Core.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Core.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Core.Dsl.Lib.Sets     as Sets
import qualified Hydra.Core.Dsl.Lib.Strings  as Strings
import qualified Hydra.Core.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Core.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Core.Overlay.Haskell.Dsl.Base         as MetaBase
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Core.Dsl.Packaging       as Packaging
import qualified Hydra.Core.Dsl.Parsing      as Parsing
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing      as Testing
import qualified Hydra.Core.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Core.Dsl.Topology     as Topology
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Core.Dsl.Typing       as Typing
import qualified Hydra.Core.Dsl.Util         as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Variants     as Variants
import qualified Hydra.Core.Dsl.Errors       as Error
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Dsls as Dsls
import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Hydra.Sources.Kernel.Terms.Encoding as Encoding
import qualified Hydra.Sources.Kernel.Terms.Decoding as Decoding


-- | Module-to-package routing for the Hydra packaging split (#474, #416 pilot).
--
-- A DSL-level, translingual port of the manifest-derived, fail-loud design in
-- @Hydra.PackageRouting@ (heads/haskell/src/main/haskell/Hydra/PackageRouting.hs).
-- Unlike that native module, this one cannot literally crash the process (the
-- DSL has no such primitive); "fail loud" here means returning a descriptive
-- @Left@ rather than silently falling back to a default package, which is the
-- behavior this module intentionally replaces.
ns :: ModuleName
ns = ModuleName "hydra.build.routing"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Dsls.ns, Names.ns, Encoding.ns, Decoding.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Manifest-derived, fail-loud routing of modules to their owning packages")}
  where
   definitions = [
     toDefinition buildRoutingMap,
     toDefinition derivedNames,
     toDefinition groupByPackageIn,
     toDefinition namespaceToPackageIn,
     toDefinition sourceWrapperName]

-- | Build a routing map from each package's declared modules.
--
-- Input: one (package name, declared module names) pair per package, where the
-- declared set is expected to be mainModules ++ testModules ++ derivedMainModules.
-- Declared (direct) entries take precedence over derived entries on collision, so
-- a hand-written hydra.core.dsl.X that is itself a declared module routes to its
-- declared owner rather than to whatever package happens to derive that name.
buildRoutingMap :: TypedTermDefinition ([(String, [ModuleName])] -> M.Map ModuleName String)
buildRoutingMap = define "buildRoutingMap" $
  doc "Build a routing map from each package's declared modules" $
  "pkgs" ~>
  "declaredPairs" <~ (Lists.concat (Lists.map
    ("p" ~> Lists.map ("m" ~> pair (var "m") (Pairs.first $ var "p")) (Pairs.second $ var "p"))
    (var "pkgs")) :: TypedTerm [(ModuleName, String)]) $
  "derivedPairs" <~ (Lists.concat (Lists.map
    ("p" ~> Lists.concat (Lists.map
      ("m" ~> Lists.map ("d" ~> pair (var "d") (Pairs.first $ var "p")) (derivedNames @@ var "m"))
      (Pairs.second $ var "p")))
    (var "pkgs")) :: TypedTerm [(ModuleName, String)]) $
  Maps.union (Maps.fromList $ var "declaredPairs") (Maps.fromList $ var "derivedPairs" :: TypedTerm (M.Map ModuleName String))

-- | The derived module names produced from a source module name: the DSL wrapper
-- module, the encoder and decoder modules, and the hydra.sources.* wrappers of the
-- latter two.
derivedNames :: TypedTermDefinition (ModuleName -> [ModuleName])
derivedNames = define "derivedNames" $
  doc "The derived module names produced from a source module name" $
  "m" ~>
  "dsl" <~ (Dsls.dslModuleName @@ var "m") $
  "enc" <~ (Encoding.encodeModuleName @@ var "m") $
  "dec" <~ (Decoding.decodeModuleName @@ var "m") $
  list [
    var "dsl",
    var "enc",
    var "dec",
    sourceWrapperName @@ var "enc",
    sourceWrapperName @@ var "dec"]

-- | The hydra.sources.<...> wrapper name for an already-derived encode/decode
-- module name: drop the first dotted segment and prepend "hydra.sources.".
sourceWrapperName :: TypedTermDefinition (ModuleName -> ModuleName)
sourceWrapperName = define "sourceWrapperName" $
  doc "The hydra.sources.<...> wrapper name for a derived encode/decode module name" $
  "ns" ~>
  "parts" <~ Strings.splitOn (string ".") (Packaging.unModuleName (var "ns")) $
  "fallback" <~ Packaging.moduleName2 (Strings.concat $ list [string "hydra.sources.", Packaging.unModuleName (var "ns")]) $
  Optionals.match (Lists.uncons $ var "parts") (var "fallback") ("uc" ~>
      Packaging.moduleName2 (Strings.concat $ list [string "hydra.sources.", Strings.join (string ".") (Pairs.second $ var "uc")]))

-- | Map a module name to the package that owns it, via a routing map.
--
-- Fails loudly (returns a descriptive Left) if the module is not in the routing
-- map. Every legitimate module is declared in some package's manifest (and so
-- appears in the map, directly or via derived-name expansion); an unrouted
-- module is a registration bug -- most often a new module not added to its
-- owning package's Manifest.mainModules. The old silent fallback to
-- "hydra-kernel" masked exactly this class of bug.
namespaceToPackageIn :: TypedTermDefinition (M.Map ModuleName String -> ModuleName -> Either Error String)
namespaceToPackageIn = define "namespaceToPackageIn" $
  doc "Map a module name to the package that owns it, failing loudly if unrouted" $
  "rm" ~> "ns" ~>
  Optionals.match (Maps.lookup (var "ns") (var "rm" :: TypedTerm (M.Map ModuleName String)))
    (left $ Error.errorOther $ Error.otherError $ Strings.concat $ list [
      string "unrouted module: ",
      Packaging.unModuleName (var "ns"),
      string " is not declared in any package's manifest (RoutingMap). Add it to the owning package's Manifest.mainModules."])
    ("p" ~> right $ var "p")

-- | Partition a list of modules by owning package, returning a list of
-- (packageName, modules) pairs. The pairs are sorted by package name for
-- deterministic output ordering. Fails loudly (propagating the first
-- unrouted-module error) if any module is not in the routing map.
groupByPackageIn :: TypedTermDefinition (M.Map ModuleName String -> [Module] -> Either Error [(String, [Module])])
groupByPackageIn = define "groupByPackageIn" $
  doc "Partition a list of modules by owning package, sorted by package name" $
  "rm" ~> "mods" ~>
  "step" <~ ("acc" ~> "m" ~>
    Eithers.bind (var "acc" :: TypedTerm (Either Error (M.Map String [Module]))) ("grouped" ~>
      Eithers.bind (namespaceToPackageIn @@ (var "rm" :: TypedTerm (M.Map ModuleName String)) @@ (Packaging.moduleName $ var "m")) ("pkg" ~>
        right $ Maps.alter
          ("mods0" ~> just $ Lists.cons (var "m") (Optionals.withDefault (list ([] :: [TypedTerm Module])) (var "mods0")))
          (var "pkg")
          (var "grouped" :: TypedTerm (M.Map String [Module]))))) $
  Eithers.map ("grouped" ~> Maps.toList (var "grouped" :: TypedTerm (M.Map String [Module])))
    (Lists.foldl (var "step") (right Maps.empty :: TypedTerm (Either Error (M.Map String [Module]))) (var "mods"))
