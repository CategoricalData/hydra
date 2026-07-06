-- | Module-to-package routing for the Hydra packaging split.
--
-- A single source of truth for "which package owns which module" and
-- "which directory under dist/json/ does that package write to". Used by
-- both the JSON writers (update-json-*) and the JSON reader
-- (bootstrap-from-json), so the two sides can never disagree about where
-- a module's JSON file should live.
--
-- The mapping is DERIVED, not hardcoded (#474). A 'RoutingMap' is built once
-- per executable from each package's declared modules (its manifest's
-- @mainModules@ + @testModules@ + @derivedMainModules@, or the equivalent
-- compiled per-package lists in "Hydra.Sources.Ext"). For each declared
-- module M of package P we record M -> P; and for each @derivedMainModules@
-- entry we additionally record its derived names
-- (@hydra.dsl.<x>@, @hydra.encode.<x>@, @hydra.decode.<x>@, and the
-- @hydra.sources.{encode,decode}.<x>@ source wrappers) -> P, reusing the
-- shipped-kernel derived-name functions so the naming rule lives in exactly
-- one place. This is what lets a long-tail type module's derived encoder /
-- decoder / DSL wrapper route back to its OWNING package instead of falling
-- through to "hydra-kernel" (the bug the old hand-curated allowlists papered
-- over).
--
-- Routing contract:
--
--   namespaceToPackageIn :: RoutingMap -> ModuleName -> String
--
--     Returns the package name (e.g. "hydra-kernel", "hydra-ext") that owns
--     a given module name. Falls back to "hydra-kernel" for any module not
--     present in the map.
--
--   namespaceToPackageJsonDirIn :: RoutingMap -> FilePath -> ModuleName -> FilePath
--
--     Given the dist-json root (e.g. "../../dist/json") and a module name,
--     returns the directory under which that module's JSON file should be
--     written: "<root>/<package>/src/main/json".
--
--   groupByPackageIn :: RoutingMap -> [Module] -> [(String, [Module])]
--
--     Partitions a list of modules by owning package, sorted by package
--     name for deterministic output ordering.

module Hydra.PackageRouting (
  RoutingMap,
  buildRoutingMap,
  namespaceToPackageIn,
  namespaceToPackageMaybeIn,
  namespaceToPackageJsonDirIn,
  namespaceToPackageTestJsonDirIn,
  groupByPackageIn,
  defaultDistJsonRoot,
) where

import Hydra.Kernel
import qualified Hydra.Dsls as Dsls
import qualified Hydra.Encoding as Encoding
import qualified Hydra.Decoding as Decoding

import Data.Function (on)
import Data.List (groupBy, sortOn)
import qualified Data.List as L
import qualified Data.Map as M
import qualified System.FilePath as FP


-- | Default root under which per-package JSON trees live, relative to the
-- haskell head working directory (heads/haskell/). Callers can override
-- this when writing elsewhere (e.g. bootstrap-from-json's --ext-json-dir).
defaultDistJsonRoot :: FilePath
defaultDistJsonRoot = "../../dist/json"

-- | A resolved module-to-package routing table.
newtype RoutingMap = RoutingMap (M.Map ModuleName String)

-- | Build a 'RoutingMap' from each package's declared modules.
--
-- Input: one @(package, declaredModuleNames)@ pair per package, where the
-- declared set is @mainModules ++ testModules ++ derivedMainModules@.
--
-- For a complete table the caller should pass the @derivedMainModules@ names
-- THEMSELVES in the declared list (so the source module routes correctly) and
-- this function expands each declared module's derived names. Since we cannot
-- tell from a bare name whether it is a @derivedMainModules@ source, we expand
-- the derived names for EVERY declared module; the derived name of a non-type
-- module simply never gets generated, so the extra map entries are harmless.
--
-- Declared (direct) entries take precedence over derived entries on collision,
-- so a hand-written @hydra.dsl.X@ that is itself a declared module routes to
-- its declared owner rather than to whatever package happens to derive that
-- name.
buildRoutingMap :: [(String, [ModuleName])] -> RoutingMap
buildRoutingMap pkgs = RoutingMap (M.union declaredMap derivedMap)
  where
    declaredMap = M.fromList [ (m, p) | (p, ms) <- pkgs, m <- ms ]
    derivedMap  = M.fromList
      [ (d, p)
      | (p, ms) <- pkgs, m <- ms, d <- derivedNames m ]

-- | The derived module names produced from a source module name, using the
-- shipped-kernel derived-name functions as the single source of truth.
derivedNames :: ModuleName -> [ModuleName]
derivedNames m =
  [ Dsls.dslModuleName m
  , Encoding.encodeModuleName m
  , Decoding.decodeModuleName m
  , sourceWrapperName (Encoding.encodeModuleName m)
  , sourceWrapperName (Decoding.decodeModuleName m) ]

-- | The @hydra.sources.<...>@ wrapper name for an already-derived encode/decode
-- module name, mirroring 'Hydra.Codegen.moduleToSourceModule': drop the first
-- dotted segment and prepend @hydra.sources.@.
sourceWrapperName :: ModuleName -> ModuleName
sourceWrapperName (ModuleName s) =
  ModuleName ("hydra.sources." ++ L.intercalate "." (drop 1 (splitOnDot s)))
  where
    splitOnDot = foldr step [""]
      where
        step '.' acc = "" : acc
        step c (cur : rest) = (c : cur) : rest
        step c []           = [[c]]

-- | Map a module name to the package that owns it, via a 'RoutingMap'.
--
-- Fails loudly if the module is not in the routing map. Every legitimate module
-- is declared in some package's manifest (and so appears in the map, directly or
-- via derived-name expansion); an unrouted module is a registration bug — most
-- often a new module not added to its package's Manifest.mainModules. The old
-- silent fallback to "hydra-kernel" masked exactly this class of bug (the
-- unrouted JSON was written into the kernel tree and then pruned by the kernel
-- sync, leaving no trace). No silent failures.
namespaceToPackageIn :: RoutingMap -> ModuleName -> String
namespaceToPackageIn (RoutingMap m) ns = case M.lookup ns m of
  Just p  -> p
  Nothing -> error $ "unrouted module: " ++ unModuleName ns
    ++ " is not declared in any package's manifest (RoutingMap). Add it to the"
    ++ " owning package's Manifest.mainModules."

-- | Total variant of 'namespaceToPackageIn': returns Nothing for an unrouted
-- namespace instead of erroring. Used where a namespace may legitimately be
-- absent from the routing map (e.g. projecting a module's declared
-- dependencies, some of which are outside the current inference universe).
namespaceToPackageMaybeIn :: RoutingMap -> ModuleName -> Maybe String
namespaceToPackageMaybeIn (RoutingMap m) ns = M.lookup ns m

-- | Given a dist-json root and a module name, compute the directory under
-- which that module's JSON file should be written.
namespaceToPackageJsonDirIn :: RoutingMap -> FilePath -> ModuleName -> FilePath
namespaceToPackageJsonDirIn rm root ns =
  root FP.</> namespaceToPackageIn rm ns FP.</> "src" FP.</> "main" FP.</> "json"

-- | Like 'namespaceToPackageJsonDirIn' but for test JSON output.
namespaceToPackageTestJsonDirIn :: RoutingMap -> FilePath -> ModuleName -> FilePath
namespaceToPackageTestJsonDirIn rm root ns =
  root FP.</> namespaceToPackageIn rm ns FP.</> "src" FP.</> "test" FP.</> "json"

-- | Partition a list of modules by owning package, returning a list of
--   (packageName, modules) groups. The groups are sorted by package name
--   for deterministic output ordering.
groupByPackageIn :: RoutingMap -> [Module] -> [(String, [Module])]
groupByPackageIn rm mods =
    fmap collapse
      $ groupBy ((==) `on` fst)
      $ sortOn fst
      $ fmap (\m -> (namespaceToPackageIn rm (moduleName m), m)) mods
  where
    collapse [] = ("", [])  -- unreachable; groupBy never returns empty inner lists
    collapse grp@((pkg, _) : _) = (pkg, fmap snd grp)
