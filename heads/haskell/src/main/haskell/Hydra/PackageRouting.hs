-- | Module-to-package routing for the Hydra packaging split.
--
-- A single source of truth for "which package owns which module" and
-- "which directory under dist/json/ does that package write to". Used by
-- both the JSON writers (update-json-*) and the JSON reader
-- (bootstrap-from-json), so the two sides can never disagree about where
-- a module's JSON file should live.
--
-- A thin shim over the generated, manifest-derived "Hydra.Build.Routing"
-- (packages/hydra-build; #560): this module builds a 'RoutingMap' from each
-- package's declared modules (its manifest's @mainModules@ + @testModules@
-- + @mainDslModules@ + @mainEncodingModules@) and delegates the actual
-- routing/grouping logic to the generated module, converting its fail-loud
-- 'Either' results back into today's total (crash-on-error) signatures so
-- the ~30 call sites across this head's executables need no changes
-- (#474 established the derive-don't-hardcode contract; #560 moved the
-- implementation into the generated module so Java/Python can share it).
--
-- Routing contract:
--
--   namespaceToPackageIn :: RoutingMap -> ModuleName -> String
--
--     Returns the package name (e.g. "hydra-kernel", "hydra-ext") that owns
--     a given module name. Crashes (via 'error') if the module is not routed.
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
  PackageManifest(..),
  buildRoutingMap,
  namespaceToPackageIn,
  namespaceToPackageMaybeIn,
  namespaceToPackageJsonDirIn,
  namespaceToPackageTestJsonDirIn,
  groupByPackageIn,
  defaultDistJsonRoot,
  fromPairs,
) where

import Hydra.Kernel hiding (buildRoutingMap, groupByPackageIn, namespaceToPackageIn)
import qualified Hydra.Build.Routing as GenRouting

import qualified Data.Map as M
import qualified System.FilePath as FP


-- | Default root under which per-package JSON trees live, relative to the
-- haskell head working directory (heads/haskell/). Callers can override
-- this when writing elsewhere (e.g. bootstrap-from-json's --ext-json-dir).
defaultDistJsonRoot :: FilePath
defaultDistJsonRoot = "../../dist/json"

-- | A resolved module-to-package routing table, wrapping the generated
-- module's map so callers keep working against an opaque 'RoutingMap' type.
newtype RoutingMap = RoutingMap (M.Map ModuleName String)

-- | A package's declared module sets, read from its manifest.json.
--
-- @packageManifestTestModules@, @packageManifestDslModules@, and
-- @packageManifestEncodingModules@ are tolerant-empty fields: a package
-- whose manifest omits one contributes nothing for it. Mirrors the
-- Java @PackageManifest@ / Python @PackageManifest@ loaders introduced
-- alongside this type (#560).
data PackageManifest = PackageManifest {
  packageManifestPackage :: String,
  packageManifestMainModules :: [ModuleName],
  packageManifestTestModules :: [ModuleName],
  packageManifestDslModules :: [ModuleName],
  packageManifestEncodingModules :: [ModuleName]
}

-- | Adapt the legacy @[(package, declaredModuleNames)]@ shape (still used by
-- 'buildRoutingMap' callers) into 'PackageManifest' values, treating the
-- whole declared list as main modules with no test/DSL/encoding split.
-- A bridge for callers migrating incrementally; not needed once a caller
-- reads 'PackageManifest' values directly from manifest.json.
fromPairs :: [(String, [ModuleName])] -> [PackageManifest]
fromPairs pairs = [ PackageManifest p ms [] [] [] | (p, ms) <- pairs ]

-- | Build a 'RoutingMap' from each package's declared modules.
--
-- Input: one @(package, declaredModuleNames)@ pair per package. Delegates
-- to the generated 'GenRouting.buildRoutingMap', which expands each
-- declared module's derived names (@hydra.dsl.<x>@, @hydra.encode.<x>@,
-- @hydra.decode.<x>@, and their @hydra.sources.*@ source wrappers) using
-- the shipped-kernel derived-name functions, so the naming rule lives in
-- exactly one place (the generated module, not this shim).
buildRoutingMap :: [(String, [ModuleName])] -> RoutingMap
buildRoutingMap pkgs = RoutingMap (GenRouting.buildRoutingMap pkgs)

-- | Map a module name to the package that owns it, via a 'RoutingMap'.
--
-- Fails loudly if the module is not in the routing map. Every legitimate module
-- is declared in some package's manifest (and so appears in the map, directly or
-- via derived-name expansion); an unrouted module is a registration bug — most
-- often a new module not added to its package's Manifest.mainModules. The old
-- silent fallback to "hydra-kernel" masked exactly this class of bug (the
-- unrouted JSON was written into the kernel tree and then pruned by the kernel
-- sync, leaving no trace). No silent failures.
--
-- Converts the generated module's fail-loud 'Either' into a crash, keeping
-- this function's total (non-Either) signature so its ~30 call sites are
-- unaffected by the #560 migration; explicit Either-handling at those call
-- sites is deferred to a later cleanup.
namespaceToPackageIn :: RoutingMap -> ModuleName -> String
namespaceToPackageIn (RoutingMap m) ns =
  either (error . showRoutingError ns) id (GenRouting.namespaceToPackageIn m ns)

showRoutingError :: ModuleName -> Error -> String
showRoutingError ns err =
  "unrouted module: " ++ unModuleName ns
  ++ " is not declared in any package's manifest (RoutingMap). Add it to the"
  ++ " owning package's Manifest.mainModules. (" ++ show err ++ ")"

-- | Total variant of 'namespaceToPackageIn': returns Nothing for an unrouted
-- namespace instead of erroring. Used where a namespace may legitimately be
-- absent from the routing map (e.g. projecting a module's declared
-- dependencies, some of which are outside the current inference universe).
namespaceToPackageMaybeIn :: RoutingMap -> ModuleName -> Maybe String
namespaceToPackageMaybeIn (RoutingMap m) ns =
  either (const Nothing) Just (GenRouting.namespaceToPackageIn m ns)

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
--
-- Converts the generated module's fail-loud 'Either' into a crash, same
-- rationale as 'namespaceToPackageIn'.
groupByPackageIn :: RoutingMap -> [Module] -> [(String, [Module])]
groupByPackageIn (RoutingMap m) mods =
  either (error . showGroupingError) id (GenRouting.groupByPackageIn m mods)

showGroupingError :: Error -> String
showGroupingError err =
  "unrouted module encountered while grouping by package: " ++ show err
