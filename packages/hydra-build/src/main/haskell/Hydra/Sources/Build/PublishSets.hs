module Hydra.Sources.Build.PublishSets where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Sets     as Sets
import qualified Hydra.Dsl.Sorting      as Sorting
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.List                   as L
import qualified Data.Set                    as S

import qualified Hydra.Sources.Kernel.Terms.Strip as Strip


-- | Publish-set closure + topological ordering (#416 P2, Core A — pure).
--
-- A DSL-level, translingual home for the dependency-closure guard that the five
-- publish drivers (publish-hackage/maven/sbt/npm/pypi.sh) each copy-paste: given
-- the package dependency graph and a candidate publish set, assert the set is
-- dependency-closed (every dependency of every member is in-set OR externally
-- satisfied) and derive a leaves-first upload order. Everything here is pure: the
-- network "already-published?" probe stays native and is supplied to
-- 'isPublishSetClosed' as the @externallySatisfied@ set (mirroring the #630
-- pattern — pure decision logic in Hydra, the I/O in the native caller).
--
-- Subsumes #573: 'publishSetForRegistry' derives each per-registry publish set
-- from 'Hydra.Sources.Build.PackagingProfile' rather than a hand-maintained array.
-- (That derivation lives here once PackagingProfile exposes the per-registry
-- eligibility predicate; the closure/topo core below is registry-agnostic.)
ns :: ModuleName
ns = ModuleName "hydra.build.publishsets"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([ModuleName "hydra.sorting", Strip.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Publish-set dependency-closure guard and leaves-first ordering, shared by the publish drivers")}
  where
   definitions = [
     toDefinition coderBaselinePackages,
     toDefinition isPublishSetClosed,
     toDefinition packageDepNames,
     toDefinition publishSetTopoOrder,
     toDefinition requiresCoders,
     toDefinition reverseDepClosure,
     toDefinition strandedDeps]

-- | The plain Haskell list backing 'coderBaselinePackages'. Exposed separately (as
-- 'expectedLibraryNames' is in @Hydra.Sources.Build.Libraries@) so the native
-- @coder-baseline.json@ emitter in @Hydra.ManifestGeneration@ can import the names
-- directly without deconstructing a generated 'Term'. This is the ONE place the
-- coder-baseline package names are written down (Option-A: names in data).
coderBaselineNames :: [String]
coderBaselineNames = ["hydra-kernel", "hydra-haskell"]

-- | The distribution packages that do NOT need coder modules loaded when their
-- JSON is transformed to a target: the two baseline packages @hydra-kernel@ and
-- @hydra-haskell@. Every other package (the coder packages themselves, and the
-- ext/data-domain packages whose modules reference coder-package types) needs the
-- coder universe loaded — see 'requiresCoders'.
--
-- This is the DATA (wrapping 'coderBaselineNames') behind the @case "$PACKAGE"@ that
-- @heads/haskell/bin/transform-json-to-target.sh@ and @heads/java/bin/
-- seed-dist-haskell.sh@ each hand-maintained (the latter's own comment noted it
-- \"mirror[s] the retired seeder's baseline-vs-coder-package split\" — the hand-copy
-- drift this promotion removes). The drivers read the emitted @coder-baseline.json@ /
-- the 'requiresCoders' predicate instead of branching on name literals. (#416)
coderBaselinePackages :: TypedTermDefinition [PackageName]
coderBaselinePackages = define "coderBaselinePackages" $
  doc "The packages that need no coders loaded during JSON->target transform (hydra-kernel, hydra-haskell)" $
  list (fmap (\n -> wrap _PackageName (string n)) coderBaselineNames)

-- | Whether the publish set is dependency-closed given the externally-satisfied
-- set: true iff 'strandedDeps' is empty. The native caller fails on false and can
-- report the stranded pairs from 'strandedDeps' for a precise diagnostic.
isPublishSetClosed :: TypedTermDefinition ([Package] -> [PackageName] -> [PackageName] -> Bool)
isPublishSetClosed = define "isPublishSetClosed" $
  doc "Whether the publish set is dependency-closed given the externally-satisfied set" $
  "packages" ~> "publishSet" ~> "externallySatisfied" ~>
    Lists.isEmpty (strandedDeps @@ var "packages" @@ var "publishSet" @@ var "externallySatisfied")

-- | The names of a package's direct Hydra dependencies.
packageDepNames :: TypedTermDefinition (Package -> [PackageName])
packageDepNames = define "packageDepNames" $
  doc "The names of a package's direct dependencies" $
  "pkg" ~>
    Lists.map
      ("d" ~> Packaging.packageDependencyName (var "d"))
      (Packaging.packageDependencies (var "pkg"))

-- | The publish set in leaves-first topological order (a package appears after
-- all of its in-set dependencies): build the adjacency list @(name, in-set deps)@
-- for the packages that are in the publish set, topologically sort it, and take
-- the linear ordering (an acyclic package graph always yields one). Replaces the
-- hand-maintained leaves-first PUBLISH_SET arrays in the publish drivers.
publishSetTopoOrder :: TypedTermDefinition ([Package] -> [PackageName] -> [PackageName])
publishSetTopoOrder = define "publishSetTopoOrder" $
  doc "The publish set in leaves-first topological order" $
  "packages" ~> "publishSet" ~>
    Eithers.either
      ("_cycles" ~> var "publishSet")   -- fallback to the input order if the graph has a cycle
      ("sorted" ~> var "sorted")
      (Sorting.topologicalSort
        (Lists.map
          ("pkg" ~> pair
            (Packaging.packageName (var "pkg"))
            (Lists.filter
              ("dep" ~> Lists.member (var "dep") (var "publishSet"))
              (packageDepNames @@ var "pkg")))
          (Lists.filter
            ("pkg" ~> Lists.member (Packaging.packageName (var "pkg")) (var "publishSet"))
            (var "packages"))))

-- | Every package that transitively depends on the root package (including the
-- root itself), sorted. This is the reverse-dependency (dirty-set forward
-- propagation) closure behind @sync-packages.sh --from <pkg>@: "regenerate <pkg>
-- and everything downstream of it." Promotes the pure graph algorithm formerly in
-- @bin/lib/hydra-packages.py cmd_reverse_closure@ (#416).
--
-- Reuses the kernel's transitive-reachability engine 'Sorting.findReachableNodes'
-- (the same helper 'Hydra.Sources.Kernel.Terms.Dependencies' uses for let-pruning),
-- fed a successor function that yields, for a package name, the set of package names
-- that directly depend on it (the reverse edges built from 'packageDepNames'). A
-- finite graph reaches its full closure without any explicit loop construct.
reverseDepClosure :: TypedTermDefinition ([Package] -> PackageName -> [PackageName])
reverseDepClosure = define "reverseDepClosure" $
  doc "Every package that transitively depends on the root (including the root), sorted" $
  "packages" ~> "root" ~>
    "dependents" <~ ("name" ~> (Sets.fromList (Lists.map
      ("pkg" ~> Packaging.packageName (var "pkg"))
      (Lists.filter
        ("pkg" ~> Lists.member (var "name") (packageDepNames @@ var "pkg"))
        (var "packages"))) :: TypedTerm (S.Set PackageName))) $
    Lists.sortBy ("n" ~> unwrap _PackageName @@ var "n") (Sets.toList
      (Sorting.findReachableNodes (var "dependents") (var "root" :: TypedTerm PackageName)))

-- | Whether a package needs the coder universe loaded when its JSON is transformed
-- to a target language: true for every package EXCEPT the two in
-- 'coderBaselinePackages'. Promotes the @case "$PACKAGE"@ decision the transform and
-- cold-seed drivers each hardcoded — the native caller maps @True -> "--include-coders"@,
-- @False -> ""@ (the load-flags string stays native; the DECISION is the promoted data).
-- An unknown package name is treated as needing coders (the drivers' safe default). (#416)
requiresCoders :: TypedTermDefinition (PackageName -> Bool)
requiresCoders = define "requiresCoders" $
  doc "Whether a package needs coders loaded during JSON->target transform (all but the baseline packages)" $
  "pkg" ~> Logic.not (Lists.member (var "pkg") (asTerm coderBaselinePackages))

-- | The (member, missingDependency) pairs that would be stranded by publishing
-- the given set: for each package in the set, each direct dependency whose name
-- is NEITHER in the publish set NOR in the externally-satisfied set (the latter
-- supplied by the native caller after its registry probe). Empty result ==
-- dependency-closed.
strandedDeps :: TypedTermDefinition ([Package] -> [PackageName] -> [PackageName] -> [(PackageName, PackageName)])
strandedDeps = define "strandedDeps" $
  doc "The (member, missingDep) pairs stranded by publishing the set (empty = closed)" $
  "packages" ~> "publishSet" ~> "externallySatisfied" ~>
    Lists.concat
      (Lists.map
        ("pkg" ~>
          Lists.map
            ("dep" ~> pair (Packaging.packageName (var "pkg")) (var "dep"))
            (Lists.filter
              ("dep" ~> Logic.and
                (Logic.not (Lists.member (var "dep") (var "publishSet")))
                (Logic.not (Lists.member (var "dep") (var "externallySatisfied"))))
              (packageDepNames @@ var "pkg")))
        (Lists.filter
          ("pkg" ~> Lists.member (Packaging.packageName (var "pkg")) (var "publishSet"))
          (var "packages")))
