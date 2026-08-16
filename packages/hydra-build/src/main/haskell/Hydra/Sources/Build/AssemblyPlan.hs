
module Hydra.Sources.Build.AssemblyPlan where

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
import qualified Hydra.Dsl.Lib.Ordering as Ordering
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


-- | The pure assembly-plan derivation (#416 P3 / assemblyplan): given a
-- distribution package, a target language, and the overlay source trees that
-- exist for that (package, language), produce the ordered list of copy entries
-- that realize the @+ copy(overlay/<lang>/<pkg>/)@ half of the governing
-- equation @dist/<lang>/<pkg>/ = transform(packages/<pkg>/) + copy(overlay/<lang>/<pkg>/)@.
--
-- This is the terms half of the assembly split; @hydra.build.assembly@ is the
-- types half (the @AssemblyPlan@ schema-of-record). Following the sync-matrix
-- precedent (and every sibling planning module), the derivation returns plain
-- tuples rather than constructing a package-local typed record: an entry is a
-- @(sourceTree, destTree, kind)@ triple where @kind@ is a string tag
-- (\"merge\" / \"wipeThenCopy\" / \"symlinkTree\" / \"symlinkDir\", matching the
-- @AssemblyEntryKind@ variants). A thin host-native executor walks the triples
-- and performs the byte-level I\/O (@cp -R@ \/ @rm -rf@ \/ @ln -sf@) plus the
-- fixed @__pycache__@\/dotfile exclusions, which stay native (the decision is
-- trivial, the effect is not translingual).
--
-- Scope (per 416-P3-assembly-design.md): this promotes §1a (the overlay-merge
-- triples common to the java\/python\/scala\/haskell copy scripts) and §1b (the
-- keep-paths a prune-stale pass must protect). The richer §1c TypeScript
-- cross-package import graph is deferred pending a design call (symlink
-- realization boundary + the build↔kernel back-edge ordering).
--
-- Trees are passed in (design §4-A): @deriveAssemblyPlan@ is pure and
-- layout-agnostic; the caller performs the @listDirectory@ effect to discover
-- which overlay trees exist and passes their source-set relative paths in.
ns :: ModuleName
ns = ModuleName "hydra.build.assemblyplan"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "The pure assembly-plan derivation: the ordered overlay-copy entries and keep-paths for a distribution package")}
  where
   definitions = [
     toDefinition deriveAssemblyPlan,
     toDefinition keepPathsFor,
     toDefinition overlayEntries,
     toDefinition remapDest,
     toDefinition sourceSetSubdir]

-- | The full assembly plan for a @(package, language)@ pair: the ordered copy
-- entries (from 'overlayEntries') paired with the keep-paths (from
-- 'keepPathsFor'). @availableTrees@ is the list of source-set-relative overlay
-- subdirectories that exist for this (package, language) — e.g. @["src"]@ for a
-- package whose only overlay tree is @overlay/<lang>/<pkg>/src@. @treeFiles@ is
-- the list of @(overlaySubdir, relPath)@ pairs the caller enumerated under those
-- trees, used to compute the keep-paths. Result is a pair @(entries, keepPaths)@
-- where @entries@ is a list of @(sourceTree, destTree, kind)@ triples and
-- @keepPaths@ is a list of @(sourceSetDir, relPath)@ pairs.
deriveAssemblyPlan :: TypedTermDefinition (String -> String -> [String] -> [(String, String)] -> ([(String, String, String)], [(String, String)]))
deriveAssemblyPlan = define "deriveAssemblyPlan" $
  doc "The full assembly plan (entries, keepPaths) for a (package, language) pair" $
  "pkg" ~> "lang" ~> "availableTrees" ~> "treeFiles" ~>
  pair
    (overlayEntries @@ var "pkg" @@ var "lang" @@ var "availableTrees")
    (keepPathsFor @@ var "pkg" @@ var "lang" @@ var "treeFiles")

-- | The keep-paths a prune-stale pass must protect for a package's overlay
-- files: one @(sourceSetDir, relPath)@ pair per file the plan copies.
-- @treeFiles@ is the list of @(overlaySubdir, relPath)@ pairs the caller
-- enumerated (via @listDirectory@) under the package's overlay trees; each is
-- remapped so its @sourceSetDir@ is the destination source-set directory the
-- file lands in (@dist/<lang>/<pkg>/<overlaySubdir>@), keeping the serialized
-- @<sourceSetDir>\\t<relPath>@ contract stable for the prune consumers.
keepPathsFor :: TypedTermDefinition (String -> String -> [(String, String)] -> [(String, String)])
keepPathsFor = define "keepPathsFor" $
  doc "The (sourceSetDir, relPath) keep-paths for a package's copied overlay files" $
  "pkg" ~> "lang" ~> "treeFiles" ~>
  Lists.map
    ("tf" ~>
      pair
        (remapDest @@ var "pkg" @@ var "lang" @@ (Pairs.first (var "tf")))
        (Pairs.second (var "tf")))
    (var "treeFiles")

-- | The ordered overlay-merge entries (§1a) for a package's available trees:
-- one @(sourceTree, destTree, "merge")@ triple per existing overlay subdirectory,
-- mapping @overlay/<lang>/<pkg>/<subdir>@ to @dist/<lang>/<pkg>/<subdir>@. This
-- is the java\/python\/scala\/haskell copy-overlay common core; the @merge@ kind
-- leaves pre-existing generated files intact. Wipe-then-copy and the symlink
-- kinds (§1c) are not emitted here.
overlayEntries :: TypedTermDefinition (String -> String -> [String] -> [(String, String, String)])
overlayEntries = define "overlayEntries" $
  doc "The ordered (sourceTree, destTree, merge) overlay-copy entries for a package's available trees" $
  "pkg" ~> "lang" ~> "availableTrees" ~>
  Lists.map
    ("subdir" ~>
      triple
        (sourceSetSubdir @@ (Strings.concat (list [string "overlay/", var "lang", string "/", var "pkg"])) @@ var "subdir")
        (sourceSetSubdir @@ (Strings.concat (list [string "dist/", var "lang", string "/", var "pkg"])) @@ var "subdir")
        (string "merge"))
    (var "availableTrees")

-- | Remap an overlay source-set directory to its distribution destination:
-- @overlay/<lang>/<pkg>/<subdir>@ becomes @dist/<lang>/<pkg>/<subdir>@ for the
-- given package and language. Used to compute a keep-path's destination
-- source-set directory.
remapDest :: TypedTermDefinition (String -> String -> String -> String)
remapDest = define "remapDest" $
  doc "Remap an overlay source-set subdir to its dist/<lang>/<pkg>/<subdir> destination" $
  "pkg" ~> "lang" ~> "subdir" ~>
  sourceSetSubdir @@ (Strings.concat (list [string "dist/", var "lang", string "/", var "pkg"])) @@ var "subdir"

-- | Join a package root directory and a source-set subdirectory with a single
-- @/@ separator, e.g. @("overlay/java/hydra-pg", "src") -> "overlay/java/hydra-pg/src"@.
sourceSetSubdir :: TypedTermDefinition (String -> String -> String)
sourceSetSubdir = define "sourceSetSubdir" $
  doc "Join a package root and a source-set subdirectory with a / separator" $
  "root" ~> "subdir" ~>
  Strings.concat (list [var "root", string "/", var "subdir"])
