module Hydra.Sources.Build.Assembly where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


-- | The type-level "assembly plan" model (#416 P3 / assembly): the pure,
-- translingual description of how a distribution package's overlay term is
-- realized on disk. It factors the @+ copy(overlay/<lang>/<pkg>/)@ half of the
-- governing equation
-- @dist/<lang>/<pkg>/ = transform(packages/<pkg>/) + copy(overlay/<lang>/<pkg>/)@
-- into data: an ordered list of copy\/symlink entries plus the keep-paths a
-- pruning consumer must protect. A thin host-native executor walks the plan and
-- performs the byte-level I/O (@cp -R@\/@ln -sf@\/@rm -rf@); every decision about
-- WHAT to copy or link lives here, so the eight per-host copy scripts and the
-- TypeScript assemble case-block share one definition instead of re-deriving it.
--
-- This module is the types half (mirroring @hydra.build.format@); the pure
-- @deriveAssemblyPlan@ derivation is the terms half
-- (@hydra.build.assemblyplan@). Paths are plain strings (worktree-relative),
-- matching the digest-file path convention in @hydra.build.format@.
ns :: ModuleName
ns = ModuleName "hydra.build.assembly"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = [],
            moduleMetadata = descriptionMetadata (Just ("Type-level model of a distribution package's assembly plan:"
              ++ " the ordered copy/symlink entries and keep-paths that a host-native executor realizes on disk"
              ++ " when merging overlay source onto a generated distribution package."
              ++ " Factors the copy(overlay/<lang>/<pkg>/) half of the governing equation into data,"
              ++ " so the per-host copy scripts share one translingual description."
              ++ " See https://github.com/CategoricalData/hydra/issues/416"))}
  where
    definitions = [
      assemblyEntry,
      assemblyEntryKind,
      assemblyPlan,
      keepPathEntry]

assemblyEntry :: TypeDefinition
assemblyEntry = define "AssemblyEntry" $
  doc ("A single copy-or-link operation in an assembly plan: realize a source tree at a destination"
    ++ " tree by the given kind, optionally excluding matching relative paths.") $
  T.record [
    "sourceTree">:
      doc "The source tree to realize, as a worktree-relative path (e.g. overlay/<lang>/<pkg>/src)"
      T.string,
    "destTree">:
      doc "The destination tree, as a worktree-relative path (e.g. dist/<lang>/<pkg>/src)"
      T.string,
    "kind">:
      doc "How the source tree is realized at the destination"
      assemblyEntryKind,
    "exclude">:
      doc "Relative path fragments to exclude when realizing the entry (e.g. __pycache__); empty means copy everything"
      (T.list T.string)]

assemblyEntryKind :: TypeDefinition
assemblyEntryKind = define "AssemblyEntryKind" $
  doc ("How an assembly entry's source tree is realized at its destination. The distinction gates the"
    ++ " host-native executor's dispatch (merge-copy vs. wipe-then-copy vs. symlink).") $
  T.union [
    "merge">:
      doc "Merge-copy the source tree's files into the destination, leaving pre-existing files intact"
      T.unit,
    "wipeThenCopy">:
      doc "Remove the destination tree entirely, then copy the source tree (an exact-mirror replacement)"
      T.unit,
    "symlinkTree">:
      doc "Symlink each of the source tree's files into the destination (per-file links; own real files win)"
      T.unit,
    "symlinkDir">:
      doc "Symlink the source directory itself into the destination as a single directory link"
      T.unit]

assemblyPlan :: TypeDefinition
assemblyPlan = define "AssemblyPlan" $
  doc ("The complete assembly plan for one distribution package in one target language: the ordered"
    ++ " copy/symlink entries to realize and the keep-paths a pruning consumer must protect."
    ++ " Entries are ordered because realization order can matter (e.g. a package that links its own"
    ++ " subtree back into an already-assembled dependency's tree).") $
  T.record [
    "entries">:
      doc "The copy/symlink entries to realize, in application order"
      (T.list assemblyEntry),
    "keepPaths">:
      doc "The files this plan is responsible for, recorded so a prune-stale pass does not delete them"
      (T.list keepPathEntry)]

keepPathEntry :: TypeDefinition
keepPathEntry = define "KeepPathEntry" $
  doc ("One protected path in a keep-paths manifest: a source-set directory paired with a file's path"
    ++ " relative to it. Serialized by the native executor as <sourceSetDir>\\t<relPath>, bit-compatible"
    ++ " with the prune-stale (#357) and digest-check --keep-paths-from consumers.") $
  T.record [
    "sourceSetDir">:
      doc "The source-set directory the file belongs to, as a worktree-relative path"
      T.string,
    "relPath">:
      doc "The file's path relative to its source-set directory"
      T.string]
