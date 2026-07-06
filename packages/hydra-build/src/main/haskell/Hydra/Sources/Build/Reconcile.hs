
module Hydra.Sources.Build.Reconcile where

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

import qualified Hydra.Sources.Kernel.Terms.Generation as Generation


-- | The pure orphan-reconcile decision for the Hydra packaging split (#530, #416 phase 2).
--
-- Given the on-disk observed relative paths (pre-filtered by the host driver: no
-- dotfiles, '/'-separated) and a description of what to keep and how to restrict the
-- scan, produce the deletion plan: the subset of observed paths that no longer
-- correspond to any generated module and are therefore safe to remove.
--
-- The function is extension- and prefix-agnostic: both existing host semantics are
-- expressed by parameter choices, not by branching on the host:
--
--   * Haskell whole-tree (Digest.reconcileOrphans): restrictExts = none,
--     restrictPrefixes = none, protectSet = {"manifest.json"} -- every observed file
--     not in the keep-set (and not protected) is an orphan.
--   * Java/Python prefix-scoped (Generation.java / generation.py): restrictExts =
--     {"json"}, restrictPrefixes = the owned second-level dirs -- only .json files
--     under an owned prefix are eligible; manifest.json survives by living outside
--     the owned prefixes.
--
-- What stays host-native (NOT promoted here): the recursive directory walk, dotfile
-- pre-filtering, the deletion itself + empty-dir pruning, and path normalization to
-- '/'. This module promotes the DECISION only.
ns :: ModuleName
ns = ModuleName "hydra.build.reconcile"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Generation.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "The pure orphan-reconcile decision: which generated files to delete")}
  where
   definitions = [
     toDefinition extensionOf,
     toDefinition isUnderAnyPrefix,
     toDefinition keepPathsForModules,
     toDefinition matchesExts,
     toDefinition matchesPrefixes,
     toDefinition ownedPrefixes,
     toDefinition reconcilePlan,
     toDefinition secondLevelDir]

-- | The extension of a relative path: the segment after the final '.' in the last
-- '/'-segment, or none if the filename has no '.'. E.g. "hydra/java/coder.json" ->
-- just "json"; "hydra/java/README" -> none; "a/b/.hidden" -> just "hidden" (dotfiles
-- are pre-filtered by the driver, so this edge case is not exercised in practice).
extensionOf :: TypedTermDefinition (String -> Maybe String)
extensionOf = define "extensionOf" $
  doc "The extension of a relative path, or none if the filename has no '.'" $
  "path" ~>
  "segs" <~ Strings.splitOn (string "/") (var "path") $
  Optionals.cases (Lists.maybeLast (var "segs")) nothing ("fileName" ~>
    "dotParts" <~ Strings.splitOn (string ".") (var "fileName") $
    Logic.ifElse (Equality.gt (Lists.length (var "dotParts")) (int32 1))
      (Lists.maybeLast (var "dotParts"))
      nothing)

-- | Whether a relative path lies under at least one of the given prefixes, matched
-- segment-wise so that prefix "hydra/java" matches "hydra/java/coder.json" but NOT
-- "hydra/javax/coder.json". A prefix matches iff its '/'-segment list is a prefix of
-- the path's '/'-segment list.
isUnderAnyPrefix :: TypedTermDefinition (S.Set String -> String -> Bool)
isUnderAnyPrefix = define "isUnderAnyPrefix" $
  doc "Whether a relative path lies under at least one prefix, matched segment-wise" $
  "prefixes" ~> "path" ~>
  "pathSegs" <~ Strings.splitOn (string "/") (var "path") $
  Lists.foldl
    ("acc" ~> "prefix" ~> Logic.or (var "acc")
      ("prefixSegs" <~ Strings.splitOn (string "/") (var "prefix") $
       Equality.equal
         (Lists.take (Lists.length (var "prefixSegs")) (var "pathSegs"))
         (var "prefixSegs")))
    false
    (Sets.toList (var "prefixes" :: TypedTerm (S.Set String)))

-- | The keep-set of relative JSON paths for a set of written module names:
-- moduleNameToPath(ns) ++ ".json" for each. This is the standard construction shared
-- by all hosts (a module hydra.java.coder keeps "hydra/java/coder.json").
keepPathsForModules :: TypedTermDefinition ([ModuleName] -> S.Set String)
keepPathsForModules = define "keepPathsForModules" $
  doc "The keep-set of relative JSON paths (moduleNameToPath(ns) ++ .json) for written modules" $
  "mods" ~>
  Sets.fromList (Lists.map
    ("m" ~> Strings.cat (list [Generation.moduleNameToPath @@ var "m", string ".json"]))
    (var "mods") :: TypedTerm [String])

-- | Whether a path's extension satisfies the optional extension restriction. A none
-- restriction admits every file (Haskell whole-tree semantics); a some restriction
-- admits only files whose extension is in the set (Java/Python .json-only semantics).
matchesExts :: TypedTermDefinition (Maybe (S.Set String) -> String -> Bool)
matchesExts = define "matchesExts" $
  doc "Whether a path's extension satisfies the optional extension restriction" $
  "restrictExts" ~> "path" ~>
  Optionals.cases (var "restrictExts" :: TypedTerm (Maybe (S.Set String))) true ("exts" ~>
    Optionals.cases (extensionOf @@ var "path") false ("ext" ~>
      Sets.member (var "ext") (var "exts" :: TypedTerm (S.Set String))))

-- | Whether a path satisfies the optional prefix restriction. A none restriction
-- admits every path (Haskell whole-tree semantics); a some restriction admits only
-- paths under one of the prefixes (Java/Python owned-prefix semantics).
matchesPrefixes :: TypedTermDefinition (Maybe (S.Set String) -> String -> Bool)
matchesPrefixes = define "matchesPrefixes" $
  doc "Whether a path satisfies the optional prefix restriction" $
  "restrictPrefixes" ~> "path" ~>
  Optionals.cases (var "restrictPrefixes" :: TypedTerm (Maybe (S.Set String))) true ("prefixes" ~>
    isUnderAnyPrefix @@ (var "prefixes" :: TypedTerm (S.Set String)) @@ var "path")

-- | The owned second-level prefixes derived from a set of written module names: the
-- second-level directory of each module's path (e.g. hydra.java.coder ->
-- "hydra/java"). This is the Java/Python prefix-scoped restriction. Modules whose path
-- has fewer than two segments yield no prefix (secondLevelDir returns none and is
-- dropped here), matching the Java/Python references' callers. An empty module set
-- yields an empty prefix set, which -- as a some(empty) restriction -- admits nothing,
-- preserving the "empty owned-prefix set => delete nothing" behavior.
ownedPrefixes :: TypedTermDefinition ([ModuleName] -> S.Set String)
ownedPrefixes = define "ownedPrefixes" $
  doc "The owned second-level prefixes (e.g. hydra/java) derived from written module names" $
  "mods" ~>
  Sets.fromList (Optionals.mapOptional
    ("m" ~> secondLevelDir @@ (Generation.moduleNameToPath @@ var "m"))
    (var "mods") :: TypedTerm [String])

-- | The pure orphan-reconcile plan: the subset of observed relative paths to delete.
--
-- A path is deleted iff it satisfies BOTH restrictions (extension and prefix) AND is
-- absent from the keep-set AND is absent from the protect-set. Observed order is
-- preserved in the output. Deletion, empty-dir pruning, and the directory walk are
-- the host driver's responsibility.
reconcilePlan :: TypedTermDefinition (
  S.Set String -> S.Set String -> Maybe (S.Set String) -> Maybe (S.Set String) -> [String] -> [String])
reconcilePlan = define "reconcilePlan" $
  doc "The pure orphan-reconcile plan: which observed paths to delete" $
  "keepSet" ~> "protectSet" ~> "restrictExts" ~> "restrictPrefixes" ~> "observed" ~>
  Lists.filter
    ("path" ~> Logic.and
      (Logic.and
        (matchesExts @@ (var "restrictExts" :: TypedTerm (Maybe (S.Set String))) @@ var "path")
        (matchesPrefixes @@ (var "restrictPrefixes" :: TypedTerm (Maybe (S.Set String))) @@ var "path"))
      (Logic.and
        (Logic.not $ Sets.member (var "path") (var "keepSet" :: TypedTerm (S.Set String)))
        (Logic.not $ Sets.member (var "path") (var "protectSet" :: TypedTerm (S.Set String)))))
    (var "observed")

-- | The second-level directory of a '/'-separated relative path: the first two
-- segments joined by '/' (e.g. "hydra/java/coder" -> just "hydra/java"). A path with
-- fewer than two segments has no second-level directory and yields none -- the
-- Java/Python references' callers drop such entries.
--
-- NOTE: this matches the shape of hydra.build.modules.secondLevelDir (#529), which was
-- not yet landed when #530 was authored. Because the signatures agree (Optional String,
-- none on <2 segments), the migration-time cleanup is a drop-in replacement, not a
-- signature reconciliation.
secondLevelDir :: TypedTermDefinition (String -> Maybe String)
secondLevelDir = define "secondLevelDir" $
  doc "The second-level directory of a '/'-separated path (e.g. hydra/java/coder -> hydra/java), or none" $
  "path" ~>
  "segs" <~ Strings.splitOn (string "/") (var "path") $
  Logic.ifElse (Equality.gte (Lists.length (var "segs")) (int32 2))
    (just $ Strings.intercalate (string "/") (Lists.take (int32 2) (var "segs")))
    nothing
