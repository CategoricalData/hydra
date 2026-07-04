-- | Test cases for the pure orphan-reconcile decision (hydra.build.reconcile, #530).
--
-- The tests pin BOTH existing host semantics side by side, so that a future
-- unification (a separate, deliberate decision) cannot silently drift either one:
--
--   * Java/Python prefix-scoped: restrictExts = {"json"}, restrictPrefixes = the
--     owned second-level dirs. Only .json files under an owned prefix are eligible.
--   * Haskell whole-tree: restrictExts = none, restrictPrefixes = none, protectSet =
--     {"manifest.json"}. Every observed file not kept and not protected is an orphan.
module Hydra.Sources.Test.Build.Reconcile where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core          as Core
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms hiding ((++))
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types         as T
import qualified Data.List                    as L
import qualified Data.Map                     as M
import qualified Data.Set                     as S

import Hydra.Testing
import qualified Hydra.Sources.Kernel.Terms.Build.Reconcile as Reconcile
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Sets     as Sets
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Dsl.Packaging    as DPackaging


ns :: ModuleName
ns = ModuleName "hydra.test.build.reconcile"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([Reconcile.ns, ShowCore.ns] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata ((Just "Test cases for the pure orphan-reconcile decision"))}
  where
    definitions = [Phantoms.toDefinition allTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    doc "Test cases for the pure orphan-reconcile decision" $
    supergroup "reconcile" [
      prefixScopedGroup,
      wholeTreeGroup,
      edgeCaseGroup,
      restrictionContrastGroup,
      helperGroup]

-- Helpers -----------------------------------------------------------------

-- | A set literal of strings.
strSet :: [String] -> TypedTerm (S.Set String)
strSet ss = Sets.fromList $ list (string <$> ss)

-- | none : no extension / prefix restriction.
noRestriction :: TypedTerm (Maybe (S.Set String))
noRestriction = nothing

-- | some restriction.
restrict :: [String] -> TypedTerm (Maybe (S.Set String))
restrict ss = just (strSet ss)

-- | Render a single string with quotes, for use in showStringList.
showString' :: TypedTerm (String -> String)
showString' = lambda "s" $ Literals.showString (var "s")

-- | Render a list of strings like "[\"a\", \"b\"]". universalCase compares the
-- serialized forms, so both actual and expected are rendered to String; passing a
-- bare [String] would leave the element type ambiguous during DSL inference.
showStringList :: TypedTerm ([String] -> String)
showStringList = lambda "xs" $ ShowCore.list_ @@ showString' @@ var "xs"

-- | Render a set of strings deterministically (ShowCore.set_ sorts), for comparing
-- the S.Set String results of keepPathsForModules / ownedPrefixes.
showStringSet :: TypedTerm (S.Set String -> String)
showStringSet = lambda "s" $ ShowCore.set_ @@ showString' @@ var "s"

-- | A list of ModuleName terms from dotted names (e.g. "hydra.java.coder").
moduleNames :: [String] -> TypedTerm [ModuleName]
moduleNames ns' = list (DPackaging.moduleName2 . string <$> ns')

-- | A set-result test case: compare a rendered S.Set String against expected.
setCase :: String -> TypedTerm (S.Set String) -> [String] -> TypedTerm TestCaseWithMetadata
setCase cname actual expected =
  universalCase cname
    (showStringSet @@ actual)
    (showStringSet @@ strSet expected)

-- | A reconcilePlan test case: compare the computed deletion plan against the
-- expected list. Observed order is preserved by reconcilePlan, so expected lists
-- are written in observed order.
planCase ::
  String ->
  [String] ->                    -- keep-set (relative paths)
  [String] ->                    -- protect-set
  TypedTerm (Maybe (S.Set String)) ->  -- restrictExts
  TypedTerm (Maybe (S.Set String)) ->  -- restrictPrefixes
  [String] ->                    -- observed
  [String] ->                    -- expected deletion plan
  TypedTerm TestCaseWithMetadata
planCase cname keep protect exts prefixes observed expected =
  universalCase cname
    (showStringList @@ (Reconcile.reconcilePlan
      @@ strSet keep
      @@ strSet protect
      @@ exts
      @@ prefixes
      @@ (list (string <$> observed) :: TypedTerm [String])))
    (showStringList @@ (list (string <$> expected) :: TypedTerm [String]))

-- The shared scenario from the issue: keep = the two written java modules, protect =
-- {manifest.json}, prefixes = {hydra/java}. The observed set mixes a stale owned
-- file, a foreign-prefix file, the manifest, and a non-JSON file.
scenarioKeep :: [String]
scenarioKeep = ["hydra/java/coder.json", "hydra/java/module.json"]

scenarioProtect :: [String]
scenarioProtect = ["manifest.json"]

scenarioPrefixes :: [String]
scenarioPrefixes = ["hydra/java"]

scenarioObserved :: [String]
scenarioObserved = [
  "hydra/java/coder.json",       -- kept
  "hydra/java/module.json",      -- kept
  "hydra/java/stale.json",       -- STALE owned .json
  "hydra/dsl/java/coder.json",   -- foreign-prefix .json
  "manifest.json",               -- protected sidecar
  "hydra/java/notes.txt"]        -- non-JSON under owned prefix

-- Test groups -------------------------------------------------------------

-- | Java/Python prefix-scoped semantics: restrictExts = {"json"}, restrictPrefixes =
-- {"hydra/java"}. Only the stale owned .json file is deleted; the foreign-prefix
-- file (wrong prefix), the manifest (protected AND outside prefix), and the .txt
-- file (wrong extension) all survive.
prefixScopedGroup :: TypedTerm TestGroup
prefixScopedGroup = subgroup "prefix-scoped (Java/Python)" [
    planCase "deletes only the stale owned .json file"
      scenarioKeep scenarioProtect (restrict ["json"]) (restrict scenarioPrefixes)
      scenarioObserved
      ["hydra/java/stale.json"],
    planCase "empty keep-set with prefix restriction still deletes only owned .json"
      [] scenarioProtect (restrict ["json"]) (restrict scenarioPrefixes)
      scenarioObserved
      ["hydra/java/coder.json", "hydra/java/module.json", "hydra/java/stale.json"],
    planCase "segment-wise prefix match rejects hydra/javax sibling"
      ["hydra/java/coder.json"] [] (restrict ["json"]) (restrict ["hydra/java"])
      ["hydra/javax/other.json", "hydra/java/stale.json"]
      ["hydra/java/stale.json"]]

-- | Haskell whole-tree semantics: no restrictions, protectSet = {"manifest.json"}.
-- Every observed file that is not kept and not protected is deleted -- including the
-- foreign-prefix file and the non-JSON file -- but the protected manifest never is.
wholeTreeGroup :: TypedTerm TestGroup
wholeTreeGroup = subgroup "whole-tree (Haskell)" [
    planCase "deletes every non-kept, non-protected file regardless of ext or prefix"
      scenarioKeep scenarioProtect noRestriction noRestriction
      scenarioObserved
      ["hydra/java/stale.json", "hydra/dsl/java/coder.json", "hydra/java/notes.txt"],
    planCase "protected manifest survives even when absent from keep-set"
      [] ["manifest.json"] noRestriction noRestriction
      ["manifest.json", "hydra/java/stale.json"]
      ["hydra/java/stale.json"]]

-- | Edge cases shared by both parameterizations.
edgeCaseGroup :: TypedTerm TestGroup
edgeCaseGroup = subgroup "edge cases" [
    planCase "empty keep-set + prefix restriction yields empty plan when nothing observed under prefix"
      [] [] (restrict ["json"]) (restrict ["hydra/java"])
      ["hydra/scala/coder.json", "manifest.json"]
      [],
    planCase "protect exact-match: sibling with same basename in another dir is NOT protected"
      [] ["manifest.json"] noRestriction noRestriction
      ["manifest.json", "sub/manifest.json"]
      ["sub/manifest.json"],
    planCase "deep nesting no-op: everything kept => empty plan"
      ["a/b/c/d/e/f.json"] [] noRestriction noRestriction
      ["a/b/c/d/e/f.json"]
      []]

-- | The none-vs-some(empty) restriction contrast on the SAME observed set: this is the
-- references' empty-guard, the invariant most likely to regress silently at migration.
-- none = admit everything; some(empty) = admit nothing. Verified for both the prefix
-- restriction and (for symmetry) the extension restriction.
restrictionContrastGroup :: TypedTerm TestGroup
restrictionContrastGroup = subgroup "restriction contrast (none vs some-empty)" [
    -- restrictPrefixes: none deletes everything non-protected...
    planCase "restrictPrefixes none deletes all non-protected"
      [] ["manifest.json"] noRestriction noRestriction
      ["hydra/java/a.json", "hydra/scala/b.txt", "manifest.json"]
      ["hydra/java/a.json", "hydra/scala/b.txt"],
    -- ...while some(empty) deletes NOTHING (no prefix can match).
    planCase "restrictPrefixes some-empty deletes nothing"
      [] ["manifest.json"] noRestriction (restrict [])
      ["hydra/java/a.json", "hydra/scala/b.txt", "manifest.json"]
      [],
    -- restrictExts symmetry: none admits all files...
    planCase "restrictExts none admits all files"
      [] [] noRestriction noRestriction
      ["a.json", "b.txt", "README"]
      ["a.json", "b.txt", "README"],
    -- ...while some(empty) admits nothing (no extension can match).
    planCase "restrictExts some-empty admits nothing"
      [] [] (restrict []) noRestriction
      ["a.json", "b.txt", "README"]
      []]

-- | Direct tests for the module-name-driven helpers the issue names as promoted:
-- keepPathsForModules (moduleNameToPath ++ ".json") and ownedPrefixes (second-level
-- dirs, single-segment modules dropped, empty input => empty set).
helperGroup :: TypedTerm TestGroup
helperGroup = subgroup "module-name helpers" [
    setCase "keepPathsForModules maps each module to its .json path"
      (Reconcile.keepPathsForModules @@ moduleNames ["hydra.java.coder", "hydra.java.module"])
      ["hydra/java/coder.json", "hydra/java/module.json"],
    setCase "keepPathsForModules [] => empty"
      (Reconcile.keepPathsForModules @@ moduleNames [])
      [],
    setCase "ownedPrefixes derives second-level dirs, deduped"
      (Reconcile.ownedPrefixes @@ moduleNames ["hydra.java.coder", "hydra.java.module", "hydra.python.coder"])
      ["hydra/java", "hydra/python"],
    setCase "ownedPrefixes [] => empty"
      (Reconcile.ownedPrefixes @@ moduleNames [])
      [],
    setCase "ownedPrefixes drops single-segment modules (no second-level dir)"
      (Reconcile.ownedPrefixes @@ moduleNames ["hydra", "hydra.java.coder"])
      ["hydra/java"]]
