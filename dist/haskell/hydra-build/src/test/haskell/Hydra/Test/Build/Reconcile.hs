-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for the pure orphan-reconcile decision

module Hydra.Test.Build.Reconcile where

import qualified Hydra.Ast as Ast
import qualified Hydra.Build.Reconcile as Reconcile
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Docs as Docs
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.File as ErrorFile
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Error.System as ErrorSystem
import qualified Hydra.Errors as Errors
import qualified Hydra.File as File
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Overlay.Haskell.Lib.Literals as Literals
import qualified Hydra.Overlay.Haskell.Lib.Sets as Sets
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.System as System
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Time as Time
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Test cases for the pure orphan-reconcile decision
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "reconcile",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "prefix-scoped (Java/Python)",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "deletes only the stale owned .json file",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) (Reconcile.reconcilePlan (Sets.fromList [
                  "hydra/java/coder.json",
                  "hydra/java/module.json"]) (Sets.fromList [
                  "manifest.json"]) (Just (Sets.fromList [
                  "json"])) (Just (Sets.fromList [
                  "hydra/java"])) [
                  "hydra/java/coder.json",
                  "hydra/java/module.json",
                  "hydra/java/stale.json",
                  "hydra/dsl/java/coder.json",
                  "manifest.json",
                  "hydra/java/notes.txt"])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) [
                  "hydra/java/stale.json"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty keep-set with prefix restriction still deletes only owned .json",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) (Reconcile.reconcilePlan (Sets.fromList []) (Sets.fromList [
                  "manifest.json"]) (Just (Sets.fromList [
                  "json"])) (Just (Sets.fromList [
                  "hydra/java"])) [
                  "hydra/java/coder.json",
                  "hydra/java/module.json",
                  "hydra/java/stale.json",
                  "hydra/dsl/java/coder.json",
                  "manifest.json",
                  "hydra/java/notes.txt"])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) [
                  "hydra/java/coder.json",
                  "hydra/java/module.json",
                  "hydra/java/stale.json"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "segment-wise prefix match rejects hydra/javax sibling",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) (Reconcile.reconcilePlan (Sets.fromList [
                  "hydra/java/coder.json"]) (Sets.fromList []) (Just (Sets.fromList [
                  "json"])) (Just (Sets.fromList [
                  "hydra/java"])) [
                  "hydra/javax/other.json",
                  "hydra/java/stale.json"])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) [
                  "hydra/java/stale.json"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "whole-tree (Haskell)",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "deletes every non-kept, non-protected file regardless of ext or prefix",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) (Reconcile.reconcilePlan (Sets.fromList [
                  "hydra/java/coder.json",
                  "hydra/java/module.json"]) (Sets.fromList [
                  "manifest.json"]) Nothing Nothing [
                  "hydra/java/coder.json",
                  "hydra/java/module.json",
                  "hydra/java/stale.json",
                  "hydra/dsl/java/coder.json",
                  "manifest.json",
                  "hydra/java/notes.txt"])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) [
                  "hydra/java/stale.json",
                  "hydra/dsl/java/coder.json",
                  "hydra/java/notes.txt"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "protected manifest survives even when absent from keep-set",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) (Reconcile.reconcilePlan (Sets.fromList []) (Sets.fromList [
                  "manifest.json"]) Nothing Nothing [
                  "manifest.json",
                  "hydra/java/stale.json"])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) [
                  "hydra/java/stale.json"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "edge cases",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty keep-set + prefix restriction yields empty plan when nothing observed under prefix",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) (Reconcile.reconcilePlan (Sets.fromList []) (Sets.fromList []) (Just (Sets.fromList [
                  "json"])) (Just (Sets.fromList [
                  "hydra/java"])) [
                  "hydra/scala/coder.json",
                  "manifest.json"])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "protect exact-match: sibling with same basename in another dir is NOT protected",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) (Reconcile.reconcilePlan (Sets.fromList []) (Sets.fromList [
                  "manifest.json"]) Nothing Nothing [
                  "manifest.json",
                  "sub/manifest.json"])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) [
                  "sub/manifest.json"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "deep nesting no-op: everything kept => empty plan",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) (Reconcile.reconcilePlan (Sets.fromList [
                  "a/b/c/d/e/f.json"]) (Sets.fromList []) Nothing Nothing [
                  "a/b/c/d/e/f.json"])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "restriction contrast (none vs some-empty)",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "restrictPrefixes none deletes all non-protected",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) (Reconcile.reconcilePlan (Sets.fromList []) (Sets.fromList [
                  "manifest.json"]) Nothing Nothing [
                  "hydra/java/a.json",
                  "hydra/scala/b.txt",
                  "manifest.json"])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) [
                  "hydra/java/a.json",
                  "hydra/scala/b.txt"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "restrictPrefixes some-empty deletes nothing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) (Reconcile.reconcilePlan (Sets.fromList []) (Sets.fromList [
                  "manifest.json"]) Nothing (Just (Sets.fromList [])) [
                  "hydra/java/a.json",
                  "hydra/scala/b.txt",
                  "manifest.json"])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "restrictExts none admits all files",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) (Reconcile.reconcilePlan (Sets.fromList []) (Sets.fromList []) Nothing Nothing [
                  "a.json",
                  "b.txt",
                  "README"])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) [
                  "a.json",
                  "b.txt",
                  "README"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "restrictExts some-empty admits nothing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) (Reconcile.reconcilePlan (Sets.fromList []) (Sets.fromList []) (Just (Sets.fromList [])) Nothing [
                  "a.json",
                  "b.txt",
                  "README"])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "module-name helpers",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "keepPathsForModules maps each module to its .json path",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\s -> ShowCore.set (\s2 -> Literals.showString s2) s) (Reconcile.keepPathsForModules [
                  Packaging.ModuleName "hydra.java.coder",
                  (Packaging.ModuleName "hydra.java.module")])),
                Testing.universalTestCaseExpected = (\_ -> (\s -> ShowCore.set (\s2 -> Literals.showString s2) s) (Sets.fromList [
                  "hydra/java/coder.json",
                  "hydra/java/module.json"]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "keepPathsForModules [] => empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\s -> ShowCore.set (\s2 -> Literals.showString s2) s) (Reconcile.keepPathsForModules [])),
                Testing.universalTestCaseExpected = (\_ -> (\s -> ShowCore.set (\s2 -> Literals.showString s2) s) (Sets.fromList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "ownedPrefixes derives second-level dirs, deduped",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\s -> ShowCore.set (\s2 -> Literals.showString s2) s) (Reconcile.ownedPrefixes [
                  Packaging.ModuleName "hydra.java.coder",
                  (Packaging.ModuleName "hydra.java.module"),
                  (Packaging.ModuleName "hydra.python.coder")])),
                Testing.universalTestCaseExpected = (\_ -> (\s -> ShowCore.set (\s2 -> Literals.showString s2) s) (Sets.fromList [
                  "hydra/java",
                  "hydra/python"]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "ownedPrefixes [] => empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\s -> ShowCore.set (\s2 -> Literals.showString s2) s) (Reconcile.ownedPrefixes [])),
                Testing.universalTestCaseExpected = (\_ -> (\s -> ShowCore.set (\s2 -> Literals.showString s2) s) (Sets.fromList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "ownedPrefixes drops single-segment modules (no second-level dir)",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\s -> ShowCore.set (\s2 -> Literals.showString s2) s) (Reconcile.ownedPrefixes [
                  Packaging.ModuleName "hydra",
                  (Packaging.ModuleName "hydra.java.coder")])),
                Testing.universalTestCaseExpected = (\_ -> (\s -> ShowCore.set (\s2 -> Literals.showString s2) s) (Sets.fromList [
                  "hydra/java"]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
