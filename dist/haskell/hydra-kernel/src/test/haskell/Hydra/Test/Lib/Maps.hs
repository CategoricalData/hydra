-- Note: this is an automatically generated file. Do not edit.
-- | Test cases for hydra.lib.maps primitives

module Hydra.Test.Lib.Maps where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Chars as Chars
import qualified Hydra.Haskell.Lib.Equality as Equality
import qualified Hydra.Haskell.Lib.Literals as Literals
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Math as Math
import qualified Hydra.Haskell.Lib.Maybes as Maybes
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Query as Query
import qualified Hydra.Reduction as Reduction
import qualified Hydra.Relational as Relational
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | Test cases for hydra.lib.maps primitives
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "hydra.lib.maps primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "alter",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "insert new key",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.alter (\_2 -> Just "new") 3 (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "new")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "update existing key",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.alter (\_2 -> Just "updated") 2 (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a"),
                  (2, "updated")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "delete key",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.alter (\_2 -> Nothing) 2 (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "bimap",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "transform both",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.bimap (\k -> Math.mul k 2) (\v -> Strings.toUpper v) (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (2, "A"),
                  (4, "B")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.bimap (\k -> Math.mul k 2) (\v -> Strings.toUpper v) M.empty)),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) M.empty)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "elems",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "get all elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) (Maps.elems (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) [
                  "a",
                  "b"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unsorted keys",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) (Maps.elems (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "c")]))),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) [
                  "a",
                  "b",
                  "c"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) (Maps.elems M.empty)),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "empty",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) Maps.empty),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) M.empty)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "filter",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "filter values starting with a",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.filter (\v -> Equality.equal (Maybes.fromMaybe 0 (Maybes.map (\c -> Chars.toLower c) (Strings.maybeCharAt 0 v))) 97) (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "ab")]))),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a"),
                  (3, "ab")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "filter all",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.filter (\v -> Equality.equal (Maybes.fromMaybe 0 (Maybes.map (\c -> Chars.toLower c) (Strings.maybeCharAt 0 v))) 97) (M.fromList [
                  (1, "b"),
                  (2, "c")]))),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) M.empty)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.filter (\v -> Equality.equal (Maybes.fromMaybe 0 (Maybes.map (\c -> Chars.toLower c) (Strings.maybeCharAt 0 v))) 97) M.empty)),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) M.empty)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "filterWithKey",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "filter by key > 1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.filterWithKey (\k -> \v -> Equality.gt k 1) (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "c")]))),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (2, "b"),
                  (3, "c")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "filter all",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.filterWithKey (\k -> \v -> Equality.gt k 1) (M.fromList [
                  (1, "a")]))),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) M.empty)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.filterWithKey (\k -> \v -> Equality.gt k 1) M.empty)),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) M.empty)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "findWithDefault",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "find existing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\s -> s) (Maps.findWithDefault "default" 2 (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = (\_ -> (\s -> s) "b")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "use default",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\s -> s) (Maps.findWithDefault "default" 3 (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = (\_ -> (\s -> s) "default")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "fromList",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "create from pairs",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.fromList [
                  (1, "a"),
                  (2, "b")])),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a"),
                  (2, "b")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "duplicate keys",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.fromList [
                  (1, "a"),
                  (1, "b")])),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "b")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.fromList [])),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) M.empty)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "insert",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "insert new key",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.insert 3 "c" (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "c")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "update existing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.insert 2 "updated" (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a"),
                  (2, "updated")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "insert into empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.insert 1 "x" M.empty)),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "x")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "keys",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "get all keys",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (Maps.keys (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "c")]))),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) [
                  1,
                  2,
                  3])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unsorted keys",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (Maps.keys (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "c")]))),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) [
                  1,
                  2,
                  3])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (Maps.keys M.empty)),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "lookup",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "find existing key",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\mx -> ShowCore.maybe (\s -> Literals.showString s) mx) (Maps.lookup 2 (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = (\_ -> (\mx -> ShowCore.maybe (\s -> Literals.showString s) mx) (Just "b"))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "key not found",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\mx -> ShowCore.maybe (\s -> Literals.showString s) mx) (Maps.lookup 3 (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = (\_ -> (\mx -> ShowCore.maybe (\s -> Literals.showString s) mx) Nothing)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lookup in empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\mx -> ShowCore.maybe (\s -> Literals.showString s) mx) (Maps.lookup 1 M.empty)),
                Testing.universalTestCaseExpected = (\_ -> (\mx -> ShowCore.maybe (\s -> Literals.showString s) mx) Nothing)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "map",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "map over values",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.map (\s -> Strings.toUpper s) (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "A"),
                  (2, "B")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "map empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.map (\s -> Strings.toUpper s) M.empty)),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) M.empty)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "mapKeys",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "double keys",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.mapKeys (\k -> Math.mul k 2) (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (2, "a"),
                  (4, "b")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.mapKeys (\k -> Math.mul k 2) M.empty)),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) M.empty)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "member",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "key exists",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Maps.member 2 (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "key missing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Maps.member 3 (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Maps.member 1 M.empty)),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "null",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Maps.null M.empty)),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "non-empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Maps.null (M.fromList [
                  (1, "a")]))),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "remove",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "remove existing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.delete 2 (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "c")]))),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a"),
                  (3, "c")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "remove non-existing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.delete 4 (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a"),
                  (2, "b")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "remove from empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.delete 1 M.empty)),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) M.empty)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "singleton",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single entry",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.singleton 42 "hello")),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (42, "hello")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "size",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "three entries",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\n -> Literals.showInt32 n) (Maps.size (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "c")]))),
                Testing.universalTestCaseExpected = (\_ -> (\n -> Literals.showInt32 n) 3)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single entry",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\n -> Literals.showInt32 n) (Maps.size (M.fromList [
                  (42, "test")]))),
                Testing.universalTestCaseExpected = (\_ -> (\n -> Literals.showInt32 n) 1)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\n -> Literals.showInt32 n) (Maps.size M.empty)),
                Testing.universalTestCaseExpected = (\_ -> (\n -> Literals.showInt32 n) 0)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "toList",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "convert to pairs",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\p -> ShowCore.pair (\n -> Literals.showInt32 n) (\s -> Literals.showString s) p) xs) (Maps.toList (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\p -> ShowCore.pair (\n -> Literals.showInt32 n) (\s -> Literals.showString s) p) xs) [
                  (1, "a"),
                  (2, "b")])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unsorted keys",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\p -> ShowCore.pair (\n -> Literals.showInt32 n) (\s -> Literals.showString s) p) xs) (Maps.toList (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "c")]))),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\p -> ShowCore.pair (\n -> Literals.showInt32 n) (\s -> Literals.showString s) p) xs) [
                  (1, "a"),
                  (2, "b"),
                  (3, "c")])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\p -> ShowCore.pair (\n -> Literals.showInt32 n) (\s -> Literals.showString s) p) xs) (Maps.toList M.empty)),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\p -> ShowCore.pair (\n -> Literals.showInt32 n) (\s -> Literals.showString s) p) xs) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "union",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "union two maps",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.union (M.fromList [
                  (1, "a"),
                  (2, "b")]) (M.fromList [
                  (2, "x"),
                  (3, "c")]))),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "c")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "union with empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.union (M.fromList [
                  (1, "a")]) M.empty)),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty with map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.union M.empty (M.fromList [
                  (1, "a")]))),
                Testing.universalTestCaseExpected = (\_ -> (\m -> ShowCore.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
