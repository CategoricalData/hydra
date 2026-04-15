-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for hydra.lib.maps primitives

module Hydra.Test.Lib.Maps where

import qualified Hydra.Lib.Chars as Chars
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as Core
import qualified Hydra.Testing as Testing
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
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.alter (\_ -> Just "new") 3 (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "new")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "update existing key",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.alter (\_ -> Just "updated") 2 (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a"),
                  (2, "updated")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "delete key",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.alter (\_ -> Nothing) 2 (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
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
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.bimap (\k -> Math.mul k 2) (\v -> Strings.toUpper v) (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (2, "A"),
                  (4, "B")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.bimap (\k -> Math.mul k 2) (\v -> Strings.toUpper v) M.empty)),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) M.empty)})),
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
                Testing.universalTestCaseActual = ((\xs -> Core.list (\s -> Literals.showString s) xs) (Maps.elems (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = ((\xs -> Core.list (\s -> Literals.showString s) xs) [
                  "a",
                  "b"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unsorted keys",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Core.list (\s -> Literals.showString s) xs) (Maps.elems (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "c")]))),
                Testing.universalTestCaseExpected = ((\xs -> Core.list (\s -> Literals.showString s) xs) [
                  "a",
                  "b",
                  "c"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Core.list (\s -> Literals.showString s) xs) (Maps.elems M.empty)),
                Testing.universalTestCaseExpected = ((\xs -> Core.list (\s -> Literals.showString s) xs) [])})),
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
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) Maps.empty),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) M.empty)})),
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
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.filter (\v -> Equality.equal (Chars.toLower (Strings.charAt 0 v)) 97) (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "ab")]))),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a"),
                  (3, "ab")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "filter all",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.filter (\v -> Equality.equal (Chars.toLower (Strings.charAt 0 v)) 97) (M.fromList [
                  (1, "b"),
                  (2, "c")]))),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) M.empty)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.filter (\v -> Equality.equal (Chars.toLower (Strings.charAt 0 v)) 97) M.empty)),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) M.empty)})),
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
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.filterWithKey (\k -> \v -> Equality.gt k 1) (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "c")]))),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (2, "b"),
                  (3, "c")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "filter all",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.filterWithKey (\k -> \v -> Equality.gt k 1) (M.fromList [
                  (1, "a")]))),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) M.empty)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.filterWithKey (\k -> \v -> Equality.gt k 1) M.empty)),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) M.empty)})),
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
                Testing.universalTestCaseActual = ((\s -> s) (Maps.findWithDefault "default" 2 (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = ((\s -> s) "b")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "use default",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Maps.findWithDefault "default" 3 (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = ((\s -> s) "default")})),
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
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.fromList [
                  (1, "a"),
                  (2, "b")])),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a"),
                  (2, "b")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "duplicate keys",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.fromList [
                  (1, "a"),
                  (1, "b")])),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "b")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.fromList [])),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) M.empty)})),
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
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.insert 3 "c" (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "c")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "update existing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.insert 2 "updated" (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a"),
                  (2, "updated")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "insert into empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.insert 1 "x" M.empty)),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
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
                Testing.universalTestCaseActual = ((\xs -> Core.list (\n -> Literals.showInt32 n) xs) (Maps.keys (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "c")]))),
                Testing.universalTestCaseExpected = ((\xs -> Core.list (\n -> Literals.showInt32 n) xs) [
                  1,
                  2,
                  3])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unsorted keys",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Core.list (\n -> Literals.showInt32 n) xs) (Maps.keys (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "c")]))),
                Testing.universalTestCaseExpected = ((\xs -> Core.list (\n -> Literals.showInt32 n) xs) [
                  1,
                  2,
                  3])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Core.list (\n -> Literals.showInt32 n) xs) (Maps.keys M.empty)),
                Testing.universalTestCaseExpected = ((\xs -> Core.list (\n -> Literals.showInt32 n) xs) [])})),
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
                Testing.universalTestCaseActual = ((\mx -> Core.maybe (\s -> Literals.showString s) mx) (Maps.lookup 2 (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = ((\mx -> Core.maybe (\s -> Literals.showString s) mx) (Just "b"))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "key not found",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\mx -> Core.maybe (\s -> Literals.showString s) mx) (Maps.lookup 3 (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = ((\mx -> Core.maybe (\s -> Literals.showString s) mx) Nothing)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lookup in empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\mx -> Core.maybe (\s -> Literals.showString s) mx) (Maps.lookup 1 M.empty)),
                Testing.universalTestCaseExpected = ((\mx -> Core.maybe (\s -> Literals.showString s) mx) Nothing)})),
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
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.map (\s -> Strings.toUpper s) (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "A"),
                  (2, "B")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "map empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.map (\s -> Strings.toUpper s) M.empty)),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) M.empty)})),
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
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.mapKeys (\k -> Math.mul k 2) (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (2, "a"),
                  (4, "b")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.mapKeys (\k -> Math.mul k 2) M.empty)),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) M.empty)})),
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
                Testing.universalTestCaseActual = ((\b -> Literals.showBoolean b) (Maps.member 2 (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = ((\b -> Literals.showBoolean b) True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "key missing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\b -> Literals.showBoolean b) (Maps.member 3 (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = ((\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\b -> Literals.showBoolean b) (Maps.member 1 M.empty)),
                Testing.universalTestCaseExpected = ((\b -> Literals.showBoolean b) False)})),
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
                Testing.universalTestCaseActual = ((\b -> Literals.showBoolean b) (Maps.null M.empty)),
                Testing.universalTestCaseExpected = ((\b -> Literals.showBoolean b) True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "non-empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\b -> Literals.showBoolean b) (Maps.null (M.fromList [
                  (1, "a")]))),
                Testing.universalTestCaseExpected = ((\b -> Literals.showBoolean b) False)})),
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
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.delete 2 (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "c")]))),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a"),
                  (3, "c")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "remove non-existing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.delete 4 (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a"),
                  (2, "b")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "remove from empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.delete 1 M.empty)),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) M.empty)})),
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
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.singleton 42 "hello")),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
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
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Maps.size (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "c")]))),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 3)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single entry",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Maps.size (M.fromList [
                  (42, "test")]))),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 1)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Maps.size M.empty)),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 0)})),
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
                Testing.universalTestCaseActual = ((\xs -> Core.list (\p -> Core.pair (\n -> Literals.showInt32 n) (\s -> Literals.showString s) p) xs) (Maps.toList (M.fromList [
                  (1, "a"),
                  (2, "b")]))),
                Testing.universalTestCaseExpected = ((\xs -> Core.list (\p -> Core.pair (\n -> Literals.showInt32 n) (\s -> Literals.showString s) p) xs) [
                  (1, "a"),
                  (2, "b")])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unsorted keys",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Core.list (\p -> Core.pair (\n -> Literals.showInt32 n) (\s -> Literals.showString s) p) xs) (Maps.toList (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "c")]))),
                Testing.universalTestCaseExpected = ((\xs -> Core.list (\p -> Core.pair (\n -> Literals.showInt32 n) (\s -> Literals.showString s) p) xs) [
                  (1, "a"),
                  (2, "b"),
                  (3, "c")])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Core.list (\p -> Core.pair (\n -> Literals.showInt32 n) (\s -> Literals.showString s) p) xs) (Maps.toList M.empty)),
                Testing.universalTestCaseExpected = ((\xs -> Core.list (\p -> Core.pair (\n -> Literals.showInt32 n) (\s -> Literals.showString s) p) xs) [])})),
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
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.union (M.fromList [
                  (1, "a"),
                  (2, "b")]) (M.fromList [
                  (2, "x"),
                  (3, "c")]))),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a"),
                  (2, "b"),
                  (3, "c")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "union with empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.union (M.fromList [
                  (1, "a")]) M.empty)),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty with map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (Maps.union M.empty (M.fromList [
                  (1, "a")]))),
                Testing.universalTestCaseExpected = ((\m -> Core.map (\n -> Literals.showInt32 n) (\s -> Literals.showString s) m) (M.fromList [
                  (1, "a")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
