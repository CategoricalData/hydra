-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for hydra.lib.sets primitives

module Hydra.Test.Lib.Sets where

import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Show.Core as Core
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Set as S

-- | Test cases for hydra.lib.sets primitives
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "hydra.lib.sets primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "empty",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty set",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) Sets.empty),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) S.empty)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "singleton",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.singleton 42)),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (S.fromList [
                  42]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "fromList",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "create from list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.fromList [
                  1,
                  2,
                  3])),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (S.fromList [
                  1,
                  2,
                  3]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "duplicates removed",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.fromList [
                  1,
                  2,
                  1,
                  3])),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (S.fromList [
                  1,
                  2,
                  3]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.fromList [])),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) S.empty)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "toList",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "convert to list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Core.list (\n -> Literals.showInt32 n) xs) (Sets.toList (S.fromList [
                  1,
                  2,
                  3]))),
                Testing.universalTestCaseExpected = ((\xs -> Core.list (\n -> Literals.showInt32 n) xs) [
                  1,
                  2,
                  3])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unsorted input",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Core.list (\n -> Literals.showInt32 n) xs) (Sets.toList (S.fromList [
                  1,
                  2,
                  3]))),
                Testing.universalTestCaseExpected = ((\xs -> Core.list (\n -> Literals.showInt32 n) xs) [
                  1,
                  2,
                  3])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty set",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Core.list (\n -> Literals.showInt32 n) xs) (Sets.toList S.empty)),
                Testing.universalTestCaseExpected = ((\xs -> Core.list (\n -> Literals.showInt32 n) xs) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "insert",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "insert new element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.insert 4 (S.fromList [
                  1,
                  2,
                  3]))),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (S.fromList [
                  1,
                  2,
                  3,
                  4]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "insert existing element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.insert 2 (S.fromList [
                  1,
                  2,
                  3]))),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (S.fromList [
                  1,
                  2,
                  3]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "insert into empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.insert 1 S.empty)),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (S.fromList [
                  1]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "delete",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "delete existing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.delete 2 (S.fromList [
                  1,
                  2,
                  3]))),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (S.fromList [
                  1,
                  3]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "delete non-existing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.delete 4 (S.fromList [
                  1,
                  2,
                  3]))),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (S.fromList [
                  1,
                  2,
                  3]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "delete from empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.delete 1 S.empty)),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) S.empty)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "member",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "element exists",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\b -> Literals.showBoolean b) (Sets.member 2 (S.fromList [
                  1,
                  2,
                  3]))),
                Testing.universalTestCaseExpected = ((\b -> Literals.showBoolean b) True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "element missing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\b -> Literals.showBoolean b) (Sets.member 4 (S.fromList [
                  1,
                  2,
                  3]))),
                Testing.universalTestCaseExpected = ((\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty set",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\b -> Literals.showBoolean b) (Sets.member 1 S.empty)),
                Testing.universalTestCaseExpected = ((\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "size",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "three elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Sets.size (S.fromList [
                  1,
                  2,
                  3]))),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 3)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Sets.size (S.fromList [
                  42]))),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 1)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty set",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Sets.size S.empty)),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 0)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "null",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty set",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\b -> Literals.showBoolean b) (Sets.null S.empty)),
                Testing.universalTestCaseExpected = ((\b -> Literals.showBoolean b) True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "non-empty set",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\b -> Literals.showBoolean b) (Sets.null (S.fromList [
                  1,
                  2]))),
                Testing.universalTestCaseExpected = ((\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "union",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "union two sets",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.union (S.fromList [
                  1,
                  2]) (S.fromList [
                  2,
                  3]))),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (S.fromList [
                  1,
                  2,
                  3]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "union with empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.union (S.fromList [
                  1,
                  2]) S.empty)),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (S.fromList [
                  1,
                  2]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty with non-empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.union S.empty (S.fromList [
                  1,
                  2]))),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (S.fromList [
                  1,
                  2]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "unions",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "union of multiple sets",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.unions [
                  S.fromList [
                    1,
                    2],
                  (S.fromList [
                    2,
                    3]),
                  (S.fromList [
                    3,
                    4])])),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (S.fromList [
                  1,
                  2,
                  3,
                  4]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "union with empty sets",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.unions [
                  S.fromList [
                    1,
                    2],
                  S.empty,
                  (S.fromList [
                    3])])),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (S.fromList [
                  1,
                  2,
                  3]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list of sets",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.unions [])),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) S.empty)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single set",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.unions [
                  S.fromList [
                    1,
                    2,
                    3]])),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (S.fromList [
                  1,
                  2,
                  3]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "intersection",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "common elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.intersection (S.fromList [
                  1,
                  2,
                  3]) (S.fromList [
                  2,
                  3,
                  4]))),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (S.fromList [
                  2,
                  3]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "no common elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.intersection (S.fromList [
                  1,
                  2]) (S.fromList [
                  3,
                  4]))),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) S.empty)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "intersection with empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.intersection (S.fromList [
                  1,
                  2]) S.empty)),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) S.empty)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "difference",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "remove elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.difference (S.fromList [
                  1,
                  2,
                  3]) (S.fromList [
                  2,
                  4]))),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (S.fromList [
                  1,
                  3]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "no overlap",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.difference (S.fromList [
                  1,
                  2]) (S.fromList [
                  3,
                  4]))),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (S.fromList [
                  1,
                  2]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "difference with empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.difference (S.fromList [
                  1,
                  2]) S.empty)),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (S.fromList [
                  1,
                  2]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "map",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "map function",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.map (\x -> Math.mul x 2) (S.fromList [
                  1,
                  2,
                  3]))),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (S.fromList [
                  2,
                  4,
                  6]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "map on empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> Core.set (\n -> Literals.showInt32 n) s) (Sets.map (\x -> Math.mul x 2) S.empty)),
                Testing.universalTestCaseExpected = ((\s -> Core.set (\n -> Literals.showInt32 n) s) S.empty)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
