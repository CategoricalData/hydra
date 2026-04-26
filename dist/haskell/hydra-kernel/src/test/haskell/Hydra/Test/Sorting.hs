-- Note: this is an automatically generated file. Do not edit.
-- | Test cases for topological sorting algorithms

module Hydra.Test.Sorting where
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as Core
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Test cases for topological sorting
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "sorting",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "topological sort",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty set",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\cs -> Strings.cat2 "left(" (Strings.cat2 (Core.list (Core.list Literals.showInt32) cs) ")")) (\xs -> Strings.cat2 "right(" (Strings.cat2 (Core.list Literals.showInt32 xs) ")")) (Sorting.topologicalSort [])),
                Testing.universalTestCaseExpected = (Eithers.either (\cs -> Strings.cat2 "left(" (Strings.cat2 (Core.list (Core.list Literals.showInt32) cs) ")")) (\xs -> Strings.cat2 "right(" (Strings.cat2 (Core.list Literals.showInt32 xs) ")")) (Right []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "singleton set",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\cs -> Strings.cat2 "left(" (Strings.cat2 (Core.list (Core.list Literals.showInt32) cs) ")")) (\xs -> Strings.cat2 "right(" (Strings.cat2 (Core.list Literals.showInt32 xs) ")")) (Sorting.topologicalSort [
                  (1, [])])),
                Testing.universalTestCaseExpected = (Eithers.either (\cs -> Strings.cat2 "left(" (Strings.cat2 (Core.list (Core.list Literals.showInt32) cs) ")")) (\xs -> Strings.cat2 "right(" (Strings.cat2 (Core.list Literals.showInt32 xs) ")")) (Right [
                  1]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "discrete set with multiple elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\cs -> Strings.cat2 "left(" (Strings.cat2 (Core.list (Core.list Literals.showInt32) cs) ")")) (\xs -> Strings.cat2 "right(" (Strings.cat2 (Core.list Literals.showInt32 xs) ")")) (Sorting.topologicalSort [
                  (3, []),
                  (1, []),
                  (2, [])])),
                Testing.universalTestCaseExpected = (Eithers.either (\cs -> Strings.cat2 "left(" (Strings.cat2 (Core.list (Core.list Literals.showInt32) cs) ")")) (\xs -> Strings.cat2 "right(" (Strings.cat2 (Core.list Literals.showInt32 xs) ")")) (Right [
                  1,
                  2,
                  3]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "linked list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\cs -> Strings.cat2 "left(" (Strings.cat2 (Core.list (Core.list Literals.showInt32) cs) ")")) (\xs -> Strings.cat2 "right(" (Strings.cat2 (Core.list Literals.showInt32 xs) ")")) (Sorting.topologicalSort [
                  (3, [
                    1]),
                  (2, [
                    3]),
                  (1, [])])),
                Testing.universalTestCaseExpected = (Eithers.either (\cs -> Strings.cat2 "left(" (Strings.cat2 (Core.list (Core.list Literals.showInt32) cs) ")")) (\xs -> Strings.cat2 "right(" (Strings.cat2 (Core.list Literals.showInt32 xs) ")")) (Right [
                  1,
                  3,
                  2]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "binary tree",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\cs -> Strings.cat2 "left(" (Strings.cat2 (Core.list (Core.list Literals.showInt32) cs) ")")) (\xs -> Strings.cat2 "right(" (Strings.cat2 (Core.list Literals.showInt32 xs) ")")) (Sorting.topologicalSort [
                  (3, [
                    1,
                    4]),
                  (4, [
                    6,
                    2]),
                  (1, [
                    5]),
                  (2, []),
                  (6, []),
                  (5, [])])),
                Testing.universalTestCaseExpected = (Eithers.either (\cs -> Strings.cat2 "left(" (Strings.cat2 (Core.list (Core.list Literals.showInt32) cs) ")")) (\xs -> Strings.cat2 "right(" (Strings.cat2 (Core.list Literals.showInt32 xs) ")")) (Right [
                  5,
                  1,
                  2,
                  6,
                  4,
                  3]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "two trees",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\cs -> Strings.cat2 "left(" (Strings.cat2 (Core.list (Core.list Literals.showInt32) cs) ")")) (\xs -> Strings.cat2 "right(" (Strings.cat2 (Core.list Literals.showInt32 xs) ")")) (Sorting.topologicalSort [
                  (3, [
                    1,
                    4]),
                  (5, [
                    6,
                    2]),
                  (2, [
                    7]),
                  (1, []),
                  (4, []),
                  (6, []),
                  (7, [])])),
                Testing.universalTestCaseExpected = (Eithers.either (\cs -> Strings.cat2 "left(" (Strings.cat2 (Core.list (Core.list Literals.showInt32) cs) ")")) (\xs -> Strings.cat2 "right(" (Strings.cat2 (Core.list Literals.showInt32 xs) ")")) (Right [
                  1,
                  7,
                  2,
                  4,
                  3,
                  6,
                  5]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "diamond DAG",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\cs -> Strings.cat2 "left(" (Strings.cat2 (Core.list (Core.list Literals.showInt32) cs) ")")) (\xs -> Strings.cat2 "right(" (Strings.cat2 (Core.list Literals.showInt32 xs) ")")) (Sorting.topologicalSort [
                  (1, [
                    3,
                    4]),
                  (3, [
                    2]),
                  (4, [
                    2]),
                  (2, [
                    5]),
                  (5, [])])),
                Testing.universalTestCaseExpected = (Eithers.either (\cs -> Strings.cat2 "left(" (Strings.cat2 (Core.list (Core.list Literals.showInt32) cs) ")")) (\xs -> Strings.cat2 "right(" (Strings.cat2 (Core.list Literals.showInt32 xs) ")")) (Right [
                  5,
                  2,
                  3,
                  4,
                  1]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "two-node cycle",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\cs -> Strings.cat2 "left(" (Strings.cat2 (Core.list (Core.list Literals.showInt32) cs) ")")) (\xs -> Strings.cat2 "right(" (Strings.cat2 (Core.list Literals.showInt32 xs) ")")) (Sorting.topologicalSort [
                  (1, [
                    2]),
                  (2, [
                    1])])),
                Testing.universalTestCaseExpected = (Eithers.either (\cs -> Strings.cat2 "left(" (Strings.cat2 (Core.list (Core.list Literals.showInt32) cs) ")")) (\xs -> Strings.cat2 "right(" (Strings.cat2 (Core.list Literals.showInt32 xs) ")")) (Left [
                  [
                    1,
                    2]]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "cycle with incoming and outgoing edges",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\cs -> Strings.cat2 "left(" (Strings.cat2 (Core.list (Core.list Literals.showInt32) cs) ")")) (\xs -> Strings.cat2 "right(" (Strings.cat2 (Core.list Literals.showInt32 xs) ")")) (Sorting.topologicalSort [
                  (1, [
                    3]),
                  (3, [
                    2]),
                  (2, [
                    3,
                    4]),
                  (4, [
                    5]),
                  (5, [])])),
                Testing.universalTestCaseExpected = (Eithers.either (\cs -> Strings.cat2 "left(" (Strings.cat2 (Core.list (Core.list Literals.showInt32) cs) ")")) (\xs -> Strings.cat2 "right(" (Strings.cat2 (Core.list Literals.showInt32 xs) ")")) (Left [
                  [
                    2,
                    3]]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "topological sort SCC",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty set",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core.list (Core.list Literals.showInt32) (Sorting.topologicalSortComponents [])),
                Testing.universalTestCaseExpected = (Core.list (Core.list Literals.showInt32) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "singleton set",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core.list (Core.list Literals.showInt32) (Sorting.topologicalSortComponents [
                  (1, [])])),
                Testing.universalTestCaseExpected = (Core.list (Core.list Literals.showInt32) [
                  [
                    1]])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "discrete set with multiple elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core.list (Core.list Literals.showInt32) (Sorting.topologicalSortComponents [
                  (3, []),
                  (1, []),
                  (2, [])])),
                Testing.universalTestCaseExpected = (Core.list (Core.list Literals.showInt32) [
                  [
                    1],
                  [
                    2],
                  [
                    3]])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single two-element component #1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core.list (Core.list Literals.showInt32) (Sorting.topologicalSortComponents [
                  (1, [
                    2]),
                  (2, [])])),
                Testing.universalTestCaseExpected = (Core.list (Core.list Literals.showInt32) [
                  [
                    2],
                  [
                    1]])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single two-element component #2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core.list (Core.list Literals.showInt32) (Sorting.topologicalSortComponents [
                  (2, [
                    1]),
                  (1, [])])),
                Testing.universalTestCaseExpected = (Core.list (Core.list Literals.showInt32) [
                  [
                    1],
                  [
                    2]])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple-element component",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core.list (Core.list Literals.showInt32) (Sorting.topologicalSortComponents [
                  (2, [
                    1,
                    3]),
                  (1, [
                    3]),
                  (3, [])])),
                Testing.universalTestCaseExpected = (Core.list (Core.list Literals.showInt32) [
                  [
                    3],
                  [
                    1],
                  [
                    2]])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "cycle of two nodes #1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core.list (Core.list Literals.showInt32) (Sorting.topologicalSortComponents [
                  (1, [
                    2]),
                  (2, [
                    1])])),
                Testing.universalTestCaseExpected = (Core.list (Core.list Literals.showInt32) [
                  [
                    1,
                    2]])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "cycle of two nodes #2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core.list (Core.list Literals.showInt32) (Sorting.topologicalSortComponents [
                  (2, [
                    1]),
                  (1, [
                    2])])),
                Testing.universalTestCaseExpected = (Core.list (Core.list Literals.showInt32) [
                  [
                    1,
                    2]])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "cycle of three nodes #1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core.list (Core.list Literals.showInt32) (Sorting.topologicalSortComponents [
                  (1, [
                    2]),
                  (2, [
                    3]),
                  (3, [
                    1])])),
                Testing.universalTestCaseExpected = (Core.list (Core.list Literals.showInt32) [
                  [
                    1,
                    2,
                    3]])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "cycle of three nodes #2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core.list (Core.list Literals.showInt32) (Sorting.topologicalSortComponents [
                  (2, [
                    1]),
                  (3, [
                    2]),
                  (1, [
                    3])])),
                Testing.universalTestCaseExpected = (Core.list (Core.list Literals.showInt32) [
                  [
                    1,
                    2,
                    3]])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple disconnected cycles",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core.list (Core.list Literals.showInt32) (Sorting.topologicalSortComponents [
                  (200, []),
                  (100, []),
                  (300, []),
                  (10, [
                    20]),
                  (20, [
                    10]),
                  (1, [
                    2]),
                  (2, [
                    3]),
                  (3, [
                    1])])),
                Testing.universalTestCaseExpected = (Core.list (Core.list Literals.showInt32) [
                  [
                    1,
                    2,
                    3],
                  [
                    10,
                    20],
                  [
                    100],
                  [
                    200],
                  [
                    300]])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "complex cycles",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core.list (Core.list Literals.showInt32) (Sorting.topologicalSortComponents [
                  (1, [
                    2,
                    3]),
                  (2, [
                    3]),
                  (3, [
                    1])])),
                Testing.universalTestCaseExpected = (Core.list (Core.list Literals.showInt32) [
                  [
                    1,
                    2,
                    3]])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "chain of three SCCs",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core.list (Core.list Literals.showInt32) (Sorting.topologicalSortComponents [
                  (1, [
                    2,
                    10]),
                  (2, [
                    3]),
                  (3, [
                    1]),
                  (10, [
                    20]),
                  (20, [
                    100,
                    10]),
                  (100, [])])),
                Testing.universalTestCaseExpected = (Core.list (Core.list Literals.showInt32) [
                  [
                    100],
                  [
                    10,
                    20],
                  [
                    1,
                    2,
                    3]])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "SCCs with dependencies to/from non-SCC nodes",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core.list (Core.list Literals.showInt32) (Sorting.topologicalSortComponents [
                  (1, [
                    2,
                    3,
                    10]),
                  (2, [
                    3]),
                  (3, [
                    1]),
                  (10, [
                    20,
                    30]),
                  (20, [
                    30]),
                  (30, []),
                  (100, [
                    200,
                    2]),
                  (200, []),
                  (300, [
                    100]),
                  (1000, []),
                  (2000, [])])),
                Testing.universalTestCaseExpected = (Core.list (Core.list Literals.showInt32) [
                  [
                    30],
                  [
                    20],
                  [
                    10],
                  [
                    1,
                    2,
                    3],
                  [
                    200],
                  [
                    100],
                  [
                    300],
                  [
                    1000],
                  [
                    2000]])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
