-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for topological sorting algorithms

module Hydra.Test.Sorting where

import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Test cases for topological sorting
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
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
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSort (Testing.TopologicalSortTestCase {
            Testing.topologicalSortTestCaseAdjacencyList = [],
            Testing.topologicalSortTestCaseExpected = (Right [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "singleton set",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSort (Testing.TopologicalSortTestCase {
            Testing.topologicalSortTestCaseAdjacencyList = [
              (1, [])],
            Testing.topologicalSortTestCaseExpected = (Right [
              1])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "discrete set with multiple elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSort (Testing.TopologicalSortTestCase {
            Testing.topologicalSortTestCaseAdjacencyList = [
              (3, []),
              (1, []),
              (2, [])],
            Testing.topologicalSortTestCaseExpected = (Right [
              1,
              2,
              3])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "linked list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSort (Testing.TopologicalSortTestCase {
            Testing.topologicalSortTestCaseAdjacencyList = [
              (3, [
                1]),
              (2, [
                3]),
              (1, [])],
            Testing.topologicalSortTestCaseExpected = (Right [
              1,
              3,
              2])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "binary tree",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSort (Testing.TopologicalSortTestCase {
            Testing.topologicalSortTestCaseAdjacencyList = [
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
              (5, [])],
            Testing.topologicalSortTestCaseExpected = (Right [
              5,
              1,
              2,
              6,
              4,
              3])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "two trees",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSort (Testing.TopologicalSortTestCase {
            Testing.topologicalSortTestCaseAdjacencyList = [
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
              (7, [])],
            Testing.topologicalSortTestCaseExpected = (Right [
              1,
              7,
              2,
              4,
              3,
              6,
              5])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "diamond DAG",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSort (Testing.TopologicalSortTestCase {
            Testing.topologicalSortTestCaseAdjacencyList = [
              (1, [
                3,
                4]),
              (3, [
                2]),
              (4, [
                2]),
              (2, [
                5]),
              (5, [])],
            Testing.topologicalSortTestCaseExpected = (Right [
              5,
              2,
              3,
              4,
              1])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "two-node cycle",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSort (Testing.TopologicalSortTestCase {
            Testing.topologicalSortTestCaseAdjacencyList = [
              (1, [
                2]),
              (2, [
                1])],
            Testing.topologicalSortTestCaseExpected = (Left [
              [
                1,
                2]])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "cycle with incoming and outgoing edges",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSort (Testing.TopologicalSortTestCase {
            Testing.topologicalSortTestCaseAdjacencyList = [
              (1, [
                3]),
              (3, [
                2]),
              (2, [
                3,
                4]),
              (4, [
                5]),
              (5, [])],
            Testing.topologicalSortTestCaseExpected = (Left [
              [
                2,
                3]])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "topological sort SCC",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty set",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSortSCC (Testing.TopologicalSortSCCTestCase {
            Testing.topologicalSortSCCTestCaseAdjacencyList = [],
            Testing.topologicalSortSCCTestCaseExpected = []})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "singleton set",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSortSCC (Testing.TopologicalSortSCCTestCase {
            Testing.topologicalSortSCCTestCaseAdjacencyList = [
              (1, [])],
            Testing.topologicalSortSCCTestCaseExpected = [
              [
                1]]})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "discrete set with multiple elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSortSCC (Testing.TopologicalSortSCCTestCase {
            Testing.topologicalSortSCCTestCaseAdjacencyList = [
              (3, []),
              (1, []),
              (2, [])],
            Testing.topologicalSortSCCTestCaseExpected = [
              [
                1],
              [
                2],
              [
                3]]})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single two-element component #1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSortSCC (Testing.TopologicalSortSCCTestCase {
            Testing.topologicalSortSCCTestCaseAdjacencyList = [
              (1, [
                2]),
              (2, [])],
            Testing.topologicalSortSCCTestCaseExpected = [
              [
                2],
              [
                1]]})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single two-element component #2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSortSCC (Testing.TopologicalSortSCCTestCase {
            Testing.topologicalSortSCCTestCaseAdjacencyList = [
              (2, [
                1]),
              (1, [])],
            Testing.topologicalSortSCCTestCaseExpected = [
              [
                1],
              [
                2]]})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple-element component",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSortSCC (Testing.TopologicalSortSCCTestCase {
            Testing.topologicalSortSCCTestCaseAdjacencyList = [
              (2, [
                1,
                3]),
              (1, [
                3]),
              (3, [])],
            Testing.topologicalSortSCCTestCaseExpected = [
              [
                3],
              [
                1],
              [
                2]]})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "cycle of two nodes #1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSortSCC (Testing.TopologicalSortSCCTestCase {
            Testing.topologicalSortSCCTestCaseAdjacencyList = [
              (1, [
                2]),
              (2, [
                1])],
            Testing.topologicalSortSCCTestCaseExpected = [
              [
                1,
                2]]})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "cycle of two nodes #2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSortSCC (Testing.TopologicalSortSCCTestCase {
            Testing.topologicalSortSCCTestCaseAdjacencyList = [
              (2, [
                1]),
              (1, [
                2])],
            Testing.topologicalSortSCCTestCaseExpected = [
              [
                1,
                2]]})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "cycle of three nodes #1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSortSCC (Testing.TopologicalSortSCCTestCase {
            Testing.topologicalSortSCCTestCaseAdjacencyList = [
              (1, [
                2]),
              (2, [
                3]),
              (3, [
                1])],
            Testing.topologicalSortSCCTestCaseExpected = [
              [
                1,
                2,
                3]]})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "cycle of three nodes #2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSortSCC (Testing.TopologicalSortSCCTestCase {
            Testing.topologicalSortSCCTestCaseAdjacencyList = [
              (2, [
                1]),
              (3, [
                2]),
              (1, [
                3])],
            Testing.topologicalSortSCCTestCaseExpected = [
              [
                1,
                2,
                3]]})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple disconnected cycles",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSortSCC (Testing.TopologicalSortSCCTestCase {
            Testing.topologicalSortSCCTestCaseAdjacencyList = [
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
                1])],
            Testing.topologicalSortSCCTestCaseExpected = [
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
                300]]})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "complex cycles",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSortSCC (Testing.TopologicalSortSCCTestCase {
            Testing.topologicalSortSCCTestCaseAdjacencyList = [
              (1, [
                2,
                3]),
              (2, [
                3]),
              (3, [
                1])],
            Testing.topologicalSortSCCTestCaseExpected = [
              [
                1,
                2,
                3]]})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "chain of three SCCs",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSortSCC (Testing.TopologicalSortSCCTestCase {
            Testing.topologicalSortSCCTestCaseAdjacencyList = [
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
              (100, [])],
            Testing.topologicalSortSCCTestCaseExpected = [
              [
                100],
              [
                10,
                20],
              [
                1,
                2,
                3]]})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "SCCs with dependencies to/from non-SCC nodes",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSortSCC (Testing.TopologicalSortSCCTestCase {
            Testing.topologicalSortSCCTestCaseAdjacencyList = [
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
              (2000, [])],
            Testing.topologicalSortSCCTestCaseExpected = [
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
                2000]]})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}
