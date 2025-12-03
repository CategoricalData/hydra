-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.sorting"},ModuleName {unModuleName = "Sorting"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.sorting"},ModuleName {unModuleName = "Sorting"})]

module Generation.Hydra.Test.SortingSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Sorting as Sorting

spec :: H.Spec
spec = H.describe "sorting" $ do
  H.describe "topological sort" $ do
    H.it "empty set" $ H.shouldBe
      (Sorting.topologicalSort [])
      (Right [] :: Either [[Int]] [Int])
    H.it "singleton set" $ H.shouldBe
      (Sorting.topologicalSort [
          (1, [])])
      (Right [
          1])
    H.it "discrete set with multiple elements" $ H.shouldBe
      (Sorting.topologicalSort [
          (3, []),
          (1, []),
          (2, [])])
      (Right [
          1,
          2,
          3])
    H.it "linked list" $ H.shouldBe
      (Sorting.topologicalSort [
          (3, [
            1]),
          (2, [
            3]),
          (1, [])])
      (Right [
          1,
          3,
          2])
    H.it "binary tree" $ H.shouldBe
      (Sorting.topologicalSort [
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
          (5, [])])
      (Right [
          5,
          1,
          2,
          6,
          4,
          3])
    H.it "two trees" $ H.shouldBe
      (Sorting.topologicalSort [
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
          (7, [])])
      (Right [
          1,
          7,
          2,
          4,
          3,
          6,
          5])
    H.it "diamond DAG" $ H.shouldBe
      (Sorting.topologicalSort [
          (1, [
            3,
            4]),
          (3, [
            2]),
          (4, [
            2]),
          (2, [
            5]),
          (5, [])])
      (Right [
          5,
          2,
          3,
          4,
          1])
    H.it "two-node cycle" $ H.shouldBe
      (Sorting.topologicalSort [
          (1, [
            2]),
          (2, [
            1])])
      (Left [
          [
            1,
            2]])
    H.it "cycle with incoming and outgoing edges" $ H.shouldBe
      (Sorting.topologicalSort [
          (1, [
            3]),
          (3, [
            2]),
          (2, [
            3,
            4]),
          (4, [
            5]),
          (5, [])])
      (Left [
          [
            2,
            3]])
  H.describe "topological sort SCC" $ do
    H.it "empty set" $ H.shouldBe
      (Sorting.topologicalSortComponents [])
      ([] :: [[Int]])
    H.it "singleton set" $ H.shouldBe
      (Sorting.topologicalSortComponents [
          (1, [])])
      ([
          [
            1]])
    H.it "discrete set with multiple elements" $ H.shouldBe
      (Sorting.topologicalSortComponents [
          (3, []),
          (1, []),
          (2, [])])
      ([
          [
            1],
          [
            2],
          [
            3]])
    H.it "single two-element component #1" $ H.shouldBe
      (Sorting.topologicalSortComponents [
          (1, [
            2]),
          (2, [])])
      ([
          [
            2],
          [
            1]])
    H.it "single two-element component #2" $ H.shouldBe
      (Sorting.topologicalSortComponents [
          (2, [
            1]),
          (1, [])])
      ([
          [
            1],
          [
            2]])
    H.it "multiple-element component" $ H.shouldBe
      (Sorting.topologicalSortComponents [
          (2, [
            1,
            3]),
          (1, [
            3]),
          (3, [])])
      ([
          [
            3],
          [
            1],
          [
            2]])
    H.it "cycle of two nodes #1" $ H.shouldBe
      (Sorting.topologicalSortComponents [
          (1, [
            2]),
          (2, [
            1])])
      ([
          [
            1,
            2]])
    H.it "cycle of two nodes #2" $ H.shouldBe
      (Sorting.topologicalSortComponents [
          (2, [
            1]),
          (1, [
            2])])
      ([
          [
            1,
            2]])
    H.it "cycle of three nodes #1" $ H.shouldBe
      (Sorting.topologicalSortComponents [
          (1, [
            2]),
          (2, [
            3]),
          (3, [
            1])])
      ([
          [
            1,
            2,
            3]])
    H.it "cycle of three nodes #2" $ H.shouldBe
      (Sorting.topologicalSortComponents [
          (2, [
            1]),
          (3, [
            2]),
          (1, [
            3])])
      ([
          [
            1,
            2,
            3]])
    H.it "multiple disconnected cycles" $ H.shouldBe
      (Sorting.topologicalSortComponents [
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
            1])])
      ([
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
            300]])
    H.it "complex cycles" $ H.shouldBe
      (Sorting.topologicalSortComponents [
          (1, [
            2,
            3]),
          (2, [
            3]),
          (3, [
            1])])
      ([
          [
            1,
            2,
            3]])
    H.it "chain of three SCCs" $ H.shouldBe
      (Sorting.topologicalSortComponents [
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
          (100, [])])
      ([
          [
            100],
          [
            10,
            20],
          [
            1,
            2,
            3]])
    H.it "SCCs with dependencies to/from non-SCC nodes" $ H.shouldBe
      (Sorting.topologicalSortComponents [
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
          (2000, [])])
      ([
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
            2000]])
