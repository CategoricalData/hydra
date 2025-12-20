-- | Test cases for topological sorting algorithms
--
-- Note: This module supersedes the Haskell-specific Hydra.SortingSpec tests.
-- Historical note: The expected orderings changed when the algorithm was updated
-- to produce a canonical ordering (sorted by node value within each SCC).
module Hydra.Sources.Test.Sorting where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Phantoms as Base
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Sorting as SortingModule

import qualified Data.Int as I


-- Note: We use Int for input types in helpers because Base.int32 expects Int
-- and produces TTerm I.Int32. The test data literals (1, 2, 3) are polymorphic.

ns :: Namespace
ns = Namespace "hydra.test.sorting"

module_ :: Module
module_ = Module ns elements
    [SortingModule.ns]
    KernelTypes.kernelTypesNamespaces
    (Just "Test cases for topological sorting algorithms")
  where
    elements = [Base.toBinding allTests]

define :: String -> TTerm a -> TBinding a
define = Base.definitionInModule module_

allTests :: TBinding TestGroup
allTests = define "allTests" $
    Base.doc "Test cases for topological sorting" $
    supergroup "sorting" [
      topologicalSortGroup,
      topologicalSortSCCGroup]

-- Helper to create adjacency list
adj :: [(Int, [Int])] -> TTerm [(Int, [Int])]
adj pairs = Base.list [Base.pair (Base.int32 n) (Base.list (Base.int32 <$> deps)) | (n, deps) <- pairs]

-- Helper for Right result (sorted list)
sorted :: [Int] -> TTerm (Either [[Int]] [Int])
sorted xs = Base.right $ Base.list (Base.int32 <$> xs)

-- Helper for Left result (cycles)
cycles :: [[Int]] -> TTerm (Either [[Int]] [Int])
cycles cs = Base.left $ Base.list [Base.list (Base.int32 <$> c) | c <- cs]

-- Helper for SCC result
sccs :: [[Int]] -> TTerm [[Int]]
sccs cs = Base.list [Base.list (Base.int32 <$> c) | c <- cs]

-- | Test cases for topological sort (without cycles)
topologicalSortGroup :: TTerm TestGroup
topologicalSortGroup = subgroup "topological sort" [
    -- Discrete sets (no dependencies)
    sortCase "empty set"
      (adj [])
      (sorted []),
    sortCase "singleton set"
      (adj [(1, [])])
      (sorted [1]),
    sortCase "discrete set with multiple elements"
      (adj [(3, []), (1, []), (2, [])])
      (sorted [1, 2, 3]),

    -- Trees and DAGs
    sortCase "linked list"
      (adj [(3, [1]), (2, [3]), (1, [])])
      (sorted [1, 3, 2]),
    sortCase "binary tree"
      (adj [(3, [1, 4]), (4, [6, 2]), (1, [5]), (2, []), (6, []), (5, [])])
      (sorted [5, 1, 2, 6, 4, 3]),
    sortCase "two trees"
      (adj [(3, [1, 4]), (5, [6, 2]), (2, [7]), (1, []), (4, []), (6, []), (7, [])])
      (sorted [1, 7, 2, 4, 3, 6, 5]),
    sortCase "diamond DAG"
      (adj [(1, [3, 4]), (3, [2]), (4, [2]), (2, [5]), (5, [])])
      (sorted [5, 2, 3, 4, 1]),

    -- Cycles (expected to fail)
    sortCase "two-node cycle"
      (adj [(1, [2]), (2, [1])])
      (cycles [[1, 2]]),
    sortCase "cycle with incoming and outgoing edges"
      (adj [(1, [3]), (3, [2]), (2, [3, 4]), (4, [5]), (5, [])])
      (cycles [[2, 3]])]

-- | Test cases for topological sort with strongly connected component detection
topologicalSortSCCGroup :: TTerm TestGroup
topologicalSortSCCGroup = subgroup "topological sort SCC" [
    -- Discrete sets
    sortSCCCase "empty set"
      (adj [])
      (sccs []),
    sortSCCCase "singleton set"
      (adj [(1, [])])
      (sccs [[1]]),
    sortSCCCase "discrete set with multiple elements"
      (adj [(3, []), (1, []), (2, [])])
      (sccs [[1], [2], [3]]),

    -- Weakly-connected components
    sortSCCCase "single two-element component #1"
      (adj [(1, [2]), (2, [])])
      (sccs [[2], [1]]),
    sortSCCCase "single two-element component #2"
      (adj [(2, [1]), (1, [])])
      (sccs [[1], [2]]),
    sortSCCCase "multiple-element component"
      (adj [(2, [1, 3]), (1, [3]), (3, [])])
      (sccs [[3], [1], [2]]),

    -- Strongly-connected components (cycles)
    sortSCCCase "cycle of two nodes #1"
      (adj [(1, [2]), (2, [1])])
      (sccs [[1, 2]]),
    sortSCCCase "cycle of two nodes #2"
      (adj [(2, [1]), (1, [2])])
      (sccs [[1, 2]]),
    sortSCCCase "cycle of three nodes #1"
      (adj [(1, [2]), (2, [3]), (3, [1])])
      (sccs [[1, 2, 3]]),
    sortSCCCase "cycle of three nodes #2"
      (adj [(2, [1]), (3, [2]), (1, [3])])
      (sccs [[1, 2, 3]]),
    sortSCCCase "multiple disconnected cycles"
      (adj ([(200, [])] ++ [(100, [])] ++ [(300, [])] ++ [(10, [20]), (20, [10])] ++ [(1, [2]), (2, [3]), (3, [1])]))
      (sccs [[1, 2, 3], [10, 20], [100], [200], [300]]),
    sortSCCCase "complex cycles"
      (adj [(1, [2, 3]), (2, [3]), (3, [1])])
      (sccs [[1, 2, 3]]),

    -- Mixed weakly- and strongly-connected components
    sortSCCCase "chain of three SCCs"
      (adj [(1, [2, 10]), (2, [3]), (3, [1]), (10, [20]), (20, [100, 10]), (100, [])])
      (sccs [[100], [10, 20], [1, 2, 3]]),
    sortSCCCase "SCCs with dependencies to/from non-SCC nodes"
      (adj [(1, [2, 3, 10]), (2, [3]), (3, [1]),
            (10, [20, 30]), (20, [30]), (30, []),
            (100, [200, 2]), (200, []), (300, [100]),
            (1000, []),
            (2000, [])])
      (sccs [[30], [20], [10], [1, 2, 3], [200], [100], [300], [1000], [2000]])]
