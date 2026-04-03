-- | Test cases for topological sorting algorithms
--
-- Note: This module supersedes the Haskell-specific Hydra.SortingSpec tests.
-- Historical note: The expected orderings changed when the algorithm was updated
-- to produce a canonical ordering (sorted by node value within each SCC).
module Hydra.Sources.Test.Sorting where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import           Hydra.Dsl.Meta.Phantoms      as Phantoms hiding ((++))
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Hydra.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Sources.Kernel.Terms.Sorting as SortingModule
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Strings  as Strings

import qualified Data.Int as I


-- Note: We use Int for input types in helpers because int32 expects Int
-- and produces TTerm I.Int32. The test data literals (1, 2, 3) are polymorphic.

ns :: Namespace
ns = Namespace "hydra.test.sorting"

module_ :: Module
module_ = Module ns elements
    [SortingModule.ns, ShowCore.ns]
    kernelTypesNamespaces
    (Just "Test cases for topological sorting algorithms")
  where
    elements = [Phantoms.toDefinition allTests]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

allTests :: TTermDefinition TestGroup
allTests = define "allTests" $
    doc "Test cases for topological sorting" $
    supergroup "sorting" [
      topologicalSortGroup,
      topologicalSortSCCGroup]

-- | Show a list of Int32 as a string like "[1, 2, 3]"
showIntList :: TTerm [Int] -> TTerm String
showIntList xs = ShowCore.list_ @@ unaryFunction Literals.showInt32 @@ xs

-- | Show a list of lists of Int32 as a string like "[[1, 2], [3]]"
showIntListList :: TTerm [[Int]] -> TTerm String
showIntListList xs = ShowCore.list_ @@ unaryFunction showIntList @@ xs

-- | Show Either [[Int]] [Int] as "left([[1, 2]])" or "right([1, 2, 3])"
showEitherResult :: TTerm (Either [[Int]] [Int]) -> TTerm String
showEitherResult = Eithers.either_
  (lambda "cs" (Strings.cat2 (string "left(") (Strings.cat2 (showIntListList (var "cs")) (string ")"))))
  (lambda "xs" (Strings.cat2 (string "right(") (Strings.cat2 (showIntList (var "xs")) (string ")"))))

-- Helper to create adjacency list
adj :: [(Int, [Int])] -> TTerm [(Int, [Int])]
adj pairs = list [pair (int32 n) (list (int32 <$> deps)) | (n, deps) <- pairs]

-- Universal sort test case
sortCase :: String -> TTerm [(Int, [Int])] -> TTerm (Either [[Int]] [Int]) -> TTerm TestCaseWithMetadata
sortCase cname adjList expected =
  universalCase cname
    (showEitherResult (SortingModule.topologicalSort @@ adjList))
    (showEitherResult expected)

-- Universal SCC test case
sortSCCCase :: String -> TTerm [(Int, [Int])] -> TTerm [[Int]] -> TTerm TestCaseWithMetadata
sortSCCCase cname adjList expected =
  universalCase cname
    (showIntListList (SortingModule.topologicalSortComponents @@ adjList))
    (showIntListList expected)

-- Helper for Right result (sorted list)
sorted :: [Int] -> TTerm (Either [[Int]] [Int])
sorted xs = right $ list (int32 <$> xs)

-- Helper for Left result (cycles)
cycles :: [[Int]] -> TTerm (Either [[Int]] [Int])
cycles cs = left $ list [list (int32 <$> c) | c <- cs]

-- Helper for SCC result
sccs :: [[Int]] -> TTerm [[Int]]
sccs cs = list [list (int32 <$> c) | c <- cs]

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
