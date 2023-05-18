module Hydra.Tools.SortingSpec where

import qualified Test.Hspec as H

import Hydra.Tools.Sorting


checkTopSort :: (Ord a, Show a) => [(a, [a])] -> [[a]] -> H.Expectation
checkTopSort adj exp = H.shouldBe (topologicalSortComponents adj) exp

checkDiscreteSet :: H.SpecWith ()
checkDiscreteSet = H.describe "Check sorting of discrete sets (no dependencies)" $ do

  H.it "Empty set" $
    checkTopSort [] ([] :: [[Int]])

  H.it "Singleton set" $
    checkTopSort
      [(1, [])]
      [[1]]

  H.it "Discrete set with multiple elements" $
    checkTopSort
      [(3, []), (1, []), (2, [])]
      [[3],[2],[1]]

checkWeaklyConnectedComponents :: H.SpecWith ()
checkWeaklyConnectedComponents = H.describe "Check weakly-connected components" $ do

  H.describe "Single, two-element component" $ do
    H.it "test #1" $ checkTopSort
      [(1, [2]), (2, [])]
      [[2], [1]]
    H.it "test #2" $ checkTopSort
      [(2, [1]), (1, [])]
      [[1], [2]]

  H.it "Multiple-element component" $ do
    checkTopSort
      [(2, [1, 3]), (1, [3]), (3, [])]
      [[3], [1], [2]]

checkStronglyConnectedComponents :: H.SpecWith ()
checkStronglyConnectedComponents = H.describe "Check strongly-connected components" $ do

  H.describe "Cycle of two nodes" $ do
    H.it "test #1" $ checkTopSort
      [(1, [2]), (2, [1])]
      [[1, 2]]
    H.it "test #2" $ checkTopSort
      [(2, [1]), (1, [2])]
      [[1, 2]]

  H.describe "Cycle of three nodes, ordered naturally" $ do

    H.it "test #1" $ checkTopSort
      [(1, [2]), (2, [3]), (3, [1])]
      [[1, 2, 3]]
    H.it "test #2" $ checkTopSort
      [(2, [1]), (3, [2]), (1, [3])]
      [[1, 2, 3]]

  H.it "Multiple, disconnected cycles, each ordered naturally" $ do
    checkTopSort
      ([(200, [])] ++ [(100, [])] ++ [(300, [])] ++ [(10, [20]), (20, [10])] ++ [(1, [2]), (2, [3]), (3, [1])])
      [[300], [200], [100], [10, 20], [1, 2, 3]]

  H.it "Complex cycles" $ do
    checkTopSort
      [(1, [2, 3]), (2, [3]), (3, [1])]
      [[1, 2, 3]]

checkMixed :: H.SpecWith ()
checkMixed = H.describe "Check graphs which are a mix of weakly- and strongly-connected components" $ do

  H.it "Chain of three SCCs" $
    checkTopSort
      [(1, [2, 10]), (2, [3]), (3, [1]), (10, [20]), (20, [100, 10]), (100, [])]
      [[100], [10, 20], [1, 2, 3]]

  H.it "SCCs with dependencies to/from non-SCC nodes" $
    checkTopSort
      [(1, [2, 3, 10]), (2, [3]), (3, [1]),
       (10, [20, 30]), (20, [30]), (30, []),
       (100, [200, 2]), (200, []), (300, [100]),
       (1000, []),
       (2000, [])]
      [[2000], [1000], [200], [30], [20], [10], [1, 2, 3], [100], [300]]

spec :: H.Spec
spec = do
  checkDiscreteSet
  checkWeaklyConnectedComponents
  checkStronglyConnectedComponents
  checkMixed
