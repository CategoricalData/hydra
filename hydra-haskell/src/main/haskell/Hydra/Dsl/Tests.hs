module Hydra.Dsl.Tests (
    module Hydra.Kernel,
    module Hydra.Testing,
    module Hydra.Sources.Libraries,
    module Hydra.Dsl.Terms,
    module Hydra.Dsl.Tests,
) where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Sources.Libraries
import Hydra.Dsl.Terms

import qualified Data.List as L
import qualified Data.Set as S


intList :: [Int] -> Term a
intList els = list (int32 <$> els)

intListList :: [[Int]] -> Term a
intListList lists = list (intList <$> lists)

primCase :: Name -> [Term a] -> Term a -> TestCase a
primCase name args output = TestCase Nothing input output
  where
    input = L.foldl (\a arg -> a @@ arg) (primitive name) args

stringList :: [String] -> Term a
stringList els = list (string <$> els)

stringSet :: Ord a => S.Set String -> Term a
stringSet strings = set $ S.fromList $ string <$> S.toList strings

testCase = TestCase Nothing
