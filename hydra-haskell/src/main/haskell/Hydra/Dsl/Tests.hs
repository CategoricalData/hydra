module Hydra.Dsl.Tests (
    module Hydra.Testing,
    module Hydra.Sources.Libraries,
    module Hydra.Dsl.Terms,
    module Hydra.Dsl.Tests,
) where

import Hydra.Core
import Hydra.Testing
import Hydra.Sources.Libraries
import Hydra.Dsl.Terms

import qualified Data.List as L
import qualified Data.Set as S


intList :: [Int] -> Term
intList els = list (int32 <$> els)

intListList :: [[Int]] -> Term
intListList lists = list (intList <$> lists)

primCase :: String -> Name -> [Term] -> Term -> TestCaseWithMetadata
primCase cname name args output = TestCaseWithMetadata cname tcase Nothing []
  where
    tcase = TestCaseEvaluation $ EvaluationTestCase EvaluationStyleEager input output
    input = L.foldl (\a arg -> a @@ arg) (primitive name) args

stringList :: [String] -> Term
stringList els = list (string <$> els)

stringSet :: S.Set String -> Term
stringSet strings = set $ S.fromList $ string <$> S.toList strings
