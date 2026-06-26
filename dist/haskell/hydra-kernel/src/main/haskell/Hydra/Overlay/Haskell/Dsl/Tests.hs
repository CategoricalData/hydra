-- | Convenience functions for writing Hydra test cases

module Hydra.Overlay.Haskell.Dsl.Tests (
    module Hydra.Testing,
    module Hydra.Overlay.Haskell.Libraries,
    module Hydra.Overlay.Haskell.Dsl.Terms,
    module Hydra.Overlay.Haskell.Dsl.Tests,
) where

import Hydra.Core
import Hydra.Testing
import Hydra.Overlay.Haskell.Libraries
import Hydra.Overlay.Haskell.Dsl.Terms

import qualified Data.List as L
import qualified Data.Set as S


intList :: [Int] -> Term
intList els = list (int32 <$> els)

intListList :: [[Int]] -> Term
intListList lists = list (intList <$> lists)

stringList :: [String] -> Term
stringList els = list (string <$> els)

stringSet :: S.Set String -> Term
stringSet strings = set $ S.fromList $ string <$> S.toList strings
