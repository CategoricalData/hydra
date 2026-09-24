-- | Convenience functions for writing Hydra test cases

module Hydra.Core.Overlay.Haskell.Dsl.Tests (
    module Hydra.Core.Testing,
    module Hydra.Core.Overlay.Haskell.Libraries,
    module Hydra.Core.Overlay.Haskell.Dsl.Terms,
    module Hydra.Core.Overlay.Haskell.Dsl.Tests,
) where

import Hydra.Core.Model
import Hydra.Core.Testing
import Hydra.Core.Overlay.Haskell.Libraries
import Hydra.Core.Overlay.Haskell.Dsl.Terms

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
