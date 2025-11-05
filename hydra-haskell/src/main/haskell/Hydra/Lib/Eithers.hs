-- | Haskell implementations of hydra.lib.eithers primitives

module Hydra.Lib.Eithers where

import qualified Data.Either as E


either :: (a -> c) -> (b -> c) -> Either a b -> c
either = E.either

fromLeft :: a -> Either a b -> a
fromLeft = E.fromLeft

fromRight :: b -> Either a b -> b
fromRight = E.fromRight

isLeft :: Either a b -> Bool
isLeft = E.isLeft

isRight :: Either a b -> Bool
isRight = E.isRight

lefts :: [Either a b] -> [a]
lefts = E.lefts

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = E.partitionEithers

rights :: [Either a b] -> [b]
rights = E.rights
