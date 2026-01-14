-- | Haskell implementations of hydra.lib.eithers primitives

module Hydra.Lib.Eithers where

import qualified Control.Monad as CM
import qualified Data.Bifunctor as BF
import qualified Data.Either as E


-- | Bind (flatMap) for Either: if Right, apply the function; if Left, return unchanged.
bind :: Either a b -> (b -> Either a c) -> Either a c
bind = (>>=)

-- | Map over both sides of an Either value.
bimap :: (a -> c) -> (b -> d) -> Either a b -> Either c d
bimap = BF.bimap

-- | Eliminate an Either value by applying one of two functions.
either :: (a -> c) -> (b -> c) -> Either a b -> c
either = E.either

-- | Extract the Left value, or return a default.
fromLeft :: a -> Either a b -> a
fromLeft = E.fromLeft

-- | Extract the Right value, or return a default.
fromRight :: b -> Either a b -> b
fromRight = E.fromRight

-- | Check if an Either is a Left value.
isLeft :: Either a b -> Bool
isLeft = E.isLeft

-- | Check if an Either is a Right value.
isRight :: Either a b -> Bool
isRight = E.isRight

-- | Extract all Left values from a list of Eithers.
lefts :: [Either a b] -> [a]
lefts = E.lefts

-- | Map a function over the Right side of an Either (standard functor map).
map :: (a -> b) -> Either c a -> Either c b
map = fmap

-- | Map a function returning Either over a list, collecting results or short-circuiting on Left.
mapList :: (a -> Either c b) -> [a] -> Either c [b]
mapList = CM.mapM

-- | Map a function returning Either over a Maybe, or return Right Nothing if Nothing.
mapMaybe :: (a -> Either c b) -> Maybe a -> Either c (Maybe b)
mapMaybe = CM.mapM

-- | Partition a list of Eithers into lefts and rights.
partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = E.partitionEithers

-- | Extract all Right values from a list of Eithers.
rights :: [Either a b] -> [b]
rights = E.rights
