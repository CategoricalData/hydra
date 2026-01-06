-- | Haskell implementations of hydra.lib.eithers primitives

module Hydra.Lib.Eithers where

import qualified Control.Monad as CM
import qualified Data.Bifunctor as BF
import qualified Data.Either as E


bind :: Either a b -> (b -> Either a c) -> Either a c
bind = (>>=)

bimap :: (a -> c) -> (b -> d) -> Either a b -> Either c d
bimap = BF.bimap

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

map :: (a -> b) -> Either c a -> Either c b
map = fmap

mapList :: (a -> Either c b) -> [a] -> Either c [b]
mapList = CM.mapM

mapMaybe :: (a -> Either c b) -> Maybe a -> Either c (Maybe b)
mapMaybe = CM.mapM

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = E.partitionEithers

rights :: [Either a b] -> [b]
rights = E.rights
