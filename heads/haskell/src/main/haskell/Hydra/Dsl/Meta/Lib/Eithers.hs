-- | Phantom-typed term DSL for the hydra.lib.eithers library

module Hydra.Dsl.Meta.Lib.Eithers where

import Hydra.Phantoms
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries
import qualified Data.Set as S


-- | Map over both sides of an Either value.
bimap :: TTerm (a -> c) -> TTerm (b -> d) -> TTerm (Either a b) -> TTerm (Either c d)
bimap = primitive3 _eithers_bimap

-- | Bind (flatMap) for Either: if Right, apply the function; if Left, return unchanged.
bind :: TTerm (Either a b) -> TTerm (b -> Either a c) -> TTerm (Either a c)
bind = primitive2 _eithers_bind

-- | Eliminate an Either value by applying one of two functions.
either_ :: TTerm (a -> c) -> TTerm (b -> c) -> TTerm (Either a b) -> TTerm c
either_ = primitive3 _eithers_either

-- | Left-fold over a list with an Either-returning function, short-circuiting on Left.
foldl :: TTerm (a -> b -> Either c a) -> TTerm a -> TTerm [b] -> TTerm (Either c a)
foldl = primitive3 _eithers_foldl

-- | Extract the Left value, or return a default.
fromLeft :: TTerm a -> TTerm (Either a b) -> TTerm a
fromLeft = primitive2 _eithers_fromLeft

-- | Extract the Right value, or return a default.
fromRight :: TTerm b -> TTerm (Either a b) -> TTerm b
fromRight = primitive2 _eithers_fromRight

-- | Check if an Either is a Left value.
isLeft :: TTerm (Either a b) -> TTerm Bool
isLeft = primitive1 _eithers_isLeft

-- | Check if an Either is a Right value.
isRight :: TTerm (Either a b) -> TTerm Bool
isRight = primitive1 _eithers_isRight

-- | Extract all Left values from a list of Eithers.
lefts :: TTerm [Either a b] -> TTerm [a]
lefts = primitive1 _eithers_lefts

-- | Map a function over the Right side of an Either (standard functor map).
map :: TTerm (a -> b) -> TTerm (Either c a) -> TTerm (Either c b)
map = primitive2 _eithers_map

-- | Map a function returning Either over a list, collecting results or short-circuiting on Left.
mapList :: TTerm (a -> Either e b) -> TTerm [a] -> TTerm (Either e [b])
mapList = primitive2 _eithers_mapList

-- | Map a function returning Either over a Maybe, or return Right Nothing if Nothing.
mapMaybe :: TTerm (a -> Either c b) -> TTerm (Maybe a) -> TTerm (Either c (Maybe b))
mapMaybe = primitive2 _eithers_mapMaybe

-- | Map a function returning Either over a Set, collecting results or short-circuiting on Left.
mapSet :: TTerm (a -> Either c b) -> TTerm (S.Set a) -> TTerm (Either c (S.Set b))
mapSet = primitive2 _eithers_mapSet

-- | Partition a list of Eithers into lefts and rights.
partitionEithers :: TTerm [Either a b] -> TTerm ([a], [b])
partitionEithers = primitive1 _eithers_partitionEithers

-- | Extract all Right values from a list of Eithers.
rights :: TTerm [Either a b] -> TTerm [b]
rights = primitive1 _eithers_rights
