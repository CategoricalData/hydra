-- | Phantom-typed term DSL for the hydra.lib.eithers library

module Hydra.Dsl.Meta.Lib.Eithers where

import Hydra.Typed
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries
import qualified Data.Set as S


-- | Map over both sides of an Either value.
bimap :: TypedTerm (a -> c) -> TypedTerm (b -> d) -> TypedTerm (Either a b) -> TypedTerm (Either c d)
bimap = primitive3 _eithers_bimap

-- | Bind (flatMap) for Either: if Right, apply the function; if Left, return unchanged.
bind :: TypedTerm (Either a b) -> TypedTerm (b -> Either a c) -> TypedTerm (Either a c)
bind = primitive2 _eithers_bind

-- | Eliminate an Either value by applying one of two functions.
either_ :: TypedTerm (a -> c) -> TypedTerm (b -> c) -> TypedTerm (Either a b) -> TypedTerm c
either_ = primitive3 _eithers_either

-- | Left-fold over a list with an Either-returning function, short-circuiting on Left.
foldl :: TypedTerm (a -> b -> Either c a) -> TypedTerm a -> TypedTerm [b] -> TypedTerm (Either c a)
foldl = primitive3 _eithers_foldl

-- | Extract the Left value, or return a default.
fromLeft :: TypedTerm a -> TypedTerm (Either a b) -> TypedTerm a
fromLeft = primitive2 _eithers_fromLeft

-- | Extract the Right value, or return a default.
fromRight :: TypedTerm b -> TypedTerm (Either a b) -> TypedTerm b
fromRight = primitive2 _eithers_fromRight

-- | Check if an Either is a Left value.
isLeft :: TypedTerm (Either a b) -> TypedTerm Bool
isLeft = primitive1 _eithers_isLeft

-- | Check if an Either is a Right value.
isRight :: TypedTerm (Either a b) -> TypedTerm Bool
isRight = primitive1 _eithers_isRight

-- | Extract all Left values from a list of Eithers.
lefts :: TypedTerm [Either a b] -> TypedTerm [a]
lefts = primitive1 _eithers_lefts

-- | Map a function over the Right side of an Either (standard functor map).
map :: TypedTerm (a -> b) -> TypedTerm (Either c a) -> TypedTerm (Either c b)
map = primitive2 _eithers_map

-- | Map a function returning Either over a list, collecting results or short-circuiting on Left.
mapList :: TypedTerm (a -> Either e b) -> TypedTerm [a] -> TypedTerm (Either e [b])
mapList = primitive2 _eithers_mapList

-- | Map a function returning Either over an optional, or return Right Nothing if Nothing.
mapOptional :: TypedTerm (a -> Either c b) -> TypedTerm (Maybe a) -> TypedTerm (Either c (Maybe b))
mapOptional = primitive2 _eithers_mapOptional

-- | Map a function returning Either over a Set, collecting results or short-circuiting on Left.
mapSet :: TypedTerm (a -> Either c b) -> TypedTerm (S.Set a) -> TypedTerm (Either c (S.Set b))
mapSet = primitive2 _eithers_mapSet

-- | Partition a list of Eithers into lefts and rights.
partitionEithers :: TypedTerm [Either a b] -> TypedTerm ([a], [b])
partitionEithers = primitive1 _eithers_partitionEithers

-- | Extract all Right values from a list of Eithers.
rights :: TypedTerm [Either a b] -> TypedTerm [b]
rights = primitive1 _eithers_rights
