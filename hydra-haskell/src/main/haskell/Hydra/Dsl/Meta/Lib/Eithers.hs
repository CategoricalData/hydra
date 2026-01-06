-- | Phantom-typed term DSL for the hydra.lib.eithers library

module Hydra.Dsl.Meta.Lib.Eithers where

import Hydra.Phantoms
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


bind :: TTerm (Either a b) -> TTerm (b -> Either a c) -> TTerm (Either a c)
bind = primitive2 _eithers_bind

bimap :: TTerm (a -> c) -> TTerm (b -> d) -> TTerm (Either a b) -> TTerm (Either c d)
bimap = primitive3 _eithers_bimap

either_ :: TTerm (a -> c) -> TTerm (b -> c) -> TTerm (Either a b) -> TTerm c
either_ = primitive3 _eithers_either

fromLeft :: TTerm a -> TTerm (Either a b) -> TTerm a
fromLeft = primitive2 _eithers_fromLeft

fromRight :: TTerm b -> TTerm (Either a b) -> TTerm b
fromRight = primitive2 _eithers_fromRight

isLeft :: TTerm (Either a b) -> TTerm Bool
isLeft = primitive1 _eithers_isLeft

isRight :: TTerm (Either a b) -> TTerm Bool
isRight = primitive1 _eithers_isRight

lefts :: TTerm [Either a b] -> TTerm [a]
lefts = primitive1 _eithers_lefts

map :: TTerm (a -> b) -> TTerm (Either c a) -> TTerm (Either c b)
map = primitive2 _eithers_map

mapList :: TTerm (a -> Either e b) -> TTerm [a] -> TTerm (Either e [b])
mapList = primitive2 _eithers_mapList

mapMaybe :: TTerm (a -> Either c b) -> TTerm (Maybe a) -> TTerm (Either c (Maybe b))
mapMaybe = primitive2 _eithers_mapMaybe

partitionEithers :: TTerm [Either a b] -> TTerm ([a], [b])
partitionEithers = primitive1 _eithers_partitionEithers

rights :: TTerm [Either a b] -> TTerm [b]
rights = primitive1 _eithers_rights
