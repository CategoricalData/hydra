-- | Phantom-typed term DSL for the hydra.lib.maybes library

module Hydra.Dsl.Lib.Maybes where

import Hydra.Phantoms
import Hydra.Dsl.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


apply :: TTerm (Maybe (a -> b)) -> TTerm (Maybe a) -> TTerm (Maybe b)
apply = primitive2 _maybes_apply

bind :: TTerm (Maybe a) -> TTerm (a -> Maybe b) -> TTerm (Maybe b)
bind = primitive2 _maybes_bind

cases :: TTerm (Maybe a) -> TTerm b -> TTerm (a -> b) -> TTerm b
cases = primitive3 _maybes_cases

cat :: TTerm [Maybe a] -> TTerm [a]
cat = primitive1 _maybes_cat

compose :: TTerm (a -> Maybe b) -> TTerm (b -> Maybe c) -> TTerm (a -> Maybe c)
compose = primitive2 _maybes_compose

fromJust :: TTerm (Maybe a) -> TTerm a
fromJust = primitive1 _maybes_fromJust

fromMaybe :: TTerm a -> TTerm (Maybe a) -> TTerm a
fromMaybe = primitive2 _maybes_fromMaybe

isJust :: TTerm (Maybe a) -> TTerm Bool
isJust = primitive1 _maybes_isJust

isNothing :: TTerm (Maybe a) -> TTerm Bool
isNothing = primitive1 _maybes_isNothing

map :: TTerm (a -> b) -> TTerm (Maybe a) -> TTerm (Maybe b)
map = primitive2 _maybes_map

mapMaybe :: TTerm (a -> Maybe b) -> TTerm [a] -> TTerm [b]
mapMaybe = primitive2 _maybes_mapMaybe

maybe :: TTerm b -> TTerm (a -> b) -> TTerm (Maybe a) -> TTerm b
maybe = primitive3 _maybes_maybe

pure :: TTerm a -> TTerm (Maybe a)
pure = primitive1 _maybes_pure
