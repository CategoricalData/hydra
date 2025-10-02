--| Phantom-typed term DSL for the hydra.lib.optionals library

module Hydra.Dsl.Lib.Optionals where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Dsl.Phantoms


apply :: TTerm (Maybe (a -> b)) -> TTerm (Maybe a) -> TTerm (Maybe b)
apply = primitive2 _optionals_apply

bind :: TTerm (Maybe a) -> TTerm (a -> Maybe b) -> TTerm (Maybe b)
bind = primitive2 _optionals_bind

cases :: TTerm (Maybe a) -> TTerm b -> TTerm (a -> b) -> TTerm b
cases = primitive3 _optionals_cases

cat :: TTerm [Maybe a] -> TTerm [a]
cat = primitive1 _optionals_cat

compose :: TTerm (a -> Maybe b) -> TTerm (b -> Maybe c) -> TTerm (a -> Maybe c)
compose = primitive2 _optionals_compose

fromJust :: TTerm (Maybe a) -> TTerm a
fromJust = primitive1 _optionals_fromJust

fromMaybe :: TTerm a -> TTerm (Maybe a) -> TTerm a
fromMaybe = primitive2 _optionals_fromMaybe

isJust :: TTerm (Maybe a) -> TTerm Bool
isJust = primitive1 _optionals_isJust

isNothing :: TTerm (Maybe a) -> TTerm Bool
isNothing = primitive1 _optionals_isNothing

map :: TTerm (a -> b) -> TTerm (Maybe a) -> TTerm (Maybe b)
map = primitive2 _optionals_map

mapMaybe :: TTerm (a -> Maybe b) -> TTerm [a] -> TTerm [b]
mapMaybe = primitive2 _optionals_mapMaybe

maybe :: TTerm b -> TTerm (a -> b) -> TTerm (Maybe a) -> TTerm b
maybe = primitive3 _optionals_maybe

pure :: TTerm a -> TTerm (Maybe a)
pure = primitive1 _optionals_pure
