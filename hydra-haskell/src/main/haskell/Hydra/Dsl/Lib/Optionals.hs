module Hydra.Dsl.Lib.Optionals where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms


apply :: TTerm (Maybe (a -> b) -> Maybe a -> Maybe b)
apply = TTerm $ Terms.primitive _optionals_apply

bind :: TTerm (Maybe a -> (a -> Maybe b) -> Maybe b)
bind = TTerm $ Terms.primitive _optionals_bind

cat :: TTerm ([Maybe a] -> [a])
cat = TTerm $ Terms.primitive _optionals_cat

compose :: TTerm ((a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c))
compose = TTerm $ Terms.primitive _optionals_compose

fromMaybe :: TTerm (a -> Maybe a -> a)
fromMaybe = TTerm $ Terms.primitive _optionals_fromMaybe

isJust :: TTerm (Maybe a -> Bool)
isJust = TTerm $ Terms.primitive _optionals_isJust

isNothing :: TTerm (Maybe a -> Bool)
isNothing = TTerm $ Terms.primitive _optionals_isNothing

map :: TTerm ((a -> b) -> Maybe a -> Maybe b)
map = TTerm $ Terms.primitive _optionals_map

maybe :: TTerm (b -> (a -> b) -> Maybe a -> b)
maybe = TTerm $ Terms.primitive _optionals_maybe

pure :: TTerm (a -> Maybe a)
pure = TTerm $ Terms.primitive _optionals_pure
