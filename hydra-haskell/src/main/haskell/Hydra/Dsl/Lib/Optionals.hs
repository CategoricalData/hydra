module Hydra.Dsl.Lib.Optionals where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms


apply :: Datum (Maybe (a -> b) -> Maybe a -> Maybe b)
apply = Datum $ Terms.primitive _optionals_apply

bind :: Datum (Maybe a -> (a -> Maybe b) -> Maybe b)
bind = Datum $ Terms.primitive _optionals_bind

cat :: Datum ([Maybe a] -> [a])
cat = Datum $ Terms.primitive _optionals_cat

fromMaybe :: Datum (a -> Maybe a -> a)
fromMaybe = Datum $ Terms.primitive _optionals_fromMaybe

isJust :: Datum (Maybe a -> Bool)
isJust = Datum $ Terms.primitive _optionals_isJust

isNothing :: Datum (Maybe a -> Bool)
isNothing = Datum $ Terms.primitive _optionals_isNothing

map :: Datum ((a -> b) -> Maybe a -> Maybe b)
map = Datum $ Terms.primitive _optionals_map

maybe :: Datum (b -> (a -> b) -> Maybe a -> b)
maybe = Datum $ Terms.primitive _optionals_maybe

pure :: Datum (a -> Maybe a)
pure = Datum $ Terms.primitive _optionals_pure
