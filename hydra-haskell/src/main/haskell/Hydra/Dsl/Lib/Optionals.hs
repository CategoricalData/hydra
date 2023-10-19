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

isJust :: Datum (Maybe a -> Bool)
isJust = Datum $ Terms.primitive _optionals_isJust

isNothing :: Datum (Maybe a -> Bool)
isNothing = Datum $ Terms.primitive _optionals_isNothing

map :: Datum ((a -> b) -> Maybe a -> Maybe b)
map = Datum $ Terms.primitive _optionals_map

pure :: Datum (a -> Maybe a)
pure = Datum $ Terms.primitive _optionals_pure
