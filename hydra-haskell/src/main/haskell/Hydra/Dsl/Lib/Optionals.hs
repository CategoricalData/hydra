module Hydra.Dsl.Lib.Optionals where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms


apply :: Datum (Maybe (x -> y) -> Maybe x -> Maybe y)
apply = Datum $ Terms.primitive _optionals_apply

bind :: Datum (Maybe x -> (x -> Maybe y) -> Maybe y)
bind = Datum $ Terms.primitive _optionals_bind

isJust :: Datum (Maybe  x-> Bool)
isJust = Datum $ Terms.primitive _optionals_isJust

isNothing :: Datum (Maybe x -> Bool)
isNothing = Datum $ Terms.primitive _optionals_isNothing

map :: Datum ((x -> y) -> Maybe x -> Maybe y)
map = Datum $ Terms.primitive _optionals_map

pure :: Datum (x -> Maybe x)
pure = Datum $ Terms.primitive _optionals_pure
