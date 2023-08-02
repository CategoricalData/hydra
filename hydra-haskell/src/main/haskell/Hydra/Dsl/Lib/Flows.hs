module Hydra.Dsl.Lib.Flows where

import Hydra.Compute
import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms


apply :: Datum (Flow s (x -> y) -> Flow s x -> Flow s y)
apply = Datum $ Terms.primitive _flows_apply

bind :: Datum (Flow s x -> (x -> Flow s y) -> Flow s y)
bind = Datum $ Terms.primitive _flows_bind

map :: Datum ((x -> y) -> Flow s x -> Flow s y)
map = Datum $ Terms.primitive _flows_map

pure :: Datum (x -> Flow s x)
pure = Datum $ Terms.primitive _flows_pure
