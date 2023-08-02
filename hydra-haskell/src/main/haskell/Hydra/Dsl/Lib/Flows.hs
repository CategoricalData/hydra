module Hydra.Dsl.Lib.Flows where

import Hydra.Dsl.Base
import Hydra.Compute
import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms


-- Primitives

apply :: Datum (Flow s (x -> y) -> Flow s x -> Flow s y)
apply = Datum $ Terms.primitive _flows_apply

bind :: Datum (Flow s x -> (x -> Flow s y) -> Flow s y)
bind = Datum $ Terms.primitive _flows_bind

map :: Datum ((x -> y) -> Flow s x -> Flow s y)
map = Datum $ Terms.primitive _flows_map

pure :: Datum (x -> Flow s x)
pure = Datum $ Terms.primitive _flows_pure

-- Accessors

flowStateValue :: Datum (FlowState s x -> Maybe x)
flowStateValue = project _FlowState _FlowState_value

unFlow :: Datum (Flow s x -> s -> Trace -> FlowState s x)
unFlow = unwrap _Flow
