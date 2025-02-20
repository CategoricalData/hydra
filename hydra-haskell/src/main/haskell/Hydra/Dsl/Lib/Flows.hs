module Hydra.Dsl.Lib.Flows where

import Hydra.Dsl.Base
import Hydra.Core
import Hydra.Compute
import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Data.Map as M


-- Primitives

apply :: TTerm (Flow s (x -> y)) -> TTerm (Flow s x) -> TTerm (Flow s y)
apply = primitive2 _flows_apply

bind :: TTerm (Flow s x) -> TTerm (x -> Flow s y) -> TTerm (Flow s y)
bind = primitive2 _flows_bind

fail :: TTerm String -> TTerm (Flow s x)
fail = primitive1 _flows_fail

map :: TTerm (x -> y) -> TTerm (Flow s x) -> TTerm (Flow s y)
map = primitive2 _flows_map

mapList :: TTerm (x -> Flow s y) -> TTerm [x] -> TTerm (Flow s [y])
mapList = primitive2 _flows_mapList

pure :: TTerm x -> TTerm (Flow s x)
pure = primitive1 _flows_pure

sequence :: TTerm [Flow s a] -> TTerm (Flow s [a])
sequence = primitive1 _flows_sequence

-- Accessors

flowState :: TTerm (Maybe x) -> TTerm s -> TTerm Trace -> TTerm (FlowState s x)
flowState value state trace = record _FlowState [
  _FlowState_value>>: value,
  _FlowState_state>>: state,
  _FlowState_trace>>: trace]

flowStateState :: TTerm (FlowState s x -> s)
flowStateState = project _FlowState _FlowState_state

flowStateTrace :: TTerm (FlowState s x -> Trace)
flowStateTrace = project _FlowState _FlowState_trace

flowStateValue :: TTerm (FlowState s x -> Maybe x)
flowStateValue = project _FlowState _FlowState_value

trace :: TTerm [String] -> TTerm [String] -> TTerm (M.Map String (Term)) -> TTerm Trace
trace stack messages other = record _Trace [
  _Trace_stack>>: stack,
  _Trace_messages>>: messages,
  _Trace_other>>: other]

traceStack :: TTerm (Trace -> [String])
traceStack = project _Trace _Trace_stack

traceMessages :: TTerm (Trace -> [String])
traceMessages = project _Trace _Trace_messages

traceOther :: TTerm (Trace -> M.Map String (Term))
traceOther = project _Trace _Trace_other

unFlow :: TTerm (Flow s x -> s -> Trace -> FlowState s x)
unFlow = unwrap _Flow
