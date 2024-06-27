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

apply :: TTerm (Flow s (x -> y) -> Flow s x -> Flow s y)
apply = TTerm $ Terms.primitive _flows_apply

bind :: TTerm (Flow s x -> (x -> Flow s y) -> Flow s y)
bind = TTerm $ Terms.primitive _flows_bind

fail :: TTerm (String -> Flow s x)
fail = TTerm $ Terms.primitive _flows_fail

map :: TTerm ((x -> y) -> Flow s x -> Flow s y)
map = TTerm $ Terms.primitive _flows_map

mapList :: TTerm ((x -> Flow s y) -> [x] -> Flow s [y])
mapList = TTerm $ Terms.primitive _flows_mapList

pure :: TTerm (x -> Flow s x)
pure = TTerm $ Terms.primitive _flows_pure

sequence :: TTerm ([Flow s a] -> Flow s [a])
sequence = TTerm $ Terms.primitive _flows_sequence

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
