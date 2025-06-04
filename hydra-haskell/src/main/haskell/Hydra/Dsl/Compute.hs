module Hydra.Dsl.Compute where

import Hydra.Kernel
import Hydra.Dsl.Phantoms
import qualified Hydra.Dsl.Core as Core

import qualified Data.Map as M
import qualified Data.Maybe as Y


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

traversalOrderPre = unitVariant _TraversalOrder _TraversalOrder_pre
traversalOrderPost = unitVariant _TraversalOrder _TraversalOrder_post
