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

flowStateState :: TTerm (FlowState s x) -> TTerm s
flowStateState fs = project _FlowState _FlowState_state @@ fs

flowStateTrace :: TTerm (FlowState s x) -> TTerm Trace
flowStateTrace fs = project _FlowState _FlowState_trace @@ fs

flowStateValue :: TTerm (FlowState s x) -> TTerm (Maybe x)
flowStateValue fs = project _FlowState _FlowState_value @@ fs

trace :: TTerm [String] -> TTerm [String] -> TTerm (M.Map String (Term)) -> TTerm Trace
trace stack messages other = record _Trace [
  _Trace_stack>>: stack,
  _Trace_messages>>: messages,
  _Trace_other>>: other]

traceStack :: TTerm Trace -> TTerm [String]
traceStack t = project _Trace _Trace_stack @@ t

traceMessages :: TTerm Trace -> TTerm [String]
traceMessages t = project _Trace _Trace_messages @@ t

traceOther :: TTerm Trace -> TTerm (M.Map String (Term))
traceOther t = project _Trace _Trace_other @@ t

unFlow :: TTerm (Flow s x) -> TTerm s -> TTerm Trace -> TTerm (FlowState s x)
unFlow f s t = unwrap _Flow @@ f @@ s @@ t
