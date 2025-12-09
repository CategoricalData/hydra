{-# LANGUAGE FlexibleContexts #-}

module Hydra.Dsl.Meta.Compute where

import Hydra.Kernel
import Hydra.Dsl.AsTerm
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Meta.Core as Core

import qualified Data.Map as M
import qualified Data.Maybe as Y


adapter :: AsTerm c (Coder s1 s2 v1 v2) => TTerm Bool -> TTerm t1 -> TTerm t2 -> c -> TTerm (Adapter s1 s2 t1 t2 v1 v2)
adapter isLossy source target coderArg = record _Adapter [
  _Adapter_isLossy>>: isLossy,
  _Adapter_source>>: source,
  _Adapter_target>>: target,
  _Adapter_coder>>: asTerm coderArg]

adapterIsLossy :: TTerm (Adapter s1 s2 t1 t2 v1 v2) -> TTerm Bool
adapterIsLossy a = project _Adapter _Adapter_isLossy @@ a

adapterSource :: TTerm (Adapter s1 s2 t1 t2 v1 v2) -> TTerm t1
adapterSource a = project _Adapter _Adapter_source @@ a

adapterTarget :: TTerm (Adapter s1 s2 t1 t2 v1 v2) -> TTerm t2
adapterTarget a = project _Adapter _Adapter_target @@ a

adapterCoder :: TTerm (Adapter s1 s2 t1 t2 v1 v2) -> TTerm (Coder s1 s2 v1 v2)
adapterCoder a = project _Adapter _Adapter_coder @@ a

adapterWithCoder :: TTerm (Adapter s1 s2 t1 t2 v1 v2) -> TTerm (Coder s1 s2 v1 v2) -> TTerm (Adapter s1 s2 t1 t2 v1 v2)
adapterWithCoder a coder = adapter
  (Hydra.Dsl.Meta.Compute.adapterIsLossy a)
  (Hydra.Dsl.Meta.Compute.adapterSource a)
  (Hydra.Dsl.Meta.Compute.adapterTarget a)
  coder

adapterWithTarget :: TTerm (Adapter s1 s2 t1 t2 v1 v2) -> TTerm t2 -> TTerm (Adapter s1 s2 t1 t2 v1 v2)
adapterWithTarget a target = adapter
  (Hydra.Dsl.Meta.Compute.adapterIsLossy a)
  (Hydra.Dsl.Meta.Compute.adapterSource a)
  target
  (Hydra.Dsl.Meta.Compute.adapterCoder a)

coder :: TTerm (v1 -> Flow s1 v2) -> TTerm (v2 -> Flow s2 v1) -> TTerm (Coder s1 s2 v1 v2)
coder encode decode = record _Coder [
  _Coder_encode>>: encode,
  _Coder_decode>>: decode]

coderEncode :: TTerm (Coder s1 s2 v1 v2) -> TTerm (v1 -> Flow s1 v2)
coderEncode c = project _Coder _Coder_encode @@ c

coderDecode :: TTerm (Coder s1 s2 v1 v2) -> TTerm (v2 -> Flow s2 v1)
coderDecode c = project _Coder _Coder_decode @@ c

flow :: TTerm (s -> Trace -> FlowState s v) -> TTerm (Flow s v)
flow = wrap _Flow

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

trace :: TTerm [String] -> TTerm [String] -> TTerm (M.Map Name Term) -> TTerm Trace
trace stack messages other = record _Trace [
  _Trace_stack>>: stack,
  _Trace_messages>>: messages,
  _Trace_other>>: other]

traceStack :: TTerm Trace -> TTerm [String]
traceStack t = project _Trace _Trace_stack @@ t

traceMessages :: TTerm Trace -> TTerm [String]
traceMessages t = project _Trace _Trace_messages @@ t

traceOther :: TTerm Trace -> TTerm (M.Map Name Term)
traceOther t = project _Trace _Trace_other @@ t

traceWithOther :: TTerm Trace -> TTerm (M.Map Name Term) -> TTerm Trace
traceWithOther t other = record _Trace [
  _Trace_stack>>: Hydra.Dsl.Meta.Compute.traceStack t,
  _Trace_messages>>: Hydra.Dsl.Meta.Compute.traceMessages t,
  _Trace_other>>: other]

unFlow :: AsTerm t Trace => TTerm (Flow s x) -> TTerm s -> t -> TTerm (FlowState s x)
unFlow f s t = unwrap _Flow @@ f @@ s @@ asTerm t
