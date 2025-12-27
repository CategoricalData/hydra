-- Note: this is an automatically generated file. Do not edit.

-- | Abstractions for single- and bidirectional transformations

module Hydra.Compute where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A two-level bidirectional encoder which adapts types to types and terms to terms
data Adapter s1 s2 t1 t2 v1 v2 = 
  Adapter {
    -- | Whether information may be lost in the course of this adaptation
    adapterIsLossy :: Bool,
    -- | The source type
    adapterSource :: t1,
    -- | The target type
    adapterTarget :: t2,
    -- | The coder for transforming instances of the source type to instances of the target type
    adapterCoder :: (Coder s1 s2 v1 v2)}

_Adapter = (Core.Name "hydra.compute.Adapter")

_Adapter_isLossy = (Core.Name "isLossy")

_Adapter_source = (Core.Name "source")

_Adapter_target = (Core.Name "target")

_Adapter_coder = (Core.Name "coder")

-- | A two-level encoder and decoder, operating both at a type level and an instance (data) level
data Bicoder s1 s2 t1 t2 v1 v2 = 
  Bicoder {
    -- | A function from source types to adapters
    bicoderEncode :: (t1 -> Adapter s1 s2 t1 t2 v1 v2),
    -- | A function from target types to adapters
    bicoderDecode :: (t2 -> Adapter s2 s1 t2 t1 v2 v1)}

_Bicoder = (Core.Name "hydra.compute.Bicoder")

_Bicoder_encode = (Core.Name "encode")

_Bicoder_decode = (Core.Name "decode")

-- | An encoder and decoder; a bidirectional flow between two types
data Coder s1 s2 v1 v2 = 
  Coder {
    -- | A function from source values to a flow of target values
    coderEncode :: (v1 -> Flow s1 v2),
    -- | A function from target values to a flow of source values
    coderDecode :: (v2 -> Flow s2 v1)}

_Coder = (Core.Name "hydra.compute.Coder")

_Coder_encode = (Core.Name "encode")

_Coder_decode = (Core.Name "decode")

-- | A variant of the State monad with built-in logging and error handling
newtype Flow s v = 
  Flow {
    unFlow :: (s -> Trace -> FlowState s v)}

_Flow = (Core.Name "hydra.compute.Flow")

-- | The result of evaluating a Flow
data FlowState s v = 
  FlowState {
    -- | The resulting value, or nothing in the case of failure
    flowStateValue :: (Maybe v),
    -- | The final state
    flowStateState :: s,
    -- | The trace (log) produced during evaluation
    flowStateTrace :: Trace}
  deriving (Eq, Ord, Read, Show)

_FlowState = (Core.Name "hydra.compute.FlowState")

_FlowState_value = (Core.Name "value")

_FlowState_state = (Core.Name "state")

_FlowState_trace = (Core.Name "trace")

-- | A container for logging and error information
data Trace = 
  Trace {
    -- | A stack of context labels
    traceStack :: [String],
    -- | A log of warnings and/or info messages
    traceMessages :: [String],
    -- | A map of string keys to arbitrary terms as values, for application-specific use
    traceOther :: (M.Map Core.Name Core.Term)}
  deriving (Eq, Ord, Read, Show)

_Trace = (Core.Name "hydra.compute.Trace")

_Trace_stack = (Core.Name "stack")

_Trace_messages = (Core.Name "messages")

_Trace_other = (Core.Name "other")
