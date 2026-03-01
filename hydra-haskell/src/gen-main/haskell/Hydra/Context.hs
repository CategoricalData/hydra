-- Note: this is an automatically generated file. Do not edit.

-- | Execution context for tracing and diagnostics

module Hydra.Context where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | An execution context for tracing and diagnostics, threaded through function calls
data Context = 
  Context {
    -- | A stack of context labels describing the current execution path
    contextTrace :: [String],
    -- | A log of warnings and/or info messages
    contextMessages :: [String],
    -- | A map of string keys to arbitrary terms as values, for application-specific use
    contextOther :: (M.Map Core.Name Core.Term)}
  deriving (Eq, Ord, Read, Show)

_Context = (Core.Name "hydra.context.Context")

_Context_trace = (Core.Name "trace")

_Context_messages = (Core.Name "messages")

_Context_other = (Core.Name "other")

-- | A particular domain object (such as an error) together with an execution context
data InContext e = 
  InContext {
    -- | A domain object; typically an error
    inContextObject :: e,
    -- | The execution context at the point of capture
    inContextContext :: Context}
  deriving (Eq, Ord, Read, Show)

_InContext = (Core.Name "hydra.context.InContext")

_InContext_object = (Core.Name "object")

_InContext_context = (Core.Name "context")
