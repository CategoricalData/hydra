-- Note: this is an automatically generated file. Do not edit.
-- | Execution context for tracing and diagnostics

module Hydra.Context where
import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
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
_Context = Core.Name "hydra.context.Context"
_Context_trace = Core.Name "trace"
_Context_messages = Core.Name "messages"
_Context_other = Core.Name "other"
