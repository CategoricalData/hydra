-- | Based on https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/source_context.proto

module Hydra.Ext.Protobuf.SourceContext where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | `SourceContext` represents information about the source of a protobuf element, like the file in which it is defined.
data SourceContext = 
  SourceContext {
    -- | The path-qualified name of the .proto file that contained the associated protobuf element.  For example: `"google/protobuf/source_context.proto"`.
    sourceContextFileName :: String}
  deriving (Eq, Ord, Read, Show)

_SourceContext = (Core.Name "hydra.ext.protobuf.sourceContext.SourceContext")

_SourceContext_fileName = (Core.Name "fileName")
