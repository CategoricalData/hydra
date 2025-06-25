-- | Based on https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/any.proto

module Hydra.Ext.Protobuf.Any where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | `Any` contains an arbitrary serialized protocol buffer message along with a URL that describes the type of the serialized message.
data Any = 
  Any {
    -- | A URL/resource name that uniquely identifies the type of the serialized protocol buffer message.
    anyTypeUrl :: String,
    -- | Must be a valid serialized protocol buffer of the above specified type.
    anyValue :: String}
  deriving (Eq, Ord, Read, Show)

_Any = (Core.Name "hydra.ext.protobuf.any.Any")

_Any_typeUrl = (Core.Name "typeUrl")

_Any_value = (Core.Name "value")
