-- | Based on https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/any.proto

module Hydra.Ext.Protobuf.Any where

import qualified Hydra.Core as Core
import Data.List
import Data.Map
import Data.Set

-- | `Any` contains an arbitrary serialized protocol buffer message along with a URL that describes the type of the serialized message.
data Any = 
  Any {
    -- | A URL/resource name that uniquely identifies the type of the serialized protocol buffer message.
    anyTypeUrl :: String,
    -- | Must be a valid serialized protocol buffer of the above specified type.
    anyValue :: String}
  deriving (Eq, Ord, Read, Show)

_Any = (Core.Name "hydra/ext/protobuf/any.Any")

_Any_typeUrl = (Core.FieldName "typeUrl")

_Any_value = (Core.FieldName "value")