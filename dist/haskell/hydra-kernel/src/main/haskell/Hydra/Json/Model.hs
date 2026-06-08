-- Note: this is an automatically generated file. Do not edit.
-- | A JSON syntax model. See the BNF at https://www.json.org

module Hydra.Json.Model where
import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | A JSON value
data Value =
  -- | A JSON array
  ValueArray [Value] |
  -- | A boolean value
  ValueBoolean Bool |
  -- | JSON's null value
  ValueNull |
  -- | A numeric value
  ValueNumber Sci.Scientific |
  -- | A JSON object as an ordered list of key/value pairs. The ordering preserves the declaration order of the record from which the object was encoded, rather than alphabetizing keys. As a consequence, equality of Value objects is order-sensitive: two objects with the same pairs in a different order are equal as JSON, but not as Hydra Values.
  ValueObject [(String, Value)] |
  -- | A string value
  ValueString String
  deriving (Eq, Ord, Read, Show)
_Value = Core.Name "hydra.json.model.Value"
_Value_array = Core.Name "array"
_Value_boolean = Core.Name "boolean"
_Value_null = Core.Name "null"
_Value_number = Core.Name "number"
_Value_object = Core.Name "object"
_Value_string = Core.Name "string"
