-- | A JSON syntax model. See the BNF at https://www.json.org

module Hydra.Json where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | A JSON value
data Value = 
  -- | A JSON array
  ValueArray [Value] |
  -- | A boolean value
  ValueBoolean Bool |
  -- | JSON's null value
  ValueNull  |
  -- | A numeric value
  ValueNumber Double |
  -- | A JSON object as a set of key/value pairs
  ValueObject (Map String Value) |
  -- | A string value
  ValueString String
  deriving (Eq, Ord, Read, Show)

_Value = (Core.Name "hydra.json.Value")

_Value_array = (Core.Name "array")

_Value_boolean = (Core.Name "boolean")

_Value_null = (Core.Name "null")

_Value_number = (Core.Name "number")

_Value_object = (Core.Name "object")

_Value_string = (Core.Name "string")