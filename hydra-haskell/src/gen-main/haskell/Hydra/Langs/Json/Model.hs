-- | A JSON syntax model. See the BNF at https://www.json.org

module Hydra.Langs.Json.Model where

import qualified Hydra.Core as Core
import Data.Int
import Data.List
import Data.Map
import Data.Set

-- | A JSON value
data Value = 
  ValueArray [Value] |
  ValueBoolean Bool |
  ValueNull  |
  ValueNumber Double |
  ValueObject (Map String Value) |
  ValueString String
  deriving (Eq, Ord, Read, Show)

_Value = (Core.Name "hydra/langs/json/model.Value")

_Value_array = (Core.FieldName "array")

_Value_boolean = (Core.FieldName "boolean")

_Value_null = (Core.FieldName "null")

_Value_number = (Core.FieldName "number")

_Value_object = (Core.FieldName "object")

_Value_string = (Core.FieldName "string")