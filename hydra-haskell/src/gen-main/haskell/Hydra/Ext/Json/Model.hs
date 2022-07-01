module Hydra.Ext.Json.Model where

import qualified Hydra.Core as Core
import Data.Map
import Data.Set

-- A JSON value
data Value 
  = ValueArray [Value]
  | ValueBoolean Bool
  | ValueNull 
  | ValueNumber Double
  | ValueObject (Map String Value)
  | ValueString String
  deriving (Eq, Ord, Read, Show)

_Value = (Core.Name "hydra/ext/json/model.Value")

_Value_array = (Core.FieldName "array")

_Value_boolean = (Core.FieldName "boolean")

_Value_null = (Core.FieldName "null")

_Value_number = (Core.FieldName "number")

_Value_object = (Core.FieldName "object")

_Value_string = (Core.FieldName "string")