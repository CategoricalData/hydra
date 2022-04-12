module Hydra.Ext.Json.Json where

import qualified Hydra.Core as Core
import Data.Map
import Data.Set

-- A numeric value
data Number 
  = Number {
    numberInteger :: Integer,
    numberFraction :: Integer,
    numberExponent :: Integer}
  deriving (Eq, Ord, Read, Show)

_Number = "hydra/ext/json/json.Number"

_Number_integer = "integer"

_Number_fraction = "fraction"

_Number_exponent = "exponent"

-- A JSON value
data Value 
  = ValueArray [Value]
  | ValueBoolean Bool
  | ValueNull 
  | ValueNumber Number
  | ValueObject (Map String Value)
  | ValueString String
  deriving (Eq, Ord, Read, Show)

_Value = "hydra/ext/json/json.Value"

_Value_array = "array"

_Value_boolean = "boolean"

_Value_null = "null"

_Value_number = "number"

_Value_object = "object"

_Value_string = "string"