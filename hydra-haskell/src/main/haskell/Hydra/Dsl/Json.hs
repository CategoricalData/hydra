module Hydra.Dsl.Json where

import Hydra.Kernel
import Hydra.Dsl.Phantoms
import Hydra.Json

import qualified Data.Map as M


valueArray :: TTerm [Value] -> TTerm Value
valueArray = variant _Value _Value_array

valueBoolean :: TTerm Bool -> TTerm Value
valueBoolean = variant _Value _Value_boolean

valueNull :: TTerm Value
valueNull = unitVariant _Value _Value_null

valueNumber :: TTerm Double -> TTerm Value
valueNumber = variant _Value _Value_number

valueObject :: TTerm (M.Map String Value) -> TTerm Value
valueObject = variant _Value _Value_object

valueString :: TTerm String -> TTerm Value
valueString = variant _Value _Value_string
