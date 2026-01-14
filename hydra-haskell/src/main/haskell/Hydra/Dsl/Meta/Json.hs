-- | Meta-DSL for constructing JSON value terms

module Hydra.Dsl.Meta.Json where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms
import Hydra.Json.Model

import qualified Data.Map as M


valueArray :: TTerm [Value] -> TTerm Value
valueArray = inject _Value _Value_array

valueBoolean :: TTerm Bool -> TTerm Value
valueBoolean = inject _Value _Value_boolean

valueNull :: TTerm Value
valueNull = injectUnit _Value _Value_null

valueNumber :: TTerm Double -> TTerm Value
valueNumber = inject _Value _Value_number

valueObject :: TTerm (M.Map String Value) -> TTerm Value
valueObject = inject _Value _Value_object

valueString :: TTerm String -> TTerm Value
valueString = inject _Value _Value_string
