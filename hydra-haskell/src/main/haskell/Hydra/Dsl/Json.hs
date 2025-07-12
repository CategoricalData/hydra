module Hydra.Dsl.Json where

import Hydra.Kernel
import Hydra.Dsl.Phantoms
import Hydra.Json

import qualified Data.Map as M


valueArray :: TTerm [Value] -> TTerm Value
valueArray vs = unitVariant _Value _Value_array @@ vs

valueBoolean :: TTerm Bool -> TTerm Value
valueBoolean b = unitVariant _Value _Value_boolean @@ b

valueNull :: TTerm Value
valueNull = unitVariant _Value _Value_null

valueNumber :: TTerm Double -> TTerm Value
valueNumber n = unitVariant _Value _Value_number @@ n

valueObject :: TTerm (M.Map String Value) -> TTerm Value
valueObject m = unitVariant _Value _Value_object @@ m

valueString :: TTerm String -> TTerm Value
valueString s = unitVariant _Value _Value_string @@ s
