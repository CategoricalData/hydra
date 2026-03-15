-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.json.model

module Hydra.Dsl.Json.Model where

import qualified Hydra.Json.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

valueArray :: ([Model.Value] -> Model.Value)
valueArray x = (Model.ValueArray x)

valueBoolean :: (Bool -> Model.Value)
valueBoolean x = (Model.ValueBoolean x)

valueNull :: Model.Value
valueNull = Model.ValueNull

valueNumber :: (Double -> Model.Value)
valueNumber x = (Model.ValueNumber x)

valueObject :: (M.Map String Model.Value -> Model.Value)
valueObject x = (Model.ValueObject x)

valueString :: (String -> Model.Value)
valueString x = (Model.ValueString x)
