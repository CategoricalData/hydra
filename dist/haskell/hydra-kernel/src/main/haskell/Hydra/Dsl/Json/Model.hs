-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.json.model

module Hydra.Dsl.Json.Model where
import qualified Hydra.Core as Core
import qualified Hydra.Json.Model as Model
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | DSL injection for the array variant of hydra.json.model.Value
valueArray :: Phantoms.TTerm [Model.Value] -> Phantoms.TTerm Model.Value
valueArray x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.json.model.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the boolean variant of hydra.json.model.Value
valueBoolean :: Phantoms.TTerm Bool -> Phantoms.TTerm Model.Value
valueBoolean x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.json.model.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the null variant of hydra.json.model.Value
valueNull :: Phantoms.TTerm Model.Value
valueNull =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.json.model.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the number variant of hydra.json.model.Value
valueNumber :: Phantoms.TTerm Sci.Scientific -> Phantoms.TTerm Model.Value
valueNumber x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.json.model.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the object variant of hydra.json.model.Value
valueObject :: Phantoms.TTerm (M.Map String Model.Value) -> Phantoms.TTerm Model.Value
valueObject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.json.model.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the string variant of hydra.json.model.Value
valueString :: Phantoms.TTerm String -> Phantoms.TTerm Model.Value
valueString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.json.model.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
