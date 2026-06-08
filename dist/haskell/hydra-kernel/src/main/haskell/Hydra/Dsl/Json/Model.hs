-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.json.model

module Hydra.Dsl.Json.Model where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Json.Model as Model
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL injection for the array variant of hydra.json.model.Value
valueArray :: Typed.TypedTerm [Model.Value] -> Typed.TypedTerm Model.Value
valueArray x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.json.model.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the boolean variant of hydra.json.model.Value
valueBoolean :: Typed.TypedTerm Bool -> Typed.TypedTerm Model.Value
valueBoolean x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.json.model.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the null variant of hydra.json.model.Value
valueNull :: Typed.TypedTerm Model.Value
valueNull =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.json.model.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the number variant of hydra.json.model.Value
valueNumber :: Typed.TypedTerm Sci.Scientific -> Typed.TypedTerm Model.Value
valueNumber x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.json.model.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the object variant of hydra.json.model.Value
valueObject :: Typed.TypedTerm [(String, Model.Value)] -> Typed.TypedTerm Model.Value
valueObject x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.json.model.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the string variant of hydra.json.model.Value
valueString :: Typed.TypedTerm String -> Typed.TypedTerm Model.Value
valueString x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.json.model.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
