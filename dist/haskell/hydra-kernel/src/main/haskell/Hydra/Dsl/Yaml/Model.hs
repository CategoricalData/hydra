-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.yaml.model

module Hydra.Dsl.Yaml.Model where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Typed as Typed
import qualified Hydra.Yaml.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | DSL injection for the mapping variant of hydra.yaml.model.Node
nodeMapping :: Typed.TypedTerm (M.Map Model.Node Model.Node) -> Typed.TypedTerm Model.Node
nodeMapping x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mapping"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the scalar variant of hydra.yaml.model.Node
nodeScalar :: Typed.TypedTerm Model.Scalar -> Typed.TypedTerm Model.Node
nodeScalar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "scalar"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the sequence variant of hydra.yaml.model.Node
nodeSequence :: Typed.TypedTerm [Model.Node] -> Typed.TypedTerm Model.Node
nodeSequence x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the bool variant of hydra.yaml.model.Scalar
scalarBool :: Typed.TypedTerm Bool -> Typed.TypedTerm Model.Scalar
scalarBool x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bool"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the decimal variant of hydra.yaml.model.Scalar
scalarDecimal :: Typed.TypedTerm Sci.Scientific -> Typed.TypedTerm Model.Scalar
scalarDecimal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decimal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the float variant of hydra.yaml.model.Scalar
scalarFloat :: Typed.TypedTerm Double -> Typed.TypedTerm Model.Scalar
scalarFloat x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the int variant of hydra.yaml.model.Scalar
scalarInt :: Typed.TypedTerm Integer -> Typed.TypedTerm Model.Scalar
scalarInt x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the null variant of hydra.yaml.model.Scalar
scalarNull :: Typed.TypedTerm Model.Scalar
scalarNull =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the str variant of hydra.yaml.model.Scalar
scalarStr :: Typed.TypedTerm String -> Typed.TypedTerm Model.Scalar
scalarStr x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "str"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
