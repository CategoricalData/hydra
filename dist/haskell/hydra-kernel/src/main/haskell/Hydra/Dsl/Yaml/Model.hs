-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.yaml.model

module Hydra.Dsl.Yaml.Model where
import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Yaml.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
nodeMapping :: Phantoms.TTerm (M.Map Model.Node Model.Node) -> Phantoms.TTerm Model.Node
nodeMapping x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mapping"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
nodeScalar :: Phantoms.TTerm Model.Scalar -> Phantoms.TTerm Model.Node
nodeScalar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "scalar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
nodeSequence :: Phantoms.TTerm [Model.Node] -> Phantoms.TTerm Model.Node
nodeSequence x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
scalarBool :: Phantoms.TTerm Bool -> Phantoms.TTerm Model.Scalar
scalarBool x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bool"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
scalarDecimal :: Phantoms.TTerm Sci.Scientific -> Phantoms.TTerm Model.Scalar
scalarDecimal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decimal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
scalarFloat :: Phantoms.TTerm Double -> Phantoms.TTerm Model.Scalar
scalarFloat x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
scalarInt :: Phantoms.TTerm Integer -> Phantoms.TTerm Model.Scalar
scalarInt x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
scalarNull :: Phantoms.TTerm Model.Scalar
scalarNull =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))
scalarStr :: Phantoms.TTerm String -> Phantoms.TTerm Model.Scalar
scalarStr x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "str"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
