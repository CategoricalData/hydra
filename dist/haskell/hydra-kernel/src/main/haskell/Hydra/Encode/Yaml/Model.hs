-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.yaml.model

module Hydra.Encode.Yaml.Model where
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Yaml.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Encoder for hydra.yaml.model.Node
node :: Model.Node -> Core.Term
node x =
    case x of
      Model.NodeMapping v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.yaml.model.Node"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "mapping"),
          Core.fieldTerm = (Core.TermMap (Maps.bimap node node v0))}})
      Model.NodeScalar v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.yaml.model.Node"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "scalar"),
          Core.fieldTerm = (scalar v0)}})
      Model.NodeSequence v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.yaml.model.Node"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "sequence"),
          Core.fieldTerm = (Core.TermList (Lists.map node v0))}})
-- | Encoder for hydra.yaml.model.Scalar
scalar :: Model.Scalar -> Core.Term
scalar x =
    case x of
      Model.ScalarBool v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "bool"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralBoolean v0))}})
      Model.ScalarDecimal v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "decimal"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralDecimal v0))}})
      Model.ScalarFloat v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "float"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 v0)))}})
      Model.ScalarInt v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "int"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint v0)))}})
      Model.ScalarNull -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "null"),
          Core.fieldTerm = Core.TermUnit}})
      Model.ScalarStr v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "str"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v0))}})
