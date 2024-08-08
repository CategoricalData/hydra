-- | A basic YAML representation model. Based on:
-- |   https://yaml.org/spec/1.2/spec.html
-- | The Serialization and Presentation properties of YAML,
-- | including directives, comments, anchors, style, formatting, and aliases, are not supported by this model.
-- | In addition, tags are omitted from this model, and non-standard scalars are unsupported.

module Hydra.Langs.Yaml.Model where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | A YAML node (value)
data Node = 
  NodeMapping (Map Node Node) |
  NodeScalar Scalar |
  NodeSequence [Node]
  deriving (Eq, Ord, Read, Show)

_Node = (Core.Name "hydra/langs/yaml/model.Node")

_Node_mapping = (Core.Name "mapping")

_Node_scalar = (Core.Name "scalar")

_Node_sequence = (Core.Name "sequence")

_Node_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/yaml/model.Node"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "mapping"),
      Core.fieldTypeType = (Core.TypeMap (Core.MapType {
        Core.mapTypeKeys = _Node_type_,
        Core.mapTypeValues = _Node_type_}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "scalar"),
      Core.fieldTypeType = _Scalar_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence"),
      Core.fieldTypeType = (Core.TypeList _Node_type_)}]}))

-- | A union of scalars supported in the YAML failsafe and JSON schemas. Other scalars are not supported here
data Scalar = 
  -- | Represents a true/false value
  ScalarBool Bool |
  -- | Represents an approximation to real numbers
  ScalarFloat Double |
  -- | Represents arbitrary sized finite mathematical integers
  ScalarInt Integer |
  -- | Represents the lack of a value
  ScalarNull  |
  -- | A string value
  ScalarStr String
  deriving (Eq, Ord, Read, Show)

_Scalar = (Core.Name "hydra/langs/yaml/model.Scalar")

_Scalar_bool = (Core.Name "bool")

_Scalar_float = (Core.Name "float")

_Scalar_int = (Core.Name "int")

_Scalar_null = (Core.Name "null")

_Scalar_str = (Core.Name "str")

_Scalar_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/yaml/model.Scalar"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bool"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "float"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeBigfloat))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "int"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "null"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "str"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)}]}))