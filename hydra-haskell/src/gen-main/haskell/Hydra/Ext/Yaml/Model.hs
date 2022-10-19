-- | A basic YAML representation model. Based on:
-- |   https://yaml.org/spec/1.2/spec.html
-- | The Serialization and Presentation properties of YAML,
-- | including directives, comments, anchors, style, formatting, and aliases, are not supported by this model.
-- | In addition, tags are omitted from this model, and non-standard scalars are unsupported.

module Hydra.Ext.Yaml.Model where

import qualified Hydra.Core as Core
import Data.List
import Data.Map
import Data.Set

-- | A YAML node (value)
data Node 
  = NodeMapping (Map Node Node)
  | NodeScalar Scalar
  | NodeSequence [Node]
  deriving (Eq, Ord, Read, Show)

_Node = (Core.Name "hydra/ext/yaml/model.Node")

_Node_mapping = (Core.FieldName "mapping")

_Node_scalar = (Core.FieldName "scalar")

_Node_sequence = (Core.FieldName "sequence")

-- | A union of scalars supported in the YAML failsafe and JSON schemas. Other scalars are not supported here
data Scalar 
  = ScalarBool Bool
  | ScalarFloat Double
  | ScalarInt Integer
  | ScalarNull 
  | ScalarStr String
  deriving (Eq, Ord, Read, Show)

_Scalar = (Core.Name "hydra/ext/yaml/model.Scalar")

_Scalar_bool = (Core.FieldName "bool")

_Scalar_float = (Core.FieldName "float")

_Scalar_int = (Core.FieldName "int")

_Scalar_null = (Core.FieldName "null")

_Scalar_str = (Core.FieldName "str")