module Hydra.Ext.Yaml.Model where

import qualified Hydra.Core as Core
import Data.Map
import Data.Set


data Node 
  = NodeMapping (Map Node Node)
  | NodeScalar Scalar
  | NodeSequence [Node]
  deriving (Eq, Ord, Read, Show)

_Node = "hydra/ext/yaml/model.Node"

_Node_mapping = "mapping"

_Node_scalar = "scalar"

_Node_sequence = "sequence"

-- A union of scalars supported in the YAML failsafe and JSON schemas. Other scalars are not supported here
data Scalar 
  = ScalarBool Bool
  | ScalarFloat Double
  | ScalarInt Integer
  | ScalarNull 
  | ScalarStr String
  deriving (Eq, Ord, Read, Show)

_Scalar = "hydra/ext/yaml/model.Scalar"

_Scalar_bool = "bool"

_Scalar_float = "float"

_Scalar_int = "int"

_Scalar_null = "null"

_Scalar_str = "str"