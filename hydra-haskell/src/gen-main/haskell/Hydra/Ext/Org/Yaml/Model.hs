-- | A basic YAML representation model. Based on:
-- |   https://yaml.org/spec/1.2/spec.html
-- | The Serialization and Presentation properties of YAML,
-- | including directives, comments, anchors, style, formatting, and aliases, are not supported by this model.
-- | In addition, tags are omitted from this model, and non-standard scalars are unsupported.

module Hydra.Ext.Org.Yaml.Model where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A YAML node (value)
data Node = 
  NodeMapping (M.Map Node Node) |
  NodeScalar Scalar |
  NodeSequence [Node]
  deriving (Eq, Ord, Read, Show)

_Node = (Core.Name "hydra.ext.org.yaml.model.Node")

_Node_mapping = (Core.Name "mapping")

_Node_scalar = (Core.Name "scalar")

_Node_sequence = (Core.Name "sequence")

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

_Scalar = (Core.Name "hydra.ext.org.yaml.model.Scalar")

_Scalar_bool = (Core.Name "bool")

_Scalar_float = (Core.Name "float")

_Scalar_int = (Core.Name "int")

_Scalar_null = (Core.Name "null")

_Scalar_str = (Core.Name "str")
