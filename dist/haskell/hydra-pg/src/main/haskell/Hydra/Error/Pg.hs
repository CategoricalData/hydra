-- Note: this is an automatically generated file. Do not edit.

-- | Error types for property graph validation

module Hydra.Error.Pg where

import qualified Hydra.Core as Core
import qualified Hydra.Pg.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | An error indicating that an edge is invalid
data InvalidEdgeError =
  -- | The edge id value is invalid
  InvalidEdgeErrorId InvalidValueError |
  -- | The in-vertex has the wrong label
  InvalidEdgeErrorInVertexLabel WrongVertexLabelError |
  -- | The in-vertex does not exist in the graph
  InvalidEdgeErrorInVertexNotFound  |
  -- | The edge label does not exist in the schema
  InvalidEdgeErrorLabel NoSuchEdgeLabelError |
  -- | The out-vertex has the wrong label
  InvalidEdgeErrorOutVertexLabel WrongVertexLabelError |
  -- | The out-vertex does not exist in the graph
  InvalidEdgeErrorOutVertexNotFound  |
  -- | A property of the edge is invalid
  InvalidEdgeErrorProperty InvalidElementPropertyError
  deriving (Eq, Ord, Read, Show)

_InvalidEdgeError = Core.Name "hydra.error.pg.InvalidEdgeError"

_InvalidEdgeError_id = Core.Name "id"

_InvalidEdgeError_inVertexLabel = Core.Name "inVertexLabel"

_InvalidEdgeError_inVertexNotFound = Core.Name "inVertexNotFound"

_InvalidEdgeError_label = Core.Name "label"

_InvalidEdgeError_outVertexLabel = Core.Name "outVertexLabel"

_InvalidEdgeError_outVertexNotFound = Core.Name "outVertexNotFound"

_InvalidEdgeError_property = Core.Name "property"

-- | An invalid property on a vertex or edge, identified by its key
data InvalidElementPropertyError =
  InvalidElementPropertyError {
    -- | The key of the invalid property
    invalidElementPropertyErrorKey :: Model.PropertyKey,
    -- | The specific error
    invalidElementPropertyErrorError :: InvalidPropertyError}
  deriving (Eq, Ord, Read, Show)

_InvalidElementPropertyError = Core.Name "hydra.error.pg.InvalidElementPropertyError"

_InvalidElementPropertyError_key = Core.Name "key"

_InvalidElementPropertyError_error = Core.Name "error"

-- | An invalid edge within a graph, identified by its id
data InvalidGraphEdgeError v =
  InvalidGraphEdgeError {
    -- | The id of the invalid edge
    invalidGraphEdgeErrorId :: v,
    -- | The specific error
    invalidGraphEdgeErrorError :: InvalidEdgeError}
  deriving (Eq, Ord, Read, Show)

_InvalidGraphEdgeError = Core.Name "hydra.error.pg.InvalidGraphEdgeError"

_InvalidGraphEdgeError_id = Core.Name "id"

_InvalidGraphEdgeError_error = Core.Name "error"

-- | An error indicating that a property graph is invalid
data InvalidGraphError v =
  -- | An edge in the graph is invalid
  InvalidGraphErrorEdge (InvalidGraphEdgeError v) |
  -- | A vertex in the graph is invalid
  InvalidGraphErrorVertex (InvalidGraphVertexError v)
  deriving (Eq, Ord, Read, Show)

_InvalidGraphError = Core.Name "hydra.error.pg.InvalidGraphError"

_InvalidGraphError_edge = Core.Name "edge"

_InvalidGraphError_vertex = Core.Name "vertex"

-- | An invalid vertex within a graph, identified by its id
data InvalidGraphVertexError v =
  InvalidGraphVertexError {
    -- | The id of the invalid vertex
    invalidGraphVertexErrorId :: v,
    -- | The specific error
    invalidGraphVertexErrorError :: InvalidVertexError}
  deriving (Eq, Ord, Read, Show)

_InvalidGraphVertexError = Core.Name "hydra.error.pg.InvalidGraphVertexError"

_InvalidGraphVertexError_id = Core.Name "id"

_InvalidGraphVertexError_error = Core.Name "error"

-- | An error indicating that a property is invalid
data InvalidPropertyError =
  -- | The property value failed type validation
  InvalidPropertyErrorInvalidValue InvalidValueError |
  -- | A required property is missing
  InvalidPropertyErrorMissingRequired Model.PropertyKey |
  -- | A property has an unexpected key not in the schema
  InvalidPropertyErrorUnexpectedKey Model.PropertyKey
  deriving (Eq, Ord, Read, Show)

_InvalidPropertyError = Core.Name "hydra.error.pg.InvalidPropertyError"

_InvalidPropertyError_invalidValue = Core.Name "invalidValue"

_InvalidPropertyError_missingRequired = Core.Name "missingRequired"

_InvalidPropertyError_unexpectedKey = Core.Name "unexpectedKey"

-- | An error indicating that a value does not match the expected type
data InvalidValueError =
  InvalidValueError {
    -- | The expected type, as a string
    invalidValueErrorExpectedType :: String,
    -- | The actual value, as a string
    invalidValueErrorValue :: String}
  deriving (Eq, Ord, Read, Show)

_InvalidValueError = Core.Name "hydra.error.pg.InvalidValueError"

_InvalidValueError_expectedType = Core.Name "expectedType"

_InvalidValueError_value = Core.Name "value"

-- | An error indicating that a vertex is invalid
data InvalidVertexError =
  -- | The vertex id value is invalid
  InvalidVertexErrorId InvalidValueError |
  -- | The vertex label does not exist in the schema
  InvalidVertexErrorLabel NoSuchVertexLabelError |
  -- | A property of the vertex is invalid
  InvalidVertexErrorProperty InvalidElementPropertyError
  deriving (Eq, Ord, Read, Show)

_InvalidVertexError = Core.Name "hydra.error.pg.InvalidVertexError"

_InvalidVertexError_id = Core.Name "id"

_InvalidVertexError_label = Core.Name "label"

_InvalidVertexError_property = Core.Name "property"

-- | An error indicating that an edge label does not exist in the schema
data NoSuchEdgeLabelError =
  NoSuchEdgeLabelError {
    -- | The edge label that was not found
    noSuchEdgeLabelErrorLabel :: Model.EdgeLabel}
  deriving (Eq, Ord, Read, Show)

_NoSuchEdgeLabelError = Core.Name "hydra.error.pg.NoSuchEdgeLabelError"

_NoSuchEdgeLabelError_label = Core.Name "label"

-- | An error indicating that a vertex label does not exist in the schema
data NoSuchVertexLabelError =
  NoSuchVertexLabelError {
    -- | The vertex label that was not found
    noSuchVertexLabelErrorLabel :: Model.VertexLabel}
  deriving (Eq, Ord, Read, Show)

_NoSuchVertexLabelError = Core.Name "hydra.error.pg.NoSuchVertexLabelError"

_NoSuchVertexLabelError_label = Core.Name "label"

-- | An error indicating that a vertex has the wrong label
data WrongVertexLabelError =
  WrongVertexLabelError {
    -- | The expected vertex label
    wrongVertexLabelErrorExpected :: Model.VertexLabel,
    -- | The actual vertex label
    wrongVertexLabelErrorActual :: Model.VertexLabel}
  deriving (Eq, Ord, Read, Show)

_WrongVertexLabelError = Core.Name "hydra.error.pg.WrongVertexLabelError"

_WrongVertexLabelError_expected = Core.Name "expected"

_WrongVertexLabelError_actual = Core.Name "actual"
