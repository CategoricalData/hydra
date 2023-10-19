-- | A model for property graph validation errors

module Hydra.Langs.Tinkerpop.Errors where

import qualified Hydra.Core as Core
import qualified Hydra.Langs.Tinkerpop.PropertyGraph as PropertyGraph
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

data TypeError t v = 
  TypeError {
    -- | An expected type
    typeErrorExpectedType :: t,
    -- | The actual value, which does not conform to the expected type
    typeErrorActualValue :: v}
  deriving (Eq, Ord, Read, Show)

_TypeError = (Core.Name "hydra/langs/tinkerpop/errors.TypeError")

_TypeError_expectedType = (Core.FieldName "expectedType")

_TypeError_actualValue = (Core.FieldName "actualValue")

data ElementValidationError t v = 
  -- | A vertex validation error
  ElementValidationErrorVertex (VertexValidationError t v) |
  -- | An edge validation error
  ElementValidationErrorEdge (EdgeValidationError t v)
  deriving (Eq, Ord, Read, Show)

_ElementValidationError = (Core.Name "hydra/langs/tinkerpop/errors.ElementValidationError")

_ElementValidationError_vertex = (Core.FieldName "vertex")

_ElementValidationError_edge = (Core.FieldName "edge")

data VertexValidationError t v = 
  VertexValidationError {
    -- | The id of the vertex which failed validation
    vertexValidationErrorId :: v,
    -- | A specific validation error for the vertex
    vertexValidationErrorError :: (BadVertex t v)}
  deriving (Eq, Ord, Read, Show)

_VertexValidationError = (Core.Name "hydra/langs/tinkerpop/errors.VertexValidationError")

_VertexValidationError_id = (Core.FieldName "id")

_VertexValidationError_error = (Core.FieldName "error")

data EdgeValidationError t v = 
  EdgeValidationError {
    -- | The id of the edge which failed validation
    edgeValidationErrorId :: v,
    -- | A specific validation error for the edge
    edgeValidationErrorError :: (BadEdge t v)}
  deriving (Eq, Ord, Read, Show)

_EdgeValidationError = (Core.Name "hydra/langs/tinkerpop/errors.EdgeValidationError")

_EdgeValidationError_id = (Core.FieldName "id")

_EdgeValidationError_error = (Core.FieldName "error")

data BadVertex t v = 
  -- | A vertex label mismatch
  BadVertexLabel VertexLabelMismatch |
  -- | The label of the vertex does not have an associated vertex type
  BadVertexLabelUnexpected PropertyGraph.VertexLabel |
  -- | A vertex id type error
  BadVertexId (TypeError t v) |
  -- | A vertex property error
  BadVertexProperty (BadProperty t v)
  deriving (Eq, Ord, Read, Show)

_BadVertex = (Core.Name "hydra/langs/tinkerpop/errors.BadVertex")

_BadVertex_label = (Core.FieldName "label")

_BadVertex_labelUnexpected = (Core.FieldName "labelUnexpected")

_BadVertex_id = (Core.FieldName "id")

_BadVertex_property = (Core.FieldName "property")

data BadEdge t v = 
  -- | An edge label mismatch
  BadEdgeLabel EdgeLabelMismatch |
  -- | The label of the edge does not have an associated edge type
  BadEdgeLabelUnexpected PropertyGraph.EdgeLabel |
  -- | An edge id type error
  BadEdgeId (TypeError t v) |
  -- | An edge property error
  BadEdgeProperty (BadProperty t v) |
  -- | The out-vertex of the edge does not exist
  BadEdgeNoSuchOutVertex v |
  -- | The in-vertex of the edge does not exist
  BadEdgeNoSuchInVertex v |
  -- | The out-vertex of the edge has the wrong label
  BadEdgeWrongOutVertexLabel VertexLabelMismatch |
  -- | The in-vertex of the edge has the wrong label
  BadEdgeWrongInVertexLabel VertexLabelMismatch
  deriving (Eq, Ord, Read, Show)

_BadEdge = (Core.Name "hydra/langs/tinkerpop/errors.BadEdge")

_BadEdge_label = (Core.FieldName "label")

_BadEdge_labelUnexpected = (Core.FieldName "labelUnexpected")

_BadEdge_id = (Core.FieldName "id")

_BadEdge_property = (Core.FieldName "property")

_BadEdge_noSuchOutVertex = (Core.FieldName "noSuchOutVertex")

_BadEdge_noSuchInVertex = (Core.FieldName "noSuchInVertex")

_BadEdge_wrongOutVertexLabel = (Core.FieldName "wrongOutVertexLabel")

_BadEdge_wrongInVertexLabel = (Core.FieldName "wrongInVertexLabel")

data BadProperty t v = 
  -- | The property key does not have an associated type
  BadPropertyUnexpectedKey PropertyGraph.PropertyKey |
  -- | A required property is missing
  BadPropertyMissingKey PropertyGraph.PropertyKey |
  -- | A property value is invalid
  BadPropertyValue (TypeError t v)
  deriving (Eq, Ord, Read, Show)

_BadProperty = (Core.Name "hydra/langs/tinkerpop/errors.BadProperty")

_BadProperty_unexpectedKey = (Core.FieldName "unexpectedKey")

_BadProperty_missingKey = (Core.FieldName "missingKey")

_BadProperty_value = (Core.FieldName "value")

data VertexLabelMismatch = 
  VertexLabelMismatch {
    -- | The expected vertex label, based on the vertex type
    vertexLabelMismatchExpected :: PropertyGraph.VertexLabel,
    -- | The actual vertex label
    vertexLabelMismatchActual :: PropertyGraph.VertexLabel}
  deriving (Eq, Ord, Read, Show)

_VertexLabelMismatch = (Core.Name "hydra/langs/tinkerpop/errors.VertexLabelMismatch")

_VertexLabelMismatch_expected = (Core.FieldName "expected")

_VertexLabelMismatch_actual = (Core.FieldName "actual")

data EdgeLabelMismatch = 
  EdgeLabelMismatch {
    -- | The expected edge label, based on the edge type
    edgeLabelMismatchExpected :: PropertyGraph.EdgeLabel,
    -- | The actual edge label
    edgeLabelMismatchActual :: PropertyGraph.EdgeLabel}
  deriving (Eq, Ord, Read, Show)

_EdgeLabelMismatch = (Core.Name "hydra/langs/tinkerpop/errors.EdgeLabelMismatch")

_EdgeLabelMismatch_expected = (Core.FieldName "expected")

_EdgeLabelMismatch_actual = (Core.FieldName "actual")