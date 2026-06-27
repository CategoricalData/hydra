-- Note: this is an automatically generated file. Do not edit.
-- | Error types for Neo4j property graph validation

module Hydra.Error.Neo4j where
import qualified Hydra.Core as Core
import qualified Hydra.Neo4j.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | An error indicating that a Neo4j property graph is invalid
data InvalidGraphError =
  -- | A node in the graph is invalid
  InvalidGraphErrorNode InvalidGraphNodeError |
  -- | A relationship in the graph is invalid
  InvalidGraphErrorRelationship InvalidGraphRelationshipError
  deriving (Eq, Ord, Read, Show)
_InvalidGraphError = Core.Name "hydra.error.neo4j.InvalidGraphError"
_InvalidGraphError_node = Core.Name "node"
_InvalidGraphError_relationship = Core.Name "relationship"
-- | An invalid node within a graph, identified by its element id
data InvalidGraphNodeError =
  InvalidGraphNodeError {
    -- | The element id of the invalid node
    invalidGraphNodeErrorId :: Model.ElementId,
    -- | The specific error
    invalidGraphNodeErrorError :: InvalidNodeError}
  deriving (Eq, Ord, Read, Show)
_InvalidGraphNodeError = Core.Name "hydra.error.neo4j.InvalidGraphNodeError"
_InvalidGraphNodeError_id = Core.Name "id"
_InvalidGraphNodeError_error = Core.Name "error"
-- | An invalid relationship within a graph, identified by its element id
data InvalidGraphRelationshipError =
  InvalidGraphRelationshipError {
    -- | The element id of the invalid relationship
    invalidGraphRelationshipErrorId :: Model.ElementId,
    -- | The specific error
    invalidGraphRelationshipErrorError :: InvalidRelationshipError}
  deriving (Eq, Ord, Read, Show)
_InvalidGraphRelationshipError = Core.Name "hydra.error.neo4j.InvalidGraphRelationshipError"
_InvalidGraphRelationshipError_id = Core.Name "id"
_InvalidGraphRelationshipError_error = Core.Name "error"
-- | An error indicating that a node is invalid
data InvalidNodeError =
  -- | A key constraint is violated
  InvalidNodeErrorKeyViolation KeyError |
  -- | The node is missing a label implied by its node element type
  InvalidNodeErrorMissingImpliedLabel MissingLabelError |
  -- | The node is missing a property required by a property existence constraint
  InvalidNodeErrorMissingProperty PropertyExistenceError |
  -- | The node's labels do not match any node element type in the graph type
  InvalidNodeErrorNoSuchLabel NoSuchLabelError |
  -- | A property uniqueness constraint is violated
  InvalidNodeErrorUniquenessViolation PropertyUniquenessError |
  -- | A property of the node violates a property type constraint
  InvalidNodeErrorWrongPropertyType PropertyTypeError
  deriving (Eq, Ord, Read, Show)
_InvalidNodeError = Core.Name "hydra.error.neo4j.InvalidNodeError"
_InvalidNodeError_keyViolation = Core.Name "keyViolation"
_InvalidNodeError_missingImpliedLabel = Core.Name "missingImpliedLabel"
_InvalidNodeError_missingProperty = Core.Name "missingProperty"
_InvalidNodeError_noSuchLabel = Core.Name "noSuchLabel"
_InvalidNodeError_uniquenessViolation = Core.Name "uniquenessViolation"
_InvalidNodeError_wrongPropertyType = Core.Name "wrongPropertyType"
-- | An error indicating that a relationship is invalid
data InvalidRelationshipError =
  -- | The end node does not exist in the graph
  InvalidRelationshipErrorEndNodeNotFound |
  -- | A key constraint is violated
  InvalidRelationshipErrorKeyViolation KeyError |
  -- | The relationship is missing a property required by a property existence constraint
  InvalidRelationshipErrorMissingProperty PropertyExistenceError |
  -- | The relationship's type is declared, but its endpoint labels match none of the declared source/target patterns for that type
  InvalidRelationshipErrorNoMatchingPattern NoMatchingPatternError |
  -- | The relationship's type does not match any relationship element type in the graph type
  InvalidRelationshipErrorNoSuchType NoSuchRelationshipTypeError |
  -- | The start node does not exist in the graph
  InvalidRelationshipErrorStartNodeNotFound |
  -- | A property uniqueness constraint is violated
  InvalidRelationshipErrorUniquenessViolation PropertyUniquenessError |
  -- | A property of the relationship violates a property type constraint
  InvalidRelationshipErrorWrongPropertyType PropertyTypeError
  deriving (Eq, Ord, Read, Show)
_InvalidRelationshipError = Core.Name "hydra.error.neo4j.InvalidRelationshipError"
_InvalidRelationshipError_endNodeNotFound = Core.Name "endNodeNotFound"
_InvalidRelationshipError_keyViolation = Core.Name "keyViolation"
_InvalidRelationshipError_missingProperty = Core.Name "missingProperty"
_InvalidRelationshipError_noMatchingPattern = Core.Name "noMatchingPattern"
_InvalidRelationshipError_noSuchType = Core.Name "noSuchType"
_InvalidRelationshipError_startNodeNotFound = Core.Name "startNodeNotFound"
_InvalidRelationshipError_uniquenessViolation = Core.Name "uniquenessViolation"
_InvalidRelationshipError_wrongPropertyType = Core.Name "wrongPropertyType"
-- | An error indicating that a key constraint is violated: the listed properties do not all exist, or their combined values are not unique
data KeyError =
  KeyError {
    -- | The properties that constitute the key
    keyErrorProperties :: [Model.Key]}
  deriving (Eq, Ord, Read, Show)
_KeyError = Core.Name "hydra.error.neo4j.KeyError"
_KeyError_properties = Core.Name "properties"
-- | An error indicating that a node is missing a label implied by its node element type
data MissingLabelError =
  MissingLabelError {
    -- | The implied label that was not present on the node
    missingLabelErrorLabel :: Model.NodeLabel}
  deriving (Eq, Ord, Read, Show)
_MissingLabelError = Core.Name "hydra.error.neo4j.MissingLabelError"
_MissingLabelError_label = Core.Name "label"
-- | An error indicating that a relationship's endpoint labels match none of the declared source/target patterns for its relationship type
data NoMatchingPatternError =
  NoMatchingPatternError {
    -- | The declared source/target label patterns for the relationship's type
    noMatchingPatternErrorAllowedPatterns :: [RelationshipPattern],
    -- | The labels of the relationship's actual start node
    noMatchingPatternErrorActualStartLabels :: [Model.NodeLabel],
    -- | The labels of the relationship's actual end node
    noMatchingPatternErrorActualEndLabels :: [Model.NodeLabel]}
  deriving (Eq, Ord, Read, Show)
_NoMatchingPatternError = Core.Name "hydra.error.neo4j.NoMatchingPatternError"
_NoMatchingPatternError_allowedPatterns = Core.Name "allowedPatterns"
_NoMatchingPatternError_actualStartLabels = Core.Name "actualStartLabels"
_NoMatchingPatternError_actualEndLabels = Core.Name "actualEndLabels"
-- | An error indicating that no node element type matches a node's labels
data NoSuchLabelError =
  NoSuchLabelError {
    -- | The labels of the node for which no node element type was found
    noSuchLabelErrorLabels :: [Model.NodeLabel]}
  deriving (Eq, Ord, Read, Show)
_NoSuchLabelError = Core.Name "hydra.error.neo4j.NoSuchLabelError"
_NoSuchLabelError_labels = Core.Name "labels"
-- | An error indicating that a relationship type does not match any relationship element type
data NoSuchRelationshipTypeError =
  NoSuchRelationshipTypeError {
    -- | The relationship type that was not found
    noSuchRelationshipTypeErrorType :: Model.RelationshipType}
  deriving (Eq, Ord, Read, Show)
_NoSuchRelationshipTypeError = Core.Name "hydra.error.neo4j.NoSuchRelationshipTypeError"
_NoSuchRelationshipTypeError_type = Core.Name "type"
-- | An error indicating that an element is missing a property required by a property existence constraint
data PropertyExistenceError =
  PropertyExistenceError {
    -- | The key of the missing property
    propertyExistenceErrorKey :: Model.Key}
  deriving (Eq, Ord, Read, Show)
_PropertyExistenceError = Core.Name "hydra.error.neo4j.PropertyExistenceError"
_PropertyExistenceError_key = Core.Name "key"
-- | An error indicating that a property's value violates a property type constraint
data PropertyTypeError =
  PropertyTypeError {
    -- | The key of the property
    propertyTypeErrorKey :: Model.Key,
    -- | The type the property's value was required to have
    propertyTypeErrorExpectedType :: Model.ValueType,
    -- | The actual value of the property
    propertyTypeErrorValue :: Model.Value}
  deriving (Eq, Ord, Read, Show)
_PropertyTypeError = Core.Name "hydra.error.neo4j.PropertyTypeError"
_PropertyTypeError_key = Core.Name "key"
_PropertyTypeError_expectedType = Core.Name "expectedType"
_PropertyTypeError_value = Core.Name "value"
-- | An error indicating that a property uniqueness constraint is violated: the combined values of the listed properties are not unique
data PropertyUniquenessError =
  PropertyUniquenessError {
    -- | The properties whose combined values must be unique
    propertyUniquenessErrorProperties :: [Model.Key]}
  deriving (Eq, Ord, Read, Show)
_PropertyUniquenessError = Core.Name "hydra.error.neo4j.PropertyUniquenessError"
_PropertyUniquenessError_properties = Core.Name "properties"
-- | A declared source/target label pattern for a relationship type, used in NoMatchingPatternError
data RelationshipPattern =
  RelationshipPattern {
    -- | The required start-node label
    relationshipPatternStartLabel :: Model.NodeLabel,
    -- | The required end-node label
    relationshipPatternEndLabel :: Model.NodeLabel}
  deriving (Eq, Ord, Read, Show)
_RelationshipPattern = Core.Name "hydra.error.neo4j.RelationshipPattern"
_RelationshipPattern_startLabel = Core.Name "startLabel"
_RelationshipPattern_endLabel = Core.Name "endLabel"
