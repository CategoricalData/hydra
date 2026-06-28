module Hydra.Sources.Error.Neo4j where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T
import qualified Hydra.Sources.Neo4j.Model      as Neo4jModel


ns :: ModuleName
ns = ModuleName "hydra.error.neo4j"

define :: String -> Type -> TypeDefinition
define = defineType ns

neo4j :: String -> Type
neo4j = typeref Neo4jModel.ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Neo4jModel.ns],
            moduleMetadata = descriptionMetadata (Just "Error types for Neo4j property graph validation")}
  where
    definitions = [
      invalidGraphError,
      invalidGraphNodeError,
      invalidGraphRelationshipError,
      invalidNodeError,
      invalidRelationshipError,
      keyError,
      missingLabelError,
      noMatchingPatternError,
      noSuchLabelError,
      noSuchRelationshipTypeError,
      propertyExistenceError,
      propertyTypeError,
      propertyUniquenessError,
      relationshipPattern]

invalidGraphError :: TypeDefinition
invalidGraphError = define "InvalidGraphError" $
  doc "An error indicating that a Neo4j property graph is invalid" $
  T.union [
    "node">:
      doc "A node in the graph is invalid" $
      invalidGraphNodeError,
    "relationship">:
      doc "A relationship in the graph is invalid" $
      invalidGraphRelationshipError]

invalidGraphNodeError :: TypeDefinition
invalidGraphNodeError = define "InvalidGraphNodeError" $
  doc "An invalid node within a graph, identified by its element id" $
  T.record [
    "id">:
      doc "The element id of the invalid node" $
      neo4j "ElementId",
    "error">:
      doc "The specific error" $
      invalidNodeError]

invalidGraphRelationshipError :: TypeDefinition
invalidGraphRelationshipError = define "InvalidGraphRelationshipError" $
  doc "An invalid relationship within a graph, identified by its element id" $
  T.record [
    "id">:
      doc "The element id of the invalid relationship" $
      neo4j "ElementId",
    "error">:
      doc "The specific error" $
      invalidRelationshipError]

invalidNodeError :: TypeDefinition
invalidNodeError = define "InvalidNodeError" $
  doc "An error indicating that a node is invalid" $
  T.union [
    "keyViolation">:
      doc "A key constraint is violated" $
      keyError,
    "missingImpliedLabel">:
      doc "The node is missing a label implied by its node element type" $
      missingLabelError,
    "missingProperty">:
      doc "The node is missing a property required by a property existence constraint" $
      propertyExistenceError,
    "noSuchLabel">:
      doc "The node's labels do not match any node element type in the graph type" $
      noSuchLabelError,
    "uniquenessViolation">:
      doc "A property uniqueness constraint is violated" $
      propertyUniquenessError,
    "wrongPropertyType">:
      doc "A property of the node violates a property type constraint" $
      propertyTypeError]

invalidRelationshipError :: TypeDefinition
invalidRelationshipError = define "InvalidRelationshipError" $
  doc "An error indicating that a relationship is invalid" $
  T.union [
    "endNodeNotFound">:
      doc "The end node does not exist in the graph" $
      T.unit,
    "keyViolation">:
      doc "A key constraint is violated" $
      keyError,
    "missingProperty">:
      doc "The relationship is missing a property required by a property existence constraint" $
      propertyExistenceError,
    "noMatchingPattern">:
      doc ("The relationship's type is declared, but its endpoint labels match none of the declared "
        ++ "source/target patterns for that type") $
      noMatchingPatternError,
    "noSuchType">:
      doc "The relationship's type does not match any relationship element type in the graph type" $
      noSuchRelationshipTypeError,
    "startNodeNotFound">:
      doc "The start node does not exist in the graph" $
      T.unit,
    "uniquenessViolation">:
      doc "A property uniqueness constraint is violated" $
      propertyUniquenessError,
    "wrongPropertyType">:
      doc "A property of the relationship violates a property type constraint" $
      propertyTypeError]

keyError :: TypeDefinition
keyError = define "KeyError" $
  doc ("An error indicating that a key constraint is violated: the listed properties do not all exist, "
    ++ "or their combined values are not unique") $
  T.record [
    "properties">:
      doc "The properties that constitute the key" $
      T.list (neo4j "Key")]

missingLabelError :: TypeDefinition
missingLabelError = define "MissingLabelError" $
  doc "An error indicating that a node is missing a label implied by its node element type" $
  T.record [
    "label">:
      doc "The implied label that was not present on the node" $
      neo4j "NodeLabel"]

propertyExistenceError :: TypeDefinition
propertyExistenceError = define "PropertyExistenceError" $
  doc "An error indicating that an element is missing a property required by a property existence constraint" $
  T.record [
    "key">:
      doc "The key of the missing property" $
      neo4j "Key"]

noMatchingPatternError :: TypeDefinition
noMatchingPatternError = define "NoMatchingPatternError" $
  doc ("An error indicating that a relationship's endpoint labels match none of the declared "
    ++ "source/target patterns for its relationship type") $
  T.record [
    "allowedPatterns">:
      doc "The declared source/target label patterns for the relationship's type" $
      T.list relationshipPattern,
    "actualStartLabels">:
      doc "The labels of the relationship's actual start node" $
      T.list (neo4j "NodeLabel"),
    "actualEndLabels">:
      doc "The labels of the relationship's actual end node" $
      T.list (neo4j "NodeLabel")]

noSuchLabelError :: TypeDefinition
noSuchLabelError = define "NoSuchLabelError" $
  doc "An error indicating that no node element type matches a node's labels" $
  T.record [
    "labels">:
      doc "The labels of the node for which no node element type was found" $
      T.list (neo4j "NodeLabel")]

noSuchRelationshipTypeError :: TypeDefinition
noSuchRelationshipTypeError = define "NoSuchRelationshipTypeError" $
  doc "An error indicating that a relationship type does not match any relationship element type" $
  T.record [
    "type">:
      doc "The relationship type that was not found" $
      neo4j "RelationshipType"]

propertyTypeError :: TypeDefinition
propertyTypeError = define "PropertyTypeError" $
  doc "An error indicating that a property's value violates a property type constraint" $
  T.record [
    "key">:
      doc "The key of the property" $
      neo4j "Key",
    "expectedType">:
      doc "The type the property's value was required to have" $
      neo4j "ValueType",
    "value">:
      doc "The actual value of the property" $
      neo4j "Value"]

propertyUniquenessError :: TypeDefinition
propertyUniquenessError = define "PropertyUniquenessError" $
  doc ("An error indicating that a property uniqueness constraint is violated: the combined values of "
    ++ "the listed properties are not unique") $
  T.record [
    "properties">:
      doc "The properties whose combined values must be unique" $
      T.list (neo4j "Key")]

relationshipPattern :: TypeDefinition
relationshipPattern = define "RelationshipPattern" $
  doc "A declared source/target label pattern for a relationship type, used in NoMatchingPatternError" $
  T.record [
    "startLabel">:
      doc "The required start-node label" $
      neo4j "NodeLabel",
    "endLabel">:
      doc "The required end-node label" $
      neo4j "NodeLabel"]
