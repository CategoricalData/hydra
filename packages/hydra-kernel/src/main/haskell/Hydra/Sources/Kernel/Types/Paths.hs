module Hydra.Sources.Kernel.Types.Paths where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.paths"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = (map toTypeDef definitions),
            moduleTermDependencies = [Core.ns],
            moduleTypeDependencies = [Core.ns],
            moduleDescription = Just "A model for subterm and subtype access patterns"}
  where
    definitions = [
      subtermEdge,
      subtermGraph,
      subtermNode,
      subtermPath,
      subtermStep,
      subtypeEdge,
      subtypeGraph,
      subtypeNode,
      subtypePath,
      subtypeStep]

-- Subterm types

subtermEdge :: Binding
subtermEdge = define "SubtermEdge" $
  doc "An edge in a subterm graph, connecting two nodes via a path" $
  T.record [
    "source">:
      doc "The source node of the edge"
      subtermNode,
    "path">:
      doc "The subterm path connecting source to target"
      subtermPath,
    "target">:
      doc "The target node of the edge"
      subtermNode]

subtermGraph :: Binding
subtermGraph = define "SubtermGraph" $
  doc "A graph of subterm nodes and edges, representing term access patterns" $
  T.record [
    "nodes">:
      doc "All nodes in the graph" $
      T.list subtermNode,
    "edges">:
      doc "All edges in the graph" $
      T.list subtermEdge]

subtermNode :: Binding
subtermNode = define "SubtermNode" $
  doc "A node in a subterm graph, representing a term or subterm" $
  T.record [
    "name">:
      doc "The qualified name of the term"
      Core.name,
    "label">:
      doc "A human-readable label for the node"
      T.string,
    "id" >:
      doc "A unique identifier for the node"
      T.string]

subtermPath :: Binding
subtermPath = define "SubtermPath" $
  doc "A sequence of subterm steps forming a path through a term" $
  T.wrap $ T.list subtermStep

subtermStep :: Binding
subtermStep = define "SubtermStep" $
  doc "A function which maps from a term to a particular immediate subterm" $
  T.union [
    "annotatedBody">:
      doc "Access the body of an annotated term"
      T.unit,
    "applicationFunction">:
      doc "Access the function of an application term"
      T.unit,
    "applicationArgument">:
      doc "Access the argument of an application term"
      T.unit,
    "lambdaBody">:
      doc "Access the body of a lambda term"
      T.unit,
    "unionCasesDefault">:
      doc "Access the default case of a union elimination"
      T.unit,
    "unionCasesBranch">:
      doc "Access a specific branch of a union elimination by field name"
      Core.name,
    "letBody">:
      doc "Access the body of a let term"
      T.unit,
    "letBinding">:
      doc "Access a specific binding in a let term by variable name"
      Core.name,
    "listElement">:
      doc "Access an element of a list by index"
      T.int32,
    "mapKey">:
      doc "Access a key in a map by index"
      T.int32,
    "mapValue">:
      doc "Access a value in a map by index"
      T.int32,
    "maybeTerm">:
      doc "Access the term inside a Just value"
      T.unit,
    "productTerm">:
      doc "Access an element of a product (tuple) by index"
      T.int32,
    "recordField">:
      doc "Access a field of a record by field name"
      Core.name,
    "setElement">:
      doc "Access an element of a set by index"
      T.int32,
    "sumTerm">:
      doc "Access the term inside a sum variant"
      T.unit,
    "typeLambdaBody">:
      doc "Access the body of a type lambda term"
      T.unit,
    "typeApplicationTerm">:
      doc "Access the term being applied to a type"
      T.unit,
    "injectionTerm">:
      doc "Access the term inside a union injection"
      T.unit,
    "wrappedTerm">:
      doc "Access the term inside a wrapped term"
      T.unit]

-- Subtype types

subtypeEdge :: Binding
subtypeEdge = define "SubtypeEdge" $
  doc "An edge in a subtype graph, connecting two nodes via a path" $
  T.record [
    "source">:
      doc "The source node of the edge"
      subtypeNode,
    "path">:
      doc "The subtype path connecting source to target"
      subtypePath,
    "target">:
      doc "The target node of the edge"
      subtypeNode]

subtypeGraph :: Binding
subtypeGraph = define "SubtypeGraph" $
  doc "A graph of subtype nodes and edges, representing type access patterns" $
  T.record [
    "nodes">:
      doc "All nodes in the graph" $
      T.list subtypeNode,
    "edges">:
      doc "All edges in the graph" $
      T.list subtypeEdge]

subtypeNode :: Binding
subtypeNode = define "SubtypeNode" $
  doc "A node in a subtype graph, representing a type or subtype" $
  T.record [
    "name">:
      doc "The qualified name of the type"
      Core.name,
    "label">:
      doc "A human-readable label for the node"
      T.string,
    "id" >:
      doc "A unique identifier for the node"
      T.string]

subtypePath :: Binding
subtypePath = define "SubtypePath" $
  doc "A sequence of subtype steps forming a path through a type" $
  T.wrap $ T.list subtypeStep

subtypeStep :: Binding
subtypeStep = define "SubtypeStep" $
  doc "A function which maps from a type to a particular immediate subtype" $
  T.union [
    "annotatedBody">:
      doc "Access the body of an annotated type"
      T.unit,
    "applicationFunction">:
      doc "Access the function of an application type"
      T.unit,
    "applicationArgument">:
      doc "Access the argument of an application type"
      T.unit,
    "eitherLeft">:
      doc "Access the left type of an either type"
      T.unit,
    "eitherRight">:
      doc "Access the right type of an either type"
      T.unit,
    "forallBody">:
      doc "Access the body of a universally quantified type"
      T.unit,
    "functionDomain">:
      doc "Access the domain type of a function type"
      T.unit,
    "functionCodomain">:
      doc "Access the codomain type of a function type"
      T.unit,
    "listElement">:
      doc "Access the element type of a list type"
      T.unit,
    "mapKeys">:
      doc "Access the key type of a map type"
      T.unit,
    "mapValues">:
      doc "Access the value type of a map type"
      T.unit,
    "maybeElement">:
      doc "Access the element type of an optional type"
      T.unit,
    "pairFirst">:
      doc "Access the first type of a pair type"
      T.unit,
    "pairSecond">:
      doc "Access the second type of a pair type"
      T.unit,
    "recordField">:
      doc "Access a field type of a record type by field name"
      Core.name,
    "setElement">:
      doc "Access the element type of a set type"
      T.unit,
    "unionField">:
      doc "Access a field type of a union type by field name"
      Core.name,
    "wrappedType">:
      doc "Access the type inside a wrapped type"
      T.unit]
