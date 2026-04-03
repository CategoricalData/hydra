-- Note: this is an automatically generated file. Do not edit.

-- | A model for subterm and subtype access patterns

module Hydra.Paths where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | An edge in a subterm graph, connecting two nodes via a path
data SubtermEdge =
  SubtermEdge {
    -- | The source node of the edge
    subtermEdgeSource :: SubtermNode,
    -- | The subterm path connecting source to target
    subtermEdgePath :: SubtermPath,
    -- | The target node of the edge
    subtermEdgeTarget :: SubtermNode}
  deriving (Eq, Ord, Read, Show)

_SubtermEdge = Core.Name "hydra.paths.SubtermEdge"

_SubtermEdge_source = Core.Name "source"

_SubtermEdge_path = Core.Name "path"

_SubtermEdge_target = Core.Name "target"

-- | A graph of subterm nodes and edges, representing term access patterns
data SubtermGraph =
  SubtermGraph {
    -- | All nodes in the graph
    subtermGraphNodes :: [SubtermNode],
    -- | All edges in the graph
    subtermGraphEdges :: [SubtermEdge]}
  deriving (Eq, Ord, Read, Show)

_SubtermGraph = Core.Name "hydra.paths.SubtermGraph"

_SubtermGraph_nodes = Core.Name "nodes"

_SubtermGraph_edges = Core.Name "edges"

-- | A node in a subterm graph, representing a term or subterm
data SubtermNode =
  SubtermNode {
    -- | The qualified name of the term
    subtermNodeName :: Core.Name,
    -- | A human-readable label for the node
    subtermNodeLabel :: String,
    -- | A unique identifier for the node
    subtermNodeId :: String}
  deriving (Eq, Ord, Read, Show)

_SubtermNode = Core.Name "hydra.paths.SubtermNode"

_SubtermNode_name = Core.Name "name"

_SubtermNode_label = Core.Name "label"

_SubtermNode_id = Core.Name "id"

-- | A sequence of subterm steps forming a path through a term
newtype SubtermPath =
  SubtermPath {
    unSubtermPath :: [SubtermStep]}
  deriving (Eq, Ord, Read, Show)

_SubtermPath = Core.Name "hydra.paths.SubtermPath"

-- | A function which maps from a term to a particular immediate subterm
data SubtermStep =
  -- | Access the body of an annotated term
  SubtermStepAnnotatedBody  |
  -- | Access the function of an application term
  SubtermStepApplicationFunction  |
  -- | Access the argument of an application term
  SubtermStepApplicationArgument  |
  -- | Access the body of a lambda term
  SubtermStepLambdaBody  |
  -- | Access the default case of a union elimination
  SubtermStepUnionCasesDefault  |
  -- | Access a specific branch of a union elimination by field name
  SubtermStepUnionCasesBranch Core.Name |
  -- | Access the body of a let term
  SubtermStepLetBody  |
  -- | Access a specific binding in a let term by variable name
  SubtermStepLetBinding Core.Name |
  -- | Access an element of a list by index
  SubtermStepListElement Int |
  -- | Access a key in a map by index
  SubtermStepMapKey Int |
  -- | Access a value in a map by index
  SubtermStepMapValue Int |
  -- | Access the term inside a Just value
  SubtermStepMaybeTerm  |
  -- | Access an element of a product (tuple) by index
  SubtermStepProductTerm Int |
  -- | Access a field of a record by field name
  SubtermStepRecordField Core.Name |
  -- | Access an element of a set by index
  SubtermStepSetElement Int |
  -- | Access the term inside a sum variant
  SubtermStepSumTerm  |
  -- | Access the body of a type lambda term
  SubtermStepTypeLambdaBody  |
  -- | Access the term being applied to a type
  SubtermStepTypeApplicationTerm  |
  -- | Access the term inside a union injection
  SubtermStepInjectionTerm  |
  -- | Access the term inside a wrapped term
  SubtermStepWrappedTerm
  deriving (Eq, Ord, Read, Show)

_SubtermStep = Core.Name "hydra.paths.SubtermStep"

_SubtermStep_annotatedBody = Core.Name "annotatedBody"

_SubtermStep_applicationFunction = Core.Name "applicationFunction"

_SubtermStep_applicationArgument = Core.Name "applicationArgument"

_SubtermStep_lambdaBody = Core.Name "lambdaBody"

_SubtermStep_unionCasesDefault = Core.Name "unionCasesDefault"

_SubtermStep_unionCasesBranch = Core.Name "unionCasesBranch"

_SubtermStep_letBody = Core.Name "letBody"

_SubtermStep_letBinding = Core.Name "letBinding"

_SubtermStep_listElement = Core.Name "listElement"

_SubtermStep_mapKey = Core.Name "mapKey"

_SubtermStep_mapValue = Core.Name "mapValue"

_SubtermStep_maybeTerm = Core.Name "maybeTerm"

_SubtermStep_productTerm = Core.Name "productTerm"

_SubtermStep_recordField = Core.Name "recordField"

_SubtermStep_setElement = Core.Name "setElement"

_SubtermStep_sumTerm = Core.Name "sumTerm"

_SubtermStep_typeLambdaBody = Core.Name "typeLambdaBody"

_SubtermStep_typeApplicationTerm = Core.Name "typeApplicationTerm"

_SubtermStep_injectionTerm = Core.Name "injectionTerm"

_SubtermStep_wrappedTerm = Core.Name "wrappedTerm"

-- | An edge in a subtype graph, connecting two nodes via a path
data SubtypeEdge =
  SubtypeEdge {
    -- | The source node of the edge
    subtypeEdgeSource :: SubtypeNode,
    -- | The subtype path connecting source to target
    subtypeEdgePath :: SubtypePath,
    -- | The target node of the edge
    subtypeEdgeTarget :: SubtypeNode}
  deriving (Eq, Ord, Read, Show)

_SubtypeEdge = Core.Name "hydra.paths.SubtypeEdge"

_SubtypeEdge_source = Core.Name "source"

_SubtypeEdge_path = Core.Name "path"

_SubtypeEdge_target = Core.Name "target"

-- | A graph of subtype nodes and edges, representing type access patterns
data SubtypeGraph =
  SubtypeGraph {
    -- | All nodes in the graph
    subtypeGraphNodes :: [SubtypeNode],
    -- | All edges in the graph
    subtypeGraphEdges :: [SubtypeEdge]}
  deriving (Eq, Ord, Read, Show)

_SubtypeGraph = Core.Name "hydra.paths.SubtypeGraph"

_SubtypeGraph_nodes = Core.Name "nodes"

_SubtypeGraph_edges = Core.Name "edges"

-- | A node in a subtype graph, representing a type or subtype
data SubtypeNode =
  SubtypeNode {
    -- | The qualified name of the type
    subtypeNodeName :: Core.Name,
    -- | A human-readable label for the node
    subtypeNodeLabel :: String,
    -- | A unique identifier for the node
    subtypeNodeId :: String}
  deriving (Eq, Ord, Read, Show)

_SubtypeNode = Core.Name "hydra.paths.SubtypeNode"

_SubtypeNode_name = Core.Name "name"

_SubtypeNode_label = Core.Name "label"

_SubtypeNode_id = Core.Name "id"

-- | A sequence of subtype steps forming a path through a type
newtype SubtypePath =
  SubtypePath {
    unSubtypePath :: [SubtypeStep]}
  deriving (Eq, Ord, Read, Show)

_SubtypePath = Core.Name "hydra.paths.SubtypePath"

-- | A function which maps from a type to a particular immediate subtype
data SubtypeStep =
  -- | Access the body of an annotated type
  SubtypeStepAnnotatedBody  |
  -- | Access the function of an application type
  SubtypeStepApplicationFunction  |
  -- | Access the argument of an application type
  SubtypeStepApplicationArgument  |
  -- | Access the left type of an either type
  SubtypeStepEitherLeft  |
  -- | Access the right type of an either type
  SubtypeStepEitherRight  |
  -- | Access the body of a universally quantified type
  SubtypeStepForallBody  |
  -- | Access the domain type of a function type
  SubtypeStepFunctionDomain  |
  -- | Access the codomain type of a function type
  SubtypeStepFunctionCodomain  |
  -- | Access the element type of a list type
  SubtypeStepListElement  |
  -- | Access the key type of a map type
  SubtypeStepMapKeys  |
  -- | Access the value type of a map type
  SubtypeStepMapValues  |
  -- | Access the element type of an optional type
  SubtypeStepMaybeElement  |
  -- | Access the first type of a pair type
  SubtypeStepPairFirst  |
  -- | Access the second type of a pair type
  SubtypeStepPairSecond  |
  -- | Access a field type of a record type by field name
  SubtypeStepRecordField Core.Name |
  -- | Access the element type of a set type
  SubtypeStepSetElement  |
  -- | Access a field type of a union type by field name
  SubtypeStepUnionField Core.Name |
  -- | Access the type inside a wrapped type
  SubtypeStepWrappedType
  deriving (Eq, Ord, Read, Show)

_SubtypeStep = Core.Name "hydra.paths.SubtypeStep"

_SubtypeStep_annotatedBody = Core.Name "annotatedBody"

_SubtypeStep_applicationFunction = Core.Name "applicationFunction"

_SubtypeStep_applicationArgument = Core.Name "applicationArgument"

_SubtypeStep_eitherLeft = Core.Name "eitherLeft"

_SubtypeStep_eitherRight = Core.Name "eitherRight"

_SubtypeStep_forallBody = Core.Name "forallBody"

_SubtypeStep_functionDomain = Core.Name "functionDomain"

_SubtypeStep_functionCodomain = Core.Name "functionCodomain"

_SubtypeStep_listElement = Core.Name "listElement"

_SubtypeStep_mapKeys = Core.Name "mapKeys"

_SubtypeStep_mapValues = Core.Name "mapValues"

_SubtypeStep_maybeElement = Core.Name "maybeElement"

_SubtypeStep_pairFirst = Core.Name "pairFirst"

_SubtypeStep_pairSecond = Core.Name "pairSecond"

_SubtypeStep_recordField = Core.Name "recordField"

_SubtypeStep_setElement = Core.Name "setElement"

_SubtypeStep_unionField = Core.Name "unionField"

_SubtypeStep_wrappedType = Core.Name "wrappedType"
