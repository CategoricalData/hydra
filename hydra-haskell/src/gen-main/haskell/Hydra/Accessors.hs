-- Note: this is an automatically generated file. Do not edit.

-- | A model for term access patterns

module Hydra.Accessors where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | An edge in an accessor graph, connecting two nodes via a path
data AccessorEdge = 
  AccessorEdge {
    -- | The source node of the edge
    accessorEdgeSource :: AccessorNode,
    -- | The accessor path connecting source to target
    accessorEdgePath :: AccessorPath,
    -- | The target node of the edge
    accessorEdgeTarget :: AccessorNode}
  deriving (Eq, Ord, Read, Show)

_AccessorEdge = (Core.Name "hydra.accessors.AccessorEdge")

_AccessorEdge_source = (Core.Name "source")

_AccessorEdge_path = (Core.Name "path")

_AccessorEdge_target = (Core.Name "target")

-- | A graph of accessor nodes and edges, representing term access patterns
data AccessorGraph = 
  AccessorGraph {
    -- | All nodes in the graph
    accessorGraphNodes :: [AccessorNode],
    -- | All edges in the graph
    accessorGraphEdges :: [AccessorEdge]}
  deriving (Eq, Ord, Read, Show)

_AccessorGraph = (Core.Name "hydra.accessors.AccessorGraph")

_AccessorGraph_nodes = (Core.Name "nodes")

_AccessorGraph_edges = (Core.Name "edges")

-- | A node in an accessor graph, representing a term or subterm
data AccessorNode = 
  AccessorNode {
    -- | The qualified name of the term
    accessorNodeName :: Core.Name,
    -- | A human-readable label for the node
    accessorNodeLabel :: String,
    -- | A unique identifier for the node
    accessorNodeId :: String}
  deriving (Eq, Ord, Read, Show)

_AccessorNode = (Core.Name "hydra.accessors.AccessorNode")

_AccessorNode_name = (Core.Name "name")

_AccessorNode_label = (Core.Name "label")

_AccessorNode_id = (Core.Name "id")

-- | A sequence of term accessors forming a path through a term
newtype AccessorPath = 
  AccessorPath {
    unAccessorPath :: [TermAccessor]}
  deriving (Eq, Ord, Read, Show)

_AccessorPath = (Core.Name "hydra.accessors.AccessorPath")

-- | A function which maps from a term to a particular immediate subterm
data TermAccessor = 
  -- | Access the body of an annotated term
  TermAccessorAnnotatedBody  |
  -- | Access the function of an application term
  TermAccessorApplicationFunction  |
  -- | Access the argument of an application term
  TermAccessorApplicationArgument  |
  -- | Access the body of a lambda term
  TermAccessorLambdaBody  |
  -- | Access the default case of a union elimination
  TermAccessorUnionCasesDefault  |
  -- | Access a specific branch of a union elimination by field name
  TermAccessorUnionCasesBranch Core.Name |
  -- | Access the body of a let term
  TermAccessorLetBody  |
  -- | Access a specific binding in a let term by variable name
  TermAccessorLetBinding Core.Name |
  -- | Access an element of a list by index
  TermAccessorListElement Int |
  -- | Access a key in a map by index
  TermAccessorMapKey Int |
  -- | Access a value in a map by index
  TermAccessorMapValue Int |
  -- | Access the term inside a Just value
  TermAccessorMaybeTerm  |
  -- | Access an element of a product (tuple) by index
  TermAccessorProductTerm Int |
  -- | Access a field of a record by field name
  TermAccessorRecordField Core.Name |
  -- | Access an element of a set by index
  TermAccessorSetElement Int |
  -- | Access the term inside a sum variant
  TermAccessorSumTerm  |
  -- | Access the body of a type lambda term
  TermAccessorTypeLambdaBody  |
  -- | Access the term being applied to a type
  TermAccessorTypeApplicationTerm  |
  -- | Access the term inside a union injection
  TermAccessorInjectionTerm  |
  -- | Access the term inside a wrapped term
  TermAccessorWrappedTerm 
  deriving (Eq, Ord, Read, Show)

_TermAccessor = (Core.Name "hydra.accessors.TermAccessor")

_TermAccessor_annotatedBody = (Core.Name "annotatedBody")

_TermAccessor_applicationFunction = (Core.Name "applicationFunction")

_TermAccessor_applicationArgument = (Core.Name "applicationArgument")

_TermAccessor_lambdaBody = (Core.Name "lambdaBody")

_TermAccessor_unionCasesDefault = (Core.Name "unionCasesDefault")

_TermAccessor_unionCasesBranch = (Core.Name "unionCasesBranch")

_TermAccessor_letBody = (Core.Name "letBody")

_TermAccessor_letBinding = (Core.Name "letBinding")

_TermAccessor_listElement = (Core.Name "listElement")

_TermAccessor_mapKey = (Core.Name "mapKey")

_TermAccessor_mapValue = (Core.Name "mapValue")

_TermAccessor_maybeTerm = (Core.Name "maybeTerm")

_TermAccessor_productTerm = (Core.Name "productTerm")

_TermAccessor_recordField = (Core.Name "recordField")

_TermAccessor_setElement = (Core.Name "setElement")

_TermAccessor_sumTerm = (Core.Name "sumTerm")

_TermAccessor_typeLambdaBody = (Core.Name "typeLambdaBody")

_TermAccessor_typeApplicationTerm = (Core.Name "typeApplicationTerm")

_TermAccessor_injectionTerm = (Core.Name "injectionTerm")

_TermAccessor_wrappedTerm = (Core.Name "wrappedTerm")
