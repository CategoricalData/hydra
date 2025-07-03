-- | A model for term access patterns

module Hydra.Accessors where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

data AccessorEdge = 
  AccessorEdge {
    accessorEdgeSource :: AccessorNode,
    accessorEdgePath :: AccessorPath,
    accessorEdgeTarget :: AccessorNode}
  deriving (Eq, Ord, Read, Show)

_AccessorEdge = (Core.Name "hydra.accessors.AccessorEdge")

_AccessorEdge_source = (Core.Name "source")

_AccessorEdge_path = (Core.Name "path")

_AccessorEdge_target = (Core.Name "target")

data AccessorGraph = 
  AccessorGraph {
    accessorGraphNodes :: [AccessorNode],
    accessorGraphEdges :: [AccessorEdge]}
  deriving (Eq, Ord, Read, Show)

_AccessorGraph = (Core.Name "hydra.accessors.AccessorGraph")

_AccessorGraph_nodes = (Core.Name "nodes")

_AccessorGraph_edges = (Core.Name "edges")

data AccessorNode = 
  AccessorNode {
    accessorNodeName :: Core.Name,
    accessorNodeLabel :: String,
    accessorNodeId :: String}
  deriving (Eq, Ord, Read, Show)

_AccessorNode = (Core.Name "hydra.accessors.AccessorNode")

_AccessorNode_name = (Core.Name "name")

_AccessorNode_label = (Core.Name "label")

_AccessorNode_id = (Core.Name "id")

newtype AccessorPath = 
  AccessorPath {
    unAccessorPath :: [TermAccessor]}
  deriving (Eq, Ord, Read, Show)

_AccessorPath = (Core.Name "hydra.accessors.AccessorPath")

-- | A function which maps from a term to a particular immediate subterm
data TermAccessor = 
  TermAccessorAnnotatedSubject  |
  TermAccessorApplicationFunction  |
  TermAccessorApplicationArgument  |
  TermAccessorLambdaBody  |
  TermAccessorUnionCasesDefault  |
  TermAccessorUnionCasesBranch Core.Name |
  TermAccessorLetEnvironment  |
  TermAccessorLetBinding Core.Name |
  TermAccessorListElement Int |
  TermAccessorMapKey Int |
  TermAccessorMapValue Int |
  TermAccessorOptionalTerm  |
  TermAccessorProductTerm Int |
  TermAccessorRecordField Core.Name |
  TermAccessorSetElement Int |
  TermAccessorSumTerm  |
  TermAccessorTypeAbstractionBody  |
  TermAccessorTypeApplicationTerm  |
  TermAccessorInjectionTerm  |
  TermAccessorWrappedTerm 
  deriving (Eq, Ord, Read, Show)

_TermAccessor = (Core.Name "hydra.accessors.TermAccessor")

_TermAccessor_annotatedSubject = (Core.Name "annotatedSubject")

_TermAccessor_applicationFunction = (Core.Name "applicationFunction")

_TermAccessor_applicationArgument = (Core.Name "applicationArgument")

_TermAccessor_lambdaBody = (Core.Name "lambdaBody")

_TermAccessor_unionCasesDefault = (Core.Name "unionCasesDefault")

_TermAccessor_unionCasesBranch = (Core.Name "unionCasesBranch")

_TermAccessor_letEnvironment = (Core.Name "letEnvironment")

_TermAccessor_letBinding = (Core.Name "letBinding")

_TermAccessor_listElement = (Core.Name "listElement")

_TermAccessor_mapKey = (Core.Name "mapKey")

_TermAccessor_mapValue = (Core.Name "mapValue")

_TermAccessor_optionalTerm = (Core.Name "optionalTerm")

_TermAccessor_productTerm = (Core.Name "productTerm")

_TermAccessor_recordField = (Core.Name "recordField")

_TermAccessor_setElement = (Core.Name "setElement")

_TermAccessor_sumTerm = (Core.Name "sumTerm")

_TermAccessor_typeAbstractionBody = (Core.Name "typeAbstractionBody")

_TermAccessor_typeApplicationTerm = (Core.Name "typeApplicationTerm")

_TermAccessor_injectionTerm = (Core.Name "injectionTerm")

_TermAccessor_wrappedTerm = (Core.Name "wrappedTerm")
