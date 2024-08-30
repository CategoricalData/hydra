-- | A model from the Graphviz DOT graph description language. Based on the grammar at https://graphviz.org/doc/info/lang.html

module Hydra.Ext.Org.Graphviz.Dot where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

newtype Id = 
  Id {
    unId :: String}
  deriving (Eq, Ord, Read, Show)

_Id = (Core.Name "hydra/ext/org/graphviz/dot.Id")

data Graph = 
  Graph {
    graphStrict :: Bool,
    graphDirected :: Bool,
    graphId :: (Maybe Id),
    graphStatements :: [Stmt]}
  deriving (Eq, Ord, Read, Show)

_Graph = (Core.Name "hydra/ext/org/graphviz/dot.Graph")

_Graph_strict = (Core.Name "strict")

_Graph_directed = (Core.Name "directed")

_Graph_id = (Core.Name "id")

_Graph_statements = (Core.Name "statements")

data Stmt = 
  StmtNode NodeStmt |
  StmtEdge EdgeStmt |
  StmtAttr AttrStmt |
  StmtEquals EqualityPair |
  StmtSubgraph Subgraph
  deriving (Eq, Ord, Read, Show)

_Stmt = (Core.Name "hydra/ext/org/graphviz/dot.Stmt")

_Stmt_node = (Core.Name "node")

_Stmt_edge = (Core.Name "edge")

_Stmt_attr = (Core.Name "attr")

_Stmt_equals = (Core.Name "equals")

_Stmt_subgraph = (Core.Name "subgraph")

data EqualityPair = 
  EqualityPair {
    equalityPairLeft :: Id,
    equalityPairRight :: Id}
  deriving (Eq, Ord, Read, Show)

_EqualityPair = (Core.Name "hydra/ext/org/graphviz/dot.EqualityPair")

_EqualityPair_left = (Core.Name "left")

_EqualityPair_right = (Core.Name "right")

data AttrStmt = 
  AttrStmt {
    attrStmtType :: AttrType,
    attrStmtAttributes :: AttrList}
  deriving (Eq, Ord, Read, Show)

_AttrStmt = (Core.Name "hydra/ext/org/graphviz/dot.AttrStmt")

_AttrStmt_type = (Core.Name "type")

_AttrStmt_attributes = (Core.Name "attributes")

data AttrType = 
  AttrTypeGraph  |
  AttrTypeNode  |
  AttrTypeEdge 
  deriving (Eq, Ord, Read, Show)

_AttrType = (Core.Name "hydra/ext/org/graphviz/dot.AttrType")

_AttrType_graph = (Core.Name "graph")

_AttrType_node = (Core.Name "node")

_AttrType_edge = (Core.Name "edge")

newtype AttrList = 
  AttrList {
    unAttrList :: [[EqualityPair]]}
  deriving (Eq, Ord, Read, Show)

_AttrList = (Core.Name "hydra/ext/org/graphviz/dot.AttrList")

data EdgeStmt = 
  EdgeStmt {
    edgeStmtLeft :: NodeOrSubgraph,
    edgeStmtRight :: [NodeOrSubgraph],
    edgeStmtAttributes :: (Maybe AttrList)}
  deriving (Eq, Ord, Read, Show)

_EdgeStmt = (Core.Name "hydra/ext/org/graphviz/dot.EdgeStmt")

_EdgeStmt_left = (Core.Name "left")

_EdgeStmt_right = (Core.Name "right")

_EdgeStmt_attributes = (Core.Name "attributes")

data NodeOrSubgraph = 
  NodeOrSubgraphNode NodeId |
  NodeOrSubgraphSubgraph Subgraph
  deriving (Eq, Ord, Read, Show)

_NodeOrSubgraph = (Core.Name "hydra/ext/org/graphviz/dot.NodeOrSubgraph")

_NodeOrSubgraph_node = (Core.Name "node")

_NodeOrSubgraph_subgraph = (Core.Name "subgraph")

data NodeStmt = 
  NodeStmt {
    nodeStmtId :: NodeId,
    nodeStmtAttributes :: (Maybe AttrList)}
  deriving (Eq, Ord, Read, Show)

_NodeStmt = (Core.Name "hydra/ext/org/graphviz/dot.NodeStmt")

_NodeStmt_id = (Core.Name "id")

_NodeStmt_attributes = (Core.Name "attributes")

data NodeId = 
  NodeId {
    nodeIdId :: Id,
    nodeIdPort :: (Maybe Port)}
  deriving (Eq, Ord, Read, Show)

_NodeId = (Core.Name "hydra/ext/org/graphviz/dot.NodeId")

_NodeId_id = (Core.Name "id")

_NodeId_port = (Core.Name "port")

data Port = 
  Port {
    portId :: (Maybe Id),
    portPosition :: (Maybe CompassPt)}
  deriving (Eq, Ord, Read, Show)

_Port = (Core.Name "hydra/ext/org/graphviz/dot.Port")

_Port_id = (Core.Name "id")

_Port_position = (Core.Name "position")

data Subgraph = 
  Subgraph {
    subgraphSubgraphId :: (Maybe SubgraphId),
    subgraphStatements :: [Stmt]}
  deriving (Eq, Ord, Read, Show)

_Subgraph = (Core.Name "hydra/ext/org/graphviz/dot.Subgraph")

_Subgraph_subgraphId = (Core.Name "subgraphId")

_Subgraph_statements = (Core.Name "statements")

newtype SubgraphId = 
  SubgraphId {
    unSubgraphId :: (Maybe Id)}
  deriving (Eq, Ord, Read, Show)

_SubgraphId = (Core.Name "hydra/ext/org/graphviz/dot.SubgraphId")

data CompassPt = 
  CompassPtN  |
  CompassPtNe  |
  CompassPtE  |
  CompassPtSe  |
  CompassPtS  |
  CompassPtSw  |
  CompassPtW  |
  CompassPtNw  |
  CompassPtC  |
  CompassPtNone 
  deriving (Eq, Ord, Read, Show)

_CompassPt = (Core.Name "hydra/ext/org/graphviz/dot.CompassPt")

_CompassPt_n = (Core.Name "n")

_CompassPt_ne = (Core.Name "ne")

_CompassPt_e = (Core.Name "e")

_CompassPt_se = (Core.Name "se")

_CompassPt_s = (Core.Name "s")

_CompassPt_sw = (Core.Name "sw")

_CompassPt_w = (Core.Name "w")

_CompassPt_nw = (Core.Name "nw")

_CompassPt_c = (Core.Name "c")

_CompassPt_none = (Core.Name "none")