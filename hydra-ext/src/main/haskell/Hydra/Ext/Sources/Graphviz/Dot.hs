module Hydra.Ext.Sources.Graphviz.Dot where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.ext.org.graphviz.dot"

define :: String -> Type -> Binding
define = defineType ns

dot :: String -> Type
dot = typeref ns

module_ :: Module
module_ = Module ns elements [] [Core.ns] $
    Just ("A model from the Graphviz DOT graph description language."
      ++ " Based on the grammar at https://graphviz.org/doc/info/lang.html")
  where
    elements = [
      id_,
      graph_,
      stmt,
      equalityPair,
      attrStmt,
      attrType,
      attrList,
      edgeStmt,
      nodeOrSubgraph,
      nodeStmt,
      nodeId,
      port,
      subgraph,
      subgraphId,
      compassPt]

id_ :: Binding
id_ = define "Id" $ T.wrap T.string

--graph	:	[ strict ] (graph | digraph) [ ID ] '{' stmt_list '}'
graph_ :: Binding
graph_ = define "Graph" $
  T.record [
    "strict">: T.boolean,
    "directed">: T.boolean,
    "id">: T.maybe $ dot "Id",
    "statements">: T.list $ dot "Stmt"]

--stmt_list	:	[ stmt [ ';' ] stmt_list ]
--stmt	:	node_stmt
--      |	edge_stmt
--      |	attr_stmt
--      |	ID '=' ID
--      |	subgraph
stmt :: Binding
stmt = define "Stmt" $
  T.union [
    "node">: dot "NodeStmt",
    "edge">: dot "EdgeStmt",
    "attr">: dot "AttrStmt",
    "equals">: dot "EqualityPair",
    "subgraph">: dot "Subgraph"]

equalityPair :: Binding
equalityPair = define "EqualityPair" $
  T.record [
    "left">: dot "Id",
    "right">: dot "Id"]

--attr_stmt	:	(graph | node | edge) attr_list
--attr_list	:	'[' [ a_list ] ']' [ attr_list ]
--a_list	:	ID '=' ID [ (';' | ',') ] [ a_list ]
attrStmt :: Binding
attrStmt = define "AttrStmt" $
  T.record [
    "type">: dot "AttrType",
    "attributes">: dot "AttrList"]

attrType :: Binding
attrType = define "AttrType" $
  T.enum ["graph", "node", "edge"]

attrList :: Binding
attrList = define "AttrList" $
  T.wrap $ nonemptyList $ nonemptyList $ dot "EqualityPair"

--edge_stmt	:	(node_id | subgraph) edgeRHS [ attr_list ]
--edgeRHS	:	edgeop (node_id | subgraph) [ edgeRHS ]
edgeStmt :: Binding
edgeStmt = define "EdgeStmt" $
  T.record [
    "left">: dot "NodeOrSubgraph",
    "right">: nonemptyList $ dot "NodeOrSubgraph",
    "attributes">: T.maybe $ dot "AttrList"]

nodeOrSubgraph :: Binding
nodeOrSubgraph = define "NodeOrSubgraph" $
  T.union [
    "node">: dot "NodeId",
    "subgraph">: dot "Subgraph"]

--node_stmt	:	node_id [ attr_list ]
nodeStmt :: Binding
nodeStmt = define "NodeStmt" $
  T.record [
    "id">: dot "NodeId",
    "attributes">: T.maybe $ dot "AttrList"]

--node_id	:	ID [ port ]
nodeId :: Binding
nodeId = define "NodeId" $
  T.record [
    "id">: dot "Id",
    "port">: T.maybe $ dot "Port"]

--port	:	':' ID [ ':' compass_pt ]
--      |	':' compass_pt
port :: Binding
port = define "Port" $
  T.record [
    "id">: T.maybe $ dot "Id",
    "position">: T.maybe $ dot "CompassPt"]

--subgraph	:	[ subgraph [ ID ] ] '{' stmt_list '}'
subgraph :: Binding
subgraph = define "Subgraph" $
  T.record [
    "subgraphId">: T.maybe $ dot "SubgraphId",
    "statements">: T.list $ dot "Stmt"]

subgraphId :: Binding
subgraphId = define "SubgraphId" $
  T.wrap $ T.maybe $ dot "Id"

--compass_pt	:	(n | ne | e | se | s | sw | w | nw | c | _)
compassPt :: Binding
compassPt = define "CompassPt" $
  T.enum ["n", "ne", "e", "se", "s", "sw", "w", "nw", "c", "none"]
