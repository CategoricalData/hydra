module Hydra.Sources.Graphviz.Dot where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


ns :: ModuleName
ns = ModuleName "hydra.graphviz.dot"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just ("A model from the Graphviz DOT graph description language."
      ++ " Based on the grammar at https://graphviz.org/doc/info/lang.html"))}
  where
    definitions = [
      attrList,
      attrStmt,
      attrType,
      compassPt,
      edgeStmt,
      equalityPair,
      graph_,
      id_,
      nodeId,
      nodeOrSubgraph,
      nodeStmt,
      port,
      stmt,
      subgraph,
      subgraphId]

attrList :: TypeDefinition
attrList = define "AttrList" $
  doc "A Graphviz DOT attribute list: one or more comma/semicolon-separated lists of key-value pairs" $
  T.wrap $ nonemptyList $ nonemptyList $ dot "EqualityPair"

--attr_stmt	:	(graph | node | edge) attr_list
--attr_list	:	'[' [ a_list ] ']' [ attr_list ]
--a_list	:	ID '=' ID [ (';' | ',') ] [ a_list ]
attrStmt :: TypeDefinition
attrStmt = define "AttrStmt" $
  doc "A Graphviz DOT attribute statement applying default attributes to graph, node, or edge elements" $
  T.record [
    "type">: dot "AttrType",
    "attributes">: dot "AttrList"]

attrType :: TypeDefinition
attrType = define "AttrType" $
  doc "The target of a Graphviz DOT attribute statement: graph, node, or edge" $
  T.enum ["graph", "node", "edge"]

--compass_pt	:	(n | ne | e | se | s | sw | w | nw | c | _)
compassPt :: TypeDefinition
compassPt = define "CompassPt" $
  doc "A Graphviz DOT compass point, used to select a specific side of a node for edge attachment" $
  T.enum ["n", "ne", "e", "se", "s", "sw", "w", "nw", "c", "none"]

dot :: String -> Type
dot = typeref ns

--edge_stmt	:	(node_id | subgraph) edgeRHS [ attr_list ]
--edgeRHS	:	edgeop (node_id | subgraph) [ edgeRHS ]
edgeStmt :: TypeDefinition
edgeStmt = define "EdgeStmt" $
  doc "A Graphviz DOT edge statement connecting a chain of nodes or subgraphs" $
  T.record [
    "left">: dot "NodeOrSubgraph",
    "right">: nonemptyList $ dot "NodeOrSubgraph",
    "attributes">: T.optional $ dot "AttrList"]

equalityPair :: TypeDefinition
equalityPair = define "EqualityPair" $
  doc "A Graphviz DOT key = value pair" $
  T.record [
    "left">: dot "Id",
    "right">: dot "Id"]

--graph	:	[ strict ] (graph | digraph) [ ID ] '{' stmt_list '}'
graph_ :: TypeDefinition
graph_ = define "Graph" $
  doc "A Graphviz DOT graph: an optionally strict, optionally directed graph with an optional ID and a list of statements" $
  T.record [
    "strict">: T.boolean,
    "directed">: T.boolean,
    "id">: T.optional $ dot "Id",
    "statements">: T.list $ dot "Stmt"]

id_ :: TypeDefinition
id_ = define "Id" $
  doc "A Graphviz DOT identifier" $
  T.wrap T.string

--node_id	:	ID [ port ]
nodeId :: TypeDefinition
nodeId = define "NodeId" $
  doc "A Graphviz DOT node identifier, with an optional port" $
  T.record [
    "id">: dot "Id",
    "port">: T.optional $ dot "Port"]

nodeOrSubgraph :: TypeDefinition
nodeOrSubgraph = define "NodeOrSubgraph" $
  doc "A Graphviz DOT node or subgraph, as an edge endpoint" $
  T.union [
    "node">: dot "NodeId",
    "subgraph">: dot "Subgraph"]

--node_stmt	:	node_id [ attr_list ]
nodeStmt :: TypeDefinition
nodeStmt = define "NodeStmt" $
  doc "A Graphviz DOT node statement, with an optional attribute list" $
  T.record [
    "id">: dot "NodeId",
    "attributes">: T.optional $ dot "AttrList"]

--port	:	':' ID [ ':' compass_pt ]
--      |	':' compass_pt
port :: TypeDefinition
port = define "Port" $
  doc "A Graphviz DOT port: an optional identifier and/or compass point specifying an edge attachment point" $
  T.record [
    "id">: T.optional $ dot "Id",
    "position">: T.optional $ dot "CompassPt"]

--stmt_list	:	[ stmt [ ';' ] stmt_list ]
--stmt	:	node_stmt
--      |	edge_stmt
--      |	attr_stmt
--      |	ID '=' ID
--      |	subgraph
stmt :: TypeDefinition
stmt = define "Stmt" $
  doc "A Graphviz DOT statement: a node, edge, attribute, equality, or subgraph statement" $
  T.union [
    "node">: dot "NodeStmt",
    "edge">: dot "EdgeStmt",
    "attr">: dot "AttrStmt",
    "equals">: dot "EqualityPair",
    "subgraph">: dot "Subgraph"]

--subgraph	:	[ subgraph [ ID ] ] '{' stmt_list '}'
subgraph :: TypeDefinition
subgraph = define "Subgraph" $
  doc "A Graphviz DOT subgraph, with an optional ID and a list of statements" $
  T.record [
    "subgraphId">: T.optional $ dot "SubgraphId",
    "statements">: T.list $ dot "Stmt"]

subgraphId :: TypeDefinition
subgraphId = define "SubgraphId" $
  doc "The optional identifier of a Graphviz DOT subgraph" $
  T.wrap $ T.optional $ dot "Id"
