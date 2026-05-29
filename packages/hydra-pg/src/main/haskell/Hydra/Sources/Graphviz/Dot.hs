module Hydra.Sources.Graphviz.Dot where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
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
            moduleDescription = Just ("A model from the Graphviz DOT graph description language."
      ++ " Based on the grammar at https://graphviz.org/doc/info/lang.html")}
  where
    definitions = [
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

attrList :: TypeDefinition
attrList = define "AttrList" $
  T.wrap $ nonemptyList $ nonemptyList $ dot "EqualityPair"

--attr_stmt	:	(graph | node | edge) attr_list
--attr_list	:	'[' [ a_list ] ']' [ attr_list ]
--a_list	:	ID '=' ID [ (';' | ',') ] [ a_list ]
attrStmt :: TypeDefinition
attrStmt = define "AttrStmt" $
  T.record [
    "type">: dot "AttrType",
    "attributes">: dot "AttrList"]

attrType :: TypeDefinition
attrType = define "AttrType" $
  T.enum ["graph", "node", "edge"]

--compass_pt	:	(n | ne | e | se | s | sw | w | nw | c | _)
compassPt :: TypeDefinition
compassPt = define "CompassPt" $
  T.enum ["n", "ne", "e", "se", "s", "sw", "w", "nw", "c", "none"]

dot :: String -> Type
dot = typeref ns

--edge_stmt	:	(node_id | subgraph) edgeRHS [ attr_list ]
--edgeRHS	:	edgeop (node_id | subgraph) [ edgeRHS ]
edgeStmt :: TypeDefinition
edgeStmt = define "EdgeStmt" $
  T.record [
    "left">: dot "NodeOrSubgraph",
    "right">: nonemptyList $ dot "NodeOrSubgraph",
    "attributes">: T.maybe $ dot "AttrList"]

equalityPair :: TypeDefinition
equalityPair = define "EqualityPair" $
  T.record [
    "left">: dot "Id",
    "right">: dot "Id"]

--graph	:	[ strict ] (graph | digraph) [ ID ] '{' stmt_list '}'
graph_ :: TypeDefinition
graph_ = define "Graph" $
  T.record [
    "strict">: T.boolean,
    "directed">: T.boolean,
    "id">: T.maybe $ dot "Id",
    "statements">: T.list $ dot "Stmt"]

id_ :: TypeDefinition
id_ = define "Id" $ T.wrap T.string

--node_id	:	ID [ port ]
nodeId :: TypeDefinition
nodeId = define "NodeId" $
  T.record [
    "id">: dot "Id",
    "port">: T.maybe $ dot "Port"]

nodeOrSubgraph :: TypeDefinition
nodeOrSubgraph = define "NodeOrSubgraph" $
  T.union [
    "node">: dot "NodeId",
    "subgraph">: dot "Subgraph"]

--node_stmt	:	node_id [ attr_list ]
nodeStmt :: TypeDefinition
nodeStmt = define "NodeStmt" $
  T.record [
    "id">: dot "NodeId",
    "attributes">: T.maybe $ dot "AttrList"]

--port	:	':' ID [ ':' compass_pt ]
--      |	':' compass_pt
port :: TypeDefinition
port = define "Port" $
  T.record [
    "id">: T.maybe $ dot "Id",
    "position">: T.maybe $ dot "CompassPt"]

--stmt_list	:	[ stmt [ ';' ] stmt_list ]
--stmt	:	node_stmt
--      |	edge_stmt
--      |	attr_stmt
--      |	ID '=' ID
--      |	subgraph
stmt :: TypeDefinition
stmt = define "Stmt" $
  T.union [
    "node">: dot "NodeStmt",
    "edge">: dot "EdgeStmt",
    "attr">: dot "AttrStmt",
    "equals">: dot "EqualityPair",
    "subgraph">: dot "Subgraph"]

--subgraph	:	[ subgraph [ ID ] ] '{' stmt_list '}'
subgraph :: TypeDefinition
subgraph = define "Subgraph" $
  T.record [
    "subgraphId">: T.maybe $ dot "SubgraphId",
    "statements">: T.list $ dot "Stmt"]

subgraphId :: TypeDefinition
subgraphId = define "SubgraphId" $
  T.wrap $ T.maybe $ dot "Id"
