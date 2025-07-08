module Hydra.Ext.Graphviz.Src.Dot where

import Hydra.Kernel
import qualified Data.List             as L
import qualified Data.Map              as M
import qualified Data.Set              as S
import qualified Data.Maybe            as Y
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Terms       as Terms
import           Hydra.Dsl.Types       as Types
import           Hydra.Sources.Kernel.Types.Core


dotModule :: Module
dotModule = Module ns elements [] [module_] $
    Just ("A model from the Graphviz DOT graph description language."
      ++ " Based on the grammar at https://graphviz.org/doc/info/lang.html")
  where
    ns = Namespace "hydra.ext.org.graphviz.dot"
    def = datatype ns
    dot = typeref ns

    elements = [

      def "Id" $ wrap string,

--graph	:	[ strict ] (graph | digraph) [ ID ] '{' stmt_list '}'
      def "Graph" $
        record [
          "strict">: boolean,
          "directed">: boolean,
          "id">: optional $ dot "Id",
          "statements">: list $ dot "Stmt"],

--stmt_list	:	[ stmt [ ';' ] stmt_list ]
--stmt	:	node_stmt
--      |	edge_stmt
--      |	attr_stmt
--      |	ID '=' ID
--      |	subgraph
      def "Stmt" $
        union [
          "node">: dot "NodeStmt",
          "edge">: dot "EdgeStmt",
          "attr">: dot "AttrStmt",
          "equals">: dot "EqualityPair",
          "subgraph">: dot "Subgraph"],
      def "EqualityPair" $
        record [
          "left">: dot "Id",
          "right">: dot "Id"],

--attr_stmt	:	(graph | node | edge) attr_list
--attr_list	:	'[' [ a_list ] ']' [ attr_list ]
--a_list	:	ID '=' ID [ (';' | ',') ] [ a_list ]
      def "AttrStmt" $
        record [
          "type">: dot "AttrType",
          "attributes">: dot "AttrList"],
      def "AttrType" $
        enum ["graph", "node", "edge"],
      def "AttrList" $
        wrap $ nonemptyList $ nonemptyList $ dot "EqualityPair",

--edge_stmt	:	(node_id | subgraph) edgeRHS [ attr_list ]
--edgeRHS	:	edgeop (node_id | subgraph) [ edgeRHS ]
      def "EdgeStmt" $
        record [
          "left">: dot "NodeOrSubgraph",
          "right">: nonemptyList $ dot "NodeOrSubgraph",
          "attributes">: optional $ dot "AttrList"],
      def "NodeOrSubgraph" $
        union [
          "node">: dot "NodeId",
          "subgraph">: dot "Subgraph"],

--node_stmt	:	node_id [ attr_list ]
      def "NodeStmt" $
        record [
          "id">: dot "NodeId",
          "attributes">: optional $ dot "AttrList"],

--node_id	:	ID [ port ]
      def "NodeId" $
        record [
          "id">: dot "Id",
          "port">: optional $ dot "Port"],

--port	:	':' ID [ ':' compass_pt ]
--      |	':' compass_pt
      def "Port" $
        record [
          "id">: optional $ dot "Id",
          "position">: optional $ dot "CompassPt"],

--subgraph	:	[ subgraph [ ID ] ] '{' stmt_list '}'
       def "Subgraph" $
         record [
           "subgraphId">: optional $ dot "SubgraphId",
           "statements">: list $ dot "Stmt"],
       def "SubgraphId" $
         wrap $ optional $ dot "Id",

--compass_pt	:	(n | ne | e | se | s | sw | w | nw | c | _)
      def "CompassPt" $
        enum ["n", "ne", "e", "se", "s", "sw", "w", "nw", "c", "none"]]
