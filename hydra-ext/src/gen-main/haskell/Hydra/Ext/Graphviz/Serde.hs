-- Note: this is an automatically generated file. Do not edit.

-- | Serialization functions for converting Graphviz DOT AST to abstract expressions

module Hydra.Ext.Graphviz.Serde where

import qualified Hydra.Ast as Ast
import qualified Hydra.Ext.Org.Graphviz.Dot as Dot
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Convert an attribute list to an expression
writeAttrList :: Dot.AttrList -> Ast.Expr
writeAttrList al =
    Serialization.spaceSep (Lists.map (\alist -> Serialization.brackets Serialization.squareBrackets Serialization.inlineStyle (Serialization.commaSep Serialization.inlineStyle (Lists.map writeEqualityPair alist))) (Dot.unAttrList al))

-- | Convert an attribute statement to an expression
writeAttrStmt :: Dot.AttrStmt -> Ast.Expr
writeAttrStmt as =
     
      let t = Dot.attrStmtType as 
          attr = Dot.attrStmtAttributes as
      in (Serialization.spaceSep [
        writeAttrType t,
        (writeAttrList attr)])

-- | Convert an attribute type to an expression
writeAttrType :: Dot.AttrType -> Ast.Expr
writeAttrType t =
    case t of
      Dot.AttrTypeGraph -> Serialization.cst "graph"
      Dot.AttrTypeNode -> Serialization.cst "node"
      Dot.AttrTypeEdge -> Serialization.cst "edge"

-- | Convert a compass point to an expression
writeCompassPt :: Dot.CompassPt -> Ast.Expr
writeCompassPt p =
    case p of
      Dot.CompassPtN -> Serialization.cst "n"
      Dot.CompassPtNe -> Serialization.cst "ne"
      Dot.CompassPtE -> Serialization.cst "e"
      Dot.CompassPtSe -> Serialization.cst "se"
      Dot.CompassPtS -> Serialization.cst "s"
      Dot.CompassPtSw -> Serialization.cst "sw"
      Dot.CompassPtW -> Serialization.cst "w"
      Dot.CompassPtNw -> Serialization.cst "nw"
      Dot.CompassPtC -> Serialization.cst "c"
      Dot.CompassPtNone -> Serialization.cst "none"

-- | Convert an edge statement to an expression
writeEdgeStmt :: Bool -> Dot.EdgeStmt -> Ast.Expr
writeEdgeStmt directed es =
     
      let l = Dot.edgeStmtLeft es 
          r = Dot.edgeStmtRight es
          attr = Dot.edgeStmtAttributes es
          arrow = Logic.ifElse directed "->" "--"
          rhsParts =
                  Lists.concat (Lists.map (\n -> [
                    Serialization.cst arrow,
                    (writeNodeOrSubgraph directed n)]) r)
          attrParts = Maybes.maybe [] (\a -> [
                writeAttrList a]) attr
      in (Serialization.spaceSep (Lists.concat [
        [
          writeNodeOrSubgraph directed l],
        rhsParts,
        attrParts]))

-- | Convert an equality pair to an expression
writeEqualityPair :: Dot.EqualityPair -> Ast.Expr
writeEqualityPair eq =
     
      let l = Dot.equalityPairLeft eq 
          r = Dot.equalityPairRight eq
      in (Serialization.spaceSep [
        writeId l,
        (Serialization.cst "="),
        (writeId r)])

-- | Convert a graph to an expression
writeGraph :: Dot.Graph -> Ast.Expr
writeGraph g =
     
      let strict = Dot.graphStrict g 
          directed = Dot.graphDirected g
          stmts = Dot.graphStatements g
          graphKeyword = Logic.ifElse directed "digraph" "graph"
          graphExpr =
                  Logic.ifElse strict (Serialization.spaceSep [
                    Serialization.cst "strict",
                    (Serialization.cst graphKeyword)]) (Serialization.cst graphKeyword)
          body =
                  Serialization.brackets Serialization.curlyBraces Serialization.fullBlockStyle (Serialization.symbolSep ";" Serialization.fullBlockStyle (Lists.map (writeStmt directed) stmts))
      in (Serialization.spaceSep [
        graphExpr,
        body])

-- | Convert an identifier to an expression
writeId :: Dot.Id -> Ast.Expr
writeId i =
    Serialization.cst (Strings.cat [
      "\"",
      (Dot.unId i),
      "\""])

-- | Convert a node identifier to an expression
writeNodeId :: Dot.NodeId -> Ast.Expr
writeNodeId nid =
     
      let i = Dot.nodeIdId nid 
          mp = Dot.nodeIdPort nid
      in (Serialization.noSep (Maybes.cat [
        Maybes.pure (writeId i),
        (Maybes.map writePort mp)]))

-- | Convert a node or subgraph to an expression
writeNodeOrSubgraph :: Bool -> Dot.NodeOrSubgraph -> Ast.Expr
writeNodeOrSubgraph directed ns =
    case ns of
      Dot.NodeOrSubgraphNode v0 -> writeNodeId v0
      Dot.NodeOrSubgraphSubgraph v0 -> writeSubgraph directed v0

-- | Convert a node statement to an expression
writeNodeStmt :: Dot.NodeStmt -> Ast.Expr
writeNodeStmt ns =
     
      let i = Dot.nodeStmtId ns 
          attr = Dot.nodeStmtAttributes ns
      in (Serialization.spaceSep (Maybes.cat [
        Maybes.pure (writeNodeId i),
        (Maybes.map writeAttrList attr)]))

-- | Convert a port to an expression
writePort :: Dot.Port -> Ast.Expr
writePort p =
     
      let mi = Dot.portId p 
          mp = Dot.portPosition p
          pre =
                  Maybes.maybe [] (\i -> [
                    Serialization.cst ":",
                    (writeId i)]) mi
          suf =
                  Maybes.maybe [] (\cp -> [
                    Serialization.cst ":",
                    (writeCompassPt cp)]) mp
      in (Serialization.noSep (Lists.concat [
        pre,
        suf]))

-- | Convert a statement to an expression
writeStmt :: Bool -> Dot.Stmt -> Ast.Expr
writeStmt directed s =
    case s of
      Dot.StmtNode v0 -> writeNodeStmt v0
      Dot.StmtEdge v0 -> writeEdgeStmt directed v0
      Dot.StmtAttr v0 -> writeAttrStmt v0
      Dot.StmtEquals v0 -> writeEqualityPair v0
      Dot.StmtSubgraph v0 -> writeSubgraph directed v0

-- | Convert a subgraph to an expression
writeSubgraph :: Bool -> Dot.Subgraph -> Ast.Expr
writeSubgraph directed sg =
     
      let mid = Dot.subgraphSubgraphId sg 
          stmts = Dot.subgraphStatements sg
          body =
                  Serialization.brackets Serialization.curlyBraces Serialization.inlineStyle (Serialization.spaceSep (Lists.map (writeStmt directed) stmts))
      in (Serialization.spaceSep (Maybes.cat [
        Maybes.map writeSubgraphId mid,
        (Maybes.pure body)]))

-- | Convert a subgraph identifier to an expression
writeSubgraphId :: Dot.SubgraphId -> Ast.Expr
writeSubgraphId sid =
    Serialization.spaceSep (Maybes.cat [
      Maybes.pure (Serialization.cst "subgraph"),
      (Maybes.map writeId (Dot.unSubgraphId sid))])
