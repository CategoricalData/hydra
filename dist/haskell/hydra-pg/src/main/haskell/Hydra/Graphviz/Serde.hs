-- Note: this is an automatically generated file. Do not edit.
-- | Serialization functions for converting Graphviz DOT AST to abstract expressions

module Hydra.Graphviz.Serde where
import qualified Hydra.Ast as Ast
import qualified Hydra.Graphviz.Dot as Dot
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Maybes as Maybes
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Convert an attribute list to an expression
attrListToExpr :: Dot.AttrList -> Ast.Expr
attrListToExpr al =
    Serialization.spaceSep (Lists.map (\alist -> Serialization.brackets Serialization.squareBrackets Serialization.inlineStyle (Serialization.commaSep Serialization.inlineStyle (Lists.map equalityPairToExpr alist))) (Dot.unAttrList al))
-- | Convert an attribute statement to an expression
attrStmtToExpr :: Dot.AttrStmt -> Ast.Expr
attrStmtToExpr as =

      let t = Dot.attrStmtType as
          attr = Dot.attrStmtAttributes as
      in (Serialization.spaceSep [
        attrTypeToExpr t,
        (attrListToExpr attr)])
-- | Convert an attribute type to an expression
attrTypeToExpr :: Dot.AttrType -> Ast.Expr
attrTypeToExpr t =
    case t of
      Dot.AttrTypeGraph -> Serialization.cst "graph"
      Dot.AttrTypeNode -> Serialization.cst "node"
      Dot.AttrTypeEdge -> Serialization.cst "edge"
-- | Convert a compass point to an expression
compassPtToExpr :: Dot.CompassPt -> Ast.Expr
compassPtToExpr p =
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
edgeStmtToExpr :: Bool -> Dot.EdgeStmt -> Ast.Expr
edgeStmtToExpr directed es =

      let l = Dot.edgeStmtLeft es
          r = Dot.edgeStmtRight es
          attr = Dot.edgeStmtAttributes es
          arrow = Logic.ifElse directed "->" "--"
          rhsParts =
                  Lists.concat (Lists.map (\n -> [
                    Serialization.cst arrow,
                    (nodeOrSubgraphToExpr directed n)]) r)
          attrParts = Maybes.maybe [] (\a -> [
                attrListToExpr a]) attr
      in (Serialization.spaceSep (Lists.concat [
        [
          nodeOrSubgraphToExpr directed l],
        rhsParts,
        attrParts]))
-- | Convert an equality pair to an expression
equalityPairToExpr :: Dot.EqualityPair -> Ast.Expr
equalityPairToExpr eq =

      let l = Dot.equalityPairLeft eq
          r = Dot.equalityPairRight eq
      in (Serialization.spaceSep [
        idToExpr l,
        (Serialization.cst "="),
        (idToExpr r)])
-- | Convert a graph to an expression
graphToExpr :: Dot.Graph -> Ast.Expr
graphToExpr g =

      let strict = Dot.graphStrict g
          directed = Dot.graphDirected g
          stmts = Dot.graphStatements g
          graphKeyword = Logic.ifElse directed "digraph" "graph"
          graphExpr =
                  Logic.ifElse strict (Serialization.spaceSep [
                    Serialization.cst "strict",
                    (Serialization.cst graphKeyword)]) (Serialization.cst graphKeyword)
          body =
                  Serialization.brackets Serialization.curlyBraces Serialization.fullBlockStyle (Serialization.symbolSep ";" Serialization.fullBlockStyle (Lists.map (stmtToExpr directed) stmts))
      in (Serialization.spaceSep [
        graphExpr,
        body])
-- | Convert an identifier to an expression
idToExpr :: Dot.Id -> Ast.Expr
idToExpr i =
    Serialization.cst (Strings.cat [
      "\"",
      (Dot.unId i),
      "\""])
-- | Convert a node identifier to an expression
nodeIdToExpr :: Dot.NodeId -> Ast.Expr
nodeIdToExpr nid =

      let i = Dot.nodeIdId nid
          mp = Dot.nodeIdPort nid
      in (Serialization.noSep (Maybes.cat [
        Maybes.pure (idToExpr i),
        (Maybes.map portToExpr mp)]))
-- | Convert a node or subgraph to an expression
nodeOrSubgraphToExpr :: Bool -> Dot.NodeOrSubgraph -> Ast.Expr
nodeOrSubgraphToExpr directed ns =
    case ns of
      Dot.NodeOrSubgraphNode v0 -> nodeIdToExpr v0
      Dot.NodeOrSubgraphSubgraph v0 -> subgraphToExpr directed v0
-- | Convert a node statement to an expression
nodeStmtToExpr :: Dot.NodeStmt -> Ast.Expr
nodeStmtToExpr ns =

      let i = Dot.nodeStmtId ns
          attr = Dot.nodeStmtAttributes ns
      in (Serialization.spaceSep (Maybes.cat [
        Maybes.pure (nodeIdToExpr i),
        (Maybes.map attrListToExpr attr)]))
-- | Convert a port to an expression
portToExpr :: Dot.Port -> Ast.Expr
portToExpr p =

      let mi = Dot.portId p
          mp = Dot.portPosition p
          pre =
                  Maybes.maybe [] (\i -> [
                    Serialization.cst ":",
                    (idToExpr i)]) mi
          suf =
                  Maybes.maybe [] (\cp -> [
                    Serialization.cst ":",
                    (compassPtToExpr cp)]) mp
      in (Serialization.noSep (Lists.concat [
        pre,
        suf]))
-- | Convert a statement to an expression
stmtToExpr :: Bool -> Dot.Stmt -> Ast.Expr
stmtToExpr directed s =
    case s of
      Dot.StmtNode v0 -> nodeStmtToExpr v0
      Dot.StmtEdge v0 -> edgeStmtToExpr directed v0
      Dot.StmtAttr v0 -> attrStmtToExpr v0
      Dot.StmtEquals v0 -> equalityPairToExpr v0
      Dot.StmtSubgraph v0 -> subgraphToExpr directed v0
-- | Convert a subgraph identifier to an expression
subgraphIdToExpr :: Dot.SubgraphId -> Ast.Expr
subgraphIdToExpr sid =
    Serialization.spaceSep (Maybes.cat [
      Maybes.pure (Serialization.cst "subgraph"),
      (Maybes.map idToExpr (Dot.unSubgraphId sid))])
-- | Convert a subgraph to an expression
subgraphToExpr :: Bool -> Dot.Subgraph -> Ast.Expr
subgraphToExpr directed sg =

      let mid = Dot.subgraphSubgraphId sg
          stmts = Dot.subgraphStatements sg
          body =
                  Serialization.brackets Serialization.curlyBraces Serialization.inlineStyle (Serialization.spaceSep (Lists.map (stmtToExpr directed) stmts))
      in (Serialization.spaceSep (Maybes.cat [
        Maybes.map subgraphIdToExpr mid,
        (Maybes.pure body)]))
