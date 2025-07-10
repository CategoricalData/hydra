module Hydra.Ext.Staging.Graphviz.Serde where

import Hydra.Kernel
import qualified Hydra.Ext.Org.Graphviz.Dot as Dot
import qualified Hydra.Ast as CT
import Hydra.Serialization
import Hydra.Formatting

import qualified Data.List as L
import qualified Data.Maybe as Y


writeAttrList :: Dot.AttrList -> CT.Expr
writeAttrList (Dot.AttrList lists) = spaceSep (writeAlist <$> lists)
  where
    writeAlist list = brackets squareBrackets inlineStyle $
      commaSep inlineStyle (writeEqualityPair <$> list)

writeAttrStmt :: Dot.AttrStmt -> CT.Expr
writeAttrStmt (Dot.AttrStmt t attr) = spaceSep [writeAttrType t, writeAttrList attr]

writeAttrType :: Dot.AttrType -> CT.Expr
writeAttrType t = case t of
    Dot.AttrTypeGraph -> cst "graph"
    Dot.AttrTypeNode -> cst "node"
    Dot.AttrTypeEdge -> cst "edge"

writeCompassPt :: Dot.CompassPt -> CT.Expr
writeCompassPt p = case p of
    Dot.CompassPtN -> cst "n"
    Dot.CompassPtNe -> cst "ne"
    Dot.CompassPtE -> cst "e"
    Dot.CompassPtSe -> cst "se"
    Dot.CompassPtS -> cst "s"
    Dot.CompassPtSw -> cst "sw"
    Dot.CompassPtW -> cst "w"
    Dot.CompassPtNw -> cst "nw"
    Dot.CompassPtC -> cst "c"
    Dot.CompassPtNone -> cst "none"

writeEdgeStmt :: Bool -> Dot.EdgeStmt -> CT.Expr
writeEdgeStmt directed (Dot.EdgeStmt l r attr) = spaceSep $
    [writeNodeOrSubgraph directed l]
    ++ L.concat (toRhs <$> r)
    ++ Y.maybe [] (\a -> [writeAttrList a]) attr
  where
    toRhs n = [arrow, writeNodeOrSubgraph directed n]
    arrow = cst $ if directed then "->" else "--"

writeEqualityPair :: Dot.EqualityPair -> CT.Expr
writeEqualityPair (Dot.EqualityPair l r) = spaceSep [writeId l, cst "=", writeId r]

writeGraph :: Dot.Graph -> CT.Expr
writeGraph g = spaceSep [header, body]
  where
    header = if Dot.graphStrict g then spaceSep[cst "strict", graph] else graph
    graph = cst $ if directed then "digraph" else "graph"
    body = brackets curlyBraces fullBlockStyle $
      symbolSep ";" fullBlockStyle (writeStmt directed <$> Dot.graphStatements g)
    directed = Dot.graphDirected g

writeId :: Dot.Id -> CT.Expr
writeId i = cst $ "\"" ++ Dot.unId i ++ "\""

writeNodeId :: Dot.NodeId -> CT.Expr
writeNodeId (Dot.NodeId i mp) = noSep $ Y.catMaybes [
  Just $ writeId i,
  writePort <$> mp]

writeNodeOrSubgraph :: Bool -> Dot.NodeOrSubgraph -> CT.Expr
writeNodeOrSubgraph directed ns = case ns of
  Dot.NodeOrSubgraphNode n -> writeNodeId n
  Dot.NodeOrSubgraphSubgraph sg -> writeSubgraph directed sg

writeNodeStmt :: Dot.NodeStmt -> CT.Expr
writeNodeStmt (Dot.NodeStmt i attr) = spaceSep $ Y.catMaybes [
    Just $ writeNodeId i,
    writeAttrList <$> attr]

writePort :: Dot.Port -> CT.Expr
writePort (Dot.Port mi mp) = noSep $ pre ++ suf
  where
    pre = case mi of
      Nothing -> []
      Just i -> [cst ":", writeId i]
    suf = case mp of
      Nothing -> []
      Just p -> [cst ":", writeCompassPt p]

writeStmt :: Bool -> Dot.Stmt -> CT.Expr
writeStmt directed s = case s of
  Dot.StmtNode n -> writeNodeStmt n
  Dot.StmtEdge e -> writeEdgeStmt directed e
  Dot.StmtAttr a -> writeAttrStmt a
  Dot.StmtEquals eq -> writeEqualityPair eq
  Dot.StmtSubgraph sg -> writeSubgraph directed sg

writeSubgraph :: Bool -> Dot.Subgraph -> CT.Expr
writeSubgraph directed (Dot.Subgraph i stmts) = spaceSep $ Y.catMaybes [
  writeSubgraphId <$> i,
  Just $ brackets curlyBraces inlineStyle $ spaceSep (writeStmt directed <$> stmts)]

writeSubgraphId :: Dot.SubgraphId -> CT.Expr
writeSubgraphId (Dot.SubgraphId i) = spaceSep $ Y.catMaybes [
  Just $ cst "subgraph",
  writeId <$> i]
