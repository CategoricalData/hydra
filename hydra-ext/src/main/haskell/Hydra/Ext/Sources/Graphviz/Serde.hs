module Hydra.Ext.Sources.Graphviz.Serde where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Meta.Accessors                  as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Meta.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars       as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta      as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan         as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import Hydra.Ast
import qualified Hydra.Ext.Org.Graphviz.Dot as Dot
import qualified Hydra.Ext.Sources.Graphviz.Dot as DotSyntax


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.graphviz.serde"

module_ :: Module
module_ = Module ns elements
    [Serialization.ns]
    (DotSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Serialization functions for converting Graphviz DOT AST to abstract expressions"
  where
    elements = [
      toBinding writeAttrList,
      toBinding writeAttrStmt,
      toBinding writeAttrType,
      toBinding writeCompassPt,
      toBinding writeEdgeStmt,
      toBinding writeEqualityPair,
      toBinding writeGraph,
      toBinding writeId,
      toBinding writeNodeId,
      toBinding writeNodeOrSubgraph,
      toBinding writeNodeStmt,
      toBinding writePort,
      toBinding writeStmt,
      toBinding writeSubgraph,
      toBinding writeSubgraphId]


writeAttrList :: TBinding (Dot.AttrList -> Expr)
writeAttrList = define "writeAttrList" $
  doc "Convert an attribute list to an expression" $
  lambda "al" $
    Serialization.spaceSep @@ (Lists.map
      (lambda "alist" $
        Serialization.brackets @@ Serialization.squareBrackets @@ Serialization.inlineStyle @@
          (Serialization.commaSep @@ Serialization.inlineStyle @@ (Lists.map writeEqualityPair (var "alist"))))
      (unwrap Dot._AttrList @@ var "al"))

writeAttrStmt :: TBinding (Dot.AttrStmt -> Expr)
writeAttrStmt = define "writeAttrStmt" $
  doc "Convert an attribute statement to an expression" $
  lambda "as" $ lets [
    "t">: project Dot._AttrStmt Dot._AttrStmt_type @@ var "as",
    "attr">: project Dot._AttrStmt Dot._AttrStmt_attributes @@ var "as"] $
    Serialization.spaceSep @@ list [writeAttrType @@ var "t", writeAttrList @@ var "attr"]

writeAttrType :: TBinding (Dot.AttrType -> Expr)
writeAttrType = define "writeAttrType" $
  doc "Convert an attribute type to an expression" $
  lambda "t" $
    cases Dot._AttrType (var "t") Nothing [
      Dot._AttrType_graph>>: constant $ Serialization.cst @@ string "graph",
      Dot._AttrType_node>>: constant $ Serialization.cst @@ string "node",
      Dot._AttrType_edge>>: constant $ Serialization.cst @@ string "edge"]

writeCompassPt :: TBinding (Dot.CompassPt -> Expr)
writeCompassPt = define "writeCompassPt" $
  doc "Convert a compass point to an expression" $
  lambda "p" $
    cases Dot._CompassPt (var "p") Nothing [
      Dot._CompassPt_n>>: constant $ Serialization.cst @@ string "n",
      Dot._CompassPt_ne>>: constant $ Serialization.cst @@ string "ne",
      Dot._CompassPt_e>>: constant $ Serialization.cst @@ string "e",
      Dot._CompassPt_se>>: constant $ Serialization.cst @@ string "se",
      Dot._CompassPt_s>>: constant $ Serialization.cst @@ string "s",
      Dot._CompassPt_sw>>: constant $ Serialization.cst @@ string "sw",
      Dot._CompassPt_w>>: constant $ Serialization.cst @@ string "w",
      Dot._CompassPt_nw>>: constant $ Serialization.cst @@ string "nw",
      Dot._CompassPt_c>>: constant $ Serialization.cst @@ string "c",
      Dot._CompassPt_none>>: constant $ Serialization.cst @@ string "none"]

writeEdgeStmt :: TBinding (Bool -> Dot.EdgeStmt -> Expr)
writeEdgeStmt = define "writeEdgeStmt" $
  doc "Convert an edge statement to an expression" $
  lambda "directed" $ lambda "es" $ lets [
    "l">: project Dot._EdgeStmt Dot._EdgeStmt_left @@ var "es",
    "r">: project Dot._EdgeStmt Dot._EdgeStmt_right @@ var "es",
    "attr">: project Dot._EdgeStmt Dot._EdgeStmt_attributes @@ var "es",
    "arrow">: Logic.ifElse (var "directed") (string "->") (string "--"),
    "rhsParts">: Lists.concat (Lists.map
      (lambda "n" $ list [Serialization.cst @@ var "arrow", writeNodeOrSubgraph @@ var "directed" @@ var "n"])
      (var "r")),
    "attrParts">: Maybes.maybe
      (list ([] :: [TTerm Expr]))
      (lambda "a" $ list [writeAttrList @@ var "a"])
      (var "attr")] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      list [writeNodeOrSubgraph @@ var "directed" @@ var "l"],
      var "rhsParts",
      var "attrParts"])

writeEqualityPair :: TBinding (Dot.EqualityPair -> Expr)
writeEqualityPair = define "writeEqualityPair" $
  doc "Convert an equality pair to an expression" $
  lambda "eq" $ lets [
    "l">: project Dot._EqualityPair Dot._EqualityPair_left @@ var "eq",
    "r">: project Dot._EqualityPair Dot._EqualityPair_right @@ var "eq"] $
    Serialization.spaceSep @@ list [writeId @@ var "l", Serialization.cst @@ string "=", writeId @@ var "r"]

writeGraph :: TBinding (Dot.Graph -> Expr)
writeGraph = define "writeGraph" $
  doc "Convert a graph to an expression" $
  lambda "g" $ lets [
    "strict">: project Dot._Graph Dot._Graph_strict @@ var "g",
    "directed">: project Dot._Graph Dot._Graph_directed @@ var "g",
    "stmts">: project Dot._Graph Dot._Graph_statements @@ var "g",
    "graphKeyword">: Logic.ifElse (var "directed") (string "digraph") (string "graph"),
    "graphExpr">: Logic.ifElse (var "strict")
      (Serialization.spaceSep @@ list [Serialization.cst @@ string "strict", Serialization.cst @@ var "graphKeyword"])
      (Serialization.cst @@ var "graphKeyword"),
    "body">: Serialization.brackets @@ Serialization.curlyBraces @@ Serialization.fullBlockStyle @@
      (Serialization.symbolSep @@ string ";" @@ Serialization.fullBlockStyle @@
        (Lists.map (writeStmt @@ var "directed") (var "stmts")))] $
    Serialization.spaceSep @@ list [var "graphExpr", var "body"]

writeId :: TBinding (Dot.Id -> Expr)
writeId = define "writeId" $
  doc "Convert an identifier to an expression" $
  lambda "i" $ Serialization.cst @@
    (Strings.cat $ list [string "\"", unwrap Dot._Id @@ var "i", string "\""])

writeNodeId :: TBinding (Dot.NodeId -> Expr)
writeNodeId = define "writeNodeId" $
  doc "Convert a node identifier to an expression" $
  lambda "nid" $ lets [
    "i">: project Dot._NodeId Dot._NodeId_id @@ var "nid",
    "mp">: project Dot._NodeId Dot._NodeId_port @@ var "nid"] $
    Serialization.noSep @@ (Maybes.cat $ list [
      Maybes.pure (writeId @@ var "i"),
      Maybes.map writePort (var "mp")])

writeNodeOrSubgraph :: TBinding (Bool -> Dot.NodeOrSubgraph -> Expr)
writeNodeOrSubgraph = define "writeNodeOrSubgraph" $
  doc "Convert a node or subgraph to an expression" $
  lambda "directed" $ lambda "ns" $
    cases Dot._NodeOrSubgraph (var "ns") Nothing [
      Dot._NodeOrSubgraph_node>>: lambda "n" $ writeNodeId @@ var "n",
      Dot._NodeOrSubgraph_subgraph>>: lambda "sg" $ writeSubgraph @@ var "directed" @@ var "sg"]

writeNodeStmt :: TBinding (Dot.NodeStmt -> Expr)
writeNodeStmt = define "writeNodeStmt" $
  doc "Convert a node statement to an expression" $
  lambda "ns" $ lets [
    "i">: project Dot._NodeStmt Dot._NodeStmt_id @@ var "ns",
    "attr">: project Dot._NodeStmt Dot._NodeStmt_attributes @@ var "ns"] $
    Serialization.spaceSep @@ (Maybes.cat $ list [
      Maybes.pure (writeNodeId @@ var "i"),
      Maybes.map writeAttrList (var "attr")])

writePort :: TBinding (Dot.Port -> Expr)
writePort = define "writePort" $
  doc "Convert a port to an expression" $
  lambda "p" $ lets [
    "mi">: project Dot._Port Dot._Port_id @@ var "p",
    "mp">: project Dot._Port Dot._Port_position @@ var "p",
    "pre">: Maybes.maybe
      (list ([] :: [TTerm Expr]))
      (lambda "i" $ list [Serialization.cst @@ string ":", writeId @@ var "i"])
      (var "mi"),
    "suf">: Maybes.maybe
      (list ([] :: [TTerm Expr]))
      (lambda "cp" $ list [Serialization.cst @@ string ":", writeCompassPt @@ var "cp"])
      (var "mp")] $
    Serialization.noSep @@ (Lists.concat $ list [var "pre", var "suf"])

writeStmt :: TBinding (Bool -> Dot.Stmt -> Expr)
writeStmt = define "writeStmt" $
  doc "Convert a statement to an expression" $
  lambda "directed" $ lambda "s" $
    cases Dot._Stmt (var "s") Nothing [
      Dot._Stmt_node>>: lambda "n" $ writeNodeStmt @@ var "n",
      Dot._Stmt_edge>>: lambda "e" $ writeEdgeStmt @@ var "directed" @@ var "e",
      Dot._Stmt_attr>>: lambda "a" $ writeAttrStmt @@ var "a",
      Dot._Stmt_equals>>: lambda "eq" $ writeEqualityPair @@ var "eq",
      Dot._Stmt_subgraph>>: lambda "sg" $ writeSubgraph @@ var "directed" @@ var "sg"]

writeSubgraph :: TBinding (Bool -> Dot.Subgraph -> Expr)
writeSubgraph = define "writeSubgraph" $
  doc "Convert a subgraph to an expression" $
  lambda "directed" $ lambda "sg" $ lets [
    "mid">: project Dot._Subgraph Dot._Subgraph_subgraphId @@ var "sg",
    "stmts">: project Dot._Subgraph Dot._Subgraph_statements @@ var "sg",
    "body">: Serialization.brackets @@ Serialization.curlyBraces @@ Serialization.inlineStyle @@
      (Serialization.spaceSep @@ (Lists.map (writeStmt @@ var "directed") (var "stmts")))] $
    Serialization.spaceSep @@ (Maybes.cat $ list [
      Maybes.map writeSubgraphId (var "mid"),
      Maybes.pure (var "body")])

writeSubgraphId :: TBinding (Dot.SubgraphId -> Expr)
writeSubgraphId = define "writeSubgraphId" $
  doc "Convert a subgraph identifier to an expression" $
  lambda "sid" $
    Serialization.spaceSep @@ (Maybes.cat $ list [
      Maybes.pure (Serialization.cst @@ string "subgraph"),
      Maybes.map writeId (unwrap Dot._SubgraphId @@ var "sid")])
