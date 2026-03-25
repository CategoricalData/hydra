-- Note: this is an automatically generated file. Do not edit.

-- | Functions for converting Hydra terms to Graphviz DOT graphs

module Hydra.Ext.Graphviz.Coder where

import qualified Hydra.Accessors as Accessors
import qualified Hydra.Core as Core
import qualified Hydra.Ext.Org.Graphviz.Dot as Dot
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Accessors as Accessors_
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Create a DOT label attribute
labelAttr :: String -> Dot.EqualityPair
labelAttr lab =
    Dot.EqualityPair {
      Dot.equalityPairLeft = (Dot.Id "label"),
      Dot.equalityPairRight = (Dot.Id lab)}

-- | Create DOT label attributes with a node style
labelAttrs :: String -> String -> Dot.AttrList
labelAttrs style lab =

      let styleAttrs =
              Logic.ifElse (Equality.equal style nodeStyleSimple) [] (Logic.ifElse (Equality.equal style nodeStyleElement) [
                Dot.EqualityPair {
                  Dot.equalityPairLeft = (Dot.Id "style"),
                  Dot.equalityPairRight = (Dot.Id "filled")},
                Dot.EqualityPair {
                  Dot.equalityPairLeft = (Dot.Id "fillcolor"),
                  Dot.equalityPairRight = (Dot.Id "lightyellow")}] (Logic.ifElse (Equality.equal style nodeStyleVariable) [
                Dot.EqualityPair {
                  Dot.equalityPairLeft = (Dot.Id "style"),
                  Dot.equalityPairRight = (Dot.Id "filled")},
                Dot.EqualityPair {
                  Dot.equalityPairLeft = (Dot.Id "fillcolor"),
                  Dot.equalityPairRight = (Dot.Id "lightcyan")}] [
                Dot.EqualityPair {
                  Dot.equalityPairLeft = (Dot.Id "style"),
                  Dot.equalityPairRight = (Dot.Id "filled")},
                Dot.EqualityPair {
                  Dot.equalityPairLeft = (Dot.Id "fillcolor"),
                  Dot.equalityPairRight = (Dot.Id "linen")}]))
      in (Dot.AttrList [
        Lists.concat2 [
          labelAttr lab] styleAttrs])

-- | The 'element' node style
nodeStyleElement :: String
nodeStyleElement = "element"

-- | The 'primitive' node style
nodeStylePrimitive :: String
nodeStylePrimitive = "primitive"

-- | The 'simple' node style
nodeStyleSimple :: String
nodeStyleSimple = "simple"

-- | The 'variable' node style
nodeStyleVariable :: String
nodeStyleVariable = "variable"

-- | Construct a map from namespace to prefix for all standard libraries
standardNamespaces :: M.Map Module.Namespace String
standardNamespaces =
    M.fromList [
      (Module.Namespace "hydra.lib.chars", "chars"),
      (Module.Namespace "hydra.lib.eithers", "eithers"),
      (Module.Namespace "hydra.lib.equality", "equality"),
      (Module.Namespace "hydra.lib.lists", "lists"),
      (Module.Namespace "hydra.lib.literals", "literals"),
      (Module.Namespace "hydra.lib.logic", "logic"),
      (Module.Namespace "hydra.lib.maps", "maps"),
      (Module.Namespace "hydra.lib.math", "math"),
      (Module.Namespace "hydra.lib.maybes", "maybes"),
      (Module.Namespace "hydra.lib.pairs", "pairs"),
      (Module.Namespace "hydra.lib.regex", "regex"),
      (Module.Namespace "hydra.lib.sets", "sets"),
      (Module.Namespace "hydra.lib.strings", "strings")]

-- | Compute a label and node style for a term
termLabel :: Bool -> M.Map Module.Namespace String -> Core.Term -> (String, String)
termLabel compact namespaces term =

      let simpleLabel = \lab -> (lab, nodeStyleSimple)
      in case term of
        Core.TermAnnotated _ -> simpleLabel "@{}"
        Core.TermApplication _ -> simpleLabel (Logic.ifElse compact "$" "apply")
        Core.TermFunction v0 -> case v0 of
          Core.FunctionLambda _ -> simpleLabel (Logic.ifElse compact "\955" "lambda")
          Core.FunctionElimination v1 -> case v1 of
            Core.EliminationRecord v2 -> simpleLabel (Strings.cat [
              "{",
              (Names.compactName namespaces (Core.projectionTypeName v2)),
              "}.",
              (Core.unName (Core.projectionField v2))])
            Core.EliminationUnion v2 -> simpleLabel (Strings.cat [
              "cases_{",
              (Names.compactName namespaces (Core.caseStatementTypeName v2)),
              "}"])
            Core.EliminationWrap v2 -> simpleLabel (Strings.cat [
              "unwrap_{",
              (Names.compactName namespaces v2),
              "}"])
            _ -> simpleLabel "?"
          Core.FunctionPrimitive v1 -> (Names.compactName namespaces v1, nodeStylePrimitive)
          _ -> simpleLabel "?"
        Core.TermLet _ -> simpleLabel "let"
        Core.TermList _ -> simpleLabel (Logic.ifElse compact "[]" "list")
        Core.TermLiteral v0 -> simpleLabel (case v0 of
          Core.LiteralBinary v1 -> Literals.binaryToString v1
          Core.LiteralBoolean v1 -> Literals.showBoolean v1
          Core.LiteralInteger v1 -> case v1 of
            Core.IntegerValueBigint v2 -> Literals.showBigint v2
            Core.IntegerValueInt8 v2 -> Literals.showInt8 v2
            Core.IntegerValueInt16 v2 -> Literals.showInt16 v2
            Core.IntegerValueInt32 v2 -> Literals.showInt32 v2
            Core.IntegerValueInt64 v2 -> Literals.showInt64 v2
            Core.IntegerValueUint8 v2 -> Literals.showUint8 v2
            Core.IntegerValueUint16 v2 -> Literals.showUint16 v2
            Core.IntegerValueUint32 v2 -> Literals.showUint32 v2
            Core.IntegerValueUint64 v2 -> Literals.showUint64 v2
            _ -> "?"
          Core.LiteralFloat v1 -> case v1 of
            Core.FloatValueBigfloat v2 -> Literals.showBigfloat v2
            Core.FloatValueFloat32 v2 -> Literals.showFloat32 v2
            Core.FloatValueFloat64 v2 -> Literals.showFloat64 v2
            _ -> "?"
          Core.LiteralString v1 -> v1
          _ -> "?")
        Core.TermMap _ -> simpleLabel (Logic.ifElse compact "<,>" "map")
        Core.TermMaybe _ -> simpleLabel (Logic.ifElse compact "opt" "optional")
        Core.TermRecord v0 -> simpleLabel (Strings.cat2 "\8743" (Names.compactName namespaces (Core.recordTypeName v0)))
        Core.TermTypeLambda _ -> simpleLabel "tyabs"
        Core.TermTypeApplication _ -> simpleLabel "tyapp"
        Core.TermUnion v0 -> simpleLabel (Strings.cat2 "\8891" (Names.compactName namespaces (Core.injectionTypeName v0)))
        Core.TermVariable v0 -> simpleLabel (Names.compactName namespaces v0)
        Core.TermWrap v0 -> simpleLabel (Strings.cat [
          "(",
          (Names.compactName namespaces (Core.wrappedTermTypeName v0)),
          ")"])
        _ -> simpleLabel "?"

-- | Convert a term to an accessor-style DOT graph
termToAccessorDotGraph :: Core.Term -> Dot.Graph
termToAccessorDotGraph term =
    Dot.Graph {
      Dot.graphStrict = False,
      Dot.graphDirected = True,
      Dot.graphId = Nothing,
      Dot.graphStatements = (termToAccessorDotStmts standardNamespaces term)}

-- | Convert a term to accessor-style DOT statements
termToAccessorDotStmts :: M.Map Module.Namespace String -> Core.Term -> [Dot.Stmt]
termToAccessorDotStmts namespaces term =

      let accessorGraph = Accessors_.termToAccessorGraph namespaces term
          nodes = Accessors.accessorGraphNodes accessorGraph
          edges = Accessors.accessorGraphEdges accessorGraph
          nodeStmt =
                  \node -> Dot.StmtNode (Dot.NodeStmt {
                    Dot.nodeStmtId = (toNodeId (Dot.Id (Accessors.accessorNodeId node))),
                    Dot.nodeStmtAttributes = (Just (Dot.AttrList [
                      [
                        labelAttr (Accessors.accessorNodeLabel node)]]))})
          edgeStmt =
                  \edge ->
                    let lab1 = Accessors.accessorNodeId (Accessors.accessorEdgeSource edge)
                        lab2 = Accessors.accessorNodeId (Accessors.accessorEdgeTarget edge)
                        pathAccessors = Accessors.unAccessorPath (Accessors.accessorEdgePath edge)
                        showPath = Strings.intercalate "/" (Maybes.cat (Lists.map Accessors_.termAccessor pathAccessors))
                    in (toEdgeStmt (Dot.Id lab1) (Dot.Id lab2) (Just (Dot.AttrList [
                      [
                        labelAttr showPath]])))
      in (Lists.concat2 (Lists.map nodeStmt nodes) (Lists.map edgeStmt edges))

-- | Convert a term to a full DOT graph
termToDotGraph :: Core.Term -> Dot.Graph
termToDotGraph term =
    Dot.Graph {
      Dot.graphStrict = False,
      Dot.graphDirected = True,
      Dot.graphId = Nothing,
      Dot.graphStatements = (termToDotStmts standardNamespaces term)}

-- | Convert a term to full DOT statements showing term structure
termToDotStmts :: M.Map Module.Namespace String -> Core.Term -> [Dot.Stmt]
termToDotStmts namespaces term =

      let encode =
              \mlabstyle -> \isElement -> \ids -> \mparent -> \stmtsVisited -> \accessorTerm ->
                let accessor = Pairs.first accessorTerm
                    currentTerm = Pairs.second accessorTerm
                    stmts = Pairs.first stmtsVisited
                    visited = Pairs.second stmtsVisited
                    termLS = termLabel True namespaces currentTerm
                    rawLabel = Pairs.first termLS
                    termNodeStyle = Pairs.second termLS
                    labelOf =
                            \vis -> \t ->
                              let tls = termLabel True namespaces t
                                  l = Pairs.first tls
                                  s = Pairs.second tls
                              in (Names.uniqueLabel vis l, s)
                    labstyle = Maybes.maybe (labelOf visited currentTerm) (\ls -> ls) mlabstyle
                    label = Pairs.first labstyle
                    style = Pairs.second labstyle
                    nodeStyle = Logic.ifElse isElement nodeStyleElement termNodeStyle
                    selfId = Dot.Id label
                    selfVisited = Sets.insert label visited
                    nodeStmt =
                            Dot.StmtNode (Dot.NodeStmt {
                              Dot.nodeStmtId = (toNodeId selfId),
                              Dot.nodeStmtAttributes = (Just (labelAttrs nodeStyle rawLabel))})
                    toAccessorEdgeStmt =
                            \acc -> \sty -> \i1 -> \i2 -> toEdgeStmt i1 i2 (Maybes.map (\s -> labelAttrs sty s) (Accessors_.termAccessor acc))
                    edgeAttrs =
                            \lab -> Dot.AttrList [
                              [
                                Dot.EqualityPair {
                                  Dot.equalityPairLeft = (Dot.Id "label"),
                                  Dot.equalityPairRight = (Dot.Id lab)}]]
                    parentStmt = Maybes.maybe [] (\parent -> [
                          toAccessorEdgeStmt accessor style parent selfId]) mparent
                    selfStmts =
                            Lists.concat [
                              stmts,
                              [
                                nodeStmt],
                              parentStmt]
                    dflt =
                            Lists.foldl (encode Nothing False ids (Just selfId)) (selfStmts, selfVisited) (Rewriting.subtermsWithAccessors currentTerm)
                in case currentTerm of
                  Core.TermFunction v0 -> case v0 of
                    Core.FunctionLambda v1 ->
                      let v = Core.lambdaParameter v1
                          body = Core.lambdaBody v1
                          vstr = Core.unName v
                          varLabel = Names.uniqueLabel selfVisited vstr
                          varId = Dot.Id varLabel
                          visited1 = Sets.insert varLabel selfVisited
                          ids1 = Maps.insert v varId ids
                          varNodeStmt =
                                  Dot.StmtNode (Dot.NodeStmt {
                                    Dot.nodeStmtId = (toNodeId varId),
                                    Dot.nodeStmtAttributes = (Just (labelAttrs nodeStyleVariable vstr))})
                          varEdgeStmt =
                                  Dot.StmtEdge (Dot.EdgeStmt {
                                    Dot.edgeStmtLeft = (toNodeOrSubgraph selfId),
                                    Dot.edgeStmtRight = [
                                      toNodeOrSubgraph varId],
                                    Dot.edgeStmtAttributes = (Just (edgeAttrs "var"))})
                      in (encode Nothing False ids1 (Just selfId) (Lists.concat [
                        selfStmts,
                        [
                          varNodeStmt,
                          varEdgeStmt]], visited1) (Accessors.TermAccessorLambdaBody, body))
                    _ -> dflt
                  Core.TermLet v0 ->
                    let bindings = Core.letBindings v0
                        env = Core.letBody v0
                        addBindingIds =
                                \idsVis -> \binding ->
                                  let curIds = Pairs.first idsVis
                                      curVis = Pairs.second idsVis
                                      bname = Core.bindingName binding
                                      bterm = Core.bindingTerm binding
                                      bls = labelOf curVis bterm
                                      blab = Pairs.first bls
                                  in (Maps.insert bname (Dot.Id blab) curIds, (Sets.insert blab curVis))
                        idsVis1 = Lists.foldl addBindingIds (ids, visited) bindings
                        ids1 = Pairs.first idsVis1
                        addBindingTerm =
                                \stVis -> \binding ->
                                  let bname = Core.bindingName binding
                                      bterm = Core.bindingTerm binding
                                      blab = Dot.unId (Maybes.fromMaybe (Dot.Id "?") (Maps.lookup bname ids1))
                                  in (encode (Just (blab, nodeStyleElement)) True ids1 (Just selfId) stVis (Accessors.TermAccessorLetBinding bname, bterm))
                        stmts1 = Lists.foldl addBindingTerm (selfStmts, selfVisited) bindings
                    in (encode Nothing False ids1 (Just selfId) stmts1 (Accessors.TermAccessorLetBody, env))
                  Core.TermVariable v0 -> Maybes.maybe dflt (\i -> (Lists.concat2 stmts [
                    toAccessorEdgeStmt accessor style (Maybes.fromMaybe selfId mparent) i], visited)) (Maps.lookup v0 ids)
                  _ -> dflt
      in (Pairs.first (encode Nothing False Maps.empty Nothing ([], Sets.empty) (Accessors.TermAccessorAnnotatedBody, term)))

-- | Create a DOT edge statement
toEdgeStmt :: Dot.Id -> Dot.Id -> Maybe Dot.AttrList -> Dot.Stmt
toEdgeStmt i1 i2 attrs =
    Dot.StmtEdge (Dot.EdgeStmt {
      Dot.edgeStmtLeft = (toNodeOrSubgraph i1),
      Dot.edgeStmtRight = [
        toNodeOrSubgraph i2],
      Dot.edgeStmtAttributes = attrs})

-- | Create a DOT NodeId from an Id
toNodeId :: Dot.Id -> Dot.NodeId
toNodeId i =
    Dot.NodeId {
      Dot.nodeIdId = i,
      Dot.nodeIdPort = Nothing}

-- | Create a DOT NodeOrSubgraph from an Id
toNodeOrSubgraph :: Dot.Id -> Dot.NodeOrSubgraph
toNodeOrSubgraph i = Dot.NodeOrSubgraphNode (toNodeId i)
