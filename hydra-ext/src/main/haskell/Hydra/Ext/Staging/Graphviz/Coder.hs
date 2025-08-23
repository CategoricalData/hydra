module Hydra.Ext.Staging.Graphviz.Coder (
  termToAccessorDotGraph,
  termToDotGraph,
  standardNamespaces,
) where

import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Ext.Org.Graphviz.Dot as Dot
import qualified Hydra.Names as Names
import qualified Hydra.Show.Accessors as ShowAccessors

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


data NodeStyle = NodeStyleSimple | NodeStyleElement | NodeStyleVariable | NodeStylePrimitive

termToAccessorDotGraph :: Term -> Dot.Graph
termToAccessorDotGraph term = Dot.Graph {
  Dot.graphStrict = False,
  Dot.graphDirected = True,
  Dot.graphId = Nothing,
  Dot.graphStatements = termToAccessorDotStmts standardNamespaces term}

termToDotGraph :: Term -> Dot.Graph
termToDotGraph term = Dot.Graph {
  Dot.graphStrict = False,
  Dot.graphDirected = True,
  Dot.graphId = Nothing,
  Dot.graphStatements = termToDotStmts standardNamespaces term}


--------------------------------------------------------------------------------

labelAttr :: String -> Dot.EqualityPair
labelAttr lab = Dot.EqualityPair (Dot.Id "label") (Dot.Id lab)

labelAttrs :: NodeStyle -> String -> Dot.AttrList
labelAttrs style lab = Dot.AttrList [[labelAttr lab] ++ styleAttrs]
  where
    styleAttrs = case style of
      NodeStyleSimple -> []
      NodeStyleElement -> filled "lightyellow"
      NodeStyleVariable -> filled "lightcyan"
      NodeStylePrimitive -> filled "linen"
    filled color = [
      Dot.EqualityPair (Dot.Id "style") (Dot.Id "filled"),
      Dot.EqualityPair (Dot.Id "fillcolor") (Dot.Id color)]

standardNamespaces :: M.Map Namespace String
standardNamespaces = M.fromList (toPair <$> standardLibraries)
  where
    toPair lib = (libraryNamespace lib, libraryPrefix lib)

termToAccessorDotStmts :: M.Map Namespace String -> Term -> [Dot.Stmt]
termToAccessorDotStmts namespaces term = (nodeStmt <$> nodes) ++ (edgeStmt <$> edges)
  where
    (AccessorGraph nodes edges) = ShowAccessors.termToAccessorGraph namespaces term
    nodeStmt (AccessorNode _ rawLabel uniqueLabel)
      = Dot.StmtNode $ Dot.NodeStmt (toNodeId $ Dot.Id uniqueLabel) $ Just $ Dot.AttrList [[labelAttr rawLabel]]
    edgeStmt (AccessorEdge (AccessorNode _ _ lab1) (AccessorPath path) (AccessorNode _ _ lab2))
      = toEdgeStmt (Dot.Id lab1) (Dot.Id lab2) $ Just $ Dot.AttrList [[labelAttr $ showPath path]]
    showPath path = L.intercalate "/" $ Y.catMaybes (ShowAccessors.termAccessor <$> path)

termToDotStmts :: M.Map Namespace String -> Term -> [Dot.Stmt]
termToDotStmts namespaces term = fst $ encode Nothing False M.empty Nothing ([], S.empty) (TermAccessorAnnotatedSubject, term)
  where
    encode mlabstyle isElement ids mparent (stmts, visited) (accessor, term) = case term of
        TermFunction (FunctionLambda (Lambda v _ body)) ->
            encode Nothing False ids1 (Just selfId) (selfStmts ++ [varNodeStmt, varEdgeStmt], visited1) (TermAccessorLambdaBody, body)
          where
            ids1 = M.insert v varId ids
            var = Names.uniqueLabel selfVisited (unName v)
            varId = Dot.Id var
            visited1 = S.insert var selfVisited
            varNodeStmt = Dot.StmtNode $ Dot.NodeStmt {
              Dot.nodeStmtId = toNodeId varId,
              Dot.nodeStmtAttributes = Just $ labelAttrs NodeStyleVariable $ unName v}
            varEdgeStmt = Dot.StmtEdge $ Dot.EdgeStmt (toNodeOrSubgraph selfId) [toNodeOrSubgraph varId] $
              Just $ edgeAttrs "var"
        TermLet (Let bindings env) -> encode Nothing False ids1 (Just selfId) (stmts1, visited2) (TermAccessorLetEnvironment, env)
          where
            (stmts1, visited2) = L.foldl addBinding (selfStmts, selfVisited) bindings
              where
                addBinding (stmts, visited) (LetBinding name trm _) = encode (Just (lab, NodeStyleElement)) True ids1 (Just selfId) (stmts, visited) (TermAccessorLetBinding name, trm)
                  where
                    lab = Dot.unId $ Y.fromJust $ M.lookup name ids1
            (ids1, visited1) = L.foldl addBinding (ids, visited) bindings
              where
                addBinding (ids, visited) (LetBinding name trm _) = (M.insert name (Dot.Id lab) ids, S.insert lab visited)
                  where
                    (lab, style) = labelOf visited trm
        TermVariable name -> case M.lookup name ids of
          Just i -> (stmts ++ [toAccessorEdgeStmt accessor style (Y.fromJust mparent) i], visited)
          Nothing -> dflt
        _ -> dflt
      where
        dflt = L.foldl (encode Nothing False ids $ Just selfId) (selfStmts, selfVisited) $ subtermsWithAccessors term
        selfVisited = S.insert label visited
        selfStmts = stmts ++ [nodeStmt] ++ parentStmt
        parentStmt = case mparent of
          Nothing -> []
          Just parent -> [toAccessorEdgeStmt accessor style parent selfId]
        toAccessorEdgeStmt accessor style i1 i2 = toEdgeStmt i1 i2 attrs
          where
            attrs = fmap (labelAttrs style) (ShowAccessors.termAccessor accessor)
        edgeAttrs lab = Dot.AttrList [[Dot.EqualityPair (Dot.Id "label") (Dot.Id lab)]]
        nodeStmt = Dot.StmtNode $ Dot.NodeStmt {
          Dot.nodeStmtId = toNodeId selfId,
          Dot.nodeStmtAttributes = Just $ labelAttrs style rawLabel}
        selfId = Dot.Id label
        (label, style) = Y.fromMaybe (labelOf visited term) mlabstyle
        labelOf visited term = (Names.uniqueLabel visited l, s)
          where
            (l, s) = termLabel True namespaces term
        (rawLabel, nodeStyle) = (l, if isElement then NodeStyleElement else s)
          where
            (l, s) = termLabel True namespaces term

termLabel :: Bool -> M.Map Namespace String -> Term -> (String, NodeStyle)
termLabel compact namespaces term = case term of
    TermAnnotated (AnnotatedTerm term1 _) -> simpleLabel "@{}"
    TermApplication _ -> simpleLabel $ if compact then "$" else "apply"
    TermFunction f -> case f of
      FunctionLambda (Lambda v _ body) -> simpleLabel $ if compact then "\x03BB" else "lambda"
      FunctionElimination e -> case e of
        EliminationProduct (TupleProjection n i _) -> simpleLabel $ "[" ++ show i ++ "/" ++ show n ++ "]"
        EliminationRecord (Projection tname fname) -> simpleLabel $ "{" ++ Names.compactName namespaces tname ++ "}." ++ unName fname
        EliminationUnion (CaseStatement tname _ _) -> simpleLabel $ "cases_{" ++ Names.compactName namespaces tname ++ "}"
        EliminationWrap name -> simpleLabel $ "unwrap_{" ++ Names.compactName namespaces name ++ "}"
      FunctionPrimitive name -> (Names.compactName namespaces name, NodeStylePrimitive)
    TermLet (Let bindings env) -> simpleLabel "let"
    TermList _ -> simpleLabel $ if compact then "[]" else "list"
    TermLiteral l -> simpleLabel $ case l of
      LiteralBinary s -> s
      LiteralBoolean b -> show b
      LiteralInteger i -> case i of
        IntegerValueBigint v -> show v
        IntegerValueInt8 v -> show v
        IntegerValueInt16 v -> show v
        IntegerValueInt32 v -> show v
        IntegerValueInt64 v -> show v
        IntegerValueUint8 v -> show v
        IntegerValueUint16 v -> show v
        IntegerValueUint32 v -> show v
        IntegerValueUint64 v -> show v
      LiteralFloat f -> case f of
        FloatValueBigfloat v -> show v
        FloatValueFloat32 v -> show v
        FloatValueFloat64 v -> show v
      LiteralString s -> s -- show s
    TermMap _ -> simpleLabel $ if compact then "<,>" else "map"
    TermOptional _ -> simpleLabel $ if compact then "opt" else "optional"
    TermProduct _ -> simpleLabel $ if compact then "\x2227" else "product"
    TermRecord (Record name _) -> simpleLabel $ "\x2227" ++ Names.compactName namespaces name
    TermTypeLambda (TypeLambda v term1) -> simpleLabel "tyabs"
    TermTypeApplication (TypedTerm term _) -> simpleLabel "tyapp"
    TermUnion (Injection tname _) -> simpleLabel $ "\x22BB" ++ Names.compactName namespaces tname
    TermVariable name -> simpleLabel $ Names.compactName namespaces name
    TermWrap (WrappedTerm name term1) -> simpleLabel $ "(" ++ Names.compactName namespaces name ++ ")"
    _ -> simpleLabel "?"
  where
    simpleLabel lab = (lab, NodeStyleSimple)

toEdgeStmt :: Dot.Id -> Dot.Id -> Maybe Dot.AttrList -> Dot.Stmt
toEdgeStmt i1 i2 attrs = Dot.StmtEdge $ Dot.EdgeStmt (toNodeOrSubgraph i1) [toNodeOrSubgraph i2] attrs

toNodeId :: Dot.Id -> Dot.NodeId
toNodeId i = Dot.NodeId i Nothing

toNodeOrSubgraph :: Dot.Id -> Dot.NodeOrSubgraph
toNodeOrSubgraph = Dot.NodeOrSubgraphNode . toNodeId


--------------------------------------------------------------------------------

{-
:set +m
:module
:module +Prelude
import Hydra.Kernel
import Hydra.Dsl.Terms as Terms
import qualified Data.Map as M
import Hydra.Ext.Staging.Graphviz.Coder
import Hydra.Ext.Staging.Graphviz.Serde as DS
import Hydra.Sources.Libraries as Libs
import Hydra.Ext.Tools.Serialization

name = Name
person n t f = record (Name "Person") [
  field "name" $ string n,
  field "twitter" $ optional (string <$> t),
  field "follows" $ list (var <$> f)]
term = letMulti [
    ("p1", person "Joshua" (Just "joshsh") ["p2", "p4", "p5"]),
    ("p2", person "Deborah" (Just "dlmcuinness") ["p1", "p5"]),
    ("p3", person "Alastair" Nothing []),
    ("p4", person "Molham" (Just "molhamaref") ["p5"]),
    ("p5", person "Ora" (Just "oralassila") ["p1", "p2"])
  ] $ letMulti [
    ("s1", record (Name "Session") [
      field "name" $ string "Graph Standards Rebooted",
      field "moderator" $ var "p1",
      field "panelists" $ list (var <$> ["p2", "p3", "p4", "p5"])])
    ] $ letMulti [
      ("sessionSize", lambda "s" (apply (apply (primitive Libs._math_add) (int32 1))   (apply (primitive Libs._lists_length) (apply (project (Name "Session") (Name "panelists")) (var "s")))))
      ] $ record (Name "SessionInfo") [
        field "session" $ var "s1",
        field "size" (apply (var "sessionSize") (var "s1")),
        field "created" $ string "2024-05-01T17:32:41Z",
        field "updated" $ string "2024-05-06T08:51:03Z"]



-- Full graph
putStrLn $ printExpr $ DS.writeGraph $ termToDotGraph term

-- Accessor graph
putStrLn $ printExpr $ DS.writeGraph $ termToAccessorDotGraph term


term = lambda "x" $ lambda "y" $ pair (var "x") (var "y")
term = lambda "x" $ lambda "y" $ first @@ (pair (var "x") (var "y"))


term = lambda "x" $ var "x"
term = (lambda "f" $ (var "f" @@ var "y")) @@ (lambda "x" $ var "x")
term = var "x" @@ var "y"


term = lambda "x" $ (primitive Libs._math_add @@ var "x" @@ int32 1)
term = (lambda "x" $ (primitive Libs._math_add @@ var "x" @@ int32 1)) @@ int32 1




term = Terms.int32 42
term = Terms.list [Terms.int32 1, Terms.int32 2, Terms.int32 3]

term = Terms.record (Hydra.Core.Name "Person") [
  Terms.field "firstName" $ Terms.string "Arthur",
  Terms.field "lastName" $ Terms.string "Dent",
  Terms.field "age" $ Terms.int32 42]

term = Terms.lambda "x" $ Terms.record (Hydra.Core.Name "Person") [
  Terms.field "firstName" $ Terms.string "Arthur",
  Terms.field "lastName" $ Terms.string "Dent",
  Terms.field "age" $ Terms.int32 42,
  Terms.field "bestFriend" $ Terms.record (Hydra.Core.Name "Person") [
    Terms.field "firstName" $ Terms.string "Ford",
    Terms.field "lastName" $ Terms.string "Prefect",
    Terms.field "age" $ Terms.int32 42,
    Terms.field "foo" $ Terms.var "x"]]

term = Terms.letMulti [("fortytwo", Terms.int32 42)] $ Terms.record (Hydra.Core.Name "Person") [
  Terms.field "firstName" $ Terms.string "Ford",
  Terms.field "lastName" $ Terms.string "Prefect",
  Terms.field "age" $ Terms.var "fortytwo"]

term = Terms.letMulti [("fortytwo", Terms.int32 42), ("ford", Terms.record (Hydra.Core.Name "Person") [
  Terms.field "firstName" $ Terms.string "Ford",
  Terms.field "lastName" $ Terms.string "Prefect",
  Terms.field "age" $ Terms.var "fortytwo",
  Terms.field "bestFriend" $ Terms.var "ford"])] $ Terms.string "foo"





vim /tmp/graph.dot \
  && dot -Tpng /tmp/graph.dot -o /tmp/graph.png \
  && open /tmp/graph.png
-}
