module Hydra.Ext.Graphviz.Coder (encodeAsDotGraph) where

import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Ext.Org.Graphviz.Dot as Dot

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


encodeAsDotGraph :: Term -> Dot.Graph
encodeAsDotGraph term = Dot.Graph {
  Dot.graphStrict = False,
  Dot.graphDirected = True,
  Dot.graphId = Nothing,
  Dot.graphStatements = encodeAsDotStatements standardNamespaces term}

encodeAsDotStatements :: M.Map Namespace String -> Term -> [Dot.Stmt]
encodeAsDotStatements namespaces term = fst $ encode Nothing M.empty Nothing ([], S.empty) (TermAccessorAnnotatedSubject, term)
  where
    encode mlabel ids mparent (stmts, visited) (accessor, term) = case term of
        TermFunction (FunctionLambda (Lambda v _ body)) ->
            encode Nothing ids1 (Just selfId) (selfStmts, selfVisited) (TermAccessorLambdaBody, body)
          where
            ids1 = M.insert v selfId ids
        TermLet (Let bindings env) -> encode Nothing ids1 (Just selfId) (stmts1, visited2) (TermAccessorLetEnvironment, env)
          where
            (stmts1, visited2) = L.foldl addBinding (selfStmts, selfVisited) bindings
              where
                addBinding (stmts, visited) (LetBinding name trm _) = encode (Just lab) ids1 (Just selfId) (stmts, visited) (TermAccessorLetBinding name, trm)
                  where
                    lab = Dot.unId $ Y.fromJust $ M.lookup name ids1
            (ids1, visited1) = L.foldl addBinding (ids, visited) bindings
              where
                addBinding (ids, visited) (LetBinding name trm _) = (M.insert name (Dot.Id lab) ids, S.insert lab visited)
                  where
                    lab = labelOf visited trm
        TermVariable name -> case M.lookup name ids of
          Just i -> (stmts ++ [toEdgeStmt (Y.fromJust mparent) i], visited)
          Nothing -> dflt
        _ -> dflt
      where
        dflt = L.foldl (encode Nothing ids $ Just selfId) (selfStmts, selfVisited) $ subtermsWithAccessors term
        selfVisited = S.insert label visited
        selfStmts = stmts ++ [nodeStmt] ++ parentStmt
        parentStmt = case mparent of
          Nothing -> []
          Just parent -> [toEdgeStmt parent selfId]
        toEdgeStmt i1 i2 = Dot.StmtEdge $ Dot.EdgeStmt (toNodeOrSubgraph i1) [toNodeOrSubgraph i2] attrs
          where
            attrs = fmap (\l -> Dot.AttrList [[Dot.EqualityPair (Dot.Id "label") (Dot.Id l)]]) elabel
            elabel = case accessor of
              TermAccessorAnnotatedSubject -> Nothing
              TermAccessorApplicationFunction -> Just "fun"
              TermAccessorApplicationArgument -> Just "arg"
              TermAccessorLambdaBody -> Just "body"
              TermAccessorListFold -> Nothing
              TermAccessorOptionalCasesNothing -> Just "nothing"
              TermAccessorOptionalCasesJust -> Just "just"
              TermAccessorUnionCasesDefault -> Just "default"
              TermAccessorUnionCasesBranch name -> Just $ "." ++ unName name
              TermAccessorLetEnvironment -> Just "env"
              TermAccessorLetBinding name -> Just $ unName name ++ "="
              TermAccessorListElement i -> Just $ idx i
              TermAccessorMapKey i -> Just $ idx i ++ ".key"
              TermAccessorMapValue i -> Just $ idx i ++ ".value"
              TermAccessorOptionalTerm -> Nothing
              TermAccessorProductTerm i -> Just $ idx i
              TermAccessorRecordField name -> Just $ "." ++ unName name
              TermAccessorSetElement i -> Just $ idx i
              TermAccessorSumTerm -> Nothing
              TermAccessorTypeAbstractionBody -> Nothing
              TermAccessorTypeApplicationTerm -> Nothing
              TermAccessorTypedTerm -> Nothing
              TermAccessorInjectionTerm -> Nothing
              TermAccessorWrappedTerm -> Nothing
        idx i = "[" ++ show i ++ "]"
        toNodeId i = Dot.NodeId i Nothing
        toNodeOrSubgraph = Dot.NodeOrSubgraphNode . toNodeId
        nodeStmt = Dot.StmtNode $ Dot.NodeStmt {
          Dot.nodeStmtId = toNodeId selfId,
          Dot.nodeStmtAttributes = Nothing}
        idOf :: Name -> String
        idOf name = case M.lookup name ids of
            Just (Dot.Id d) -> d
            Nothing -> case mns of
              Nothing -> unName name
              Just ns -> case M.lookup ns namespaces of
                Just pre -> pre ++ ":" ++ local
                Nothing -> unName name
          where
            (QualifiedName mns local) = qualifyNameLazy name
        withLabel = id
        selfId = Dot.Id label
        label = Y.fromMaybe (labelOf visited term) mlabel
        labelOf visited term = if S.member label0 visited then label0 ++ "'" else label0
          where
            label0 = rawLabelOf term
        rawLabelOf term = case term of
          TermAnnotated (AnnotatedTerm term1 _) -> withLabel "@{...}"
          TermApplication _ -> withLabel "apply"
          TermFunction f -> case f of
            FunctionLambda (Lambda v _ body) -> withLabel $ "\\\\" ++ unName v ++ "."
            FunctionElimination e -> case e of
              EliminationList _ -> withLabel "fold"
              EliminationOptional _ -> withLabel "foldOpt"
              EliminationProduct (TupleProjection i _) -> withLabel $ "[" ++ show i ++ "]"
              EliminationRecord (Projection tname fname) -> withLabel $ "{" ++ idOf tname ++ "}." ++ unName fname
              EliminationUnion (CaseStatement tname _ _) -> withLabel "cases_{" ++ idOf tname ++ "}"
              EliminationWrap name -> withLabel $ "unwrap_{" ++ idOf name ++ "}"
            FunctionPrimitive name -> withLabel $ idOf name
          TermLet (Let bindings env) -> withLabel "let"
          TermList _ -> withLabel "list"
          TermLiteral l -> withLabel $ case l of
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
            LiteralString s -> s
          TermMap _ -> withLabel "map"
          TermOptional _ -> withLabel "optional"
          TermProduct _ -> withLabel "product"
          TermRecord (Record name _) -> withLabel $ "record_{" ++ idOf name ++ "}"
          TermTypeAbstraction (TypeAbstraction v term1) -> withLabel "tyabs"
          TermTypeApplication (TypedTerm term _) -> withLabel "tyapp"
          TermUnion (Injection tname _) -> "union_{" ++ idOf tname ++ "}"
          TermTyped (TypedTerm term1 _) -> withLabel ":t"
          TermVariable name -> withLabel $ idOf name
          TermWrap (WrappedTerm name term1) -> "wrap_{" ++ idOf name ++ "}"
          _ -> withLabel "?"

standardNamespaces :: M.Map Namespace String
standardNamespaces = M.fromList (toPair <$> standardLibraries)
  where
    toPair lib = (libraryNamespace lib, libraryPrefix lib)


{-
:set +m
import Hydra.Dsl.Terms as Terms
import qualified Data.Map as M
import Hydra.Ext.Graphviz.Serde as DS

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

putStrLn $ printExpr $ DS.writeGraph $ encodeAsDotGraph term

vim /tmp/graph.dot \
  && dot -Tpng /tmp/graph.dot -o /tmp/graph.png \
  && open /tmp/graph.png
-}