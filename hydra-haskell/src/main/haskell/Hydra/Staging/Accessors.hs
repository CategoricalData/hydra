-- | Utilities for working with term accessors

module Hydra.Staging.Accessors where

import Hydra.Kernel

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


type AccessorPath = [TermAccessor]
data AccessorNode = AccessorNode Name String String deriving Show
data AccessorEdge = AccessorEdge AccessorNode AccessorPath AccessorNode deriving Show
data AccessorGraph = AccessorGraph [AccessorNode] [AccessorEdge] deriving Show

showTermAccessor :: TermAccessor -> Maybe String
showTermAccessor accessor = case accessor of
    TermAccessorAnnotatedSubject -> Nothing
    TermAccessorApplicationFunction -> Just "fun"
    TermAccessorApplicationArgument -> Just "arg"
    TermAccessorLambdaBody -> Just "body"
    TermAccessorOptionalCasesNothing -> Just "nothing"
    TermAccessorOptionalCasesJust -> Just "just"
    TermAccessorUnionCasesDefault -> Just "default"
    TermAccessorUnionCasesBranch name -> Just $ "." ++ unName name
    TermAccessorLetEnvironment -> Just "in"
    TermAccessorLetBinding name -> Just $ unName name ++ "="
    TermAccessorListElement i -> idx i
    TermAccessorMapKey i -> idxSuff ".key" i
    TermAccessorMapValue i -> idxSuff ".value" i
    TermAccessorOptionalTerm -> Just "just"
    TermAccessorProductTerm i -> idx i
    TermAccessorRecordField name -> Just $ "." ++ unName name
    TermAccessorSetElement i -> idx i
    TermAccessorSumTerm -> Nothing
    TermAccessorTypeAbstractionBody -> Nothing
    TermAccessorTypeApplicationTerm -> Nothing
    TermAccessorTypedTerm -> Nothing
    TermAccessorInjectionTerm -> Nothing
    TermAccessorWrappedTerm -> Nothing
  where
--    idx i = Just $ "[" ++ show i ++ "]" -- TODO: restore this
    idx i = Nothing
    idxSuff suffix i = Y.maybe (Just suffix) Just $ fmap (\s -> s ++ suffix) $ idx i

termToAccessorGraph :: M.Map Namespace String -> Term -> AccessorGraph
termToAccessorGraph namespaces term = AccessorGraph nodesX edgesX
  where
    (nodesX, edgesX, _) = helper M.empty Nothing [] ([], [], S.empty) (dontCareAccessor, term)
    dontCareAccessor = TermAccessorAnnotatedSubject
    helper ids mroot path (nodes, edges, visited) (accessor, term) = case term of
        TermLet (Let bindings env) -> helper ids1 mroot nextPath (nodes2, edges2, visited2) (TermAccessorLetEnvironment, env)
          where
            (nodes2, edges2, visited2) = L.foldl addBinding (nodes1++nodes, edges, visited1) (L.zip nodes1 bindings)
              where
                addBinding (nodes, edges, visited) (root, (LetBinding name term1 _))
                    = helper ids1 (Just root) [] (nodes, edges, visited) (dontCareAccessor, term1)
            (nodes1, visited1, ids1) = L.foldl addBinding ([], visited, ids) (letBindingName <$> bindings)
              where
                addBinding (nodes, visited, ids) name =
                    ((node:nodes), S.insert uniqueLabel visited, M.insert name node ids)
                  where
                    node = AccessorNode name rawLabel uniqueLabel
                    rawLabel = toCompactName namespaces name
                    uniqueLabel = toUniqueLabel visited rawLabel
        TermVariable name -> case mroot of
          Nothing -> (nodes, edges, visited)
          Just root -> case M.lookup name ids of
            Nothing -> (nodes, edges, visited)
            Just node -> (nodes, edge:edges, visited)
              where
                edge = AccessorEdge root (L.reverse nextPath) node
        _ -> L.foldl (helper ids mroot nextPath) (nodes, edges, visited) $ subtermsWithAccessors term
      where
        nextPath = accessor:path

toUniqueLabel :: S.Set String -> String -> String
toUniqueLabel visited l = if S.member l visited then toUniqueLabel visited (l ++ "'") else l
