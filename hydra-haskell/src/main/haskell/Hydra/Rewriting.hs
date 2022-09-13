module Hydra.Rewriting where

import Hydra.Core
import Hydra.Monads
import Hydra.Graph
import Hydra.Lexical
import Hydra.Evaluation
import Hydra.CoreDecoding
import Hydra.Sorting

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


data TraversalOrder = TraversalOrderPre | TraversalOrderPost

foldOverTerm :: TraversalOrder -> (a -> Term m -> a) -> a -> Term m -> a
foldOverTerm order fld b0 term = case order of
    TraversalOrderPre -> L.foldl (foldOverTerm order fld) (fld b0 term) children
    TraversalOrderPost -> fld (L.foldl (foldOverTerm order fld) b0 children) term
  where
    children = subterms term

foldOverType :: TraversalOrder -> (a -> Type m -> a) -> a -> Type m -> a
foldOverType order fld b0 typ = case order of
    TraversalOrderPre -> L.foldl (foldOverType order fld) (fld b0 typ) children
    TraversalOrderPost -> fld (L.foldl (foldOverType order fld) b0 children) typ
  where
    children = subtypes typ

freeVariablesInTerm :: Term m -> S.Set Variable
freeVariablesInTerm term = case term of
  TermFunction (FunctionLambda (Lambda var body)) -> S.delete var $ freeVariablesInTerm body
  TermVariable v -> S.fromList [v]
  _ -> L.foldl (\s t -> S.union s $ freeVariablesInTerm t) S.empty $ subterms term

graphDependencies :: Bool -> Bool -> Bool -> Graph m -> S.Set GraphName
graphDependencies withEls withPrims withNoms g = S.delete (graphName g) graphNames
  where
    graphNames = S.fromList (graphNameOf <$> S.toList elNames)
    elNames = L.foldl (\s t -> S.union s $ termDependencyNames withEls withPrims withNoms t) S.empty $
      (elementData <$> graphElements g) ++ (elementSchema <$> graphElements g)

isFreeIn :: Variable -> Term m -> Bool
isFreeIn v term = not $ S.member v $ freeVariablesInTerm term

replaceFreeVariableType :: Ord m => VariableType -> Type m -> Type m -> Type m
replaceFreeVariableType v rep = rewriteType mapExpr id
  where
    mapExpr recurse t = case t of
      TypeLambda (LambdaType v' body) -> if v == v'
        then t
        else TypeLambda $ LambdaType v' $ recurse body
      TypeVariable v' -> if v == v' then rep else t
      _ -> recurse t

rewriteTerm :: Ord b => ((Term a -> Term b) -> Term a -> Term b) -> (a -> b) -> Term a -> Term b
rewriteTerm mapExpr mapMeta = replace
  where
    replace = mapExpr recurse
    replaceField f = f {fieldTerm = replace (fieldTerm f)}
    recurse term = case term of
      TermAnnotated (Annotated ex ma) -> TermAnnotated $ Annotated (replace ex) (mapMeta ma)
      TermApplication (Application lhs rhs) -> TermApplication $ Application (replace lhs) (replace rhs)
      TermElement name -> TermElement name
      TermFunction fun -> TermFunction $ case fun of
        FunctionCompareTo other -> FunctionCompareTo $ replace other
        FunctionElimination e -> FunctionElimination $ case e of
          EliminationElement -> EliminationElement
          EliminationNominal name -> EliminationNominal name
          EliminationOptional (OptionalCases nothing just) -> EliminationOptional
            (OptionalCases (replace nothing) (replace just))
          EliminationRecord p -> EliminationRecord p
          EliminationUnion (CaseStatement n cases) -> EliminationUnion $ CaseStatement n (replaceField <$> cases)
        FunctionLambda (Lambda v body) -> FunctionLambda $ Lambda v $ replace body
        FunctionPrimitive name -> FunctionPrimitive name
      TermLet (Let v t1 t2) -> TermLet $ Let v (replace t1) (replace t2)
      TermList els -> TermList $ replace <$> els
      TermLiteral v -> TermLiteral v
      TermMap m -> TermMap $ M.fromList $ (\(k, v) -> (replace k, replace v)) <$> M.toList m
      TermNominal (Named name t) -> TermNominal (Named name $ replace t)
      TermOptional m -> TermOptional $ replace <$> m
      TermRecord (Record n fields) -> TermRecord $ Record n $ replaceField <$> fields
      TermSet s -> TermSet $ S.fromList $ replace <$> S.toList s
      TermUnion (Union n field) -> TermUnion $ Union n $ replaceField field
      TermVariable v -> TermVariable v

rewriteTermMeta :: Ord b => (a -> b) -> Term a -> Term b
rewriteTermMeta = rewriteTerm mapExpr
  where
    mapExpr recurse term = recurse term

rewriteType :: ((Type a -> Type b) -> Type a -> Type b) -> (a -> b) -> Type a -> Type b
rewriteType mapExpr mapMeta = replace
  where
    replace = mapExpr recurse
    replaceField f = f {fieldTypeType = replace (fieldTypeType f)}
    recurse typ = case typ of
      TypeAnnotated (Annotated t ann) -> TypeAnnotated $ Annotated (replace t) (mapMeta ann)
      TypeApplication (ApplicationType lhs rhs) -> TypeApplication $ ApplicationType (replace lhs) (replace rhs)
      TypeElement t -> TypeElement $ replace t
      TypeFunction (FunctionType dom cod) -> TypeFunction (FunctionType (replace dom) (replace cod))
      TypeLambda (LambdaType v b) -> TypeLambda (LambdaType v $ replace b)
      TypeList t -> TypeList $ replace t
      TypeLiteral lt -> TypeLiteral lt
      TypeMap (MapType kt vt) -> TypeMap (MapType (replace kt) (replace vt))
      TypeNominal name -> TypeNominal name
      TypeOptional t -> TypeOptional $ replace t
      TypeRecord (RowType name fields) -> TypeRecord $ RowType name (replaceField <$> fields)
      TypeSet t -> TypeSet $ replace t
      TypeUnion (RowType name fields) -> TypeUnion $ RowType name (replaceField <$> fields)
      TypeVariable v -> TypeVariable v

rewriteTypeMeta :: (a -> b) -> Type a -> Type b
rewriteTypeMeta = rewriteType mapExpr
  where
    mapExpr recurse term = recurse term

simplifyTerm :: Ord m => Term m -> Term m
simplifyTerm = rewriteTerm simplify id
  where
    simplify recurse term = recurse $ case term of
      TermApplication (Application lhs rhs) -> case lhs of
        TermFunction (FunctionLambda (Lambda var body)) ->
          if S.member var (freeVariablesInTerm body)
            then case rhs of
              TermVariable v -> simplifyTerm $ substituteVariable var v body
              _ -> term
            else simplifyTerm body
        _ -> term
      _ -> term

substituteVariable :: Ord m => Variable -> Variable -> Term m -> Term m
substituteVariable from to = rewriteTerm replace id
  where
    replace recurse term = case term of
      TermVariable x -> recurse $ (TermVariable $ if x == from then to else x)
      TermFunction (FunctionLambda (Lambda var _)) -> if var == from
        then term
        else recurse term
      _ -> recurse term

subterms :: Term m -> [Term m]
subterms term = case term of
  TermAnnotated (Annotated t _) -> [t]
  TermApplication (Application lhs rhs) -> [lhs, rhs]
  TermFunction f -> case f of
    FunctionCompareTo other -> [other]
    FunctionElimination e -> case e of
      EliminationOptional (OptionalCases nothing just) -> [nothing, just]
      EliminationUnion (CaseStatement _ cases) -> fieldTerm <$> cases
      _ -> []
    FunctionLambda (Lambda _ body) -> [body]
    _ -> []
  TermLet (Let _ t1 t2) -> [t1, t2]
  TermList els -> els
  TermMap m -> L.concat ((\(k, v) -> [k, v]) <$> M.toList m)
  TermNominal (Named _ t) -> [t]
  TermOptional m -> Y.maybeToList m
  TermRecord (Record n fields) -> fieldTerm <$> fields
  TermSet s -> S.toList s
  TermUnion (Union _ field) -> [fieldTerm field]
  _ -> []

subtypes :: Type m -> [Type m]
subtypes typ = case typ of
  TypeAnnotated (Annotated t _) -> [t]
  TypeApplication (ApplicationType lhs rhs) -> [lhs, rhs]
  TypeElement et -> [et]
  TypeFunction (FunctionType dom cod) -> [dom, cod]
  TypeLambda (LambdaType v body) -> [body]
  TypeList lt -> [lt]
  TypeLiteral _ -> []
  TypeMap (MapType kt vt) -> [kt, vt]
  TypeNominal _ -> []
  TypeOptional ot -> [ot]
  TypeRecord rt -> fieldTypeType <$> rowTypeFields rt
  TypeSet st -> [st]
  TypeUnion rt -> fieldTypeType <$> rowTypeFields rt
  TypeVariable _ -> []

termDependencyNames :: Bool -> Bool -> Bool -> Term m -> S.Set Name
termDependencyNames withEls withPrims withNoms = foldOverTerm TraversalOrderPre addNames S.empty
  where
    addNames names term = case term of
      TermElement name -> if withEls then S.insert name names else names
      TermFunction (FunctionPrimitive name) -> if withPrims then S.insert name names else names
      TermNominal (Named name _) -> if withNoms then S.insert name names else names
      _ -> names

topologicalSortElements :: [Element m] -> Maybe [Name]
topologicalSortElements els = topologicalSort $ adjlist <$> els
  where
    adjlist e = (elementName e, S.toList $ termDependencyNames True True True $ elementData e)

typeDependencies :: Show m => Name -> GraphFlow m (M.Map Name (Type m))
typeDependencies name = deps (S.fromList [name]) M.empty
  where
    deps seeds names = if S.null seeds
        then return names
        else do
          pairs <- CM.mapM toPair $ S.toList seeds
          let newNames = M.union names (M.fromList pairs)
          let refs = L.foldl S.union S.empty (typeDependencyNames <$> (snd <$> pairs))
          let visited = S.fromList $ M.keys names
          let newSeeds = S.difference refs visited
          deps newSeeds newNames
      where
        toPair name = do
          typ <- requireType name
          return (name, typ)

    requireType name = do
      pushTrc "type dependencies"
      el <- requireElement name
      decodeType (elementData el)

typeDependencyNames :: Type m -> S.Set Name
typeDependencyNames = foldOverType TraversalOrderPre addNames S.empty
  where
    addNames names typ = case typ of
      TypeNominal name -> S.insert name names
      _ -> names
