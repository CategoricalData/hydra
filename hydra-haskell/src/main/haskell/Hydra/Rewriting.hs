module Hydra.Rewriting where

import Hydra.Core
import Hydra.Impl.Haskell.Extras
import Hydra.Graph
import Hydra.Primitives
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
freeVariablesInTerm term = case termExpr term of
  TermExprFunction (FunctionLambda (Lambda var body)) -> S.delete var $ freeVariablesInTerm body
  TermExprVariable v -> S.fromList [v]
  _ -> L.foldl (\s t -> S.union s $ freeVariablesInTerm t) S.empty $ subterms term

isFreeIn :: Variable -> Term m -> Bool
isFreeIn v term = not $ S.member v $ freeVariablesInTerm term

replaceFreeTypeVariable :: Ord m => TypeVariable -> Type m -> Type m -> Type m
replaceFreeTypeVariable v rep = rewriteType mapExpr id
  where
    mapExpr recurse t = case typeExpr t of
      TypeExprLambda (TypeLambda v' body) -> if v == v'
        then t
        else t {typeExpr = TypeExprLambda $ TypeLambda v' $ recurse body}
      TypeExprVariable v' -> if v == v' then rep else t
      _ -> recurse t

rewriteTerm :: (Ord a, Ord b) => ((Term a -> Term b) -> Term a -> Term b) -> (a -> b) -> Term a -> Term b
rewriteTerm mapExpr mapMeta = replace
  where
    replace = mapExpr recurse
    replaceField f = f {fieldTerm = replace (fieldTerm f)}
    recurse (Term expr meta) = Term expr1 $ mapMeta meta
      where
        expr1 = case expr of
          TermExprApplication (Application lhs rhs) -> TermExprApplication $ Application (replace lhs) (replace rhs)
          TermExprElement name -> TermExprElement name
          TermExprFunction fun -> TermExprFunction $ case fun of
            FunctionCompareTo other -> FunctionCompareTo $ replace other
            FunctionElimination e -> FunctionElimination $ case e of
              EliminationElement -> EliminationElement
              EliminationNominal name -> EliminationNominal name
              EliminationOptional (OptionalCases nothing just) -> EliminationOptional
                (OptionalCases (replace nothing) (replace just))
              EliminationRecord fname -> EliminationRecord fname
              EliminationUnion fields -> EliminationUnion $ replaceField <$> fields
            FunctionLambda (Lambda v body) -> FunctionLambda $ Lambda v $ replace body
            FunctionPrimitive name -> FunctionPrimitive name
          TermExprLet (Let v t1 t2) -> TermExprLet $ Let v (replace t1) (replace t2)
          TermExprList els -> TermExprList $ replace <$> els
          TermExprLiteral v -> TermExprLiteral v
          TermExprMap m -> TermExprMap $ M.fromList $ (\(k, v) -> (replace k, replace v)) <$> M.toList m
          TermExprNominal (Named name t) -> TermExprNominal (Named name $ replace t)
          TermExprOptional m -> TermExprOptional $ replace <$> m
          TermExprRecord fields -> TermExprRecord $ replaceField <$> fields
          TermExprSet s -> TermExprSet $ S.fromList $ replace <$> S.toList s
          TermExprUnion field -> TermExprUnion $ replaceField field
          TermExprVariable v -> TermExprVariable v

rewriteTermMeta :: (Ord a, Ord b) => (a -> b) -> Term a -> Term b
rewriteTermMeta mapMeta = rewriteTerm mapExpr mapMeta
  where
    mapExpr recurse term = recurse term

rewriteType :: (Ord a, Ord b) => ((Type a -> Type b) -> Type a -> Type b) -> (a -> b) -> Type a -> Type b
rewriteType mapExpr mapMeta = replace
  where
    replace = mapExpr recurse
    replaceField f = f {fieldTypeType = replace (fieldTypeType f)}
    recurse (Type expr meta) = Type expr1 $ mapMeta meta
      where
        expr1 = case expr of
          TypeExprApplication (TypeApplication lhs rhs) -> TypeExprApplication $ TypeApplication (replace lhs) (replace rhs)
          TypeExprElement t -> TypeExprElement $ replace t
          TypeExprFunction (FunctionType dom cod) -> TypeExprFunction (FunctionType (replace dom) (replace cod))
          TypeExprLambda (TypeLambda v b) -> TypeExprLambda (TypeLambda v $ replace b)
          TypeExprList t -> TypeExprList $ replace t
          TypeExprLiteral lt -> TypeExprLiteral lt
          TypeExprMap (MapType kt vt) -> TypeExprMap (MapType (replace kt) (replace vt))
          TypeExprNominal name -> TypeExprNominal name
          TypeExprOptional t -> TypeExprOptional $ replace t
          TypeExprRecord fields -> TypeExprRecord $ replaceField <$> fields
          TypeExprSet t -> TypeExprSet $ replace t
          TypeExprUnion fields -> TypeExprUnion $ replaceField <$> fields
          TypeExprVariable v -> TypeExprVariable v

rewriteTypeMeta :: (Ord a, Ord b) => (a -> b) -> Type a -> Type b
rewriteTypeMeta mapMeta = rewriteType mapExpr mapMeta
  where
    mapExpr recurse term = recurse term

simplifyTerm :: (Default m, Ord m) => Term m -> Term m
simplifyTerm = rewriteTerm simplify id
  where
    simplify recurse term = recurse $ case termExpr term of
      TermExprApplication (Application lhs rhs) -> case termExpr lhs of
        TermExprFunction (FunctionLambda (Lambda var body)) ->
          if S.member var (freeVariablesInTerm body)
            then case termExpr rhs of
              TermExprVariable v -> simplifyTerm $ substituteVariable var v body
              _ -> term
            else simplifyTerm body
        _ -> term
      _ -> term

stripMeta :: (Default m, Ord m) => Term m -> Term m
stripMeta = rewriteTermMeta $ const dflt

substituteVariable :: Ord m => Variable -> Variable -> Term m -> Term m
substituteVariable from to = rewriteTerm replace id
  where
    replace recurse term = case termExpr term of
      TermExprVariable x -> recurse $ Term (TermExprVariable $ if x == from then to else x) $ termMeta term
      TermExprFunction (FunctionLambda (Lambda var _)) -> if var == from
        then term
        else recurse term
      _ -> recurse term

subterms :: Term m -> [Term m]
subterms term = case termExpr term of
  TermExprApplication (Application lhs rhs) -> [lhs, rhs]
  TermExprFunction f -> case f of
    FunctionCompareTo other -> [other]
    FunctionElimination e -> case e of
      EliminationOptional (OptionalCases nothing just) -> [nothing, just]
      EliminationUnion cases -> fieldTerm <$> cases
      _ -> []
    FunctionLambda (Lambda _ body) -> [body]
    _ -> []
  TermExprLet (Let _ t1 t2) -> [t1, t2]
  TermExprList els -> els
  TermExprMap m -> L.concat ((\(k, v) -> [k, v]) <$> M.toList m)
  TermExprNominal (Named _ t) -> [t]
  TermExprOptional m -> Y.maybeToList m
  TermExprRecord fields -> fieldTerm <$> fields
  TermExprSet s -> S.toList s
  TermExprUnion field -> [fieldTerm field]
  _ -> []

subtypes :: Type m -> [Type m]
subtypes typ = case typeExpr typ of
  TypeExprApplication (TypeApplication lhs rhs) -> [lhs, rhs]
  TypeExprElement et -> [et]
  TypeExprFunction (FunctionType dom cod) -> [dom, cod]
  TypeExprLambda (TypeLambda v body) -> [body]
  TypeExprList lt -> [lt]
  TypeExprLiteral _ -> []
  TypeExprMap (MapType kt vt) -> [kt, vt]
  TypeExprNominal _ -> []
  TypeExprOptional ot -> [ot]
  TypeExprRecord fields -> fieldTypeType <$> fields
  TypeExprSet st -> [st]
  TypeExprUnion fields -> fieldTypeType <$> fields
  TypeExprVariable _ -> []

termDependencyNames :: Bool -> Bool -> Bool -> Term m -> S.Set Name
termDependencyNames withEls withPrims withNoms = foldOverTerm TraversalOrderPre addNames S.empty
  where
    addNames names term = case termExpr term of
      TermExprElement name -> if withEls then S.insert name names else names
      TermExprFunction (FunctionPrimitive name) -> if withPrims then S.insert name names else names
      TermExprNominal (Named name _) -> if withNoms then S.insert name names else names
      _ -> names

topologicalSortElements :: [Element m] -> Maybe [Name]
topologicalSortElements els = topologicalSort $ adjlist <$> els
  where
    adjlist e = (elementName e, S.toList $ termDependencyNames True True True $ elementData e)

typeDependencies :: (Default m, Show m) => Context m -> Name -> Result (M.Map Name (Type m))
typeDependencies scx name = deps (S.fromList [name]) M.empty
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
          typ <- requireType scx name
          return (name, typ)

    requireType scx name = do
      el <- requireElement (Just "type dependencies") scx name
      decodeType scx (elementData el)

typeDependencyNames :: Type m -> S.Set Name
typeDependencyNames = foldOverType TraversalOrderPre addNames S.empty
  where
    addNames names typ = case typeExpr typ of
      TypeExprNominal name -> S.insert name names
      _ -> names
