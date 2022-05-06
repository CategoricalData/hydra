module Hydra.Rewriting (
  TraversalOrder(..),
  foldOverData,
  foldOverType,
  freeVariablesInData,
  isFreeIn,
  rewriteData,
  rewriteDataMeta,
  rewriteType,
  rewriteTypeMeta,
  simplifyData,
  stripMeta,
  substituteVariable,
  subterms,
  subtypes,
  termDependencyNames,
  topologicalSortElements,
  typeDependencyNames,
  typeDependencies,
  ) where

import Hydra.Core
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Dsl.CoreMeta
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

foldOverData :: TraversalOrder -> (a -> Data m -> a) -> a -> Data m -> a
foldOverData order fld b0 term = case order of
    TraversalOrderPre -> L.foldl (foldOverData order fld) (fld b0 term) children
    TraversalOrderPost -> fld (L.foldl (foldOverData order fld) b0 children) term
  where
    children = subterms term

foldOverType :: TraversalOrder -> (a -> Type m -> a) -> a -> Type m -> a
foldOverType order fld b0 typ = case order of
    TraversalOrderPre -> L.foldl (foldOverType order fld) (fld b0 typ) children
    TraversalOrderPost -> fld (L.foldl (foldOverType order fld) b0 children) typ
  where
    children = subtypes typ

freeVariablesInData :: Data m -> S.Set Variable
freeVariablesInData term = case dataTerm term of
  DataTermFunction (FunctionLambda (Lambda var body)) -> S.delete var $ freeVariablesInData body
  DataTermVariable v -> S.fromList [v]
  _ -> L.foldl (\s t -> S.union s $ freeVariablesInData t) S.empty $ subterms term

isFreeIn :: Variable -> Data m -> Bool
isFreeIn v term = not $ S.member v $ freeVariablesInData term

rewriteData :: (Ord a, Ord b) => ((Data a -> Data b) -> Data a -> Data b) -> (a -> b) -> Data a -> Data b
rewriteData mapData mapMeta = replace
  where
    replace = mapData recurse
    replaceField f = f {fieldData = replace (fieldData f)}
    recurse (Data expr meta) = Data expr1 $ mapMeta meta
      where
        expr1 = case expr of
          DataTermApplication (Application lhs rhs) -> DataTermApplication $ Application (replace lhs) (replace rhs)
          DataTermElement name -> DataTermElement name
          DataTermFunction fun -> DataTermFunction $ case fun of
            FunctionCases fields -> FunctionCases $ replaceField <$> fields
            FunctionCompareTo other -> FunctionCompareTo $ replace other
            FunctionDelta -> FunctionDelta
            FunctionLambda (Lambda v body) -> FunctionLambda $ Lambda v $ replace body
            FunctionOptionalCases (OptionalCases nothing just) -> FunctionOptionalCases
              (OptionalCases (replace nothing) (replace just))
            FunctionPrimitive name -> FunctionPrimitive name
            FunctionProjection fname -> FunctionProjection fname
          DataTermLet (Let v t1 t2) -> DataTermLet $ Let v (replace t1) (replace t2)
          DataTermList els -> DataTermList $ replace <$> els
          DataTermLiteral v -> DataTermLiteral v
          DataTermMap m -> DataTermMap $ M.fromList $ (\(k, v) -> (replace k, replace v)) <$> M.toList m
          DataTermNominal (Named name t) -> DataTermNominal (Named name $ replace t)
          DataTermOptional m -> DataTermOptional $ replace <$> m
          DataTermRecord fields -> DataTermRecord $ replaceField <$> fields
          DataTermSet s -> DataTermSet $ S.fromList $ replace <$> S.toList s
          DataTermTypeAbstraction (TypeAbstraction v b0) -> DataTermTypeAbstraction $ TypeAbstraction v (replace b0)
          DataTermTypeApplication (TypeApplication f t) -> DataTermTypeApplication $ TypeApplication (replace f) $
            rewriteTypeMeta mapMeta t
          DataTermUnion field -> DataTermUnion $ replaceField field
          DataTermVariable v -> DataTermVariable v

rewriteDataMeta :: (Ord a, Ord b) => (a -> b) -> Data a -> Data b
rewriteDataMeta mapMeta = rewriteData mapData mapMeta
  where
    mapData recurse term = recurse term

rewriteType :: (Ord a, Ord b) => ((Type a -> Type b) -> Type a -> Type b) -> (a -> b) -> Type a -> Type b
rewriteType mapData mapMeta = replace
  where
    replace = mapData recurse
    replaceField f = f {fieldTypeType = replace (fieldTypeType f)}
    recurse (Type expr meta) = Type expr1 $ mapMeta meta
      where
        expr1 = case expr of
          TypeTermElement t -> TypeTermElement $ replace t
          TypeTermFunction (FunctionType dom cod) -> TypeTermFunction (FunctionType (replace dom) (replace cod))
          TypeTermList t -> TypeTermList $ replace t
          TypeTermLiteral lt -> TypeTermLiteral lt
          TypeTermMap (MapType kt vt) -> TypeTermMap (MapType (replace kt) (replace vt))
          TypeTermNominal name -> TypeTermNominal name
          TypeTermOptional t -> TypeTermOptional $ replace t
          TypeTermRecord fields -> TypeTermRecord $ replaceField <$> fields
          TypeTermSet t -> TypeTermSet $ replace t
          TypeTermUnion fields -> TypeTermUnion $ replaceField <$> fields
          TypeTermUniversal (UniversalType v b) -> TypeTermUniversal (UniversalType v $ replace b)
          TypeTermVariable v -> TypeTermVariable v

rewriteTypeMeta :: (Ord a, Ord b) => (a -> b) -> Type a -> Type b
rewriteTypeMeta mapMeta = rewriteType mapData mapMeta
  where
    mapData recurse term = recurse term

simplifyData :: (Default m, Ord m) => Data m -> Data m
simplifyData = rewriteData simplify id
  where
    simplify recurse term = recurse $ case dataTerm term of
      DataTermApplication (Application lhs rhs) -> case dataTerm lhs of
        DataTermFunction (FunctionLambda (Lambda var body)) -> if S.member var (freeVariablesInData body)
          then case dataTerm rhs of
            DataTermVariable v -> substituteVariable var v body
            _ -> term
          else body
        _ -> term
      _ -> term

stripMeta :: (Default m, Ord m) => Data m -> Data m
stripMeta = rewriteDataMeta $ \_ -> dflt

substituteVariable :: (Default m, Ord m) => Variable -> Variable -> Data m -> Data m
substituteVariable from to = rewriteData replace id
  where
    replace recurse term = case dataTerm term of
      DataTermVariable x -> recurse $ Data (DataTermVariable $ if x == from then to else x) $ dataMeta term
      DataTermFunction (FunctionLambda (Lambda var _)) -> if var == from
        then term
        else recurse term
      _ -> recurse term

subterms :: Data m -> [Data m]
subterms term = case dataTerm term of
  DataTermApplication (Application lhs rhs) -> [lhs, rhs]
  DataTermFunction f -> case f of
    FunctionCases cases -> fieldData <$> cases
    FunctionCompareTo other -> [other]
    FunctionLambda (Lambda _ body) -> [body]
    FunctionOptionalCases (OptionalCases nothing just) -> [nothing, just]
    _ -> []
  DataTermLet (Let _ t1 t2) -> [t1, t2]
  DataTermList els -> els
  DataTermMap m -> L.concat ((\(k, v) -> [k, v]) <$> M.toList m)
  DataTermNominal (Named _ t) -> [t]
  DataTermOptional m -> Y.maybeToList m
  DataTermRecord fields -> fieldData <$> fields
  DataTermSet s -> S.toList s
  DataTermUnion field -> [fieldData field]
  _ -> []

subtypes :: Type m -> [Type m]
subtypes typ = case typeTerm typ of
  TypeTermElement et -> [et]
  TypeTermFunction (FunctionType dom cod) -> [dom, cod]
  TypeTermList lt -> [lt]
  TypeTermLiteral _ -> []
  TypeTermMap (MapType kt vt) -> [kt, vt]
  TypeTermNominal _ -> []
  TypeTermOptional ot -> [ot]
  TypeTermRecord fields -> fieldTypeType <$> fields
  TypeTermSet st -> [st]
  TypeTermUnion fields -> fieldTypeType <$> fields
  TypeTermUniversal (UniversalType v body) -> [body]
  TypeTermVariable _ -> []

termDependencyNames :: Bool -> Bool -> Bool -> Data m -> S.Set Name
termDependencyNames withEls withPrims withNoms = foldOverData TraversalOrderPre addNames S.empty
  where
    addNames names term = case dataTerm term of
      DataTermElement name -> if withEls then S.insert name names else names
      DataTermFunction (FunctionPrimitive name) -> if withPrims then S.insert name names else names
      DataTermNominal (Named name _) -> if withNoms then S.insert name names else names
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
      el <- requireElement scx name
      decodeType scx (elementData el)

typeDependencyNames :: Type m -> S.Set Name
typeDependencyNames = foldOverType TraversalOrderPre addNames S.empty
  where
    addNames names typ = case typeTerm typ of
      TypeTermNominal name -> S.insert name names
      _ -> names
