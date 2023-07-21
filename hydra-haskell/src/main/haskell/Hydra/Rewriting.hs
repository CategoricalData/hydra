-- | Functions for type and term rewriting

module Hydra.Rewriting where

import Hydra.Basics
import Hydra.Coders
import Hydra.Compute
import Hydra.Core
import Hydra.TypeEncoding
import Hydra.Extras
import Hydra.Graph
import Hydra.Flows
import Hydra.Module
import Hydra.Lexical
import Hydra.Mantle
import Hydra.Tools.Sorting

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


elementsWithDependencies :: [Element a] -> GraphFlow a [Element a]
elementsWithDependencies original = CM.mapM requireElement allDepNames
  where
    depNames = S.toList . termDependencyNames True False False . elementData
    allDepNames = L.nub $ (elementName <$> original) ++ (L.concat $ depNames <$> original)

-- | Turn arbitrary terms like 'add 42' into terms like '\x.add 42 x',
--   whose arity (in the absences of application terms) is equal to the depth of nested lambdas.
--   This function leaves application terms intact, simply rewriting their left and right subterms.
expandLambdas :: (Ord a, Show a) => Term a -> GraphFlow a (Term a)
expandLambdas term = do
    g <- getState
    rewriteTermM (expand g Nothing []) (pure . id) term
  where
    expand g mtyp args recurse term = case term of
        TermAnnotated (Annotated term' ann) -> do
          mt <- annotationClassTermType (graphAnnotations g) term
          expanded <- expand g (Y.maybe mtyp Just mt) args recurse term'
          return $ TermAnnotated $ Annotated expanded ann
        TermApplication (Application lhs rhs) -> do
          rhs' <- expandLambdas rhs
          expand g Nothing (rhs':args) recurse lhs
        TermFunction f -> case f of
          FunctionElimination _ -> pad g mtyp args 1 <$> recurse term
          FunctionPrimitive name -> do
            prim <- requirePrimitive name
            return $ pad g mtyp args (primitiveArity prim) term
          _ -> passThrough
        _ -> passThrough
      where
        passThrough = pad g mtyp args 0 <$> recurse term

    pad g mtyp args arity term = L.foldl lam (app mtyp term args') $ L.reverse variables
      where
        variables = L.take (max 0 (arity - L.length args)) ((\i -> Name $ "v" ++ show i) <$> [1..])
        args' = args ++ (TermVariable <$> variables)

        lam body v = TermFunction $ FunctionLambda $ Lambda v body

        app mtyp lhs args = case args of
          [] -> lhs
          (a:rest) -> app mtyp' (TermApplication $ Application lhs' a) rest
            where
              lhs' = annotationClassSetTermType (graphAnnotations g) mtyp lhs
              mtyp' = case mtyp of
                Just (TypeFunction (FunctionType _ cod)) -> Just cod
                Nothing -> Nothing

foldOverTerm :: TraversalOrder -> (x -> Term a -> x) -> x -> Term a -> x
foldOverTerm order fld b0 term = case order of
    TraversalOrderPre -> L.foldl (foldOverTerm order fld) (fld b0 term) children
    TraversalOrderPost -> fld (L.foldl (foldOverTerm order fld) b0 children) term
  where
    children = subterms term

foldOverType :: TraversalOrder -> (x -> Type a -> x) -> x -> Type a -> x
foldOverType order fld b0 typ = case order of
    TraversalOrderPre -> L.foldl (foldOverType order fld) (fld b0 typ) children
    TraversalOrderPost -> fld (L.foldl (foldOverType order fld) b0 children) typ
  where
    children = subtypes typ

freeVariablesInScheme :: Show a => TypeScheme a -> S.Set Name
freeVariablesInScheme (TypeScheme vars t) = S.difference (freeVariablesInType t) (S.fromList vars)

freeVariablesInTerm :: Term a -> S.Set Name
freeVariablesInTerm term = case term of
  TermFunction (FunctionLambda (Lambda var body)) -> S.delete var $ freeVariablesInTerm body
  TermVariable v -> S.fromList [v]
  _ -> L.foldl (\s t -> S.union s $ freeVariablesInTerm t) S.empty $ subterms term

freeVariablesInType :: Type a -> S.Set Name
freeVariablesInType typ = case typ of
    TypeLambda (LambdaType v body) -> S.delete v $ freeVariablesInType body
    TypeVariable v -> S.fromList [v]
    _ -> L.foldl (\s t -> S.union s $ freeVariablesInType t) S.empty $ subtypes typ

isFreeIn :: Name -> Term a -> Bool
isFreeIn v term = not $ S.member v $ freeVariablesInTerm term

-- | Recursively remove term annotations, including within subterms
removeTermAnnotations :: Ord a => Term a -> Term a
removeTermAnnotations = rewriteTerm remove id
  where
    remove recurse term = case term of
      TermAnnotated (Annotated term' _) -> remove recurse term'
      _ -> recurse term

-- | Recursively remove type annotations, including within subtypes
removeTypeAnnotations :: Ord a => Type a -> Type a
removeTypeAnnotations = rewriteType remove id
  where
    remove recurse typ = case recurse typ of
      TypeAnnotated (Annotated typ' _) -> remove recurse typ'
      _ -> recurse typ

replaceFreeName :: Ord a => Name -> Type a -> Type a -> Type a
replaceFreeName v rep = rewriteType mapExpr id
  where
    mapExpr recurse t = case t of
      TypeLambda (LambdaType v' body) -> if v == v'
        then t
        else TypeLambda $ LambdaType v' $ recurse body
      TypeVariable v' -> if v == v' then rep else t
      _ -> recurse t

rewrite :: ((x -> y) -> x -> y) -> ((x -> y) -> x -> y) -> x -> y
rewrite fsub f = recurse
  where
    recurse = f (fsub recurse)

rewriteTerm :: Ord b => ((Term a -> Term b) -> Term a -> Term b) -> (a -> b) -> Term a -> Term b
rewriteTerm f mf = rewrite fsub f
  where
    fsub recurse term = case term of
        TermAnnotated (Annotated ex ann) -> TermAnnotated $ Annotated (recurse ex) (mf ann)
        TermApplication (Application lhs rhs) -> TermApplication $ Application (recurse lhs) (recurse rhs)
        TermFunction fun -> TermFunction $ case fun of
          FunctionElimination e -> FunctionElimination $ case e of
            EliminationWrap name -> EliminationWrap name
            EliminationOptional (OptionalCases nothing just) -> EliminationOptional
              (OptionalCases (recurse nothing) (recurse just))
            EliminationRecord p -> EliminationRecord p
            EliminationUnion (CaseStatement n def cases) -> EliminationUnion $ CaseStatement n (recurse <$> def) (forField <$> cases)
          FunctionLambda (Lambda v body) -> FunctionLambda $ Lambda v $ recurse body
          FunctionPrimitive name -> FunctionPrimitive name
        TermLet (Let bindings env) -> TermLet $ Let (M.fromList (mapBinding <$> M.toList bindings)) (recurse env)
          where
            mapBinding (k, t) = (k, recurse t)
        TermList els -> TermList $ recurse <$> els
        TermLiteral v -> TermLiteral v
        TermMap m -> TermMap $ M.fromList $ (\(k, v) -> (recurse k, recurse v)) <$> M.toList m
        TermWrap (Nominal name t) -> TermWrap (Nominal name $ recurse t)
        TermOptional m -> TermOptional $ recurse <$> m
        TermProduct tuple -> TermProduct (recurse <$> tuple)
        TermRecord (Record n fields) -> TermRecord $ Record n $ forField <$> fields
        TermSet s -> TermSet $ S.fromList $ recurse <$> S.toList s
        TermSum (Sum i s trm) -> TermSum $ Sum i s $ recurse trm
        TermUnion (Injection n field) -> TermUnion $ Injection n $ forField field
        TermVariable v -> TermVariable v
      where
        forField f = f {fieldTerm = recurse (fieldTerm f)}

rewriteTermM :: Ord b =>
  ((Term a -> Flow s (Term b)) -> Term a -> (Flow s (Term b))) ->
  (a -> Flow s b) ->
  Term a ->
  Flow s (Term b)
rewriteTermM f mf = rewrite fsub f
  where
    fsub recurse term = case term of
        TermAnnotated (Annotated ex ma) -> TermAnnotated <$> (Annotated <$> recurse ex <*> mf ma)
        TermApplication (Application lhs rhs) -> TermApplication <$> (Application <$> recurse lhs <*> recurse rhs)
        TermFunction fun -> TermFunction <$> case fun of
          FunctionElimination e -> FunctionElimination <$> case e of
            EliminationWrap name -> pure $ EliminationWrap name
            EliminationOptional (OptionalCases nothing just) -> EliminationOptional <$>
              (OptionalCases <$> recurse nothing <*> recurse just)
            EliminationRecord p -> pure $ EliminationRecord p
            EliminationUnion (CaseStatement n def cases) -> do
              rdef <- case def of
                Nothing -> pure Nothing
                Just t -> Just <$> recurse t
              EliminationUnion <$> (CaseStatement n rdef <$> (CM.mapM forField cases))
          FunctionLambda (Lambda v body) -> FunctionLambda <$> (Lambda v <$> recurse body)
          FunctionPrimitive name -> pure $ FunctionPrimitive name
        TermLet (Let bindings env) -> TermLet <$> (Let <$> (M.fromList <$> (CM.mapM mapBinding $ M.toList bindings)) <*> recurse env)
          where
            mapBinding (k, t) = do
              t' <- recurse t
              return (k, t')
        TermList els -> TermList <$> (CM.mapM recurse els)
        TermLiteral v -> pure $ TermLiteral v
        TermMap m -> TermMap <$> (M.fromList <$> CM.mapM forPair (M.toList m))
          where
            forPair (k, v) = do
              km <- recurse k
              vm <- recurse v
              return (km, vm)
        TermOptional m -> TermOptional <$> (CM.mapM recurse m)
        TermProduct tuple -> TermProduct <$> (CM.mapM recurse tuple)
        TermRecord (Record n fields) -> TermRecord <$> (Record n <$> (CM.mapM forField fields))
        TermSet s -> TermSet <$> (S.fromList <$> (CM.mapM recurse $ S.toList s))
        TermSum (Sum i s trm) -> TermSum <$> (Sum i s <$> recurse trm)
        TermUnion (Injection n field) -> TermUnion <$> (Injection n <$> forField field)
        TermVariable v -> pure $ TermVariable v
        TermWrap (Nominal name t) -> TermWrap <$> (Nominal name <$> recurse t)
      where
        forField f = do
          t <- recurse (fieldTerm f)
          return f {fieldTerm = t}

rewriteTermMeta :: Ord b => (a -> b) -> Term a -> Term b
rewriteTermMeta = rewriteTerm mapExpr
  where
    mapExpr recurse term = recurse term

rewriteTermMetaM :: Ord b => (a -> Flow s b) -> Term a -> Flow s (Term b)
rewriteTermMetaM = rewriteTermM mapExpr
  where
    mapExpr recurse term = recurse term

rewriteType :: ((Type a -> Type b) -> Type a -> Type b) -> (a -> b) -> Type a -> Type b
rewriteType f mf = rewrite fsub f
  where
    fsub recurse typ = case typ of
        TypeAnnotated (Annotated t ann) -> TypeAnnotated $ Annotated (recurse t) (mf ann)
        TypeApplication (ApplicationType lhs rhs) -> TypeApplication $ ApplicationType (recurse lhs) (recurse rhs)
        TypeFunction (FunctionType dom cod) -> TypeFunction (FunctionType (recurse dom) (recurse cod))
        TypeLambda (LambdaType v b) -> TypeLambda (LambdaType v $ recurse b)
        TypeList t -> TypeList $ recurse t
        TypeLiteral lt -> TypeLiteral lt
        TypeMap (MapType kt vt) -> TypeMap (MapType (recurse kt) (recurse vt))
        TypeOptional t -> TypeOptional $ recurse t
        TypeProduct types -> TypeProduct (recurse <$> types)
        TypeRecord (RowType name extends fields) -> TypeRecord $ RowType name extends (forField <$> fields)
        TypeSet t -> TypeSet $ recurse t
        TypeSum types -> TypeSum (recurse <$> types)
        TypeUnion (RowType name extends fields) -> TypeUnion $ RowType name extends (forField <$> fields)
        TypeVariable v -> TypeVariable v
        TypeWrap (Nominal name t) -> TypeWrap $ Nominal name $ recurse t
      where
        forField f = f {fieldTypeType = recurse (fieldTypeType f)}

rewriteTypeM :: Ord b =>
  ((Type a -> Flow s (Type b)) -> Type a -> (Flow s (Type b))) ->
  (a -> Flow s b) ->
  Type a ->
  Flow s (Type b)
rewriteTypeM f mf = rewrite fsub f
  where
    fsub recurse typ = case typ of
        TypeAnnotated (Annotated t ann) -> TypeAnnotated <$> (Annotated <$> recurse t <*> mf ann)
        TypeApplication (ApplicationType lhs rhs) -> TypeApplication <$> (ApplicationType <$> recurse lhs <*> recurse rhs)
        TypeFunction (FunctionType dom cod) -> TypeFunction <$> (FunctionType <$> recurse dom <*> recurse cod)
        TypeLambda (LambdaType v b) -> TypeLambda <$> (LambdaType <$> pure v <*> recurse b)
        TypeList t -> TypeList <$> recurse t
        TypeLiteral lt -> pure $ TypeLiteral lt
        TypeMap (MapType kt vt) -> TypeMap <$> (MapType <$> recurse kt <*> recurse vt)
        TypeOptional t -> TypeOptional <$> recurse t
        TypeProduct types -> TypeProduct <$> CM.mapM recurse types
        TypeRecord (RowType name extends fields) ->
          TypeRecord <$> (RowType <$> pure name <*> pure extends <*> CM.mapM forField fields)
        TypeSet t -> TypeSet <$> recurse t
        TypeSum types -> TypeSum <$> CM.mapM recurse types
        TypeUnion (RowType name extends fields) ->
          TypeUnion <$> (RowType <$> pure name <*> pure extends <*> CM.mapM forField fields)
        TypeVariable v -> pure $ TypeVariable v
        TypeWrap (Nominal name t) -> TypeWrap <$> (Nominal <$> pure name <*> recurse t)
      where
        forField f = do
          t <- recurse $ fieldTypeType f
          return f {fieldTypeType = t}

rewriteTypeMeta :: (a -> b) -> Type a -> Type b
rewriteTypeMeta = rewriteType mapExpr
  where
    mapExpr recurse term = recurse term

simplifyTerm :: Ord a => Term a -> Term a
simplifyTerm = rewriteTerm simplify id
  where
    simplify recurse term = recurse $ case stripTerm term of
      TermApplication (Application lhs rhs) -> case stripTerm lhs of
        TermFunction (FunctionLambda (Lambda var body)) ->
          if S.member var (freeVariablesInTerm body)
            then case stripTerm rhs of
              TermVariable v -> simplifyTerm $ substituteVariable var v body
              _ -> term
            else simplifyTerm body
        _ -> term
      _ -> term

substituteVariable :: Ord a => Name -> Name -> Term a -> Term a
substituteVariable from to = rewriteTerm replace id
  where
    replace recurse term = case term of
      TermVariable x -> recurse $ (TermVariable $ if x == from then to else x)
      TermFunction (FunctionLambda (Lambda var _)) -> if var == from
        then term
        else recurse term
      _ -> recurse term

subterms :: Term a -> [Term a]
subterms term = case term of
  TermAnnotated (Annotated t _) -> [t]
  TermApplication (Application lhs rhs) -> [lhs, rhs]
  TermFunction f -> case f of
    FunctionElimination e -> case e of
      EliminationOptional (OptionalCases nothing just) -> [nothing, just]
      EliminationUnion (CaseStatement _ def cases) -> (fieldTerm <$> cases) ++ (Y.catMaybes [def])
      _ -> []
    FunctionLambda (Lambda _ body) -> [body]
    _ -> []
  TermLet (Let bindings env) -> (snd <$> M.toList bindings) ++ [env]
  TermList els -> els
  TermMap m -> L.concat ((\(k, v) -> [k, v]) <$> M.toList m)
  TermWrap (Nominal _ t) -> [t]
  TermOptional m -> Y.maybeToList m
  TermProduct tuple -> tuple
  TermRecord (Record n fields) -> fieldTerm <$> fields
  TermSet s -> S.toList s
  TermSum (Sum _ _ trm) -> [trm]
  TermUnion (Injection _ field) -> [fieldTerm field]
  _ -> []

subtypes :: Type a -> [Type a]
subtypes typ = case typ of
  TypeAnnotated (Annotated t _) -> [t]
  TypeApplication (ApplicationType lhs rhs) -> [lhs, rhs]
  TypeFunction (FunctionType dom cod) -> [dom, cod]
  TypeLambda (LambdaType v body) -> [body]
  TypeList lt -> [lt]
  TypeLiteral _ -> []
  TypeMap (MapType kt vt) -> [kt, vt]
  TypeOptional ot -> [ot]
  TypeProduct types -> types
  TypeRecord rt -> fieldTypeType <$> rowTypeFields rt
  TypeSet st -> [st]
  TypeSum types -> types
  TypeUnion rt -> fieldTypeType <$> rowTypeFields rt
  TypeVariable _ -> []
  TypeWrap (Nominal _ t) -> [t]

-- Note: does not distinguish between bound and free variables; use freeVariablesInTerm for that
termDependencyNames :: Bool -> Bool -> Bool -> Term a -> S.Set Name
termDependencyNames withVars withPrims withNoms = foldOverTerm TraversalOrderPre addNames S.empty
  where
    addNames names term = case term of
        TermFunction f -> case f of
          FunctionPrimitive name -> prim name
          FunctionElimination e -> case e of
            EliminationRecord (Projection name _) -> nominal name
            EliminationUnion (CaseStatement name _ _) -> nominal name
            EliminationWrap name -> nominal name
            _ -> names
          _ -> names
        TermRecord (Record name _) -> nominal name
        TermUnion (Injection name _) -> nominal name
        TermVariable name -> var name
        TermWrap (Nominal name _) -> nominal name
        _ -> names
      where
        nominal name = if withNoms then S.insert name names else names
        prim name = if withPrims then S.insert name names else names
        var name = if withVars then S.insert name names else names

topologicalSortElements :: [Element a] -> Either [[Name]] [Name]
topologicalSortElements els = topologicalSort $ adjlist <$> els
  where
    adjlist e = (elementName e, S.toList $ termDependencyNames False True True $ elementData e)

typeDependencyNames :: Type a -> S.Set Name
typeDependencyNames = freeVariablesInType
