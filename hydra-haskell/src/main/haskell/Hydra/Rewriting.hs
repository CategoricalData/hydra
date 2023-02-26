-- | Functions for type and term rewriting

module Hydra.Rewriting where

import Hydra.Core
import Hydra.Graph
import Hydra.Monads
import Hydra.Module
import Hydra.Lexical
import Hydra.Mantle
import Hydra.Util.Sorting

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


-- | Turn arbitrary terms like 'add 42' into terms like '\x.add 42 x',
--   whose arity (in the absences of application terms) is equal to the depth of nested lambdas.
--   This function leaves application terms intact, simply rewriting their left and right subterms.
expandLambdas :: Ord m => Term m -> GraphFlow m (Term m)
expandLambdas = rewriteTermM (expand []) (pure . id)
  where
    expand args recurse term = case term of
        TermAnnotated (Annotated term' ann) -> TermAnnotated <$> (Annotated <$> expand args recurse term' <*> pure ann)
        TermApplication (Application lhs rhs) -> do
          rhs' <- expandLambdas rhs
          expand (rhs':args) recurse lhs
        TermFunction f -> case f of
          FunctionElimination _ -> pad args 1 <$> recurse term
          FunctionLambda _ -> passThrough
          FunctionPrimitive name -> do
            prim <- requirePrimitive name
            return $ pad args (primitiveFunctionArity prim) term
        _ -> passThrough
      where
        passThrough = pad args 0 <$> recurse term

    pad args arity term = L.foldl lam (L.foldl app term args') $ L.reverse variables
      where
        variables = L.take (max 0 (arity - L.length args)) ((\i -> Name $ "v" ++ show i) <$> [1..])
        args' = args ++ (TermVariable <$> variables)

        app lhs rhs = TermApplication $ Application lhs rhs
        lam body v = TermFunction $ FunctionLambda $ Lambda v body

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

freeVariablesInScheme :: Show m => TypeScheme m -> S.Set Name
freeVariablesInScheme (TypeScheme vars t) = S.difference (freeVariablesInType t) (S.fromList vars)

freeVariablesInTerm :: Term m -> S.Set Name
freeVariablesInTerm term = case term of
  TermAnnotated (Annotated term1 _) -> freeVariablesInTerm term1
  TermFunction (FunctionLambda (Lambda var body)) -> S.delete var $ freeVariablesInTerm body
  TermVariable v -> S.fromList [v]
  _ -> L.foldl (\s t -> S.union s $ freeVariablesInTerm t) S.empty $ subterms term

freeVariablesInType :: Type m -> S.Set Name
freeVariablesInType = foldOverType TraversalOrderPost fld S.empty
  where
    fld vars typ = case typ of
      TypeVariable v -> S.insert v vars
      _ -> vars

moduleDependencyNamespaces :: Bool -> Bool -> Bool -> Module m -> S.Set Namespace
moduleDependencyNamespaces withEls withPrims withNoms mod = S.delete (moduleNamespace mod) names
  where
    names = S.fromList (namespaceOfEager <$> S.toList elNames)
    elNames = L.foldl (\s t -> S.union s $ termDependencyNames withEls withPrims withNoms t) S.empty $
      (elementData <$> moduleElements mod) ++ (elementSchema <$> moduleElements mod)

isFreeIn :: Name -> Term m -> Bool
isFreeIn v term = not $ S.member v $ freeVariablesInTerm term

-- | Recursively remove term annotations, including within subterms
removeTermAnnotations :: Ord m => Term m -> Term m
removeTermAnnotations = rewriteTerm remove id
  where
    remove recurse term = case term of
      TermAnnotated (Annotated term' _) -> remove recurse term'
      _ -> recurse term

-- | Recursively remove type annotations, including within subtypes
removeTypeAnnotations :: Ord m => Type m -> Type m
removeTypeAnnotations = rewriteType remove id
  where
    remove recurse typ = case recurse typ of
      TypeAnnotated (Annotated typ' _) -> remove recurse typ'
      _ -> recurse typ

replaceFreeName :: Ord m => Name -> Type m -> Type m -> Type m
replaceFreeName v rep = rewriteType mapExpr id
  where
    mapExpr recurse t = case t of
      TypeLambda (LambdaType v' body) -> if v == v'
        then t
        else TypeLambda $ LambdaType v' $ recurse body
      TypeVariable v' -> if v == v' then rep else t
      _ -> recurse t

rewrite :: ((a -> b) -> a -> b) -> ((a -> b) -> a -> b) -> a -> b
rewrite fsub f = recurse
  where
    recurse = f (fsub recurse)

rewriteTerm :: Ord b => ((Term a -> Term b) -> Term a -> Term b) -> (a -> b) -> Term a -> Term b
rewriteTerm f mf = rewrite fsub f
  where
    fsub recurse term = case term of
        TermAnnotated (Annotated ex ann) -> TermAnnotated $ Annotated (recurse ex) (mf ann)
        TermApplication (Application lhs rhs) -> TermApplication $ Application (recurse lhs) (recurse rhs)
        TermElement name -> TermElement name
        TermFunction fun -> TermFunction $ case fun of
          FunctionElimination e -> FunctionElimination $ case e of
            EliminationElement -> EliminationElement
            EliminationWrap name -> EliminationWrap name
            EliminationOptional (OptionalCases nothing just) -> EliminationOptional
              (OptionalCases (recurse nothing) (recurse just))
            EliminationRecord p -> EliminationRecord p
            EliminationUnion (CaseStatement n cases) -> EliminationUnion $ CaseStatement n (forField <$> cases)
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

rewriteTermM :: Ord b => ((Term a -> Flow s (Term b)) -> Term a -> (Flow s (Term b))) -> (a -> Flow s b) -> Term a -> Flow s (Term b)
rewriteTermM f mf = rewrite fsub f
  where
    fsub recurse term = case term of
        TermAnnotated (Annotated ex ma) -> TermAnnotated <$> (Annotated <$> recurse ex <*> mf ma)
        TermApplication (Application lhs rhs) -> TermApplication <$> (Application <$> recurse lhs <*> recurse rhs)
        TermElement name -> pure $ TermElement name
        TermFunction fun -> TermFunction <$> case fun of
          FunctionElimination e -> FunctionElimination <$> case e of
            EliminationElement -> pure EliminationElement
            EliminationWrap name -> pure $ EliminationWrap name
            EliminationOptional (OptionalCases nothing just) -> EliminationOptional <$>
              (OptionalCases <$> recurse nothing <*> recurse just)
            EliminationRecord p -> pure $ EliminationRecord p
            EliminationUnion (CaseStatement n cases) -> EliminationUnion <$> (CaseStatement n <$> (CM.mapM forField cases))
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
        TermWrap (Nominal name t) -> TermWrap <$> (Nominal name <$> recurse t)
        TermOptional m -> TermOptional <$> (CM.mapM recurse m)
        TermProduct tuple -> TermProduct <$> (CM.mapM recurse tuple)
        TermRecord (Record n fields) -> TermRecord <$> (Record n <$> (CM.mapM forField fields))
        TermSet s -> TermSet <$> (S.fromList <$> (CM.mapM recurse $ S.toList s))
        TermSum (Sum i s trm) -> TermSum <$> (Sum i s <$> recurse trm)
        TermUnion (Injection n field) -> TermUnion <$> (Injection n <$> forField field)
        TermVariable v -> pure $ TermVariable v
      where
        forField f = do
          t <- recurse (fieldTerm f)
          return f {fieldTerm = t}

rewriteTermMeta :: Ord b => (a -> b) -> Term a -> Term b
rewriteTermMeta = rewriteTerm mapExpr
  where
    mapExpr recurse term = recurse term

rewriteType :: ((Type a -> Type b) -> Type a -> Type b) -> (a -> b) -> Type a -> Type b
rewriteType f mf = rewrite fsub f
  where
    fsub recurse typ = case typ of
        TypeAnnotated (Annotated t ann) -> TypeAnnotated $ Annotated (recurse t) (mf ann)
        TypeApplication (ApplicationType lhs rhs) -> TypeApplication $ ApplicationType (recurse lhs) (recurse rhs)
        TypeElement t -> TypeElement $ recurse t
        TypeFunction (FunctionType dom cod) -> TypeFunction (FunctionType (recurse dom) (recurse cod))
        TypeLambda (LambdaType v b) -> TypeLambda (LambdaType v $ recurse b)
        TypeList t -> TypeList $ recurse t
        TypeLiteral lt -> TypeLiteral lt
        TypeMap (MapType kt vt) -> TypeMap (MapType (recurse kt) (recurse vt))
        TypeWrap name -> TypeWrap name
        TypeOptional t -> TypeOptional $ recurse t
        TypeProduct types -> TypeProduct (recurse <$> types)
        TypeRecord (RowType name extends fields) -> TypeRecord $ RowType name extends (forfield <$> fields)
        TypeSet t -> TypeSet $ recurse t
        TypeSum types -> TypeSum (recurse <$> types)
        TypeUnion (RowType name extends fields) -> TypeUnion $ RowType name extends (forfield <$> fields)
        TypeVariable v -> TypeVariable v
      where
        forfield f = f {fieldTypeType = recurse (fieldTypeType f)}

rewriteTypeMeta :: (a -> b) -> Type a -> Type b
rewriteTypeMeta = rewriteType mapExpr
  where
    mapExpr recurse term = recurse term

simplifyTerm :: Ord m => Term m -> Term m
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

substituteVariable :: Ord m => Name -> Name -> Term m -> Term m
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
    FunctionElimination e -> case e of
      EliminationOptional (OptionalCases nothing just) -> [nothing, just]
      EliminationUnion (CaseStatement _ cases) -> fieldTerm <$> cases
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
  TypeWrap _ -> []
  TypeOptional ot -> [ot]
  TypeProduct types -> types
  TypeRecord rt -> fieldTypeType <$> rowTypeFields rt
  TypeSet st -> [st]
  TypeSum types -> types
  TypeUnion rt -> fieldTypeType <$> rowTypeFields rt
  TypeVariable _ -> []

termDependencyNames :: Bool -> Bool -> Bool -> Term m -> S.Set Name
termDependencyNames withEls withPrims withNoms = foldOverTerm TraversalOrderPre addNames S.empty
  where
    addNames names term = case term of
      TermElement name -> if withEls then S.insert name names else names
      TermFunction (FunctionPrimitive name) -> if withPrims then S.insert name names else names
      TermWrap (Nominal name _) -> if withNoms then S.insert name names else names
      _ -> names

topologicalSortElements :: [Element m] -> Maybe [Name]
topologicalSortElements els = topologicalSort $ adjlist <$> els
  where
    adjlist e = (elementName e, S.toList $ termDependencyNames True True True $ elementData e)
