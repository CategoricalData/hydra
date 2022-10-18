module Hydra.Rewriting where

import Hydra.Core
import Hydra.Monads
import Hydra.Module
import Hydra.Lexical
import Hydra.Compute
import Hydra.CoreDecoding
import Hydra.Sorting

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


-- | Turn arbitrary terms like 'compareTo 42' into terms like '\x.compareTo 42 x',
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
          FunctionCompareTo _ -> pad args 1 <$> recurse term
          FunctionElimination _ -> pad args 1 <$> recurse term
          FunctionLambda _ -> passThrough
          FunctionPrimitive name -> do
            prim <- requirePrimitiveFunction name
            return $ pad args (primitiveFunctionArity prim) term
        _ -> passThrough
      where
        passThrough = pad args 0 <$> recurse term

    pad args arity term = L.foldl lam (L.foldl app term args') $ L.reverse variables
      where
        variables = L.take (max 0 (arity - L.length args)) ((\i -> Variable $ "v" ++ show i) <$> [1..])
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

freeVariablesInScheme :: Show m => TypeScheme m -> S.Set VariableType
freeVariablesInScheme (TypeScheme vars t) = S.difference (freeVariablesInType t) (S.fromList vars)

freeVariablesInTerm :: Term m -> S.Set Variable
freeVariablesInTerm term = case term of
  TermAnnotated (Annotated term1 _) -> freeVariablesInTerm term1
  TermFunction (FunctionLambda (Lambda var body)) -> S.delete var $ freeVariablesInTerm body
  TermVariable v -> S.fromList [v]
  _ -> L.foldl (\s t -> S.union s $ freeVariablesInTerm t) S.empty $ subterms term

freeVariablesInType :: Type m -> S.Set VariableType
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
          FunctionCompareTo other -> FunctionCompareTo $ recurse other
          FunctionElimination e -> FunctionElimination $ case e of
            EliminationElement -> EliminationElement
            EliminationNominal name -> EliminationNominal name
            EliminationOptional (OptionalCases nothing just) -> EliminationOptional
              (OptionalCases (recurse nothing) (recurse just))
            EliminationRecord p -> EliminationRecord p
            EliminationUnion (CaseStatement n cases) -> EliminationUnion $ CaseStatement n (forField <$> cases)
          FunctionLambda (Lambda v body) -> FunctionLambda $ Lambda v $ recurse body
          FunctionPrimitive name -> FunctionPrimitive name
        TermLet (Let v t1 t2) -> TermLet $ Let v (recurse t1) (recurse t2)
        TermList els -> TermList $ recurse <$> els
        TermLiteral v -> TermLiteral v
        TermMap m -> TermMap $ M.fromList $ (\(k, v) -> (recurse k, recurse v)) <$> M.toList m
        TermNominal (Named name t) -> TermNominal (Named name $ recurse t)
        TermOptional m -> TermOptional $ recurse <$> m
        TermProduct tuple -> TermProduct (recurse <$> tuple)
        TermRecord (Record n fields) -> TermRecord $ Record n $ forField <$> fields
        TermSet s -> TermSet $ S.fromList $ recurse <$> S.toList s
        TermSum (Sum i s trm) -> TermSum $ Sum i s $ recurse trm
        TermUnion (Union n field) -> TermUnion $ Union n $ forField field
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
          FunctionCompareTo other -> FunctionCompareTo <$> recurse other
          FunctionElimination e -> FunctionElimination <$> case e of
            EliminationElement -> pure EliminationElement
            EliminationNominal name -> pure $ EliminationNominal name
            EliminationOptional (OptionalCases nothing just) -> EliminationOptional <$>
              (OptionalCases <$> recurse nothing <*> recurse just)
            EliminationRecord p -> pure $ EliminationRecord p
            EliminationUnion (CaseStatement n cases) -> EliminationUnion <$> (CaseStatement n <$> (CM.mapM forField cases))
          FunctionLambda (Lambda v body) -> FunctionLambda <$> (Lambda v <$> recurse body)
          FunctionPrimitive name -> pure $ FunctionPrimitive name
        TermLet (Let v t1 t2) -> TermLet <$> (Let v <$> recurse t1 <*> recurse t2)
        TermList els -> TermList <$> (CM.mapM recurse els)
        TermLiteral v -> pure $ TermLiteral v
        TermMap m -> TermMap <$> (M.fromList <$> CM.mapM forPair (M.toList m))
          where
            forPair (k, v) = do
              km <- recurse k
              vm <- recurse v
              return (km, vm)
        TermNominal (Named name t) -> TermNominal <$> (Named name <$> recurse t)
        TermOptional m -> TermOptional <$> (CM.mapM recurse m)
        TermProduct tuple -> TermProduct <$> (CM.mapM recurse tuple)
        TermRecord (Record n fields) -> TermRecord <$> (Record n <$> (CM.mapM forField fields))
        TermSet s -> TermSet <$> (S.fromList <$> (CM.mapM recurse $ S.toList s))
        TermSum (Sum i s trm) -> TermSum <$> (Sum i s <$> recurse trm)
        TermUnion (Union n field) -> TermUnion <$> (Union n <$> forField field)
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
        TypeNominal name -> TypeNominal name
        TypeOptional t -> TypeOptional $ recurse t
        TypeProduct types -> TypeProduct (recurse <$> types)
        TypeRecord (RowType name fields) -> TypeRecord $ RowType name (forfield <$> fields)
        TypeSet t -> TypeSet $ recurse t
        TypeSum types -> TypeSum (recurse <$> types)
        TypeUnion (RowType name fields) -> TypeUnion $ RowType name (forfield <$> fields)
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
  TermProduct tuple -> tuple
  TermRecord (Record n fields) -> fieldTerm <$> fields
  TermSet s -> S.toList s
  TermSum (Sum _ _ trm) -> [trm]
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
      withTrace ("type dependencies of " ++ unName name) $ do
        el <- requireElement name
        decodeType (elementData el)

typeDependencyNames :: Type m -> S.Set Name
typeDependencyNames = foldOverType TraversalOrderPre addNames S.empty
  where
    addNames names typ = case typ of
      TypeNominal name -> S.insert name names
      _ -> names
