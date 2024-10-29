-- | Functions for type and term rewriting

module Hydra.Rewriting where

import Hydra.Basics
import Hydra.Strip
import Hydra.Coders
import Hydra.Compute
import Hydra.Core
import Hydra.CoreEncoding
import Hydra.Extras
import Hydra.Graph
import Hydra.Module
import Hydra.Lexical
import Hydra.Mantle
import Hydra.Tools.Sorting
import Hydra.Tier1
import Hydra.Tier2
import Hydra.Tools.Debug

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


-- beneathTermAnnotations :: (Term -> Term) -> Term -> Term
-- beneathTermAnnotations f term = case term of
--   TermAnnotated (AnnotatedTerm term1 ann) ->
--     TermAnnotated (AnnotatedTerm (beneathTermAnnotations f term1) ann)
--   TermTyped (TypedTerm term1 typ) ->
--     TermTyped $ TypedTerm (beneathTermAnnotations f term1) typ
--   _ -> f term
--
-- beneathTermAnnotationsM :: (Term -> Flow s Term) -> Term -> Flow s Term
-- beneathTermAnnotationsM f term = case term of
--   TermAnnotated (AnnotatedTerm term1 ann) ->
--     TermAnnotated <$> (AnnotatedTerm <$> beneathTermAnnotationsM f term1 <*> pure ann)
--   TermTyped (TypedTerm term1 typ) ->
--     TermTyped <$> (TypedTerm <$> beneathTermAnnotationsM f term1 <*> pure typ)
--   _ -> f term

-- | Apply a transformation to the first type beneath a chain of annotations
beneathTypeAnnotations :: (Type -> Type) -> Type -> Type
beneathTypeAnnotations f t = case t of
  TypeAnnotated (AnnotatedType t1 ann) -> TypeAnnotated (AnnotatedType (beneathTypeAnnotations f t1) ann)
  _ -> f t

elementsWithDependencies :: [Element] -> Flow Graph [Element]
elementsWithDependencies original = CM.mapM requireElement allDepNames
  where
    depNames = S.toList . termDependencyNames True False False . elementData
    allDepNames = L.nub $ (elementName <$> original) ++ (L.concat $ depNames <$> original)

-- | Recursively transform arbitrary terms like 'add 42' into terms like '\x.add 42 x',
--   whose arity (in the absence of application terms) is equal to the depth of nested lambdas.
--   This is useful for targets like Java with weaker support for currying.
expandTypedLambdas :: Term -> Term
expandTypedLambdas = rewriteTerm rewrite
  where
    rewrite recurse term = case getFunType term of
        Nothing -> recurse term
        Just (doms, cod) -> expand doms cod term
      where
        toNaryFunType typ = case stripType typ of
          TypeFunction (FunctionType dom0 cod0) -> (dom0 : doms, cod1)
            where
              (doms, cod1) = toNaryFunType cod0
          d -> ([], d)
        getFunType term = toNaryFunType <$> getTermType term

        expand doms cod term = case term of
          TermAnnotated (AnnotatedTerm term1 ann) -> TermAnnotated $ AnnotatedTerm (expand doms cod term1) ann
          TermApplication (Application lhs rhs) -> case getTermType rhs of
            Nothing -> recurse term
            Just typ -> TermApplication $ Application (expand (typ:doms) cod lhs) $ expandTypedLambdas rhs
          TermFunction f -> case f of
            FunctionLambda (Lambda var d body) -> TermFunction $ FunctionLambda $ Lambda var d $
              expand (L.tail doms) cod body
            _ -> pad 1 doms cod term
          TermLet (Let bindings env) -> TermLet $ Let (expandBinding <$> bindings) $ expand doms cod env
            where
              expandBinding (LetBinding name term1 typ) = LetBinding name (expandTypedLambdas term1) typ
          TermTyped (TypedTerm term1 typ) -> TermTyped $ TypedTerm (expand doms cod term1) typ
          _ -> recurse term

        pad i doms cod term = if L.null doms
            then term
            else TermFunction $ FunctionLambda $ Lambda var (Just dom) $
              pad (i+1) (L.tail doms) cod $
              -- TODO: omit this type annotation if practical; a type annotation on application terms
              --       shouldn't really be necessary.
              typed (toFunctionType (L.tail doms) cod) $
              TermApplication $ Application (typed (toFunctionType doms cod) term) $ TermVariable var
          where
            dom = L.head doms
            typed typ term = TermTyped $ TypedTerm term typ
            toFunctionType doms cod = L.foldl (\c d -> TypeFunction $ FunctionType d c) cod doms
            var = Name $ "v" ++ show i

flattenLetTerms :: Term -> Term
flattenLetTerms = rewriteTerm flatten
  where
    flatten recurse term = case recurse term of
      TermLet (Let bindings body) -> TermLet $ Let newBindings body
        where
          newBindings = L.concat (forResult . rewriteBinding <$> bindings)
            where
              forResult (h, rest) = (h:rest)
          rewriteBinding (LetBinding k0 v0 t) = case v0 of
            TermAnnotated (AnnotatedTerm v1 ann) -> ((LetBinding k0 (TermAnnotated $ AnnotatedTerm v2 ann) t), deps)
              where
                ((LetBinding _ v2 _), deps) = rewriteBinding (LetBinding k0 v1 t)
            TermLet (Let bindings1 body1) -> ((LetBinding k0 newBody t), newBinding <$> bindings1)
              where
                newBody = replaceVars body1
                newBinding (LetBinding kn vn t) = LetBinding (qualify kn) (replaceVars vn) t
                qualify n = Name $ prefix ++ unName n
                replaceVars = substituteVariables subst
                subst = M.fromList (toSubstPair <$> bindings1)
                  where
                    toSubstPair (LetBinding n _ _) = (n, qualify n)
                prefix = unName k0 ++ "_"
            v1 -> ((LetBinding k0 v1 t), [])
      term0 -> term0

freeVariablesInScheme :: TypeScheme -> S.Set Name
freeVariablesInScheme (TypeScheme vars t) = S.difference (freeVariablesInType t) (S.fromList vars)

-- | Inline all type variables in a type using the provided schema.
--   Note: this function is only appropriate for nonrecursive type definitions.
inlineType :: M.Map Name Type -> Type -> Flow s Type
inlineType schema = rewriteTypeM f
  where
    f recurse typ = do
      tr <- recurse typ
      case tr of
        TypeVariable v -> case M.lookup v schema of
          Just t -> inlineType schema t
          Nothing -> fail $ "No such type in schema: " ++ unName v
        t -> return t

isFreeIn :: Name -> Term -> Bool
isFreeIn v term = not $ S.member v $ freeVariablesInTerm term

-- | Recursively remove term annotations, including within subterms
removeTermAnnotations :: Term -> Term
removeTermAnnotations = rewriteTerm remove
  where
    remove recurse term = case recurse term of
      TermAnnotated (AnnotatedTerm term1 _) -> term1
      TermTyped (TypedTerm term1 _) -> term1
      t -> t

-- | Recursively remove type annotations, including within subtypes
removeTypeAnnotations :: Type -> Type
removeTypeAnnotations = rewriteType remove
  where
    remove recurse typ = case recurse typ of
      TypeAnnotated (AnnotatedType typ' _) -> typ'
      t -> t

replaceFreeName :: Name -> Type -> Type -> Type
replaceFreeName v rep = rewriteType mapExpr
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

rewriteTerm :: ((Term -> Term) -> Term -> Term) -> Term -> Term
rewriteTerm f = rewrite fsub f
  where
    fsub recurse term = case term of
        TermAnnotated (AnnotatedTerm ex ann) -> TermAnnotated $ AnnotatedTerm (recurse ex) ann
        TermApplication (Application lhs rhs) -> TermApplication $ Application (recurse lhs) (recurse rhs)
        TermFunction fun -> TermFunction $ case fun of
          FunctionElimination e -> FunctionElimination $ case e of
            EliminationList fld -> EliminationList $ recurse fld
            EliminationOptional (OptionalCases nothing just) -> EliminationOptional
              (OptionalCases (recurse nothing) (recurse just))
            EliminationProduct tp -> EliminationProduct tp
            EliminationRecord p -> EliminationRecord p
            EliminationUnion (CaseStatement n def cases) -> EliminationUnion $ CaseStatement n (recurse <$> def) (forField <$> cases)
            EliminationWrap name -> EliminationWrap name
          FunctionLambda (Lambda v d body) -> FunctionLambda $ Lambda v d $ recurse body
          FunctionPrimitive name -> FunctionPrimitive name
        TermLet (Let bindings env) -> TermLet $ Let (mapBinding <$> bindings) (recurse env)
          where
            mapBinding (LetBinding k v t) = LetBinding k (recurse v) t
        TermList els -> TermList $ recurse <$> els
        TermLiteral v -> TermLiteral v
        TermMap m -> TermMap $ M.fromList $ (\(k, v) -> (recurse k, recurse v)) <$> M.toList m
        TermWrap (WrappedTerm name t) -> TermWrap (WrappedTerm name $ recurse t)
        TermOptional m -> TermOptional $ recurse <$> m
        TermProduct tuple -> TermProduct (recurse <$> tuple)
        TermRecord (Record n fields) -> TermRecord $ Record n $ forField <$> fields
        TermSet s -> TermSet $ S.fromList $ recurse <$> S.toList s
        TermSum (Sum i s trm) -> TermSum $ Sum i s $ recurse trm
        TermTypeAbstraction (TypeAbstraction v t) -> TermTypeAbstraction $ TypeAbstraction v $ recurse t
        TermTypeApplication (TypedTerm term1 type2) -> TermTypeApplication $ TypedTerm (recurse term1) type2
        TermTyped (TypedTerm term1 type2) -> TermTyped $ TypedTerm (recurse term1) type2
        TermUnion (Injection n field) -> TermUnion $ Injection n $ forField field
        TermVariable v -> TermVariable v
      where
        forField f = f {fieldTerm = recurse (fieldTerm f)}

rewriteTermM ::
  ((Term -> Flow s Term) -> Term -> (Flow s Term)) -> Term -> Flow s Term
rewriteTermM f = rewrite fsub f
  where
    fsub recurse term = case term of
        TermAnnotated (AnnotatedTerm ex ma) -> TermAnnotated <$> (AnnotatedTerm <$> recurse ex <*> pure ma)
        TermApplication (Application lhs rhs) -> TermApplication <$> (Application <$> recurse lhs <*> recurse rhs)
        TermFunction fun -> TermFunction <$> case fun of
          FunctionElimination e -> FunctionElimination <$> case e of
            EliminationList fld -> EliminationList <$> recurse fld
            EliminationOptional (OptionalCases nothing just) -> EliminationOptional <$>
              (OptionalCases <$> recurse nothing <*> recurse just)
            EliminationProduct tp -> pure $ EliminationProduct tp
            EliminationRecord p -> pure $ EliminationRecord p
            EliminationUnion (CaseStatement n def cases) -> do
              rdef <- case def of
                Nothing -> pure Nothing
                Just t -> Just <$> recurse t
              EliminationUnion <$> (CaseStatement n rdef <$> (CM.mapM forField cases))
            EliminationWrap name -> pure $ EliminationWrap name
          FunctionLambda (Lambda v d body) -> FunctionLambda <$> (Lambda v d <$> recurse body)
          FunctionPrimitive name -> pure $ FunctionPrimitive name
        TermLet (Let bindings env) -> TermLet <$> (Let <$> (CM.mapM mapBinding bindings) <*> recurse env)
          where
            mapBinding (LetBinding k v t) = do
              v' <- recurse v
              return $ LetBinding k v' t
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
        TermTyped (TypedTerm term1 type2) -> TermTyped <$> (TypedTerm <$> recurse term1 <*> pure type2)
        TermUnion (Injection n field) -> TermUnion <$> (Injection n <$> forField field)
        TermVariable v -> pure $ TermVariable v
        TermWrap (WrappedTerm name t) -> TermWrap <$> (WrappedTerm name <$> recurse t)
      where
        forField f = do
          t <- recurse (fieldTerm f)
          return f {fieldTerm = t}

rewriteTermMeta :: (M.Map Name Term -> M.Map Name Term) -> Term -> Term
rewriteTermMeta mapping = rewriteTerm rewrite
  where
    rewrite recurse term = case recurse term of
      TermAnnotated (AnnotatedTerm term1 ann) -> TermAnnotated $ AnnotatedTerm term1 $ mapping ann
      t -> t

rewriteTermMetaM :: (M.Map Name Term -> Flow s (M.Map Name Term)) -> Term -> Flow s Term
rewriteTermMetaM mapping = rewriteTermM rewrite
  where
    rewrite recurse term = do
      r <- recurse term
      case r of
        TermAnnotated (AnnotatedTerm term1 ann) -> TermAnnotated <$> (AnnotatedTerm <$> pure term1 <*> mapping ann)
        t -> pure t

rewriteType :: ((Type -> Type) -> Type -> Type) -> Type -> Type
rewriteType f = rewrite fsub f
  where
    fsub recurse typ = case typ of
        TypeAnnotated (AnnotatedType t ann) -> TypeAnnotated $ AnnotatedType (recurse t) ann
        TypeApplication (ApplicationType lhs rhs) -> TypeApplication $ ApplicationType (recurse lhs) (recurse rhs)
        TypeFunction (FunctionType dom cod) -> TypeFunction (FunctionType (recurse dom) (recurse cod))
        TypeLambda (LambdaType v b) -> TypeLambda (LambdaType v $ recurse b)
        TypeList t -> TypeList $ recurse t
        TypeLiteral lt -> TypeLiteral lt
        TypeMap (MapType kt vt) -> TypeMap (MapType (recurse kt) (recurse vt))
        TypeOptional t -> TypeOptional $ recurse t
        TypeProduct types -> TypeProduct (recurse <$> types)
        TypeRecord (RowType name fields) -> TypeRecord $ RowType name (forField <$> fields)
        TypeSet t -> TypeSet $ recurse t
        TypeSum types -> TypeSum (recurse <$> types)
        TypeUnion (RowType name fields) -> TypeUnion $ RowType name (forField <$> fields)
        TypeVariable v -> TypeVariable v
        TypeWrap (WrappedType name t) -> TypeWrap $ WrappedType name $ recurse t
      where
        forField f = f {fieldTypeType = recurse (fieldTypeType f)}

rewriteTypeM ::
  ((Type -> Flow s Type) -> Type -> (Flow s Type)) ->
  Type ->
  Flow s Type
rewriteTypeM f = rewrite fsub f
  where
    fsub recurse typ = case typ of
        TypeAnnotated (AnnotatedType t ann) -> TypeAnnotated <$> (AnnotatedType <$> recurse t <*> pure ann)
        TypeApplication (ApplicationType lhs rhs) -> TypeApplication <$> (ApplicationType <$> recurse lhs <*> recurse rhs)
        TypeFunction (FunctionType dom cod) -> TypeFunction <$> (FunctionType <$> recurse dom <*> recurse cod)
        TypeLambda (LambdaType v b) -> TypeLambda <$> (LambdaType <$> pure v <*> recurse b)
        TypeList t -> TypeList <$> recurse t
        TypeLiteral lt -> pure $ TypeLiteral lt
        TypeMap (MapType kt vt) -> TypeMap <$> (MapType <$> recurse kt <*> recurse vt)
        TypeOptional t -> TypeOptional <$> recurse t
        TypeProduct types -> TypeProduct <$> CM.mapM recurse types
        TypeRecord (RowType name fields) ->
          TypeRecord <$> (RowType <$> pure name <*> CM.mapM forField fields)
        TypeSet t -> TypeSet <$> recurse t
        TypeSum types -> TypeSum <$> CM.mapM recurse types
        TypeUnion (RowType name fields) ->
          TypeUnion <$> (RowType <$> pure name <*> CM.mapM forField fields)
        TypeVariable v -> pure $ TypeVariable v
        TypeWrap (WrappedType name t) -> TypeWrap <$> (WrappedType <$> pure name <*> recurse t)
      where
        forField f = do
          t <- recurse $ fieldTypeType f
          return f {fieldTypeType = t}

rewriteTypeMeta :: (M.Map Name Term -> M.Map Name Term) -> Type -> Type
rewriteTypeMeta mapping = rewriteType rewrite
  where
    rewrite recurse typ = case recurse typ of
      TypeAnnotated (AnnotatedType typ1 ann) -> TypeAnnotated $ AnnotatedType typ1 $ mapping ann
      t -> t

simplifyTerm :: Term -> Term
simplifyTerm = rewriteTerm simplify
  where
    simplify recurse term = recurse $ case fullyStripTerm term of
      TermApplication (Application lhs rhs) -> case fullyStripTerm lhs of
        TermFunction (FunctionLambda (Lambda var d body)) ->
          if S.member var (freeVariablesInTerm body)
            then case fullyStripTerm rhs of
              TermVariable v -> simplifyTerm $ substituteVariable var v body
              _ -> term
            else simplifyTerm body
        _ -> term
      _ -> term

stripTermRecursive :: Term -> Term
stripTermRecursive = rewriteTerm strip
  where
    strip recurse term = case recurse term of
      TermAnnotated (AnnotatedTerm t _) -> t
      TermTyped (TypedTerm t _) -> t
      t -> t

substituteVariable :: Name -> Name -> Term -> Term
substituteVariable from to = rewriteTerm replace
  where
    replace recurse term = case term of
      TermVariable x -> (TermVariable $ if x == from then to else x)
      TermFunction (FunctionLambda (Lambda var _ _)) -> if var == from
        then term
        else recurse term
      _ -> recurse term

substituteVariables :: M.Map Name Name -> Term -> Term
substituteVariables subst = rewriteTerm replace
  where
    replace recurse term = case term of
      TermVariable n -> TermVariable $ Y.fromMaybe n $ M.lookup n subst
      TermFunction (FunctionLambda (Lambda v _ _ )) -> case M.lookup v subst of
        Nothing -> recurse term
        Just _ -> term
      _ -> recurse term

-- Note: does not distinguish between bound and free variables; use freeVariablesInTerm for that
termDependencyNames :: Bool -> Bool -> Bool -> Term -> S.Set Name
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
        TermWrap (WrappedTerm name _) -> nominal name
        _ -> names
      where
        nominal name = if withNoms then S.insert name names else names
        prim name = if withPrims then S.insert name names else names
        var name = if withVars then S.insert name names else names

-- Topological sort of connected components, in terms of dependencies between varable/term binding pairs
topologicalSortBindings :: M.Map Name Term -> [[(Name, Term)]]
topologicalSortBindings bindingMap = fmap (fmap toPair) (topologicalSortComponents (depsOf <$> bindings))
  where
    bindings = M.toList bindingMap
    keys = S.fromList (fst <$> bindings)
    depsOf (name, term) = (name, if hasTypeAnnotation term
      then []
      else S.toList (S.intersection keys $ freeVariablesInTerm term))
    toPair name = (name, Y.fromMaybe (TermLiteral $ LiteralString "Impossible!") $ M.lookup name bindingMap)
    hasTypeAnnotation term = case term of
      TermAnnotated (AnnotatedTerm term1 _) -> hasTypeAnnotation term1
      TermTyped _ -> True
      _ -> False

topologicalSortElements :: [Element] -> Either [[Name]] [Name]
topologicalSortElements els = topologicalSort $ adjlist <$> els
  where
    adjlist e = (elementName e, S.toList $ termDependencyNames False True True $ elementData e)

typeDependencyNames :: Type -> S.Set Name
typeDependencyNames = freeVariablesInType
