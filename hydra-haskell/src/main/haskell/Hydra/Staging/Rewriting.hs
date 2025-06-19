-- | Functions for type and term rewriting

module Hydra.Staging.Rewriting (
  module Hydra.Rewriting,
  module Hydra.Staging.Rewriting,
) where

import Hydra.Annotations
import Hydra.Qnames
import Hydra.Strip
import Hydra.Coders
import Hydra.Compute
import Hydra.Core
import Hydra.CoreEncoding
import Hydra.Graph
import Hydra.Module
import Hydra.Lexical
import Hydra.Mantle
import Hydra.Sorting
import Hydra.Rewriting
import Hydra.Lexical
import Hydra.Tools.Debug

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


elementsWithDependencies :: [Element] -> Flow Graph [Element]
elementsWithDependencies original = CM.mapM requireElement allDepNames
  where
    depNames = S.toList . termDependencyNames True False False . elementTerm
    allDepNames = L.nub $ (elementName <$> original) ++ (L.concat $ depNames <$> original)

-- | A variation of expandLambdas which also attaches type annotations when "padding" function terms.
-- TODO: merge into expandLambdas
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

-- | Same as freeVariablesInType, but ignores the binding action of "lambda types".
freeVariablesInTypeSimple :: Type -> S.Set Name
freeVariablesInTypeSimple = foldOverType TraversalOrderPre helper S.empty
  where
    helper types typ = case typ of
      TypeVariable v -> S.insert v types
      _ -> types

freeVariablesInTypeScheme :: TypeScheme -> S.Set Name
freeVariablesInTypeScheme (TypeScheme vars t) = S.difference (freeVariablesInType t) (S.fromList vars)

freeVariablesInTypeSchemeSimple :: TypeScheme -> S.Set Name
freeVariablesInTypeSchemeSimple (TypeScheme vars t) = S.difference (freeVariablesInTypeSimple t) (S.fromList vars)

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

-- | Recursively replace the type variables of let bindings with the systematic type variables t0, t1, t2, ...,
--   e.g. (key = value : forall v, a, v_12. type) becomes (key = norm(S,value) : forall t0, t1, t2. S(type)),
--   S(type) represents the substitution {t0/v, t1/a, t2/v_12} applied to the type, and norm(S, value) is the result
--   of recursively normalizing the value, replacing the domain type variables of lambdas according to S,
--   and descending into further let bindings; for the latter, type variable shadowing is taken into account.
--   Note that the type variables "t0", "t1", etc. are considered to be reserved names;
--   any free occurrences of these names in the type expressions of a term have undefined semantics.
--   If collisions are possible, substitute type variables first before applying this function.
normalizeTypeVariablesInTerm :: Term -> Term
normalizeTypeVariablesInTerm = rewriteWithSubst (M.empty, S.empty)
  where
    rewriteWithSubst (subst, boundVars) = rewriteTerm rewrite
      where
        rewrite recurse term = case term of
            TermFunction (FunctionLambda (Lambda v mdom body)) -> TermFunction $ FunctionLambda $
              Lambda v (fmap (substType subst) mdom) $ rewrite recurse body
            TermLet (Let bindings env) -> TermLet $ Let (rewriteBinding <$> bindings) $ rewrite recurse env
            --TermTyped...
            TermTypeAbstraction (TypeAbstraction name term1) -> TermTypeAbstraction $ TypeAbstraction (replaceName subst name) $ rewrite recurse term1
            TermTypeApplication (TypedTerm term1 typ) -> TermTypeApplication $ TypedTerm (rewrite recurse term1) $ substType subst typ
            _ -> recurse term
          where
            rewriteBinding b@(LetBinding key value mts) = case mts of
              Nothing -> b
              Just (TypeScheme vars typ) -> LetBinding key newValue $ Just $ TypeScheme newVars $ substType newSubst typ
                where
                  newValue = rewriteWithSubst (newSubst, S.union boundVars $ S.fromList newVars) value
                  newSubst = M.union (M.fromList $ L.zip vars newVars) subst
                  newVars = L.take (L.length vars) $ L.filter (\n -> not $ S.member n boundVars) normalVariables
    normalVariables = fmap (\i -> Name $ "t" ++ show i) [0..]
    substType subst = rewriteType rewrite
      where
        rewrite recurse typ = case recurse typ of
          TypeVariable v -> TypeVariable $ replaceName subst v
          --TypeApplication...
          --TypeForall...
          t -> t
    replaceName subst v = Y.fromMaybe v $ M.lookup v subst

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
      TypeForall (ForallType v' body) -> if v == v'
        then t
        else TypeForall $ ForallType v' $ recurse body
      TypeVariable v' -> if v == v' then rep else t
      _ -> recurse t

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
        TypeForall (ForallType v b) -> TypeForall <$> (ForallType <$> pure v <*> recurse b)
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

stripTypeRecursive :: Type -> Type
stripTypeRecursive = rewriteType strip
  where
    strip recurse typ = case recurse typ of
      TypeAnnotated (AnnotatedType t _) -> t
      t -> t

stripTypeSchemeRecursive :: TypeScheme -> TypeScheme
stripTypeSchemeRecursive (TypeScheme vars typ) = TypeScheme vars $ stripTypeRecursive typ

stripTypesFromTerm :: Term -> Term
stripTypesFromTerm = rewriteTerm strip
  where
    strip recurse term = case recurse term of
      -- Note: other annotations are not stripped; only types are.
      TermFunction f -> case f of
         FunctionElimination (EliminationProduct (TupleProjection index arity _)) -> TermFunction $ FunctionElimination
           $ EliminationProduct $ TupleProjection index arity Nothing
         FunctionLambda (Lambda v _ b) -> TermFunction $ FunctionLambda $ Lambda v Nothing b
         _ -> TermFunction f
      TermLet (Let bindings env) -> TermLet $ Let (fmap stripBinding bindings) env
        where
          stripBinding (LetBinding v t _) = LetBinding v t Nothing
      TermTypeAbstraction (TypeAbstraction _ b) -> b
      TermTypeApplication (TypedTerm t _) -> t
      TermTyped (TypedTerm t _) -> t
      t -> t

substituteTypeVariables :: M.Map Name Name -> Type -> Type
substituteTypeVariables subst = rewriteType replace
  where
    replace recurse typ = case typ of
      TypeVariable n -> TypeVariable $ Y.fromMaybe n $ M.lookup n subst
      _ -> recurse typ

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

toShortNames :: [Name] -> M.Map Name Name
toShortNames original = M.fromList $ L.concat (renameGroup <$> M.toList groups)
  where
    groups = groupNamesByLocal original
    renameGroup (local, names) = L.zipWith rename (S.toList names) [1..]
      where
        rename name i = (name, Name $ if i > 1 then local ++ show i else local)
    groupNamesByLocal names = L.foldl addName M.empty names
      where
        addName acc name = M.insert local (S.insert name group) acc
          where
            local = localNameOf name
            group = Y.fromMaybe S.empty $ M.lookup local acc

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

topologicalSortElements :: [Element] -> Either_ [[Name]] [Name]
topologicalSortElements els = topologicalSort $ adjlist <$> els
  where
    adjlist e = (elementName e, S.toList $ termDependencyNames False True True $ elementTerm e)
