-- | Variable substitution and normalization of type expressions
module Hydra.Substitution where

import Hydra.Core
import Hydra.Mantle
import Hydra.Rewriting
import Hydra.Tier1
import Hydra.Dsl.Types as Types

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


type Subst a = M.Map Name (Type)

composeSubst :: Subst Kv -> Subst Kv -> Subst Kv
composeSubst s1 s2 = M.union s1 $ M.map (substituteInType s1) s2

normalVariables :: [Name]
normalVariables = normalVariable <$> [0..]

-- | Type variable naming convention follows Haskell: t0, t1, etc.
normalVariable :: Int -> Name
normalVariable i = Name $ "t" ++ show i

normalizeScheme :: TypeScheme -> TypeScheme
normalizeScheme ts@(TypeScheme _ body) = TypeScheme (fmap snd ord) (normalizeType body)
  where
    ord = L.zip (S.toList $ freeVariablesInType body) normalVariables

    normalizeFieldType (FieldType fname typ) = FieldType fname $ normalizeType typ

    normalizeType typ = case typ of
      TypeApplication (ApplicationType lhs rhs) -> TypeApplication (ApplicationType (normalizeType lhs) (normalizeType rhs))
      TypeAnnotated (AnnotatedType t ann) -> TypeAnnotated (AnnotatedType (normalizeType t) ann)
      TypeFunction (FunctionType dom cod) -> function (normalizeType dom) (normalizeType cod)
      TypeList t -> list $ normalizeType t
      TypeLiteral _ -> typ
      TypeMap (MapType kt vt) -> Types.map (normalizeType kt) (normalizeType vt)
      TypeOptional t -> optional $ normalizeType t
      TypeProduct types -> TypeProduct (normalizeType <$> types)
      TypeRecord (RowType n e fields) -> TypeRecord $ RowType n e (normalizeFieldType <$> fields)
      TypeSet t -> set $ normalizeType t
      TypeSum types -> TypeSum (normalizeType <$> types)
      TypeUnion (RowType n e fields) -> TypeUnion $ RowType n e (normalizeFieldType <$> fields)
      TypeLambda (LambdaType (Name v) t) -> TypeLambda (LambdaType (Name v) $ normalizeType t)
      TypeVariable v -> case Prelude.lookup v ord of
        Just (Name v1) -> var v1
        Nothing -> error $ "type variable " ++ show v ++ " not in signature of type scheme: " ++ show ts
      TypeWrap _ -> typ

substituteInScheme :: M.Map Name (Type) -> TypeScheme -> TypeScheme
substituteInScheme s (TypeScheme as t) = TypeScheme as $ substituteInType s' t
  where
    s' = L.foldr M.delete s as

substituteInType :: M.Map Name (Type) -> Type -> Type
substituteInType s typ = case typ of
    TypeApplication (ApplicationType lhs rhs) -> TypeApplication (ApplicationType (subst lhs) (subst rhs))
    TypeAnnotated (AnnotatedType t ann) -> TypeAnnotated (AnnotatedType (subst t) ann)
    TypeFunction (FunctionType dom cod) -> function (subst dom) (subst cod)
    TypeList t -> list $ subst t
    TypeLiteral _ -> typ
    TypeMap (MapType kt vt) -> Types.map (subst kt) (subst vt)
    TypeOptional t -> optional $ subst t
    TypeProduct types -> TypeProduct (subst <$> types)
    TypeRecord (RowType n e fields) -> TypeRecord $ RowType n e (substField <$> fields)
    TypeSet t -> set $ subst t
    TypeSum types -> TypeSum (subst <$> types)
    TypeUnion (RowType n e fields) -> TypeUnion $ RowType n e (substField <$> fields)
    TypeLambda (LambdaType var@(Name v) body) -> if Y.isNothing (M.lookup var s)
      then TypeLambda (LambdaType (Name v) (subst body))
      else typ
    TypeVariable a -> M.findWithDefault typ a s
    TypeWrap _ -> typ -- because we do not allow names to be bound to types with free variables
  where
    subst = substituteInType s
    substField (FieldType fname t) = FieldType fname $ subst t
