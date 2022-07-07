module Hydra.Types.Substitution where

import Hydra.Core
import Hydra.Impl.Haskell.Default
import Hydra.Impl.Haskell.Dsl.Types as Types

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


type Subst m = M.Map VariableType (Type m)

composeSubst :: Default m => Subst m -> Subst m -> Subst m
composeSubst s1 s2 = M.union s1 $ M.map (substituteInType s1) s2

freeVariablesInScheme :: TypeScheme m -> S.Set VariableType
freeVariablesInScheme (TypeScheme vars t) = S.difference (freeVariablesInType t) (S.fromList vars)

freeVariablesInType :: Type m -> S.Set VariableType
freeVariablesInType typ = S.fromList $ fv typ
  where
    fv typ = case typ of
      TypeAnnotated (Annotated t _) -> fv t
      TypeElement t -> fv t
      TypeFunction (FunctionType dom cod) -> fv dom ++ fv cod
      TypeList t -> fv t
      TypeLiteral _ -> []
      TypeMap (MapType kt vt) -> fv kt ++ fv vt
      TypeNominal _ -> [] -- because we do not allow names to be bound to types with free variables
      TypeOptional t -> fv t
      TypeRecord tfields -> L.concat (fv . fieldTypeType <$> tfields)
      TypeSet t -> fv t
      TypeUnion tfields -> L.concat (fv . fieldTypeType <$> tfields)
      TypeLambda (LambdaType v body) -> v:(fv body)
      TypeVariable v -> [v]

normalVariables :: [VariableType]
normalVariables = (\n -> VariableType $ "v" ++ show n) <$> [1..]

normalizeScheme :: Default m => TypeScheme m -> TypeScheme m
normalizeScheme (TypeScheme _ body) = TypeScheme (fmap snd ord) (normalizeType body)
  where
    ord = L.zip (S.toList $ freeVariablesInType body) normalVariables

    normalizeFieldType (FieldType fname typ) = FieldType fname $ normalizeType typ

    normalizeType typ = case typ of
      TypeAnnotated (Annotated t ann) -> TypeAnnotated (Annotated (normalizeType t) ann)
      TypeElement t -> element $ normalizeType t
      TypeFunction (FunctionType dom cod) -> function (normalizeType dom) (normalizeType cod)
      TypeList t -> list $ normalizeType t
      TypeLiteral l -> typ
      TypeMap (MapType kt vt) -> Types.map (normalizeType kt) (normalizeType vt)
      TypeNominal _ -> typ
      TypeOptional t -> optional $ normalizeType t
      TypeRecord fields -> record (normalizeFieldType <$> fields)
      TypeSet t -> set $ normalizeType t
      TypeUnion fields -> union (normalizeFieldType <$> fields)
      TypeLambda (LambdaType (VariableType v) t) -> universal v $ normalizeType t
      TypeVariable v -> case Prelude.lookup v ord of
        Just (VariableType v1) -> variable v1
        Nothing -> error "type variable not in signature"

substituteInScheme :: Default m => M.Map VariableType (Type m) -> TypeScheme m -> TypeScheme m
substituteInScheme s (TypeScheme as t) = TypeScheme as $ substituteInType s' t
  where
    s' = L.foldr M.delete s as

substituteInType :: Default m => M.Map VariableType (Type m) -> Type m -> Type m
substituteInType s typ = case typ of
    TypeAnnotated (Annotated t ann) -> TypeAnnotated (Annotated (subst t) ann)
    TypeElement t -> element $ subst t
    TypeFunction (FunctionType dom cod) -> function (subst dom) (subst cod)
    TypeList t -> list $ subst t
    TypeLiteral _ -> typ
    TypeMap (MapType kt vt) -> Types.map (subst kt) (subst vt)
    TypeNominal _ -> typ -- because we do not allow names to be bound to types with free variables
    TypeOptional t -> optional $ subst t
    TypeRecord tfields -> record (substField <$> tfields)
    TypeSet t -> set $ subst t
    TypeUnion tfields -> union (substField <$> tfields)
    TypeLambda (LambdaType var@(VariableType v) body) -> if Y.isNothing (M.lookup var s)
      then Types.universal v (subst body)
      else typ
    TypeVariable a -> M.findWithDefault typ a s
  where
    subst = substituteInType s
    substField (FieldType fname t) = FieldType fname $ subst t
