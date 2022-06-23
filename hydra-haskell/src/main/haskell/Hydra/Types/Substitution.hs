module Hydra.Types.Substitution where

import Hydra.Core
import Hydra.Impl.Haskell.Default
import Hydra.Impl.Haskell.Dsl.Types as Types

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


type Subst m = M.Map TypeVariable (Type m)

composeSubst :: Default m => Subst m -> Subst m -> Subst m
composeSubst s1 s2 = M.union s1 $ M.map (substituteInType s1) s2

freeVariablesInScheme :: TypeScheme m -> S.Set TypeVariable
freeVariablesInScheme (TypeScheme vars t) = S.difference (freeVariablesInType t) (S.fromList vars)

freeVariablesInType :: Type m -> S.Set TypeVariable
freeVariablesInType typ = S.fromList $ fv typ
  where
    fv typ = case typeExpr typ of
      TypeExprElement t -> fv t
      TypeExprFunction (FunctionType dom cod) -> fv dom ++ fv cod
      TypeExprList t -> fv t
      TypeExprLiteral _ -> []
      TypeExprMap (MapType kt vt) -> fv kt ++ fv vt
      TypeExprNominal _ -> [] -- because we do not allow names to be bound to types with free variables
      TypeExprOptional t -> fv t
      TypeExprRecord tfields -> L.concat (fv . fieldTypeType <$> tfields)
      TypeExprSet t -> fv t
      TypeExprUnion tfields -> L.concat (fv . fieldTypeType <$> tfields)
      TypeExprLambda (TypeLambda v body) -> v:(fv body)
      TypeExprVariable v -> [v]

normalVariables :: [TypeVariable]
normalVariables = (\n -> TypeVariable $ "v" ++ show n) <$> [1..]

normalizeScheme :: Default m => TypeScheme m -> TypeScheme m
normalizeScheme (TypeScheme _ body) = TypeScheme (fmap snd ord) (normalizeType body)
  where
    ord = L.zip (S.toList $ freeVariablesInType body) normalVariables

    normalizeFieldType (FieldType fname typ) = FieldType fname $ normalizeType typ

    normalizeType typ = case typeExpr typ of
      TypeExprElement t -> element $ normalizeType t
      TypeExprFunction (FunctionType dom cod) -> function (normalizeType dom) (normalizeType cod)
      TypeExprList t -> list $ normalizeType t
      TypeExprLiteral l -> typ
      TypeExprMap (MapType kt vt) -> Types.map (normalizeType kt) (normalizeType vt)
      TypeExprNominal _ -> typ
      TypeExprOptional t -> optional $ normalizeType t
      TypeExprRecord fields -> record (normalizeFieldType <$> fields)
      TypeExprSet t -> set $ normalizeType t
      TypeExprUnion fields -> union (normalizeFieldType <$> fields)
      TypeExprLambda (TypeLambda (TypeVariable v) t) -> universal v $ normalizeType t
      TypeExprVariable v -> case Prelude.lookup v ord of
        Just (TypeVariable v1) -> variable v1
        Nothing -> error "type variable not in signature"

substituteInScheme :: Default m => M.Map TypeVariable (Type m) -> TypeScheme m -> TypeScheme m
substituteInScheme s (TypeScheme as t) = TypeScheme as $ substituteInType s' t
  where
    s' = L.foldr M.delete s as

substituteInType :: Default m => M.Map TypeVariable (Type m) -> Type m -> Type m
substituteInType s typ = case typeExpr typ of
    TypeExprElement t -> element $ subst t
    TypeExprFunction (FunctionType dom cod) -> function (subst dom) (subst cod)
    TypeExprList t -> list $ subst t
    TypeExprLiteral _ -> typ
    TypeExprMap (MapType kt vt) -> Types.map (subst kt) (subst vt)
    TypeExprNominal _ -> typ -- because we do not allow names to be bound to types with free variables
    TypeExprOptional t -> optional $ subst t
    TypeExprRecord tfields -> record (substField <$> tfields)
    TypeExprSet t -> set $ subst t
    TypeExprUnion tfields -> union (substField <$> tfields)
    TypeExprLambda (TypeLambda var@(TypeVariable v) body) -> if Y.isNothing (M.lookup var s)
      then Types.universal v (subst body)
      else typ
    TypeExprVariable a -> M.findWithDefault typ a s
  where
    subst = substituteInType s
    substField (FieldType fname t) = FieldType fname $ subst t
