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
    fv typ = case typeTerm typ of
      TypeTermElement t -> fv t
      TypeTermFunction (FunctionType dom cod) -> fv dom ++ fv cod
      TypeTermList t -> fv t
      TypeTermLiteral _ -> []
      TypeTermMap (MapType kt vt) -> fv kt ++ fv vt
      TypeTermNominal _ -> [] -- because we do not allow names to be bound to types with free variables
      TypeTermOptional t -> fv t
      TypeTermRecord tfields -> L.concat (fv . fieldTypeType <$> tfields)
      TypeTermSet t -> fv t
      TypeTermUnion tfields -> L.concat (fv . fieldTypeType <$> tfields)
      TypeTermUniversal (UniversalType v body) -> v:(fv body)
      TypeTermVariable v -> [v]

normalVariables :: [String]
normalVariables = (\n -> "v" ++ show n) <$> [1..]

normalizeScheme :: Default m => TypeScheme m -> TypeScheme m
normalizeScheme (TypeScheme _ body) = TypeScheme (fmap snd ord) (normalizeType body)
  where
    ord = L.zip (S.toList $ freeVariablesInType body) normalVariables

    normalizeFieldType (FieldType fname typ) = FieldType fname $ normalizeType typ

    normalizeType typ = case typeTerm typ of
      TypeTermElement t -> element $ normalizeType t
      TypeTermFunction (FunctionType dom cod) -> function (normalizeType dom) (normalizeType cod)
      TypeTermList t -> list $ normalizeType t
      TypeTermLiteral l -> typ
      TypeTermMap (MapType kt vt) -> Types.map (normalizeType kt) (normalizeType vt)
      TypeTermNominal _ -> typ
      TypeTermOptional t -> optional $ normalizeType t
      TypeTermRecord fields -> record (normalizeFieldType <$> fields)
      TypeTermSet t -> set $ normalizeType t
      TypeTermUnion fields -> union (normalizeFieldType <$> fields)
      TypeTermUniversal (UniversalType v t) -> universal v $ normalizeType t
      TypeTermVariable v -> case Prelude.lookup v ord of
        Just v1 -> variable v1
        Nothing -> error "type variable not in signature"

substituteInScheme :: Default m => M.Map TypeVariable (Type m) -> TypeScheme m -> TypeScheme m
substituteInScheme s (TypeScheme as t) = TypeScheme as $ substituteInType s' t
  where
    s' = L.foldr M.delete s as

substituteInType :: Default m => M.Map TypeVariable (Type m) -> Type m -> Type m
substituteInType s typ = case typeTerm typ of
    TypeTermElement t -> element $ subst t
    TypeTermFunction (FunctionType dom cod) -> function (subst dom) (subst cod)
    TypeTermList t -> list $ subst t
    TypeTermLiteral _ -> typ
    TypeTermMap (MapType kt vt) -> Types.map (subst kt) (subst vt)
    TypeTermNominal _ -> typ -- because we do not allow names to be bound to types with free variables
    TypeTermOptional t -> optional $ subst t
    TypeTermRecord tfields -> record (substField <$> tfields)
    TypeTermSet t -> set $ subst t
    TypeTermUnion tfields -> union (substField <$> tfields)
    TypeTermUniversal (UniversalType v body) -> if Y.isNothing (M.lookup v s)
      then Types.universal v (subst body)
      else typ
    TypeTermVariable a -> M.findWithDefault typ a s
  where
    subst = substituteInType s
    substField (FieldType fname t) = FieldType fname $ subst t
