module Hydra.Types.Substitution where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl.Types as Types

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


type Subst = M.Map TypeVariable Type

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = M.union s1 $ M.map (substituteInType s1) s2

freeVariablesInScheme :: TypeScheme -> S.Set TypeVariable
freeVariablesInScheme (TypeScheme vars t) = S.difference (freeVariablesInType t) (S.fromList vars)

freeVariablesInType :: Type -> S.Set TypeVariable
freeVariablesInType typ = S.fromList $ fv typ
  where
    fv typ = case typ of
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
      TypeUniversal (UniversalType v body) -> v:(fv body)
      TypeVariable v -> [v]

normalVariables :: [String]
normalVariables = (\n -> "v" ++ show n) <$> [1..]

normalizeScheme :: TypeScheme -> TypeScheme
normalizeScheme (TypeScheme _ body) = TypeScheme (fmap snd ord) (normalizeType body)
  where
    ord = L.zip (S.toList $ freeVariablesInType body) normalVariables

    normalizeFieldType (FieldType fname typ) = FieldType fname $ normalizeType typ

    normalizeType typ = case typ of
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
      TypeUniversal (UniversalType v t) -> universal v $ normalizeType t
      TypeVariable v -> case Prelude.lookup v ord of
        Just v1 -> variable v1
        Nothing -> error "type variable not in signature"

substituteInScheme :: M.Map TypeVariable Type -> TypeScheme -> TypeScheme
substituteInScheme s (TypeScheme as t) = TypeScheme as $ substituteInType s' t
  where
    s' = L.foldr M.delete s as

substituteInType :: M.Map TypeVariable Type -> Type -> Type
substituteInType s typ = case typ of
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
    TypeUniversal (UniversalType v body) -> if Y.isNothing (M.lookup v s)
      then TypeUniversal $ UniversalType v (subst body)
      else typ
    TypeVariable a -> M.findWithDefault typ a s
  where
    subst = substituteInType s
    substField (FieldType fname t) = FieldType fname $ subst t
