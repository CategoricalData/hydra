module Hydra.Prototyping.Types.Substitution where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl.Terms

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


type Subst = M.Map TypeVariable Type

composeSubstitutions :: Subst -> Subst -> Subst
composeSubstitutions s1 s2 = M.union s1 $ M.map (sustituteVariablesInType s1) s2

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
      TypeVariable v -> [v]

freeVariablesInTypeScheme :: TypeScheme -> S.Set TypeVariable
freeVariablesInTypeScheme (TypeScheme as t) = S.difference (freeVariablesInType t) (S.fromList as)

normalVariables :: [String]
normalVariables = (\n -> "v" ++ show n) <$> [1..]

normalizeTypeScheme :: TypeScheme -> TypeScheme
normalizeTypeScheme (TypeScheme _ body) = TypeScheme (fmap snd ord) (normalizeType body)
  where
    ord = L.zip (S.toList $ freeVariablesInType body) normalVariables

    normalizeFieldType (FieldType fname typ) = FieldType fname $ normalizeType typ

    normalizeType typ = case typ of
      TypeElement t -> TypeElement $ normalizeType t
      TypeFunction (FunctionType dom cod) -> functionType (normalizeType dom) (normalizeType cod)
      TypeList t -> TypeList $ normalizeType t
      TypeLiteral l -> typ
      TypeMap (MapType kt vt) -> TypeMap $ MapType (normalizeType kt) (normalizeType vt)
      TypeNominal _ -> typ
      TypeOptional t -> TypeOptional $ normalizeType t
      TypeRecord fields -> TypeRecord (normalizeFieldType <$> fields)
      TypeSet t -> TypeSet $ normalizeType t
      TypeUnion fields -> TypeUnion (normalizeFieldType <$> fields)
      TypeUniversal (UniversalType v t) -> TypeUniversal $ UniversalType v $ normalizeType t
      TypeVariable v -> case Prelude.lookup v ord of
        Just v1 -> TypeVariable v1
        Nothing -> error "type variable not in signature"

sustituteVariablesInType :: M.Map TypeVariable Type -> Type -> Type
sustituteVariablesInType s typ = case typ of
    TypeElement t -> elementType $ subst t
    TypeFunction (FunctionType dom cod) -> functionType (subst dom) (subst cod)
    TypeList t -> listType $ subst t
    TypeLiteral _ -> typ
    TypeMap (MapType kt vt) -> mapType (subst kt) (subst vt)
    TypeNominal _ -> typ -- because we do not allow names to be bound to types with free variables
    TypeOptional t -> optionalType $ subst t
    TypeRecord tfields -> recordType (substField <$> tfields)
    TypeSet t -> setType $ subst t
    TypeUnion tfields -> unionType (substField <$> tfields)
    TypeVariable a -> M.findWithDefault typ a s
  where
    subst = sustituteVariablesInType s
    substField (FieldType fname t) = FieldType fname $ subst t

sustituteVariablesInTypeScheme :: M.Map TypeVariable Type -> TypeScheme -> TypeScheme
sustituteVariablesInTypeScheme s (TypeScheme as t) = TypeScheme as $ sustituteVariablesInType s' t
  where
    s' = L.foldr M.delete s as
