module Hydra.Prototyping.Helpers (
  functionType,
  nominalType,
  stringTerm,
  unitTerm,
  unitVariant,
  variant
) where

import Hydra.Core


functionType :: Type -> Type -> Type
functionType dom cod = TypeFunction $ FunctionType dom cod

nominalType :: Name -> Type
nominalType = TypeNominal

stringTerm = TermAtomic . AtomicValueString

unitTerm :: Term
unitTerm = TermRecord []

unitVariant :: FieldName -> Term
unitVariant fname = variant fname unitTerm

variant :: FieldName -> Term -> Term
variant fname term = TermUnion (Field fname term)
