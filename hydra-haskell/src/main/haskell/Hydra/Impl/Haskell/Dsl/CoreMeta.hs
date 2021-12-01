module Hydra.Impl.Haskell.Dsl.CoreMeta (
  nominalCases,
  nominalProjection,
  nominalRecord,
  nominalTerm,
  nominalUnion,
  withDoc,
  withType,
  module Hydra.Impl.Haskell.Dsl.Terms,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Extras


nominalCases :: Default a => Context a -> Name -> [Field a] -> Type -> Term a
nominalCases context name fields codomain = withType context (functionType (nominalType name) codomain) $ cases fields

nominalProjection :: Default a => Context a -> Name -> FieldName -> Type -> Term a
nominalProjection context name fname ftype = withType context
  (functionType (nominalType name) ftype) $
  projection (Projection fname name) -- TODO: simplify me

nominalRecord :: Default a => Context a -> Name -> [Field a] -> Term a
nominalRecord context name fields = withType context (nominalType name) $ record fields

nominalTerm :: Context a -> Name -> Term a -> Term a
nominalTerm context name = withType context (nominalType name)

nominalUnion :: Default a => Context a -> Name -> Field a -> Term a
nominalUnion context name field = withType context (nominalType name) $ union name field -- TODO: simplify me

withType :: Context a -> Type -> Term a -> Term a
withType context typ term = term { termMeta = contextSetTypeOf context (Just typ) (termMeta term)}


withDoc :: String -> Term Meta -> Term Meta
withDoc desc term = term { termMeta = (termMeta term) {metaDescription = Just desc}}
