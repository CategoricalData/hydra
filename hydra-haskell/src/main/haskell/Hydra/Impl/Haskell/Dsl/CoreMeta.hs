module Hydra.Impl.Haskell.Dsl.CoreMeta (
  nominalCases,
  nominalMatchWithVariants,
  nominalProjection,
  nominalRecord,
  nominalTerm,
  nominalUnion,
  nominalUnitVariant,
  nominalVariant,
  nominalWithFunction,
  nominalWithVariant,
  withDoc,
  withType,
  module Hydra.Impl.Haskell.Dsl.Terms,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Extras


nominalCases :: Default a => Context a -> Name -> [Field a] -> Type -> Term a
nominalCases cx name fields codomain = withType cx (functionType (nominalType name) codomain) $ cases fields

nominalMatchWithVariants :: Default a => Context a -> Name -> [(FieldName, FieldName)] -> Term a
nominalMatchWithVariants cx name = cases . fmap toField
  where
    toField (from, to) = Field from $ constFunction $ nominalUnitVariant cx name to

nominalProjection :: Default a => Context a -> Name -> FieldName -> Type -> Term a
nominalProjection cx name fname ftype = withType cx (functionType (nominalType name) ftype) $ projection fname

nominalRecord :: Default a => Context a -> Name -> [Field a] -> Term a
nominalRecord cx name fields = nominalTerm cx name $ record fields

nominalTerm :: Context a -> Name -> Term a -> Term a
nominalTerm cx name = withType cx (nominalType name)

nominalUnion :: Default a => Context a -> Name -> Field a -> Term a
nominalUnion cx name field = withType cx (nominalType name) $ union field

nominalUnitVariant :: Default a => Context a -> Name -> FieldName -> Term a
nominalUnitVariant cx name fname = nominalVariant cx name fname unitTerm

nominalVariant :: Default a => Context a -> Name -> FieldName -> Term a -> Term a
nominalVariant cx name fname term = nominalTerm cx name $ variant fname term

nominalWithFunction :: Default a => Context a -> Name -> FieldName -> Element a -> Term a
nominalWithFunction cx name fname el = lambda var $ nominalVariant cx name fname $ apply (elementRef el) (variable var)
  where var = "x"

nominalWithVariant :: Default a => Context a -> Name -> FieldName -> Term a
nominalWithVariant cx name fname = constFunction $ nominalUnitVariant cx name fname

withType :: Context a -> Type -> Term a -> Term a
withType cx typ term = term { termMeta = contextSetTypeOf cx (Just typ) (termMeta term)}


withDoc :: String -> Term Meta -> Term Meta
withDoc desc term = term { termMeta = (termMeta term) {metaDescription = Just desc}}
