module Hydra.Impl.Haskell.Dsl.CoreMeta where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Extras


nominalProjection :: Context m -> Name -> FieldName -> Type m -> Term m
nominalProjection cx name fname ftype = withType cx (Types.function (Types.nominal name) ftype) $ projection fname

nominalRecord :: Context m -> Name -> [Field m] -> Term m
nominalRecord cx name fields = named cx name $ record fields

named :: Context m -> Name -> Term m -> Term m
named cx name = withType cx (Types.nominal name)

nominalUnion :: Context m -> Name -> Field m -> Term m
nominalUnion cx name field = withType cx (Types.nominal name) $ union field

nominalUnitVariant :: Context m -> Name -> FieldName -> Term m
nominalUnitVariant cx name fname = nominalVariant cx name fname unit

nominalVariant :: Context m -> Name -> FieldName -> Term m -> Term m
nominalVariant cx name fname term = named cx name $ variant fname term

nominalWithFunction :: Context m -> Name -> FieldName -> Element m -> Term m
nominalWithFunction cx name fname el = lambda var $ nominalVariant cx name fname $ apply (elementRef el) (variable var)
  where var = "x"

nominalWithUnitVariant :: Context m -> Name -> FieldName -> Term m
nominalWithUnitVariant cx name fname = constFunction $ nominalUnitVariant cx name fname

nominalWithVariant :: Context m -> Name -> FieldName -> Term m -> Term m
nominalWithVariant cx name fname term = constFunction $ nominalVariant cx name fname term

withType :: Context m -> Type m -> Term m -> Term m
withType cx typ = annotationClassSetTermType (contextAnnotations cx) cx (Just typ)
