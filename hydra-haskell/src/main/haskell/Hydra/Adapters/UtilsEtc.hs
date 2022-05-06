module Hydra.Adapters.UtilsEtc (
  literalTypeIsSupported,
  chooseAdapter,
  floatTypeIsSupported,
  idAdapter,
  integerTypeIsSupported,
  qualify,
  typeIsSupported,
  module Hydra.Adapters.Utils
) where

import Hydra.Core
import Hydra.Basics
import Hydra.Steps
import Hydra.Impl.Haskell.Extras
import Hydra.Adapter
import Hydra.Adapters.Utils

import qualified Data.List as L
import qualified Data.Set as S


chooseAdapter :: Show t =>
    (t -> [Qualified (Adapter t v)])
 -> (t -> Bool)
 -> (t -> String)
 -> t
 -> Qualified (Adapter t v)
chooseAdapter alts supported describe typ = if supported typ
  then pure $ Adapter False typ typ idStep
  else do
    raw <- sequence (alts typ)
    let candidates = L.filter (supported . adapterTarget) raw
    if L.null candidates
      then fail $ "no adapters found for " ++ describe typ
        ++ (if L.null raw
           then ""
           else " (discarded " ++ show (L.length raw) ++ " unsupported types: " ++ show (adapterTarget <$> raw) ++ ")")
        ++ ". Type definition: " ++ show typ
      else return $ L.head candidates

idAdapter :: Type m -> Adapter (Type m) (Term m)
idAdapter t = Adapter False t t idStep

qualify :: String -> a -> Qualified a
qualify msg x = Qualified (Just x) [msg]

literalTypeIsSupported :: Language_Constraints m -> LiteralType -> Bool
literalTypeIsSupported constraints at = S.member (literalTypeVariant at) (languageConstraintsLiteralVariants constraints)
  && case at of
    LiteralTypeFloat ft -> floatTypeIsSupported constraints ft
    LiteralTypeInteger it -> integerTypeIsSupported constraints it
    _ -> True

floatTypeIsSupported :: Language_Constraints m -> FloatType -> Bool
floatTypeIsSupported constraints ft = S.member ft $ languageConstraintsFloatTypes constraints

integerTypeIsSupported :: Language_Constraints m -> IntegerType -> Bool
integerTypeIsSupported constraints it = S.member it $ languageConstraintsIntegerTypes constraints

typeIsSupported :: Language_Constraints m -> Type m -> Bool
typeIsSupported constraints t = languageConstraintsTypes constraints t -- these are *additional* type constraints
  && S.member (typeVariant t) (languageConstraintsTypeVariants constraints)
  && case typeData t of
    TypeExprLiteral at -> literalTypeIsSupported constraints at
    TypeExprFunction (FunctionType dom cod) -> typeIsSupported constraints dom && typeIsSupported constraints cod
    TypeExprList lt -> typeIsSupported constraints lt
    TypeExprMap (MapType kt vt) -> typeIsSupported constraints kt && typeIsSupported constraints vt
    TypeExprNominal _ -> True -- TODO: dereference the type
    TypeExprOptional t -> typeIsSupported constraints t
    TypeExprRecord sfields -> and $ typeIsSupported constraints . fieldTypeType <$> sfields
    TypeExprSet st -> typeIsSupported constraints st
    TypeExprUnion sfields -> and $ typeIsSupported constraints . fieldTypeType <$> sfields
    _ -> True
