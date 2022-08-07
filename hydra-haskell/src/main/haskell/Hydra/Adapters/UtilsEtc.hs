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
  then pure $ Adapter False typ typ idCoder
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
idAdapter t = Adapter False t t idCoder

qualify :: String -> a -> Qualified a
qualify msg x = Qualified (Just x) [msg]

literalTypeIsSupported :: LanguageConstraints m -> LiteralType -> Bool
literalTypeIsSupported constraints at = S.member (literalTypeVariant at) (languageConstraintsLiteralVariants constraints)
  && case at of
    LiteralTypeFloat ft -> floatTypeIsSupported constraints ft
    LiteralTypeInteger it -> integerTypeIsSupported constraints it
    _ -> True

floatTypeIsSupported :: LanguageConstraints m -> FloatType -> Bool
floatTypeIsSupported constraints ft = S.member ft $ languageConstraintsFloatTypes constraints

integerTypeIsSupported :: LanguageConstraints m -> IntegerType -> Bool
integerTypeIsSupported constraints it = S.member it $ languageConstraintsIntegerTypes constraints

typeIsSupported :: LanguageConstraints m -> Type m -> Bool
typeIsSupported constraints t = languageConstraintsTypes constraints t -- these are *additional* type constraints
  && S.member (typeVariant t) (languageConstraintsTypeVariants constraints)
  && case t of
    TypeAnnotated (Annotated at _) -> typeIsSupported constraints at
    TypeLiteral at -> literalTypeIsSupported constraints at
    TypeFunction (FunctionType dom cod) -> typeIsSupported constraints dom && typeIsSupported constraints cod
    TypeList lt -> typeIsSupported constraints lt
    TypeMap (MapType kt vt) -> typeIsSupported constraints kt && typeIsSupported constraints vt
    TypeNominal _ -> True -- TODO: dereference the type
    TypeOptional t -> typeIsSupported constraints t
    TypeRecord rt -> and $ typeIsSupported constraints . fieldTypeType <$> rowTypeFields rt
    TypeSet st -> typeIsSupported constraints st
    TypeUnion rt -> and $ typeIsSupported constraints . fieldTypeType <$> rowTypeFields rt
    _ -> True
