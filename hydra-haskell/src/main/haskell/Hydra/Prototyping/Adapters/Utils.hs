module Hydra.Prototyping.Adapters.Utils (
  literalTypeIsSupported,
  chooseAdapter,
  describeType,
  floatTypeIsSupported,
  idAdapter,
  integerTypeIsSupported,
  qualify,
  typeIsSupported,
  module Hydra.Adapters.Utils
) where

import Hydra.Core
import Hydra.Basics
import Hydra.Prototyping.Steps
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

describeType :: Type -> String
describeType t = case t of
  TypeLiteral at -> describeLiteralType at
  TypeElement t -> "elements containing " ++ describeType t
  TypeFunction (FunctionType dom cod) -> "functions from " ++ describeType dom ++ " to " ++ describeType cod
  TypeList t -> "lists of " ++ describeType t
  TypeMap (MapType kt vt) -> "maps from " ++ describeType kt ++ " to " ++ describeType vt
  TypeNominal name -> "alias for " ++ name
  TypeOptional t -> "optional " ++ describeType t
  TypeRecord _ -> "records of a particular set of fields"
  TypeSet t -> "sets of " ++ describeType t
  TypeUnion _ -> "unions of a particular set of fields"
  TypeUniversal _ -> "polymorphic terms"
  TypeVariable _ -> "unspecified/parameteric terms"

idAdapter :: Type -> Adapter Type (Term a)
idAdapter t = Adapter False t t idStep

qualify :: String -> a -> Qualified a
qualify msg x = Qualified (Just x) [msg]

literalTypeIsSupported :: Language_Constraints -> LiteralType -> Bool
literalTypeIsSupported constraints at = S.member (literalTypeVariant at) (languageConstraintsLiteralVariants constraints)
  && case at of
    LiteralTypeFloat ft -> floatTypeIsSupported constraints ft
    LiteralTypeInteger it -> integerTypeIsSupported constraints it
    _ -> True

floatTypeIsSupported :: Language_Constraints -> FloatType -> Bool
floatTypeIsSupported constraints ft = S.member ft $ languageConstraintsFloatTypes constraints

integerTypeIsSupported :: Language_Constraints -> IntegerType -> Bool
integerTypeIsSupported constraints it = S.member it $ languageConstraintsIntegerTypes constraints

typeIsSupported :: Language_Constraints -> Type -> Bool
typeIsSupported constraints t = languageConstraintsTypes constraints t -- these are *additional* type constraints
  && S.member (typeVariant t) (languageConstraintsTypeVariants constraints)
  && case t of
    TypeLiteral at -> literalTypeIsSupported constraints at
    TypeFunction (FunctionType dom cod) -> typeIsSupported constraints dom && typeIsSupported constraints cod
    TypeList lt -> typeIsSupported constraints lt
    TypeMap (MapType kt vt) -> typeIsSupported constraints kt && typeIsSupported constraints vt
    TypeNominal _ -> True -- TODO: dereference the type
    TypeOptional t -> typeIsSupported constraints t
    TypeRecord sfields -> and $ typeIsSupported constraints . fieldTypeType <$> sfields
    TypeSet st -> typeIsSupported constraints st
    TypeUnion sfields -> and $ typeIsSupported constraints . fieldTypeType <$> sfields
    _ -> True
