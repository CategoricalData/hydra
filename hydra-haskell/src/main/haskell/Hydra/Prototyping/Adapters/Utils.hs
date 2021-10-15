module Hydra.Prototyping.Adapters.Utils (
  atomicTypeIsSupported,
  chooseAdapter,
  describeAtomicType,
  describeFloatType,
  describeIntegerType,
  describePrecision,
  describeType,
  floatTypeIsSupported,
  integerTypeIsSupported,
  qualify,
  typeIsSupported,
  unqualify,
) where

import Hydra.Core
import Hydra.Prototyping.Basics
import Hydra.Prototyping.Steps
import Hydra.Impl.Haskell.Extras
import Hydra.Adapter

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
          ++ if L.null raw then "" else " (discarded " ++ show (L.length raw) ++ " unsupported types: " ++ show (adapterTarget <$> raw) ++ ")"
        else return $ L.head candidates

describeAtomicType :: AtomicType -> String
describeAtomicType t = case t of
  AtomicTypeBinary -> "binary strings"
  AtomicTypeBoolean -> "boolean values"
  AtomicTypeFloat ft -> describeFloatType ft
  AtomicTypeInteger it -> describeIntegerType it
  AtomicTypeString -> "character strings"

describeFloatType :: FloatType -> String
describeFloatType t = describePrecision (floatTypePrecision t) ++ " floating-point numbers" 

describeIntegerType :: IntegerType -> String
describeIntegerType t = describePrecision (integerTypePrecision t) ++ " integers"

describePrecision :: Precision -> String
describePrecision p = case p of
  PrecisionArbitrary -> "arbitrary-precision"
  PrecisionBits bits -> show bits ++ "-bit"

describeType :: Type -> String
describeType t = case t of
  TypeAtomic at -> describeAtomicType at
  TypeElement t -> "elements containing " ++ describeType t 
  TypeFunction (FunctionType dom cod) -> "functions from " ++ describeType dom ++ " to " ++ describeType cod
  TypeList t -> "lists of " ++ describeType t
  TypeMap (MapType kt vt) -> "maps from " ++ describeType kt ++ " to " ++ describeType vt
  TypeNominal name -> "alias for " ++ name
  TypeOptional t -> "optional " ++ describeType t
  TypeRecord _ -> "records of a particular set of fields"
  TypeSet t -> "sets of " ++ describeType t
  TypeUnion _ -> "unions of a particular set of fields"

qualify :: String -> a -> Qualified a
qualify msg x = Qualified (Just x) [msg]

atomicTypeIsSupported :: Language_Constraints -> AtomicType -> Bool
atomicTypeIsSupported constraints at = S.member (atomicTypeVariant at) (languageConstraintsAtomicVariants constraints)
  && case at of
    AtomicTypeFloat ft -> floatTypeIsSupported constraints ft
    AtomicTypeInteger it -> integerTypeIsSupported constraints it
    _ -> True
    
floatTypeIsSupported :: Language_Constraints -> FloatType -> Bool
floatTypeIsSupported constraints ft = S.member (floatTypeVariant ft) $ languageConstraintsFloatVariants constraints

integerTypeIsSupported :: Language_Constraints -> IntegerType -> Bool
integerTypeIsSupported constraints it = S.member (integerTypeVariant it) $ languageConstraintsIntegerVariants constraints

typeIsSupported :: Language_Constraints -> Type -> Bool
typeIsSupported constraints t = S.member (typeVariant t) (languageConstraintsTypeVariants constraints)
  && case t of
    TypeAtomic at -> atomicTypeIsSupported constraints at
    TypeFunction (FunctionType dom cod) -> typeIsSupported constraints dom && typeIsSupported constraints cod
    TypeList lt -> typeIsSupported constraints lt
    TypeMap (MapType kt vt) -> typeIsSupported constraints kt && typeIsSupported constraints vt
    TypeNominal _ -> True -- TODO: dereference the type
    TypeOptional t -> typeIsSupported constraints t
    TypeRecord sfields -> or $ typeIsSupported constraints . fieldTypeType <$> sfields
    TypeSet st -> typeIsSupported constraints st
    TypeUnion sfields -> or $ typeIsSupported constraints . fieldTypeType <$> sfields
    _ -> True

unqualify :: Qualified a -> a
unqualify (Qualified (Just x) _) = x
