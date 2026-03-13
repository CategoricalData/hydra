-- Note: this is an automatically generated file. Do not edit.

-- | Additional adapter utilities, above and beyond the generated ones.

module Hydra.Adapt.Utils where

import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Reflect as Reflect
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as Core_
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Create a bidirectional coder from a direction-aware function
bidirectional :: ((Coders.CoderDirection -> Context.Context -> t0 -> Either (Context.InContext Error.Error) t0) -> Compute.Coder t0 t0)
bidirectional f = Compute.Coder {
  Compute.coderEncode = (f Coders.CoderDirectionEncode),
  Compute.coderDecode = (f Coders.CoderDirectionDecode)}

-- | Choose an appropriate adapter for a type
chooseAdapter :: ((t0 -> Either String [Compute.Adapter t0 t0 t1 t1]) -> (t0 -> Bool) -> (t0 -> String) -> (t0 -> String) -> t0 -> Either String (Compute.Adapter t0 t0 t1 t1))
chooseAdapter alts supported show describe typ = (Logic.ifElse (supported typ) (Right (Compute.Adapter {
  Compute.adapterIsLossy = False,
  Compute.adapterSource = typ,
  Compute.adapterTarget = typ,
  Compute.adapterCoder = idCoder})) (Eithers.bind (alts typ) (\raw ->  
  let candidates = (Lists.filter (\adapter -> supported (Compute.adapterTarget adapter)) raw)
  in (Logic.ifElse (Lists.null candidates) (Left (Strings.cat [
    "no adapters found for ",
    (describe typ),
    (Logic.ifElse (Lists.null raw) "" (Strings.cat [
      " (discarded ",
      (Literals.showInt32 (Lists.length raw)),
      " unsupported candidate types: ",
      (Core_.list show (Lists.map Compute.adapterTarget raw)),
      ")"])),
    ". Original type: ",
    (show typ)])) (Right (Lists.head candidates))))))

-- | Compose two coders
composeCoders :: (Compute.Coder t0 t1 -> Compute.Coder t1 t2 -> Compute.Coder t0 t2)
composeCoders c1 c2 = Compute.Coder {
  Compute.coderEncode = (\cx -> \a -> Eithers.bind (Compute.coderEncode c1 cx a) (\b1 -> Compute.coderEncode c2 cx b1)),
  Compute.coderDecode = (\cx -> \c -> Eithers.bind (Compute.coderDecode c2 cx c) (\b2 -> Compute.coderDecode c1 cx b2))}

-- | Apply coder in the specified direction
encodeDecode :: (Coders.CoderDirection -> Compute.Coder t0 t0 -> Context.Context -> t0 -> Either (Context.InContext Error.Error) t0)
encodeDecode dir coder cx term = ((\x -> case x of
  Coders.CoderDirectionEncode -> (Compute.coderEncode coder cx term)
  Coders.CoderDirectionDecode -> (Compute.coderDecode coder cx term)) dir)

-- | Check if float type is supported by language constraints
floatTypeIsSupported :: (Coders.LanguageConstraints -> Core.FloatType -> Bool)
floatTypeIsSupported constraints ft = (Sets.member ft (Coders.languageConstraintsFloatTypes constraints))

-- | Identity adapter
idAdapter :: (t0 -> Compute.Adapter t0 t0 t1 t1)
idAdapter t = Compute.Adapter {
  Compute.adapterIsLossy = False,
  Compute.adapterSource = t,
  Compute.adapterTarget = t,
  Compute.adapterCoder = idCoder}

-- | Identity coder
idCoder :: (Compute.Coder t0 t0)
idCoder = Compute.Coder {
  Compute.coderEncode = (\_cx -> \x -> Right x),
  Compute.coderDecode = (\_cx -> \x -> Right x)}

-- | Check if integer type is supported by language constraints
integerTypeIsSupported :: (Coders.LanguageConstraints -> Core.IntegerType -> Bool)
integerTypeIsSupported constraints it = (Sets.member it (Coders.languageConstraintsIntegerTypes constraints))

-- | Check if literal type is supported by language constraints
literalTypeIsSupported :: (Coders.LanguageConstraints -> Core.LiteralType -> Bool)
literalTypeIsSupported constraints lt =  
  let isSupported = (\lt -> (\x -> case x of
          Core.LiteralTypeFloat v0 -> (floatTypeIsSupported constraints v0)
          Core.LiteralTypeInteger v0 -> (integerTypeIsSupported constraints v0)
          _ -> True) lt)
  in (Logic.and (Sets.member (Reflect.literalTypeVariant lt) (Coders.languageConstraintsLiteralVariants constraints)) (isSupported lt))

-- | Check if type is supported by language constraints
typeIsSupported :: (Coders.LanguageConstraints -> Core.Type -> Bool)
typeIsSupported constraints t =  
  let base = (Rewriting.deannotateType t)
  in  
    let isVariable = (\v -> (\x -> case x of
            Variants.TypeVariantVariable -> True
            _ -> False) v)
    in  
      let isSupportedVariant = (\v -> Logic.or (isVariable v) (Sets.member v (Coders.languageConstraintsTypeVariants constraints)))
      in  
        let isSupported = (\base -> (\x -> case x of
                Core.TypeAnnotated v0 -> (typeIsSupported constraints (Core.annotatedTypeBody v0))
                Core.TypeApplication v0 -> (Logic.and (typeIsSupported constraints (Core.applicationTypeFunction v0)) (typeIsSupported constraints (Core.applicationTypeArgument v0)))
                Core.TypeEither v0 -> (Logic.and (typeIsSupported constraints (Core.eitherTypeLeft v0)) (typeIsSupported constraints (Core.eitherTypeRight v0)))
                Core.TypeForall v0 -> (typeIsSupported constraints (Core.forallTypeBody v0))
                Core.TypeFunction v0 -> (Logic.and (typeIsSupported constraints (Core.functionTypeDomain v0)) (typeIsSupported constraints (Core.functionTypeCodomain v0)))
                Core.TypeList v0 -> (typeIsSupported constraints v0)
                Core.TypeLiteral v0 -> (literalTypeIsSupported constraints v0)
                Core.TypeMap v0 -> (Logic.and (typeIsSupported constraints (Core.mapTypeKeys v0)) (typeIsSupported constraints (Core.mapTypeValues v0)))
                Core.TypeMaybe v0 -> (typeIsSupported constraints v0)
                Core.TypePair v0 -> (Logic.and (typeIsSupported constraints (Core.pairTypeFirst v0)) (typeIsSupported constraints (Core.pairTypeSecond v0)))
                Core.TypeRecord v0 -> (Lists.foldl Logic.and True (Lists.map (\field -> typeIsSupported constraints (Core.fieldTypeType field)) (Core.rowTypeFields v0)))
                Core.TypeSet v0 -> (typeIsSupported constraints v0)
                Core.TypeUnion v0 -> (Lists.foldl Logic.and True (Lists.map (\field -> typeIsSupported constraints (Core.fieldTypeType field)) (Core.rowTypeFields v0)))
                Core.TypeUnit -> True
                Core.TypeWrap v0 -> (typeIsSupported constraints (Core.wrappedTypeBody v0))
                Core.TypeVariable _ -> True) base)
        in (Logic.and (Coders.languageConstraintsTypes constraints base) (Logic.and (isSupportedVariant (Reflect.typeVariant base)) (isSupported base)))

-- | Create a unidirectional coder
unidirectionalCoder :: ((Context.Context -> t0 -> Either (Context.InContext Error.Error) t1) -> Compute.Coder t0 t1)
unidirectionalCoder m = Compute.Coder {
  Compute.coderEncode = m,
  Compute.coderDecode = (\cx -> \_ -> Left (Context.InContext {
    Context.inContextObject = (Error.ErrorOther (Error.OtherError "inbound mapping is unsupported")),
    Context.inContextContext = cx}))}
