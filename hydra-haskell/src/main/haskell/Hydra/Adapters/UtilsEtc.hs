module Hydra.Adapters.UtilsEtc (
  module Hydra.Adapters.UtilsEtc,
  module Hydra.Adapters.Utils,
  module Hydra.Common,
) where

import Hydra.Common
import Hydra.Core
import Hydra.Basics
import Hydra.Graph
import Hydra.Monads
import Hydra.Adapter
import Hydra.Adapters.Utils
import qualified Hydra.Lib.Strings as Strings
import Hydra.Util.Formatting
import Hydra.Rewriting

import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad


bidirectional :: (CoderDirection -> b -> Result b) -> Coder b b
bidirectional m = Coder (m CoderDirectionEncode) (m CoderDirectionDecode)

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
      
composeCoders :: Coder a b -> Coder b c -> Coder a c
composeCoders s1 s2 = Coder {
  coderEncode = coderEncode s1 >=> coderEncode s2,
  coderDecode = coderDecode s2 >=> coderDecode s1}

encodeDecode :: CoderDirection -> Coder a a -> a -> Result a
encodeDecode dir = case dir of
  CoderDirectionEncode -> coderEncode
  CoderDirectionDecode -> coderDecode

floatTypeIsSupported :: LanguageConstraints m -> FloatType -> Bool
floatTypeIsSupported constraints ft = S.member ft $ languageConstraintsFloatTypes constraints

idAdapter :: Type m -> Adapter (Type m) (Term m)
idAdapter t = Adapter False t t idCoder

idCoder :: Coder a a
idCoder = Coder pure pure

integerTypeIsSupported :: LanguageConstraints m -> IntegerType -> Bool
integerTypeIsSupported constraints it = S.member it $ languageConstraintsIntegerTypes constraints

literalTypeIsSupported :: LanguageConstraints m -> LiteralType -> Bool
literalTypeIsSupported constraints at = S.member (literalTypeVariant at) (languageConstraintsLiteralVariants constraints)
  && case at of
    LiteralTypeFloat ft -> floatTypeIsSupported constraints ft
    LiteralTypeInteger it -> integerTypeIsSupported constraints it
    _ -> True

nameToFilePath :: Bool -> FileExtension -> Name -> FilePath
nameToFilePath caps ext name = graphNameToFilePath caps ext $ GraphName $ gname ++ "/" ++ local
  where
    (GraphName gname, local) = toQname name

qualify :: String -> a -> Qualified a
qualify msg x = Qualified (Just x) [msg]

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

unidirectionalCoder :: (a -> Result b) -> Coder a b
unidirectionalCoder m = Coder {
  coderEncode = m,
  coderDecode = \_ -> fail "inbound mapping is unsupported"}
