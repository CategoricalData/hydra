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


bidirectional :: (CoderDirection -> b -> Flow s b) -> Coder s b b
bidirectional m = Coder (m CoderDirectionEncode) (m CoderDirectionDecode)

chooseAdapter :: Show t =>
    (t -> [Flow so (Adapter si t v)])
 -> (t -> Bool)
 -> (t -> String)
 -> t
 -> Flow so (Adapter si t v)
chooseAdapter alts supported describe typ = if supported typ
  then pure $ Adapter False typ typ idCoder
  else do
    raw <- sequence (alts typ)
    let candidates = L.filter (supported . adapterTarget) raw
    if L.null candidates
      then fail $ "no adapters found for " ++ describe typ
        ++ (if L.null raw
           then ""
           else " (discarded " ++ show (L.length raw) ++ " unsupported candidate types: " ++ show (adapterTarget <$> raw) ++ ")")
        ++ ". Original type: " ++ show typ
      else return $ L.head candidates

composeCoders :: Coder s a b -> Coder s b c -> Coder s a c
composeCoders c1 c2 = Coder {
  coderEncode = coderEncode c1 >=> coderEncode c2,
  coderDecode = coderDecode c2 >=> coderDecode c1}

encodeDecode :: CoderDirection -> Coder s a a -> a -> Flow s a
encodeDecode dir = case dir of
  CoderDirectionEncode -> coderEncode
  CoderDirectionDecode -> coderDecode

floatTypeIsSupported :: LanguageConstraints m -> FloatType -> Bool
floatTypeIsSupported constraints ft = S.member ft $ languageConstraintsFloatTypes constraints

idAdapter :: Type m -> Adapter s (Type m) (Term m)
idAdapter t = Adapter False t t idCoder

idCoder :: Coder s a a
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
nameToFilePath caps ext name = namespaceToFilePath caps ext $ Namespace $ gname ++ "/" ++ local
  where
    (Namespace gname, local) = toQname name

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

unidirectionalCoder :: (a -> Flow s b) -> Coder s a b
unidirectionalCoder m = Coder {
  coderEncode = m,
  coderDecode = \_ -> fail "inbound mapping is unsupported"}
