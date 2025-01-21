-- | Additional adapter utilities, above and beyond the generated ones

module Hydra.AdapterUtils (
  module Hydra.AdapterUtils,
  module Hydra.Printing,
) where

import Hydra.Coders
import Hydra.Compute
import Hydra.Core
import Hydra.Basics
import Hydra.Module
import Hydra.Printing
import Hydra.Mantle
import Hydra.Strip
import Hydra.Annotations
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Dsl.Expect as Expect
import qualified Hydra.Dsl.Terms as Terms

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Maybe as Y
import Control.Monad


type SymmetricAdapter s t v = Adapter s s t t v v

type TypeAdapter = Type -> Flow AdapterContext (SymmetricAdapter AdapterContext Type Term)

key_types = Name "types"

bidirectional :: (CoderDirection -> b -> Flow s b) -> Coder s s b b
bidirectional f = Coder (f CoderDirectionEncode) (f CoderDirectionDecode)

chooseAdapter :: (Eq t, Ord t, Show t) =>
    (t -> Flow so [SymmetricAdapter si t v])
 -> (t -> Bool)
 -> (t -> String)
 -> t
 -> Flow so (SymmetricAdapter si t v)
chooseAdapter alts supported describe typ = if supported typ
  then pure $ Adapter False typ typ idCoder
  else do
    -- Uncomment to debug adapter cycles
    -- debugCheckType typ

    raw <- alts typ
    let candidates = L.filter (supported . adapterTarget) raw
    if L.null candidates
      then fail $ "no adapters found for " ++ describe typ
        ++ (if L.null raw
           then ""
           else " (discarded " ++ show (L.length raw) ++ " unsupported candidate types: " ++ show (adapterTarget <$> raw) ++ ")")
        ++ ". Original type: " ++ show typ
      else do
        -- Uncomment to debug adapter cycles
        -- debugRemoveType typ

        return $ L.head candidates

composeCoders :: Coder s s a b -> Coder s s b c -> Coder s s a c
composeCoders c1 c2 = Coder {
  coderEncode = coderEncode c1 >=> coderEncode c2,
  coderDecode = coderDecode c2 >=> coderDecode c1}

debugCheckType :: (Eq t, Ord t, Show t) => t -> Flow s ()
debugCheckType typ = do
  let s = show typ
  types <- getAttrWithDefault key_types (Terms.set S.empty) >>= Expect.set Expect.string
  if S.member s types
    then fail $ "detected a cycle; type has already been encountered: " ++ show typ
    else putAttr key_types $ Terms.set $ S.fromList (Terms.string <$> (S.toList $ S.insert s types))
  return ()

debugRemoveType :: (Eq t, Ord t, Show t) => t -> Flow s ()
debugRemoveType typ = do
  let s = show typ
  types <- getAttrWithDefault key_types (Terms.set S.empty) >>= Expect.set Expect.string
  let types' = S.delete s types
  putAttr key_types $ Terms.set $ S.fromList (Terms.string <$> (S.toList $ S.insert s types'))

encodeDecode :: CoderDirection -> Coder s s x x -> x -> Flow s x
encodeDecode dir = case dir of
  CoderDirectionEncode -> coderEncode
  CoderDirectionDecode -> coderDecode

floatTypeIsSupported :: LanguageConstraints -> FloatType -> Bool
floatTypeIsSupported constraints ft = S.member ft $ languageConstraintsFloatTypes constraints

idAdapter :: t -> SymmetricAdapter s t v
idAdapter t = Adapter False t t idCoder

idCoder :: Coder s s a a
idCoder = Coder pure pure

integerTypeIsSupported :: LanguageConstraints -> IntegerType -> Bool
integerTypeIsSupported constraints it = S.member it $ languageConstraintsIntegerTypes constraints

literalTypeIsSupported :: LanguageConstraints -> LiteralType -> Bool
literalTypeIsSupported constraints at = S.member (literalTypeVariant at) (languageConstraintsLiteralVariants constraints)
  && case at of
    LiteralTypeFloat ft -> floatTypeIsSupported constraints ft
    LiteralTypeInteger it -> integerTypeIsSupported constraints it
    _ -> True

nameToFilePath :: Bool -> FileExtension -> Name -> FilePath
nameToFilePath caps ext name = namespaceToFilePath caps ext $ Namespace $ prefix ++ local
  where
    QualifiedName ns local = qualifyNameEager name
    prefix = Y.maybe "" (\(Namespace gname) -> gname ++ "/") ns

typeIsSupported :: LanguageConstraints -> Type -> Bool
typeIsSupported constraints t = languageConstraintsTypes constraints base -- these are *additional* type constraints
  && isSupportedVariant (typeVariant base)
  && case base of
    TypeAnnotated (AnnotatedType t _) -> typeIsSupported constraints t
    TypeApplication (ApplicationType lhs rhs) -> typeIsSupported constraints lhs && typeIsSupported constraints rhs
    TypeLambda (LambdaType _ body) -> typeIsSupported constraints body
    TypeFunction (FunctionType dom cod) -> typeIsSupported constraints dom && typeIsSupported constraints cod
    TypeList lt -> typeIsSupported constraints lt
    TypeLiteral at -> literalTypeIsSupported constraints at
    TypeMap (MapType kt vt) -> typeIsSupported constraints kt && typeIsSupported constraints vt
    TypeOptional t -> typeIsSupported constraints t
    TypeProduct types -> and (typeIsSupported constraints <$> types)
    TypeRecord rt -> and (typeIsSupported constraints . fieldTypeType <$> rowTypeFields rt)
    TypeSet st -> typeIsSupported constraints st
    TypeSum types -> and (typeIsSupported constraints <$> types)
    TypeUnion rt -> and (typeIsSupported constraints . fieldTypeType <$> rowTypeFields rt)
    TypeWrap (WrappedType _ t) -> typeIsSupported constraints t
    TypeVariable _ -> True
  where
    isSupportedVariant v = v == TypeVariantVariable || S.member v (languageConstraintsTypeVariants constraints)
    base = stripType t

unidirectionalCoder :: (a -> Flow s b) -> Coder s s a b
unidirectionalCoder m = Coder {
  coderEncode = m,
  coderDecode = \_ -> fail "inbound mapping is unsupported"}
