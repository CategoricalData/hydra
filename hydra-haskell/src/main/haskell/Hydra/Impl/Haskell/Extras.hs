module Hydra.Impl.Haskell.Extras (
  Default(..),
  convertFloatValue,
  convertIntegerValue,
  eitherToQualified,
  elementAsTypedTerm,
  localNameOf,
  qualifiedToResult,
  requireType,
  resultToQualified,
  toQname,
  unidirectionalStep,
  module Hydra.Errors
  ) where

import Hydra.Core
import Hydra.Errors
import Hydra.Graph
import Hydra.Prototyping.Primitives
import Hydra.Prototyping.Steps
import Hydra.Prototyping.CoreDecoding

import qualified Data.List as L
import qualified Data.List.Split as LS


class Default a where dflt :: a
instance Default () where dflt = ()
instance Default Meta where dflt = Meta {
  metaDescription = Nothing,
  metaType = Nothing}

instance Functor Qualified where
  fmap f (Qualified x msgs) = Qualified (fmap f x) msgs
instance Applicative Qualified where
  pure x = Qualified (Just x) []
  Qualified f mf <*> Qualified x mx = Qualified (f <*> x) $ mf <> mx
instance Monad Qualified where
  Qualified x m >>= f = case x of
    Nothing -> Qualified Nothing m
    Just x' -> Qualified fx $ m2 <> m
      where Qualified fx m2 = f x'
instance MonadFail Qualified where
  fail msg = Qualified Nothing [msg]

convertFloatValue :: FloatType -> FloatValue -> FloatValue
convertFloatValue target = encoder . decoder
  where
    decoder fv = case fv of
      FloatValueBigfloat d -> d
      FloatValueFloat32 f -> realToFrac f
      FloatValueFloat64 d -> d
    encoder d = case target of
      FloatTypeBigfloat -> FloatValueBigfloat d
      FloatTypeFloat32 -> FloatValueFloat32 $ realToFrac d
      FloatTypeFloat64 -> FloatValueFloat64 d

convertIntegerValue :: IntegerType -> IntegerValue -> IntegerValue
convertIntegerValue target = encoder . decoder
  where
    decoder iv = case iv of
      IntegerValueBigint v -> v
      IntegerValueInt8 v -> fromIntegral v
      IntegerValueInt16 v -> fromIntegral v
      IntegerValueInt32 v -> fromIntegral v
      IntegerValueInt64 v -> fromIntegral v
      IntegerValueUint8 v -> fromIntegral v
      IntegerValueUint16 v -> fromIntegral v
      IntegerValueUint32 v -> fromIntegral v
      IntegerValueUint64 v -> fromIntegral v
    encoder d = case target of
      IntegerTypeBigint -> IntegerValueBigint d
      IntegerTypeInt8 -> IntegerValueInt8 $ fromIntegral d
      IntegerTypeInt16 -> IntegerValueInt16 $ fromIntegral d
      IntegerTypeInt32 -> IntegerValueInt32 $ fromIntegral d
      IntegerTypeInt64 -> IntegerValueInt64 $ fromIntegral d
      IntegerTypeUint8 -> IntegerValueUint8 $ fromIntegral d
      IntegerTypeUint16 -> IntegerValueUint16 $ fromIntegral d
      IntegerTypeUint32 -> IntegerValueUint32 $ fromIntegral d
      IntegerTypeUint64 -> IntegerValueUint64 $ fromIntegral d

elementAsTypedTerm :: Show a => Context a -> Element a -> Result (TypedTerm a)
elementAsTypedTerm schemaCtx el = TypedTerm <$> decodeType schemaCtx (elementSchema el) <*> pure (elementData el)

localNameOf :: Name -> String
localNameOf = snd . toQname

eitherToQualified :: Result a -> Qualified a
eitherToQualified e = case e of
  ResultFailure msg -> Qualified Nothing [msg]
  ResultSuccess x -> Qualified (Just x) []

qualifiedToResult :: Qualified a -> Result a
qualifiedToResult (Qualified x m) = case x of
  Nothing -> fail $ L.head m
  Just x' -> pure x'

requireType :: Show a => Context a -> Name -> Result Type
requireType scx name = do
  el <- requireElement scx name
  decodeType scx $ elementData el

resultToQualified :: Result a -> Qualified a
resultToQualified r = case r of
  ResultSuccess x -> pure x
  ResultFailure msg -> fail msg

toQname :: Name -> (String, String)
toQname name = (ns, local)
  where
    [ns, local] = LS.splitOn "." name

unidirectionalStep :: (a -> Result b) -> Step a b
unidirectionalStep m = Step {
  stepOut = m,
  stepIn = \_ -> fail "inbound mapping is unsupported"}
