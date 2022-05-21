module Hydra.Impl.Haskell.Extras (
  Default(..),
  convertFloatValue,
  convertIntegerValue,
  debug,
  eitherToQualified,
  elementAsTypedData,
  fieldTypes,
  fromQname,
  graphNameOf,
  localNameOf,
  qualifiedToResult,
  requireType,
  resultToQualified,
  setContextElements,
  toQname,
  unexpected,
  unidirectionalStep,
  module Hydra.Common,
  module Hydra.Errors,
  module Hydra.Impl.Haskell.Default
  ) where

import Hydra.Common
import Hydra.Core
import Hydra.Impl.Haskell.Default
import Hydra.Errors
import Hydra.Graph
import Hydra.Primitives
import Hydra.Steps
import Hydra.CoreDecoding
import qualified Hydra.Lib.Strings as Strings

import qualified Data.List as L
import qualified Data.Map as M


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

debug :: String -> Result m -> Result m
debug msg r = case r of
  ResultSuccess _ -> r
  ResultFailure msg1 -> ResultFailure $ "failure[" ++ msg ++ "]: " ++ msg1

eitherToQualified :: Result m -> Qualified m
eitherToQualified e = case e of
  ResultFailure msg -> Qualified Nothing [msg]
  ResultSuccess x -> Qualified (Just x) []

elementAsTypedData :: (Default m, Show m) => Context m -> Element m -> Result (TypedData m)
elementAsTypedData schemaCtx el = TypedData <$> decodeType schemaCtx (elementSchema el) <*> pure (elementData el)

fieldTypes :: (Default m, Show m) => Context m -> Type m -> Result (M.Map FieldName (Type m))
fieldTypes scx t = case typeTerm t of
    TypeTermRecord fields -> pure $ toMap fields
    TypeTermUnion fields -> pure $ toMap fields
    TypeTermElement et -> fieldTypes scx et
    TypeTermNominal name -> do
      el <- requireElement scx name
      decodeType scx (elementData el) >>= fieldTypes scx
    TypeTermUniversal (UniversalType _ body) -> fieldTypes scx body
    _ -> fail $ "expected record or union type, but found " ++ show t
  where
    toMap fields = M.fromList (toPair <$> fields)
    toPair (FieldType fname ftype) = (fname, ftype)

fromQname :: GraphName -> String -> Name
fromQname (GraphName ns) local = Name $ ns ++ "." ++ local

graphNameOf :: Name -> GraphName
graphNameOf = fst . toQname

localNameOf :: Name -> String
localNameOf = snd . toQname

qualifiedToResult :: Qualified m -> Result m
qualifiedToResult (Qualified x m) = case x of
  Nothing -> fail $ L.head m
  Just x' -> pure x'

requireType :: (Default m, Show m) => Context m -> Name -> Result (Type m)
requireType scx name = do
  el <- requireElement scx name
  decodeType scx $ elementData el

resultToQualified :: Result m -> Qualified m
resultToQualified r = case r of
  ResultSuccess x -> pure x
  ResultFailure msg -> fail msg

setContextElements :: [Graph m] -> Context m -> Context m
setContextElements graphs cx = cx { contextElements = M.fromList $
  ((\e -> (elementName e, e)) <$> (L.concat (graphElements <$> graphs)))}

toQname :: Name -> (GraphName, String)
toQname (Name name) = case Strings.splitOn "." name of
  (ns:rest) -> (GraphName ns, L.intercalate "." rest)
  _ -> (GraphName "UNKNOWN", name)

unexpected :: (MonadFail m, Show a1) => String -> a1 -> m a2
unexpected cat obj = fail $ "unexpected " ++ cat ++ ": " ++ show obj

unidirectionalStep :: (a -> Result b) -> Step a b
unidirectionalStep m = Step {
  stepOut = m,
  stepIn = \_ -> fail "inbound mapping is unsupported"}
