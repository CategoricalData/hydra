module Hydra.Common where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import qualified Hydra.Lib.Strings as Strings
import Hydra.Util.Formatting

import qualified Data.List as L
import qualified Data.Set as S


newtype FileExtension = FileExtension String

debug :: Bool
debug = True

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

fromQname :: GraphName -> String -> Name
fromQname (GraphName ns) local = Name $ ns ++ "." ++ local

graphNameOf :: Name -> GraphName
graphNameOf = fst . toQname

graphNameToFilePath :: Bool -> FileExtension -> GraphName -> FilePath
graphNameToFilePath caps (FileExtension ext) (GraphName name) = L.intercalate "/" parts ++ "." ++ ext
  where
    parts = (if caps then capitalize else id) <$> Strings.splitOn "/" name

isEncodedType :: Eq m => Context m -> Term m -> Bool
isEncodedType cx term = termExpr cx term == TermElement _Type

isType :: Eq m => Context m -> Type m -> Bool
isType cx typ = typeExpr cx typ == TypeNominal _Type

localNameOf :: Name -> String
localNameOf = snd . toQname

failWithTrace cx msg = fail $ "Error (" ++ printTrace cx ++ "): " ++ msg

printTrace :: Context m -> String
printTrace = L.intercalate " > " . L.reverse . contextTrace

pushTrace :: String -> Context m -> Context m
pushTrace msg cx = if debug
  then cx {contextTrace = msg:contextTrace cx}
  else cx

toQname :: Name -> (GraphName, String)
toQname (Name name) = case Strings.splitOn "." name of
  (ns:rest) -> (GraphName ns, L.intercalate "." rest)
  _ -> (GraphName "UNKNOWN", name)

termExpr :: Context m -> Term m -> Term m
termExpr cx = annotationClassTermExpr $ contextAnnotations cx

termMeta :: Context m -> Term m -> m
termMeta cx = annotationClassTermMeta $ contextAnnotations cx

typeExpr :: Context m -> Type m -> Type m
typeExpr cx = annotationClassTypeExpr $ contextAnnotations cx

typeMeta :: Context m -> Type m -> m
typeMeta cx = annotationClassTypeMeta $ contextAnnotations cx
