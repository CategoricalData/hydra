module Hydra.Common where

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Default
import qualified Hydra.Lib.Strings as Strings

import qualified Data.List as L


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

isEncodedType :: Eq m => Term m -> Bool
isEncodedType term = termExpr term == TermElement _Type

isType :: Eq m => Type m -> Bool
isType typ = typeExpr typ == TypeNominal _Type

localNameOf :: Name -> String
localNameOf = snd . toQname

toQname :: Name -> (GraphName, String)
toQname (Name name) = case Strings.splitOn "." name of
  (ns:rest) -> (GraphName ns, L.intercalate "." rest)
  _ -> (GraphName "UNKNOWN", name)

termExpr :: Term m -> Term m
termExpr t = case t of
  TermAnnotated (Annotated t' _) -> termExpr t'
  _ -> t

termMeta :: Default m => Term m -> m
termMeta t = case t of
  TermAnnotated a -> annotatedAnnotation a
  _ -> dflt

typeExpr :: Type m -> Type m
typeExpr t = case t of
  TypeAnnotated (Annotated t' _) -> typeExpr t'
  _ -> t

typeMeta :: Default m => Type m -> m
typeMeta t = case t of
  TypeAnnotated a -> annotatedAnnotation a
  _ -> dflt
