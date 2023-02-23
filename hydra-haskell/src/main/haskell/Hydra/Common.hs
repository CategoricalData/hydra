-- | Common functions for working with terms, types, and names

module Hydra.Common where

import Hydra.Core
import Hydra.Compute
import Hydra.Mantle
import Hydra.Module
import qualified Hydra.Lib.Strings as Strings
import Hydra.Util.Formatting

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


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

elementsToGraph :: Maybe (Graph m) -> [Element m] -> Graph m
elementsToGraph msg els = Graph elementMap msg
  where
    elementMap = M.fromList (toPair <$> els)
      where
        toPair el = (elementName el, el)

fromQname :: Namespace -> String -> Name
fromQname ns local = Name $ unNamespace ns ++ "." ++ local

namespaceToFilePath :: Bool -> FileExtension -> Namespace -> FilePath
namespaceToFilePath caps (FileExtension ext) (Namespace name) = L.intercalate "/" parts ++ "." ++ ext
  where
    parts = (if caps then capitalize else id) <$> Strings.splitOn "/" name

isEncodedType :: Eq m => Context m -> Term m -> Bool
isEncodedType cx term = stripTerm term == TermElement _Type

isType :: Eq m => Context m -> Type m -> Bool
isType cx typ = case stripType typ of
  TypeWrapped _Type -> True
  TypeUnion (RowType _Type _ _) -> True
  TypeApplication (ApplicationType lhs _) -> isType cx lhs
  _ -> False

localNameOfLazy :: Name -> String
localNameOfLazy = snd . toQnameLazy

localNameOfEager :: Name -> String
localNameOfEager = snd . toQnameEager

namespaceOfLazy :: Name -> Namespace
namespaceOfLazy = fst . toQnameLazy

namespaceOfEager :: Name -> Namespace
namespaceOfEager = fst . toQnameEager

placeholderName :: Name
placeholderName = Name "Placeholder"

skipAnnotations :: (a -> Maybe (Annotated a m)) -> a -> a
skipAnnotations getAnn t = skip t
  where
    skip t = case getAnn t of
      Nothing -> t
      Just (Annotated t' _) -> skip t'

stripTerm :: Term m -> Term m
stripTerm = skipAnnotations $ \t -> case t of
  TermAnnotated a -> Just a
  _ -> Nothing

stripType :: Type m -> Type m
stripType = skipAnnotations $ \t -> case t of
  TypeAnnotated a -> Just a
  _ -> Nothing

termMeta :: Context m -> Term m -> m
termMeta cx = annotationClassTermAnnotation $ contextAnnotations cx

toQnameLazy :: Name -> (Namespace, String)
toQnameLazy (Name name) = case L.reverse $ Strings.splitOn "." name of
  (local:rest) -> (Namespace $ L.intercalate "." $ L.reverse rest, local)
  _ -> (Namespace "UNKNOWN", name)

toQnameEager :: Name -> (Namespace, String)
toQnameEager (Name name) = case Strings.splitOn "." name of
  (ns:rest) -> (Namespace ns, L.intercalate "." rest)
  _ -> (Namespace "UNKNOWN", name)

typeMeta :: Context m -> Type m -> m
typeMeta cx = annotationClassTypeAnnotation $ contextAnnotations cx

unitTypeName :: Name
unitTypeName = Name "hydra/core.UnitType"
