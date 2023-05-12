-- | Common functions for working with terms, types, and names

module Hydra.Common where

import Hydra.Basics
import Hydra.Core
import Hydra.Compute
import Hydra.Flows
import Hydra.Graph
import Hydra.Module
import Hydra.Tools.Formatting
import qualified Hydra.Lib.Strings as Strings

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

elementsToGraph :: Graph a -> Maybe (Graph a) -> [Element a] -> Graph a
elementsToGraph parent schema els = parent {graphElements = elementMap, graphSchema = schema}
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

isEncodedType :: Eq a => Graph a -> Term a -> Bool
isEncodedType cx term = stripTerm term == TermElement _Type

isType :: Eq a => Type a -> Bool
isType typ = case stripType typ of
  TypeWrap _Type -> True
  TypeUnion (RowType _Type _ _) -> True
  TypeApplication (ApplicationType lhs _) -> isType lhs
  _ -> False

isUnitTerm :: Eq a => Term a -> Bool
isUnitTerm t = stripTerm t == TermRecord (Record _UnitType [])

isUnitType :: Eq a => Type a -> Bool
isUnitType t = stripType t == TypeRecord (RowType _UnitType Nothing [])

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

requireTypeAnnotation :: Show a => Term a -> Flow (Graph a) (Type a)
requireTypeAnnotation term = do
  anns <- graphAnnotations <$> getState
  mt <- annotationClassTermType anns term
  case mt of
    Nothing -> fail $ "missing type annotation" ++ " in " ++ show term
    Just t -> pure t

stripTerm :: Term a -> Term a
stripTerm = skipAnnotations $ \t -> case t of
  TermAnnotated a -> Just a
  _ -> Nothing

stripType :: Type a -> Type a
stripType = skipAnnotations $ \t -> case t of
  TypeAnnotated a -> Just a
  _ -> Nothing

-- | Splits a Name on the last "." into a namespace and a local part
toQnameLazy :: Name -> (Namespace, String)
toQnameLazy (Name name) = case L.reverse $ Strings.splitOn "." name of
  (local:rest) -> (Namespace $ L.intercalate "." $ L.reverse rest, local)
  _ -> (Namespace "UNKNOWN", name)

-- | Splits a Name on the first "." into a namespace and a local part
toQnameEager :: Name -> (Namespace, String)
toQnameEager (Name name) = case Strings.splitOn "." name of
  (ns:rest) -> (Namespace ns, L.intercalate "." rest)
  _ -> (Namespace "UNKNOWN", name)
