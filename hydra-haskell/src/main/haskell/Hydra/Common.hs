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

fieldMap :: [Field a] -> M.Map FieldName (Term a)
fieldMap fields = M.fromList (toPair <$> fields)
  where
    toPair f = (fieldName f, fieldTerm f)

fieldTypeMap :: [FieldType a] -> M.Map FieldName (Type a)
fieldTypeMap fields = M.fromList (toPair <$> fields)
  where
    toPair f = (fieldTypeName f, fieldTypeType f)

namespaceToFilePath :: Bool -> FileExtension -> Namespace -> FilePath
namespaceToFilePath caps (FileExtension ext) (Namespace name) = L.intercalate "/" parts ++ "." ++ ext
  where
    parts = (if caps then capitalize else id) <$> Strings.splitOn "/" name

isType :: Eq a => Type a -> Bool
isType typ = case stripType typ of
  TypeApplication (ApplicationType lhs _) -> isType lhs
  TypeLambda (LambdaType _ body) -> isType body
  TypeUnion (RowType _Type _ _) -> True
  TypeVariable _Type -> True
  TypeWrap _Type -> True
  _ -> False

isUnitTerm :: Eq a => Term a -> Bool
isUnitTerm t = stripTerm t == TermRecord (Record _UnitType [])

isUnitType :: Eq a => Type a -> Bool
isUnitType t = stripType t == TypeRecord (RowType _UnitType Nothing [])

localNameOfLazy :: Name -> String
localNameOfLazy = qualifiedNameLocal . qualifyNameLazy

localNameOfEager :: Name -> String
localNameOfEager = qualifiedNameLocal . qualifyNameEager

namespaceOfLazy :: Name -> Maybe Namespace
namespaceOfLazy = qualifiedNameNamespace . qualifyNameLazy

namespaceOfEager :: Name -> Maybe Namespace
namespaceOfEager = qualifiedNameNamespace . qualifyNameEager

placeholderName :: Name
placeholderName = Name "Placeholder"

-- | Splits a Name on the last "." into a namespace and a local part
qualifyNameLazy :: Name -> QualifiedName
qualifyNameLazy (Name name) = case L.reverse $ Strings.splitOn "." name of
  (local:rest) -> QualifiedName (Just $ Namespace $ L.intercalate "." $ L.reverse rest) local
  _ -> QualifiedName Nothing name

-- | Splits a Name on the first "." into a namespace and a local part
qualifyNameEager :: Name -> QualifiedName
qualifyNameEager (Name name) = case Strings.splitOn "." name of
  [n] -> QualifiedName Nothing name
  (ns:rest) -> QualifiedName (Just $ Namespace ns) (L.intercalate "." rest)

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

unqualifyName :: QualifiedName -> Name
unqualifyName (QualifiedName ns local) = Name $ prefix ++ local
  where
    prefix = case ns of
      Nothing -> ""
      Just n -> unNamespace n ++ "."
