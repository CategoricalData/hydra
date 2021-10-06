module Hydra.Prototyping.CoreDecoding (
  decodeAtomicType,
  decodeFieldType,
  decodeFieldTypes,
  decodeFloatType,
  decodeFunctionType,
  decodeIntegerType,
  decodeMapType,
  decodeString,
  decodeType,
  ) where

import Hydra.Core

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M


decodeAtomicType :: Term -> Either String AtomicType
decodeAtomicType = matchUnion [
  matchUnitField _AtomicType_binary AtomicTypeBinary,
  matchUnitField _AtomicType_boolean AtomicTypeBoolean,
  (_AtomicType_float, fmap AtomicTypeFloat . decodeFloatType),
  (_AtomicType_integer, fmap AtomicTypeInteger . decodeIntegerType),
  matchUnitField _AtomicType_string AtomicTypeString]

decodeFieldType :: Term -> Either String FieldType
decodeFieldType = matchRecord $ \m -> FieldType
  <$> getField m _FieldType_name decodeString
  <*> getField m _FieldType_type decodeType

decodeFieldTypes :: Term -> Either String [FieldType]
decodeFieldTypes term = case term of
  TermList els -> CM.mapM decodeFieldType els
  _ -> Left "expected a list"

decodeFloatType :: Term -> Either String FloatType
decodeFloatType = matchEnum [
  (_FloatType_bigfloat, FloatTypeBigfloat),
  (_FloatType_float32, FloatTypeFloat32),
  (_FloatType_float64, FloatTypeFloat64)]

decodeFunctionType :: Term -> Either String FunctionType
decodeFunctionType = matchRecord $ \m -> FunctionType
  <$> getField m _FunctionType_domain decodeType
  <*> getField m _FunctionType_codomain decodeType

decodeIntegerType :: Term -> Either String IntegerType
decodeIntegerType = matchEnum [
  (_IntegerType_bigint, IntegerTypeBigint),
  (_IntegerType_int8, IntegerTypeInt8),
  (_IntegerType_int16, IntegerTypeInt16),
  (_IntegerType_int32, IntegerTypeInt32),
  (_IntegerType_int64, IntegerTypeInt64),
  (_IntegerType_uint8, IntegerTypeUint8),
  (_IntegerType_uint16, IntegerTypeUint16),
  (_IntegerType_uint32, IntegerTypeUint32),
  (_IntegerType_uint64, IntegerTypeUint64)]

decodeMapType :: Term -> Either String MapType
decodeMapType = matchRecord $ \m -> MapType
  <$> getField m _MapType_keys decodeType
  <*> getField m _MapType_values decodeType

decodeString :: Term -> Either String String
decodeString term = case term of
  TermAtomic av -> case av of
    AtomicValueString s -> pure s
    _ -> Left "expected a string value"
  _ -> Left "expected an atomic value"

decodeType :: Term -> Either String Type
decodeType = matchUnion [
    (_Type_atomic, fmap TypeAtomic . decodeAtomicType),
    (_Type_element, fmap TypeElement . decodeType),
    (_Type_function, fmap TypeFunction . decodeFunctionType),
    (_Type_list, fmap TypeList . decodeType),
    (_Type_map, fmap TypeMap . decodeMapType), 
    (_Type_nominal, fmap TypeNominal . decodeString), 
    (_Type_record, fmap TypeRecord . decodeFieldTypes),
    (_Type_set, fmap TypeSet . decodeType),
    (_Type_union, fmap TypeUnion . decodeFieldTypes)]

getField :: M.Map FieldName Term -> FieldName -> (Term -> Either String a) -> Either String a
getField m fname decode = case M.lookup fname m of
  Nothing -> Left $ "expected field " ++ show fname ++ " not found"
  Just val -> decode val

matchEnum :: [(FieldName, a)] -> Term -> Either String a
matchEnum = matchUnion . fmap (uncurry matchUnitField)

matchRecord :: (M.Map FieldName Term -> Either String a) -> Term -> Either String a
matchRecord decode term = case term of
  TermRecord fields -> decode $ M.fromList $ fmap (\(Field fname val) -> (fname, val)) fields
  _ -> Left "expected a record"

matchUnion :: [(FieldName, Term -> Either String a)] -> Term -> Either String a
matchUnion pairs term = case term of
    TermUnion (Field fname val) -> case M.lookup fname mapping of
      Nothing -> Left $ "no matching case for field " ++ show fname
      Just f -> f val
    _ -> Left $ "expected a union with one of {" ++ L.intercalate ", " (fst <$> pairs) ++ "}"
      ++ ". Got: " ++ show term
  where
    mapping = M.fromList pairs

matchUnitField :: FieldName -> b -> (FieldName, a -> Either String b)
matchUnitField fname x = (fname, \_ -> pure x)
