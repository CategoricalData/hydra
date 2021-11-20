module Hydra.Prototyping.CoreDecoding (
  decodeAbstractType,
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
import Hydra.Prototyping.Steps
import Hydra.Prototyping.Primitives

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M


decodeAbstractType :: Show a => Context a -> Term a -> Result AbstractType
decodeAbstractType context = matchRecord context $ \m -> AbstractType
  <$> getField m _AbstractType_variable decodeString
  <*> getField m _AbstractType_body (decodeType context)

decodeAtomicType :: Show a => Context a -> Term a -> Result AtomicType
decodeAtomicType context = matchUnion context [
  matchUnitField _AtomicType_binary AtomicTypeBinary,
  matchUnitField _AtomicType_boolean AtomicTypeBoolean,
  (_AtomicType_float, fmap AtomicTypeFloat . decodeFloatType context),
  (_AtomicType_integer, fmap AtomicTypeInteger . decodeIntegerType context),
  matchUnitField _AtomicType_string AtomicTypeString]

decodeFieldType :: Show a => Context a -> Term a -> Result FieldType
decodeFieldType context = matchRecord context $ \m -> FieldType
  <$> getField m _FieldType_name decodeString
  <*> getField m _FieldType_type (decodeType context)

decodeFieldTypes :: Show a => Context a -> Term a -> Result [FieldType]
decodeFieldTypes context term = case termData term of
  ExpressionList els -> CM.mapM (decodeFieldType context) els
  _ -> fail "expected a list"

decodeFloatType :: Show a => Context a -> Term a -> Result FloatType
decodeFloatType context = matchEnum context [
  (_FloatType_bigfloat, FloatTypeBigfloat),
  (_FloatType_float32, FloatTypeFloat32),
  (_FloatType_float64, FloatTypeFloat64)]

decodeFunctionType :: Show a => Context a -> Term a -> Result FunctionType
decodeFunctionType context = matchRecord context $ \m -> FunctionType
  <$> getField m _FunctionType_domain (decodeType context)
  <*> getField m _FunctionType_codomain (decodeType context)

decodeIntegerType :: Show a => Context a -> Term a -> Result IntegerType
decodeIntegerType context = matchEnum context [
  (_IntegerType_bigint, IntegerTypeBigint),
  (_IntegerType_int8, IntegerTypeInt8),
  (_IntegerType_int16, IntegerTypeInt16),
  (_IntegerType_int32, IntegerTypeInt32),
  (_IntegerType_int64, IntegerTypeInt64),
  (_IntegerType_uint8, IntegerTypeUint8),
  (_IntegerType_uint16, IntegerTypeUint16),
  (_IntegerType_uint32, IntegerTypeUint32),
  (_IntegerType_uint64, IntegerTypeUint64)]

decodeMapType :: Show a => Context a -> Term a -> Result MapType
decodeMapType context = matchRecord context $ \m -> MapType
  <$> getField m _MapType_keys (decodeType context)
  <*> getField m _MapType_values (decodeType context)

decodeString :: Term a -> Result String
decodeString term = case termData term of
  ExpressionAtomic av -> case av of
    AtomicValueString s -> pure s
    _ -> fail "expected a string value"
  _ -> fail "expected an atomic value"

decodeType :: Show a => Context a -> Term a -> Result Type
decodeType context = matchUnion context [
    (_Type_abstract, fmap TypeAbstract . decodeAbstractType context),
    (_Type_atomic, fmap TypeAtomic . decodeAtomicType context),
    (_Type_element, fmap TypeElement . decodeType context),
    (_Type_function, fmap TypeFunction . decodeFunctionType context),
    (_Type_list, fmap TypeList . decodeType context),
    (_Type_map, fmap TypeMap . decodeMapType context),
    (_Type_nominal, fmap TypeNominal . decodeString),
    (_Type_optional, fmap TypeOptional . decodeType context),
    (_Type_record, fmap TypeRecord . decodeFieldTypes context),
    (_Type_set, fmap TypeSet . decodeType context),
    (_Type_union, fmap TypeUnion . decodeFieldTypes context),
    (_Type_variable, fmap TypeVariable . decodeString)]

deref :: Context a -> Term a -> Result (Term a)
deref context term = case termData term of
  ExpressionElement name -> dereferenceElement context name >>= deref context
  _ -> pure term

getField :: M.Map FieldName (Term a) -> FieldName -> (Term a -> Result b) -> Result b
getField m fname decode = case M.lookup fname m of
  Nothing -> fail $ "expected field " ++ show fname ++ " not found"
  Just val -> decode val

matchEnum :: Show a => Context a -> [(FieldName, b)] -> Term a -> Result b
matchEnum context = matchUnion context . fmap (uncurry matchUnitField)

matchRecord :: Context a -> (M.Map FieldName (Term a) -> Result b) -> Term a -> Result b
matchRecord context decode term = do
  term' <- deref context term
  case termData term' of
    ExpressionRecord fields -> decode $ M.fromList $ fmap (\(Field fname val) -> (fname, val)) fields
    _ -> fail "expected a record"

matchUnion :: Show a => Context a -> [(FieldName, Term a -> Result b)] -> Term a -> Result b
matchUnion context pairs term = do
    term' <- deref context term
    case termData term' of
      ExpressionUnion (Field fname val) -> case M.lookup fname mapping of
        Nothing -> fail $ "no matching case for field " ++ show fname
        Just f -> f val
      _ -> fail $ "expected a union with one of {" ++ L.intercalate ", " (fst <$> pairs) ++ "}"
        ++ ". Got: " ++ show term
  where
    mapping = M.fromList pairs

matchUnitField :: FieldName -> b -> (FieldName, a -> Result b)
matchUnitField fname x = (fname, \_ -> pure x)
