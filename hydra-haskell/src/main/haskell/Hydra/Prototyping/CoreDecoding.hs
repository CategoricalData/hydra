module Hydra.Prototyping.CoreDecoding (
  decodeLiteralType,
  decodeFieldType,
  decodeFieldTypes,
  decodeFloatType,
  decodeFunctionType,
  decodeIntegerType,
  decodeMapType,
  decodeString,
  decodeType,
  decodeUniversalType,
  ) where

import Hydra.Core
import Hydra.Prototyping.Steps
import Hydra.Prototyping.Primitives

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M


decodeElement :: Term m -> Result Name
decodeElement term = case termData term of
  ExpressionElement name -> pure name
  _ -> fail "expected an element"

decodeFieldType :: Show m => Context m -> Term m -> Result FieldType
decodeFieldType cx = matchRecord cx $ \m -> FieldType
  <$> getField m _FieldType_name decodeString
  <*> getField m _FieldType_type (decodeType cx)

decodeFieldTypes :: Show m => Context m -> Term m -> Result [FieldType]
decodeFieldTypes cx term = case termData term of
  ExpressionList els -> CM.mapM (decodeFieldType cx) els
  _ -> fail "expected a list"

decodeFloatType :: Show m => Context m -> Term m -> Result FloatType
decodeFloatType cx = matchEnum cx [
  (_FloatType_bigfloat, FloatTypeBigfloat),
  (_FloatType_float32, FloatTypeFloat32),
  (_FloatType_float64, FloatTypeFloat64)]

decodeFunctionType :: Show m => Context m -> Term m -> Result FunctionType
decodeFunctionType cx = matchRecord cx $ \m -> FunctionType
  <$> getField m _FunctionType_domain (decodeType cx)
  <*> getField m _FunctionType_codomain (decodeType cx)

decodeIntegerType :: Show m => Context m -> Term m -> Result IntegerType
decodeIntegerType cx = matchEnum cx [
  (_IntegerType_bigint, IntegerTypeBigint),
  (_IntegerType_int8, IntegerTypeInt8),
  (_IntegerType_int16, IntegerTypeInt16),
  (_IntegerType_int32, IntegerTypeInt32),
  (_IntegerType_int64, IntegerTypeInt64),
  (_IntegerType_uint8, IntegerTypeUint8),
  (_IntegerType_uint16, IntegerTypeUint16),
  (_IntegerType_uint32, IntegerTypeUint32),
  (_IntegerType_uint64, IntegerTypeUint64)]

decodeLiteralType :: Show m => Context m -> Term m -> Result LiteralType
decodeLiteralType cx = matchUnion cx [
  matchUnitField _LiteralType_binary LiteralTypeBinary,
  matchUnitField _LiteralType_boolean LiteralTypeBoolean,
  (_LiteralType_float, fmap LiteralTypeFloat . decodeFloatType cx),
  (_LiteralType_integer, fmap LiteralTypeInteger . decodeIntegerType cx),
  matchUnitField _LiteralType_string LiteralTypeString]

decodeMapType :: Show m => Context m -> Term m -> Result MapType
decodeMapType cx = matchRecord cx $ \m -> MapType
  <$> getField m _MapType_keys (decodeType cx)
  <*> getField m _MapType_values (decodeType cx)

decodeString :: Term m -> Result String
decodeString term = case termData term of
  ExpressionLiteral av -> case av of
    LiteralString s -> pure s
    _ -> fail "expected a string value"
  _ -> fail "expected a literal value"

decodeType :: Show m => Context m -> Term m -> Result Type
decodeType cx term = case termData term of
  ExpressionElement name -> pure $ TypeNominal name
  _ -> matchUnion cx [
    (_Type_literal, fmap TypeLiteral . decodeLiteralType cx),
    (_Type_element, fmap TypeElement . decodeType cx),
    (_Type_function, fmap TypeFunction . decodeFunctionType cx),
    (_Type_list, fmap TypeList . decodeType cx),
    (_Type_map, fmap TypeMap . decodeMapType cx),
    (_Type_nominal, fmap TypeNominal . decodeElement),
    (_Type_optional, fmap TypeOptional . decodeType cx),
    (_Type_record, fmap TypeRecord . decodeFieldTypes cx),
    (_Type_set, fmap TypeSet . decodeType cx),
    (_Type_union, fmap TypeUnion . decodeFieldTypes cx),
    (_Type_universal, fmap TypeUniversal . decodeUniversalType cx),
    (_Type_variable, fmap TypeVariable . decodeString)] term

decodeUniversalType :: Show m => Context m -> Term m -> Result UniversalType
decodeUniversalType cx = matchRecord cx $ \m -> UniversalType
  <$> getField m _UniversalType_variable decodeString
  <*> getField m _UniversalType_body (decodeType cx)

getField :: M.Map FieldName (Term m) -> FieldName -> (Term m -> Result b) -> Result b
getField m fname decode = case M.lookup fname m of
  Nothing -> fail $ "expected field " ++ show fname ++ " not found"
  Just val -> decode val

matchEnum :: Show m => Context m -> [(FieldName, b)] -> Term m -> Result b
matchEnum cx = matchUnion cx . fmap (uncurry matchUnitField)

matchRecord :: Context m -> (M.Map FieldName (Term m) -> Result b) -> Term m -> Result b
matchRecord cx decode term = do
  term' <- deref cx term
  case termData term' of
    ExpressionRecord fields -> decode $ M.fromList $ fmap (\(Field fname val) -> (fname, val)) fields
    _ -> fail "expected a record"

matchUnion :: Show m => Context m -> [(FieldName, Term m -> Result b)] -> Term m -> Result b
matchUnion cx pairs term = do
    term' <- deref cx term
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
