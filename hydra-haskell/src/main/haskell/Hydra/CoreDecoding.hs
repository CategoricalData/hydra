module Hydra.CoreDecoding (
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
import Hydra.Steps
import Hydra.Primitives
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Default

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M


decodeElement :: Data m -> Result Name
decodeElement term = case dataTerm term of
  DataTermElement name -> pure name
  _ -> fail "expected an element"

decodeFieldType :: (Default m, Show m) => Context m -> Data m -> Result (FieldType m)
decodeFieldType cx = matchRecord cx $ \m -> FieldType
  <$> getField m _FieldType_name decodeString
  <*> getField m _FieldType_type (decodeType cx)

decodeFieldTypes :: (Default m, Show m) => Context m -> Data m -> Result [FieldType m]
decodeFieldTypes cx term = case dataTerm term of
  DataTermList els -> CM.mapM (decodeFieldType cx) els
  _ -> fail "expected a list"

decodeFloatType :: (Default m, Show m) => Context m -> Data m -> Result FloatType
decodeFloatType cx = matchEnum cx [
  (_FloatType_bigfloat, FloatTypeBigfloat),
  (_FloatType_float32, FloatTypeFloat32),
  (_FloatType_float64, FloatTypeFloat64)]

decodeFunctionType :: (Default m, Show m) => Context m -> Data m -> Result (FunctionType m)
decodeFunctionType cx = matchRecord cx $ \m -> FunctionType
  <$> getField m _FunctionType_domain (decodeType cx)
  <*> getField m _FunctionType_codomain (decodeType cx)

decodeIntegerType :: (Default m, Show m) => Context m -> Data m -> Result IntegerType
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

decodeLiteralType :: (Default m, Show m) => Context m -> Data m -> Result LiteralType
decodeLiteralType cx = matchUnion cx [
  matchUnitField _LiteralType_binary LiteralTypeBinary,
  matchUnitField _LiteralType_boolean LiteralTypeBoolean,
  (_LiteralType_float, fmap LiteralTypeFloat . decodeFloatType cx),
  (_LiteralType_integer, fmap LiteralTypeInteger . decodeIntegerType cx),
  matchUnitField _LiteralType_string LiteralTypeString]

decodeMapType :: (Default m, Show m) => Context m -> Data m -> Result (MapType m)
decodeMapType cx = matchRecord cx $ \m -> MapType
  <$> getField m _MapType_keys (decodeType cx)
  <*> getField m _MapType_values (decodeType cx)

decodeString :: Data m -> Result String
decodeString term = case dataTerm term of
  DataTermLiteral av -> case av of
    LiteralString s -> pure s
    _ -> fail "expected a string value"
  _ -> fail "expected a literal value"

decodeType :: (Default m, Show m) => Context m -> Data m -> Result (Type m)
decodeType cx dat = case dataTerm dat of
  DataTermElement name -> pure $ Types.nominal name
  _ -> (\t -> Type t (dataMeta dat)) <$> matchUnion cx [
    (_TypeTerm_literal, fmap TypeTermLiteral . decodeLiteralType cx),
    (_TypeTerm_element, fmap TypeTermElement . decodeType cx),
    (_TypeTerm_function, fmap TypeTermFunction . decodeFunctionType cx),
    (_TypeTerm_list, fmap TypeTermList . decodeType cx),
    (_TypeTerm_map, fmap TypeTermMap . decodeMapType cx),
    (_TypeTerm_nominal, fmap TypeTermNominal . decodeElement),
    (_TypeTerm_optional, fmap TypeTermOptional . decodeType cx),
    (_TypeTerm_record, fmap TypeTermRecord . decodeFieldTypes cx),
    (_TypeTerm_set, fmap TypeTermSet . decodeType cx),
    (_TypeTerm_union, fmap TypeTermUnion . decodeFieldTypes cx),
    (_TypeTerm_universal, fmap TypeTermUniversal . decodeUniversalType cx),
    (_TypeTerm_variable, fmap (TypeTermVariable . TypeVariable) . decodeString)] dat

decodeUniversalType :: (Default m, Show m) => Context m -> Data m -> Result (UniversalType m)
decodeUniversalType cx = matchRecord cx $ \m -> UniversalType
  <$> (TypeVariable <$> getField m _UniversalType_variable decodeString)
  <*> getField m _UniversalType_body (decodeType cx)

getField :: M.Map FieldName (Data m) -> FieldName -> (Data m -> Result b) -> Result b
getField m fname decode = case M.lookup fname m of
  Nothing -> fail $ "expected field " ++ show fname ++ " not found"
  Just val -> decode val

matchEnum :: (Default m, Show m) => Context m -> [(FieldName, b)] -> Data m -> Result b
matchEnum cx = matchUnion cx . fmap (uncurry matchUnitField)

matchRecord :: Context m -> (M.Map FieldName (Data m) -> Result b) -> Data m -> Result b
matchRecord cx decode term = do
  term' <- deref cx term
  case dataTerm term' of
    DataTermRecord fields -> decode $ M.fromList $ fmap (\(Field fname val) -> (fname, val)) fields
    _ -> fail "expected a record"

matchUnion :: (Default m, Show m) => Context m -> [(FieldName, Data m -> Result b)] -> Data m -> Result b
matchUnion cx pairs term = do
    term' <- deref cx term
    case dataTerm term' of
      DataTermUnion (Field fname val) -> case M.lookup fname mapping of
        Nothing -> fail $ "no matching case for field " ++ show fname
        Just f -> f val
      _ -> fail $ "expected a union with one of {" ++ L.intercalate ", " (fst <$> pairs) ++ "}"
        ++ ". Got: " ++ show term
  where
    mapping = M.fromList pairs

matchUnitField :: FieldName -> b -> (FieldName, a -> Result b)
matchUnitField fname x = (fname, \_ -> pure x)
