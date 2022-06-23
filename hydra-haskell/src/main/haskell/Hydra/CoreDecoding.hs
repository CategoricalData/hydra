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
  decodeTypeLambda,
  ) where

import Hydra.Common
import Hydra.Core
import Hydra.Steps
import Hydra.Primitives
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Default

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M


decodeElement :: Term m -> Result Name
decodeElement term = case termExpr term of
  TermExprElement name -> pure name
  _ -> fail "expected an element"

decodeFieldType :: (Default m, Show m) => Context m -> Term m -> Result (FieldType m)
decodeFieldType cx = matchRecord cx $ \m -> FieldType
  <$> (FieldName <$> getField m _FieldType_name decodeString)
  <*> getField m _FieldType_type (decodeType cx)

decodeFieldTypes :: (Default m, Show m) => Context m -> Term m -> Result [FieldType m]
decodeFieldTypes cx term = case termExpr term of
  TermExprList els -> CM.mapM (decodeFieldType cx) els
  _ -> fail "expected a list"

decodeFloatType :: (Default m, Show m) => Context m -> Term m -> Result FloatType
decodeFloatType cx = matchEnum cx [
  (_FloatType_bigfloat, FloatTypeBigfloat),
  (_FloatType_float32, FloatTypeFloat32),
  (_FloatType_float64, FloatTypeFloat64)]

decodeFunctionType :: (Default m, Show m) => Context m -> Term m -> Result (FunctionType m)
decodeFunctionType cx = matchRecord cx $ \m -> FunctionType
  <$> getField m _FunctionType_domain (decodeType cx)
  <*> getField m _FunctionType_codomain (decodeType cx)

decodeIntegerType :: (Default m, Show m) => Context m -> Term m -> Result IntegerType
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

decodeLiteralType :: (Default m, Show m) => Context m -> Term m -> Result LiteralType
decodeLiteralType cx = matchUnion cx [
  matchUnitField _LiteralType_binary LiteralTypeBinary,
  matchUnitField _LiteralType_boolean LiteralTypeBoolean,
  (_LiteralType_float, fmap LiteralTypeFloat . decodeFloatType cx),
  (_LiteralType_integer, fmap LiteralTypeInteger . decodeIntegerType cx),
  matchUnitField _LiteralType_string LiteralTypeString]

decodeMapType :: (Default m, Show m) => Context m -> Term m -> Result (MapType m)
decodeMapType cx = matchRecord cx $ \m -> MapType
  <$> getField m _MapType_keys (decodeType cx)
  <*> getField m _MapType_values (decodeType cx)

decodeString :: Term m -> Result String
decodeString term = case termExpr term of
  TermExprLiteral av -> case av of
    LiteralString s -> pure s
    _ -> fail "expected a string value"
  _ -> fail "expected a literal value"

decodeType :: (Default m, Show m) => Context m -> Term m -> Result (Type m)
decodeType cx dat = case termExpr dat of
  TermExprElement name -> pure $ Types.nominal name
  _ -> (\t -> Type t (termMeta dat)) <$> matchUnion cx [
    (_TypeExpr_application, fmap TypeExprApplication . decodeTypeApplication cx),
    (_TypeExpr_element, fmap TypeExprElement . decodeType cx),
    (_TypeExpr_function, fmap TypeExprFunction . decodeFunctionType cx),
    (_TypeExpr_lambda, fmap TypeExprLambda . decodeTypeLambda cx),
    (_TypeExpr_list, fmap TypeExprList . decodeType cx),
    (_TypeExpr_literal, fmap TypeExprLiteral . decodeLiteralType cx),
    (_TypeExpr_map, fmap TypeExprMap . decodeMapType cx),
    (_TypeExpr_nominal, fmap TypeExprNominal . decodeElement),
    (_TypeExpr_optional, fmap TypeExprOptional . decodeType cx),
    (_TypeExpr_record, fmap TypeExprRecord . decodeFieldTypes cx),
    (_TypeExpr_set, fmap TypeExprSet . decodeType cx),
    (_TypeExpr_union, fmap TypeExprUnion . decodeFieldTypes cx),
    (_TypeExpr_variable, fmap (TypeExprVariable . TypeVariable) . decodeString)] dat

decodeTypeApplication :: (Default m, Show m) => Context m -> Term m -> Result (TypeApplication m)
decodeTypeApplication cx = matchRecord cx $ \m -> TypeApplication
  <$> getField m _TypeApplication_function (decodeType cx)
  <*> getField m _TypeApplication_argument (decodeType cx)
  
decodeTypeLambda :: (Default m, Show m) => Context m -> Term m -> Result (TypeLambda m)
decodeTypeLambda cx = matchRecord cx $ \m -> TypeLambda
  <$> (TypeVariable <$> getField m _TypeLambda_parameter decodeString)
  <*> getField m _TypeLambda_body (decodeType cx)

getField :: M.Map FieldName (Term m) -> FieldName -> (Term m -> Result b) -> Result b
getField m fname decode = case M.lookup fname m of
  Nothing -> fail $ "expected field " ++ show fname ++ " not found"
  Just val -> decode val

matchEnum :: (Default m, Show m) => Context m -> [(FieldName, b)] -> Term m -> Result b
matchEnum cx = matchUnion cx . fmap (uncurry matchUnitField)

matchRecord :: Context m -> (M.Map FieldName (Term m) -> Result b) -> Term m -> Result b
matchRecord cx decode term = do
  term' <- deref cx term
  case termExpr term' of
    TermExprRecord fields -> decode $ M.fromList $ fmap (\(Field fname val) -> (fname, val)) fields
    _ -> fail "expected a record"

matchUnion :: (Default m, Show m) => Context m -> [(FieldName, Term m -> Result b)] -> Term m -> Result b
matchUnion cx pairs term = do
    term' <- deref cx term
    case termExpr term' of
      TermExprUnion (Field fname val) -> case M.lookup fname mapping of
        Nothing -> fail $ "no matching case for field " ++ show fname
        Just f -> f val
      _ -> fail $ "expected a union with one of {" ++ L.intercalate ", " (unFieldName . fst <$> pairs) ++ "}"
        ++ ". Got: " ++ show term
  where
    mapping = M.fromList pairs

matchUnitField :: FieldName -> b -> (FieldName, a -> Result b)
matchUnitField fname x = (fname, \_ -> pure x)
