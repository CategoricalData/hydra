module Hydra.CoreEncoding (
    encodeApplication,
    encodeElimination,
    encodeLiteralType,
    encodeLiteral,
    encodeLiteralVariant,
    encodeField,
    encodeFieldType,
    encodeFloatType,
    encodeFunction,
    encodeFunctionType,
    encodeIntegerType,
    encodeLambda,
    encodeData,
    encodeType,
    encodeTypeVariant,
  ) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Impl.Haskell.Extras

import Prelude hiding (map)
import qualified Data.Map as M
import qualified Data.Set as S


encodeApplication :: (Default m, Ord m) => Context m -> Application m -> Data m
encodeApplication cx (Application f a) = nominalRecord cx _Application [
  Field _Application_function $ encodeData cx f,
  Field _Application_argument $ encodeData cx f]

encodeElimination :: (Default m, Ord m) => Context m -> Elimination m -> Data m
encodeElimination cx e = case e of
  EliminationElement -> unitVariant _Elimination_element
  EliminationNominal (Name name) -> variant _Elimination_nominal $ stringValue name
  EliminationOptional cases -> variant _Elimination_optional $ encodeOptionalCases cx cases
  EliminationRecord (FieldName fname) -> variant _Elimination_record $ stringValue fname
  EliminationUnion cases -> variant _Elimination_union $ list $ encodeField cx <$> cases

encodeLiteralType :: Default m => Context m -> LiteralType -> Data m
encodeLiteralType cx at = case at of
  LiteralTypeBinary -> unitVariant _LiteralType_binary
  LiteralTypeBoolean -> unitVariant _LiteralType_boolean
  LiteralTypeFloat ft -> variant _LiteralType_float $ encodeFloatType cx ft
  LiteralTypeInteger it -> variant _LiteralType_integer $ encodeIntegerType cx it
  LiteralTypeString -> unitVariant _LiteralType_string

encodeLiteral :: Default m => Context m -> Literal -> Data m
encodeLiteral cx = atomic

encodeLiteralVariant :: Default m => Context m -> LiteralVariant -> Data m
encodeLiteralVariant cx av = unitVariant $ case av of
  LiteralVariantBinary -> _LiteralVariant_binary
  LiteralVariantBoolean -> _LiteralVariant_boolean
  LiteralVariantFloat -> _LiteralVariant_float
  LiteralVariantInteger -> _LiteralVariant_integer
  LiteralVariantString -> _LiteralVariant_string

encodeField :: (Default m, Ord m) => Context m -> Field m -> Data m
encodeField cx (Field (FieldName name) term) = nominalRecord cx _Field [
  Field _Field_name $ stringValue name,
  Field _Field_data $ encodeData cx term]

encodeFieldType :: Default m => Context m -> FieldType m -> Data m
encodeFieldType cx (FieldType (FieldName fname) t) = nominalRecord cx _FieldType [
  Field _FieldType_name $ stringValue fname,
  Field _FieldType_type $ encodeType cx t]

encodeFloatType :: Default m => Context m -> FloatType -> Data m
encodeFloatType cx ft = unitVariant $ case ft of
  FloatTypeBigfloat -> _FloatType_bigfloat
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

encodeFunction :: (Default m, Ord m) => Context m -> Function m -> Data m
encodeFunction cx f = case f of
  FunctionCompareTo other -> variant _Function_compareTo $ encodeData cx other
  FunctionElimination e -> variant _Function_compareTo $ encodeElimination cx e
  FunctionLambda l -> variant _Function_lambda $ encodeLambda cx l
  FunctionPrimitive (Name name) -> variant _Function_primitive $ stringValue name

encodeFunctionType :: Default m => Context m -> FunctionType m -> Data m
encodeFunctionType cx (FunctionType dom cod) = nominalRecord cx _FunctionType [
  Field _FunctionType_domain $ encodeType cx dom,
  Field _FunctionType_codomain $ encodeType cx cod]

encodeIntegerType :: Default m => Context m -> IntegerType -> Data m
encodeIntegerType cx it = unitVariant $ case it of
  IntegerTypeBigint -> _IntegerType_bigint
  IntegerTypeInt8 -> _IntegerType_int8
  IntegerTypeInt16 -> _IntegerType_int16
  IntegerTypeInt32 -> _IntegerType_int32
  IntegerTypeInt64 -> _IntegerType_int64
  IntegerTypeUint8 -> _IntegerType_uint8
  IntegerTypeUint16 -> _IntegerType_uint16
  IntegerTypeUint32 -> _IntegerType_uint32
  IntegerTypeUint64 -> _IntegerType_uint64

encodeLambda :: (Default m, Ord m) => Context m -> Lambda m -> Data m
encodeLambda cx (Lambda (Variable v) b) = nominalRecord cx _Lambda [
  Field _Lambda_parameter $ stringValue v,
  Field _Lambda_body $ encodeData cx b]

encodeMapType :: Default m => Context m -> MapType m -> Data m
encodeMapType cx (MapType kt vt) = nominalRecord cx _MapType [
  Field _MapType_keys $ encodeType cx kt,
  Field _MapType_values $ encodeType cx vt]

encodeNamed :: (Default m, Ord m) => Context m -> Named m -> Data m
encodeNamed cx (Named (Name name) term) = nominalRecord cx _Named [
  Field _Named_typeName $ stringValue name,
  Field _Named_term $ encodeData cx term]

encodeOptionalCases :: (Default m, Ord m) => Context m -> OptionalCases m -> Data m
encodeOptionalCases cx (OptionalCases nothing just) = nominalRecord cx _OptionalCases [
  Field _OptionalCases_nothing $ encodeData cx nothing,
  Field _OptionalCases_just $ encodeData cx just]

encodeData :: (Default m, Ord m) => Context m -> Data m -> Data m
encodeData cx term = case dataTerm term of
  DataTermApplication a -> variant _DataTerm_application $ encodeApplication cx a
  DataTermLiteral av -> variant _DataTerm_literal $ encodeLiteral cx av
  DataTermElement (Name name) -> variant _DataTerm_element $ stringValue name
  DataTermFunction f -> variant _DataTerm_function $ encodeFunction cx f
  DataTermList terms -> variant _DataTerm_list $ list $ encodeData cx <$> terms
  DataTermMap m -> variant _DataTerm_map $ map $ M.fromList $ encodePair <$> M.toList m
    where encodePair (k, v) = (encodeData cx k, encodeData cx v)
  DataTermNominal ntt -> variant _DataTerm_nominal $ encodeNamed cx ntt
  DataTermOptional m -> variant _DataTerm_optional $ optional $ encodeData cx <$> m
  DataTermRecord fields -> variant _DataTerm_record $ list $ encodeField cx <$> fields
  DataTermSet terms -> variant _DataTerm_set $ set $ S.fromList $ encodeData cx <$> S.toList terms
  DataTermUnion field -> variant _DataTerm_union $ encodeField cx field
  DataTermVariable (Variable var) -> variant _DataTerm_variable $ stringValue var

encodeType :: Default m => Context m -> Type m -> Data m
encodeType cx typ = setMeta (typeMeta typ) $ case typeTerm typ of
  TypeTermLiteral at -> variant _TypeTerm_literal $ encodeLiteralType cx at
  TypeTermElement t -> variant _TypeTerm_element $ encodeType cx t
  TypeTermFunction ft -> variant _TypeTerm_function $ encodeFunctionType cx ft
  TypeTermList t -> variant _TypeTerm_list $ encodeType cx t
  TypeTermMap mt -> variant _TypeTerm_map $ encodeMapType cx mt
  TypeTermNominal name -> variant _TypeTerm_nominal $ element name
  TypeTermOptional t -> variant _TypeTerm_optional $ encodeType cx t
  TypeTermRecord fields -> variant _TypeTerm_record $ list $ fmap (encodeFieldType cx) fields
  TypeTermSet t -> variant _TypeTerm_set $ encodeType cx t
  TypeTermUnion fields -> variant _TypeTerm_union $ list $ fmap (encodeFieldType cx) fields
  TypeTermUniversal ut -> variant _TypeTerm_universal $ encodeUniversalType cx ut
  TypeTermVariable (TypeVariable var) -> variant _TypeTerm_variable $ stringValue var

encodeTypeVariant :: Default m => Context m -> TypeVariant -> Data m
encodeTypeVariant cx tv = unitVariant $ case tv of
  TypeVariantLiteral -> _TypeVariant_literal
  TypeVariantElement -> _TypeVariant_element
  TypeVariantFunction -> _TypeVariant_function
  TypeVariantList -> _TypeVariant_list
  TypeVariantMap -> _TypeVariant_map
  TypeVariantNominal -> _TypeVariant_nominal
  TypeVariantOptional -> _TypeVariant_optional
  TypeVariantRecord -> _TypeVariant_record
  TypeVariantSet -> _TypeVariant_set
  TypeVariantUnion -> _TypeVariant_union
  TypeVariantUniversal -> _TypeVariant_universal
  TypeVariantVariable -> _TypeVariant_variable

encodeUniversalType :: Default m => Context m -> UniversalType m -> Data m
encodeUniversalType cx (UniversalType (TypeVariable var) body) = nominalRecord cx _UniversalType [
  Field _UniversalType_variable $ stringValue var,
  Field _UniversalType_body $ encodeType cx body]
