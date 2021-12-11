module Hydra.Impl.Haskell.Sources.Basics (
  basicsGraph,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Impl.Haskell.Dsl.Elements
import Hydra.Lib.Lists


_hydra_basics :: Name
_hydra_basics = "hydra/basics"

basicsGraph :: Graph Meta
basicsGraph = standardGraph _hydra_basics [
  basicsFloatTypePrecision,
  basicsFloatTypes,
  basicsFloatValueType,
  standardFunctionVariant,
  standardFunctionVariants,
  basicsIntegerTypeIsSigned,
  basicsIntegerTypePrecision,
  basicsIntegerTypes,
  basicsIntegerValueType,
  basicsLiteralType,
  basicsLiteralTypeVariant,
  basicsLiteralVariant,
  basicsLiteralVariants,
  basicsTermVariant,
  basicsTermVariants,
  basicsTmpTestLists,
  basicsTypeVariant,
  basicsTypeVariants]

floatTypePrecision :: FloatType -> Precision
floatTypePrecision v = case v of
  FloatTypeBigfloat -> PrecisionArbitrary
  FloatTypeFloat32 -> PrecisionBits 32
  FloatTypeFloat64 -> PrecisionBits 64

basicsFloatTypePrecision :: Context Meta -> Element Meta
basicsFloatTypePrecision cx = standardFunction cx _hydra_basics "floatTypePrecision"
  "Find the precision of a given floating-point type"
  (nominalType _FloatType) (nominalType _Precision) $
  nominalMatch cx _FloatType (nominalType _Precision) [
    (_FloatType_bigfloat, nominalWithUnitVariant cx _Precision _Precision_arbitrary),
    (_FloatType_float32, nominalWithVariant cx _Precision _Precision_bits (int32Value 32)),
    (_FloatType_float64, nominalWithVariant cx _Precision _Precision_bits (int32Value 64))]

basicsFloatTypes :: Context Meta -> Element Meta
basicsFloatTypes cx = standardElement cx _hydra_basics "floatTypes"
    "All floating-point types in a canonical order"
    (listType $ nominalType _FloatType)
    (list $ withType cx (nominalType _FloatType) . unitVariant <$> [
      _FloatType_bigfloat,
      _FloatType_float32,
      _FloatType_float64])

basicsFloatValueType :: Context Meta -> Element Meta
basicsFloatValueType cx = standardFunction cx _hydra_basics "floatValueType"
  "Find the float type for a given floating-point value"
  (nominalType _FloatValue) (nominalType _FloatType) $
  nominalMatchWithVariants cx (nominalType _FloatValue) (nominalType _FloatType) [
    (_FloatValue_bigfloat, _FloatType_bigfloat),
    (_FloatValue_float32,  _FloatType_float32),
    (_FloatValue_float64,  _FloatType_float64)]

standardFunctionVariant :: Context Meta -> Element Meta
standardFunctionVariant cx = standardFunction cx _hydra_basics "functionVariant"
  "Find the function variant (constructor) for a given function"
  (universal "a" $ nominalType _Function) (nominalType _FunctionVariant) $
  nominalMatchWithVariants cx (universal "a" $ nominalType _Function) (nominalType _FunctionVariant) [
    (_Function_cases,         _FunctionVariant_cases),
    (_Function_compareTo,     _FunctionVariant_compareTo),
    (_Function_data,          _FunctionVariant_data),
    (_Function_lambda,        _FunctionVariant_lambda),
    (_Function_optionalCases, _FunctionVariant_optionalCases),
    (_Function_primitive,     _FunctionVariant_primitive),
    (_Function_projection,    _FunctionVariant_projection)]

standardFunctionVariants :: Context Meta -> Element Meta
standardFunctionVariants cx = standardElement cx _hydra_basics "functionVariants"
    "All function variants (constructors), in a canonical order"
    (listType $ nominalType _FunctionVariant)
    (list $ withType cx (nominalType _FunctionVariant) . unitVariant <$> [
      _FunctionVariant_cases,
      _FunctionVariant_compareTo,
      _FunctionVariant_data,
      _FunctionVariant_lambda,
      _FunctionVariant_optionalCases,
      _FunctionVariant_primitive,
      _FunctionVariant_projection])

basicsIntegerTypeIsSigned :: Context Meta -> Element Meta
basicsIntegerTypeIsSigned cx = standardFunction cx _hydra_basics "integerTypeIsSigned"
  "Find whether a given integer type is signed (true) or unsigned (false)"
  (nominalType _IntegerType) booleanType $
  nominalMatch cx _IntegerType booleanType [
    (_IntegerType_bigint, constFunction $ booleanValue True),
    (_IntegerType_int8, constFunction $ booleanValue True),
    (_IntegerType_int16, constFunction $ booleanValue True),
    (_IntegerType_int32, constFunction $ booleanValue True),
    (_IntegerType_int64, constFunction $ booleanValue True),
    (_IntegerType_uint8, constFunction $ booleanValue False),
    (_IntegerType_uint16, constFunction $ booleanValue False),
    (_IntegerType_uint32, constFunction $ booleanValue False),
    (_IntegerType_uint64, constFunction $ booleanValue False)]

basicsIntegerTypePrecision :: Context Meta -> Element Meta
basicsIntegerTypePrecision cx = standardFunction cx _hydra_basics "integerTypePrecision"
  "Find the precision of a given integer type"
  (nominalType _IntegerType) (nominalType _Precision) $
  nominalMatch cx _IntegerType (nominalType _Precision) [
    (_IntegerType_bigint, nominalWithUnitVariant cx _Precision _Precision_arbitrary),
    (_IntegerType_int8, nominalWithVariant cx _Precision _Precision_bits (int32Value 8)),
    (_IntegerType_int16, nominalWithVariant cx _Precision _Precision_bits (int32Value 16)),
    (_IntegerType_int32, nominalWithVariant cx _Precision _Precision_bits (int32Value 32)),
    (_IntegerType_int64, nominalWithVariant cx _Precision _Precision_bits (int32Value 64)),
    (_IntegerType_uint8, nominalWithVariant cx _Precision _Precision_bits (int32Value 8)),
    (_IntegerType_uint16, nominalWithVariant cx _Precision _Precision_bits (int32Value 16)),
    (_IntegerType_uint32, nominalWithVariant cx _Precision _Precision_bits (int32Value 32)),
    (_IntegerType_uint64, nominalWithVariant cx _Precision _Precision_bits (int32Value 64))]

basicsIntegerTypes :: Context Meta -> Element Meta
basicsIntegerTypes cx = standardElement cx _hydra_basics "integerTypes"
    "All integer types, in a canonical order"
    (listType $ nominalType _IntegerType)
    (list $ withType cx (nominalType _IntegerType) . unitVariant <$> [
      _IntegerType_bigint,
      _IntegerType_int8,
      _IntegerType_int16,
      _IntegerType_int32,
      _IntegerType_int64,
      _IntegerType_uint8,
      _IntegerType_uint16,
      _IntegerType_uint32,
      _IntegerType_uint64])

basicsIntegerValueType :: Context Meta -> Element Meta
basicsIntegerValueType cx = standardFunction cx _hydra_basics "integerValueType"
  "Find the integer type for a given integer value"
  (nominalType _IntegerValue) (nominalType _IntegerType) $
  nominalMatchWithVariants cx (nominalType _IntegerValue) (nominalType _IntegerType) [
    (_IntegerValue_bigint, _IntegerType_bigint),
    (_IntegerValue_int8,   _IntegerType_int8),
    (_IntegerValue_int16,  _IntegerType_int16),
    (_IntegerValue_int32,  _IntegerType_int32),
    (_IntegerValue_int64,  _IntegerType_int64),
    (_IntegerValue_uint8,  _IntegerType_uint8),
    (_IntegerValue_uint16, _IntegerType_uint16),
    (_IntegerValue_uint32, _IntegerType_uint32),
    (_IntegerValue_uint64, _IntegerType_uint64)]

basicsLiteralType :: Context Meta -> Element Meta
basicsLiteralType cx = standardFunction cx _hydra_basics "literalType"
  "Find the literal type for a given literal value"
  (nominalType _Literal) (nominalType _LiteralType) $
  nominalMatch cx _Literal (nominalType _LiteralType) [
    (_Literal_binary,  nominalWithUnitVariant cx  _LiteralType _LiteralType_binary),
    (_Literal_boolean, nominalWithUnitVariant cx  _LiteralType _LiteralType_boolean),
    (_Literal_float,   nominalWithFunction cx _LiteralType _LiteralType_float (basicsFloatValueType cx)),
    (_Literal_integer, nominalWithFunction cx _LiteralType _LiteralType_integer (basicsIntegerValueType cx)),
    (_Literal_string,  nominalWithUnitVariant cx  _LiteralType _LiteralType_string)]

basicsLiteralTypeVariant :: Context Meta -> Element Meta
basicsLiteralTypeVariant cx = standardFunction cx _hydra_basics "literalTypeVariant"
  "Find the literal type variant (constructor) for a given literal value"
  (nominalType _LiteralType) (nominalType _LiteralVariant) $
  nominalMatchWithVariants cx (nominalType _LiteralType) (nominalType _LiteralVariant) [
    (_LiteralType_binary,  _LiteralVariant_binary),
    (_LiteralType_boolean, _LiteralVariant_boolean),
    (_LiteralType_float,   _LiteralVariant_float),
    (_LiteralType_integer, _LiteralVariant_integer),
    (_LiteralType_string,  _LiteralVariant_string)]

basicsLiteralVariant :: Context Meta -> Element Meta
basicsLiteralVariant cx = standardFunction cx _hydra_basics "literalVariant"
  "Find the literal variant (constructor) for a given literal value"
  (nominalType _Literal) (nominalType _LiteralVariant) $
  compose (elementRef $ basicsLiteralTypeVariant cx) (elementRef $ basicsLiteralType cx)

basicsLiteralVariants :: Context Meta -> Element Meta
basicsLiteralVariants cx = standardElement cx _hydra_basics "literalVariants"
  "All literal variants, in a canonical order"
  (listType $ nominalType _LiteralVariant)
  (list $ withType cx (nominalType _LiteralVariant) . unitVariant <$> [
    _LiteralVariant_binary,
    _LiteralVariant_boolean,
    _LiteralVariant_float,
    _LiteralVariant_integer,
    _LiteralVariant_string])

basicsTermVariant :: Context Meta -> Element Meta
basicsTermVariant cx = standardFunction cx _hydra_basics "termVariant"
  "Find the term variant (constructor) for a given term"
  (universal "a" $ nominalType _Term) (nominalType _TermVariant) $
  lambda "term" $ apply
    (nominalMatchWithVariants cx (universal "a" $ nominalType _Expression) (nominalType _TermVariant) [
          (_Expression_application,     _TermVariant_application),
          (_Expression_element,         _TermVariant_element),
          (_Expression_function,        _TermVariant_function),
          (_Expression_list,            _TermVariant_list),
          (_Expression_literal,         _TermVariant_literal),
          (_Expression_map,             _TermVariant_map),
          (_Expression_nominal,         _TermVariant_nominal),
          (_Expression_optional,        _TermVariant_optional),
          (_Expression_record,          _TermVariant_record),
          (_Expression_set,             _TermVariant_set),
          (_Expression_typeAbstraction, _TermVariant_typeAbstraction),
          (_Expression_typeApplication, _TermVariant_typeApplication),
          (_Expression_union,           _TermVariant_union),
          (_Expression_variable,        _TermVariant_variable)])
    (apply (nominalProjection cx _Term _Term_data (nominalType _Term)) $ variable "term")

basicsTermVariants :: Context Meta -> Element Meta
basicsTermVariants cx = standardElement cx _hydra_basics "termVariants"
  "All term (expression) variants, in a canonical order"
  (listType $ nominalType _TermVariant)
  (list $ withType cx (nominalType _TermVariant) . unitVariant <$> [
    _TermVariant_application,
    _TermVariant_literal,
    _TermVariant_element,
    _TermVariant_function,
    _TermVariant_list,
    _TermVariant_map,
    _TermVariant_nominal,
    _TermVariant_optional,
    _TermVariant_record,
    _TermVariant_set,
    _TermVariant_union,
    _TermVariant_variable])

-- TODO: remove once there are other polymorphic functions in use
basicsTmpTestLists :: Context Meta -> Element Meta
basicsTmpTestLists cx = standardFunction cx _hydra_basics "testLists"
  "TODO: temporary. Just a token polymorphic function for testing"
  (listType $ listType $ typeVariable "a") int32Type
  (lambda "lists" (apply (primitive _lists_length) (apply (primitive _lists_concat) $ variable "lists")))

basicsTypeVariant :: Context Meta -> Element Meta
basicsTypeVariant cx = standardFunction cx _hydra_basics "typeVariant"
  "Find the type variant (constructor) for a given type"
  (nominalType _Type) (nominalType _TypeVariant) $
  nominalMatchWithVariants cx (nominalType _Type) (nominalType _TypeVariant) [
    (_Type_element,   _TypeVariant_element),
    (_Type_function,  _TypeVariant_function),
    (_Type_list,      _TypeVariant_list),
    (_Type_literal,   _TypeVariant_literal),
    (_Type_map,       _TypeVariant_map),
    (_Type_nominal,   _TypeVariant_nominal),
    (_Type_optional,  _TypeVariant_optional),
    (_Type_record,    _TypeVariant_record),
    (_Type_set,       _TypeVariant_set),
    (_Type_union,     _TypeVariant_union),
    (_Type_universal, _TypeVariant_universal),
    (_Type_variable,  _TypeVariant_variable)]

basicsTypeVariants :: Context Meta -> Element Meta
basicsTypeVariants cx = standardElement cx _hydra_basics "typeVariants"
  "All type variants, in a canonical order"
  (listType $ nominalType _TypeVariant)
  (list $ withType cx (nominalType _TypeVariant) . unitVariant <$> [
    _TypeVariant_literal,
    _TypeVariant_element,
    _TypeVariant_function,
    _TypeVariant_list,
    _TypeVariant_map,
    _TypeVariant_nominal,
    _TypeVariant_optional,
    _TypeVariant_record,
    _TypeVariant_set,
    _TypeVariant_union,
    _TypeVariant_universal,
    _TypeVariant_variable])
