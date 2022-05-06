module Hydra.Impl.Haskell.Sources.Basics where

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Impl.Haskell.Dsl.Standard
import Hydra.Impl.Haskell.Sources.Libraries
import qualified Hydra.Impl.Haskell.Dsl.Types as Types


hydraBasicsModule = Module hydraBasics []

hydraBasicsName :: Name
hydraBasicsName = "hydra/basics"

hydraBasics :: Graph Meta
hydraBasics = standardGraph hydraBasicsName [
  floatTypePrecision,
  floatTypes,
  floatValueType,
  functionVariant,
  functionVariants,
  integerTypeIsSigned,
  integerTypePrecision,
  integerTypes,
  integerValueType,
  literalType,
  literalTypeVariant,
  literalVariant,
  literalVariants,
  qname,
  termVariant,
  termVariants,
  testLists,
  typeVariant,
  typeVariants]

floatTypePrecision :: Element Meta
floatTypePrecision = standardFunction hydraBasicsName "floatTypePrecision"
  "Find the precision of a given floating-point type"
  (Types.nominal _FloatType) (Types.nominal _Precision) $
  standardMatch _FloatType (Types.nominal _Precision) [
    (_FloatType_bigfloat, standardWithUnitVariant _Precision _Precision_arbitrary),
    (_FloatType_float32, standardWithVariant _Precision _Precision_bits (int32Value 32)),
    (_FloatType_float64, standardWithVariant _Precision _Precision_bits (int32Value 64))]

floatTypes :: Element Meta
floatTypes = standardElement hydraBasicsName "floatTypes"
    "All floating-point types in a canonical order"
    (Types.list $ Types.nominal _FloatType)
    (list $ standardWithType (Types.nominal _FloatType) . unitVariant <$> [
      _FloatType_bigfloat,
      _FloatType_float32,
      _FloatType_float64])

floatValueType :: Element Meta
floatValueType = standardFunction hydraBasicsName "floatValueType"
  "Find the float type for a given floating-point value"
  (Types.nominal _FloatValue) (Types.nominal _FloatType) $
  standardMatchWithVariants (Types.nominal _FloatValue) (Types.nominal _FloatType) [
    (_FloatValue_bigfloat, _FloatType_bigfloat),
    (_FloatValue_float32,  _FloatType_float32),
    (_FloatValue_float64,  _FloatType_float64)]

functionVariant :: Element Meta
functionVariant = standardFunction hydraBasicsName "functionVariant"
  "Find the function variant (constructor) for a given function"
  (Types.universal "m" $ Types.nominal _Function) (Types.nominal _FunctionVariant) $
  standardMatchWithVariants (Types.universal "m" $ Types.nominal _Function) (Types.nominal _FunctionVariant) [
    (_Function_cases,         _FunctionVariant_cases),
    (_Function_compareTo,     _FunctionVariant_compareTo),
    (_Function_data,          _FunctionVariant_data),
    (_Function_lambda,        _FunctionVariant_lambda),
    (_Function_optionalCases, _FunctionVariant_optionalCases),
    (_Function_primitive,     _FunctionVariant_primitive),
    (_Function_projection,    _FunctionVariant_projection)]

functionVariants :: Element Meta
functionVariants = standardElement hydraBasicsName "functionVariants"
    "All function variants (constructors), in a canonical order"
    (Types.list $ Types.nominal _FunctionVariant)
    (list $ standardWithType (Types.nominal _FunctionVariant) . unitVariant <$> [
      _FunctionVariant_cases,
      _FunctionVariant_compareTo,
      _FunctionVariant_data,
      _FunctionVariant_lambda,
      _FunctionVariant_optionalCases,
      _FunctionVariant_primitive,
      _FunctionVariant_projection])

integerTypeIsSigned :: Element Meta
integerTypeIsSigned = standardFunction hydraBasicsName "integerTypeIsSigned"
  "Find whether a given integer type is signed (true) or unsigned (false)"
  (Types.nominal _IntegerType) Types.boolean $
  standardMatch _IntegerType Types.boolean [
    (_IntegerType_bigint, constFunction $ booleanValue True),
    (_IntegerType_int8, constFunction $ booleanValue True),
    (_IntegerType_int16, constFunction $ booleanValue True),
    (_IntegerType_int32, constFunction $ booleanValue True),
    (_IntegerType_int64, constFunction $ booleanValue True),
    (_IntegerType_uint8, constFunction $ booleanValue False),
    (_IntegerType_uint16, constFunction $ booleanValue False),
    (_IntegerType_uint32, constFunction $ booleanValue False),
    (_IntegerType_uint64, constFunction $ booleanValue False)]

integerTypePrecision :: Element Meta
integerTypePrecision = standardFunction hydraBasicsName "integerTypePrecision"
  "Find the precision of a given integer type"
  (Types.nominal _IntegerType) (Types.nominal _Precision) $
  standardMatch _IntegerType (Types.nominal _Precision) [
    (_IntegerType_bigint, standardWithUnitVariant _Precision _Precision_arbitrary),
    (_IntegerType_int8, standardWithVariant _Precision _Precision_bits (int32Value 8)),
    (_IntegerType_int16, standardWithVariant _Precision _Precision_bits (int32Value 16)),
    (_IntegerType_int32, standardWithVariant _Precision _Precision_bits (int32Value 32)),
    (_IntegerType_int64, standardWithVariant _Precision _Precision_bits (int32Value 64)),
    (_IntegerType_uint8, standardWithVariant _Precision _Precision_bits (int32Value 8)),
    (_IntegerType_uint16, standardWithVariant _Precision _Precision_bits (int32Value 16)),
    (_IntegerType_uint32, standardWithVariant _Precision _Precision_bits (int32Value 32)),
    (_IntegerType_uint64, standardWithVariant _Precision _Precision_bits (int32Value 64))]

integerTypes :: Element Meta
integerTypes = standardElement hydraBasicsName "integerTypes"
    "All integer types, in a canonical order"
    (Types.list $ Types.nominal _IntegerType)
    (list $ standardWithType (Types.nominal _IntegerType) . unitVariant <$> [
      _IntegerType_bigint,
      _IntegerType_int8,
      _IntegerType_int16,
      _IntegerType_int32,
      _IntegerType_int64,
      _IntegerType_uint8,
      _IntegerType_uint16,
      _IntegerType_uint32,
      _IntegerType_uint64])

integerValueType :: Element Meta
integerValueType = standardFunction hydraBasicsName "integerValueType"
  "Find the integer type for a given integer value"
  (Types.nominal _IntegerValue) (Types.nominal _IntegerType) $
  standardMatchWithVariants (Types.nominal _IntegerValue) (Types.nominal _IntegerType) [
    (_IntegerValue_bigint, _IntegerType_bigint),
    (_IntegerValue_int8,   _IntegerType_int8),
    (_IntegerValue_int16,  _IntegerType_int16),
    (_IntegerValue_int32,  _IntegerType_int32),
    (_IntegerValue_int64,  _IntegerType_int64),
    (_IntegerValue_uint8,  _IntegerType_uint8),
    (_IntegerValue_uint16, _IntegerType_uint16),
    (_IntegerValue_uint32, _IntegerType_uint32),
    (_IntegerValue_uint64, _IntegerType_uint64)]

literalType :: Element Meta
literalType = standardFunction hydraBasicsName "literalType"
  "Find the literal type for a given literal value"
  (Types.nominal _Literal) (Types.nominal _LiteralType) $
  standardMatch _Literal (Types.nominal _LiteralType) [
    (_Literal_binary,  standardWithUnitVariant  _LiteralType _LiteralType_binary),
    (_Literal_boolean, standardWithUnitVariant  _LiteralType _LiteralType_boolean),
    (_Literal_float,   standardWithFunction _LiteralType _LiteralType_float floatValueType),
    (_Literal_integer, standardWithFunction _LiteralType _LiteralType_integer integerValueType),
    (_Literal_string,  standardWithUnitVariant  _LiteralType _LiteralType_string)]

literalTypeVariant :: Element Meta
literalTypeVariant = standardFunction hydraBasicsName "literalTypeVariant"
  "Find the literal type variant (constructor) for a given literal value"
  (Types.nominal _LiteralType) (Types.nominal _LiteralVariant) $
  standardMatchWithVariants (Types.nominal _LiteralType) (Types.nominal _LiteralVariant) [
    (_LiteralType_binary,  _LiteralVariant_binary),
    (_LiteralType_boolean, _LiteralVariant_boolean),
    (_LiteralType_float,   _LiteralVariant_float),
    (_LiteralType_integer, _LiteralVariant_integer),
    (_LiteralType_string,  _LiteralVariant_string)]

literalVariant :: Element Meta
literalVariant = standardFunction hydraBasicsName "literalVariant"
  "Find the literal variant (constructor) for a given literal value"
  (Types.nominal _Literal) (Types.nominal _LiteralVariant) $
  compose (elementRef literalTypeVariant) (elementRef literalType)

literalVariants :: Element Meta
literalVariants = standardElement hydraBasicsName "literalVariants"
  "All literal variants, in a canonical order"
  (Types.list $ Types.nominal _LiteralVariant)
  (list $ standardWithType (Types.nominal _LiteralVariant) . unitVariant <$> [
    _LiteralVariant_binary,
    _LiteralVariant_boolean,
    _LiteralVariant_float,
    _LiteralVariant_integer,
    _LiteralVariant_string])

qname :: Element Meta
qname = standardFunction hydraBasicsName "qname"
  "Construct a qualified (dot-separated) name"
  (Types.nominal _Name) (Types.function Types.string $ Types.nominal _Name)
  (lambda "ns" (lambda "name" (apply (primitive _strings_cat) (list [
   variable "ns", stringValue ".", variable "name"]))))

termVariant :: Element Meta
termVariant = standardFunction hydraBasicsName "termVariant"
  "Find the term variant (constructor) for a given term"
  (Types.universal "m" $ Types.nominal _Data) (Types.nominal _DataVariant) $
  lambda "term" $ apply
    (standardMatchWithVariants (Types.universal "m" $ Types.nominal _DataTerm) (Types.nominal _DataVariant) [
          (_DataTerm_application,     _DataVariant_application),
          (_DataTerm_element,         _DataVariant_element),
          (_DataTerm_function,        _DataVariant_function),
          (_DataTerm_list,            _DataVariant_list),
          (_DataTerm_literal,         _DataVariant_literal),
          (_DataTerm_map,             _DataVariant_map),
          (_DataTerm_nominal,         _DataVariant_nominal),
          (_DataTerm_optional,        _DataVariant_optional),
          (_DataTerm_record,          _DataVariant_record),
          (_DataTerm_set,             _DataVariant_set),
          (_DataTerm_typeAbstraction, _DataVariant_typeAbstraction),
          (_DataTerm_typeApplication, _DataVariant_typeApplication),
          (_DataTerm_union,           _DataVariant_union),
          (_DataTerm_variable,        _DataVariant_variable)])
    (apply (project (Types.universal "m" $ Types.nominal _Data) _Data_term (Types.universal "m" $ Types.nominal _DataTerm))
      $ variable "term")

termVariants :: Element Meta
termVariants = standardElement hydraBasicsName "termVariants"
  "All term (expression) variants, in a canonical order"
  (Types.list $ Types.nominal _DataVariant)
  (list $ standardWithType (Types.nominal _DataVariant) . unitVariant <$> [
    _DataVariant_application,
    _DataVariant_literal,
    _DataVariant_element,
    _DataVariant_function,
    _DataVariant_list,
    _DataVariant_map,
    _DataVariant_nominal,
    _DataVariant_optional,
    _DataVariant_record,
    _DataVariant_set,
    _DataVariant_union,
    _DataVariant_variable])

-- TODO: remove once there are other polymorphic functions in use
testLists :: Element Meta
testLists = standardFunction hydraBasicsName "testLists"
  "TODO: temporary. Just a token polymorphic function for testing"
  (Types.list $ Types.list $ Types.variable "a") Types.int32
  (lambda "els" (apply (primitive _lists_length) (apply (primitive _lists_concat) $ variable "els")))

typeVariant :: Element Meta
typeVariant = standardFunction hydraBasicsName "typeVariant"
  "Find the type variant (constructor) for a given type"
  (Types.universal "m" $ Types.nominal _Type) (Types.nominal _TypeVariant) $
  lambda "typ" $ apply
    (standardMatchWithVariants (Types.universal "m" $ Types.nominal _TypeTerm) (Types.nominal _TypeVariant) [
        (_TypeTerm_element,   _TypeVariant_element),
        (_TypeTerm_function,  _TypeVariant_function),
        (_TypeTerm_list,      _TypeVariant_list),
        (_TypeTerm_literal,   _TypeVariant_literal),
        (_TypeTerm_map,       _TypeVariant_map),
        (_TypeTerm_nominal,   _TypeVariant_nominal),
        (_TypeTerm_optional,  _TypeVariant_optional),
        (_TypeTerm_record,    _TypeVariant_record),
        (_TypeTerm_set,       _TypeVariant_set),
        (_TypeTerm_union,     _TypeVariant_union),
        (_TypeTerm_universal, _TypeVariant_universal),
        (_TypeTerm_variable,  _TypeVariant_variable)])
    (apply (project (Types.universal "m" $ Types.nominal _Type) _Type_term (Types.universal "m" $ Types.nominal _TypeTerm))
      $ variable "typ")

typeVariants :: Element Meta
typeVariants = standardElement hydraBasicsName "typeVariants"
  "All type variants, in a canonical order"
  (Types.list $ Types.nominal _TypeVariant)
  (list $ standardWithType (Types.nominal _TypeVariant) . unitVariant <$> [
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
