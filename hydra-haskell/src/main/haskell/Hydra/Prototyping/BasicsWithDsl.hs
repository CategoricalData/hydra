module Hydra.Prototyping.BasicsWithDsl (
  basicsGraph,
) where

import Hydra.Core
import Hydra.Graph
import Hydra.Prototyping.CoreEncoding
import Hydra.Impl.Haskell.Dsl


basicsElement :: String -> Type -> Term -> Element
basicsElement name typ = Element ("hydra/basics." ++ name) (encodeType typ)

basicsFunction :: String -> Name -> Name -> Term -> Element
basicsFunction name dom cod = basicsElement name typ
  where
    typ = functionType (nominalType dom) (nominalType cod)

basicsGraph :: Graph
basicsGraph = Graph "hydra/basics" elements dataTerms schemaGraph
  where
    dataTerms = const True -- TODO
    schemaGraph = "hydra/core"
    elements = [
        basicsAtomicTypeVariant,
        basicsAtomicValueType,
        basicsAtomicValueVariant,
        basicsFloatTypeVariant,
        basicsFloatValueType,
        basicsFloatValueVariant,
        basicsFunctionVariant,
        basicsIntegerTypeVariant,
        basicsIntegerValueType,
        basicsIntegerValueVariant,
        basicsTermVariant,
        basicsTypeVariant]

basicsAtomicTypeVariant :: Element
basicsAtomicTypeVariant = basicsFunction "atomicTypeVariant" _AtomicType _AtomicVariant $
  matchWithVariants [
    (_AtomicType_binary,  _AtomicVariant_binary),
    (_AtomicType_boolean, _AtomicVariant_boolean),
    (_AtomicType_float,   _AtomicVariant_float),
    (_AtomicType_integer, _AtomicVariant_integer),
    (_AtomicType_string,  _AtomicVariant_string)]

basicsAtomicValueType :: Element
basicsAtomicValueType = basicsFunction "atomicValueType" _AtomicValue _AtomicType $
  match [
    (_AtomicValue_binary,  withVariant  _AtomicType_binary),
    (_AtomicValue_boolean, withVariant  _AtomicType_boolean),
    (_AtomicValue_float,   withFunction _AtomicType_float basicsFloatValueType),
    (_AtomicValue_integer, withFunction _AtomicType_integer basicsIntegerValueType),
    (_AtomicValue_string,  withVariant  _AtomicType_string)]

basicsAtomicValueVariant :: Element
basicsAtomicValueVariant = basicsFunction "atomicValueVariant" _AtomicValue _AtomicVariant $
  compose (elementRef basicsAtomicTypeVariant) (elementRef basicsAtomicValueType)

basicsFloatTypeVariant :: Element
basicsFloatTypeVariant = basicsFunction "floatTypeVariant" _FloatType _FloatVariant $
  matchWithVariants [
    (_FloatType_bigfloat, _FloatVariant_bigfloat),
    (_FloatType_float32,  _FloatVariant_float32),
    (_FloatType_float64,  _FloatVariant_float64)]

basicsFloatValueType :: Element
basicsFloatValueType = basicsFunction "floatValueType" _FloatValue _FloatType $
  matchWithVariants [
    (_FloatValue_bigfloat, _FloatType_bigfloat),
    (_FloatValue_float32,  _FloatType_float32),
    (_FloatValue_float64,  _FloatType_float64)]

basicsFloatValueVariant :: Element
basicsFloatValueVariant = basicsFunction "floatValueVariant" _FloatValue _FloatVariant $
  compose (elementRef basicsFloatTypeVariant) (elementRef basicsFloatValueType)

basicsIntegerTypeVariant :: Element
basicsIntegerTypeVariant = basicsFunction "integerTypeVariant" _IntegerType _IntegerVariant $
  matchWithVariants [
    (_IntegerType_bigint, _IntegerVariant_bigint),
    (_IntegerType_int8,   _IntegerVariant_int8),
    (_IntegerType_int16,  _IntegerVariant_int16),
    (_IntegerType_int32,  _IntegerVariant_int32),
    (_IntegerType_int64,  _IntegerVariant_int64),
    (_IntegerType_uint8,  _IntegerVariant_uint8),
    (_IntegerType_uint16, _IntegerVariant_uint16),
    (_IntegerType_uint32, _IntegerVariant_uint32),
    (_IntegerType_uint64, _IntegerVariant_uint64)]

basicsIntegerValueType :: Element
basicsIntegerValueType = basicsFunction "integerValueType"_IntegerValue _IntegerType $
  matchWithVariants [
    (_IntegerValue_bigint, _IntegerValue_bigint),
    (_IntegerValue_int8,   _IntegerValue_int8),
    (_IntegerValue_int16,  _IntegerValue_int16),
    (_IntegerValue_int32,  _IntegerValue_int32),
    (_IntegerValue_int64,  _IntegerValue_int64),
    (_IntegerValue_uint8,  _IntegerValue_uint8),
    (_IntegerValue_uint16, _IntegerValue_uint16),
    (_IntegerValue_uint32, _IntegerValue_uint32),
    (_IntegerValue_uint64, _IntegerValue_uint64)]

basicsIntegerValueVariant :: Element
basicsIntegerValueVariant = basicsFunction "integerValueVariant" _IntegerValue _IntegerVariant $
  compose (elementRef basicsIntegerTypeVariant) (elementRef basicsIntegerValueType)

basicsFunctionVariant :: Element
basicsFunctionVariant = basicsFunction "functionVariant" _Function _FunctionVariant $
  matchWithVariants [
    (_Function_cases,      _FunctionVariant_cases),
    (_Function_compareTo,  _FunctionVariant_compareTo),
    (_Function_data,       _FunctionVariant_data),
    (_Function_lambda,     _FunctionVariant_lambda),
    (_Function_primitive,  _FunctionVariant_primitive),
    (_Function_projection, _FunctionVariant_projection)]
    
basicsTermVariant :: Element
basicsTermVariant = basicsFunction "termVariant" _Term _TermVariant $
  matchWithVariants [
    (_Term_atomic,     _TermVariant_atomic),
    (_Term_element,    _TermVariant_element),
    (_Term_function,   _TermVariant_function),
    (_Term_list,       _TermVariant_list),
    (_Term_map,        _TermVariant_map),
    (_Term_optional,   _TermVariant_optional),
    (_Term_record,     _TermVariant_record),
    (_Term_set,        _TermVariant_set),
    (_Term_union,      _TermVariant_union),
    (_Term_variable,   _TermVariant_variable)]

basicsTypeVariant :: Element
basicsTypeVariant = basicsFunction "typeVariant" _Type _TypeVariant $
  matchWithVariants [
    (_Type_atomic,    _TypeVariant_atomic),
    (_Type_element,   _TypeVariant_element),
    (_Type_function,  _TypeVariant_function),
    (_Type_list,      _TypeVariant_list),
    (_Type_map,       _TypeVariant_map),
    (_Type_nominal,   _TypeVariant_nominal),
    (_Type_optional,  _TypeVariant_optional),
    (_Type_record,    _TypeVariant_record),
    (_Type_set,       _TypeVariant_set),
    (_Type_union,     _TypeVariant_union),
    (_Type_universal, _TypeVariant_universal),
    (_Type_variable,  _TypeVariant_variable)]
