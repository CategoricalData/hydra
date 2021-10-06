module Hydra.Prototyping.BasicsWithDsl (
  basicsGraph,
  ) where

import Hydra.Core
import Hydra.Graph
import Hydra.Prototyping.CoreEncoding
import Hydra.Ext.Haskell.Dsl


basicsElement :: String -> Type -> Term -> Element
basicsElement name typ dat = Element ("hydra/basics." ++ name) (encodeType typ) dat

basicsFunction :: String -> Name -> Name -> Term -> Element
basicsFunction name dom cod dat = basicsElement name typ dat
  where
    typ = functionType (nominalType dom) (nominalType cod)

basicsGraph :: Graph
basicsGraph = Graph "hydra/basics" elements dataTerms schemaGraph
  where
    dataTerms = \t -> True -- TODO
    schemaGraph = "none" -- TODO
    elements = [
        atomicTypeVariant,
        atomicValueType,
        atomicValueVariant,
        floatTypeVariant,
        floatValueType,
        floatValueVariant,
        integerTypeVariant,
        integerValueType,
        integerValueVariant,
        termVariant,
        typeVariant]


atomicTypeVariant :: Element
atomicTypeVariant = basicsFunction "atomicTypeVariant" _AtomicType _AtomicVariant $
  matchWithVariants [
    (_AtomicType_binary,  _AtomicVariant_binary),
    (_AtomicType_boolean, _AtomicVariant_boolean),
    (_AtomicType_float,   _AtomicVariant_float),
    (_AtomicType_integer, _AtomicVariant_integer),
    (_AtomicType_string,  _AtomicVariant_string)]

atomicValueType :: Element
atomicValueType = basicsFunction "atomicValueType" _AtomicValue _AtomicType $
  match [
    (_AtomicValue_binary,  withVariant  _AtomicType_binary),
    (_AtomicValue_boolean, withVariant  _AtomicType_boolean),
    (_AtomicValue_float,   withFunction _AtomicType_float floatValueType),
    (_AtomicValue_integer, withFunction _AtomicType_integer integerValueType),
    (_AtomicValue_string,  withVariant  _AtomicType_string)]

atomicValueVariant :: Element
atomicValueVariant = basicsFunction "atomicValueVariant" _AtomicValue _AtomicVariant $
  compose (funcRef atomicTypeVariant) (funcRef atomicValueType)

floatTypeVariant :: Element
floatTypeVariant = basicsFunction "floatTypeVariant" _FloatType _FloatVariant $
  matchWithVariants [
    (_FloatType_bigfloat, _FloatVariant_bigfloat),
    (_FloatType_float32,  _FloatVariant_float32),
    (_FloatType_float64,  _FloatVariant_float64)]

floatValueType :: Element
floatValueType = basicsFunction "floatValueType" _FloatValue _FloatType $
  matchWithVariants [
    (_FloatValue_bigfloat, _FloatType_bigfloat),
    (_FloatValue_float32,  _FloatType_float32),
    (_FloatValue_float64,  _FloatType_float64)]

floatValueVariant :: Element
floatValueVariant = basicsFunction "floatValueVariant" _FloatValue _FloatVariant $
  compose (funcRef floatTypeVariant) (funcRef floatValueType)

integerTypeVariant :: Element
integerTypeVariant = basicsFunction "integerTypeVariant" _IntegerType _IntegerVariant $
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

integerValueType :: Element
integerValueType = basicsFunction "integerValueType"_IntegerValue _IntegerType $
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

integerValueVariant :: Element
integerValueVariant = basicsFunction "integerValueVariant" _IntegerValue _IntegerVariant $
  compose (funcRef integerTypeVariant) (funcRef integerValueType)

termVariant :: Element
termVariant = basicsFunction "termVariant" _Term _TermVariant $
  matchWithVariants [
    (_Term_atomic,     _TermVariant_atomic),
    (_Term_cases,      _TermVariant_cases),
    (_Term_compareTo,  _TermVariant_compareTo),
    (_Term_data,       _TermVariant_data),
    (_Term_element,    _TermVariant_element),
    (_Term_function,   _TermVariant_function),
    (_Term_lambda,     _TermVariant_lambda),
    (_Term_map,        _TermVariant_map),
    (_Term_list,       _TermVariant_list),
    (_Term_projection, _TermVariant_projection),
    (_Term_record,     _TermVariant_record),
    (_Term_set,        _TermVariant_set),
    (_Term_union,      _TermVariant_union),
    (_Term_variable,   _TermVariant_variable)]

typeVariant :: Element
typeVariant = basicsFunction "typeVariant" _Type _TypeVariant $
  matchWithVariants [
    (_Type_atomic,   _TypeVariant_atomic),
    (_Type_element,  _TypeVariant_element),
    (_Type_function, _TypeVariant_function),
    (_Type_list,     _TypeVariant_list),
    (_Type_map,      _TypeVariant_map),
    (_Type_nominal,  _TypeVariant_nominal),
    (_Type_record,   _TypeVariant_record),
    (_Type_set,      _TypeVariant_set),
    (_Type_union,    _TypeVariant_union)]
