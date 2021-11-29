package hydra.prototyping

import hydra.core.*;
import hydra.impl.scala.*


def atomicTypeAsTerm[a](meta: a, at: LiteralType): Term[a] = at match
  case LiteralType.binary() => unitVariant(meta, _LiteralType, _LiteralType_binary)
  case LiteralType.boolean() => unitVariant(meta, _LiteralType, _LiteralType_boolean)
  case LiteralType.float(ft) => variant(meta, _LiteralType, _LiteralType_float, floatTypeAsTerm(meta, ft))
  case LiteralType.integer(it) => variant(meta, _LiteralType, _LiteralType_integer, integerTypeAsTerm(meta, it))
  case LiteralType.string() => unitVariant(meta, _LiteralType, _LiteralType_string)

def fieldTypeAsTerm[a](meta: a, ft: FieldType): Term[a] = recordTerm(meta, Seq(
  Field(_FieldType_name, stringTerm(meta, ft.name)),
  Field(_FieldType_type, typeAsTerm(meta, ft.`type`))))

def floatTypeAsTerm[a](meta: a, ft: FloatType): Term[a] = unitVariant(meta, _FloatType, ft match
  case FloatType.bigfloat() => _FloatType_bigfloat
  case FloatType.float32() => _FloatType_float32
  case FloatType.float64() => _FloatType_float64)

def functionTypeAsTerm[a](meta: a, ft: FunctionType): Term[a] = recordTerm(meta, Seq(
  Field(_FunctionType_domain, typeAsTerm(meta, ft.domain)),
  Field(_FunctionType_codomain, typeAsTerm(meta, ft.codomain))))

def integerTypeAsTerm[a](meta: a, it: IntegerType): Term[a] = unitVariant(meta, _IntegerType, it match
  case IntegerType.bigint() => _IntegerType_bigint
  case IntegerType.int8() => _IntegerType_int8
  case IntegerType.int16() => _IntegerType_int16
  case IntegerType.int32() => _IntegerType_int32
  case IntegerType.int64() => _IntegerType_int64
  case IntegerType.uint8() => _IntegerType_uint8
  case IntegerType.uint16() => _IntegerType_uint16
  case IntegerType.uint32() => _IntegerType_uint32
  case IntegerType.uint64() => _IntegerType_uint64)

def mapTypeAsTerm[a](meta: a, mt: MapType): Term[a] = recordTerm(meta, Seq(
  Field(_MapType_keys, typeAsTerm(meta, mt.keys)),
  Field(_MapType_values, typeAsTerm(meta, mt.values))))

def typeAsTerm[a](meta: a, typ: Type): Term[a] = typ match
  case Type.literal(at) => variant(meta, _Type, _Type_literal, atomicTypeAsTerm(meta, at))
  case Type.element(t) => variant(meta, _Type, _Type_element, typeAsTerm(meta, t))
  case Type.function(ft) => variant(meta, _Type, _Type_function, functionTypeAsTerm(meta, ft))
  case Type.list(t) => variant(meta, _Type, _Type_list, typeAsTerm(meta, t))
  case Type.map(mt) => variant(meta, _Type, _Type_map, mapTypeAsTerm(meta, mt))
  case Type.nominal(name) => variant(meta, _Type, _Type_nominal, stringTerm(meta, name))
  case Type.optional(ot) => variant(meta, _Type, _Type_optional, typeAsTerm(meta, ot))
  case Type.record(fields) => variant(meta, _Type, _Type_record, list(meta, fields.map(ft => fieldTypeAsTerm(meta, ft))))
  case Type.set(t) => variant(meta, _Type, _Type_set, typeAsTerm(meta, t))
  case Type.union(fields) => variant(meta, _Type, _Type_union, list(meta, fields.map(ft => fieldTypeAsTerm(meta, ft))))
