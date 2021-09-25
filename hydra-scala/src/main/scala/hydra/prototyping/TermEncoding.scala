package hydra.prototyping

import hydra.core.*;


def atomicTypeAsTerm(at: AtomicType): Term = at match
  case AtomicType.binary() => unitVariant(_AtomicType_binary)
  case AtomicType.boolean() => unitVariant(_AtomicType_boolean)
  case AtomicType.float(ft) => variant(_AtomicType_float, floatTypeAsTerm(ft))
  case AtomicType.integer(it) => variant(_AtomicType_integer, integerTypeAsTerm(it))
  case AtomicType.string() => unitVariant(_AtomicType_string)

def fieldTypeAsTerm(ft: FieldType): Term = Term.record(Seq(
  Field(_FieldType_name, stringTerm(ft.name)),
  Field(_FieldType_type, typeAsTerm(ft.`type`))))

def floatTypeAsTerm(ft: FloatType): Term = unitVariant(ft match
  case FloatType.bigfloat() => _FloatType_bigfloat
  case FloatType.float32() => _FloatType_float32
  case FloatType.float64() => _FloatType_float64)

def functionTypeAsTerm(ft: FunctionType): Term = Term.record(Seq(
  Field(_FunctionType_domain, typeAsTerm(ft.domain)),
  Field(_FunctionType_codomain, typeAsTerm(ft.codomain))))

def integerTypeAsTerm(it: IntegerType): Term = unitVariant(it match
  case IntegerType.bigint() => _IntegerType_bigint
  case IntegerType.int8() => _IntegerType_int8
  case IntegerType.int16() => _IntegerType_int16
  case IntegerType.int32() => _IntegerType_int32
  case IntegerType.int64() => _IntegerType_int64
  case IntegerType.uint8() => _IntegerType_uint8
  case IntegerType.uint16() => _IntegerType_uint16
  case IntegerType.uint32() => _IntegerType_uint32
  case IntegerType.uint64() => _IntegerType_uint64)

def mapTypeAsTerm(mt: MapType): Term = Term.record(Seq(
  Field(_MapType_keys, typeAsTerm(mt.keys)),
  Field(_MapType_values, typeAsTerm(mt.values))))

def typeAsTerm(typ: Type): Term = typ match
  case Type.atomic(at) => variant(_Type_atomic, atomicTypeAsTerm(at))
  case Type.element(t) => variant(_Type_element, typeAsTerm(t))
  case Type.function(ft) => variant(_Type_function, functionTypeAsTerm(ft))
  case Type.list(t) => variant(_Type_list, typeAsTerm(t))
  case Type.map(mt) => variant(_Type_map, mapTypeAsTerm(mt)) 
  case Type.nominal(name) => variant(_Type_nominal, stringTerm(name))
  case Type.record(fields) => variant(_Type_record, Term.list(fields.map(fieldTypeAsTerm)))
  case Type.set(t) => variant(_Type_set, typeAsTerm(t))
  case Type.union(fields) => variant(_Type_union, Term.list(fields.map(fieldTypeAsTerm)))
