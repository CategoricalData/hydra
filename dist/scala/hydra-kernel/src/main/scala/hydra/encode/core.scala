package hydra.encode.core

import hydra.core.*

def annotatedTerm(x: hydra.core.AnnotatedTerm): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.AnnotatedTerm", Seq(hydra.core.Field("body", hydra.encode.core.term(x.body)),
     hydra.core.Field("annotation", hydra.core.Term.map(hydra.lib.maps.bimap[hydra.core.Name, hydra.core.Term,
     hydra.core.Term, hydra.core.Term](hydra.encode.core.name)(hydra.encode.core.term)(x.annotation))))))

def annotatedType(x: hydra.core.AnnotatedType): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.AnnotatedType", Seq(hydra.core.Field("body", hydra.encode.core.`type`(x.body)),
     hydra.core.Field("annotation", hydra.core.Term.map(hydra.lib.maps.bimap[hydra.core.Name, hydra.core.Term,
     hydra.core.Term, hydra.core.Term](hydra.encode.core.name)(hydra.encode.core.term)(x.annotation))))))

def application(x: hydra.core.Application): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.Application", Seq(hydra.core.Field("function",
     hydra.encode.core.term(x.function)), hydra.core.Field("argument", hydra.encode.core.term(x.argument)))))

def applicationType(x: hydra.core.ApplicationType): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.ApplicationType", Seq(hydra.core.Field("function",
     hydra.encode.core.`type`(x.function)), hydra.core.Field("argument", hydra.encode.core.`type`(x.argument)))))

def binding(x: hydra.core.Binding): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.Binding", Seq(hydra.core.Field("name", hydra.encode.core.name(x.name)),
     hydra.core.Field("term", hydra.encode.core.term(x.term)), hydra.core.Field("type", hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.core.TypeScheme,
     hydra.core.Term](hydra.encode.core.typeScheme)(x.`type`))))))

def caseStatement(x: hydra.core.CaseStatement): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.CaseStatement", Seq(hydra.core.Field("typeName",
     hydra.encode.core.name(x.typeName)), hydra.core.Field("default", hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.core.Term,
     hydra.core.Term](hydra.encode.core.term)(x.default))), hydra.core.Field("cases", hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Field,
     hydra.core.Term](hydra.encode.core.field)(x.cases))))))

def eitherType(x: hydra.core.EitherType): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.EitherType", Seq(hydra.core.Field("left", hydra.encode.core.`type`(x.left)),
     hydra.core.Field("right", hydra.encode.core.`type`(x.right)))))

def field(x: hydra.core.Field): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.Field", Seq(hydra.core.Field("name", hydra.encode.core.name(x.name)),
     hydra.core.Field("term", hydra.encode.core.term(x.term)))))

def fieldType(x: hydra.core.FieldType): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.FieldType", Seq(hydra.core.Field("name", hydra.encode.core.name(x.name)),
     hydra.core.Field("type", hydra.encode.core.`type`(x.`type`)))))

def floatType(v1: hydra.core.FloatType): hydra.core.Term =
  v1 match
  case hydra.core.FloatType.bigfloat => hydra.core.Term.inject(hydra.core.Injection("hydra.core.FloatType", hydra.core.Field("bigfloat", hydra.core.Term.unit)))
  case hydra.core.FloatType.float32 => hydra.core.Term.inject(hydra.core.Injection("hydra.core.FloatType", hydra.core.Field("float32", hydra.core.Term.unit)))
  case hydra.core.FloatType.float64 => hydra.core.Term.inject(hydra.core.Injection("hydra.core.FloatType", hydra.core.Field("float64", hydra.core.Term.unit)))

def floatValue(v1: hydra.core.FloatValue): hydra.core.Term =
  v1 match
  case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.FloatValue",
     hydra.core.Field("bigfloat", hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat_y))))))
  case hydra.core.FloatValue.float32(v_FloatValue_float32_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.FloatValue",
     hydra.core.Field("float32", hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(v_FloatValue_float32_y))))))
  case hydra.core.FloatValue.float64(v_FloatValue_float64_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.FloatValue",
     hydra.core.Field("float64", hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(v_FloatValue_float64_y))))))

def forallType(x: hydra.core.ForallType): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.ForallType", Seq(hydra.core.Field("parameter",
     hydra.encode.core.name(x.parameter)), hydra.core.Field("body", hydra.encode.core.`type`(x.body)))))

def functionType(x: hydra.core.FunctionType): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.FunctionType", Seq(hydra.core.Field("domain", hydra.encode.core.`type`(x.domain)),
     hydra.core.Field("codomain", hydra.encode.core.`type`(x.codomain)))))

def injection(x: hydra.core.Injection): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.Injection", Seq(hydra.core.Field("typeName", hydra.encode.core.name(x.typeName)),
     hydra.core.Field("field", hydra.encode.core.field(x.field)))))

def integerType(v1: hydra.core.IntegerType): hydra.core.Term =
  v1 match
  case hydra.core.IntegerType.bigint => hydra.core.Term.inject(hydra.core.Injection("hydra.core.IntegerType", hydra.core.Field("bigint", hydra.core.Term.unit)))
  case hydra.core.IntegerType.int8 => hydra.core.Term.inject(hydra.core.Injection("hydra.core.IntegerType", hydra.core.Field("int8", hydra.core.Term.unit)))
  case hydra.core.IntegerType.int16 => hydra.core.Term.inject(hydra.core.Injection("hydra.core.IntegerType", hydra.core.Field("int16", hydra.core.Term.unit)))
  case hydra.core.IntegerType.int32 => hydra.core.Term.inject(hydra.core.Injection("hydra.core.IntegerType", hydra.core.Field("int32", hydra.core.Term.unit)))
  case hydra.core.IntegerType.int64 => hydra.core.Term.inject(hydra.core.Injection("hydra.core.IntegerType", hydra.core.Field("int64", hydra.core.Term.unit)))
  case hydra.core.IntegerType.uint8 => hydra.core.Term.inject(hydra.core.Injection("hydra.core.IntegerType", hydra.core.Field("uint8", hydra.core.Term.unit)))
  case hydra.core.IntegerType.uint16 => hydra.core.Term.inject(hydra.core.Injection("hydra.core.IntegerType", hydra.core.Field("uint16", hydra.core.Term.unit)))
  case hydra.core.IntegerType.uint32 => hydra.core.Term.inject(hydra.core.Injection("hydra.core.IntegerType", hydra.core.Field("uint32", hydra.core.Term.unit)))
  case hydra.core.IntegerType.uint64 => hydra.core.Term.inject(hydra.core.Injection("hydra.core.IntegerType", hydra.core.Field("uint64", hydra.core.Term.unit)))

def integerValue(v1: hydra.core.IntegerValue): hydra.core.Term =
  v1 match
  case hydra.core.IntegerValue.bigint(v_IntegerValue_bigint_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.IntegerValue",
     hydra.core.Field("bigint", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(v_IntegerValue_bigint_y))))))
  case hydra.core.IntegerValue.int8(v_IntegerValue_int8_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.IntegerValue",
     hydra.core.Field("int8", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int8(v_IntegerValue_int8_y))))))
  case hydra.core.IntegerValue.int16(v_IntegerValue_int16_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.IntegerValue",
     hydra.core.Field("int16", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int16(v_IntegerValue_int16_y))))))
  case hydra.core.IntegerValue.int32(v_IntegerValue_int32_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.IntegerValue",
     hydra.core.Field("int32", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(v_IntegerValue_int32_y))))))
  case hydra.core.IntegerValue.int64(v_IntegerValue_int64_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.IntegerValue",
     hydra.core.Field("int64", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int64(v_IntegerValue_int64_y))))))
  case hydra.core.IntegerValue.uint8(v_IntegerValue_uint8_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.IntegerValue",
     hydra.core.Field("uint8", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint8(v_IntegerValue_uint8_y))))))
  case hydra.core.IntegerValue.uint16(v_IntegerValue_uint16_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.IntegerValue",
     hydra.core.Field("uint16", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint16(v_IntegerValue_uint16_y))))))
  case hydra.core.IntegerValue.uint32(v_IntegerValue_uint32_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.IntegerValue",
     hydra.core.Field("uint32", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint32(v_IntegerValue_uint32_y))))))
  case hydra.core.IntegerValue.uint64(v_IntegerValue_uint64_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.IntegerValue",
     hydra.core.Field("uint64", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint64(v_IntegerValue_uint64_y))))))

def lambda(x: hydra.core.Lambda): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.Lambda", Seq(hydra.core.Field("parameter", hydra.encode.core.name(x.parameter)),
     hydra.core.Field("domain", hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.core.Type, hydra.core.Term](hydra.encode.core.`type`)(x.domain))),
     hydra.core.Field("body", hydra.encode.core.term(x.body)))))

def let(x: hydra.core.Let): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.Let", Seq(hydra.core.Field("bindings", hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Binding,
     hydra.core.Term](hydra.encode.core.binding)(x.bindings))), hydra.core.Field("body", hydra.encode.core.term(x.body)))))

def literal(v1: hydra.core.Literal): hydra.core.Term =
  v1 match
  case hydra.core.Literal.binary(v_Literal_binary_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Literal",
     hydra.core.Field("binary", hydra.core.Term.literal(hydra.core.Literal.binary(v_Literal_binary_y)))))
  case hydra.core.Literal.boolean(v_Literal_boolean_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Literal",
     hydra.core.Field("boolean", hydra.core.Term.literal(hydra.core.Literal.boolean(v_Literal_boolean_y)))))
  case hydra.core.Literal.float(v_Literal_float_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Literal",
     hydra.core.Field("float", hydra.encode.core.floatValue(v_Literal_float_y))))
  case hydra.core.Literal.integer(v_Literal_integer_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Literal",
     hydra.core.Field("integer", hydra.encode.core.integerValue(v_Literal_integer_y))))
  case hydra.core.Literal.string(v_Literal_string_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Literal",
     hydra.core.Field("string", hydra.core.Term.literal(hydra.core.Literal.string(v_Literal_string_y)))))

def literalType(v1: hydra.core.LiteralType): hydra.core.Term =
  v1 match
  case hydra.core.LiteralType.binary => hydra.core.Term.inject(hydra.core.Injection("hydra.core.LiteralType", hydra.core.Field("binary", hydra.core.Term.unit)))
  case hydra.core.LiteralType.boolean => hydra.core.Term.inject(hydra.core.Injection("hydra.core.LiteralType",
     hydra.core.Field("boolean", hydra.core.Term.unit)))
  case hydra.core.LiteralType.float(v_LiteralType_float_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.LiteralType",
     hydra.core.Field("float", hydra.encode.core.floatType(v_LiteralType_float_y))))
  case hydra.core.LiteralType.integer(v_LiteralType_integer_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.LiteralType",
     hydra.core.Field("integer", hydra.encode.core.integerType(v_LiteralType_integer_y))))
  case hydra.core.LiteralType.string => hydra.core.Term.inject(hydra.core.Injection("hydra.core.LiteralType", hydra.core.Field("string", hydra.core.Term.unit)))

def mapType(x: hydra.core.MapType): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.MapType", Seq(hydra.core.Field("keys", hydra.encode.core.`type`(x.keys)),
     hydra.core.Field("values", hydra.encode.core.`type`(x.values)))))

def name(x: hydra.core.Name): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.core.Name", hydra.core.Term.literal(hydra.core.Literal.string(x))))

def pairType(x: hydra.core.PairType): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.PairType", Seq(hydra.core.Field("first", hydra.encode.core.`type`(x.first)),
     hydra.core.Field("second", hydra.encode.core.`type`(x.second)))))

def projection(x: hydra.core.Projection): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.Projection", Seq(hydra.core.Field("typeName", hydra.encode.core.name(x.typeName)),
     hydra.core.Field("field", hydra.encode.core.name(x.field)))))

def record(x: hydra.core.Record): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.Record", Seq(hydra.core.Field("typeName", hydra.encode.core.name(x.typeName)),
     hydra.core.Field("fields", hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Field, hydra.core.Term](hydra.encode.core.field)(x.fields))))))

def term(v1: hydra.core.Term): hydra.core.Term =
  v1 match
  case hydra.core.Term.annotated(v_Term_annotated_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("annotated", hydra.encode.core.annotatedTerm(v_Term_annotated_y))))
  case hydra.core.Term.application(v_Term_application_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("application", hydra.encode.core.application(v_Term_application_y))))
  case hydra.core.Term.cases(v_Term_cases_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("cases", hydra.encode.core.caseStatement(v_Term_cases_y))))
  case hydra.core.Term.either(v_Term_either_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("either", hydra.core.Term.either(hydra.lib.eithers.bimap[hydra.core.Term, hydra.core.Term,
     hydra.core.Term, hydra.core.Term](hydra.encode.core.term)(hydra.encode.core.term)(v_Term_either_y)))))
  case hydra.core.Term.inject(v_Term_inject_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("inject", hydra.encode.core.injection(v_Term_inject_y))))
  case hydra.core.Term.lambda(v_Term_lambda_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("lambda", hydra.encode.core.lambda(v_Term_lambda_y))))
  case hydra.core.Term.let(v_Term_let_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("let", hydra.encode.core.let(v_Term_let_y))))
  case hydra.core.Term.list(v_Term_list_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("list", hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Term, hydra.core.Term](hydra.encode.core.term)(v_Term_list_y)))))
  case hydra.core.Term.literal(v_Term_literal_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("literal", hydra.encode.core.literal(v_Term_literal_y))))
  case hydra.core.Term.map(v_Term_map_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("map", hydra.core.Term.map(hydra.lib.maps.bimap[hydra.core.Term, hydra.core.Term,
     hydra.core.Term, hydra.core.Term](hydra.encode.core.term)(hydra.encode.core.term)(v_Term_map_y)))))
  case hydra.core.Term.maybe(v_Term_maybe_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("maybe", hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.core.Term, hydra.core.Term](hydra.encode.core.term)(v_Term_maybe_y)))))
  case hydra.core.Term.pair(v_Term_pair_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("pair", hydra.core.Term.pair(hydra.lib.pairs.bimap[hydra.core.Term, hydra.core.Term,
     hydra.core.Term, hydra.core.Term](hydra.encode.core.term)(hydra.encode.core.term)(v_Term_pair_y)))))
  case hydra.core.Term.project(v_Term_project_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("project", hydra.encode.core.projection(v_Term_project_y))))
  case hydra.core.Term.record(v_Term_record_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("record", hydra.encode.core.record(v_Term_record_y))))
  case hydra.core.Term.set(v_Term_set_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("set", hydra.core.Term.set(hydra.lib.sets.map[hydra.core.Term, hydra.core.Term](hydra.encode.core.term)(v_Term_set_y)))))
  case hydra.core.Term.typeApplication(v_Term_typeApplication_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("typeApplication", hydra.encode.core.typeApplicationTerm(v_Term_typeApplication_y))))
  case hydra.core.Term.typeLambda(v_Term_typeLambda_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("typeLambda", hydra.encode.core.typeLambda(v_Term_typeLambda_y))))
  case hydra.core.Term.unit => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term", hydra.core.Field("unit", hydra.core.Term.unit)))
  case hydra.core.Term.unwrap(v_Term_unwrap_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("unwrap", hydra.encode.core.name(v_Term_unwrap_y))))
  case hydra.core.Term.variable(v_Term_variable_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("variable", hydra.encode.core.name(v_Term_variable_y))))
  case hydra.core.Term.wrap(v_Term_wrap_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("wrap", hydra.encode.core.wrappedTerm(v_Term_wrap_y))))

def `type`(v1: hydra.core.Type): hydra.core.Term =
  v1 match
  case hydra.core.Type.annotated(v_Type_annotated_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Type",
     hydra.core.Field("annotated", hydra.encode.core.annotatedType(v_Type_annotated_y))))
  case hydra.core.Type.application(v_Type_application_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Type",
     hydra.core.Field("application", hydra.encode.core.applicationType(v_Type_application_y))))
  case hydra.core.Type.either(v_Type_either_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Type",
     hydra.core.Field("either", hydra.encode.core.eitherType(v_Type_either_y))))
  case hydra.core.Type.forall(v_Type_forall_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Type",
     hydra.core.Field("forall", hydra.encode.core.forallType(v_Type_forall_y))))
  case hydra.core.Type.function(v_Type_function_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Type",
     hydra.core.Field("function", hydra.encode.core.functionType(v_Type_function_y))))
  case hydra.core.Type.list(v_Type_list_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Type",
     hydra.core.Field("list", hydra.encode.core.`type`(v_Type_list_y))))
  case hydra.core.Type.literal(v_Type_literal_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Type",
     hydra.core.Field("literal", hydra.encode.core.literalType(v_Type_literal_y))))
  case hydra.core.Type.map(v_Type_map_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Type",
     hydra.core.Field("map", hydra.encode.core.mapType(v_Type_map_y))))
  case hydra.core.Type.maybe(v_Type_maybe_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Type",
     hydra.core.Field("maybe", hydra.encode.core.`type`(v_Type_maybe_y))))
  case hydra.core.Type.pair(v_Type_pair_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Type",
     hydra.core.Field("pair", hydra.encode.core.pairType(v_Type_pair_y))))
  case hydra.core.Type.record(v_Type_record_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Type",
     hydra.core.Field("record", hydra.core.Term.list(hydra.lib.lists.map[hydra.core.FieldType, hydra.core.Term](hydra.encode.core.fieldType)(v_Type_record_y)))))
  case hydra.core.Type.set(v_Type_set_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Type",
     hydra.core.Field("set", hydra.encode.core.`type`(v_Type_set_y))))
  case hydra.core.Type.union(v_Type_union_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Type",
     hydra.core.Field("union", hydra.core.Term.list(hydra.lib.lists.map[hydra.core.FieldType, hydra.core.Term](hydra.encode.core.fieldType)(v_Type_union_y)))))
  case hydra.core.Type.unit => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Type", hydra.core.Field("unit", hydra.core.Term.unit)))
  case hydra.core.Type.variable(v_Type_variable_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Type",
     hydra.core.Field("variable", hydra.encode.core.name(v_Type_variable_y))))
  case hydra.core.Type.void => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Type", hydra.core.Field("void", hydra.core.Term.unit)))
  case hydra.core.Type.wrap(v_Type_wrap_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.core.Type",
     hydra.core.Field("wrap", hydra.encode.core.`type`(v_Type_wrap_y))))

def typeApplicationTerm(x: hydra.core.TypeApplicationTerm): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.TypeApplicationTerm", Seq(hydra.core.Field("body",
     hydra.encode.core.term(x.body)), hydra.core.Field("type", hydra.encode.core.`type`(x.`type`)))))

def typeLambda(x: hydra.core.TypeLambda): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.TypeLambda", Seq(hydra.core.Field("parameter",
     hydra.encode.core.name(x.parameter)), hydra.core.Field("body", hydra.encode.core.term(x.body)))))

def typeScheme(x: hydra.core.TypeScheme): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.TypeScheme", Seq(hydra.core.Field("variables",
     hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Name, hydra.core.Term](hydra.encode.core.name)(x.variables))),
     hydra.core.Field("type", hydra.encode.core.`type`(x.`type`)), hydra.core.Field("constraints", hydra.core.Term.maybe(hydra.lib.maybes.map[Map[hydra.core.Name,
     hydra.core.TypeVariableMetadata], hydra.core.Term]((m: Map[hydra.core.Name, hydra.core.TypeVariableMetadata]) =>
  hydra.core.Term.map(hydra.lib.maps.bimap[hydra.core.Name, hydra.core.Term, hydra.core.TypeVariableMetadata,
     hydra.core.Term](hydra.encode.core.name)(hydra.encode.core.typeVariableMetadata)(m)))(x.constraints))))))

def typeVariableMetadata(x: hydra.core.TypeVariableMetadata): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.TypeVariableMetadata", Seq(hydra.core.Field("classes",
     hydra.core.Term.set(hydra.lib.sets.map[hydra.core.Name, hydra.core.Term](hydra.encode.core.name)(x.classes))))))

def wrappedTerm(x: hydra.core.WrappedTerm): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.WrappedTerm", Seq(hydra.core.Field("typeName",
     hydra.encode.core.name(x.typeName)), hydra.core.Field("body", hydra.encode.core.term(x.body)))))
