package hydra.encoding

import hydra.core.*

import hydra.errors.*

import hydra.packaging.*

def encodeBinding[T0](cx: T0)(graph: hydra.graph.Graph)(b: hydra.core.Binding): Either[hydra.errors.DecodingError, hydra.core.Binding] =
  hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.core.Binding](hydra.decode.core.`type`(graph)(b.term))((typ: hydra.core.Type) =>
  Right(hydra.core.Binding(hydra.encoding.encodeBindingName(b.name), hydra.encoding.encodeTypeNamed(b.name)(typ),
     Some(hydra.encoding.encoderTypeSchemeNamed(b.name)(typ)))))

def encodeBindingName(n: hydra.core.Name): hydra.core.Name =
  hydra.lib.logic.ifElse[hydra.core.Name](hydra.lib.logic.not(hydra.lib.lists.`null`[scala.Predef.String](hydra.lib.lists.tail[scala.Predef.String](hydra.lib.strings.splitOn(".")(n)))))(hydra.lib.strings.intercalate(".")(hydra.lib.lists.concat2[scala.Predef.String](Seq("hydra",
     "encode"))(hydra.lib.lists.concat2[scala.Predef.String](hydra.lib.lists.tail[scala.Predef.String](hydra.lib.lists.init[scala.Predef.String](hydra.lib.strings.splitOn(".")(n))))(Seq(hydra.formatting.decapitalize(hydra.names.localNameOf(n)))))))(hydra.formatting.decapitalize(hydra.names.localNameOf(n)))

def encodeEitherType(et: hydra.core.EitherType): hydra.core.Term =
  hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("e", None, hydra.core.Term.union(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("either", hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.eithers.bimap"),
     hydra.encoding.encodeType(et.left))), hydra.encoding.encodeType(et.right))), hydra.core.Term.variable("e")))))))))

def encodeFieldValue(typeName: hydra.core.Name)(fieldName: hydra.core.Name)(fieldType: hydra.core.Type): hydra.core.Term =
  hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("y", None, hydra.core.Term.union(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("union", hydra.encoding.encodeInjection(typeName)(fieldName)(hydra.core.Term.application(hydra.core.Application(hydra.encoding.encodeType(fieldType),
     hydra.core.Term.variable("y"))))))))))

def encodeFloatValue(floatType: hydra.core.FloatType)(valTerm: hydra.core.Term): hydra.core.Term =
  hydra.core.Term.union(hydra.core.Injection("hydra.core.FloatValue", hydra.core.Field(floatType match
  case hydra.core.FloatType.bigfloat => "bigfloat"
  case hydra.core.FloatType.float32 => "float32"
  case hydra.core.FloatType.float64 => "float64", valTerm)))

def encodeForallType(ft: hydra.core.ForallType): hydra.core.Term =
  hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(hydra.encoding.encodeBindingName(ft.parameter),
     None, hydra.encoding.encodeType(ft.body))))

def encodeInjection(typeName: hydra.core.Name)(fieldName: hydra.core.Name)(fieldTerm: hydra.core.Term): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.core.Injection", Seq(hydra.core.Field("typeName", hydra.encoding.encodeName(typeName)),
     hydra.core.Field("field", hydra.core.Term.record(hydra.core.Record("hydra.core.Field", Seq(hydra.core.Field("name",
     hydra.encoding.encodeName(fieldName)), hydra.core.Field("term", fieldTerm))))))))

def encodeIntegerValue(intType: hydra.core.IntegerType)(valTerm: hydra.core.Term): hydra.core.Term =
  hydra.core.Term.union(hydra.core.Injection("hydra.core.IntegerValue", hydra.core.Field(intType match
  case hydra.core.IntegerType.bigint => "bigint"
  case hydra.core.IntegerType.int8 => "int8"
  case hydra.core.IntegerType.int16 => "int16"
  case hydra.core.IntegerType.int32 => "int32"
  case hydra.core.IntegerType.int64 => "int64"
  case hydra.core.IntegerType.uint8 => "uint8"
  case hydra.core.IntegerType.uint16 => "uint16"
  case hydra.core.IntegerType.uint32 => "uint32"
  case hydra.core.IntegerType.uint64 => "uint64", valTerm)))

def encodeListType(elemType: hydra.core.Type): hydra.core.Term =
  hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("xs", None, hydra.core.Term.union(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("list", hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.map"),
     hydra.encoding.encodeType(elemType))), hydra.core.Term.variable("xs")))))))))

def encodeLiteralType(v1: hydra.core.LiteralType): hydra.core.Term =
  v1 match
  case hydra.core.LiteralType.binary => hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("x",
     None, hydra.core.Term.union(hydra.core.Injection("hydra.core.Term", hydra.core.Field("literal", hydra.core.Term.union(hydra.core.Injection("hydra.core.Literal",
     hydra.core.Field("binary", hydra.core.Term.variable("x"))))))))))
  case hydra.core.LiteralType.boolean => hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("x",
     None, hydra.core.Term.union(hydra.core.Injection("hydra.core.Term", hydra.core.Field("literal", hydra.core.Term.union(hydra.core.Injection("hydra.core.Literal",
     hydra.core.Field("boolean", hydra.core.Term.variable("x"))))))))))
  case hydra.core.LiteralType.string => hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("x",
     None, hydra.core.Term.union(hydra.core.Injection("hydra.core.Term", hydra.core.Field("literal", hydra.core.Term.union(hydra.core.Injection("hydra.core.Literal",
     hydra.core.Field("string", hydra.core.Term.variable("x"))))))))))
  case hydra.core.LiteralType.integer(v_LiteralType_integer_intType) => hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("x",
     None, hydra.core.Term.union(hydra.core.Injection("hydra.core.Term", hydra.core.Field("literal", hydra.core.Term.union(hydra.core.Injection("hydra.core.Literal",
     hydra.core.Field("integer", hydra.encoding.encodeIntegerValue(v_LiteralType_integer_intType)(hydra.core.Term.variable("x")))))))))))
  case hydra.core.LiteralType.float(v_LiteralType_float_floatType) => hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("x",
     None, hydra.core.Term.union(hydra.core.Injection("hydra.core.Term", hydra.core.Field("literal", hydra.core.Term.union(hydra.core.Injection("hydra.core.Literal",
     hydra.core.Field("float", hydra.encoding.encodeFloatValue(v_LiteralType_float_floatType)(hydra.core.Term.variable("x")))))))))))
  case _ => hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("x", None, hydra.core.Term.variable("x"))))

def encodeMapType(mt: hydra.core.MapType): hydra.core.Term =
  hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("m", None, hydra.core.Term.union(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("map", hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.maps.bimap"),
     hydra.encoding.encodeType(mt.keys))), hydra.encoding.encodeType(mt.values))), hydra.core.Term.variable("m")))))))))

def encodeModule(cx: hydra.context.Context)(graph: hydra.graph.Graph)(mod: hydra.packaging.Module): Either[hydra.errors.Error, Option[hydra.packaging.Module]] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Binding], Option[hydra.packaging.Module]](hydra.encoding.filterTypeBindings(cx)(graph)(hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Definition,
     Option[hydra.core.Binding]]((d: hydra.packaging.Definition) =>
  d match
  case hydra.packaging.Definition.`type`(v_Definition_type_td) => Some({
    lazy val schemaTerm: hydra.core.Term = hydra.core.Term.variable("hydra.core.Type")
    {
      lazy val dataTerm: hydra.core.Term = hydra.annotations.normalizeTermAnnotations(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.encode.core.`type`(v_Definition_type_td.`type`.`type`),
         hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Term](Seq(Tuple2(hydra.constants.key_type,
         schemaTerm))))))
      hydra.core.Binding(v_Definition_type_td.name, dataTerm, Some(hydra.core.TypeScheme(Seq(), hydra.core.Type.variable("hydra.core.Type"), None)))
    }
  })
  case _ => None)(mod.definitions))))((typeBindings: Seq[hydra.core.Binding]) =>
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, Option[hydra.packaging.Module]]](hydra.lib.lists.`null`[hydra.core.Binding](typeBindings))(Right(None))(hydra.lib.eithers.bind[hydra.errors.Error,
     Seq[hydra.core.Binding], Option[hydra.packaging.Module]](hydra.lib.eithers.mapList[hydra.core.Binding,
     hydra.core.Binding, hydra.errors.Error]((b: hydra.core.Binding) =>
  hydra.lib.eithers.bimap[hydra.errors.DecodingError, hydra.core.Binding, hydra.errors.Error, hydra.core.Binding]((_e: hydra.errors.DecodingError) => hydra.errors.Error.decoding(_e))((x: hydra.core.Binding) => x)(hydra.encoding.encodeBinding(cx)(graph)(b)))(typeBindings))((encodedBindings: Seq[hydra.core.Binding]) =>
  Right(Some(hydra.packaging.Module(hydra.encoding.encodeNamespace(mod.namespace), hydra.lib.lists.map[hydra.core.Binding,
     hydra.packaging.Definition]((b: hydra.core.Binding) =>
  hydra.packaging.Definition.term(hydra.packaging.TermDefinition(b.name, (b.term), (b.`type`))))(encodedBindings),
     hydra.lib.lists.nub[hydra.packaging.Namespace](hydra.lib.lists.concat2[hydra.packaging.Namespace](hydra.lib.lists.map[hydra.packaging.Namespace,
     hydra.packaging.Namespace](hydra.encoding.encodeNamespace)(mod.typeDependencies))(hydra.lib.lists.map[hydra.packaging.Namespace,
     hydra.packaging.Namespace](hydra.encoding.encodeNamespace)(mod.termDependencies))), Seq(mod.namespace),
     Some(hydra.lib.strings.cat(Seq("Term encoders for ", (mod.namespace))))))))))

def encodeName(n: hydra.core.Name): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.core.Name", hydra.core.Term.literal(hydra.core.Literal.string(n))))

def encodeNamespace(ns: hydra.packaging.Namespace): hydra.packaging.Namespace =
  hydra.lib.strings.cat(Seq("hydra.encode.", hydra.lib.strings.intercalate(".")(hydra.lib.lists.tail[scala.Predef.String](hydra.lib.strings.splitOn(".")(ns)))))

def encodeOptionalType(elemType: hydra.core.Type): hydra.core.Term =
  hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("opt", None, hydra.core.Term.union(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("maybe", hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.maybes.map"),
     hydra.encoding.encodeType(elemType))), hydra.core.Term.variable("opt")))))))))

def encodePairType(pt: hydra.core.PairType): hydra.core.Term =
  hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("p", None, hydra.core.Term.union(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("pair", hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.bimap"),
     hydra.encoding.encodeType(pt.first))), hydra.encoding.encodeType(pt.second))), hydra.core.Term.variable("p")))))))))

def encodeRecordType(rt: Seq[hydra.core.FieldType]): hydra.core.Term = hydra.encoding.encodeRecordTypeNamed("unknown")(rt)

def encodeRecordTypeNamed(ename: hydra.core.Name)(rt: Seq[hydra.core.FieldType]): hydra.core.Term =
  hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("x", None, hydra.core.Term.union(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("record", hydra.core.Term.record(hydra.core.Record("hydra.core.Record", Seq(hydra.core.Field("typeName",
     hydra.encoding.encodeName(ename)), hydra.core.Field("fields", hydra.core.Term.list(hydra.lib.lists.map[hydra.core.FieldType,
     hydra.core.Term]((ft: hydra.core.FieldType) =>
  hydra.core.Term.record(hydra.core.Record("hydra.core.Field", Seq(hydra.core.Field("name", hydra.encoding.encodeName(ft.name)),
     hydra.core.Field("term", hydra.core.Term.application(hydra.core.Application(hydra.encoding.encodeType(ft.`type`),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.function(hydra.core.Function.elimination(hydra.core.Elimination.record(hydra.core.Projection(ename,
     (ft.name))))), hydra.core.Term.variable("x"))))))))))(rt))))))))))))

def encodeSetType(elemType: hydra.core.Type): hydra.core.Term =
  hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("s", None, hydra.core.Term.union(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("set", hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.sets.map"),
     hydra.encoding.encodeType(elemType))), hydra.core.Term.variable("s")))))))))

def encodeType(v1: hydra.core.Type): hydra.core.Term =
  v1 match
  case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.encoding.encodeType(v_Type_annotated_at.body)
  case hydra.core.Type.application(v_Type_application_appType) => hydra.core.Term.application(hydra.core.Application(hydra.encoding.encodeType(v_Type_application_appType.function),
     hydra.encoding.encodeType(v_Type_application_appType.argument)))
  case hydra.core.Type.either(v_Type_either_et) => hydra.encoding.encodeEitherType(v_Type_either_et)
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.encoding.encodeForallType(v_Type_forall_ft)
  case hydra.core.Type.function(v_Type_function__) => hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("x",
     None, hydra.core.Term.variable("x"))))
  case hydra.core.Type.list(v_Type_list_elemType) => hydra.encoding.encodeListType(v_Type_list_elemType)
  case hydra.core.Type.literal(v_Type_literal_lt) => hydra.encoding.encodeLiteralType(v_Type_literal_lt)
  case hydra.core.Type.map(v_Type_map_mt) => hydra.encoding.encodeMapType(v_Type_map_mt)
  case hydra.core.Type.maybe(v_Type_maybe_elemType) => hydra.encoding.encodeOptionalType(v_Type_maybe_elemType)
  case hydra.core.Type.pair(v_Type_pair_pt) => hydra.encoding.encodePairType(v_Type_pair_pt)
  case hydra.core.Type.record(v_Type_record_rt) => hydra.encoding.encodeRecordType(v_Type_record_rt)
  case hydra.core.Type.set(v_Type_set_elemType) => hydra.encoding.encodeSetType(v_Type_set_elemType)
  case hydra.core.Type.union(v_Type_union_rt) => hydra.encoding.encodeUnionType(v_Type_union_rt)
  case hydra.core.Type.wrap(v_Type_wrap_wt) => hydra.encoding.encodeWrappedType(v_Type_wrap_wt)
  case hydra.core.Type.unit => hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("_",
     None, hydra.core.Term.union(hydra.core.Injection("hydra.core.Term", hydra.core.Field("unit", hydra.core.Term.unit))))))
  case hydra.core.Type.void => hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("_",
     None, hydra.core.Term.union(hydra.core.Injection("hydra.core.Term", hydra.core.Field("unit", hydra.core.Term.unit))))))
  case hydra.core.Type.variable(v_Type_variable_typeName) => hydra.core.Term.variable(hydra.encoding.encodeBindingName(v_Type_variable_typeName))
  case _ => hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("x", None, hydra.core.Term.variable("x"))))

def encodeTypeNamed(ename: hydra.core.Name)(typ: hydra.core.Type): hydra.core.Term =
  typ match
  case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.encoding.encodeTypeNamed(ename)(v_Type_annotated_at.body)
  case hydra.core.Type.application(v_Type_application_appType) => hydra.core.Term.application(hydra.core.Application(hydra.encoding.encodeType(v_Type_application_appType.function),
     hydra.encoding.encodeType(v_Type_application_appType.argument)))
  case hydra.core.Type.either(v_Type_either_et) => hydra.encoding.encodeEitherType(v_Type_either_et)
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(hydra.encoding.encodeBindingName(v_Type_forall_ft.parameter),
     None, hydra.encoding.encodeTypeNamed(ename)(v_Type_forall_ft.body))))
  case hydra.core.Type.function(v_Type_function__) => hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("x",
     None, hydra.core.Term.variable("x"))))
  case hydra.core.Type.list(v_Type_list_elemType) => hydra.encoding.encodeListType(v_Type_list_elemType)
  case hydra.core.Type.literal(v_Type_literal_lt) => hydra.encoding.encodeLiteralType(v_Type_literal_lt)
  case hydra.core.Type.map(v_Type_map_mt) => hydra.encoding.encodeMapType(v_Type_map_mt)
  case hydra.core.Type.maybe(v_Type_maybe_elemType) => hydra.encoding.encodeOptionalType(v_Type_maybe_elemType)
  case hydra.core.Type.pair(v_Type_pair_pt) => hydra.encoding.encodePairType(v_Type_pair_pt)
  case hydra.core.Type.record(v_Type_record_rt) => hydra.encoding.encodeRecordTypeNamed(ename)(v_Type_record_rt)
  case hydra.core.Type.set(v_Type_set_elemType) => hydra.encoding.encodeSetType(v_Type_set_elemType)
  case hydra.core.Type.union(v_Type_union_rt) => hydra.encoding.encodeUnionTypeNamed(ename)(v_Type_union_rt)
  case hydra.core.Type.wrap(v_Type_wrap_wt) => hydra.encoding.encodeWrappedTypeNamed(ename)(v_Type_wrap_wt)
  case hydra.core.Type.unit => hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("_",
     None, hydra.core.Term.union(hydra.core.Injection("hydra.core.Term", hydra.core.Field("unit", hydra.core.Term.unit))))))
  case hydra.core.Type.void => hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("_",
     None, hydra.core.Term.union(hydra.core.Injection("hydra.core.Term", hydra.core.Field("unit", hydra.core.Term.unit))))))
  case hydra.core.Type.variable(v_Type_variable_typeName) => hydra.core.Term.variable(hydra.encoding.encodeBindingName(v_Type_variable_typeName))
  case _ => hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("x", None, hydra.core.Term.variable("x"))))

def encodeUnionType(rt: Seq[hydra.core.FieldType]): hydra.core.Term = hydra.encoding.encodeUnionTypeNamed("unknown")(rt)

def encodeUnionTypeNamed(ename: hydra.core.Name)(rt: Seq[hydra.core.FieldType]): hydra.core.Term =
  hydra.core.Term.function(hydra.core.Function.elimination(hydra.core.Elimination.union(hydra.core.CaseStatement(ename,
     None, hydra.lib.lists.map[hydra.core.FieldType, hydra.core.Field]((ft: hydra.core.FieldType) =>
  hydra.core.Field(ft.name, hydra.encoding.encodeFieldValue(ename)(ft.name)(ft.`type`)))(rt)))))

def encodeWrappedType(wt: hydra.core.Type): hydra.core.Term = hydra.encoding.encodeWrappedTypeNamed("unknown")(wt)

def encodeWrappedTypeNamed(ename: hydra.core.Name)(wt: hydra.core.Type): hydra.core.Term =
  hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda("x", None, hydra.core.Term.union(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("wrap", hydra.core.Term.record(hydra.core.Record("hydra.core.WrappedTerm", Seq(hydra.core.Field("typeName",
     hydra.encoding.encodeName(ename)), hydra.core.Field("body", hydra.core.Term.application(hydra.core.Application(hydra.encoding.encodeType(wt),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.function(hydra.core.Function.elimination(hydra.core.Elimination.wrap(ename))),
     hydra.core.Term.variable("x")))))))))))))))

def encoderCollectForallVariables(typ: hydra.core.Type): Seq[hydra.core.Name] =
  typ match
  case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.encoding.encoderCollectForallVariables(v_Type_annotated_at.body)
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.lib.lists.cons[hydra.core.Name](v_Type_forall_ft.parameter)(hydra.encoding.encoderCollectForallVariables(v_Type_forall_ft.body))
  case _ => Seq()

def encoderCollectOrdVars(typ: hydra.core.Type): Seq[hydra.core.Name] =
  typ match
  case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.encoding.encoderCollectOrdVars(v_Type_annotated_at.body)
  case hydra.core.Type.application(v_Type_application_appType) => hydra.lib.lists.concat2[hydra.core.Name](hydra.encoding.encoderCollectOrdVars(v_Type_application_appType.function))(hydra.encoding.encoderCollectOrdVars(v_Type_application_appType.argument))
  case hydra.core.Type.either(v_Type_either_et) => hydra.lib.lists.concat2[hydra.core.Name](hydra.encoding.encoderCollectOrdVars(v_Type_either_et.left))(hydra.encoding.encoderCollectOrdVars(v_Type_either_et.right))
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.encoding.encoderCollectOrdVars(v_Type_forall_ft.body)
  case hydra.core.Type.list(v_Type_list_elemType) => hydra.encoding.encoderCollectOrdVars(v_Type_list_elemType)
  case hydra.core.Type.map(v_Type_map_mt) => hydra.lib.lists.concat[hydra.core.Name](Seq(hydra.encoding.encoderCollectTypeVarsFromType(v_Type_map_mt.keys),
     hydra.encoding.encoderCollectOrdVars(v_Type_map_mt.keys), hydra.encoding.encoderCollectOrdVars(v_Type_map_mt.values)))
  case hydra.core.Type.maybe(v_Type_maybe_elemType) => hydra.encoding.encoderCollectOrdVars(v_Type_maybe_elemType)
  case hydra.core.Type.pair(v_Type_pair_pt) => hydra.lib.lists.concat2[hydra.core.Name](hydra.encoding.encoderCollectOrdVars(v_Type_pair_pt.first))(hydra.encoding.encoderCollectOrdVars(v_Type_pair_pt.second))
  case hydra.core.Type.record(v_Type_record_rt) => hydra.lib.lists.concat[hydra.core.Name](hydra.lib.lists.map[hydra.core.FieldType,
     Seq[hydra.core.Name]]((ft: hydra.core.FieldType) => hydra.encoding.encoderCollectOrdVars(ft.`type`))(v_Type_record_rt))
  case hydra.core.Type.set(v_Type_set_elemType) => hydra.lib.lists.concat2[hydra.core.Name](hydra.encoding.encoderCollectTypeVarsFromType(v_Type_set_elemType))(hydra.encoding.encoderCollectOrdVars(v_Type_set_elemType))
  case hydra.core.Type.union(v_Type_union_rt) => hydra.lib.lists.concat[hydra.core.Name](hydra.lib.lists.map[hydra.core.FieldType,
     Seq[hydra.core.Name]]((ft: hydra.core.FieldType) => hydra.encoding.encoderCollectOrdVars(ft.`type`))(v_Type_union_rt))
  case hydra.core.Type.wrap(v_Type_wrap_wt) => hydra.encoding.encoderCollectOrdVars(v_Type_wrap_wt)
  case _ => Seq()

def encoderCollectTypeVarsFromType(typ: hydra.core.Type): Seq[hydra.core.Name] =
  typ match
  case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.encoding.encoderCollectTypeVarsFromType(v_Type_annotated_at.body)
  case hydra.core.Type.application(v_Type_application_appType) => hydra.lib.lists.concat2[hydra.core.Name](hydra.encoding.encoderCollectTypeVarsFromType(v_Type_application_appType.function))(hydra.encoding.encoderCollectTypeVarsFromType(v_Type_application_appType.argument))
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.encoding.encoderCollectTypeVarsFromType(v_Type_forall_ft.body)
  case hydra.core.Type.list(v_Type_list_elemType) => hydra.encoding.encoderCollectTypeVarsFromType(v_Type_list_elemType)
  case hydra.core.Type.map(v_Type_map_mt) => hydra.lib.lists.concat2[hydra.core.Name](hydra.encoding.encoderCollectTypeVarsFromType(v_Type_map_mt.keys))(hydra.encoding.encoderCollectTypeVarsFromType(v_Type_map_mt.values))
  case hydra.core.Type.maybe(v_Type_maybe_elemType) => hydra.encoding.encoderCollectTypeVarsFromType(v_Type_maybe_elemType)
  case hydra.core.Type.pair(v_Type_pair_pt) => hydra.lib.lists.concat2[hydra.core.Name](hydra.encoding.encoderCollectTypeVarsFromType(v_Type_pair_pt.first))(hydra.encoding.encoderCollectTypeVarsFromType(v_Type_pair_pt.second))
  case hydra.core.Type.record(v_Type_record_rt) => hydra.lib.lists.concat[hydra.core.Name](hydra.lib.lists.map[hydra.core.FieldType,
     Seq[hydra.core.Name]]((ft: hydra.core.FieldType) => hydra.encoding.encoderCollectTypeVarsFromType(ft.`type`))(v_Type_record_rt))
  case hydra.core.Type.set(v_Type_set_elemType) => hydra.encoding.encoderCollectTypeVarsFromType(v_Type_set_elemType)
  case hydra.core.Type.union(v_Type_union_rt) => hydra.lib.lists.concat[hydra.core.Name](hydra.lib.lists.map[hydra.core.FieldType,
     Seq[hydra.core.Name]]((ft: hydra.core.FieldType) => hydra.encoding.encoderCollectTypeVarsFromType(ft.`type`))(v_Type_union_rt))
  case hydra.core.Type.variable(v_Type_variable_name) => Seq(v_Type_variable_name)
  case hydra.core.Type.wrap(v_Type_wrap_wt) => hydra.encoding.encoderCollectTypeVarsFromType(v_Type_wrap_wt)
  case _ => Seq()

def encoderFullResultType(typ: hydra.core.Type): hydra.core.Type =
  typ match
  case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.encoding.encoderFullResultType(v_Type_annotated_at.body)
  case hydra.core.Type.application(v_Type_application_appType) => hydra.core.Type.application(hydra.core.ApplicationType(hydra.encoding.encoderFullResultType(v_Type_application_appType.function),
     (v_Type_application_appType.argument)))
  case hydra.core.Type.either(v_Type_either_et) => hydra.core.Type.either(hydra.core.EitherType(hydra.encoding.encoderFullResultType(v_Type_either_et.left),
     hydra.encoding.encoderFullResultType(v_Type_either_et.right)))
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.core.Type.application(hydra.core.ApplicationType(hydra.encoding.encoderFullResultType(v_Type_forall_ft.body),
     hydra.core.Type.variable(v_Type_forall_ft.parameter)))
  case hydra.core.Type.list(v_Type_list_elemType) => hydra.core.Type.list(hydra.encoding.encoderFullResultType(v_Type_list_elemType))
  case hydra.core.Type.literal(v_Type_literal__) => hydra.core.Type.variable("hydra.core.Literal")
  case hydra.core.Type.map(v_Type_map_mt) => hydra.core.Type.map(hydra.core.MapType(hydra.encoding.encoderFullResultType(v_Type_map_mt.keys),
     hydra.encoding.encoderFullResultType(v_Type_map_mt.values)))
  case hydra.core.Type.maybe(v_Type_maybe_elemType) => hydra.core.Type.maybe(hydra.encoding.encoderFullResultType(v_Type_maybe_elemType))
  case hydra.core.Type.pair(v_Type_pair_pt) => hydra.core.Type.pair(hydra.core.PairType(hydra.encoding.encoderFullResultType(v_Type_pair_pt.first),
     hydra.encoding.encoderFullResultType(v_Type_pair_pt.second)))
  case hydra.core.Type.record(v_Type_record__) => hydra.core.Type.variable("hydra.core.Term")
  case hydra.core.Type.set(v_Type_set_elemType) => hydra.core.Type.set(hydra.encoding.encoderFullResultType(v_Type_set_elemType))
  case hydra.core.Type.union(v_Type_union__) => hydra.core.Type.variable("hydra.core.Term")
  case hydra.core.Type.unit => hydra.core.Type.unit
  case hydra.core.Type.variable(v_Type_variable_name) => hydra.core.Type.variable(v_Type_variable_name)
  case hydra.core.Type.void => hydra.core.Type.void
  case hydra.core.Type.wrap(v_Type_wrap__) => hydra.core.Type.variable("hydra.core.Term")
  case _ => hydra.core.Type.variable("hydra.core.Term")

def encoderFullResultTypeNamed(ename: hydra.core.Name)(typ: hydra.core.Type): hydra.core.Type =
  typ match
  case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.encoding.encoderFullResultTypeNamed(ename)(v_Type_annotated_at.body)
  case hydra.core.Type.application(v_Type_application_appType) => hydra.core.Type.application(hydra.core.ApplicationType(hydra.encoding.encoderFullResultType(v_Type_application_appType.function),
     (v_Type_application_appType.argument)))
  case hydra.core.Type.either(v_Type_either_et) => hydra.core.Type.either(hydra.core.EitherType(hydra.encoding.encoderFullResultType(v_Type_either_et.left),
     hydra.encoding.encoderFullResultType(v_Type_either_et.right)))
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.core.Type.application(hydra.core.ApplicationType(hydra.encoding.encoderFullResultTypeNamed(ename)(v_Type_forall_ft.body),
     hydra.core.Type.variable(v_Type_forall_ft.parameter)))
  case hydra.core.Type.list(v_Type_list_elemType) => hydra.core.Type.list(hydra.encoding.encoderFullResultType(v_Type_list_elemType))
  case hydra.core.Type.literal(v_Type_literal__) => hydra.core.Type.variable("hydra.core.Literal")
  case hydra.core.Type.map(v_Type_map_mt) => hydra.core.Type.map(hydra.core.MapType(hydra.encoding.encoderFullResultType(v_Type_map_mt.keys),
     hydra.encoding.encoderFullResultType(v_Type_map_mt.values)))
  case hydra.core.Type.maybe(v_Type_maybe_elemType) => hydra.core.Type.maybe(hydra.encoding.encoderFullResultType(v_Type_maybe_elemType))
  case hydra.core.Type.pair(v_Type_pair_pt) => hydra.core.Type.pair(hydra.core.PairType(hydra.encoding.encoderFullResultType(v_Type_pair_pt.first),
     hydra.encoding.encoderFullResultType(v_Type_pair_pt.second)))
  case hydra.core.Type.record(v_Type_record__) => hydra.core.Type.variable(ename)
  case hydra.core.Type.set(v_Type_set_elemType) => hydra.core.Type.set(hydra.encoding.encoderFullResultType(v_Type_set_elemType))
  case hydra.core.Type.union(v_Type_union__) => hydra.core.Type.variable(ename)
  case hydra.core.Type.unit => hydra.core.Type.unit
  case hydra.core.Type.variable(v_Type_variable_name) => hydra.core.Type.variable(v_Type_variable_name)
  case hydra.core.Type.void => hydra.core.Type.void
  case hydra.core.Type.wrap(v_Type_wrap__) => hydra.core.Type.variable(ename)
  case _ => hydra.core.Type.variable("hydra.core.Term")

def encoderType(typ: hydra.core.Type): hydra.core.Type =
  {
  lazy val resultType: hydra.core.Type = hydra.encoding.encoderFullResultType(typ)
  lazy val baseType: hydra.core.Type = hydra.core.Type.function(hydra.core.FunctionType(resultType, hydra.core.Type.variable("hydra.core.Term")))
  hydra.encoding.prependForallEncoders(baseType)(typ)
}

def encoderTypeNamed(ename: hydra.core.Name)(typ: hydra.core.Type): hydra.core.Type =
  {
  lazy val resultType: hydra.core.Type = hydra.encoding.encoderFullResultTypeNamed(ename)(typ)
  lazy val baseType: hydra.core.Type = hydra.core.Type.function(hydra.core.FunctionType(resultType, hydra.core.Type.variable("hydra.core.Term")))
  hydra.encoding.prependForallEncoders(baseType)(typ)
}

def encoderTypeScheme(typ: hydra.core.Type): hydra.core.TypeScheme =
  {
  lazy val typeVars: Seq[hydra.core.Name] = hydra.encoding.encoderCollectForallVariables(typ)
  lazy val encoderFunType: hydra.core.Type = hydra.encoding.encoderType(typ)
  lazy val allOrdVars: Seq[hydra.core.Name] = hydra.encoding.encoderCollectOrdVars(typ)
  lazy val ordVars: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) => hydra.lib.lists.elem[hydra.core.Name](v)(typeVars))(allOrdVars)
  lazy val constraints: Option[Map[hydra.core.Name, hydra.core.TypeVariableMetadata]] = hydra.lib.logic.ifElse[Option[Map[hydra.core.Name,
     hydra.core.TypeVariableMetadata]]](hydra.lib.lists.`null`[hydra.core.Name](ordVars))(None)(Some(hydra.lib.maps.fromList[hydra.core.Name,
     hydra.core.TypeVariableMetadata](hydra.lib.lists.map[hydra.core.Name, Tuple2[hydra.core.Name, hydra.core.TypeVariableMetadata]]((v: hydra.core.Name) =>
    Tuple2(v, hydra.core.TypeVariableMetadata(hydra.lib.sets.singleton[hydra.core.Name]("ordering"))))(ordVars))))
  hydra.core.TypeScheme(typeVars, encoderFunType, constraints)
}

def encoderTypeSchemeNamed(ename: hydra.core.Name)(typ: hydra.core.Type): hydra.core.TypeScheme =
  {
  lazy val typeVars: Seq[hydra.core.Name] = hydra.encoding.encoderCollectForallVariables(typ)
  lazy val encoderFunType: hydra.core.Type = hydra.encoding.encoderTypeNamed(ename)(typ)
  lazy val allOrdVars: Seq[hydra.core.Name] = hydra.encoding.encoderCollectOrdVars(typ)
  lazy val ordVars: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) => hydra.lib.lists.elem[hydra.core.Name](v)(typeVars))(allOrdVars)
  lazy val constraints: Option[Map[hydra.core.Name, hydra.core.TypeVariableMetadata]] = hydra.lib.logic.ifElse[Option[Map[hydra.core.Name,
     hydra.core.TypeVariableMetadata]]](hydra.lib.lists.`null`[hydra.core.Name](ordVars))(None)(Some(hydra.lib.maps.fromList[hydra.core.Name,
     hydra.core.TypeVariableMetadata](hydra.lib.lists.map[hydra.core.Name, Tuple2[hydra.core.Name, hydra.core.TypeVariableMetadata]]((v: hydra.core.Name) =>
    Tuple2(v, hydra.core.TypeVariableMetadata(hydra.lib.sets.singleton[hydra.core.Name]("ordering"))))(ordVars))))
  hydra.core.TypeScheme(typeVars, encoderFunType, constraints)
}

def filterTypeBindings(cx: hydra.context.Context)(graph: hydra.graph.Graph)(bindings: Seq[hydra.core.Binding]): Either[hydra.errors.Error,
   Seq[hydra.core.Binding]] =
  hydra.lib.eithers.map[Seq[Option[hydra.core.Binding]], Seq[hydra.core.Binding], hydra.errors.Error](hydra.lib.maybes.cat[hydra.core.Binding])(hydra.lib.eithers.mapList[hydra.core.Binding,
     Option[hydra.core.Binding], hydra.errors.Error]((v1: hydra.core.Binding) => hydra.encoding.isEncodableBinding(cx)(graph)(v1))(hydra.lib.lists.filter[hydra.core.Binding](hydra.annotations.isNativeType)(bindings)))

def isEncodableBinding(cx: hydra.context.Context)(graph: hydra.graph.Graph)(b: hydra.core.Binding): Either[hydra.errors.Error, Option[hydra.core.Binding]] =
  hydra.lib.eithers.bind[hydra.errors.Error, Boolean, Option[hydra.core.Binding]](hydra.predicates.isSerializableByName(cx)(graph)(b.name))((serializable: Boolean) =>
  Right(hydra.lib.logic.ifElse[Option[hydra.core.Binding]](serializable)(Some(b))(None)))

def isUnitType(v1: hydra.core.Type): Boolean =
  v1 match
  case hydra.core.Type.unit => true
  case _ => false

def prependForallEncoders(baseType: hydra.core.Type)(typ: hydra.core.Type): hydra.core.Type =
  typ match
  case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.encoding.prependForallEncoders(baseType)(v_Type_annotated_at.body)
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable(v_Type_forall_ft.parameter),
     hydra.core.Type.variable("hydra.core.Term"))), hydra.encoding.prependForallEncoders(baseType)(v_Type_forall_ft.body)))
  case _ => baseType
