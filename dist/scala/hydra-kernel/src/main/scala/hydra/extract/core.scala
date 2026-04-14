package hydra.extract.core

import hydra.core.*

import hydra.errors.*

def bigfloat(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.errors.Error, BigDecimal] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Literal, BigDecimal](hydra.extract.core.literal(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.FloatValue, BigDecimal](hydra.extract.core.floatLiteral(l))((f: hydra.core.FloatValue) => hydra.extract.core.bigfloatValue(f)))

def bigfloatValue(v: hydra.core.FloatValue): Either[hydra.errors.Error, BigDecimal] =
  v match
  case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat_f) => Right(v_FloatValue_bigfloat_f)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("bigfloat",
     hydra.show.core.float(v)))))

def bigint(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.errors.Error, BigInt] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Literal, BigInt](hydra.extract.core.literal(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.IntegerValue, BigInt](hydra.extract.core.integerLiteral(l))((i: hydra.core.IntegerValue) => hydra.extract.core.bigintValue(i)))

def bigintValue(v: hydra.core.IntegerValue): Either[hydra.errors.Error, BigInt] =
  v match
  case hydra.core.IntegerValue.bigint(v_IntegerValue_bigint_i) => Right(v_IntegerValue_bigint_i)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("bigint",
     hydra.show.core.integer(v)))))

def binary(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.errors.Error, scala.Predef.String] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Literal, scala.Predef.String](hydra.extract.core.literal(graph)(t))((l: hydra.core.Literal) => hydra.extract.core.binaryLiteral(l))

def binaryLiteral(v: hydra.core.Literal): Either[hydra.errors.Error, scala.Predef.String] =
  v match
  case hydra.core.Literal.binary(v_Literal_binary_b) => Right(v_Literal_binary_b)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("binary",
     hydra.show.core.literal(v)))))

def boolean(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.errors.Error, Boolean] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Literal, Boolean](hydra.extract.core.literal(graph)(t))((l: hydra.core.Literal) => hydra.extract.core.booleanLiteral(l))

def booleanLiteral(v: hydra.core.Literal): Either[hydra.errors.Error, Boolean] =
  v match
  case hydra.core.Literal.boolean(v_Literal_boolean_b) => Right(v_Literal_boolean_b)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("boolean",
     hydra.show.core.literal(v)))))

def caseField(name: hydra.core.Name)(n: scala.Predef.String)(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Field] =
  {
  lazy val fieldName: hydra.core.Name = n
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.CaseStatement, hydra.core.Field](hydra.extract.core.cases(name)(graph)(term))((cs: hydra.core.CaseStatement) =>
    {
    lazy val matching: Seq[hydra.core.Field] = hydra.lib.lists.filter[hydra.core.Field]((f: hydra.core.Field) =>
      hydra.lib.equality.equal[scala.Predef.String](f.name)(fieldName))(cs.cases)
    hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.core.Field]](hydra.lib.lists.`null`[hydra.core.Field](matching))(Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("matching case",
       "no matching case")))))(Right(hydra.lib.lists.head[hydra.core.Field](matching)))
  })
}

def cases(name: hydra.core.Name)(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.CaseStatement] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.CaseStatement](hydra.lexical.stripAndDereferenceTerm(graph)(term0))((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.cases(v_Term_cases_cs) => hydra.lib.logic.ifElse[Either[hydra.errors.Error,
     hydra.core.CaseStatement]](hydra.lib.equality.equal[scala.Predef.String](v_Term_cases_cs.typeName)(name))(Right(v_Term_cases_cs))(Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError(hydra.lib.strings.cat2("case statement for type ")(name),
     hydra.show.core.term(term))))))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("case statement",
     hydra.show.core.term(term))))))

def decodeEither[T0, T1](leftDecoder: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError,
   T0]))(rightDecoder: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError,
   T1]))(g: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.DecodingError,
   Either[T0, T1]] =
  hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, Either[T0, T1]](hydra.extract.core.stripWithDecodingError(g)(term))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term,
     hydra.core.Term, Either[hydra.errors.DecodingError, Either[T0, T1]]]((lv: hydra.core.Term) =>
    hydra.lib.eithers.map[T0, Either[T0, T1], hydra.errors.DecodingError]((x: T0) => Left(x))(leftDecoder(g)(lv)))((rv: hydra.core.Term) =>
    hydra.lib.eithers.map[T1, Either[T0, T1], hydra.errors.DecodingError]((x: T1) => Right(x))(rightDecoder(g)(rv)))(v_Term_either_e)
  case _ => Left("expected either value"))

def decodeList[T0](elemDecoder: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError,
   T0]))(g: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.DecodingError,
   Seq[T0]] =
  hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, Seq[T0]](hydra.extract.core.stripWithDecodingError(g)(term))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.list(v_Term_list_els) => hydra.lib.eithers.mapList[hydra.core.Term,
     T0, hydra.errors.DecodingError]((v1: hydra.core.Term) => elemDecoder(g)(v1))(v_Term_list_els)
  case _ => Left("expected list"))

def decodeMap[T0, T1](keyDecoder: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError,
   T0]))(valDecoder: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError,
   T1]))(g: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.DecodingError,
   Map[T0, T1]] =
  hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, Map[T0, T1]](hydra.extract.core.stripWithDecodingError(g)(term))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.map(v_Term_map_m) => hydra.lib.eithers.map[Seq[Tuple2[T0, T1]],
     Map[T0, T1], hydra.errors.DecodingError](hydra.lib.maps.fromList[T0, T1])(hydra.lib.eithers.mapList[Tuple2[hydra.core.Term,
     hydra.core.Term], Tuple2[T0, T1], hydra.errors.DecodingError]((kv: Tuple2[hydra.core.Term,
     hydra.core.Term]) =>
    hydra.lib.eithers.bind[hydra.errors.DecodingError, T0, Tuple2[T0, T1]](keyDecoder(g)(hydra.lib.pairs.first[hydra.core.Term,
       hydra.core.Term](kv)))((k: T0) =>
    hydra.lib.eithers.map[T1, Tuple2[T0, T1], hydra.errors.DecodingError]((v: T1) => Tuple2(k,
       v))(valDecoder(g)(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](kv)))))(hydra.lib.maps.toList[hydra.core.Term,
       hydra.core.Term](v_Term_map_m)))
  case _ => Left("expected map"))

def decodeMaybe[T0](elemDecoder: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError,
   T0]))(g: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.DecodingError,
   Option[T0]] =
  hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, Option[T0]](hydra.extract.core.stripWithDecodingError(g)(term))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.maybe(v_Term_maybe_opt) => hydra.lib.eithers.mapMaybe[hydra.core.Term,
     T0, hydra.errors.DecodingError]((v1: hydra.core.Term) => elemDecoder(g)(v1))(v_Term_maybe_opt)
  case _ => Left("expected optional value"))

def decodePair[T0, T1](firstDecoder: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError,
   T0]))(secondDecoder: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError,
   T1]))(g: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.DecodingError,
   Tuple2[T0, T1]] =
  hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, Tuple2[T0, T1]](hydra.extract.core.stripWithDecodingError(g)(term))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.eithers.bind[hydra.errors.DecodingError,
     T0, Tuple2[T0, T1]](firstDecoder(g)(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p)))((f: T0) =>
    hydra.lib.eithers.map[T1, Tuple2[T0, T1], hydra.errors.DecodingError]((s: T1) => Tuple2(f,
       s))(secondDecoder(g)(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p))))
  case _ => Left("expected pair"))

def decodeSet[T0](elemDecoder: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError,
   T0]))(g: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.DecodingError,
   scala.collection.immutable.Set[T0]] =
  hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, scala.collection.immutable.Set[T0]](hydra.extract.core.stripWithDecodingError(g)(term))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.set(v_Term_set_s) => hydra.lib.eithers.map[Seq[T0], scala.collection.immutable.Set[T0],
     hydra.errors.DecodingError](hydra.lib.sets.fromList[T0])(hydra.lib.eithers.mapList[hydra.core.Term,
     T0, hydra.errors.DecodingError]((v1: hydra.core.Term) => elemDecoder(g)(v1))(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s)))
  case _ => Left("expected set"))

def decodeUnit(g: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.DecodingError, Unit] =
  hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, Unit](hydra.extract.core.stripWithDecodingError(g)(term))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.unit => Right(())
  case _ => Left("expected a unit value"))

def decodeWrapped[T0](bodyDecoder: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError,
   T0]))(g: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.DecodingError,
   T0] =
  hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, T0](hydra.extract.core.stripWithDecodingError(g)(term))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wt) => bodyDecoder(g)(v_Term_wrap_wt.body)
  case _ => Left("expected wrapped value"))

def eitherTerm[T0, T1](leftFun: (hydra.core.Term => Either[hydra.errors.Error, T0]))(rightFun: (hydra.core.Term => Either[hydra.errors.Error,
   T1]))(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.errors.Error,
   Either[T0, T1]] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, Either[T0, T1]](hydra.lexical.stripAndDereferenceTerm(graph)(term0))((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.either(v_Term_either_et) => hydra.lib.eithers.either[hydra.core.Term,
     hydra.core.Term, Either[hydra.errors.Error, Either[T0, T1]]]((l: hydra.core.Term) =>
    hydra.lib.eithers.map[T0, Either[T0, T1], hydra.errors.Error]((x: T0) => Left(x))(leftFun(l)))((r: hydra.core.Term) =>
    hydra.lib.eithers.map[T1, Either[T0, T1], hydra.errors.Error]((x: T1) => Right(x))(rightFun(r)))(v_Term_either_et)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("either value",
     hydra.show.core.term(term))))))

def eitherType(typ: hydra.core.Type): Either[hydra.errors.Error, hydra.core.EitherType] =
  {
  lazy val stripped: hydra.core.Type = hydra.strip.deannotateType(typ)
  stripped match
    case hydra.core.Type.either(v_Type_either_et) => Right(v_Type_either_et)
    case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("either type",
       hydra.show.core.`type`(typ)))))
}

def field[T0](fname: hydra.core.Name)(mapping: (hydra.core.Term => Either[hydra.errors.Error,
   T0]))(graph: hydra.graph.Graph)(fields: Seq[hydra.core.Field]): Either[hydra.errors.Error,
   T0] =
  {
  lazy val matchingFields: Seq[hydra.core.Field] = hydra.lib.lists.filter[hydra.core.Field]((f: hydra.core.Field) => hydra.lib.equality.equal[scala.Predef.String](f.name)(fname))(fields)
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, T0]](hydra.lib.lists.`null`[hydra.core.Field](matchingFields))(Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError(hydra.lib.strings.cat2("field ")(fname),
     "no matching field")))))(hydra.lib.logic.ifElse[Either[hydra.errors.Error, T0]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Field](matchingFields))(1))(hydra.lib.eithers.bind[hydra.errors.Error,
     hydra.core.Term, T0](hydra.lexical.stripAndDereferenceTerm(graph)(hydra.lib.lists.head[hydra.core.Field](matchingFields).term))((stripped: hydra.core.Term) => mapping(stripped)))(Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("single field",
     hydra.lib.strings.cat2("multiple fields named ")(fname)))))))
}

def float32(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.errors.Error, Float] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Literal, Float](hydra.extract.core.literal(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.FloatValue, Float](hydra.extract.core.floatLiteral(l))((f: hydra.core.FloatValue) => hydra.extract.core.float32Value(f)))

def float32Value(v: hydra.core.FloatValue): Either[hydra.errors.Error, Float] =
  v match
  case hydra.core.FloatValue.float32(v_FloatValue_float32_f) => Right(v_FloatValue_float32_f)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("float32",
     hydra.show.core.float(v)))))

def float64(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.errors.Error, Double] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Literal, Double](hydra.extract.core.literal(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.FloatValue, Double](hydra.extract.core.floatLiteral(l))((f: hydra.core.FloatValue) => hydra.extract.core.float64Value(f)))

def float64Value(v: hydra.core.FloatValue): Either[hydra.errors.Error, Double] =
  v match
  case hydra.core.FloatValue.float64(v_FloatValue_float64_f) => Right(v_FloatValue_float64_f)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("float64",
     hydra.show.core.float(v)))))

def floatLiteral(lit: hydra.core.Literal): Either[hydra.errors.Error, hydra.core.FloatValue] =
  lit match
  case hydra.core.Literal.float(v_Literal_float_v) => Right(v_Literal_float_v)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("floating-point value",
     hydra.show.core.literal(lit)))))

def floatValue(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.errors.Error, hydra.core.FloatValue] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Literal, hydra.core.FloatValue](hydra.extract.core.literal(graph)(t))((l: hydra.core.Literal) => hydra.extract.core.floatLiteral(l))

def functionType(typ: hydra.core.Type): Either[hydra.errors.Error, hydra.core.FunctionType] =
  {
  lazy val stripped: hydra.core.Type = hydra.strip.deannotateType(typ)
  stripped match
    case hydra.core.Type.function(v_Type_function_ft) => Right(v_Type_function_ft)
    case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("function type",
       hydra.show.core.`type`(typ)))))
}

def injection(expected: hydra.core.Name)(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Field] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Field](hydra.lexical.stripAndDereferenceTerm(graph)(term0))((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.inject(v_Term_inject_injection) => hydra.lib.logic.ifElse[Either[hydra.errors.Error,
     hydra.core.Field]](hydra.lib.equality.equal[scala.Predef.String](v_Term_inject_injection.typeName)(expected))(Right(v_Term_inject_injection.field))(Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError(hydra.lib.strings.cat2("injection of type ")(expected),
     (v_Term_inject_injection.typeName))))))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("injection",
     hydra.show.core.term(term))))))

def int16(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.errors.Error, Short] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Literal, Short](hydra.extract.core.literal(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.IntegerValue, Short](hydra.extract.core.integerLiteral(l))((i: hydra.core.IntegerValue) => hydra.extract.core.int16Value(i)))

def int16Value(v: hydra.core.IntegerValue): Either[hydra.errors.Error, Short] =
  v match
  case hydra.core.IntegerValue.int16(v_IntegerValue_int16_i) => Right(v_IntegerValue_int16_i)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("int16",
     hydra.show.core.integer(v)))))

def int32(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.errors.Error, Int] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Literal, Int](hydra.extract.core.literal(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.IntegerValue, Int](hydra.extract.core.integerLiteral(l))((i: hydra.core.IntegerValue) => hydra.extract.core.int32Value(i)))

def int32Value(v: hydra.core.IntegerValue): Either[hydra.errors.Error, Int] =
  v match
  case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("int32",
     hydra.show.core.integer(v)))))

def int64(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.errors.Error, Long] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Literal, Long](hydra.extract.core.literal(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.IntegerValue, Long](hydra.extract.core.integerLiteral(l))((i: hydra.core.IntegerValue) => hydra.extract.core.int64Value(i)))

def int64Value(v: hydra.core.IntegerValue): Either[hydra.errors.Error, Long] =
  v match
  case hydra.core.IntegerValue.int64(v_IntegerValue_int64_i) => Right(v_IntegerValue_int64_i)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("int64",
     hydra.show.core.integer(v)))))

def int8(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.errors.Error, Byte] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Literal, Byte](hydra.extract.core.literal(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.IntegerValue, Byte](hydra.extract.core.integerLiteral(l))((i: hydra.core.IntegerValue) => hydra.extract.core.int8Value(i)))

def int8Value(v: hydra.core.IntegerValue): Either[hydra.errors.Error, Byte] =
  v match
  case hydra.core.IntegerValue.int8(v_IntegerValue_int8_i) => Right(v_IntegerValue_int8_i)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("int8",
     hydra.show.core.integer(v)))))

def integerLiteral(lit: hydra.core.Literal): Either[hydra.errors.Error, hydra.core.IntegerValue] =
  lit match
  case hydra.core.Literal.integer(v_Literal_integer_v) => Right(v_Literal_integer_v)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("integer value",
     hydra.show.core.literal(lit)))))

def integerValue(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.errors.Error, hydra.core.IntegerValue] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Literal, hydra.core.IntegerValue](hydra.extract.core.literal(graph)(t))((l: hydra.core.Literal) => hydra.extract.core.integerLiteral(l))

def lambda(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Lambda] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Lambda](hydra.lexical.stripAndDereferenceTerm(graph)(term0))((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.lambda(v_Term_lambda_l) => Right(v_Term_lambda_l)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("lambda",
     hydra.show.core.term(term))))))

def lambdaBody(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Term] =
  hydra.lib.eithers.map[hydra.core.Lambda, hydra.core.Term, hydra.errors.Error]((x: hydra.core.Lambda) => (x.body))(hydra.extract.core.lambda(graph)(term))

def let(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Let] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Let](hydra.lexical.stripAndDereferenceTerm(graph)(term0))((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.let(v_Term_let_lt) => Right(v_Term_let_lt)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("let term",
     hydra.show.core.term(term))))))

def letBinding(n: scala.Predef.String)(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  {
  lazy val name: hydra.core.Name = n
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Let, hydra.core.Term](hydra.extract.core.let(graph)(term))((letExpr: hydra.core.Let) =>
    {
    lazy val matchingBindings: Seq[hydra.core.Binding] = hydra.lib.lists.filter[hydra.core.Binding]((b: hydra.core.Binding) => hydra.lib.equality.equal[scala.Predef.String](b.name)(name))(letExpr.bindings)
    hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.core.Term]](hydra.lib.lists.`null`[hydra.core.Binding](matchingBindings))(Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.noSuchBinding(hydra.errors.NoSuchBindingError(name)))))(hydra.lib.logic.ifElse[Either[hydra.errors.Error,
       hydra.core.Term]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Binding](matchingBindings))(1))(Right(hydra.lib.lists.head[hydra.core.Binding](matchingBindings).term))(Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.multipleBindings(hydra.errors.MultipleBindingsError(name))))))
  })
}

def list(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.Error, Seq[hydra.core.Term]] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, Seq[hydra.core.Term]](hydra.lexical.stripAndDereferenceTerm(graph)(term))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.list(v_Term_list_l) => Right(v_Term_list_l)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("list",
     hydra.show.core.term(stripped))))))

def listHead(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(graph)(term))((l: Seq[hydra.core.Term]) =>
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.core.Term]](hydra.lib.lists.`null`[hydra.core.Term](l))(Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("non-empty list",
     "empty list")))))(Right(hydra.lib.lists.head[hydra.core.Term](l))))

def listOf[T0](f: (hydra.core.Term => Either[hydra.errors.Error, T0]))(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.Error,
   Seq[T0]] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], Seq[T0]](hydra.extract.core.list(graph)(term))((els: Seq[hydra.core.Term]) =>
  hydra.lib.eithers.mapList[hydra.core.Term, T0, hydra.errors.Error](f)(els))

def listType(typ: hydra.core.Type): Either[hydra.errors.Error, hydra.core.Type] =
  {
  lazy val stripped: hydra.core.Type = hydra.strip.deannotateType(typ)
  stripped match
    case hydra.core.Type.list(v_Type_list_t) => Right(v_Type_list_t)
    case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("list type",
       hydra.show.core.`type`(typ)))))
}

def literal(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Literal] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Literal](hydra.lexical.stripAndDereferenceTerm(graph)(term0))((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.literal(v_Term_literal_lit) => Right(v_Term_literal_lit)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("literal",
     hydra.show.core.term(term))))))

def map[T0, T1](fk: (hydra.core.Term => Either[hydra.errors.Error, T0]))(fv: (hydra.core.Term => Either[hydra.errors.Error,
   T1]))(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.errors.Error,
   Map[T0, T1]] =
  {
  def pair(kvPair: Tuple2[hydra.core.Term, hydra.core.Term]): Either[hydra.errors.Error, Tuple2[T0, T1]] =
    {
    lazy val kterm: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](kvPair)
    lazy val vterm: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](kvPair)
    hydra.lib.eithers.bind[hydra.errors.Error, T0, Tuple2[T0, T1]](fk(kterm))((kval: T0) =>
      hydra.lib.eithers.bind[hydra.errors.Error, T1, Tuple2[T0, T1]](fv(vterm))((vval: T1) => Right(Tuple2(kval,
         vval))))
  }
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, Map[T0, T1]](hydra.lexical.stripAndDereferenceTerm(graph)(term0))((term: hydra.core.Term) =>
    term match
    case hydra.core.Term.map(v_Term_map_m) => hydra.lib.eithers.map[Seq[Tuple2[T0,
       T1]], Map[T0, T1], hydra.errors.Error](hydra.lib.maps.fromList[T0, T1])(hydra.lib.eithers.mapList[Tuple2[hydra.core.Term,
       hydra.core.Term], Tuple2[T0, T1], hydra.errors.Error](pair)(hydra.lib.maps.toList[hydra.core.Term,
       hydra.core.Term](v_Term_map_m)))
    case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("map",
       hydra.show.core.term(term))))))
}

def mapType(typ: hydra.core.Type): Either[hydra.errors.Error, hydra.core.MapType] =
  {
  lazy val stripped: hydra.core.Type = hydra.strip.deannotateType(typ)
  stripped match
    case hydra.core.Type.map(v_Type_map_mt) => Right(v_Type_map_mt)
    case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("map type",
       hydra.show.core.`type`(typ)))))
}

def maybeTerm[T0](f: (hydra.core.Term => Either[hydra.errors.Error, T0]))(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.errors.Error,
   Option[T0]] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, Option[T0]](hydra.lexical.stripAndDereferenceTerm(graph)(term0))((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.maybe(v_Term_maybe_mt) => hydra.lib.maybes.maybe[Either[hydra.errors.Error,
     Option[T0]], hydra.core.Term](Right(None))((t: hydra.core.Term) =>
    hydra.lib.eithers.map[T0, Option[T0], hydra.errors.Error](hydra.lib.maybes.pure[T0])(f(t)))(v_Term_maybe_mt)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("maybe value",
     hydra.show.core.term(term))))))

def maybeType(typ: hydra.core.Type): Either[hydra.errors.Error, hydra.core.Type] =
  {
  lazy val stripped: hydra.core.Type = hydra.strip.deannotateType(typ)
  stripped match
    case hydra.core.Type.maybe(v_Type_maybe_t) => Right(v_Type_maybe_t)
    case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("maybe type",
       hydra.show.core.`type`(typ)))))
}

def nArgs[T0](name: hydra.core.Name)(n: Int)(args: Seq[T0]): Either[hydra.errors.Error, Unit] =
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, Unit]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[T0](args))(n))(Right(()))(Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError(hydra.lib.strings.cat(Seq(hydra.lib.literals.showInt32(n),
     " arguments to primitive ", hydra.lib.literals.showString(name))), hydra.lib.literals.showInt32(hydra.lib.lists.length[T0](args)))))))

def pair[T0, T1](kf: (hydra.core.Term => Either[hydra.errors.Error, T0]))(vf: (hydra.core.Term => Either[hydra.errors.Error,
   T1]))(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.errors.Error,
   Tuple2[T0, T1]] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, Tuple2[T0, T1]](hydra.lexical.stripAndDereferenceTerm(graph)(term0))((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.eithers.bind[hydra.errors.Error,
     T0, Tuple2[T0, T1]](kf(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p)))((kVal: T0) =>
    hydra.lib.eithers.bind[hydra.errors.Error, T1, Tuple2[T0, T1]](vf(hydra.lib.pairs.second[hydra.core.Term,
       hydra.core.Term](v_Term_pair_p)))((vVal: T1) => Right(Tuple2(kVal, vVal))))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("pair",
     hydra.show.core.term(term))))))

def record(expected: hydra.core.Name)(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.errors.Error,
   Seq[hydra.core.Field]] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Record, Seq[hydra.core.Field]](hydra.extract.core.termRecord(graph)(term0))((record: hydra.core.Record) =>
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, Seq[hydra.core.Field]]](hydra.lib.equality.equal[hydra.core.Name](record.typeName)(expected))(Right(record.fields))(Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError(hydra.lib.strings.cat2("record of type ")(expected),
     (record.typeName)))))))

def recordType[T0](ename: T0)(typ: hydra.core.Type): Either[hydra.errors.Error, Seq[hydra.core.FieldType]] =
  {
  lazy val stripped: hydra.core.Type = hydra.strip.deannotateType(typ)
  stripped match
    case hydra.core.Type.record(v_Type_record_fields) => Right(v_Type_record_fields)
    case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("record type",
       hydra.show.core.`type`(typ)))))
}

def requireField[T0, T1, T2](fieldName: scala.Predef.String)(decoder: (T0 => T1 => Either[hydra.errors.DecodingError,
   T2]))(fieldMap: Map[hydra.core.Name, T1])(g: T0): Either[hydra.errors.DecodingError,
   T2] =
  hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, T2], T1](Left(hydra.lib.strings.cat(Seq("missing field ",
     fieldName, " in record"))))((fieldTerm: T1) => decoder(g)(fieldTerm))(hydra.lib.maps.lookup[hydra.core.Name,
     T1](fieldName)(fieldMap))

def set(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.Error,
   scala.collection.immutable.Set[hydra.core.Term]] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, scala.collection.immutable.Set[hydra.core.Term]](hydra.lexical.stripAndDereferenceTerm(graph)(term))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.set(v_Term_set_s) => Right(v_Term_set_s)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("set",
     hydra.show.core.term(stripped))))))

def setOf[T0](f: (hydra.core.Term => Either[hydra.errors.Error, T0]))(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.Error,
   scala.collection.immutable.Set[T0]] =
  hydra.lib.eithers.bind[hydra.errors.Error, scala.collection.immutable.Set[hydra.core.Term],
     scala.collection.immutable.Set[T0]](hydra.extract.core.set(graph)(term))((els: scala.collection.immutable.Set[hydra.core.Term]) =>
  hydra.lib.eithers.mapSet[hydra.core.Term, T0, hydra.errors.Error](f)(els))

def setType(typ: hydra.core.Type): Either[hydra.errors.Error, hydra.core.Type] =
  {
  lazy val stripped: hydra.core.Type = hydra.strip.deannotateType(typ)
  stripped match
    case hydra.core.Type.set(v_Type_set_t) => Right(v_Type_set_t)
    case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("set type",
       hydra.show.core.`type`(typ)))))
}

def string(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.errors.Error, scala.Predef.String] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Literal, scala.Predef.String](hydra.extract.core.literal(graph)(t))((l: hydra.core.Literal) => hydra.extract.core.stringLiteral(l))

def stringLiteral(v: hydra.core.Literal): Either[hydra.errors.Error, scala.Predef.String] =
  v match
  case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("string",
     hydra.show.core.literal(v)))))

def stripWithDecodingError(g: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.core.Term] =
  hydra.lib.eithers.bimap[hydra.errors.Error, hydra.core.Term, hydra.errors.DecodingError,
     hydra.core.Term]((_e: hydra.errors.Error) => hydra.show.errors.error(_e))((x: hydra.core.Term) => x)(hydra.lexical.stripAndDereferenceTermEither(g)(term))

def termRecord(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Record] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Record](hydra.lexical.stripAndDereferenceTerm(graph)(term0))((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.record(v_Term_record_record) => Right(v_Term_record_record)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("record",
     hydra.show.core.term(term))))))

def toFieldMap(record: hydra.core.Record): Map[hydra.core.Name, hydra.core.Term] =
  hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Term](hydra.lib.lists.map[hydra.core.Field,
     Tuple2[hydra.core.Name, hydra.core.Term]]((f: hydra.core.Field) => Tuple2(f.name,
     (f.term)))(record.fields))

def uint16(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.errors.Error, Int] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Literal, Int](hydra.extract.core.literal(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.IntegerValue, Int](hydra.extract.core.integerLiteral(l))((i: hydra.core.IntegerValue) => hydra.extract.core.uint16Value(i)))

def uint16Value(v: hydra.core.IntegerValue): Either[hydra.errors.Error, Int] =
  v match
  case hydra.core.IntegerValue.uint16(v_IntegerValue_uint16_i) => Right(v_IntegerValue_uint16_i)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("uint16",
     hydra.show.core.integer(v)))))

def uint32(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.errors.Error, Long] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Literal, Long](hydra.extract.core.literal(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.IntegerValue, Long](hydra.extract.core.integerLiteral(l))((i: hydra.core.IntegerValue) => hydra.extract.core.uint32Value(i)))

def uint32Value(v: hydra.core.IntegerValue): Either[hydra.errors.Error, Long] =
  v match
  case hydra.core.IntegerValue.uint32(v_IntegerValue_uint32_i) => Right(v_IntegerValue_uint32_i)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("uint32",
     hydra.show.core.integer(v)))))

def uint64(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.errors.Error, BigInt] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Literal, BigInt](hydra.extract.core.literal(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.IntegerValue, BigInt](hydra.extract.core.integerLiteral(l))((i: hydra.core.IntegerValue) => hydra.extract.core.uint64Value(i)))

def uint64Value(v: hydra.core.IntegerValue): Either[hydra.errors.Error, BigInt] =
  v match
  case hydra.core.IntegerValue.uint64(v_IntegerValue_uint64_i) => Right(v_IntegerValue_uint64_i)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("uint64",
     hydra.show.core.integer(v)))))

def uint8(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.errors.Error, Byte] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Literal, Byte](hydra.extract.core.literal(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.IntegerValue, Byte](hydra.extract.core.integerLiteral(l))((i: hydra.core.IntegerValue) => hydra.extract.core.uint8Value(i)))

def uint8Value(v: hydra.core.IntegerValue): Either[hydra.errors.Error, Byte] =
  v match
  case hydra.core.IntegerValue.uint8(v_IntegerValue_uint8_i) => Right(v_IntegerValue_uint8_i)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("uint8",
     hydra.show.core.integer(v)))))

def unionType[T0](ename: T0)(typ: hydra.core.Type): Either[hydra.errors.Error, Seq[hydra.core.FieldType]] =
  {
  lazy val stripped: hydra.core.Type = hydra.strip.deannotateType(typ)
  stripped match
    case hydra.core.Type.union(v_Type_union_fields) => Right(v_Type_union_fields)
    case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("union type",
       hydra.show.core.`type`(typ)))))
}

def unit(term: hydra.core.Term): Either[hydra.errors.Error, Unit] =
  term match
  case hydra.core.Term.unit => Right(())
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("unit",
     hydra.show.core.term(term)))))

def unitVariant(tname: hydra.core.Name)(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Name] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Field, hydra.core.Name](hydra.extract.core.injection(tname)(graph)(term))((field: hydra.core.Field) =>
  hydra.lib.eithers.bind[hydra.errors.Error, Unit, hydra.core.Name](hydra.extract.core.unit(field.term))((ignored: Unit) => Right(field.name)))

def wrap(expected: hydra.core.Name)(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Term](hydra.lexical.stripAndDereferenceTerm(graph)(term0))((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.logic.ifElse[Either[hydra.errors.Error,
     hydra.core.Term]](hydra.lib.equality.equal[scala.Predef.String](v_Term_wrap_wrappedTerm.typeName)(expected))(Right(v_Term_wrap_wrappedTerm.body))(Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError(hydra.lib.strings.cat2("wrapper of type ")(expected),
     (v_Term_wrap_wrappedTerm.typeName))))))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError(hydra.lib.strings.cat2(hydra.lib.strings.cat2("wrap(")(expected))(")"),
     hydra.show.core.term(term))))))

def wrappedType[T0](ename: T0)(typ: hydra.core.Type): Either[hydra.errors.Error, hydra.core.Type] =
  {
  lazy val stripped: hydra.core.Type = hydra.strip.deannotateType(typ)
  stripped match
    case hydra.core.Type.wrap(v_Type_wrap_innerType) => Right(v_Type_wrap_innerType)
    case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("wrapped type",
       hydra.show.core.`type`(typ)))))
}
