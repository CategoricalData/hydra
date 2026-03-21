package hydra.decode.core

import hydra.core.*

import hydra.error.*

import hydra.lib.eithers

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.strings

def annotatedTerm(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.AnnotatedTerm] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.AnnotatedTerm]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Term, hydra.core.AnnotatedTerm](hydra.extract.helpers.requireField("body")(hydra.decode.core.term)(fieldMap)(cx))((field_body: hydra.core.Term) =>
      eithers.bind[hydra.error.DecodingError, Map[hydra.core.Name, hydra.core.Term], hydra.core.AnnotatedTerm](hydra.extract.helpers.requireField("annotation")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMap(hydra.decode.core.name)(hydra.decode.core.term)(v1)(v2))(fieldMap)(cx))((field_annotation: Map[hydra.core.Name,
         hydra.core.Term]) =>
      Right(hydra.core.AnnotatedTerm(field_body, field_annotation))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def annotatedType(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.AnnotatedType] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.AnnotatedType]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Type, hydra.core.AnnotatedType](hydra.extract.helpers.requireField("body")(hydra.decode.core.`type`)(fieldMap)(cx))((field_body: hydra.core.Type) =>
      eithers.bind[hydra.error.DecodingError, Map[hydra.core.Name, hydra.core.Term], hydra.core.AnnotatedType](hydra.extract.helpers.requireField("annotation")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMap(hydra.decode.core.name)(hydra.decode.core.term)(v1)(v2))(fieldMap)(cx))((field_annotation: Map[hydra.core.Name,
         hydra.core.Term]) =>
      Right(hydra.core.AnnotatedType(field_body, field_annotation))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def application(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.Application] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.Application]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Term, hydra.core.Application](hydra.extract.helpers.requireField("function")(hydra.decode.core.term)(fieldMap)(cx))((field_function: hydra.core.Term) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Term, hydra.core.Application](hydra.extract.helpers.requireField("argument")(hydra.decode.core.term)(fieldMap)(cx))((field_argument: hydra.core.Term) =>
      Right(hydra.core.Application(field_function, field_argument))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def applicationType(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.ApplicationType] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.ApplicationType]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Type, hydra.core.ApplicationType](hydra.extract.helpers.requireField("function")(hydra.decode.core.`type`)(fieldMap)(cx))((field_function: hydra.core.Type) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Type, hydra.core.ApplicationType](hydra.extract.helpers.requireField("argument")(hydra.decode.core.`type`)(fieldMap)(cx))((field_argument: hydra.core.Type) =>
      Right(hydra.core.ApplicationType(field_function, field_argument))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def binding(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.Binding] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.Binding]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Name, hydra.core.Binding](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Term, hydra.core.Binding](hydra.extract.helpers.requireField("term")(hydra.decode.core.term)(fieldMap)(cx))((field_term: hydra.core.Term) =>
      eithers.bind[hydra.error.DecodingError, Option[hydra.core.TypeScheme], hydra.core.Binding](hydra.extract.helpers.requireField("type")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMaybe(hydra.decode.core.typeScheme)(v1)(v2))(fieldMap)(cx))((field_type: Option[hydra.core.TypeScheme]) =>
      Right(hydra.core.Binding(field_name, field_term, field_type)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def caseStatement(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.CaseStatement] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.CaseStatement]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Name, hydra.core.CaseStatement](hydra.extract.helpers.requireField("typeName")(hydra.decode.core.name)(fieldMap)(cx))((field_typeName: hydra.core.Name) =>
      eithers.bind[hydra.error.DecodingError, Option[hydra.core.Term], hydra.core.CaseStatement](hydra.extract.helpers.requireField("default")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMaybe(hydra.decode.core.term)(v1)(v2))(fieldMap)(cx))((field_default: Option[hydra.core.Term]) =>
      eithers.bind[hydra.error.DecodingError, Seq[hydra.core.Field], hydra.core.CaseStatement](hydra.extract.helpers.requireField("cases")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.core.field)(v1)(v2))(fieldMap)(cx))((field_cases: Seq[hydra.core.Field]) =>
      Right(hydra.core.CaseStatement(field_typeName, field_default, field_cases)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def eitherType(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.EitherType] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.EitherType]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Type, hydra.core.EitherType](hydra.extract.helpers.requireField("left")(hydra.decode.core.`type`)(fieldMap)(cx))((field_left: hydra.core.Type) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Type, hydra.core.EitherType](hydra.extract.helpers.requireField("right")(hydra.decode.core.`type`)(fieldMap)(cx))((field_right: hydra.core.Type) => Right(hydra.core.EitherType(field_left,
         field_right))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def pairType(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.PairType] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.PairType]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Type, hydra.core.PairType](hydra.extract.helpers.requireField("first")(hydra.decode.core.`type`)(fieldMap)(cx))((field_first: hydra.core.Type) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Type, hydra.core.PairType](hydra.extract.helpers.requireField("second")(hydra.decode.core.`type`)(fieldMap)(cx))((field_second: hydra.core.Type) => Right(hydra.core.PairType(field_first,
         field_second))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def elimination(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.Elimination] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.Elimination]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.core.Elimination])] = maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.core.Elimination]](Seq(Tuple2("record",
       (input: hydra.core.Term) =>
      eithers.map[hydra.core.Projection, hydra.core.Elimination, hydra.error.DecodingError]((t: hydra.core.Projection) => hydra.core.Elimination.record(t))(hydra.decode.core.projection(cx)(input))),
         Tuple2("union", (input: hydra.core.Term) =>
      eithers.map[hydra.core.CaseStatement, hydra.core.Elimination, hydra.error.DecodingError]((t: hydra.core.CaseStatement) => hydra.core.Elimination.union(t))(hydra.decode.core.caseStatement(cx)(input))),
         Tuple2("wrap", (input: hydra.core.Term) =>
      eithers.map[hydra.core.Name, hydra.core.Elimination, hydra.error.DecodingError]((t: hydra.core.Name) => hydra.core.Elimination.wrap(t))(hydra.decode.core.name(cx)(input)))))
    maybes.maybe[Either[hydra.error.DecodingError, hydra.core.Elimination], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.core.Elimination]](Left(strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.core.Elimination])) => f(fterm))(maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.core.Elimination]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def field(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.Field] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.Field]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Name, hydra.core.Field](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Term, hydra.core.Field](hydra.extract.helpers.requireField("term")(hydra.decode.core.term)(fieldMap)(cx))((field_term: hydra.core.Term) => Right(hydra.core.Field(field_name,
         field_term))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def fieldType(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.FieldType] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.FieldType]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Name, hydra.core.FieldType](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Type, hydra.core.FieldType](hydra.extract.helpers.requireField("type")(hydra.decode.core.`type`)(fieldMap)(cx))((field_type: hydra.core.Type) => Right(hydra.core.FieldType(field_name,
         field_type))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def floatType(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.FloatType] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.FloatType]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.core.FloatType])] = maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.core.FloatType]](Seq(Tuple2("bigfloat",
       (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.core.FloatType, hydra.error.DecodingError]((t: Unit) => hydra.core.FloatType.bigfloat)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("float32", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.core.FloatType, hydra.error.DecodingError]((t: Unit) => hydra.core.FloatType.float32)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("float64", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.core.FloatType, hydra.error.DecodingError]((t: Unit) => hydra.core.FloatType.float64)(hydra.extract.helpers.decodeUnit(cx)(input)))))
    maybes.maybe[Either[hydra.error.DecodingError, hydra.core.FloatType], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.core.FloatType]](Left(strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.core.FloatType])) => f(fterm))(maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.core.FloatType]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def floatValue(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.FloatValue] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.FloatValue]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.core.FloatValue])] = maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.core.FloatValue]](Seq(Tuple2("bigfloat",
       (input: hydra.core.Term) =>
      eithers.map[BigDecimal, hydra.core.FloatValue, hydra.error.DecodingError]((t: BigDecimal) => hydra.core.FloatValue.bigfloat(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, BigDecimal]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.float(v_Literal_float_v1) => v_Literal_float_v1 match
          case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat_f) => Right(v_FloatValue_bigfloat_f)
          case _ => Left("expected bigfloat value")
        case _ => Left("expected bigfloat literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("float32", (input: hydra.core.Term) =>
      eithers.map[Float, hydra.core.FloatValue, hydra.error.DecodingError]((t: Float) => hydra.core.FloatValue.float32(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, Float]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.float(v_Literal_float_v1) => v_Literal_float_v1 match
          case hydra.core.FloatValue.float32(v_FloatValue_float32_f) => Right(v_FloatValue_float32_f)
          case _ => Left("expected float32 value")
        case _ => Left("expected float32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("float64", (input: hydra.core.Term) =>
      eithers.map[Double, hydra.core.FloatValue, hydra.error.DecodingError]((t: Double) => hydra.core.FloatValue.float64(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, Double]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.float(v_Literal_float_v1) => v_Literal_float_v1 match
          case hydra.core.FloatValue.float64(v_FloatValue_float64_f) => Right(v_FloatValue_float64_f)
          case _ => Left("expected float64 value")
        case _ => Left("expected float64 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input))))))
    maybes.maybe[Either[hydra.error.DecodingError, hydra.core.FloatValue], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.core.FloatValue]](Left(strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.core.FloatValue])) => f(fterm))(maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.core.FloatValue]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def forallType(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.ForallType] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.ForallType]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Name, hydra.core.ForallType](hydra.extract.helpers.requireField("parameter")(hydra.decode.core.name)(fieldMap)(cx))((field_parameter: hydra.core.Name) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Type, hydra.core.ForallType](hydra.extract.helpers.requireField("body")(hydra.decode.core.`type`)(fieldMap)(cx))((field_body: hydra.core.Type) => Right(hydra.core.ForallType(field_parameter,
         field_body))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def function(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.Function] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.Function]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.core.Function])] = maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.core.Function]](Seq(Tuple2("elimination",
       (input: hydra.core.Term) =>
      eithers.map[hydra.core.Elimination, hydra.core.Function, hydra.error.DecodingError]((t: hydra.core.Elimination) => hydra.core.Function.elimination(t))(hydra.decode.core.elimination(cx)(input))),
         Tuple2("lambda", (input: hydra.core.Term) =>
      eithers.map[hydra.core.Lambda, hydra.core.Function, hydra.error.DecodingError]((t: hydra.core.Lambda) => hydra.core.Function.lambda(t))(hydra.decode.core.lambda(cx)(input))),
         Tuple2("primitive", (input: hydra.core.Term) =>
      eithers.map[hydra.core.Name, hydra.core.Function, hydra.error.DecodingError]((t: hydra.core.Name) => hydra.core.Function.primitive(t))(hydra.decode.core.name(cx)(input)))))
    maybes.maybe[Either[hydra.error.DecodingError, hydra.core.Function], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.core.Function]](Left(strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.core.Function])) => f(fterm))(maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.core.Function]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def functionType(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.FunctionType] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.FunctionType]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Type, hydra.core.FunctionType](hydra.extract.helpers.requireField("domain")(hydra.decode.core.`type`)(fieldMap)(cx))((field_domain: hydra.core.Type) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Type, hydra.core.FunctionType](hydra.extract.helpers.requireField("codomain")(hydra.decode.core.`type`)(fieldMap)(cx))((field_codomain: hydra.core.Type) => Right(hydra.core.FunctionType(field_domain,
         field_codomain))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def injection(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.Injection] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.Injection]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Name, hydra.core.Injection](hydra.extract.helpers.requireField("typeName")(hydra.decode.core.name)(fieldMap)(cx))((field_typeName: hydra.core.Name) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Field, hydra.core.Injection](hydra.extract.helpers.requireField("field")(hydra.decode.core.field)(fieldMap)(cx))((field_field: hydra.core.Field) => Right(hydra.core.Injection(field_typeName,
         field_field))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def integerType(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.IntegerType] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.IntegerType]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.core.IntegerType])] = maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.core.IntegerType]](Seq(Tuple2("bigint",
       (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.core.IntegerType, hydra.error.DecodingError]((t: Unit) => hydra.core.IntegerType.bigint)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("int8", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.core.IntegerType, hydra.error.DecodingError]((t: Unit) => hydra.core.IntegerType.int8)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("int16", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.core.IntegerType, hydra.error.DecodingError]((t: Unit) => hydra.core.IntegerType.int16)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("int32", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.core.IntegerType, hydra.error.DecodingError]((t: Unit) => hydra.core.IntegerType.int32)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("int64", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.core.IntegerType, hydra.error.DecodingError]((t: Unit) => hydra.core.IntegerType.int64)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("uint8", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.core.IntegerType, hydra.error.DecodingError]((t: Unit) => hydra.core.IntegerType.uint8)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("uint16", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.core.IntegerType, hydra.error.DecodingError]((t: Unit) => hydra.core.IntegerType.uint16)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("uint32", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.core.IntegerType, hydra.error.DecodingError]((t: Unit) => hydra.core.IntegerType.uint32)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("uint64", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.core.IntegerType, hydra.error.DecodingError]((t: Unit) => hydra.core.IntegerType.uint64)(hydra.extract.helpers.decodeUnit(cx)(input)))))
    maybes.maybe[Either[hydra.error.DecodingError, hydra.core.IntegerType], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.core.IntegerType]](Left(strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.core.IntegerType])) => f(fterm))(maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.core.IntegerType]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def integerValue(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.IntegerValue] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.IntegerValue]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.core.IntegerValue])] = maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.core.IntegerValue]](Seq(Tuple2("bigint",
       (input: hydra.core.Term) =>
      eithers.map[BigInt, hydra.core.IntegerValue, hydra.error.DecodingError]((t: BigInt) => hydra.core.IntegerValue.bigint(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, BigInt]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.bigint(v_IntegerValue_bigint_i) => Right(v_IntegerValue_bigint_i)
          case _ => Left("expected bigint value")
        case _ => Left("expected bigint literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("int8", (input: hydra.core.Term) =>
      eithers.map[Byte, hydra.core.IntegerValue, hydra.error.DecodingError]((t: Byte) => hydra.core.IntegerValue.int8(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, Byte]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int8(v_IntegerValue_int8_i) => Right(v_IntegerValue_int8_i)
          case _ => Left("expected int8 value")
        case _ => Left("expected int8 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("int16", (input: hydra.core.Term) =>
      eithers.map[Short, hydra.core.IntegerValue, hydra.error.DecodingError]((t: Short) => hydra.core.IntegerValue.int16(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, Short]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int16(v_IntegerValue_int16_i) => Right(v_IntegerValue_int16_i)
          case _ => Left("expected int16 value")
        case _ => Left("expected int16 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("int32", (input: hydra.core.Term) =>
      eithers.map[Int, hydra.core.IntegerValue, hydra.error.DecodingError]((t: Int) => hydra.core.IntegerValue.int32(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("int64", (input: hydra.core.Term) =>
      eithers.map[Long, hydra.core.IntegerValue, hydra.error.DecodingError]((t: Long) => hydra.core.IntegerValue.int64(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, Long]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int64(v_IntegerValue_int64_i) => Right(v_IntegerValue_int64_i)
          case _ => Left("expected int64 value")
        case _ => Left("expected int64 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("uint8", (input: hydra.core.Term) =>
      eithers.map[Byte, hydra.core.IntegerValue, hydra.error.DecodingError]((t: Byte) => hydra.core.IntegerValue.uint8(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, Byte]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.uint8(v_IntegerValue_uint8_i) => Right(v_IntegerValue_uint8_i)
          case _ => Left("expected uint8 value")
        case _ => Left("expected uint8 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("uint16", (input: hydra.core.Term) =>
      eithers.map[Int, hydra.core.IntegerValue, hydra.error.DecodingError]((t: Int) => hydra.core.IntegerValue.uint16(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.uint16(v_IntegerValue_uint16_i) => Right(v_IntegerValue_uint16_i)
          case _ => Left("expected uint16 value")
        case _ => Left("expected uint16 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("uint32", (input: hydra.core.Term) =>
      eithers.map[Long, hydra.core.IntegerValue, hydra.error.DecodingError]((t: Long) => hydra.core.IntegerValue.uint32(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, Long]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.uint32(v_IntegerValue_uint32_i) => Right(v_IntegerValue_uint32_i)
          case _ => Left("expected uint32 value")
        case _ => Left("expected uint32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("uint64", (input: hydra.core.Term) =>
      eithers.map[BigInt, hydra.core.IntegerValue, hydra.error.DecodingError]((t: BigInt) => hydra.core.IntegerValue.uint64(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, BigInt]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.uint64(v_IntegerValue_uint64_i) => Right(v_IntegerValue_uint64_i)
          case _ => Left("expected uint64 value")
        case _ => Left("expected uint64 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input))))))
    maybes.maybe[Either[hydra.error.DecodingError, hydra.core.IntegerValue], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.core.IntegerValue]](Left(strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.core.IntegerValue])) => f(fterm))(maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.core.IntegerValue]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def lambda(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.Lambda] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.Lambda]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Name, hydra.core.Lambda](hydra.extract.helpers.requireField("parameter")(hydra.decode.core.name)(fieldMap)(cx))((field_parameter: hydra.core.Name) =>
      eithers.bind[hydra.error.DecodingError, Option[hydra.core.Type], hydra.core.Lambda](hydra.extract.helpers.requireField("domain")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMaybe(hydra.decode.core.`type`)(v1)(v2))(fieldMap)(cx))((field_domain: Option[hydra.core.Type]) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Term, hydra.core.Lambda](hydra.extract.helpers.requireField("body")(hydra.decode.core.term)(fieldMap)(cx))((field_body: hydra.core.Term) =>
      Right(hydra.core.Lambda(field_parameter, field_domain, field_body)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def let(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.Let] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.Let]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, Seq[hydra.core.Binding], hydra.core.Let](hydra.extract.helpers.requireField("bindings")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.core.binding)(v1)(v2))(fieldMap)(cx))((field_bindings: Seq[hydra.core.Binding]) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Term, hydra.core.Let](hydra.extract.helpers.requireField("body")(hydra.decode.core.term)(fieldMap)(cx))((field_body: hydra.core.Term) => Right(hydra.core.Let(field_bindings,
         field_body))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def literal(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.Literal] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.Literal]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.core.Literal])] = maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.core.Literal]](Seq(Tuple2("binary",
       (input: hydra.core.Term) =>
      eithers.map[scala.Predef.String, hydra.core.Literal, hydra.error.DecodingError]((t: scala.Predef.String) => hydra.core.Literal.binary(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.binary(v_Literal_binary_b) => Right(v_Literal_binary_b)
        case _ => Left("expected binary literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("boolean", (input: hydra.core.Term) =>
      eithers.map[Boolean, hydra.core.Literal, hydra.error.DecodingError]((t: Boolean) => hydra.core.Literal.boolean(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, Boolean]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.boolean(v_Literal_boolean_b) => Right(v_Literal_boolean_b)
        case _ => Left("expected boolean literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("float", (input: hydra.core.Term) =>
      eithers.map[hydra.core.FloatValue, hydra.core.Literal, hydra.error.DecodingError]((t: hydra.core.FloatValue) => hydra.core.Literal.float(t))(hydra.decode.core.floatValue(cx)(input))),
         Tuple2("integer", (input: hydra.core.Term) =>
      eithers.map[hydra.core.IntegerValue, hydra.core.Literal, hydra.error.DecodingError]((t: hydra.core.IntegerValue) => hydra.core.Literal.integer(t))(hydra.decode.core.integerValue(cx)(input))),
         Tuple2("string", (input: hydra.core.Term) =>
      eithers.map[scala.Predef.String, hydra.core.Literal, hydra.error.DecodingError]((t: scala.Predef.String) => hydra.core.Literal.string(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input))))))
    maybes.maybe[Either[hydra.error.DecodingError, hydra.core.Literal], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.core.Literal]](Left(strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.core.Literal])) => f(fterm))(maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.core.Literal]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def literalType(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.LiteralType] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.LiteralType]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.core.LiteralType])] = maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.core.LiteralType]](Seq(Tuple2("binary",
       (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.core.LiteralType, hydra.error.DecodingError]((t: Unit) => hydra.core.LiteralType.binary)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("boolean", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.core.LiteralType, hydra.error.DecodingError]((t: Unit) => hydra.core.LiteralType.boolean)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("float", (input: hydra.core.Term) =>
      eithers.map[hydra.core.FloatType, hydra.core.LiteralType, hydra.error.DecodingError]((t: hydra.core.FloatType) => hydra.core.LiteralType.float(t))(hydra.decode.core.floatType(cx)(input))),
         Tuple2("integer", (input: hydra.core.Term) =>
      eithers.map[hydra.core.IntegerType, hydra.core.LiteralType, hydra.error.DecodingError]((t: hydra.core.IntegerType) => hydra.core.LiteralType.integer(t))(hydra.decode.core.integerType(cx)(input))),
         Tuple2("string", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.core.LiteralType, hydra.error.DecodingError]((t: Unit) => hydra.core.LiteralType.string)(hydra.extract.helpers.decodeUnit(cx)(input)))))
    maybes.maybe[Either[hydra.error.DecodingError, hydra.core.LiteralType], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.core.LiteralType]](Left(strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.core.LiteralType])) => f(fterm))(maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.core.LiteralType]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def mapType(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.MapType] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.MapType]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Type, hydra.core.MapType](hydra.extract.helpers.requireField("keys")(hydra.decode.core.`type`)(fieldMap)(cx))((field_keys: hydra.core.Type) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Type, hydra.core.MapType](hydra.extract.helpers.requireField("values")(hydra.decode.core.`type`)(fieldMap)(cx))((field_values: hydra.core.Type) => Right(hydra.core.MapType(field_keys,
         field_values))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def name(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.Name] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.Name]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => eithers.map[scala.Predef.String, hydra.core.Name,
     hydra.error.DecodingError]((b: scala.Predef.String) => b)(eithers.either[scala.Predef.String, hydra.core.Term,
     Either[hydra.error.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def projection(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.Projection] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.Projection]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Name, hydra.core.Projection](hydra.extract.helpers.requireField("typeName")(hydra.decode.core.name)(fieldMap)(cx))((field_typeName: hydra.core.Name) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Name, hydra.core.Projection](hydra.extract.helpers.requireField("field")(hydra.decode.core.name)(fieldMap)(cx))((field_field: hydra.core.Name) => Right(hydra.core.Projection(field_typeName,
         field_field))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def record(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.Record] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.Record]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Name, hydra.core.Record](hydra.extract.helpers.requireField("typeName")(hydra.decode.core.name)(fieldMap)(cx))((field_typeName: hydra.core.Name) =>
      eithers.bind[hydra.error.DecodingError, Seq[hydra.core.Field], hydra.core.Record](hydra.extract.helpers.requireField("fields")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.core.field)(v1)(v2))(fieldMap)(cx))((field_fields: Seq[hydra.core.Field]) => Right(hydra.core.Record(field_typeName,
         field_fields))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def term(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.Term] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.Term]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.core.Term])] = maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.core.Term]](Seq(Tuple2("annotated",
       (input: hydra.core.Term) =>
      eithers.map[hydra.core.AnnotatedTerm, hydra.core.Term, hydra.error.DecodingError]((t: hydra.core.AnnotatedTerm) => hydra.core.Term.annotated(t))(hydra.decode.core.annotatedTerm(cx)(input))),
         Tuple2("application", (input: hydra.core.Term) =>
      eithers.map[hydra.core.Application, hydra.core.Term, hydra.error.DecodingError]((t: hydra.core.Application) => hydra.core.Term.application(t))(hydra.decode.core.application(cx)(input))),
         Tuple2("either", (input: hydra.core.Term) =>
      eithers.map[Either[hydra.core.Term, hydra.core.Term], hydra.core.Term, hydra.error.DecodingError]((t: Either[hydra.core.Term,
         hydra.core.Term]) => hydra.core.Term.either(t))(hydra.extract.helpers.decodeEither(hydra.decode.core.term)(hydra.decode.core.term)(cx)(input))),
         Tuple2("function", (input: hydra.core.Term) =>
      eithers.map[hydra.core.Function, hydra.core.Term, hydra.error.DecodingError]((t: hydra.core.Function) => hydra.core.Term.function(t))(hydra.decode.core.function(cx)(input))),
         Tuple2("let", (input: hydra.core.Term) =>
      eithers.map[hydra.core.Let, hydra.core.Term, hydra.error.DecodingError]((t: hydra.core.Let) => hydra.core.Term.let(t))(hydra.decode.core.let(cx)(input))),
         Tuple2("list", (input: hydra.core.Term) =>
      eithers.map[Seq[hydra.core.Term], hydra.core.Term, hydra.error.DecodingError]((t: Seq[hydra.core.Term]) => hydra.core.Term.list(t))(hydra.extract.helpers.decodeList(hydra.decode.core.term)(cx)(input))),
         Tuple2("literal", (input: hydra.core.Term) =>
      eithers.map[hydra.core.Literal, hydra.core.Term, hydra.error.DecodingError]((t: hydra.core.Literal) => hydra.core.Term.literal(t))(hydra.decode.core.literal(cx)(input))),
         Tuple2("map", (input: hydra.core.Term) =>
      eithers.map[Map[hydra.core.Term, hydra.core.Term], hydra.core.Term, hydra.error.DecodingError]((t: Map[hydra.core.Term,
         hydra.core.Term]) => hydra.core.Term.map(t))(hydra.extract.helpers.decodeMap(hydra.decode.core.term)(hydra.decode.core.term)(cx)(input))),
         Tuple2("maybe", (input: hydra.core.Term) =>
      eithers.map[Option[hydra.core.Term], hydra.core.Term, hydra.error.DecodingError]((t: Option[hydra.core.Term]) => hydra.core.Term.maybe(t))(hydra.extract.helpers.decodeMaybe(hydra.decode.core.term)(cx)(input))),
         Tuple2("pair", (input: hydra.core.Term) =>
      eithers.map[Tuple2[hydra.core.Term, hydra.core.Term], hydra.core.Term, hydra.error.DecodingError]((t: Tuple2[hydra.core.Term,
         hydra.core.Term]) => hydra.core.Term.pair(t))(hydra.extract.helpers.decodePair(hydra.decode.core.term)(hydra.decode.core.term)(cx)(input))),
         Tuple2("record", (input: hydra.core.Term) =>
      eithers.map[hydra.core.Record, hydra.core.Term, hydra.error.DecodingError]((t: hydra.core.Record) => hydra.core.Term.record(t))(hydra.decode.core.record(cx)(input))),
         Tuple2("set", (input: hydra.core.Term) =>
      eithers.map[scala.collection.immutable.Set[hydra.core.Term], hydra.core.Term, hydra.error.DecodingError]((t: scala.collection.immutable.Set[hydra.core.Term]) => hydra.core.Term.set(t))(hydra.extract.helpers.decodeSet(hydra.decode.core.term)(cx)(input))),
         Tuple2("typeApplication", (input: hydra.core.Term) =>
      eithers.map[hydra.core.TypeApplicationTerm, hydra.core.Term, hydra.error.DecodingError]((t: hydra.core.TypeApplicationTerm) => hydra.core.Term.typeApplication(t))(hydra.decode.core.typeApplicationTerm(cx)(input))),
         Tuple2("typeLambda", (input: hydra.core.Term) =>
      eithers.map[hydra.core.TypeLambda, hydra.core.Term, hydra.error.DecodingError]((t: hydra.core.TypeLambda) => hydra.core.Term.typeLambda(t))(hydra.decode.core.typeLambda(cx)(input))),
         Tuple2("union", (input: hydra.core.Term) =>
      eithers.map[hydra.core.Injection, hydra.core.Term, hydra.error.DecodingError]((t: hydra.core.Injection) => hydra.core.Term.union(t))(hydra.decode.core.injection(cx)(input))),
         Tuple2("unit", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.core.Term, hydra.error.DecodingError]((t: Unit) => hydra.core.Term.unit)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("variable", (input: hydra.core.Term) =>
      eithers.map[hydra.core.Name, hydra.core.Term, hydra.error.DecodingError]((t: hydra.core.Name) => hydra.core.Term.variable(t))(hydra.decode.core.name(cx)(input))),
         Tuple2("wrap", (input: hydra.core.Term) =>
      eithers.map[hydra.core.WrappedTerm, hydra.core.Term, hydra.error.DecodingError]((t: hydra.core.WrappedTerm) => hydra.core.Term.wrap(t))(hydra.decode.core.wrappedTerm(cx)(input)))))
    maybes.maybe[Either[hydra.error.DecodingError, hydra.core.Term], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.core.Term]](Left(strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.core.Term])) => f(fterm))(maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.core.Term]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def `type`(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.Type] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.Type]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.core.Type])] = maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.core.Type]](Seq(Tuple2("annotated",
       (input: hydra.core.Term) =>
      eithers.map[hydra.core.AnnotatedType, hydra.core.Type, hydra.error.DecodingError]((t: hydra.core.AnnotatedType) => hydra.core.Type.annotated(t))(hydra.decode.core.annotatedType(cx)(input))),
         Tuple2("application", (input: hydra.core.Term) =>
      eithers.map[hydra.core.ApplicationType, hydra.core.Type, hydra.error.DecodingError]((t: hydra.core.ApplicationType) => hydra.core.Type.application(t))(hydra.decode.core.applicationType(cx)(input))),
         Tuple2("either", (input: hydra.core.Term) =>
      eithers.map[hydra.core.EitherType, hydra.core.Type, hydra.error.DecodingError]((t: hydra.core.EitherType) => hydra.core.Type.either(t))(hydra.decode.core.eitherType(cx)(input))),
         Tuple2("forall", (input: hydra.core.Term) =>
      eithers.map[hydra.core.ForallType, hydra.core.Type, hydra.error.DecodingError]((t: hydra.core.ForallType) => hydra.core.Type.forall(t))(hydra.decode.core.forallType(cx)(input))),
         Tuple2("function", (input: hydra.core.Term) =>
      eithers.map[hydra.core.FunctionType, hydra.core.Type, hydra.error.DecodingError]((t: hydra.core.FunctionType) => hydra.core.Type.function(t))(hydra.decode.core.functionType(cx)(input))),
         Tuple2("list", (input: hydra.core.Term) =>
      eithers.map[hydra.core.Type, hydra.core.Type, hydra.error.DecodingError]((t: hydra.core.Type) => hydra.core.Type.list(t))(hydra.decode.core.`type`(cx)(input))),
         Tuple2("literal", (input: hydra.core.Term) =>
      eithers.map[hydra.core.LiteralType, hydra.core.Type, hydra.error.DecodingError]((t: hydra.core.LiteralType) => hydra.core.Type.literal(t))(hydra.decode.core.literalType(cx)(input))),
         Tuple2("map", (input: hydra.core.Term) =>
      eithers.map[hydra.core.MapType, hydra.core.Type, hydra.error.DecodingError]((t: hydra.core.MapType) => hydra.core.Type.map(t))(hydra.decode.core.mapType(cx)(input))),
         Tuple2("maybe", (input: hydra.core.Term) =>
      eithers.map[hydra.core.Type, hydra.core.Type, hydra.error.DecodingError]((t: hydra.core.Type) => hydra.core.Type.maybe(t))(hydra.decode.core.`type`(cx)(input))),
         Tuple2("pair", (input: hydra.core.Term) =>
      eithers.map[hydra.core.PairType, hydra.core.Type, hydra.error.DecodingError]((t: hydra.core.PairType) => hydra.core.Type.pair(t))(hydra.decode.core.pairType(cx)(input))),
         Tuple2("record", (input: hydra.core.Term) =>
      eithers.map[Seq[hydra.core.FieldType], hydra.core.Type, hydra.error.DecodingError]((t: Seq[hydra.core.FieldType]) => hydra.core.Type.record(t))(hydra.extract.helpers.decodeList(hydra.decode.core.fieldType)(cx)(input))),
         Tuple2("set", (input: hydra.core.Term) =>
      eithers.map[hydra.core.Type, hydra.core.Type, hydra.error.DecodingError]((t: hydra.core.Type) => hydra.core.Type.set(t))(hydra.decode.core.`type`(cx)(input))),
         Tuple2("union", (input: hydra.core.Term) =>
      eithers.map[Seq[hydra.core.FieldType], hydra.core.Type, hydra.error.DecodingError]((t: Seq[hydra.core.FieldType]) => hydra.core.Type.union(t))(hydra.extract.helpers.decodeList(hydra.decode.core.fieldType)(cx)(input))),
         Tuple2("unit", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.core.Type, hydra.error.DecodingError]((t: Unit) => hydra.core.Type.unit)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("variable", (input: hydra.core.Term) =>
      eithers.map[hydra.core.Name, hydra.core.Type, hydra.error.DecodingError]((t: hydra.core.Name) => hydra.core.Type.variable(t))(hydra.decode.core.name(cx)(input))),
         Tuple2("wrap", (input: hydra.core.Term) =>
      eithers.map[hydra.core.Type, hydra.core.Type, hydra.error.DecodingError]((t: hydra.core.Type) => hydra.core.Type.wrap(t))(hydra.decode.core.`type`(cx)(input)))))
    maybes.maybe[Either[hydra.error.DecodingError, hydra.core.Type], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.core.Type]](Left(strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.core.Type])) => f(fterm))(maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.core.Type]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def typeApplicationTerm(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.TypeApplicationTerm] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.TypeApplicationTerm]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Term, hydra.core.TypeApplicationTerm](hydra.extract.helpers.requireField("body")(hydra.decode.core.term)(fieldMap)(cx))((field_body: hydra.core.Term) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Type, hydra.core.TypeApplicationTerm](hydra.extract.helpers.requireField("type")(hydra.decode.core.`type`)(fieldMap)(cx))((field_type: hydra.core.Type) =>
      Right(hydra.core.TypeApplicationTerm(field_body, field_type))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def typeLambda(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.TypeLambda] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.TypeLambda]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Name, hydra.core.TypeLambda](hydra.extract.helpers.requireField("parameter")(hydra.decode.core.name)(fieldMap)(cx))((field_parameter: hydra.core.Name) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Term, hydra.core.TypeLambda](hydra.extract.helpers.requireField("body")(hydra.decode.core.term)(fieldMap)(cx))((field_body: hydra.core.Term) => Right(hydra.core.TypeLambda(field_parameter,
         field_body))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def typeScheme(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.TypeScheme] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.TypeScheme]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, Seq[hydra.core.Name], hydra.core.TypeScheme](hydra.extract.helpers.requireField("variables")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.core.name)(v1)(v2))(fieldMap)(cx))((field_variables: Seq[hydra.core.Name]) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Type, hydra.core.TypeScheme](hydra.extract.helpers.requireField("type")(hydra.decode.core.`type`)(fieldMap)(cx))((field_type: hydra.core.Type) =>
      eithers.bind[hydra.error.DecodingError, Option[Map[hydra.core.Name, hydra.core.TypeVariableMetadata]],
         hydra.core.TypeScheme](hydra.extract.helpers.requireField("constraints")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMaybe((v12: hydra.graph.Graph) =>
      (v22: hydra.core.Term) =>
      hydra.extract.helpers.decodeMap(hydra.decode.core.name)(hydra.decode.core.typeVariableMetadata)(v12)(v22))(v1)(v2))(fieldMap)(cx))((field_constraints: Option[Map[hydra.core.Name,
         hydra.core.TypeVariableMetadata]]) =>
      Right(hydra.core.TypeScheme(field_variables, field_type, field_constraints)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def typeVariableMetadata(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.TypeVariableMetadata] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.TypeVariableMetadata]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, scala.collection.immutable.Set[hydra.core.Name], hydra.core.TypeVariableMetadata](hydra.extract.helpers.requireField("classes")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeSet(hydra.decode.core.name)(v1)(v2))(fieldMap)(cx))((field_classes: scala.collection.immutable.Set[hydra.core.Name]) => Right(hydra.core.TypeVariableMetadata(field_classes)))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def wrappedTerm(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.core.WrappedTerm] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.core.WrappedTerm]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Name, hydra.core.WrappedTerm](hydra.extract.helpers.requireField("typeName")(hydra.decode.core.name)(fieldMap)(cx))((field_typeName: hydra.core.Name) =>
      eithers.bind[hydra.error.DecodingError, hydra.core.Term, hydra.core.WrappedTerm](hydra.extract.helpers.requireField("body")(hydra.decode.core.term)(fieldMap)(cx))((field_body: hydra.core.Term) => Right(hydra.core.WrappedTerm(field_typeName,
         field_body))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
