package hydra.decode.parsing

import hydra.core.*

import hydra.errors.*

import hydra.parsing.*

def parseError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.parsing.ParseError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.parsing.ParseError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.parsing.ParseError](hydra.extract.core.requireField("message")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_message: scala.Predef.String) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.parsing.ParseError](hydra.extract.core.requireField("remainder")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_remainder: scala.Predef.String) =>
      Right(hydra.parsing.ParseError(field_message, field_remainder))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def parseResult[T0](a: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T0]))(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.parsing.ParseResult[T0]] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.parsing.ParseResult[T0]]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.inject(v_Term_inject_inj) => {
    lazy val field: hydra.core.Field = (v_Term_inject_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.parsing.ParseResult[T0]])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.parsing.ParseResult[T0]]](Seq(Tuple2("success",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.parsing.ParseSuccess[T0], hydra.parsing.ParseResult[T0], hydra.errors.DecodingError]((t: hydra.parsing.ParseSuccess[T0]) => hydra.parsing.ParseResult.success(t))(hydra.decode.parsing.parseSuccess(a)(cx)(input))),
         Tuple2("failure", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.parsing.ParseError, hydra.parsing.ParseResult[T0], hydra.errors.DecodingError]((t: hydra.parsing.ParseError) => hydra.parsing.ParseResult.failure(t))(hydra.decode.parsing.parseError(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.parsing.ParseResult[T0]], (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.parsing.ParseResult[T0]]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.parsing.ParseResult[T0]])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.parsing.ParseResult[T0]]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def parseSuccess[T0](a: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T0]))(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.parsing.ParseSuccess[T0]] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.parsing.ParseSuccess[T0]]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, T0, hydra.parsing.ParseSuccess[T0]](hydra.extract.core.requireField("value")(a)(fieldMap)(cx))((field_value: T0) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.parsing.ParseSuccess[T0]](hydra.extract.core.requireField("remainder")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_remainder: scala.Predef.String) =>
      Right(hydra.parsing.ParseSuccess(field_value, field_remainder))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))
