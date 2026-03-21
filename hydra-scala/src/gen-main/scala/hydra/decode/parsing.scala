package hydra.decode.parsing

import hydra.core.*

import hydra.error.*

import hydra.parsing.*

import hydra.lib.eithers

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.strings

def parseError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.parsing.ParseError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.parsing.ParseError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.error.DecodingError, scala.Predef.String, hydra.parsing.ParseError](hydra.extract.helpers.requireField("message")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError,
         scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_message: scala.Predef.String) =>
      hydra.lib.eithers.bind[hydra.error.DecodingError, scala.Predef.String, hydra.parsing.ParseError](hydra.extract.helpers.requireField("remainder")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError,
         scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_remainder: scala.Predef.String) =>
      Right(hydra.parsing.ParseError(field_message, field_remainder))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def parseResult[T0](a: (hydra.graph.Graph => hydra.core.Term => Either[hydra.error.DecodingError, T0]))(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError,
   hydra.parsing.ParseResult[T0]] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.parsing.ParseResult[T0]]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.parsing.ParseResult[T0]])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.parsing.ParseResult[T0]]](Seq(Tuple2("success",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.parsing.ParseSuccess[T0], hydra.parsing.ParseResult[T0], hydra.error.DecodingError]((t: hydra.parsing.ParseSuccess[T0]) => hydra.parsing.ParseResult.success(t))(hydra.decode.parsing.parseSuccess(a)(cx)(input))),
         Tuple2("failure", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.parsing.ParseError, hydra.parsing.ParseResult[T0], hydra.error.DecodingError]((t: hydra.parsing.ParseError) => hydra.parsing.ParseResult.failure(t))(hydra.decode.parsing.parseError(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.error.DecodingError, hydra.parsing.ParseResult[T0]], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.parsing.ParseResult[T0]]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.parsing.ParseResult[T0]])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.parsing.ParseResult[T0]]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def parseSuccess[T0](a: (hydra.graph.Graph => hydra.core.Term => Either[hydra.error.DecodingError, T0]))(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError,
   hydra.parsing.ParseSuccess[T0]] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.parsing.ParseSuccess[T0]]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.error.DecodingError, T0, hydra.parsing.ParseSuccess[T0]](hydra.extract.helpers.requireField("value")(a)(fieldMap)(cx))((field_value: T0) =>
      hydra.lib.eithers.bind[hydra.error.DecodingError, scala.Predef.String, hydra.parsing.ParseSuccess[T0]](hydra.extract.helpers.requireField("remainder")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError,
         scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_remainder: scala.Predef.String) =>
      Right(hydra.parsing.ParseSuccess(field_value, field_remainder))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
