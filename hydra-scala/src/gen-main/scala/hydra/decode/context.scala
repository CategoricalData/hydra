package hydra.decode.context

import hydra.context.*

import hydra.core.*

import hydra.error.*

import hydra.lib.eithers

def context(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.context.Context] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.context.Context]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.error.DecodingError, Seq[scala.Predef.String], hydra.context.Context](hydra.extract.helpers.requireField("trace")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError,
         scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(v1)(v2))(fieldMap)(cx))((field_trace: Seq[scala.Predef.String]) =>
      hydra.lib.eithers.bind[hydra.error.DecodingError, Seq[scala.Predef.String], hydra.context.Context](hydra.extract.helpers.requireField("messages")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError,
         scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(v1)(v2))(fieldMap)(cx))((field_messages: Seq[scala.Predef.String]) =>
      hydra.lib.eithers.bind[hydra.error.DecodingError, Map[hydra.core.Name, hydra.core.Term], hydra.context.Context](hydra.extract.helpers.requireField("other")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMap(hydra.decode.core.name)(hydra.decode.core.term)(v1)(v2))(fieldMap)(cx))((field_other: Map[hydra.core.Name,
         hydra.core.Term]) =>
      Right(hydra.context.Context(field_trace, field_messages, field_other)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def inContext[T0](e: (hydra.graph.Graph => hydra.core.Term => Either[hydra.error.DecodingError, T0]))(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError,
   hydra.context.InContext[T0]] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.context.InContext[T0]]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.error.DecodingError, T0, hydra.context.InContext[T0]](hydra.extract.helpers.requireField("object")(e)(fieldMap)(cx))((field_object: T0) =>
      hydra.lib.eithers.bind[hydra.error.DecodingError, hydra.context.Context, hydra.context.InContext[T0]](hydra.extract.helpers.requireField("context")(hydra.decode.context.context)(fieldMap)(cx))((field_context: hydra.context.Context) => Right(hydra.context.InContext(field_object,
         field_context))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
