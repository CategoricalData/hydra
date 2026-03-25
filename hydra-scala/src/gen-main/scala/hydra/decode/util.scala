package hydra.decode.util

import hydra.core.*

import hydra.errors.*

import hydra.util.*

import hydra.lib.eithers

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.strings

def caseConvention(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.util.CaseConvention] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.util.CaseConvention]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.util.CaseConvention])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.util.CaseConvention]](Seq(Tuple2("camel",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.util.CaseConvention, hydra.errors.DecodingError]((t: Unit) => hydra.util.CaseConvention.camel(t))(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("pascal", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.util.CaseConvention, hydra.errors.DecodingError]((t: Unit) => hydra.util.CaseConvention.pascal(t))(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("lowerSnake", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.util.CaseConvention, hydra.errors.DecodingError]((t: Unit) => hydra.util.CaseConvention.lowerSnake(t))(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("upperSnake", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.util.CaseConvention, hydra.errors.DecodingError]((t: Unit) => hydra.util.CaseConvention.upperSnake(t))(hydra.extract.helpers.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.util.CaseConvention], (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.util.CaseConvention]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.util.CaseConvention])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.util.CaseConvention]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def comparison(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.util.Comparison] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.util.Comparison]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.util.Comparison])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.util.Comparison]](Seq(Tuple2("lessThan",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.util.Comparison, hydra.errors.DecodingError]((t: Unit) => hydra.util.Comparison.lessThan(t))(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("equalTo", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.util.Comparison, hydra.errors.DecodingError]((t: Unit) => hydra.util.Comparison.equalTo(t))(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("greaterThan", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.util.Comparison, hydra.errors.DecodingError]((t: Unit) => hydra.util.Comparison.greaterThan(t))(hydra.extract.helpers.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.util.Comparison], (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.util.Comparison]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.util.Comparison])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.util.Comparison]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def precision(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.util.Precision] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.util.Precision]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.util.Precision])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.util.Precision]](Seq(Tuple2("arbitrary",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.util.Precision, hydra.errors.DecodingError]((t: Unit) => hydra.util.Precision.arbitrary(t))(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("bits", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Int, hydra.util.Precision, hydra.errors.DecodingError]((t: Int) => hydra.util.Precision.bits(t))(hydra.lib.eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.errors.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input))))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.util.Precision], (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.util.Precision]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.util.Precision])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.util.Precision]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
