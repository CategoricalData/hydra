package hydra.decode.coders

import hydra.coders.*

import hydra.core.*

import hydra.errors.*

import hydra.lib.eithers

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.strings

def coderDirection(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.coders.CoderDirection] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.coders.CoderDirection]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.coders.CoderDirection])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.coders.CoderDirection]](Seq(Tuple2("encode",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.coders.CoderDirection, hydra.errors.DecodingError]((t: Unit) => hydra.coders.CoderDirection.encode(t))(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("decode", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.coders.CoderDirection, hydra.errors.DecodingError]((t: Unit) => hydra.coders.CoderDirection.decode(t))(hydra.extract.helpers.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.coders.CoderDirection], (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.coders.CoderDirection]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.coders.CoderDirection])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.coders.CoderDirection]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def languageName(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.coders.LanguageName] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.coders.LanguageName]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[scala.Predef.String, hydra.coders.LanguageName,
     hydra.errors.DecodingError]((b: scala.Predef.String) => b)(hydra.lib.eithers.either[scala.Predef.String,
     hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def traversalOrder(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.coders.TraversalOrder] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.coders.TraversalOrder]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.coders.TraversalOrder])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.coders.TraversalOrder]](Seq(Tuple2("pre",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.coders.TraversalOrder, hydra.errors.DecodingError]((t: Unit) => hydra.coders.TraversalOrder.pre(t))(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("post", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.coders.TraversalOrder, hydra.errors.DecodingError]((t: Unit) => hydra.coders.TraversalOrder.post(t))(hydra.extract.helpers.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.coders.TraversalOrder], (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.coders.TraversalOrder]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.coders.TraversalOrder])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.coders.TraversalOrder]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
