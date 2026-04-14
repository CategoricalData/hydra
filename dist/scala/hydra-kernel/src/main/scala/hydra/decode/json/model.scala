package hydra.decode.json.model

import hydra.core.*

import hydra.errors.*

import hydra.json.model.*

def value(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.json.model.Value] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.json.model.Value]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.inject(v_Term_inject_inj) => {
    lazy val field: hydra.core.Field = (v_Term_inject_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.json.model.Value])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.json.model.Value]](Seq(Tuple2("array",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Seq[hydra.json.model.Value], hydra.json.model.Value, hydra.errors.DecodingError]((t: Seq[hydra.json.model.Value]) => hydra.json.model.Value.array(t))(hydra.extract.core.decodeList(hydra.decode.json.model.value)(cx)(input))),
         Tuple2("boolean", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Boolean, hydra.json.model.Value, hydra.errors.DecodingError]((t: Boolean) => hydra.json.model.Value.boolean(t))(hydra.lib.eithers.either[hydra.errors.DecodingError,
         hydra.core.Term, Either[hydra.errors.DecodingError, Boolean]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.boolean(v_Literal_boolean_b) => Right(v_Literal_boolean_b)
        case _ => Left("expected boolean literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx)(input)))), Tuple2("null", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.json.model.Value, hydra.errors.DecodingError]((t: Unit) => hydra.json.model.Value.`null`)(hydra.extract.core.decodeUnit(cx)(input))),
         Tuple2("number", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[BigDecimal, hydra.json.model.Value, hydra.errors.DecodingError]((t: BigDecimal) => hydra.json.model.Value.number(t))(hydra.lib.eithers.either[hydra.errors.DecodingError,
         hydra.core.Term, Either[hydra.errors.DecodingError, BigDecimal]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.float(v_Literal_float_v1) => v_Literal_float_v1 match
          case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat_f) => Right(v_FloatValue_bigfloat_f)
          case _ => Left("expected bigfloat value")
        case _ => Left("expected bigfloat literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx)(input)))), Tuple2("object", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Map[scala.Predef.String, hydra.json.model.Value], hydra.json.model.Value,
         hydra.errors.DecodingError]((t: Map[scala.Predef.String, hydra.json.model.Value]) => hydra.json.model.Value.`object`(t))(hydra.extract.core.decodeMap((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(hydra.decode.json.model.value)(cx)(input))),
         Tuple2("string", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[scala.Predef.String, hydra.json.model.Value, hydra.errors.DecodingError]((t: scala.Predef.String) => hydra.json.model.Value.string(t))(hydra.lib.eithers.either[hydra.errors.DecodingError,
         hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx)(input))))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.json.model.Value], (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.json.model.Value]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.json.model.Value])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.json.model.Value]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.extract.core.stripWithDecodingError(cx)(raw))
