package hydra.decode.errors

import hydra.core.*

import hydra.errors.*

import hydra.lib.eithers

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.strings

def decodingError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.errors.DecodingError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.errors.DecodingError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[scala.Predef.String, hydra.errors.DecodingError,
     hydra.errors.DecodingError]((b: scala.Predef.String) => b)(hydra.lib.eithers.either[scala.Predef.String,
     hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def error(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.errors.Error] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.errors.Error]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.errors.Error])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.errors.Error]](Seq(Tuple2("checking",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.checking.CheckingError, hydra.errors.Error, hydra.errors.DecodingError]((t: hydra.error.checking.CheckingError) => hydra.errors.Error.checking(t))(hydra.decode.error.checking.checkingError(cx)(input))),
         Tuple2("decoding", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.errors.DecodingError, hydra.errors.Error, hydra.errors.DecodingError]((t: hydra.errors.DecodingError) => hydra.errors.Error.decoding(t))(hydra.decode.errors.decodingError(cx)(input))),
         Tuple2("duplicateBinding", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.DuplicateBindingError, hydra.errors.Error, hydra.errors.DecodingError]((t: hydra.error.core.DuplicateBindingError) => hydra.errors.Error.duplicateBinding(t))(hydra.decode.error.core.duplicateBindingError(cx)(input))),
         Tuple2("duplicateField", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.DuplicateFieldError, hydra.errors.Error, hydra.errors.DecodingError]((t: hydra.error.core.DuplicateFieldError) => hydra.errors.Error.duplicateField(t))(hydra.decode.error.core.duplicateFieldError(cx)(input))),
         Tuple2("other", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.errors.OtherError, hydra.errors.Error, hydra.errors.DecodingError]((t: hydra.errors.OtherError) => hydra.errors.Error.other(t))(hydra.decode.errors.otherError(cx)(input))),
         Tuple2("undefinedField", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.UndefinedFieldError, hydra.errors.Error, hydra.errors.DecodingError]((t: hydra.error.core.UndefinedFieldError) => hydra.errors.Error.undefinedField(t))(hydra.decode.error.core.undefinedFieldError(cx)(input))),
         Tuple2("undefinedTermVariable", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.UndefinedTermVariableError, hydra.errors.Error, hydra.errors.DecodingError]((t: hydra.error.core.UndefinedTermVariableError) => hydra.errors.Error.undefinedTermVariable(t))(hydra.decode.error.core.undefinedTermVariableError(cx)(input))),
         Tuple2("untypedTermVariable", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.UntypedTermVariableError, hydra.errors.Error, hydra.errors.DecodingError]((t: hydra.error.core.UntypedTermVariableError) => hydra.errors.Error.untypedTermVariable(t))(hydra.decode.error.core.untypedTermVariableError(cx)(input))),
         Tuple2("unexpectedTermVariant", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.UnexpectedTermVariantError, hydra.errors.Error, hydra.errors.DecodingError]((t: hydra.error.core.UnexpectedTermVariantError) => hydra.errors.Error.unexpectedTermVariant(t))(hydra.decode.error.core.unexpectedTermVariantError(cx)(input))),
         Tuple2("unexpectedTypeVariant", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.UnexpectedTypeVariantError, hydra.errors.Error, hydra.errors.DecodingError]((t: hydra.error.core.UnexpectedTypeVariantError) => hydra.errors.Error.unexpectedTypeVariant(t))(hydra.decode.error.core.unexpectedTypeVariantError(cx)(input))),
         Tuple2("unification", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.errors.UnificationError, hydra.errors.Error, hydra.errors.DecodingError]((t: hydra.errors.UnificationError) => hydra.errors.Error.unification(t))(hydra.decode.errors.unificationError(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.errors.Error], (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.errors.Error]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.errors.Error])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.errors.Error]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def otherError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.errors.OtherError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.errors.OtherError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[scala.Predef.String, hydra.errors.OtherError,
     hydra.errors.DecodingError]((b: scala.Predef.String) => b)(hydra.lib.eithers.either[scala.Predef.String,
     hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def unificationError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.errors.UnificationError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.errors.UnificationError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.errors.UnificationError](hydra.extract.core.requireField("leftType")(hydra.decode.core.`type`)(fieldMap)(cx))((field_leftType: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.errors.UnificationError](hydra.extract.core.requireField("rightType")(hydra.decode.core.`type`)(fieldMap)(cx))((field_rightType: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.errors.UnificationError](hydra.extract.core.requireField("message")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_message: scala.Predef.String) =>
      Right(hydra.errors.UnificationError(field_leftType, field_rightType, field_message)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
