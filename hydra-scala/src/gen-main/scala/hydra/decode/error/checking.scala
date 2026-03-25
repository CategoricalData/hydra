package hydra.decode.error.checking

import hydra.core.*

import hydra.error.checking.*

import hydra.errors.*

import hydra.lib.eithers

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.strings

def checkingError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.checking.CheckingError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.checking.CheckingError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.error.checking.CheckingError])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.error.checking.CheckingError]](Seq(Tuple2("incorrectUnification",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.checking.IncorrectUnificationError, hydra.error.checking.CheckingError,
         hydra.errors.DecodingError]((t: hydra.error.checking.IncorrectUnificationError) => hydra.error.checking.CheckingError.incorrectUnification(t))(hydra.decode.error.checking.incorrectUnificationError(cx)(input))),
         Tuple2("notAForallType", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.checking.NotAForallTypeError, hydra.error.checking.CheckingError,
         hydra.errors.DecodingError]((t: hydra.error.checking.NotAForallTypeError) => hydra.error.checking.CheckingError.notAForallType(t))(hydra.decode.error.checking.notAForallTypeError(cx)(input))),
         Tuple2("notAFunctionType", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.checking.NotAFunctionTypeError, hydra.error.checking.CheckingError,
         hydra.errors.DecodingError]((t: hydra.error.checking.NotAFunctionTypeError) => hydra.error.checking.CheckingError.notAFunctionType(t))(hydra.decode.error.checking.notAFunctionTypeError(cx)(input))),
         Tuple2("typeArityMismatch", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.checking.TypeArityMismatchError, hydra.error.checking.CheckingError,
         hydra.errors.DecodingError]((t: hydra.error.checking.TypeArityMismatchError) => hydra.error.checking.CheckingError.typeArityMismatch(t))(hydra.decode.error.checking.typeArityMismatchError(cx)(input))),
         Tuple2("typeMismatch", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.checking.TypeMismatchError, hydra.error.checking.CheckingError,
         hydra.errors.DecodingError]((t: hydra.error.checking.TypeMismatchError) => hydra.error.checking.CheckingError.typeMismatch(t))(hydra.decode.error.checking.typeMismatchError(cx)(input))),
         Tuple2("unboundTypeVariables", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.checking.UnboundTypeVariablesError, hydra.error.checking.CheckingError,
         hydra.errors.DecodingError]((t: hydra.error.checking.UnboundTypeVariablesError) => hydra.error.checking.CheckingError.unboundTypeVariables(t))(hydra.decode.error.checking.unboundTypeVariablesError(cx)(input))),
         Tuple2("unequalTypes", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.checking.UnequalTypesError, hydra.error.checking.CheckingError,
         hydra.errors.DecodingError]((t: hydra.error.checking.UnequalTypesError) => hydra.error.checking.CheckingError.unequalTypes(t))(hydra.decode.error.checking.unequalTypesError(cx)(input))),
         Tuple2("unsupportedTermVariant", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.checking.UnsupportedTermVariantError, hydra.error.checking.CheckingError,
         hydra.errors.DecodingError]((t: hydra.error.checking.UnsupportedTermVariantError) => hydra.error.checking.CheckingError.unsupportedTermVariant(t))(hydra.decode.error.checking.unsupportedTermVariantError(cx)(input))),
         Tuple2("untypedLambda", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.checking.UntypedLambdaError, hydra.error.checking.CheckingError,
         hydra.errors.DecodingError]((t: hydra.error.checking.UntypedLambdaError) => hydra.error.checking.CheckingError.untypedLambda(t))(hydra.decode.error.checking.untypedLambdaError(cx)(input))),
         Tuple2("untypedLetBinding", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.checking.UntypedLetBindingError, hydra.error.checking.CheckingError,
         hydra.errors.DecodingError]((t: hydra.error.checking.UntypedLetBindingError) => hydra.error.checking.CheckingError.untypedLetBinding(t))(hydra.decode.error.checking.untypedLetBindingError(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.error.checking.CheckingError], (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.error.checking.CheckingError]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.error.checking.CheckingError])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.error.checking.CheckingError]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def incorrectUnificationError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.checking.IncorrectUnificationError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.checking.IncorrectUnificationError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.typing.TypeSubst, hydra.error.checking.IncorrectUnificationError](hydra.extract.helpers.requireField("substitution")(hydra.decode.typing.typeSubst)(fieldMap)(cx))((field_substitution: hydra.typing.TypeSubst) =>
      Right(hydra.error.checking.IncorrectUnificationError(field_substitution)))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def notAForallTypeError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.checking.NotAForallTypeError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.checking.NotAForallTypeError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.error.checking.NotAForallTypeError](hydra.extract.helpers.requireField("type")(hydra.decode.core.`type`)(fieldMap)(cx))((field_type: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.core.Type], hydra.error.checking.NotAForallTypeError](hydra.extract.helpers.requireField("typeArguments")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.core.`type`)(v1)(v2))(fieldMap)(cx))((field_typeArguments: Seq[hydra.core.Type]) =>
      Right(hydra.error.checking.NotAForallTypeError(field_type, field_typeArguments))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def notAFunctionTypeError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.checking.NotAFunctionTypeError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.checking.NotAFunctionTypeError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.error.checking.NotAFunctionTypeError](hydra.extract.helpers.requireField("type")(hydra.decode.core.`type`)(fieldMap)(cx))((field_type: hydra.core.Type) =>
      Right(hydra.error.checking.NotAFunctionTypeError(field_type)))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def typeArityMismatchError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.checking.TypeArityMismatchError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.checking.TypeArityMismatchError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.error.checking.TypeArityMismatchError](hydra.extract.helpers.requireField("type")(hydra.decode.core.`type`)(fieldMap)(cx))((field_type: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Int, hydra.error.checking.TypeArityMismatchError](hydra.extract.helpers.requireField("expectedArity")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError,
         Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_expectedArity: Int) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Int, hydra.error.checking.TypeArityMismatchError](hydra.extract.helpers.requireField("actualArity")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError,
         Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_actualArity: Int) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.core.Type], hydra.error.checking.TypeArityMismatchError](hydra.extract.helpers.requireField("typeArguments")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.core.`type`)(v1)(v2))(fieldMap)(cx))((field_typeArguments: Seq[hydra.core.Type]) =>
      Right(hydra.error.checking.TypeArityMismatchError(field_type, field_expectedArity, field_actualArity, field_typeArguments))))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def typeMismatchError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.checking.TypeMismatchError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.checking.TypeMismatchError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.error.checking.TypeMismatchError](hydra.extract.helpers.requireField("expectedType")(hydra.decode.core.`type`)(fieldMap)(cx))((field_expectedType: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.error.checking.TypeMismatchError](hydra.extract.helpers.requireField("actualType")(hydra.decode.core.`type`)(fieldMap)(cx))((field_actualType: hydra.core.Type) =>
      Right(hydra.error.checking.TypeMismatchError(field_expectedType, field_actualType))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def unboundTypeVariablesError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.checking.UnboundTypeVariablesError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.checking.UnboundTypeVariablesError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.collection.immutable.Set[hydra.core.Name],
       hydra.error.checking.UnboundTypeVariablesError](hydra.extract.helpers.requireField("variables")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeSet(hydra.decode.core.name)(v1)(v2))(fieldMap)(cx))((field_variables: scala.collection.immutable.Set[hydra.core.Name]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.error.checking.UnboundTypeVariablesError](hydra.extract.helpers.requireField("type")(hydra.decode.core.`type`)(fieldMap)(cx))((field_type: hydra.core.Type) =>
      Right(hydra.error.checking.UnboundTypeVariablesError(field_variables, field_type))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def unequalTypesError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.checking.UnequalTypesError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.checking.UnequalTypesError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.core.Type], hydra.error.checking.UnequalTypesError](hydra.extract.helpers.requireField("types")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.core.`type`)(v1)(v2))(fieldMap)(cx))((field_types: Seq[hydra.core.Type]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.error.checking.UnequalTypesError](hydra.extract.helpers.requireField("description")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_description: scala.Predef.String) =>
      Right(hydra.error.checking.UnequalTypesError(field_types, field_description))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def unsupportedTermVariantError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.checking.UnsupportedTermVariantError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.checking.UnsupportedTermVariantError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.variants.TermVariant, hydra.error.checking.UnsupportedTermVariantError](hydra.extract.helpers.requireField("termVariant")(hydra.decode.variants.termVariant)(fieldMap)(cx))((field_termVariant: hydra.variants.TermVariant) =>
      Right(hydra.error.checking.UnsupportedTermVariantError(field_termVariant)))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def untypedLambdaError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.checking.UntypedLambdaError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.checking.UntypedLambdaError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    Right(hydra.error.checking.UntypedLambdaError())
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def untypedLetBindingError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.checking.UntypedLetBindingError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.checking.UntypedLetBindingError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Binding, hydra.error.checking.UntypedLetBindingError](hydra.extract.helpers.requireField("binding")(hydra.decode.core.binding)(fieldMap)(cx))((field_binding: hydra.core.Binding) =>
      Right(hydra.error.checking.UntypedLetBindingError(field_binding)))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
