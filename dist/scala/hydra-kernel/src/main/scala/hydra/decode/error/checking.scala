package hydra.decode.error.checking

import hydra.core.*

import hydra.error.checking.*

import hydra.errors.*

def checkingError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.checking.CheckingError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.error.checking.CheckingError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.inject(v_Term_inject_inj) => {
    lazy val field: hydra.core.Field = (v_Term_inject_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.error.checking.CheckingError])] = hydra.lib.maps.fromList[hydra.core.Name,
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
         Tuple2("other", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.checking.OtherCheckingError, hydra.error.checking.CheckingError,
         hydra.errors.DecodingError]((t: hydra.error.checking.OtherCheckingError) => hydra.error.checking.CheckingError.other(t))(hydra.decode.error.checking.otherCheckingError(cx)(input))),
         Tuple2("typeArityMismatch", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.checking.TypeArityMismatchError, hydra.error.checking.CheckingError,
         hydra.errors.DecodingError]((t: hydra.error.checking.TypeArityMismatchError) => hydra.error.checking.CheckingError.typeArityMismatch(t))(hydra.decode.error.checking.typeArityMismatchError(cx)(input))),
         Tuple2("typeMismatch", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.checking.TypeMismatchError, hydra.error.checking.CheckingError,
         hydra.errors.DecodingError]((t: hydra.error.checking.TypeMismatchError) => hydra.error.checking.CheckingError.typeMismatch(t))(hydra.decode.error.checking.typeMismatchError(cx)(input))),
         Tuple2("unboundTypeVariables", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.checking.UnboundTypeVariablesError, hydra.error.checking.CheckingError,
         hydra.errors.DecodingError]((t: hydra.error.checking.UnboundTypeVariablesError) => hydra.error.checking.CheckingError.unboundTypeVariables(t))(hydra.decode.error.checking.unboundTypeVariablesError(cx)(input))),
         Tuple2("undefinedTermVariable", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.checking.UndefinedTermVariableCheckingError,
         hydra.error.checking.CheckingError, hydra.errors.DecodingError]((t: hydra.error.checking.UndefinedTermVariableCheckingError) => hydra.error.checking.CheckingError.undefinedTermVariable(t))(hydra.decode.error.checking.undefinedTermVariableCheckingError(cx)(input))),
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
         hydra.errors.DecodingError]((t: hydra.error.checking.UntypedLetBindingError) => hydra.error.checking.CheckingError.untypedLetBinding(t))(hydra.decode.error.checking.untypedLetBindingError(cx)(input))),
         Tuple2("untypedTermVariable", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.checking.UntypedTermVariableCheckingError,
         hydra.error.checking.CheckingError, hydra.errors.DecodingError]((t: hydra.error.checking.UntypedTermVariableCheckingError) => hydra.error.checking.CheckingError.untypedTermVariable(t))(hydra.decode.error.checking.untypedTermVariableCheckingError(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.error.checking.CheckingError],
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.error.checking.CheckingError]](Left(hydra.lib.strings.cat(Seq("no such field ",
       fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.error.checking.CheckingError])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.error.checking.CheckingError]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def incorrectUnificationError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.checking.IncorrectUnificationError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.error.checking.IncorrectUnificationError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.typing.TypeSubst, hydra.error.checking.IncorrectUnificationError](hydra.extract.core.requireField("substitution")(hydra.decode.typing.typeSubst)(fieldMap)(cx))((field_substitution: hydra.typing.TypeSubst) =>
      Right(hydra.error.checking.IncorrectUnificationError(field_substitution)))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def notAForallTypeError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.checking.NotAForallTypeError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.error.checking.NotAForallTypeError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.error.checking.NotAForallTypeError](hydra.extract.core.requireField("type")(hydra.decode.core.`type`)(fieldMap)(cx))((field_type: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.core.Type], hydra.error.checking.NotAForallTypeError](hydra.extract.core.requireField("typeArguments")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeList(hydra.decode.core.`type`)(v1)(v2))(fieldMap)(cx))((field_typeArguments: Seq[hydra.core.Type]) =>
      Right(hydra.error.checking.NotAForallTypeError(field_type, field_typeArguments))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def notAFunctionTypeError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.checking.NotAFunctionTypeError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.error.checking.NotAFunctionTypeError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.error.checking.NotAFunctionTypeError](hydra.extract.core.requireField("type")(hydra.decode.core.`type`)(fieldMap)(cx))((field_type: hydra.core.Type) =>
      Right(hydra.error.checking.NotAFunctionTypeError(field_type)))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def otherCheckingError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.checking.OtherCheckingError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.error.checking.OtherCheckingError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.paths.SubtermPath, hydra.error.checking.OtherCheckingError](hydra.extract.core.requireField("path")(hydra.decode.paths.subtermPath)(fieldMap)(cx))((field_path: hydra.paths.SubtermPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.error.checking.OtherCheckingError](hydra.extract.core.requireField("message")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_message: scala.Predef.String) =>
      Right(hydra.error.checking.OtherCheckingError(field_path, field_message))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def typeArityMismatchError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.checking.TypeArityMismatchError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.error.checking.TypeArityMismatchError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.error.checking.TypeArityMismatchError](hydra.extract.core.requireField("type")(hydra.decode.core.`type`)(fieldMap)(cx))((field_type: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Int, hydra.error.checking.TypeArityMismatchError](hydra.extract.core.requireField("expectedArity")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         Int]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_expectedArity: Int) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Int, hydra.error.checking.TypeArityMismatchError](hydra.extract.core.requireField("actualArity")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         Int]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_actualArity: Int) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.core.Type], hydra.error.checking.TypeArityMismatchError](hydra.extract.core.requireField("typeArguments")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeList(hydra.decode.core.`type`)(v1)(v2))(fieldMap)(cx))((field_typeArguments: Seq[hydra.core.Type]) =>
      Right(hydra.error.checking.TypeArityMismatchError(field_type, field_expectedArity,
         field_actualArity, field_typeArguments))))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def typeMismatchError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.checking.TypeMismatchError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.error.checking.TypeMismatchError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.error.checking.TypeMismatchError](hydra.extract.core.requireField("expectedType")(hydra.decode.core.`type`)(fieldMap)(cx))((field_expectedType: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.error.checking.TypeMismatchError](hydra.extract.core.requireField("actualType")(hydra.decode.core.`type`)(fieldMap)(cx))((field_actualType: hydra.core.Type) =>
      Right(hydra.error.checking.TypeMismatchError(field_expectedType, field_actualType))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def unboundTypeVariablesError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.checking.UnboundTypeVariablesError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.error.checking.UnboundTypeVariablesError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.collection.immutable.Set[hydra.core.Name],
       hydra.error.checking.UnboundTypeVariablesError](hydra.extract.core.requireField("variables")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) => hydra.extract.core.decodeSet(hydra.decode.core.name)(v1)(v2))(fieldMap)(cx))((field_variables: scala.collection.immutable.Set[hydra.core.Name]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.error.checking.UnboundTypeVariablesError](hydra.extract.core.requireField("type")(hydra.decode.core.`type`)(fieldMap)(cx))((field_type: hydra.core.Type) =>
      Right(hydra.error.checking.UnboundTypeVariablesError(field_variables, field_type))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def undefinedTermVariableCheckingError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.checking.UndefinedTermVariableCheckingError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.error.checking.UndefinedTermVariableCheckingError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.paths.SubtermPath, hydra.error.checking.UndefinedTermVariableCheckingError](hydra.extract.core.requireField("path")(hydra.decode.paths.subtermPath)(fieldMap)(cx))((field_path: hydra.paths.SubtermPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.checking.UndefinedTermVariableCheckingError](hydra.extract.core.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.checking.UndefinedTermVariableCheckingError(field_path, field_name))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def unequalTypesError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.checking.UnequalTypesError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.error.checking.UnequalTypesError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.core.Type], hydra.error.checking.UnequalTypesError](hydra.extract.core.requireField("types")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeList(hydra.decode.core.`type`)(v1)(v2))(fieldMap)(cx))((field_types: Seq[hydra.core.Type]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.error.checking.UnequalTypesError](hydra.extract.core.requireField("description")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_description: scala.Predef.String) =>
      Right(hydra.error.checking.UnequalTypesError(field_types, field_description))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def unsupportedTermVariantError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.checking.UnsupportedTermVariantError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.error.checking.UnsupportedTermVariantError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.variants.TermVariant,
       hydra.error.checking.UnsupportedTermVariantError](hydra.extract.core.requireField("termVariant")(hydra.decode.variants.termVariant)(fieldMap)(cx))((field_termVariant: hydra.variants.TermVariant) =>
      Right(hydra.error.checking.UnsupportedTermVariantError(field_termVariant)))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def untypedLambdaError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.checking.UntypedLambdaError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.error.checking.UntypedLambdaError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    Right(hydra.error.checking.UntypedLambdaError())
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def untypedLetBindingError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.checking.UntypedLetBindingError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.error.checking.UntypedLetBindingError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Binding, hydra.error.checking.UntypedLetBindingError](hydra.extract.core.requireField("binding")(hydra.decode.core.binding)(fieldMap)(cx))((field_binding: hydra.core.Binding) =>
      Right(hydra.error.checking.UntypedLetBindingError(field_binding)))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def untypedTermVariableCheckingError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.checking.UntypedTermVariableCheckingError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.error.checking.UntypedTermVariableCheckingError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.paths.SubtermPath, hydra.error.checking.UntypedTermVariableCheckingError](hydra.extract.core.requireField("path")(hydra.decode.paths.subtermPath)(fieldMap)(cx))((field_path: hydra.paths.SubtermPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.checking.UntypedTermVariableCheckingError](hydra.extract.core.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.checking.UntypedTermVariableCheckingError(field_path, field_name))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))
