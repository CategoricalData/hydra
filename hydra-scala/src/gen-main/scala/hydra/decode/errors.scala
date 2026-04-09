package hydra.decode.errors

import hydra.core.*

import hydra.errors.*

def decodingError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.errors.DecodingError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.errors.DecodingError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[scala.Predef.String, hydra.errors.DecodingError,
     hydra.errors.DecodingError]((b: scala.Predef.String) => b)(hydra.lib.eithers.either[hydra.errors.DecodingError,
     hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def emptyListError(cx: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.errors.DecodingError, Unit] = hydra.extract.core.decodeUnit(cx)(t)

def error(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.errors.Error] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.errors.Error]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
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
         Tuple2("extraction", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.errors.ExtractionError, hydra.errors.Error, hydra.errors.DecodingError]((t: hydra.errors.ExtractionError) => hydra.errors.Error.extraction(t))(hydra.decode.errors.extractionError(cx)(input))),
         Tuple2("inference", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.errors.InferenceError, hydra.errors.Error, hydra.errors.DecodingError]((t: hydra.errors.InferenceError) => hydra.errors.Error.inference(t))(hydra.decode.errors.inferenceError(cx)(input))),
         Tuple2("other", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.errors.OtherError, hydra.errors.Error, hydra.errors.DecodingError]((t: hydra.errors.OtherError) => hydra.errors.Error.other(t))(hydra.decode.errors.otherError(cx)(input))),
         Tuple2("resolution", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.errors.ResolutionError, hydra.errors.Error, hydra.errors.DecodingError]((t: hydra.errors.ResolutionError) => hydra.errors.Error.resolution(t))(hydra.decode.errors.resolutionError(cx)(input))),
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
  case _ => Left("expected union"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def extractionError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.errors.ExtractionError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.errors.ExtractionError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.errors.ExtractionError])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.errors.ExtractionError]](Seq(Tuple2("emptyList",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.errors.ExtractionError, hydra.errors.DecodingError]((t: Unit) => hydra.errors.ExtractionError.emptyList(t))(hydra.decode.errors.emptyListError(cx)(input))),
         Tuple2("multipleBindings", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.errors.MultipleBindingsError, hydra.errors.ExtractionError, hydra.errors.DecodingError]((t: hydra.errors.MultipleBindingsError) => hydra.errors.ExtractionError.multipleBindings(t))(hydra.decode.errors.multipleBindingsError(cx)(input))),
         Tuple2("multipleFields", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.errors.MultipleFieldsError, hydra.errors.ExtractionError, hydra.errors.DecodingError]((t: hydra.errors.MultipleFieldsError) => hydra.errors.ExtractionError.multipleFields(t))(hydra.decode.errors.multipleFieldsError(cx)(input))),
         Tuple2("noMatchingField", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.errors.NoMatchingFieldError, hydra.errors.ExtractionError, hydra.errors.DecodingError]((t: hydra.errors.NoMatchingFieldError) => hydra.errors.ExtractionError.noMatchingField(t))(hydra.decode.errors.noMatchingFieldError(cx)(input))),
         Tuple2("noSuchBinding", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.errors.NoSuchBindingError, hydra.errors.ExtractionError, hydra.errors.DecodingError]((t: hydra.errors.NoSuchBindingError) => hydra.errors.ExtractionError.noSuchBinding(t))(hydra.decode.errors.noSuchBindingError(cx)(input))),
         Tuple2("notEnoughCases", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.errors.ExtractionError, hydra.errors.DecodingError]((t: Unit) => hydra.errors.ExtractionError.notEnoughCases(t))(hydra.decode.errors.notEnoughCasesError(cx)(input))),
         Tuple2("unexpectedShape", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.errors.UnexpectedShapeError, hydra.errors.ExtractionError, hydra.errors.DecodingError]((t: hydra.errors.UnexpectedShapeError) => hydra.errors.ExtractionError.unexpectedShape(t))(hydra.decode.errors.unexpectedShapeError(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.errors.ExtractionError], (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.errors.ExtractionError]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.errors.ExtractionError])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.errors.ExtractionError]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def inferenceError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.errors.InferenceError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.errors.InferenceError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.errors.InferenceError])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.errors.InferenceError]](Seq(Tuple2("checking",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.checking.CheckingError, hydra.errors.InferenceError, hydra.errors.DecodingError]((t: hydra.error.checking.CheckingError) => hydra.errors.InferenceError.checking(t))(hydra.decode.error.checking.checkingError(cx)(input))),
         Tuple2("other", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.errors.OtherInferenceError, hydra.errors.InferenceError, hydra.errors.DecodingError]((t: hydra.errors.OtherInferenceError) => hydra.errors.InferenceError.other(t))(hydra.decode.errors.otherInferenceError(cx)(input))),
         Tuple2("unification", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.errors.UnificationInferenceError, hydra.errors.InferenceError, hydra.errors.DecodingError]((t: hydra.errors.UnificationInferenceError) => hydra.errors.InferenceError.unification(t))(hydra.decode.errors.unificationInferenceError(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.errors.InferenceError], (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.errors.InferenceError]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.errors.InferenceError])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.errors.InferenceError]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def multipleBindingsError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.errors.MultipleBindingsError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.errors.MultipleBindingsError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.errors.MultipleBindingsError](hydra.extract.core.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) => Right(hydra.errors.MultipleBindingsError(field_name)))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def multipleFieldsError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.errors.MultipleFieldsError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.errors.MultipleFieldsError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.errors.MultipleFieldsError](hydra.extract.core.requireField("fieldName")(hydra.decode.core.name)(fieldMap)(cx))((field_fieldName: hydra.core.Name) => Right(hydra.errors.MultipleFieldsError(field_fieldName)))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def noMatchingFieldError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.errors.NoMatchingFieldError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.errors.NoMatchingFieldError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.errors.NoMatchingFieldError](hydra.extract.core.requireField("fieldName")(hydra.decode.core.name)(fieldMap)(cx))((field_fieldName: hydra.core.Name) => Right(hydra.errors.NoMatchingFieldError(field_fieldName)))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def noSuchBindingError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.errors.NoSuchBindingError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.errors.NoSuchBindingError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.errors.NoSuchBindingError](hydra.extract.core.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) => Right(hydra.errors.NoSuchBindingError(field_name)))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def noSuchPrimitiveError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.errors.NoSuchPrimitiveError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.errors.NoSuchPrimitiveError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.errors.NoSuchPrimitiveError](hydra.extract.core.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) => Right(hydra.errors.NoSuchPrimitiveError(field_name)))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def notEnoughCasesError(cx: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.errors.DecodingError, Unit] = hydra.extract.core.decodeUnit(cx)(t)

def otherError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.errors.OtherError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.errors.OtherError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[scala.Predef.String, hydra.errors.OtherError,
     hydra.errors.DecodingError]((b: scala.Predef.String) => b)(hydra.lib.eithers.either[hydra.errors.DecodingError,
     hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def otherInferenceError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.errors.OtherInferenceError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.errors.OtherInferenceError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.paths.SubtermPath, hydra.errors.OtherInferenceError](hydra.extract.core.requireField("path")(hydra.decode.paths.subtermPath)(fieldMap)(cx))((field_path: hydra.paths.SubtermPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.errors.OtherInferenceError](hydra.extract.core.requireField("message")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_message: scala.Predef.String) =>
      Right(hydra.errors.OtherInferenceError(field_path, field_message))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def otherResolutionError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.errors.OtherResolutionError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.errors.OtherResolutionError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[scala.Predef.String, hydra.errors.OtherResolutionError,
     hydra.errors.DecodingError]((b: scala.Predef.String) => b)(hydra.lib.eithers.either[hydra.errors.DecodingError,
     hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def resolutionError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.errors.ResolutionError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.errors.ResolutionError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.errors.ResolutionError])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.errors.ResolutionError]](Seq(Tuple2("noSuchBinding",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.errors.NoSuchBindingError, hydra.errors.ResolutionError, hydra.errors.DecodingError]((t: hydra.errors.NoSuchBindingError) => hydra.errors.ResolutionError.noSuchBinding(t))(hydra.decode.errors.noSuchBindingError(cx)(input))),
         Tuple2("noSuchPrimitive", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.errors.NoSuchPrimitiveError, hydra.errors.ResolutionError, hydra.errors.DecodingError]((t: hydra.errors.NoSuchPrimitiveError) => hydra.errors.ResolutionError.noSuchPrimitive(t))(hydra.decode.errors.noSuchPrimitiveError(cx)(input))),
         Tuple2("noMatchingField", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.errors.NoMatchingFieldError, hydra.errors.ResolutionError, hydra.errors.DecodingError]((t: hydra.errors.NoMatchingFieldError) => hydra.errors.ResolutionError.noMatchingField(t))(hydra.decode.errors.noMatchingFieldError(cx)(input))),
         Tuple2("other", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.errors.OtherResolutionError, hydra.errors.ResolutionError, hydra.errors.DecodingError]((t: hydra.errors.OtherResolutionError) => hydra.errors.ResolutionError.other(t))(hydra.decode.errors.otherResolutionError(cx)(input))),
         Tuple2("unexpectedShape", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.errors.UnexpectedShapeError, hydra.errors.ResolutionError, hydra.errors.DecodingError]((t: hydra.errors.UnexpectedShapeError) => hydra.errors.ResolutionError.unexpectedShape(t))(hydra.decode.errors.unexpectedShapeError(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.errors.ResolutionError], (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.errors.ResolutionError]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.errors.ResolutionError])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.errors.ResolutionError]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def unexpectedShapeError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.errors.UnexpectedShapeError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.errors.UnexpectedShapeError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.errors.UnexpectedShapeError](hydra.extract.core.requireField("expected")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_expected: scala.Predef.String) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.errors.UnexpectedShapeError](hydra.extract.core.requireField("actual")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_actual: scala.Predef.String) =>
      Right(hydra.errors.UnexpectedShapeError(field_expected, field_actual))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def unificationError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.errors.UnificationError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.errors.UnificationError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.errors.UnificationError](hydra.extract.core.requireField("leftType")(hydra.decode.core.`type`)(fieldMap)(cx))((field_leftType: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.errors.UnificationError](hydra.extract.core.requireField("rightType")(hydra.decode.core.`type`)(fieldMap)(cx))((field_rightType: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.errors.UnificationError](hydra.extract.core.requireField("message")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_message: scala.Predef.String) =>
      Right(hydra.errors.UnificationError(field_leftType, field_rightType, field_message)))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def unificationInferenceError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.errors.UnificationInferenceError] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.errors.UnificationInferenceError]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.paths.SubtermPath, hydra.errors.UnificationInferenceError](hydra.extract.core.requireField("path")(hydra.decode.paths.subtermPath)(fieldMap)(cx))((field_path: hydra.paths.SubtermPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.errors.UnificationError, hydra.errors.UnificationInferenceError](hydra.extract.core.requireField("cause")(hydra.decode.errors.unificationError)(fieldMap)(cx))((field_cause: hydra.errors.UnificationError) =>
      Right(hydra.errors.UnificationInferenceError(field_path, field_cause))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))
