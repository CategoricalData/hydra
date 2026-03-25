package hydra.decode.error.core

import hydra.core.*

import hydra.error.core.*

import hydra.errors.*

import hydra.lib.eithers

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.strings

def constantConditionError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.ConstantConditionError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.ConstantConditionError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.ConstantConditionError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Boolean, hydra.error.core.ConstantConditionError](hydra.extract.helpers.requireField("value")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError,
         Boolean]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.boolean(v_Literal_boolean_b) => Right(v_Literal_boolean_b)
        case _ => Left("expected boolean literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_value: Boolean) =>
      Right(hydra.error.core.ConstantConditionError(field_location, field_value))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def duplicateBindingError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.DuplicateBindingError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.DuplicateBindingError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.DuplicateBindingError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.DuplicateBindingError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.DuplicateBindingError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def duplicateFieldError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.DuplicateFieldError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.DuplicateFieldError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.DuplicateFieldError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.DuplicateFieldError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.DuplicateFieldError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def duplicateRecordTypeFieldNamesError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.core.DuplicateRecordTypeFieldNamesError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.DuplicateRecordTypeFieldNamesError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.DuplicateRecordTypeFieldNamesError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.DuplicateRecordTypeFieldNamesError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.DuplicateRecordTypeFieldNamesError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def duplicateUnionTypeFieldNamesError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.core.DuplicateUnionTypeFieldNamesError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.DuplicateUnionTypeFieldNamesError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.DuplicateUnionTypeFieldNamesError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.DuplicateUnionTypeFieldNamesError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.DuplicateUnionTypeFieldNamesError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def emptyCaseStatementError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.EmptyCaseStatementError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.EmptyCaseStatementError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.EmptyCaseStatementError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.EmptyCaseStatementError](hydra.extract.helpers.requireField("typeName")(hydra.decode.core.name)(fieldMap)(cx))((field_typeName: hydra.core.Name) =>
      Right(hydra.error.core.EmptyCaseStatementError(field_location, field_typeName))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def emptyLetBindingsError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.EmptyLetBindingsError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.EmptyLetBindingsError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.EmptyLetBindingsError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      Right(hydra.error.core.EmptyLetBindingsError(field_location)))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def emptyRecordTypeError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.EmptyRecordTypeError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.EmptyRecordTypeError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.EmptyRecordTypeError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) => Right(hydra.error.core.EmptyRecordTypeError(field_location)))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def emptyTermAnnotationError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.EmptyTermAnnotationError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.EmptyTermAnnotationError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.EmptyTermAnnotationError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      Right(hydra.error.core.EmptyTermAnnotationError(field_location)))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def emptyTypeAnnotationError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.EmptyTypeAnnotationError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.EmptyTypeAnnotationError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.EmptyTypeAnnotationError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      Right(hydra.error.core.EmptyTypeAnnotationError(field_location)))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def emptyTypeNameInTermError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.EmptyTypeNameInTermError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.EmptyTypeNameInTermError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.EmptyTypeNameInTermError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      Right(hydra.error.core.EmptyTypeNameInTermError(field_location)))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def emptyUnionTypeError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.EmptyUnionTypeError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.EmptyUnionTypeError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.EmptyUnionTypeError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) => Right(hydra.error.core.EmptyUnionTypeError(field_location)))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def invalidForallParameterNameError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.core.InvalidForallParameterNameError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.InvalidForallParameterNameError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.InvalidForallParameterNameError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.InvalidForallParameterNameError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.InvalidForallParameterNameError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def invalidLambdaParameterNameError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.core.InvalidLambdaParameterNameError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.InvalidLambdaParameterNameError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.InvalidLambdaParameterNameError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.InvalidLambdaParameterNameError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.InvalidLambdaParameterNameError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def invalidLetBindingNameError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.InvalidLetBindingNameError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.InvalidLetBindingNameError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.InvalidLetBindingNameError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.InvalidLetBindingNameError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.InvalidLetBindingNameError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def invalidTermError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.InvalidTermError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.InvalidTermError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.error.core.InvalidTermError])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.error.core.InvalidTermError]](Seq(Tuple2("constantCondition",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.ConstantConditionError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.ConstantConditionError) => hydra.error.core.InvalidTermError.constantCondition(t))(hydra.decode.error.core.constantConditionError(cx)(input))),
         Tuple2("duplicateBinding", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.DuplicateBindingError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.DuplicateBindingError) => hydra.error.core.InvalidTermError.duplicateBinding(t))(hydra.decode.error.core.duplicateBindingError(cx)(input))),
         Tuple2("duplicateField", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.DuplicateFieldError, hydra.error.core.InvalidTermError, hydra.errors.DecodingError]((t: hydra.error.core.DuplicateFieldError) => hydra.error.core.InvalidTermError.duplicateField(t))(hydra.decode.error.core.duplicateFieldError(cx)(input))),
         Tuple2("emptyCaseStatement", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.EmptyCaseStatementError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.EmptyCaseStatementError) => hydra.error.core.InvalidTermError.emptyCaseStatement(t))(hydra.decode.error.core.emptyCaseStatementError(cx)(input))),
         Tuple2("emptyLetBindings", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.EmptyLetBindingsError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.EmptyLetBindingsError) => hydra.error.core.InvalidTermError.emptyLetBindings(t))(hydra.decode.error.core.emptyLetBindingsError(cx)(input))),
         Tuple2("emptyTermAnnotation", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.EmptyTermAnnotationError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.EmptyTermAnnotationError) => hydra.error.core.InvalidTermError.emptyTermAnnotation(t))(hydra.decode.error.core.emptyTermAnnotationError(cx)(input))),
         Tuple2("emptyTypeNameInTerm", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.EmptyTypeNameInTermError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.EmptyTypeNameInTermError) => hydra.error.core.InvalidTermError.emptyTypeNameInTerm(t))(hydra.decode.error.core.emptyTypeNameInTermError(cx)(input))),
         Tuple2("invalidLambdaParameterName", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.InvalidLambdaParameterNameError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.InvalidLambdaParameterNameError) =>
      hydra.error.core.InvalidTermError.invalidLambdaParameterName(t))(hydra.decode.error.core.invalidLambdaParameterNameError(cx)(input))),
         Tuple2("invalidLetBindingName", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.InvalidLetBindingNameError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.InvalidLetBindingNameError) => hydra.error.core.InvalidTermError.invalidLetBindingName(t))(hydra.decode.error.core.invalidLetBindingNameError(cx)(input))),
         Tuple2("invalidTypeLambdaParameterName", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.InvalidTypeLambdaParameterNameError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.InvalidTypeLambdaParameterNameError) =>
      hydra.error.core.InvalidTermError.invalidTypeLambdaParameterName(t))(hydra.decode.error.core.invalidTypeLambdaParameterNameError(cx)(input))),
         Tuple2("nestedTermAnnotation", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.NestedTermAnnotationError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.NestedTermAnnotationError) => hydra.error.core.InvalidTermError.nestedTermAnnotation(t))(hydra.decode.error.core.nestedTermAnnotationError(cx)(input))),
         Tuple2("redundantWrapUnwrap", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.RedundantWrapUnwrapError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.RedundantWrapUnwrapError) => hydra.error.core.InvalidTermError.redundantWrapUnwrap(t))(hydra.decode.error.core.redundantWrapUnwrapError(cx)(input))),
         Tuple2("selfApplication", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.SelfApplicationError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.SelfApplicationError) => hydra.error.core.InvalidTermError.selfApplication(t))(hydra.decode.error.core.selfApplicationError(cx)(input))),
         Tuple2("termVariableShadowing", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.TermVariableShadowingError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.TermVariableShadowingError) => hydra.error.core.InvalidTermError.termVariableShadowing(t))(hydra.decode.error.core.termVariableShadowingError(cx)(input))),
         Tuple2("typeVariableShadowingInTypeLambda", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.TypeVariableShadowingInTypeLambdaError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.TypeVariableShadowingInTypeLambdaError) =>
      hydra.error.core.InvalidTermError.typeVariableShadowingInTypeLambda(t))(hydra.decode.error.core.typeVariableShadowingInTypeLambdaError(cx)(input))),
         Tuple2("undefinedTermVariable", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.UndefinedTermVariableError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.UndefinedTermVariableError) => hydra.error.core.InvalidTermError.undefinedTermVariable(t))(hydra.decode.error.core.undefinedTermVariableError(cx)(input))),
         Tuple2("undefinedTypeVariableInBindingType", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.UndefinedTypeVariableInBindingTypeError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.UndefinedTypeVariableInBindingTypeError) =>
      hydra.error.core.InvalidTermError.undefinedTypeVariableInBindingType(t))(hydra.decode.error.core.undefinedTypeVariableInBindingTypeError(cx)(input))),
         Tuple2("undefinedTypeVariableInLambdaDomain", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.UndefinedTypeVariableInLambdaDomainError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.UndefinedTypeVariableInLambdaDomainError) =>
      hydra.error.core.InvalidTermError.undefinedTypeVariableInLambdaDomain(t))(hydra.decode.error.core.undefinedTypeVariableInLambdaDomainError(cx)(input))),
         Tuple2("undefinedTypeVariableInTypeApplication", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.UndefinedTypeVariableInTypeApplicationError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.UndefinedTypeVariableInTypeApplicationError) =>
      hydra.error.core.InvalidTermError.undefinedTypeVariableInTypeApplication(t))(hydra.decode.error.core.undefinedTypeVariableInTypeApplicationError(cx)(input))),
         Tuple2("unknownPrimitiveName", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.UnknownPrimitiveNameError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.UnknownPrimitiveNameError) => hydra.error.core.InvalidTermError.unknownPrimitiveName(t))(hydra.decode.error.core.unknownPrimitiveNameError(cx)(input))),
         Tuple2("unnecessaryIdentityApplication", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.UnnecessaryIdentityApplicationError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.UnnecessaryIdentityApplicationError) =>
      hydra.error.core.InvalidTermError.unnecessaryIdentityApplication(t))(hydra.decode.error.core.unnecessaryIdentityApplicationError(cx)(input))),
         Tuple2("untypedTermVariable", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.UntypedTermVariableError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.UntypedTermVariableError) => hydra.error.core.InvalidTermError.untypedTermVariable(t))(hydra.decode.error.core.untypedTermVariableError(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.error.core.InvalidTermError], (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.error.core.InvalidTermError]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.error.core.InvalidTermError])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.error.core.InvalidTermError]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def invalidTypeError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.InvalidTypeError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.InvalidTypeError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.error.core.InvalidTypeError])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.error.core.InvalidTypeError]](Seq(Tuple2("duplicateRecordTypeFieldNames",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.DuplicateRecordTypeFieldNamesError, hydra.error.core.InvalidTypeError,
         hydra.errors.DecodingError]((t: hydra.error.core.DuplicateRecordTypeFieldNamesError) =>
      hydra.error.core.InvalidTypeError.duplicateRecordTypeFieldNames(t))(hydra.decode.error.core.duplicateRecordTypeFieldNamesError(cx)(input))),
         Tuple2("duplicateUnionTypeFieldNames", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.DuplicateUnionTypeFieldNamesError, hydra.error.core.InvalidTypeError,
         hydra.errors.DecodingError]((t: hydra.error.core.DuplicateUnionTypeFieldNamesError) =>
      hydra.error.core.InvalidTypeError.duplicateUnionTypeFieldNames(t))(hydra.decode.error.core.duplicateUnionTypeFieldNamesError(cx)(input))),
         Tuple2("emptyRecordType", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.EmptyRecordTypeError, hydra.error.core.InvalidTypeError,
         hydra.errors.DecodingError]((t: hydra.error.core.EmptyRecordTypeError) => hydra.error.core.InvalidTypeError.emptyRecordType(t))(hydra.decode.error.core.emptyRecordTypeError(cx)(input))),
         Tuple2("emptyTypeAnnotation", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.EmptyTypeAnnotationError, hydra.error.core.InvalidTypeError,
         hydra.errors.DecodingError]((t: hydra.error.core.EmptyTypeAnnotationError) => hydra.error.core.InvalidTypeError.emptyTypeAnnotation(t))(hydra.decode.error.core.emptyTypeAnnotationError(cx)(input))),
         Tuple2("emptyUnionType", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.EmptyUnionTypeError, hydra.error.core.InvalidTypeError, hydra.errors.DecodingError]((t: hydra.error.core.EmptyUnionTypeError) => hydra.error.core.InvalidTypeError.emptyUnionType(t))(hydra.decode.error.core.emptyUnionTypeError(cx)(input))),
         Tuple2("invalidForallParameterName", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.InvalidForallParameterNameError, hydra.error.core.InvalidTypeError,
         hydra.errors.DecodingError]((t: hydra.error.core.InvalidForallParameterNameError) =>
      hydra.error.core.InvalidTypeError.invalidForallParameterName(t))(hydra.decode.error.core.invalidForallParameterNameError(cx)(input))),
         Tuple2("invalidTypeSchemeVariableName", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.InvalidTypeSchemeVariableNameError, hydra.error.core.InvalidTypeError,
         hydra.errors.DecodingError]((t: hydra.error.core.InvalidTypeSchemeVariableNameError) =>
      hydra.error.core.InvalidTypeError.invalidTypeSchemeVariableName(t))(hydra.decode.error.core.invalidTypeSchemeVariableNameError(cx)(input))),
         Tuple2("nestedTypeAnnotation", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.NestedTypeAnnotationError, hydra.error.core.InvalidTypeError,
         hydra.errors.DecodingError]((t: hydra.error.core.NestedTypeAnnotationError) => hydra.error.core.InvalidTypeError.nestedTypeAnnotation(t))(hydra.decode.error.core.nestedTypeAnnotationError(cx)(input))),
         Tuple2("nonComparableMapKeyType", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.NonComparableMapKeyTypeError, hydra.error.core.InvalidTypeError,
         hydra.errors.DecodingError]((t: hydra.error.core.NonComparableMapKeyTypeError) => hydra.error.core.InvalidTypeError.nonComparableMapKeyType(t))(hydra.decode.error.core.nonComparableMapKeyTypeError(cx)(input))),
         Tuple2("nonComparableSetElementType", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.NonComparableSetElementTypeError, hydra.error.core.InvalidTypeError,
         hydra.errors.DecodingError]((t: hydra.error.core.NonComparableSetElementTypeError) =>
      hydra.error.core.InvalidTypeError.nonComparableSetElementType(t))(hydra.decode.error.core.nonComparableSetElementTypeError(cx)(input))),
         Tuple2("singleVariantUnion", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.SingleVariantUnionError, hydra.error.core.InvalidTypeError,
         hydra.errors.DecodingError]((t: hydra.error.core.SingleVariantUnionError) => hydra.error.core.InvalidTypeError.singleVariantUnion(t))(hydra.decode.error.core.singleVariantUnionError(cx)(input))),
         Tuple2("typeVariableShadowingInForall", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.TypeVariableShadowingInForallError, hydra.error.core.InvalidTypeError,
         hydra.errors.DecodingError]((t: hydra.error.core.TypeVariableShadowingInForallError) =>
      hydra.error.core.InvalidTypeError.typeVariableShadowingInForall(t))(hydra.decode.error.core.typeVariableShadowingInForallError(cx)(input))),
         Tuple2("undefinedTypeVariable", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.UndefinedTypeVariableError, hydra.error.core.InvalidTypeError,
         hydra.errors.DecodingError]((t: hydra.error.core.UndefinedTypeVariableError) => hydra.error.core.InvalidTypeError.undefinedTypeVariable(t))(hydra.decode.error.core.undefinedTypeVariableError(cx)(input))),
         Tuple2("voidInNonBottomPosition", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.VoidInNonBottomPositionError, hydra.error.core.InvalidTypeError,
         hydra.errors.DecodingError]((t: hydra.error.core.VoidInNonBottomPositionError) => hydra.error.core.InvalidTypeError.voidInNonBottomPosition(t))(hydra.decode.error.core.voidInNonBottomPositionError(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.error.core.InvalidTypeError], (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.error.core.InvalidTypeError]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.error.core.InvalidTypeError])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.error.core.InvalidTypeError]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def invalidTypeLambdaParameterNameError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.core.InvalidTypeLambdaParameterNameError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.InvalidTypeLambdaParameterNameError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.InvalidTypeLambdaParameterNameError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.InvalidTypeLambdaParameterNameError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.InvalidTypeLambdaParameterNameError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def invalidTypeSchemeVariableNameError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.core.InvalidTypeSchemeVariableNameError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.InvalidTypeSchemeVariableNameError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.InvalidTypeSchemeVariableNameError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.InvalidTypeSchemeVariableNameError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.InvalidTypeSchemeVariableNameError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def nestedTermAnnotationError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.NestedTermAnnotationError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.NestedTermAnnotationError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.NestedTermAnnotationError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      Right(hydra.error.core.NestedTermAnnotationError(field_location)))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def nestedTypeAnnotationError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.NestedTypeAnnotationError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.NestedTypeAnnotationError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.NestedTypeAnnotationError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      Right(hydra.error.core.NestedTypeAnnotationError(field_location)))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def nonComparableMapKeyTypeError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.core.NonComparableMapKeyTypeError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.NonComparableMapKeyTypeError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.NonComparableMapKeyTypeError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.error.core.NonComparableMapKeyTypeError](hydra.extract.helpers.requireField("keyType")(hydra.decode.core.`type`)(fieldMap)(cx))((field_keyType: hydra.core.Type) =>
      Right(hydra.error.core.NonComparableMapKeyTypeError(field_location, field_keyType))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def nonComparableSetElementTypeError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.core.NonComparableSetElementTypeError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.NonComparableSetElementTypeError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.NonComparableSetElementTypeError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.error.core.NonComparableSetElementTypeError](hydra.extract.helpers.requireField("elementType")(hydra.decode.core.`type`)(fieldMap)(cx))((field_elementType: hydra.core.Type) =>
      Right(hydra.error.core.NonComparableSetElementTypeError(field_location, field_elementType))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def redundantWrapUnwrapError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.RedundantWrapUnwrapError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.RedundantWrapUnwrapError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.RedundantWrapUnwrapError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.RedundantWrapUnwrapError](hydra.extract.helpers.requireField("typeName")(hydra.decode.core.name)(fieldMap)(cx))((field_typeName: hydra.core.Name) =>
      Right(hydra.error.core.RedundantWrapUnwrapError(field_location, field_typeName))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def selfApplicationError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.SelfApplicationError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.SelfApplicationError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.SelfApplicationError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.SelfApplicationError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.SelfApplicationError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def singleVariantUnionError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.SingleVariantUnionError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.SingleVariantUnionError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.SingleVariantUnionError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.SingleVariantUnionError](hydra.extract.helpers.requireField("fieldName")(hydra.decode.core.name)(fieldMap)(cx))((field_fieldName: hydra.core.Name) =>
      Right(hydra.error.core.SingleVariantUnionError(field_location, field_fieldName))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def termVariableShadowingError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.TermVariableShadowingError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.TermVariableShadowingError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.TermVariableShadowingError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.TermVariableShadowingError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.TermVariableShadowingError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def typeVariableShadowingInForallError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.core.TypeVariableShadowingInForallError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInForallError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.TypeVariableShadowingInForallError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.TypeVariableShadowingInForallError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.TypeVariableShadowingInForallError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def typeVariableShadowingInTypeLambdaError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.core.TypeVariableShadowingInTypeLambdaError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInTypeLambdaError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.TypeVariableShadowingInTypeLambdaError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.TypeVariableShadowingInTypeLambdaError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.TypeVariableShadowingInTypeLambdaError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def undefinedFieldError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.UndefinedFieldError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.UndefinedFieldError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.UndefinedFieldError](hydra.extract.helpers.requireField("fieldName")(hydra.decode.core.name)(fieldMap)(cx))((field_fieldName: hydra.core.Name) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.UndefinedFieldError](hydra.extract.helpers.requireField("typeName")(hydra.decode.core.name)(fieldMap)(cx))((field_typeName: hydra.core.Name) =>
      Right(hydra.error.core.UndefinedFieldError(field_fieldName, field_typeName))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def undefinedTermVariableError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.UndefinedTermVariableError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.UndefinedTermVariableError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.UndefinedTermVariableError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.UndefinedTermVariableError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.UndefinedTermVariableError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def undefinedTypeVariableError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.UndefinedTypeVariableError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.UndefinedTypeVariableError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.UndefinedTypeVariableError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def undefinedTypeVariableInBindingTypeError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.core.UndefinedTypeVariableInBindingTypeError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInBindingTypeError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.UndefinedTypeVariableInBindingTypeError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.UndefinedTypeVariableInBindingTypeError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.UndefinedTypeVariableInBindingTypeError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def undefinedTypeVariableInLambdaDomainError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.core.UndefinedTypeVariableInLambdaDomainError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInLambdaDomainError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.UndefinedTypeVariableInLambdaDomainError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.UndefinedTypeVariableInLambdaDomainError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.UndefinedTypeVariableInLambdaDomainError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def undefinedTypeVariableInTypeApplicationError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.core.UndefinedTypeVariableInTypeApplicationError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInTypeApplicationError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.UndefinedTypeVariableInTypeApplicationError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.UndefinedTypeVariableInTypeApplicationError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.UndefinedTypeVariableInTypeApplicationError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def unexpectedTermVariantError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.UnexpectedTermVariantError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.UnexpectedTermVariantError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.variants.TermVariant, hydra.error.core.UnexpectedTermVariantError](hydra.extract.helpers.requireField("expectedVariant")(hydra.decode.variants.termVariant)(fieldMap)(cx))((field_expectedVariant: hydra.variants.TermVariant) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.error.core.UnexpectedTermVariantError](hydra.extract.helpers.requireField("actualTerm")(hydra.decode.core.term)(fieldMap)(cx))((field_actualTerm: hydra.core.Term) =>
      Right(hydra.error.core.UnexpectedTermVariantError(field_expectedVariant, field_actualTerm))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def unexpectedTypeVariantError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.UnexpectedTypeVariantError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.UnexpectedTypeVariantError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.variants.TypeVariant, hydra.error.core.UnexpectedTypeVariantError](hydra.extract.helpers.requireField("expectedVariant")(hydra.decode.variants.typeVariant)(fieldMap)(cx))((field_expectedVariant: hydra.variants.TypeVariant) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.error.core.UnexpectedTypeVariantError](hydra.extract.helpers.requireField("actualType")(hydra.decode.core.`type`)(fieldMap)(cx))((field_actualType: hydra.core.Type) =>
      Right(hydra.error.core.UnexpectedTypeVariantError(field_expectedVariant, field_actualType))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def unknownPrimitiveNameError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.UnknownPrimitiveNameError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.UnknownPrimitiveNameError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.UnknownPrimitiveNameError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.UnknownPrimitiveNameError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.UnknownPrimitiveNameError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def unnecessaryIdentityApplicationError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.core.UnnecessaryIdentityApplicationError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.UnnecessaryIdentityApplicationError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.UnnecessaryIdentityApplicationError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      Right(hydra.error.core.UnnecessaryIdentityApplicationError(field_location)))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def untypedTermVariableError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.UntypedTermVariableError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.UntypedTermVariableError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.UntypedTermVariableError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.UntypedTermVariableError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.UntypedTermVariableError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def voidInNonBottomPositionError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.error.core.VoidInNonBottomPositionError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.VoidInNonBottomPositionError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.VoidInNonBottomPositionError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      Right(hydra.error.core.VoidInNonBottomPositionError(field_location)))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
