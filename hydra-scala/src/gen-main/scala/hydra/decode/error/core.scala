package hydra.decode.error.core

import hydra.core.*

import hydra.error.core.*

import hydra.errors.*

import hydra.lib.eithers

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.strings

def duplicateBindingError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.DuplicateBindingError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.DuplicateBindingError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.DuplicateBindingError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.DuplicateBindingError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.DuplicateBindingError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def duplicateFieldError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.DuplicateFieldError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.DuplicateFieldError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.accessors.AccessorPath, hydra.error.core.DuplicateFieldError](hydra.extract.helpers.requireField("location")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_location: hydra.accessors.AccessorPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.DuplicateFieldError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      Right(hydra.error.core.DuplicateFieldError(field_location, field_name))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def invalidTermError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.InvalidTermError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.InvalidTermError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.error.core.InvalidTermError])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.error.core.InvalidTermError]](Seq(Tuple2("duplicateBinding",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.DuplicateBindingError, hydra.error.core.InvalidTermError,
         hydra.errors.DecodingError]((t: hydra.error.core.DuplicateBindingError) => hydra.error.core.InvalidTermError.duplicateBinding(t))(hydra.decode.error.core.duplicateBindingError(cx)(input))),
         Tuple2("duplicateField", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.error.core.DuplicateFieldError, hydra.error.core.InvalidTermError, hydra.errors.DecodingError]((t: hydra.error.core.DuplicateFieldError) => hydra.error.core.InvalidTermError.duplicateField(t))(hydra.decode.error.core.duplicateFieldError(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.error.core.InvalidTermError], (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.error.core.InvalidTermError]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.error.core.InvalidTermError])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.error.core.InvalidTermError]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def undefinedFieldError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.UndefinedFieldError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.UndefinedFieldError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.UndefinedFieldError](hydra.extract.helpers.requireField("fieldName")(hydra.decode.core.name)(fieldMap)(cx))((field_fieldName: hydra.core.Name) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.UndefinedFieldError](hydra.extract.helpers.requireField("typeName")(hydra.decode.core.name)(fieldMap)(cx))((field_typeName: hydra.core.Name) =>
      Right(hydra.error.core.UndefinedFieldError(field_fieldName, field_typeName))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def undefinedTermError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.UndefinedTermError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.UndefinedTermError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.UndefinedTermError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) => Right(hydra.error.core.UndefinedTermError(field_name)))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def undefinedTypeError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.UndefinedTypeError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.UndefinedTypeError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.error.core.UndefinedTypeError](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) => Right(hydra.error.core.UndefinedTypeError(field_name)))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def unexpectedTermVariantError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.UnexpectedTermVariantError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.UnexpectedTermVariantError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.variants.TermVariant, hydra.error.core.UnexpectedTermVariantError](hydra.extract.helpers.requireField("expectedVariant")(hydra.decode.variants.termVariant)(fieldMap)(cx))((field_expectedVariant: hydra.variants.TermVariant) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.error.core.UnexpectedTermVariantError](hydra.extract.helpers.requireField("actualTerm")(hydra.decode.core.term)(fieldMap)(cx))((field_actualTerm: hydra.core.Term) =>
      Right(hydra.error.core.UnexpectedTermVariantError(field_expectedVariant, field_actualTerm))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def unexpectedTypeVariantError(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.error.core.UnexpectedTypeVariantError] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.error.core.UnexpectedTypeVariantError]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.variants.TypeVariant, hydra.error.core.UnexpectedTypeVariantError](hydra.extract.helpers.requireField("expectedVariant")(hydra.decode.variants.typeVariant)(fieldMap)(cx))((field_expectedVariant: hydra.variants.TypeVariant) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.error.core.UnexpectedTypeVariantError](hydra.extract.helpers.requireField("actualType")(hydra.decode.core.`type`)(fieldMap)(cx))((field_actualType: hydra.core.Type) =>
      Right(hydra.error.core.UnexpectedTypeVariantError(field_expectedVariant, field_actualType))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
