package hydra.encode.error.core

import hydra.core.*

import hydra.error.core.*

def duplicateBindingError(x: hydra.error.core.DuplicateBindingError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.DuplicateBindingError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name", hydra.encode.core.name(x.name)))))

def duplicateFieldError(x: hydra.error.core.DuplicateFieldError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.DuplicateFieldError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name", hydra.encode.core.name(x.name)))))

def invalidTermError(v1: hydra.error.core.InvalidTermError): hydra.core.Term =
  v1 match
  case hydra.error.core.InvalidTermError.duplicateBinding(v_InvalidTermError_duplicateBinding_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("duplicateBinding", hydra.encode.error.core.duplicateBindingError(v_InvalidTermError_duplicateBinding_y))))
  case hydra.error.core.InvalidTermError.duplicateField(v_InvalidTermError_duplicateField_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("duplicateField", hydra.encode.error.core.duplicateFieldError(v_InvalidTermError_duplicateField_y))))

def undefinedFieldError(x: hydra.error.core.UndefinedFieldError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.UndefinedFieldError", Seq(hydra.core.Field("fieldName",
     hydra.encode.core.name(x.fieldName)), hydra.core.Field("typeName", hydra.encode.core.name(x.typeName)))))

def undefinedTermError(x: hydra.error.core.UndefinedTermError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.UndefinedTermError", Seq(hydra.core.Field("name", hydra.encode.core.name(x.name)))))

def undefinedTypeError(x: hydra.error.core.UndefinedTypeError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.UndefinedTypeError", Seq(hydra.core.Field("name", hydra.encode.core.name(x.name)))))

def unexpectedTermVariantError(x: hydra.error.core.UnexpectedTermVariantError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.UnexpectedTermVariantError", Seq(hydra.core.Field("expectedVariant",
     hydra.encode.variants.termVariant(x.expectedVariant)), hydra.core.Field("actualTerm", hydra.encode.core.term(x.actualTerm)))))

def unexpectedTypeVariantError(x: hydra.error.core.UnexpectedTypeVariantError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.UnexpectedTypeVariantError", Seq(hydra.core.Field("expectedVariant",
     hydra.encode.variants.typeVariant(x.expectedVariant)), hydra.core.Field("actualType", hydra.encode.core.`type`(x.actualType)))))
