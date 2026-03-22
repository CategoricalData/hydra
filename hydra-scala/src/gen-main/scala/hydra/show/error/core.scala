package hydra.show.error.core

import hydra.core.*

import hydra.error.core.*

import hydra.lib.strings

def duplicateBindingError(e: hydra.error.core.DuplicateBindingError): scala.Predef.String = hydra.lib.strings.cat(Seq("duplicate binding: ", (e.name)))

def duplicateFieldError(e: hydra.error.core.DuplicateFieldError): scala.Predef.String = hydra.lib.strings.cat(Seq("duplicate field: ", (e.name)))

def invalidTermError(e: hydra.error.core.InvalidTermError): scala.Predef.String =
  hydra.lib.strings.cat2("invalid term: ")(e match
  case hydra.error.core.InvalidTermError.duplicateBinding(v_InvalidTermError_duplicateBinding_v1) => hydra.show.error.core.duplicateBindingError(v_InvalidTermError_duplicateBinding_v1)
  case hydra.error.core.InvalidTermError.duplicateField(v_InvalidTermError_duplicateField_v1) => hydra.show.error.core.duplicateFieldError(v_InvalidTermError_duplicateField_v1))

def undefinedFieldError(e: hydra.error.core.UndefinedFieldError): scala.Predef.String =
  {
  val fname: hydra.core.Name = (e.fieldName)
  val tname: hydra.core.Name = (e.typeName)
  hydra.lib.strings.cat(Seq("no such field \"", fname, "\" in type \"", tname, "\""))
}

def undefinedTermError(e: hydra.error.core.UndefinedTermError): scala.Predef.String = hydra.lib.strings.cat2("undefined term: ")(e.name)

def undefinedTypeError(e: hydra.error.core.UndefinedTypeError): scala.Predef.String = hydra.lib.strings.cat2("undefined type: ")(e.name)

def unexpectedTermVariantError(e: hydra.error.core.UnexpectedTermVariantError): scala.Predef.String =
  {
  val expected: hydra.variants.TermVariant = (e.expectedVariant)
  val actual: hydra.core.Term = (e.actualTerm)
  hydra.lib.strings.cat(Seq("expected ", hydra.show.meta.termVariant(expected), " term but found ", hydra.show.core.term(actual)))
}

def unexpectedTypeVariantError(e: hydra.error.core.UnexpectedTypeVariantError): scala.Predef.String =
  {
  val expected: hydra.variants.TypeVariant = (e.expectedVariant)
  val actual: hydra.core.Type = (e.actualType)
  hydra.lib.strings.cat(Seq("expected ", hydra.show.meta.typeVariant(expected), " type but found ", hydra.show.core.`type`(actual)))
}
