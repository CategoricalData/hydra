package hydra.show.error

import hydra.core.*

import hydra.error.*

import hydra.lib.lists

import hydra.lib.literals

import hydra.lib.sets

import hydra.lib.strings

def checkingError(ce: hydra.error.CheckingError): scala.Predef.String =
  ce match
  case hydra.error.CheckingError.incorrectUnification(v_CheckingError_incorrectUnification_v1) => hydra.show.error.incorrectUnificationError(v_CheckingError_incorrectUnification_v1)
  case hydra.error.CheckingError.notAForallType(v_CheckingError_notAForallType_v1) => hydra.show.error.notAForallTypeError(v_CheckingError_notAForallType_v1)
  case hydra.error.CheckingError.notAFunctionType(v_CheckingError_notAFunctionType_v1) => hydra.show.error.notAFunctionTypeError(v_CheckingError_notAFunctionType_v1)
  case hydra.error.CheckingError.typeArityMismatch(v_CheckingError_typeArityMismatch_v1) => hydra.show.error.typeArityMismatchError(v_CheckingError_typeArityMismatch_v1)
  case hydra.error.CheckingError.typeMismatch(v_CheckingError_typeMismatch_v1) => hydra.show.error.typeMismatchError(v_CheckingError_typeMismatch_v1)
  case hydra.error.CheckingError.unboundTypeVariables(v_CheckingError_unboundTypeVariables_v1) => hydra.show.error.unboundTypeVariablesError(v_CheckingError_unboundTypeVariables_v1)
  case hydra.error.CheckingError.unequalTypes(v_CheckingError_unequalTypes_v1) => hydra.show.error.unequalTypesError(v_CheckingError_unequalTypes_v1)
  case hydra.error.CheckingError.unsupportedTermVariant(v_CheckingError_unsupportedTermVariant_v1) => hydra.show.error.unsupportedTermVariantError(v_CheckingError_unsupportedTermVariant_v1)
  case hydra.error.CheckingError.untypedLambda(v_CheckingError_untypedLambda_v1) => hydra.show.error.untypedLambdaError(v_CheckingError_untypedLambda_v1)
  case hydra.error.CheckingError.untypedLetBinding(v_CheckingError_untypedLetBinding_v1) => hydra.show.error.untypedLetBindingError(v_CheckingError_untypedLetBinding_v1)

def decodingError(de: hydra.error.DecodingError): scala.Predef.String = strings.cat2("decoding error: ")(de)

def duplicateBindingError(e: hydra.error.DuplicateBindingError): scala.Predef.String = strings.cat2("duplicate binding: ")(e.name)

def duplicateFieldError(e: hydra.error.DuplicateFieldError): scala.Predef.String = strings.cat2("duplicate field: ")(e.name)

def error(e: hydra.error.Error): scala.Predef.String =
  e match
  case hydra.error.Error.checking(v_Error_checking_v1) => hydra.show.error.checkingError(v_Error_checking_v1)
  case hydra.error.Error.decoding(v_Error_decoding_v1) => hydra.show.error.decodingError(v_Error_decoding_v1)
  case hydra.error.Error.duplicateBinding(v_Error_duplicateBinding_v1) => hydra.show.error.duplicateBindingError(v_Error_duplicateBinding_v1)
  case hydra.error.Error.duplicateField(v_Error_duplicateField_v1) => hydra.show.error.duplicateFieldError(v_Error_duplicateField_v1)
  case hydra.error.Error.other(v_Error_other_v1) => hydra.show.error.otherError(v_Error_other_v1)
  case hydra.error.Error.undefinedField(v_Error_undefinedField_v1) => hydra.show.error.undefinedFieldError(v_Error_undefinedField_v1)
  case hydra.error.Error.undefinedTerm(v_Error_undefinedTerm_v1) => hydra.show.error.undefinedTermError(v_Error_undefinedTerm_v1)
  case hydra.error.Error.undefinedType(v_Error_undefinedType_v1) => hydra.show.error.undefinedTypeError(v_Error_undefinedType_v1)
  case hydra.error.Error.unexpectedTermVariant(v_Error_unexpectedTermVariant_v1) => hydra.show.error.unexpectedTermVariantError(v_Error_unexpectedTermVariant_v1)
  case hydra.error.Error.unexpectedTypeVariant(v_Error_unexpectedTypeVariant_v1) => hydra.show.error.unexpectedTypeVariantError(v_Error_unexpectedTypeVariant_v1)
  case hydra.error.Error.unification(v_Error_unification_v1) => hydra.show.error.unificationError(v_Error_unification_v1)

def incorrectUnificationError(e: hydra.error.IncorrectUnificationError): scala.Predef.String =
  {
  val subst: hydra.typing.TypeSubst = (e.substitution)
  strings.cat2("incorrect unification: ")(hydra.show.typing.typeSubst(subst))
}

def notAForallTypeError(e: hydra.error.NotAForallTypeError): scala.Predef.String =
  {
  val typ: hydra.core.Type = (e.`type`)
  val args: Seq[hydra.core.Type] = (e.typeArguments)
  strings.cat(Seq("not a forall type: ", hydra.show.core.`type`(typ), ". Trying to apply ", literals.showInt32(lists.length[hydra.core.Type](args)),
     " type argument(s): ", hydra.formatting.showList(hydra.show.core.`type`)(args)))
}

def notAFunctionTypeError(e: hydra.error.NotAFunctionTypeError): scala.Predef.String =
  {
  val typ: hydra.core.Type = (e.`type`)
  strings.cat2("not a function type: ")(hydra.show.core.`type`(typ))
}

def otherError(oe: hydra.error.OtherError): scala.Predef.String = oe

def typeArityMismatchError(e: hydra.error.TypeArityMismatchError): scala.Predef.String =
  {
  val typ: hydra.core.Type = (e.`type`)
  val expected: Int = (e.expectedArity)
  val actual: Int = (e.actualArity)
  val args: Seq[hydra.core.Type] = (e.typeArguments)
  strings.cat(Seq("type ", hydra.show.core.`type`(typ), " applied to the wrong number of type arguments (expected ",
     literals.showInt32(expected), ", got ", literals.showInt32(actual), "): ", hydra.formatting.showList(hydra.show.core.`type`)(args)))
}

def typeMismatchError(e: hydra.error.TypeMismatchError): scala.Predef.String =
  {
  val expected: hydra.core.Type = (e.expectedType)
  val actual: hydra.core.Type = (e.actualType)
  strings.cat(Seq("type mismatch: expected ", hydra.show.core.`type`(expected), " but found ", hydra.show.core.`type`(actual)))
}

def unboundTypeVariablesError(e: hydra.error.UnboundTypeVariablesError): scala.Predef.String =
  {
  val vars: scala.collection.immutable.Set[hydra.core.Name] = (e.variables)
  val typ: hydra.core.Type = (e.`type`)
  strings.cat(Seq("unbound type variables: {", strings.intercalate(", ")(lists.map[hydra.core.Name, scala.Predef.String]((x) => x)(sets.toList[hydra.core.Name](vars))),
     "} in type ", hydra.show.core.`type`(typ)))
}

def undefinedFieldError(e: hydra.error.UndefinedFieldError): scala.Predef.String =
  {
  val fname: hydra.core.Name = (e.fieldName)
  val tname: hydra.core.Name = (e.typeName)
  strings.cat(Seq("no such field \"", fname, "\" in type \"", tname, "\""))
}

def undefinedTermError(e: hydra.error.UndefinedTermError): scala.Predef.String = strings.cat2("undefined term: ")(e.name)

def undefinedTypeError(e: hydra.error.UndefinedTypeError): scala.Predef.String = strings.cat2("undefined type: ")(e.name)

def unequalTypesError(e: hydra.error.UnequalTypesError): scala.Predef.String =
  {
  val types: Seq[hydra.core.Type] = (e.types)
  val desc: scala.Predef.String = (e.description)
  strings.cat(Seq("unequal types ", hydra.formatting.showList(hydra.show.core.`type`)(types), " in ", desc))
}

def unexpectedTermVariantError(e: hydra.error.UnexpectedTermVariantError): scala.Predef.String =
  {
  val expected: hydra.variants.TermVariant = (e.expectedVariant)
  val actual: hydra.core.Term = (e.actualTerm)
  strings.cat(Seq("expected ", hydra.show.meta.termVariant(expected), " term but found ", hydra.show.core.term(actual)))
}

def unexpectedTypeVariantError(e: hydra.error.UnexpectedTypeVariantError): scala.Predef.String =
  {
  val expected: hydra.variants.TypeVariant = (e.expectedVariant)
  val actual: hydra.core.Type = (e.actualType)
  strings.cat(Seq("expected ", hydra.show.meta.typeVariant(expected), " type but found ", hydra.show.core.`type`(actual)))
}

def unificationError(e: hydra.error.UnificationError): scala.Predef.String =
  {
  val lt: hydra.core.Type = (e.leftType)
  val rt: hydra.core.Type = (e.rightType)
  val msg: scala.Predef.String = (e.message)
  strings.cat(Seq("unification error: cannot unify ", hydra.show.core.`type`(lt), " with ", hydra.show.core.`type`(rt), ": ", msg))
}

def unsupportedTermVariantError(e: hydra.error.UnsupportedTermVariantError): scala.Predef.String =
  strings.cat2("unsupported term variant: ")(hydra.show.meta.termVariant(e.termVariant))

def untypedLambdaError[T0](_x: T0): scala.Predef.String = "untyped lambda"

def untypedLetBindingError(e: hydra.error.UntypedLetBindingError): scala.Predef.String =
  {
  val b: hydra.core.Binding = (e.binding)
  strings.cat2("untyped let binding: ")(hydra.show.core.binding(b))
}
