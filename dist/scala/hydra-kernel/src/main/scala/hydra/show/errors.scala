package hydra.show.errors

import hydra.core.*

import hydra.error.checking.*

import hydra.errors.*

def checkingError(ce: hydra.error.checking.CheckingError): scala.Predef.String =
  ce match
  case hydra.error.checking.CheckingError.incorrectUnification(v_CheckingError_incorrectUnification_v1) => hydra.show.errors.incorrectUnificationError(v_CheckingError_incorrectUnification_v1)
  case hydra.error.checking.CheckingError.notAForallType(v_CheckingError_notAForallType_v1) => hydra.show.errors.notAForallTypeError(v_CheckingError_notAForallType_v1)
  case hydra.error.checking.CheckingError.notAFunctionType(v_CheckingError_notAFunctionType_v1) => hydra.show.errors.notAFunctionTypeError(v_CheckingError_notAFunctionType_v1)
  case hydra.error.checking.CheckingError.typeArityMismatch(v_CheckingError_typeArityMismatch_v1) => hydra.show.errors.typeArityMismatchError(v_CheckingError_typeArityMismatch_v1)
  case hydra.error.checking.CheckingError.typeMismatch(v_CheckingError_typeMismatch_v1) => hydra.show.errors.typeMismatchError(v_CheckingError_typeMismatch_v1)
  case hydra.error.checking.CheckingError.unboundTypeVariables(v_CheckingError_unboundTypeVariables_v1) => hydra.show.errors.unboundTypeVariablesError(v_CheckingError_unboundTypeVariables_v1)
  case hydra.error.checking.CheckingError.unequalTypes(v_CheckingError_unequalTypes_v1) => hydra.show.errors.unequalTypesError(v_CheckingError_unequalTypes_v1)
  case hydra.error.checking.CheckingError.unsupportedTermVariant(v_CheckingError_unsupportedTermVariant_v1) => hydra.show.errors.unsupportedTermVariantError(v_CheckingError_unsupportedTermVariant_v1)
  case hydra.error.checking.CheckingError.untypedLambda(v_CheckingError_untypedLambda_v1) => hydra.show.errors.untypedLambdaError(v_CheckingError_untypedLambda_v1)
  case hydra.error.checking.CheckingError.untypedLetBinding(v_CheckingError_untypedLetBinding_v1) => hydra.show.errors.untypedLetBindingError(v_CheckingError_untypedLetBinding_v1)

def decodingError(de: hydra.errors.DecodingError): scala.Predef.String = hydra.lib.strings.cat2("decoding error: ")(de)

def error(e: hydra.errors.Error): scala.Predef.String =
  e match
  case hydra.errors.Error.checking(v_Error_checking_v1) => hydra.show.errors.checkingError(v_Error_checking_v1)
  case hydra.errors.Error.decoding(v_Error_decoding_v1) => hydra.show.errors.decodingError(v_Error_decoding_v1)
  case hydra.errors.Error.duplicateBinding(v_Error_duplicateBinding_v1) => hydra.show.error.core.duplicateBindingError(v_Error_duplicateBinding_v1)
  case hydra.errors.Error.duplicateField(v_Error_duplicateField_v1) => hydra.show.error.core.duplicateFieldError(v_Error_duplicateField_v1)
  case hydra.errors.Error.extraction(v_Error_extraction__) => "extraction error"
  case hydra.errors.Error.inference(v_Error_inference__) => "inference error"
  case hydra.errors.Error.other(v_Error_other_v1) => hydra.show.errors.otherError(v_Error_other_v1)
  case hydra.errors.Error.resolution(v_Error_resolution__) => "resolution error"
  case hydra.errors.Error.undefinedField(v_Error_undefinedField_v1) => hydra.show.error.core.undefinedFieldError(v_Error_undefinedField_v1)
  case hydra.errors.Error.undefinedTermVariable(v_Error_undefinedTermVariable_v1) => hydra.show.error.core.undefinedTermVariableError(v_Error_undefinedTermVariable_v1)
  case hydra.errors.Error.untypedTermVariable(v_Error_untypedTermVariable_v1) => hydra.show.error.core.untypedTermVariableError(v_Error_untypedTermVariable_v1)
  case hydra.errors.Error.unexpectedTermVariant(v_Error_unexpectedTermVariant_v1) => hydra.show.error.core.unexpectedTermVariantError(v_Error_unexpectedTermVariant_v1)
  case hydra.errors.Error.unexpectedTypeVariant(v_Error_unexpectedTypeVariant_v1) => hydra.show.error.core.unexpectedTypeVariantError(v_Error_unexpectedTypeVariant_v1)
  case hydra.errors.Error.unification(v_Error_unification_v1) => hydra.show.errors.unificationError(v_Error_unification_v1)

def incorrectUnificationError(e: hydra.error.checking.IncorrectUnificationError): scala.Predef.String =
  {
  lazy val subst: hydra.typing.TypeSubst = (e.substitution)
  hydra.lib.strings.cat2("incorrect unification: ")(hydra.show.typing.typeSubst(subst))
}

def notAForallTypeError(e: hydra.error.checking.NotAForallTypeError): scala.Predef.String =
  {
  lazy val typ: hydra.core.Type = (e.`type`)
  lazy val args: Seq[hydra.core.Type] = (e.typeArguments)
  hydra.lib.strings.cat(Seq("not a forall type: ", hydra.show.core.`type`(typ), ". Trying to apply ",
     hydra.lib.literals.showInt32(hydra.lib.lists.length[hydra.core.Type](args)),
     " type argument(s): ", hydra.formatting.showList(hydra.show.core.`type`)(args)))
}

def notAFunctionTypeError(e: hydra.error.checking.NotAFunctionTypeError): scala.Predef.String =
  {
  lazy val typ: hydra.core.Type = (e.`type`)
  hydra.lib.strings.cat2("not a function type: ")(hydra.show.core.`type`(typ))
}

def otherError(oe: hydra.errors.OtherError): scala.Predef.String = oe

def typeArityMismatchError(e: hydra.error.checking.TypeArityMismatchError): scala.Predef.String =
  {
  lazy val typ: hydra.core.Type = (e.`type`)
  lazy val expected: Int = (e.expectedArity)
  lazy val actual: Int = (e.actualArity)
  lazy val args: Seq[hydra.core.Type] = (e.typeArguments)
  hydra.lib.strings.cat(Seq("type ", hydra.show.core.`type`(typ), " applied to the wrong number of type arguments (expected ",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.lib.literals.showInt32(expected), ", got ", hydra.lib.literals.showInt32(actual),
     "): ", hydra.formatting.showList(hydra.show.core.`type`)(args)))
}

def typeMismatchError(e: hydra.error.checking.TypeMismatchError): scala.Predef.String =
  {
  lazy val expected: hydra.core.Type = (e.expectedType)
  lazy val actual: hydra.core.Type = (e.actualType)
  hydra.lib.strings.cat(Seq("type mismatch: expected ", hydra.show.core.`type`(expected),
     " but found ", hydra.show.core.`type`(actual)))
}

def unboundTypeVariablesError(e: hydra.error.checking.UnboundTypeVariablesError): scala.Predef.String =
  {
  lazy val vars: scala.collection.immutable.Set[hydra.core.Name] = (e.variables)
  lazy val typ: hydra.core.Type = (e.`type`)
  hydra.lib.strings.cat(Seq("unbound type variables: {", hydra.lib.strings.intercalate(", ")(hydra.lib.lists.map[hydra.core.Name,
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     scala.Predef.String]((x) => x)(hydra.lib.sets.toList[hydra.core.Name](vars))),
     "} in type ", hydra.show.core.`type`(typ)))
}

def unequalTypesError(e: hydra.error.checking.UnequalTypesError): scala.Predef.String =
  {
  lazy val types: Seq[hydra.core.Type] = (e.types)
  lazy val desc: scala.Predef.String = (e.description)
  hydra.lib.strings.cat(Seq("unequal types ", hydra.formatting.showList(hydra.show.core.`type`)(types), " in ", desc))
}

def unificationError(e: hydra.errors.UnificationError): scala.Predef.String =
  {
  lazy val lt: hydra.core.Type = (e.leftType)
  lazy val rt: hydra.core.Type = (e.rightType)
  lazy val msg: scala.Predef.String = (e.message)
  hydra.lib.strings.cat(Seq("unification error: cannot unify ", hydra.show.core.`type`(lt),
     " with ", hydra.show.core.`type`(rt), ": ", msg))
}

def unsupportedTermVariantError(e: hydra.error.checking.UnsupportedTermVariantError): scala.Predef.String =
  hydra.lib.strings.cat2("unsupported term variant: ")(hydra.show.variants.termVariant(e.termVariant))

def untypedLambdaError[T0](_x: T0): scala.Predef.String = "untyped lambda"

def untypedLetBindingError(e: hydra.error.checking.UntypedLetBindingError): scala.Predef.String =
  {
  lazy val b: hydra.core.Binding = (e.binding)
  hydra.lib.strings.cat2("untyped let binding: ")(hydra.show.core.binding(b))
}
