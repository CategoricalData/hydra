package hydra.encode.error.checking

import hydra.core.*

import hydra.error.checking.*

def checkingError(v1: hydra.error.checking.CheckingError): hydra.core.Term =
  v1 match
  case hydra.error.checking.CheckingError.incorrectUnification(v_CheckingError_incorrectUnification_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.error.checking.CheckingError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("incorrectUnification", hydra.encode.error.checking.incorrectUnificationError(v_CheckingError_incorrectUnification_y))))
  case hydra.error.checking.CheckingError.notAForallType(v_CheckingError_notAForallType_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.error.checking.CheckingError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("notAForallType", hydra.encode.error.checking.notAForallTypeError(v_CheckingError_notAForallType_y))))
  case hydra.error.checking.CheckingError.notAFunctionType(v_CheckingError_notAFunctionType_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.error.checking.CheckingError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("notAFunctionType", hydra.encode.error.checking.notAFunctionTypeError(v_CheckingError_notAFunctionType_y))))
  case hydra.error.checking.CheckingError.other(v_CheckingError_other_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.error.checking.CheckingError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("other", hydra.encode.error.checking.otherCheckingError(v_CheckingError_other_y))))
  case hydra.error.checking.CheckingError.typeArityMismatch(v_CheckingError_typeArityMismatch_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.error.checking.CheckingError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("typeArityMismatch", hydra.encode.error.checking.typeArityMismatchError(v_CheckingError_typeArityMismatch_y))))
  case hydra.error.checking.CheckingError.typeMismatch(v_CheckingError_typeMismatch_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.error.checking.CheckingError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("typeMismatch", hydra.encode.error.checking.typeMismatchError(v_CheckingError_typeMismatch_y))))
  case hydra.error.checking.CheckingError.unboundTypeVariables(v_CheckingError_unboundTypeVariables_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.error.checking.CheckingError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("unboundTypeVariables", hydra.encode.error.checking.unboundTypeVariablesError(v_CheckingError_unboundTypeVariables_y))))
  case hydra.error.checking.CheckingError.undefinedTermVariable(v_CheckingError_undefinedTermVariable_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.error.checking.CheckingError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("undefinedTermVariable", hydra.encode.error.checking.undefinedTermVariableCheckingError(v_CheckingError_undefinedTermVariable_y))))
  case hydra.error.checking.CheckingError.unequalTypes(v_CheckingError_unequalTypes_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.error.checking.CheckingError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("unequalTypes", hydra.encode.error.checking.unequalTypesError(v_CheckingError_unequalTypes_y))))
  case hydra.error.checking.CheckingError.unsupportedTermVariant(v_CheckingError_unsupportedTermVariant_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.error.checking.CheckingError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("unsupportedTermVariant", hydra.encode.error.checking.unsupportedTermVariantError(v_CheckingError_unsupportedTermVariant_y))))
  case hydra.error.checking.CheckingError.untypedLambda(v_CheckingError_untypedLambda_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.error.checking.CheckingError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("untypedLambda", hydra.encode.error.checking.untypedLambdaError(v_CheckingError_untypedLambda_y))))
  case hydra.error.checking.CheckingError.untypedLetBinding(v_CheckingError_untypedLetBinding_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.error.checking.CheckingError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("untypedLetBinding", hydra.encode.error.checking.untypedLetBindingError(v_CheckingError_untypedLetBinding_y))))
  case hydra.error.checking.CheckingError.untypedTermVariable(v_CheckingError_untypedTermVariable_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.error.checking.CheckingError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("untypedTermVariable", hydra.encode.error.checking.untypedTermVariableCheckingError(v_CheckingError_untypedTermVariable_y))))

def incorrectUnificationError(x: hydra.error.checking.IncorrectUnificationError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.checking.IncorrectUnificationError",
     Seq(hydra.core.Field("substitution", hydra.encode.typing.typeSubst(x.substitution)))))

def notAForallTypeError(x: hydra.error.checking.NotAForallTypeError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.checking.NotAForallTypeError",
     Seq(hydra.core.Field("type", hydra.encode.core.`type`(x.`type`)), hydra.core.Field("typeArguments",
     hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Type, hydra.core.Term](hydra.encode.core.`type`)(x.typeArguments))))))

def notAFunctionTypeError(x: hydra.error.checking.NotAFunctionTypeError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.checking.NotAFunctionTypeError",
     Seq(hydra.core.Field("type", hydra.encode.core.`type`(x.`type`)))))

def otherCheckingError(x: hydra.error.checking.OtherCheckingError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.checking.OtherCheckingError",
     Seq(hydra.core.Field("path", hydra.encode.paths.subtermPath(x.path)), hydra.core.Field("message",
     hydra.core.Term.literal(hydra.core.Literal.string(x.message))))))

def typeArityMismatchError(x: hydra.error.checking.TypeArityMismatchError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.checking.TypeArityMismatchError",
     Seq(hydra.core.Field("type", hydra.encode.core.`type`(x.`type`)), hydra.core.Field("expectedArity",
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(x.expectedArity)))),
     hydra.core.Field("actualArity", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(x.actualArity)))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Field("typeArguments", hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Type,
     hydra.core.Term](hydra.encode.core.`type`)(x.typeArguments))))))

def typeMismatchError(x: hydra.error.checking.TypeMismatchError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.checking.TypeMismatchError",
     Seq(hydra.core.Field("expectedType", hydra.encode.core.`type`(x.expectedType)),
     hydra.core.Field("actualType", hydra.encode.core.`type`(x.actualType)))))

def unboundTypeVariablesError(x: hydra.error.checking.UnboundTypeVariablesError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.checking.UnboundTypeVariablesError",
     Seq(hydra.core.Field("variables", hydra.core.Term.set(hydra.lib.sets.map[hydra.core.Name,
     hydra.core.Term](hydra.encode.core.name)(x.variables))), hydra.core.Field("type",
     hydra.encode.core.`type`(x.`type`)))))

def undefinedTermVariableCheckingError(x: hydra.error.checking.UndefinedTermVariableCheckingError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.checking.UndefinedTermVariableCheckingError",
     Seq(hydra.core.Field("path", hydra.encode.paths.subtermPath(x.path)), hydra.core.Field("name",
     hydra.encode.core.name(x.name)))))

def unequalTypesError(x: hydra.error.checking.UnequalTypesError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.checking.UnequalTypesError",
     Seq(hydra.core.Field("types", hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Type,
     hydra.core.Term](hydra.encode.core.`type`)(x.types))), hydra.core.Field("description",
     hydra.core.Term.literal(hydra.core.Literal.string(x.description))))))

def unsupportedTermVariantError(x: hydra.error.checking.UnsupportedTermVariantError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.checking.UnsupportedTermVariantError",
     Seq(hydra.core.Field("termVariant", hydra.encode.variants.termVariant(x.termVariant)))))

def untypedLambdaError[T0](x: T0): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.checking.UntypedLambdaError", Seq()))

def untypedLetBindingError(x: hydra.error.checking.UntypedLetBindingError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.checking.UntypedLetBindingError",
     Seq(hydra.core.Field("binding", hydra.encode.core.binding(x.binding)))))

def untypedTermVariableCheckingError(x: hydra.error.checking.UntypedTermVariableCheckingError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.checking.UntypedTermVariableCheckingError",
     Seq(hydra.core.Field("path", hydra.encode.paths.subtermPath(x.path)), hydra.core.Field("name",
     hydra.encode.core.name(x.name)))))
