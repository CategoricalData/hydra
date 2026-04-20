package hydra.encode.errors

import hydra.core.*

import hydra.errors.*

def decodingError(x: hydra.errors.DecodingError): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.errors.DecodingError", hydra.core.Term.literal(hydra.core.Literal.string(x))))

def emptyListError[T0](_x: T0): hydra.core.Term = hydra.core.Term.unit

def error(v1: hydra.errors.Error): hydra.core.Term =
  v1 match
  case hydra.errors.Error.checking(v_Error_checking_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.Error",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("checking", hydra.encode.error.checking.checkingError(v_Error_checking_y))))
  case hydra.errors.Error.decoding(v_Error_decoding_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.Error",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("decoding", hydra.encode.errors.decodingError(v_Error_decoding_y))))
  case hydra.errors.Error.duplicateBinding(v_Error_duplicateBinding_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.Error",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("duplicateBinding", hydra.encode.error.core.duplicateBindingError(v_Error_duplicateBinding_y))))
  case hydra.errors.Error.duplicateField(v_Error_duplicateField_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.Error",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("duplicateField", hydra.encode.error.core.duplicateFieldError(v_Error_duplicateField_y))))
  case hydra.errors.Error.extraction(v_Error_extraction_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.Error",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("extraction", hydra.encode.errors.extractionError(v_Error_extraction_y))))
  case hydra.errors.Error.inference(v_Error_inference_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.Error",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("inference", hydra.encode.errors.inferenceError(v_Error_inference_y))))
  case hydra.errors.Error.other(v_Error_other_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.Error",
     hydra.core.Field("other", hydra.encode.errors.otherError(v_Error_other_y))))
  case hydra.errors.Error.resolution(v_Error_resolution_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.Error",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("resolution", hydra.encode.errors.resolutionError(v_Error_resolution_y))))
  case hydra.errors.Error.undefinedField(v_Error_undefinedField_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.Error",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("undefinedField", hydra.encode.error.core.undefinedFieldError(v_Error_undefinedField_y))))
  case hydra.errors.Error.undefinedTermVariable(v_Error_undefinedTermVariable_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.Error",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("undefinedTermVariable", hydra.encode.error.core.undefinedTermVariableError(v_Error_undefinedTermVariable_y))))
  case hydra.errors.Error.untypedTermVariable(v_Error_untypedTermVariable_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.Error",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("untypedTermVariable", hydra.encode.error.core.untypedTermVariableError(v_Error_untypedTermVariable_y))))
  case hydra.errors.Error.unexpectedTermVariant(v_Error_unexpectedTermVariant_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.Error",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("unexpectedTermVariant", hydra.encode.error.core.unexpectedTermVariantError(v_Error_unexpectedTermVariant_y))))
  case hydra.errors.Error.unexpectedTypeVariant(v_Error_unexpectedTypeVariant_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.Error",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("unexpectedTypeVariant", hydra.encode.error.core.unexpectedTypeVariantError(v_Error_unexpectedTypeVariant_y))))
  case hydra.errors.Error.unification(v_Error_unification_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.Error",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("unification", hydra.encode.errors.unificationError(v_Error_unification_y))))

def extractionError(v1: hydra.errors.ExtractionError): hydra.core.Term =
  v1 match
  case hydra.errors.ExtractionError.emptyList(v_ExtractionError_emptyList_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.ExtractionError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("emptyList", hydra.encode.errors.emptyListError(v_ExtractionError_emptyList_y))))
  case hydra.errors.ExtractionError.multipleBindings(v_ExtractionError_multipleBindings_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.ExtractionError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("multipleBindings", hydra.encode.errors.multipleBindingsError(v_ExtractionError_multipleBindings_y))))
  case hydra.errors.ExtractionError.multipleFields(v_ExtractionError_multipleFields_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.ExtractionError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("multipleFields", hydra.encode.errors.multipleFieldsError(v_ExtractionError_multipleFields_y))))
  case hydra.errors.ExtractionError.noMatchingField(v_ExtractionError_noMatchingField_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.ExtractionError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("noMatchingField", hydra.encode.errors.noMatchingFieldError(v_ExtractionError_noMatchingField_y))))
  case hydra.errors.ExtractionError.noSuchBinding(v_ExtractionError_noSuchBinding_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.ExtractionError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("noSuchBinding", hydra.encode.errors.noSuchBindingError(v_ExtractionError_noSuchBinding_y))))
  case hydra.errors.ExtractionError.notEnoughCases(v_ExtractionError_notEnoughCases_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.ExtractionError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("notEnoughCases", hydra.encode.errors.notEnoughCasesError(v_ExtractionError_notEnoughCases_y))))
  case hydra.errors.ExtractionError.unexpectedShape(v_ExtractionError_unexpectedShape_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.ExtractionError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("unexpectedShape", hydra.encode.errors.unexpectedShapeError(v_ExtractionError_unexpectedShape_y))))

def inferenceError(v1: hydra.errors.InferenceError): hydra.core.Term =
  v1 match
  case hydra.errors.InferenceError.checking(v_InferenceError_checking_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.InferenceError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("checking", hydra.encode.error.checking.checkingError(v_InferenceError_checking_y))))
  case hydra.errors.InferenceError.other(v_InferenceError_other_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.InferenceError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("other", hydra.encode.errors.otherInferenceError(v_InferenceError_other_y))))
  case hydra.errors.InferenceError.unification(v_InferenceError_unification_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.InferenceError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("unification", hydra.encode.errors.unificationInferenceError(v_InferenceError_unification_y))))

def multipleBindingsError(x: hydra.errors.MultipleBindingsError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.errors.MultipleBindingsError", Seq(hydra.core.Field("name",
     hydra.encode.core.name(x.name)))))

def multipleFieldsError(x: hydra.errors.MultipleFieldsError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.errors.MultipleFieldsError", Seq(hydra.core.Field("fieldName",
     hydra.encode.core.name(x.fieldName)))))

def noMatchingFieldError(x: hydra.errors.NoMatchingFieldError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.errors.NoMatchingFieldError", Seq(hydra.core.Field("fieldName",
     hydra.encode.core.name(x.fieldName)))))

def noSuchBindingError(x: hydra.errors.NoSuchBindingError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.errors.NoSuchBindingError", Seq(hydra.core.Field("name",
     hydra.encode.core.name(x.name)))))

def noSuchPrimitiveError(x: hydra.errors.NoSuchPrimitiveError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.errors.NoSuchPrimitiveError", Seq(hydra.core.Field("name",
     hydra.encode.core.name(x.name)))))

def notEnoughCasesError[T0](_x: T0): hydra.core.Term = hydra.core.Term.unit

def otherError(x: hydra.errors.OtherError): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.errors.OtherError", hydra.core.Term.literal(hydra.core.Literal.string(x))))

def otherInferenceError(x: hydra.errors.OtherInferenceError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.errors.OtherInferenceError", Seq(hydra.core.Field("path",
     hydra.encode.paths.subtermPath(x.path)), hydra.core.Field("message", hydra.core.Term.literal(hydra.core.Literal.string(x.message))))))

def otherResolutionError(x: hydra.errors.OtherResolutionError): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.errors.OtherResolutionError",
     hydra.core.Term.literal(hydra.core.Literal.string(x))))

def resolutionError(v1: hydra.errors.ResolutionError): hydra.core.Term =
  v1 match
  case hydra.errors.ResolutionError.noSuchBinding(v_ResolutionError_noSuchBinding_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.ResolutionError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("noSuchBinding", hydra.encode.errors.noSuchBindingError(v_ResolutionError_noSuchBinding_y))))
  case hydra.errors.ResolutionError.noSuchPrimitive(v_ResolutionError_noSuchPrimitive_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.ResolutionError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("noSuchPrimitive", hydra.encode.errors.noSuchPrimitiveError(v_ResolutionError_noSuchPrimitive_y))))
  case hydra.errors.ResolutionError.noMatchingField(v_ResolutionError_noMatchingField_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.ResolutionError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("noMatchingField", hydra.encode.errors.noMatchingFieldError(v_ResolutionError_noMatchingField_y))))
  case hydra.errors.ResolutionError.other(v_ResolutionError_other_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.ResolutionError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("other", hydra.encode.errors.otherResolutionError(v_ResolutionError_other_y))))
  case hydra.errors.ResolutionError.unexpectedShape(v_ResolutionError_unexpectedShape_y) => hydra.core.Term.inject(hydra.core.Injection("hydra.errors.ResolutionError",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Field("unexpectedShape", hydra.encode.errors.unexpectedShapeError(v_ResolutionError_unexpectedShape_y))))

def unexpectedShapeError(x: hydra.errors.UnexpectedShapeError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.errors.UnexpectedShapeError", Seq(hydra.core.Field("expected",
     hydra.core.Term.literal(hydra.core.Literal.string(x.expected))), hydra.core.Field("actual",
     hydra.core.Term.literal(hydra.core.Literal.string(x.actual))))))

def unificationError(x: hydra.errors.UnificationError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.errors.UnificationError", Seq(hydra.core.Field("leftType",
     hydra.encode.core.`type`(x.leftType)), hydra.core.Field("rightType", hydra.encode.core.`type`(x.rightType)),
     hydra.core.Field("message", hydra.core.Term.literal(hydra.core.Literal.string(x.message))))))

def unificationInferenceError(x: hydra.errors.UnificationInferenceError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.errors.UnificationInferenceError",
     Seq(hydra.core.Field("path", hydra.encode.paths.subtermPath(x.path)), hydra.core.Field("cause",
     hydra.encode.errors.unificationError(x.cause)))))
