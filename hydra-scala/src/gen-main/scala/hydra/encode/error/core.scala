package hydra.encode.error.core

import hydra.core.*

import hydra.error.core.*

def constantConditionError(x: hydra.error.core.ConstantConditionError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.ConstantConditionError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("value", hydra.core.Term.literal(hydra.core.Literal.boolean(x.value))))))

def duplicateBindingError(x: hydra.error.core.DuplicateBindingError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.DuplicateBindingError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name", hydra.encode.core.name(x.name)))))

def duplicateFieldError(x: hydra.error.core.DuplicateFieldError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.DuplicateFieldError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name", hydra.encode.core.name(x.name)))))

def duplicateRecordTypeFieldNamesError(x: hydra.error.core.DuplicateRecordTypeFieldNamesError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.DuplicateRecordTypeFieldNamesError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name", hydra.encode.core.name(x.name)))))

def duplicateUnionTypeFieldNamesError(x: hydra.error.core.DuplicateUnionTypeFieldNamesError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.DuplicateUnionTypeFieldNamesError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name", hydra.encode.core.name(x.name)))))

def emptyCaseStatementError(x: hydra.error.core.EmptyCaseStatementError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.EmptyCaseStatementError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("typeName", hydra.encode.core.name(x.typeName)))))

def emptyLetBindingsError(x: hydra.error.core.EmptyLetBindingsError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.EmptyLetBindingsError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)))))

def emptyRecordTypeError(x: hydra.error.core.EmptyRecordTypeError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.EmptyRecordTypeError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)))))

def emptyTermAnnotationError(x: hydra.error.core.EmptyTermAnnotationError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.EmptyTermAnnotationError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)))))

def emptyTypeAnnotationError(x: hydra.error.core.EmptyTypeAnnotationError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.EmptyTypeAnnotationError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)))))

def emptyTypeNameInTermError(x: hydra.error.core.EmptyTypeNameInTermError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.EmptyTypeNameInTermError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)))))

def emptyUnionTypeError(x: hydra.error.core.EmptyUnionTypeError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.EmptyUnionTypeError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)))))

def invalidForallParameterNameError(x: hydra.error.core.InvalidForallParameterNameError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.InvalidForallParameterNameError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name", hydra.encode.core.name(x.name)))))

def invalidLambdaParameterNameError(x: hydra.error.core.InvalidLambdaParameterNameError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.InvalidLambdaParameterNameError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name", hydra.encode.core.name(x.name)))))

def invalidLetBindingNameError(x: hydra.error.core.InvalidLetBindingNameError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.InvalidLetBindingNameError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name", hydra.encode.core.name(x.name)))))

def invalidTermError(v1: hydra.error.core.InvalidTermError): hydra.core.Term =
  v1 match
  case hydra.error.core.InvalidTermError.constantCondition(v_InvalidTermError_constantCondition_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("constantCondition", hydra.encode.error.core.constantConditionError(v_InvalidTermError_constantCondition_y))))
  case hydra.error.core.InvalidTermError.duplicateBinding(v_InvalidTermError_duplicateBinding_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("duplicateBinding", hydra.encode.error.core.duplicateBindingError(v_InvalidTermError_duplicateBinding_y))))
  case hydra.error.core.InvalidTermError.duplicateField(v_InvalidTermError_duplicateField_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("duplicateField", hydra.encode.error.core.duplicateFieldError(v_InvalidTermError_duplicateField_y))))
  case hydra.error.core.InvalidTermError.emptyCaseStatement(v_InvalidTermError_emptyCaseStatement_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("emptyCaseStatement", hydra.encode.error.core.emptyCaseStatementError(v_InvalidTermError_emptyCaseStatement_y))))
  case hydra.error.core.InvalidTermError.emptyLetBindings(v_InvalidTermError_emptyLetBindings_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("emptyLetBindings", hydra.encode.error.core.emptyLetBindingsError(v_InvalidTermError_emptyLetBindings_y))))
  case hydra.error.core.InvalidTermError.emptyTermAnnotation(v_InvalidTermError_emptyTermAnnotation_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("emptyTermAnnotation", hydra.encode.error.core.emptyTermAnnotationError(v_InvalidTermError_emptyTermAnnotation_y))))
  case hydra.error.core.InvalidTermError.emptyTypeNameInTerm(v_InvalidTermError_emptyTypeNameInTerm_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("emptyTypeNameInTerm", hydra.encode.error.core.emptyTypeNameInTermError(v_InvalidTermError_emptyTypeNameInTerm_y))))
  case hydra.error.core.InvalidTermError.invalidLambdaParameterName(v_InvalidTermError_invalidLambdaParameterName_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("invalidLambdaParameterName", hydra.encode.error.core.invalidLambdaParameterNameError(v_InvalidTermError_invalidLambdaParameterName_y))))
  case hydra.error.core.InvalidTermError.invalidLetBindingName(v_InvalidTermError_invalidLetBindingName_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("invalidLetBindingName", hydra.encode.error.core.invalidLetBindingNameError(v_InvalidTermError_invalidLetBindingName_y))))
  case hydra.error.core.InvalidTermError.invalidTypeLambdaParameterName(v_InvalidTermError_invalidTypeLambdaParameterName_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("invalidTypeLambdaParameterName", hydra.encode.error.core.invalidTypeLambdaParameterNameError(v_InvalidTermError_invalidTypeLambdaParameterName_y))))
  case hydra.error.core.InvalidTermError.nestedTermAnnotation(v_InvalidTermError_nestedTermAnnotation_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("nestedTermAnnotation", hydra.encode.error.core.nestedTermAnnotationError(v_InvalidTermError_nestedTermAnnotation_y))))
  case hydra.error.core.InvalidTermError.redundantWrapUnwrap(v_InvalidTermError_redundantWrapUnwrap_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("redundantWrapUnwrap", hydra.encode.error.core.redundantWrapUnwrapError(v_InvalidTermError_redundantWrapUnwrap_y))))
  case hydra.error.core.InvalidTermError.selfApplication(v_InvalidTermError_selfApplication_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("selfApplication", hydra.encode.error.core.selfApplicationError(v_InvalidTermError_selfApplication_y))))
  case hydra.error.core.InvalidTermError.termVariableShadowing(v_InvalidTermError_termVariableShadowing_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("termVariableShadowing", hydra.encode.error.core.termVariableShadowingError(v_InvalidTermError_termVariableShadowing_y))))
  case hydra.error.core.InvalidTermError.typeVariableShadowingInTypeLambda(v_InvalidTermError_typeVariableShadowingInTypeLambda_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("typeVariableShadowingInTypeLambda", hydra.encode.error.core.typeVariableShadowingInTypeLambdaError(v_InvalidTermError_typeVariableShadowingInTypeLambda_y))))
  case hydra.error.core.InvalidTermError.undefinedTermVariable(v_InvalidTermError_undefinedTermVariable_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("undefinedTermVariable", hydra.encode.error.core.undefinedTermVariableError(v_InvalidTermError_undefinedTermVariable_y))))
  case hydra.error.core.InvalidTermError.undefinedTypeVariableInBindingType(v_InvalidTermError_undefinedTypeVariableInBindingType_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("undefinedTypeVariableInBindingType", hydra.encode.error.core.undefinedTypeVariableInBindingTypeError(v_InvalidTermError_undefinedTypeVariableInBindingType_y))))
  case hydra.error.core.InvalidTermError.undefinedTypeVariableInLambdaDomain(v_InvalidTermError_undefinedTypeVariableInLambdaDomain_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("undefinedTypeVariableInLambdaDomain", hydra.encode.error.core.undefinedTypeVariableInLambdaDomainError(v_InvalidTermError_undefinedTypeVariableInLambdaDomain_y))))
  case hydra.error.core.InvalidTermError.undefinedTypeVariableInTypeApplication(v_InvalidTermError_undefinedTypeVariableInTypeApplication_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("undefinedTypeVariableInTypeApplication", hydra.encode.error.core.undefinedTypeVariableInTypeApplicationError(v_InvalidTermError_undefinedTypeVariableInTypeApplication_y))))
  case hydra.error.core.InvalidTermError.unknownPrimitiveName(v_InvalidTermError_unknownPrimitiveName_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("unknownPrimitiveName", hydra.encode.error.core.unknownPrimitiveNameError(v_InvalidTermError_unknownPrimitiveName_y))))
  case hydra.error.core.InvalidTermError.unnecessaryIdentityApplication(v_InvalidTermError_unnecessaryIdentityApplication_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("unnecessaryIdentityApplication", hydra.encode.error.core.unnecessaryIdentityApplicationError(v_InvalidTermError_unnecessaryIdentityApplication_y))))
  case hydra.error.core.InvalidTermError.untypedTermVariable(v_InvalidTermError_untypedTermVariable_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTermError",
     hydra.core.Field("untypedTermVariable", hydra.encode.error.core.untypedTermVariableError(v_InvalidTermError_untypedTermVariable_y))))

def invalidTypeError(v1: hydra.error.core.InvalidTypeError): hydra.core.Term =
  v1 match
  case hydra.error.core.InvalidTypeError.duplicateRecordTypeFieldNames(v_InvalidTypeError_duplicateRecordTypeFieldNames_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTypeError",
     hydra.core.Field("duplicateRecordTypeFieldNames", hydra.encode.error.core.duplicateRecordTypeFieldNamesError(v_InvalidTypeError_duplicateRecordTypeFieldNames_y))))
  case hydra.error.core.InvalidTypeError.duplicateUnionTypeFieldNames(v_InvalidTypeError_duplicateUnionTypeFieldNames_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTypeError",
     hydra.core.Field("duplicateUnionTypeFieldNames", hydra.encode.error.core.duplicateUnionTypeFieldNamesError(v_InvalidTypeError_duplicateUnionTypeFieldNames_y))))
  case hydra.error.core.InvalidTypeError.emptyRecordType(v_InvalidTypeError_emptyRecordType_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTypeError",
     hydra.core.Field("emptyRecordType", hydra.encode.error.core.emptyRecordTypeError(v_InvalidTypeError_emptyRecordType_y))))
  case hydra.error.core.InvalidTypeError.emptyTypeAnnotation(v_InvalidTypeError_emptyTypeAnnotation_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTypeError",
     hydra.core.Field("emptyTypeAnnotation", hydra.encode.error.core.emptyTypeAnnotationError(v_InvalidTypeError_emptyTypeAnnotation_y))))
  case hydra.error.core.InvalidTypeError.emptyUnionType(v_InvalidTypeError_emptyUnionType_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTypeError",
     hydra.core.Field("emptyUnionType", hydra.encode.error.core.emptyUnionTypeError(v_InvalidTypeError_emptyUnionType_y))))
  case hydra.error.core.InvalidTypeError.invalidForallParameterName(v_InvalidTypeError_invalidForallParameterName_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTypeError",
     hydra.core.Field("invalidForallParameterName", hydra.encode.error.core.invalidForallParameterNameError(v_InvalidTypeError_invalidForallParameterName_y))))
  case hydra.error.core.InvalidTypeError.invalidTypeSchemeVariableName(v_InvalidTypeError_invalidTypeSchemeVariableName_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTypeError",
     hydra.core.Field("invalidTypeSchemeVariableName", hydra.encode.error.core.invalidTypeSchemeVariableNameError(v_InvalidTypeError_invalidTypeSchemeVariableName_y))))
  case hydra.error.core.InvalidTypeError.nestedTypeAnnotation(v_InvalidTypeError_nestedTypeAnnotation_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTypeError",
     hydra.core.Field("nestedTypeAnnotation", hydra.encode.error.core.nestedTypeAnnotationError(v_InvalidTypeError_nestedTypeAnnotation_y))))
  case hydra.error.core.InvalidTypeError.nonComparableMapKeyType(v_InvalidTypeError_nonComparableMapKeyType_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTypeError",
     hydra.core.Field("nonComparableMapKeyType", hydra.encode.error.core.nonComparableMapKeyTypeError(v_InvalidTypeError_nonComparableMapKeyType_y))))
  case hydra.error.core.InvalidTypeError.nonComparableSetElementType(v_InvalidTypeError_nonComparableSetElementType_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTypeError",
     hydra.core.Field("nonComparableSetElementType", hydra.encode.error.core.nonComparableSetElementTypeError(v_InvalidTypeError_nonComparableSetElementType_y))))
  case hydra.error.core.InvalidTypeError.singleVariantUnion(v_InvalidTypeError_singleVariantUnion_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTypeError",
     hydra.core.Field("singleVariantUnion", hydra.encode.error.core.singleVariantUnionError(v_InvalidTypeError_singleVariantUnion_y))))
  case hydra.error.core.InvalidTypeError.typeVariableShadowingInForall(v_InvalidTypeError_typeVariableShadowingInForall_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTypeError",
     hydra.core.Field("typeVariableShadowingInForall", hydra.encode.error.core.typeVariableShadowingInForallError(v_InvalidTypeError_typeVariableShadowingInForall_y))))
  case hydra.error.core.InvalidTypeError.undefinedTypeVariable(v_InvalidTypeError_undefinedTypeVariable_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTypeError",
     hydra.core.Field("undefinedTypeVariable", hydra.encode.error.core.undefinedTypeVariableError(v_InvalidTypeError_undefinedTypeVariable_y))))
  case hydra.error.core.InvalidTypeError.voidInNonBottomPosition(v_InvalidTypeError_voidInNonBottomPosition_y) => hydra.core.Term.union(hydra.core.Injection("hydra.error.core.InvalidTypeError",
     hydra.core.Field("voidInNonBottomPosition", hydra.encode.error.core.voidInNonBottomPositionError(v_InvalidTypeError_voidInNonBottomPosition_y))))

def invalidTypeLambdaParameterNameError(x: hydra.error.core.InvalidTypeLambdaParameterNameError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.InvalidTypeLambdaParameterNameError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name", hydra.encode.core.name(x.name)))))

def invalidTypeSchemeVariableNameError(x: hydra.error.core.InvalidTypeSchemeVariableNameError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.InvalidTypeSchemeVariableNameError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name", hydra.encode.core.name(x.name)))))

def nestedTermAnnotationError(x: hydra.error.core.NestedTermAnnotationError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.NestedTermAnnotationError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)))))

def nestedTypeAnnotationError(x: hydra.error.core.NestedTypeAnnotationError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.NestedTypeAnnotationError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)))))

def nonComparableMapKeyTypeError(x: hydra.error.core.NonComparableMapKeyTypeError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.NonComparableMapKeyTypeError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("keyType", hydra.encode.core.`type`(x.keyType)))))

def nonComparableSetElementTypeError(x: hydra.error.core.NonComparableSetElementTypeError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.NonComparableSetElementTypeError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("elementType", hydra.encode.core.`type`(x.elementType)))))

def redundantWrapUnwrapError(x: hydra.error.core.RedundantWrapUnwrapError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.RedundantWrapUnwrapError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("typeName", hydra.encode.core.name(x.typeName)))))

def selfApplicationError(x: hydra.error.core.SelfApplicationError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.SelfApplicationError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name", hydra.encode.core.name(x.name)))))

def singleVariantUnionError(x: hydra.error.core.SingleVariantUnionError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.SingleVariantUnionError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("fieldName", hydra.encode.core.name(x.fieldName)))))

def termVariableShadowingError(x: hydra.error.core.TermVariableShadowingError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.TermVariableShadowingError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name", hydra.encode.core.name(x.name)))))

def typeVariableShadowingInForallError(x: hydra.error.core.TypeVariableShadowingInForallError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.TypeVariableShadowingInForallError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name", hydra.encode.core.name(x.name)))))

def typeVariableShadowingInTypeLambdaError(x: hydra.error.core.TypeVariableShadowingInTypeLambdaError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.TypeVariableShadowingInTypeLambdaError",
     Seq(hydra.core.Field("location", hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name",
     hydra.encode.core.name(x.name)))))

def undefinedFieldError(x: hydra.error.core.UndefinedFieldError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.UndefinedFieldError", Seq(hydra.core.Field("fieldName",
     hydra.encode.core.name(x.fieldName)), hydra.core.Field("typeName", hydra.encode.core.name(x.typeName)))))

def undefinedTermVariableError(x: hydra.error.core.UndefinedTermVariableError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.UndefinedTermVariableError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name", hydra.encode.core.name(x.name)))))

def undefinedTypeVariableError(x: hydra.error.core.UndefinedTypeVariableError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.UndefinedTypeVariableError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name", hydra.encode.core.name(x.name)))))

def undefinedTypeVariableInBindingTypeError(x: hydra.error.core.UndefinedTypeVariableInBindingTypeError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.UndefinedTypeVariableInBindingTypeError",
     Seq(hydra.core.Field("location", hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name",
     hydra.encode.core.name(x.name)))))

def undefinedTypeVariableInLambdaDomainError(x: hydra.error.core.UndefinedTypeVariableInLambdaDomainError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.UndefinedTypeVariableInLambdaDomainError",
     Seq(hydra.core.Field("location", hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name",
     hydra.encode.core.name(x.name)))))

def undefinedTypeVariableInTypeApplicationError(x: hydra.error.core.UndefinedTypeVariableInTypeApplicationError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.UndefinedTypeVariableInTypeApplicationError",
     Seq(hydra.core.Field("location", hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name",
     hydra.encode.core.name(x.name)))))

def unexpectedTermVariantError(x: hydra.error.core.UnexpectedTermVariantError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.UnexpectedTermVariantError", Seq(hydra.core.Field("expectedVariant",
     hydra.encode.variants.termVariant(x.expectedVariant)), hydra.core.Field("actualTerm", hydra.encode.core.term(x.actualTerm)))))

def unexpectedTypeVariantError(x: hydra.error.core.UnexpectedTypeVariantError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.UnexpectedTypeVariantError", Seq(hydra.core.Field("expectedVariant",
     hydra.encode.variants.typeVariant(x.expectedVariant)), hydra.core.Field("actualType", hydra.encode.core.`type`(x.actualType)))))

def unknownPrimitiveNameError(x: hydra.error.core.UnknownPrimitiveNameError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.UnknownPrimitiveNameError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name", hydra.encode.core.name(x.name)))))

def unnecessaryIdentityApplicationError(x: hydra.error.core.UnnecessaryIdentityApplicationError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.UnnecessaryIdentityApplicationError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)))))

def untypedTermVariableError(x: hydra.error.core.UntypedTermVariableError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.UntypedTermVariableError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)), hydra.core.Field("name", hydra.encode.core.name(x.name)))))

def voidInNonBottomPositionError(x: hydra.error.core.VoidInNonBottomPositionError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.error.core.VoidInNonBottomPositionError", Seq(hydra.core.Field("location",
     hydra.encode.accessors.accessorPath(x.location)))))
