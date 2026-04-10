(* Error types for core type and term validation *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.paths hydra.core hydra.variants.
Record VoidInNonBottomPositionError : Type := Build_VoidInNonBottomPositionError {
  voidInNonBottomPositionError_location : SubtermPath
}.

Record UntypedTermVariableError : Type := Build_UntypedTermVariableError {
  untypedTermVariableError_location : SubtermPath ;
  untypedTermVariableError_name : Name
}.

Record UnnecessaryIdentityApplicationError : Type := Build_UnnecessaryIdentityApplicationError {
  unnecessaryIdentityApplicationError_location : SubtermPath
}.

Record UnknownPrimitiveNameError : Type := Build_UnknownPrimitiveNameError {
  unknownPrimitiveNameError_location : SubtermPath ;
  unknownPrimitiveNameError_name : Name
}.

Record UnexpectedTypeVariantError : Type := Build_UnexpectedTypeVariantError {
  unexpectedTypeVariantError_expectedVariant : TypeVariant ;
  unexpectedTypeVariantError_actualType : Type_
}.

Record UnexpectedTermVariantError : Type := Build_UnexpectedTermVariantError {
  unexpectedTermVariantError_expectedVariant : TermVariant ;
  unexpectedTermVariantError_actualTerm : Term
}.

Record UndefinedTypeVariableInTypeApplicationError : Type := Build_UndefinedTypeVariableInTypeApplicationError {
  undefinedTypeVariableInTypeApplicationError_location : SubtermPath ;
  undefinedTypeVariableInTypeApplicationError_name : Name
}.

Record UndefinedTypeVariableInLambdaDomainError : Type := Build_UndefinedTypeVariableInLambdaDomainError {
  undefinedTypeVariableInLambdaDomainError_location : SubtermPath ;
  undefinedTypeVariableInLambdaDomainError_name : Name
}.

Record UndefinedTypeVariableInBindingTypeError : Type := Build_UndefinedTypeVariableInBindingTypeError {
  undefinedTypeVariableInBindingTypeError_location : SubtermPath ;
  undefinedTypeVariableInBindingTypeError_name : Name
}.

Record UndefinedTypeVariableError : Type := Build_UndefinedTypeVariableError {
  undefinedTypeVariableError_location : SubtermPath ;
  undefinedTypeVariableError_name : Name
}.

Record UndefinedTermVariableError : Type := Build_UndefinedTermVariableError {
  undefinedTermVariableError_location : SubtermPath ;
  undefinedTermVariableError_name : Name
}.

Record UndefinedFieldError : Type := Build_UndefinedFieldError {
  undefinedFieldError_fieldName : Name ;
  undefinedFieldError_typeName : Name
}.

Record TypeVariableShadowingInTypeLambdaError : Type := Build_TypeVariableShadowingInTypeLambdaError {
  typeVariableShadowingInTypeLambdaError_location : SubtermPath ;
  typeVariableShadowingInTypeLambdaError_name : Name
}.

Record TypeVariableShadowingInForallError : Type := Build_TypeVariableShadowingInForallError {
  typeVariableShadowingInForallError_location : SubtermPath ;
  typeVariableShadowingInForallError_name : Name
}.

Record TermVariableShadowingError : Type := Build_TermVariableShadowingError {
  termVariableShadowingError_location : SubtermPath ;
  termVariableShadowingError_name : Name
}.

Record SingleVariantUnionError : Type := Build_SingleVariantUnionError {
  singleVariantUnionError_location : SubtermPath ;
  singleVariantUnionError_fieldName : Name
}.

Record SelfApplicationError : Type := Build_SelfApplicationError {
  selfApplicationError_location : SubtermPath ;
  selfApplicationError_name : Name
}.

Record RedundantWrapUnwrapError : Type := Build_RedundantWrapUnwrapError {
  redundantWrapUnwrapError_location : SubtermPath ;
  redundantWrapUnwrapError_typeName : Name
}.

Record NonComparableSetElementTypeError : Type := Build_NonComparableSetElementTypeError {
  nonComparableSetElementTypeError_location : SubtermPath ;
  nonComparableSetElementTypeError_elementType : Type_
}.

Record NonComparableMapKeyTypeError : Type := Build_NonComparableMapKeyTypeError {
  nonComparableMapKeyTypeError_location : SubtermPath ;
  nonComparableMapKeyTypeError_keyType : Type_
}.

Record NestedTypeAnnotationError : Type := Build_NestedTypeAnnotationError {
  nestedTypeAnnotationError_location : SubtermPath
}.

Record NestedTermAnnotationError : Type := Build_NestedTermAnnotationError {
  nestedTermAnnotationError_location : SubtermPath
}.

Record InvalidTypeSchemeVariableNameError : Type := Build_InvalidTypeSchemeVariableNameError {
  invalidTypeSchemeVariableNameError_location : SubtermPath ;
  invalidTypeSchemeVariableNameError_name : Name
}.

Record InvalidTypeLambdaParameterNameError : Type := Build_InvalidTypeLambdaParameterNameError {
  invalidTypeLambdaParameterNameError_location : SubtermPath ;
  invalidTypeLambdaParameterNameError_name : Name
}.

Record InvalidLetBindingNameError : Type := Build_InvalidLetBindingNameError {
  invalidLetBindingNameError_location : SubtermPath ;
  invalidLetBindingNameError_name : Name
}.

Record InvalidLambdaParameterNameError : Type := Build_InvalidLambdaParameterNameError {
  invalidLambdaParameterNameError_location : SubtermPath ;
  invalidLambdaParameterNameError_name : Name
}.

Record InvalidForallParameterNameError : Type := Build_InvalidForallParameterNameError {
  invalidForallParameterNameError_location : SubtermPath ;
  invalidForallParameterNameError_name : Name
}.

Record EmptyUnionTypeError : Type := Build_EmptyUnionTypeError {
  emptyUnionTypeError_location : SubtermPath
}.

Record EmptyTypeNameInTermError : Type := Build_EmptyTypeNameInTermError {
  emptyTypeNameInTermError_location : SubtermPath
}.

Record EmptyTypeAnnotationError : Type := Build_EmptyTypeAnnotationError {
  emptyTypeAnnotationError_location : SubtermPath
}.

Record EmptyTermAnnotationError : Type := Build_EmptyTermAnnotationError {
  emptyTermAnnotationError_location : SubtermPath
}.

Record EmptyRecordTypeError : Type := Build_EmptyRecordTypeError {
  emptyRecordTypeError_location : SubtermPath
}.

Record EmptyLetBindingsError : Type := Build_EmptyLetBindingsError {
  emptyLetBindingsError_location : SubtermPath
}.

Record EmptyCaseStatementError : Type := Build_EmptyCaseStatementError {
  emptyCaseStatementError_location : SubtermPath ;
  emptyCaseStatementError_typeName : Name
}.

Record DuplicateUnionTypeFieldNamesError : Type := Build_DuplicateUnionTypeFieldNamesError {
  duplicateUnionTypeFieldNamesError_location : SubtermPath ;
  duplicateUnionTypeFieldNamesError_name : Name
}.

Record DuplicateRecordTypeFieldNamesError : Type := Build_DuplicateRecordTypeFieldNamesError {
  duplicateRecordTypeFieldNamesError_location : SubtermPath ;
  duplicateRecordTypeFieldNamesError_name : Name
}.

Inductive InvalidTypeError : Type :=
| InvalidTypeError_DuplicateRecordTypeFieldNames : DuplicateRecordTypeFieldNamesError -> InvalidTypeError
| InvalidTypeError_DuplicateUnionTypeFieldNames : DuplicateUnionTypeFieldNamesError -> InvalidTypeError
| InvalidTypeError_EmptyRecordType : EmptyRecordTypeError -> InvalidTypeError
| InvalidTypeError_EmptyTypeAnnotation : EmptyTypeAnnotationError -> InvalidTypeError
| InvalidTypeError_EmptyUnionType : EmptyUnionTypeError -> InvalidTypeError
| InvalidTypeError_InvalidForallParameterName : InvalidForallParameterNameError -> InvalidTypeError
| InvalidTypeError_InvalidTypeSchemeVariableName : InvalidTypeSchemeVariableNameError -> InvalidTypeError
| InvalidTypeError_NestedTypeAnnotation : NestedTypeAnnotationError -> InvalidTypeError
| InvalidTypeError_NonComparableMapKeyType : NonComparableMapKeyTypeError -> InvalidTypeError
| InvalidTypeError_NonComparableSetElementType : NonComparableSetElementTypeError -> InvalidTypeError
| InvalidTypeError_SingleVariantUnion : SingleVariantUnionError -> InvalidTypeError
| InvalidTypeError_TypeVariableShadowingInForall : TypeVariableShadowingInForallError -> InvalidTypeError
| InvalidTypeError_UndefinedTypeVariable : UndefinedTypeVariableError -> InvalidTypeError
| InvalidTypeError_VoidInNonBottomPosition : VoidInNonBottomPositionError -> InvalidTypeError.

Record DuplicateFieldError : Type := Build_DuplicateFieldError {
  duplicateFieldError_location : SubtermPath ;
  duplicateFieldError_name : Name
}.

Record DuplicateBindingError : Type := Build_DuplicateBindingError {
  duplicateBindingError_location : SubtermPath ;
  duplicateBindingError_name : Name
}.

Record ConstantConditionError : Type := Build_ConstantConditionError {
  constantConditionError_location : SubtermPath ;
  constantConditionError_value : bool
}.

Inductive InvalidTermError : Type :=
| InvalidTermError_ConstantCondition : ConstantConditionError -> InvalidTermError
| InvalidTermError_DuplicateBinding : DuplicateBindingError -> InvalidTermError
| InvalidTermError_DuplicateField : DuplicateFieldError -> InvalidTermError
| InvalidTermError_EmptyCaseStatement : EmptyCaseStatementError -> InvalidTermError
| InvalidTermError_EmptyLetBindings : EmptyLetBindingsError -> InvalidTermError
| InvalidTermError_EmptyTermAnnotation : EmptyTermAnnotationError -> InvalidTermError
| InvalidTermError_EmptyTypeNameInTerm : EmptyTypeNameInTermError -> InvalidTermError
| InvalidTermError_InvalidLambdaParameterName : InvalidLambdaParameterNameError -> InvalidTermError
| InvalidTermError_InvalidLetBindingName : InvalidLetBindingNameError -> InvalidTermError
| InvalidTermError_InvalidTypeLambdaParameterName : InvalidTypeLambdaParameterNameError -> InvalidTermError
| InvalidTermError_NestedTermAnnotation : NestedTermAnnotationError -> InvalidTermError
| InvalidTermError_RedundantWrapUnwrap : RedundantWrapUnwrapError -> InvalidTermError
| InvalidTermError_SelfApplication : SelfApplicationError -> InvalidTermError
| InvalidTermError_TermVariableShadowing : TermVariableShadowingError -> InvalidTermError
| InvalidTermError_TypeVariableShadowingInTypeLambda : TypeVariableShadowingInTypeLambdaError -> InvalidTermError
| InvalidTermError_UndefinedTermVariable : UndefinedTermVariableError -> InvalidTermError
| InvalidTermError_UndefinedTypeVariableInBindingType : UndefinedTypeVariableInBindingTypeError -> InvalidTermError
| InvalidTermError_UndefinedTypeVariableInLambdaDomain : UndefinedTypeVariableInLambdaDomainError -> InvalidTermError
| InvalidTermError_UndefinedTypeVariableInTypeApplication : UndefinedTypeVariableInTypeApplicationError -> InvalidTermError
| InvalidTermError_UnknownPrimitiveName : UnknownPrimitiveNameError -> InvalidTermError
| InvalidTermError_UnnecessaryIdentityApplication : UnnecessaryIdentityApplicationError -> InvalidTermError
| InvalidTermError_UntypedTermVariable : UntypedTermVariableError -> InvalidTermError.

