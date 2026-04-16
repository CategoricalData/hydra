(* Error types for core type and term validation *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.paths hydra.variants.
Record ConstantConditionError : Type := Build_ConstantConditionError {
constantConditionError_location : SubtermPath ;
constantConditionError_value : bool ;
}.

Record DuplicateBindingError : Type := Build_DuplicateBindingError {
duplicateBindingError_location : SubtermPath ;
duplicateBindingError_name : Name ;
}.

Record DuplicateFieldError : Type := Build_DuplicateFieldError {
duplicateFieldError_location : SubtermPath ;
duplicateFieldError_name : Name ;
}.

Record DuplicateRecordTypeFieldNamesError : Type := Build_DuplicateRecordTypeFieldNamesError {
duplicateRecordTypeFieldNamesError_location : SubtermPath ;
duplicateRecordTypeFieldNamesError_name : Name ;
}.

Record DuplicateUnionTypeFieldNamesError : Type := Build_DuplicateUnionTypeFieldNamesError {
duplicateUnionTypeFieldNamesError_location : SubtermPath ;
duplicateUnionTypeFieldNamesError_name : Name ;
}.

Record EmptyCaseStatementError : Type := Build_EmptyCaseStatementError {
emptyCaseStatementError_location : SubtermPath ;
emptyCaseStatementError_typeName : Name ;
}.

Record EmptyLetBindingsError : Type := Build_EmptyLetBindingsError {
emptyLetBindingsError_location : SubtermPath ;
}.

Record EmptyRecordTypeError : Type := Build_EmptyRecordTypeError {
emptyRecordTypeError_location : SubtermPath ;
}.

Record EmptyTermAnnotationError : Type := Build_EmptyTermAnnotationError {
emptyTermAnnotationError_location : SubtermPath ;
}.

Record EmptyTypeAnnotationError : Type := Build_EmptyTypeAnnotationError {
emptyTypeAnnotationError_location : SubtermPath ;
}.

Record EmptyTypeNameInTermError : Type := Build_EmptyTypeNameInTermError {
emptyTypeNameInTermError_location : SubtermPath ;
}.

Record EmptyUnionTypeError : Type := Build_EmptyUnionTypeError {
emptyUnionTypeError_location : SubtermPath ;
}.

Record InvalidForallParameterNameError : Type := Build_InvalidForallParameterNameError {
invalidForallParameterNameError_location : SubtermPath ;
invalidForallParameterNameError_name : Name ;
}.

Record InvalidLambdaParameterNameError : Type := Build_InvalidLambdaParameterNameError {
invalidLambdaParameterNameError_location : SubtermPath ;
invalidLambdaParameterNameError_name : Name ;
}.

Record InvalidLetBindingNameError : Type := Build_InvalidLetBindingNameError {
invalidLetBindingNameError_location : SubtermPath ;
invalidLetBindingNameError_name : Name ;
}.

Record InvalidTypeLambdaParameterNameError : Type := Build_InvalidTypeLambdaParameterNameError {
invalidTypeLambdaParameterNameError_location : SubtermPath ;
invalidTypeLambdaParameterNameError_name : Name ;
}.

Record NestedTermAnnotationError : Type := Build_NestedTermAnnotationError {
nestedTermAnnotationError_location : SubtermPath ;
}.

Record RedundantWrapUnwrapError : Type := Build_RedundantWrapUnwrapError {
redundantWrapUnwrapError_location : SubtermPath ;
redundantWrapUnwrapError_typeName : Name ;
}.

Record SelfApplicationError : Type := Build_SelfApplicationError {
selfApplicationError_location : SubtermPath ;
selfApplicationError_name : Name ;
}.

Record TermVariableShadowingError : Type := Build_TermVariableShadowingError {
termVariableShadowingError_location : SubtermPath ;
termVariableShadowingError_name : Name ;
}.

Record TypeVariableShadowingInTypeLambdaError : Type := Build_TypeVariableShadowingInTypeLambdaError {
typeVariableShadowingInTypeLambdaError_location : SubtermPath ;
typeVariableShadowingInTypeLambdaError_name : Name ;
}.

Record UndefinedTermVariableError : Type := Build_UndefinedTermVariableError {
undefinedTermVariableError_location : SubtermPath ;
undefinedTermVariableError_name : Name ;
}.

Record UndefinedTypeVariableInBindingTypeError : Type := Build_UndefinedTypeVariableInBindingTypeError {
undefinedTypeVariableInBindingTypeError_location : SubtermPath ;
undefinedTypeVariableInBindingTypeError_name : Name ;
}.

Record UndefinedTypeVariableInLambdaDomainError : Type := Build_UndefinedTypeVariableInLambdaDomainError {
undefinedTypeVariableInLambdaDomainError_location : SubtermPath ;
undefinedTypeVariableInLambdaDomainError_name : Name ;
}.

Record UndefinedTypeVariableInTypeApplicationError : Type := Build_UndefinedTypeVariableInTypeApplicationError {
undefinedTypeVariableInTypeApplicationError_location : SubtermPath ;
undefinedTypeVariableInTypeApplicationError_name : Name ;
}.

Record UnknownPrimitiveNameError : Type := Build_UnknownPrimitiveNameError {
unknownPrimitiveNameError_location : SubtermPath ;
unknownPrimitiveNameError_name : Name ;
}.

Record UnnecessaryIdentityApplicationError : Type := Build_UnnecessaryIdentityApplicationError {
unnecessaryIdentityApplicationError_location : SubtermPath ;
}.

Record UntypedTermVariableError : Type := Build_UntypedTermVariableError {
untypedTermVariableError_location : SubtermPath ;
untypedTermVariableError_name : Name ;
}.

Inductive InvalidTermError : Type :=
| InvalidTermError_ConstantCondition : forall (_ : ConstantConditionError) , InvalidTermError
| InvalidTermError_DuplicateBinding : forall (_ : DuplicateBindingError) , InvalidTermError
| InvalidTermError_DuplicateField : forall (_ : DuplicateFieldError) , InvalidTermError
| InvalidTermError_EmptyCaseStatement : forall (_ : EmptyCaseStatementError) , InvalidTermError
| InvalidTermError_EmptyLetBindings : forall (_ : EmptyLetBindingsError) , InvalidTermError
| InvalidTermError_EmptyTermAnnotation : forall (_ : EmptyTermAnnotationError) , InvalidTermError
| InvalidTermError_EmptyTypeNameInTerm : forall (_ : EmptyTypeNameInTermError) , InvalidTermError
| InvalidTermError_InvalidLambdaParameterName : forall (_ : InvalidLambdaParameterNameError) , InvalidTermError
| InvalidTermError_InvalidLetBindingName : forall (_ : InvalidLetBindingNameError) , InvalidTermError
| InvalidTermError_InvalidTypeLambdaParameterName : forall (_ : InvalidTypeLambdaParameterNameError) , InvalidTermError
| InvalidTermError_NestedTermAnnotation : forall (_ : NestedTermAnnotationError) , InvalidTermError
| InvalidTermError_RedundantWrapUnwrap : forall (_ : RedundantWrapUnwrapError) , InvalidTermError
| InvalidTermError_SelfApplication : forall (_ : SelfApplicationError) , InvalidTermError
| InvalidTermError_TermVariableShadowing : forall (_ : TermVariableShadowingError) , InvalidTermError
| InvalidTermError_TypeVariableShadowingInTypeLambda : forall (_ : TypeVariableShadowingInTypeLambdaError) , InvalidTermError
| InvalidTermError_UndefinedTermVariable : forall (_ : UndefinedTermVariableError) , InvalidTermError
| InvalidTermError_UndefinedTypeVariableInBindingType : forall (_ : UndefinedTypeVariableInBindingTypeError) , InvalidTermError
| InvalidTermError_UndefinedTypeVariableInLambdaDomain : forall (_ : UndefinedTypeVariableInLambdaDomainError) , InvalidTermError
| InvalidTermError_UndefinedTypeVariableInTypeApplication : forall (_ : UndefinedTypeVariableInTypeApplicationError) , InvalidTermError
| InvalidTermError_UnknownPrimitiveName : forall (_ : UnknownPrimitiveNameError) , InvalidTermError
| InvalidTermError_UnnecessaryIdentityApplication : forall (_ : UnnecessaryIdentityApplicationError) , InvalidTermError
| InvalidTermError_UntypedTermVariable : forall (_ : UntypedTermVariableError) , InvalidTermError.

Record InvalidTypeSchemeVariableNameError : Type := Build_InvalidTypeSchemeVariableNameError {
invalidTypeSchemeVariableNameError_location : SubtermPath ;
invalidTypeSchemeVariableNameError_name : Name ;
}.

Record NestedTypeAnnotationError : Type := Build_NestedTypeAnnotationError {
nestedTypeAnnotationError_location : SubtermPath ;
}.

Record NonComparableMapKeyTypeError : Type := Build_NonComparableMapKeyTypeError {
nonComparableMapKeyTypeError_location : SubtermPath ;
nonComparableMapKeyTypeError_keyType : Type_ ;
}.

Record NonComparableSetElementTypeError : Type := Build_NonComparableSetElementTypeError {
nonComparableSetElementTypeError_location : SubtermPath ;
nonComparableSetElementTypeError_elementType : Type_ ;
}.

Record SingleVariantUnionError : Type := Build_SingleVariantUnionError {
singleVariantUnionError_location : SubtermPath ;
singleVariantUnionError_fieldName : Name ;
}.

Record TypeVariableShadowingInForallError : Type := Build_TypeVariableShadowingInForallError {
typeVariableShadowingInForallError_location : SubtermPath ;
typeVariableShadowingInForallError_name : Name ;
}.

Record UndefinedTypeVariableError : Type := Build_UndefinedTypeVariableError {
undefinedTypeVariableError_location : SubtermPath ;
undefinedTypeVariableError_name : Name ;
}.

Record VoidInNonBottomPositionError : Type := Build_VoidInNonBottomPositionError {
voidInNonBottomPositionError_location : SubtermPath ;
}.

Inductive InvalidTypeError : Type :=
| InvalidTypeError_DuplicateRecordTypeFieldNames : forall (_ : DuplicateRecordTypeFieldNamesError) , InvalidTypeError
| InvalidTypeError_DuplicateUnionTypeFieldNames : forall (_ : DuplicateUnionTypeFieldNamesError) , InvalidTypeError
| InvalidTypeError_EmptyRecordType : forall (_ : EmptyRecordTypeError) , InvalidTypeError
| InvalidTypeError_EmptyTypeAnnotation : forall (_ : EmptyTypeAnnotationError) , InvalidTypeError
| InvalidTypeError_EmptyUnionType : forall (_ : EmptyUnionTypeError) , InvalidTypeError
| InvalidTypeError_InvalidForallParameterName : forall (_ : InvalidForallParameterNameError) , InvalidTypeError
| InvalidTypeError_InvalidTypeSchemeVariableName : forall (_ : InvalidTypeSchemeVariableNameError) , InvalidTypeError
| InvalidTypeError_NestedTypeAnnotation : forall (_ : NestedTypeAnnotationError) , InvalidTypeError
| InvalidTypeError_NonComparableMapKeyType : forall (_ : NonComparableMapKeyTypeError) , InvalidTypeError
| InvalidTypeError_NonComparableSetElementType : forall (_ : NonComparableSetElementTypeError) , InvalidTypeError
| InvalidTypeError_SingleVariantUnion : forall (_ : SingleVariantUnionError) , InvalidTypeError
| InvalidTypeError_TypeVariableShadowingInForall : forall (_ : TypeVariableShadowingInForallError) , InvalidTypeError
| InvalidTypeError_UndefinedTypeVariable : forall (_ : UndefinedTypeVariableError) , InvalidTypeError
| InvalidTypeError_VoidInNonBottomPosition : forall (_ : VoidInNonBottomPositionError) , InvalidTypeError.

Record UndefinedFieldError : Type := Build_UndefinedFieldError {
undefinedFieldError_fieldName : Name ;
undefinedFieldError_typeName : Name ;
}.

Record UnexpectedTermVariantError : Type := Build_UnexpectedTermVariantError {
unexpectedTermVariantError_expectedVariant : TermVariant ;
unexpectedTermVariantError_actualTerm : Term ;
}.

Record UnexpectedTypeVariantError : Type := Build_UnexpectedTypeVariantError {
unexpectedTypeVariantError_expectedVariant : TypeVariant ;
unexpectedTypeVariantError_actualType : Type_ ;
}.

