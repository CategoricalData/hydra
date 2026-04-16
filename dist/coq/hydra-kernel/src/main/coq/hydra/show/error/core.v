(* String representations of hydra.error.core types *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.error.core hydra.lib.literals hydra.lib.strings hydra.show.core hydra.show.variants.

Definition constantConditionError : forall (_ : ConstantConditionError) , string := fun (e : ConstantConditionError) => (strings.cat) ((cons) ("constant condition: ifElse with literal "%string) ((cons) ((literals.showBoolean) ((fun r_ => (constantConditionError_value) (r_)) (e))) (nil))).
Definition duplicateBindingError : forall (_ : DuplicateBindingError) , string := fun (e : DuplicateBindingError) => (strings.cat) ((cons) ("duplicate binding: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (duplicateBindingError_name) (r_)) (e))) (nil))).
Definition duplicateFieldError : forall (_ : DuplicateFieldError) , string := fun (e : DuplicateFieldError) => (strings.cat) ((cons) ("duplicate field: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (duplicateFieldError_name) (r_)) (e))) (nil))).
Definition duplicateRecordTypeFieldNamesError : forall (_ : DuplicateRecordTypeFieldNamesError) , string := fun (e : DuplicateRecordTypeFieldNamesError) => (strings.cat) ((cons) ("duplicate field in record type: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (duplicateRecordTypeFieldNamesError_name) (r_)) (e))) (nil))).
Definition duplicateUnionTypeFieldNamesError : forall (_ : DuplicateUnionTypeFieldNamesError) , string := fun (e : DuplicateUnionTypeFieldNamesError) => (strings.cat) ((cons) ("duplicate field in union type: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (duplicateUnionTypeFieldNamesError_name) (r_)) (e))) (nil))).
Definition emptyCaseStatementError : forall (_ : EmptyCaseStatementError) , string := fun (e : EmptyCaseStatementError) => (strings.cat) ((cons) ("empty case statement for type: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (emptyCaseStatementError_typeName) (r_)) (e))) (nil))).
Definition emptyLetBindingsError (t0 : Type) : forall (_ : t0) , string := fun (e : t0) => "let expression with no bindings"%string.
Arguments emptyLetBindingsError {t0}.
Definition emptyRecordTypeError (t0 : Type) : forall (_ : t0) , string := fun (e : t0) => "record type with no fields (use TypeUnit instead)"%string.
Arguments emptyRecordTypeError {t0}.
Definition emptyTermAnnotationError (t0 : Type) : forall (_ : t0) , string := fun (e : t0) => "term annotation with empty annotation map"%string.
Arguments emptyTermAnnotationError {t0}.
Definition emptyTypeAnnotationError (t0 : Type) : forall (_ : t0) , string := fun (e : t0) => "type annotation with empty annotation map"%string.
Arguments emptyTypeAnnotationError {t0}.
Definition emptyTypeNameInTermError (t0 : Type) : forall (_ : t0) , string := fun (e : t0) => "term with empty type name"%string.
Arguments emptyTypeNameInTermError {t0}.
Definition emptyUnionTypeError (t0 : Type) : forall (_ : t0) , string := fun (e : t0) => "union type with no alternatives (use TypeVoid instead)"%string.
Arguments emptyUnionTypeError {t0}.
Definition invalidForallParameterNameError : forall (_ : InvalidForallParameterNameError) , string := fun (e : InvalidForallParameterNameError) => (strings.cat) ((cons) ("invalid forall parameter name: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (invalidForallParameterNameError_name) (r_)) (e))) (nil))).
Definition invalidLambdaParameterNameError : forall (_ : InvalidLambdaParameterNameError) , string := fun (e : InvalidLambdaParameterNameError) => (strings.cat) ((cons) ("invalid lambda parameter name: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (invalidLambdaParameterNameError_name) (r_)) (e))) (nil))).
Definition invalidLetBindingNameError : forall (_ : InvalidLetBindingNameError) , string := fun (e : InvalidLetBindingNameError) => (strings.cat) ((cons) ("invalid let binding name: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (invalidLetBindingNameError_name) (r_)) (e))) (nil))).
Definition invalidTypeLambdaParameterNameError : forall (_ : InvalidTypeLambdaParameterNameError) , string := fun (e : InvalidTypeLambdaParameterNameError) => (strings.cat) ((cons) ("invalid type lambda parameter name: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (invalidTypeLambdaParameterNameError_name) (r_)) (e))) (nil))).
Definition nestedTermAnnotationError (t0 : Type) : forall (_ : t0) , string := fun (e : t0) => "nested term annotations should be merged"%string.
Arguments nestedTermAnnotationError {t0}.
Definition redundantWrapUnwrapError : forall (_ : RedundantWrapUnwrapError) , string := fun (e : RedundantWrapUnwrapError) => (strings.cat) ((cons) ("redundant wrap/unwrap for type: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (redundantWrapUnwrapError_typeName) (r_)) (e))) (nil))).
Definition selfApplicationError : forall (_ : SelfApplicationError) , string := fun (e : SelfApplicationError) => (strings.cat) ((cons) ("self-application of variable: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (selfApplicationError_name) (r_)) (e))) (nil))).
Definition termVariableShadowingError : forall (_ : TermVariableShadowingError) , string := fun (e : TermVariableShadowingError) => (strings.cat) ((cons) ("variable shadowing: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (termVariableShadowingError_name) (r_)) (e))) (nil))).
Definition typeVariableShadowingInTypeLambdaError : forall (_ : TypeVariableShadowingInTypeLambdaError) , string := fun (e : TypeVariableShadowingInTypeLambdaError) => (strings.cat) ((cons) ("type variable shadowing in type lambda: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (typeVariableShadowingInTypeLambdaError_name) (r_)) (e))) (nil))).
Definition undefinedTermVariableError : forall (_ : UndefinedTermVariableError) , string := fun (e : UndefinedTermVariableError) => (strings.cat) ((cons) ("undefined term variable: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (undefinedTermVariableError_name) (r_)) (e))) (nil))).
Definition undefinedTypeVariableInBindingTypeError : forall (_ : UndefinedTypeVariableInBindingTypeError) , string := fun (e : UndefinedTypeVariableInBindingTypeError) => (strings.cat) ((cons) ("undefined type variable in binding type: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (undefinedTypeVariableInBindingTypeError_name) (r_)) (e))) (nil))).
Definition undefinedTypeVariableInLambdaDomainError : forall (_ : UndefinedTypeVariableInLambdaDomainError) , string := fun (e : UndefinedTypeVariableInLambdaDomainError) => (strings.cat) ((cons) ("undefined type variable in lambda domain: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (undefinedTypeVariableInLambdaDomainError_name) (r_)) (e))) (nil))).
Definition undefinedTypeVariableInTypeApplicationError : forall (_ : UndefinedTypeVariableInTypeApplicationError) , string := fun (e : UndefinedTypeVariableInTypeApplicationError) => (strings.cat) ((cons) ("undefined type variable in type application: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (undefinedTypeVariableInTypeApplicationError_name) (r_)) (e))) (nil))).
Definition unknownPrimitiveNameError : forall (_ : UnknownPrimitiveNameError) , string := fun (e : UnknownPrimitiveNameError) => (strings.cat) ((cons) ("unknown primitive: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (unknownPrimitiveNameError_name) (r_)) (e))) (nil))).
Definition unnecessaryIdentityApplicationError (t0 : Type) : forall (_ : t0) , string := fun (e : t0) => "unnecessary application of identity lambda"%string.
Arguments unnecessaryIdentityApplicationError {t0}.
Definition untypedTermVariableError : forall (_ : UntypedTermVariableError) , string := fun (e : UntypedTermVariableError) => (strings.cat) ((cons) ("untyped term variable: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (untypedTermVariableError_name) (r_)) (e))) (nil))).
Definition invalidTermError : forall (_ : InvalidTermError) , string := fun (e : InvalidTermError) => ((strings.cat2) ("invalid term: "%string)) ((fun x_ => match x_ with
| InvalidTermError_ConstantCondition v_ => (constantConditionError) (v_)
| InvalidTermError_DuplicateBinding v_ => (duplicateBindingError) (v_)
| InvalidTermError_DuplicateField v_ => (duplicateFieldError) (v_)
| InvalidTermError_EmptyCaseStatement v_ => (emptyCaseStatementError) (v_)
| InvalidTermError_EmptyLetBindings v_ => (emptyLetBindingsError) (v_)
| InvalidTermError_EmptyTermAnnotation v_ => (emptyTermAnnotationError) (v_)
| InvalidTermError_EmptyTypeNameInTerm v_ => (emptyTypeNameInTermError) (v_)
| InvalidTermError_InvalidLambdaParameterName v_ => (invalidLambdaParameterNameError) (v_)
| InvalidTermError_InvalidLetBindingName v_ => (invalidLetBindingNameError) (v_)
| InvalidTermError_InvalidTypeLambdaParameterName v_ => (invalidTypeLambdaParameterNameError) (v_)
| InvalidTermError_NestedTermAnnotation v_ => (nestedTermAnnotationError) (v_)
| InvalidTermError_RedundantWrapUnwrap v_ => (redundantWrapUnwrapError) (v_)
| InvalidTermError_SelfApplication v_ => (selfApplicationError) (v_)
| InvalidTermError_TermVariableShadowing v_ => (termVariableShadowingError) (v_)
| InvalidTermError_TypeVariableShadowingInTypeLambda v_ => (typeVariableShadowingInTypeLambdaError) (v_)
| InvalidTermError_UndefinedTermVariable v_ => (undefinedTermVariableError) (v_)
| InvalidTermError_UndefinedTypeVariableInBindingType v_ => (undefinedTypeVariableInBindingTypeError) (v_)
| InvalidTermError_UndefinedTypeVariableInLambdaDomain v_ => (undefinedTypeVariableInLambdaDomainError) (v_)
| InvalidTermError_UndefinedTypeVariableInTypeApplication v_ => (undefinedTypeVariableInTypeApplicationError) (v_)
| InvalidTermError_UnknownPrimitiveName v_ => (unknownPrimitiveNameError) (v_)
| InvalidTermError_UnnecessaryIdentityApplication v_ => (unnecessaryIdentityApplicationError) (v_)
| InvalidTermError_UntypedTermVariable v_ => (untypedTermVariableError) (v_)
end) (e)).
Definition invalidTypeSchemeVariableNameError : forall (_ : InvalidTypeSchemeVariableNameError) , string := fun (e : InvalidTypeSchemeVariableNameError) => (strings.cat) ((cons) ("invalid type scheme variable name: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (invalidTypeSchemeVariableNameError_name) (r_)) (e))) (nil))).
Definition nestedTypeAnnotationError (t0 : Type) : forall (_ : t0) , string := fun (e : t0) => "nested type annotations should be merged"%string.
Arguments nestedTypeAnnotationError {t0}.
Definition nonComparableMapKeyTypeError : forall (_ : NonComparableMapKeyTypeError) , string := fun (e : NonComparableMapKeyTypeError) => (strings.cat) ((cons) ("map key type contains a function type: "%string) ((cons) ((hydra.show.core.type) ((fun r_ => (nonComparableMapKeyTypeError_keyType) (r_)) (e))) (nil))).
Definition nonComparableSetElementTypeError : forall (_ : NonComparableSetElementTypeError) , string := fun (e : NonComparableSetElementTypeError) => (strings.cat) ((cons) ("set element type contains a function type: "%string) ((cons) ((hydra.show.core.type) ((fun r_ => (nonComparableSetElementTypeError_elementType) (r_)) (e))) (nil))).
Definition singleVariantUnionError : forall (_ : SingleVariantUnionError) , string := fun (e : SingleVariantUnionError) => (strings.cat) ((cons) ("union type with single variant: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (singleVariantUnionError_fieldName) (r_)) (e))) (nil))).
Definition typeVariableShadowingInForallError : forall (_ : TypeVariableShadowingInForallError) , string := fun (e : TypeVariableShadowingInForallError) => (strings.cat) ((cons) ("type variable shadowing in forall: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (typeVariableShadowingInForallError_name) (r_)) (e))) (nil))).
Definition undefinedTypeVariableError : forall (_ : UndefinedTypeVariableError) , string := fun (e : UndefinedTypeVariableError) => (strings.cat) ((cons) ("undefined type variable: "%string) ((cons) ((fun w_ => w_) ((fun r_ => (undefinedTypeVariableError_name) (r_)) (e))) (nil))).
Definition voidInNonBottomPositionError (t0 : Type) : forall (_ : t0) , string := fun (e : t0) => "TypeVoid in a position where no value can be constructed"%string.
Arguments voidInNonBottomPositionError {t0}.
Definition invalidTypeError : forall (_ : InvalidTypeError) , string := fun (e : InvalidTypeError) => ((strings.cat2) ("invalid type: "%string)) ((fun x_ => match x_ with
| InvalidTypeError_DuplicateRecordTypeFieldNames v_ => (duplicateRecordTypeFieldNamesError) (v_)
| InvalidTypeError_DuplicateUnionTypeFieldNames v_ => (duplicateUnionTypeFieldNamesError) (v_)
| InvalidTypeError_EmptyRecordType v_ => (emptyRecordTypeError) (v_)
| InvalidTypeError_EmptyTypeAnnotation v_ => (emptyTypeAnnotationError) (v_)
| InvalidTypeError_EmptyUnionType v_ => (emptyUnionTypeError) (v_)
| InvalidTypeError_InvalidForallParameterName v_ => (invalidForallParameterNameError) (v_)
| InvalidTypeError_InvalidTypeSchemeVariableName v_ => (invalidTypeSchemeVariableNameError) (v_)
| InvalidTypeError_NestedTypeAnnotation v_ => (nestedTypeAnnotationError) (v_)
| InvalidTypeError_NonComparableMapKeyType v_ => (nonComparableMapKeyTypeError) (v_)
| InvalidTypeError_NonComparableSetElementType v_ => (nonComparableSetElementTypeError) (v_)
| InvalidTypeError_SingleVariantUnion v_ => (singleVariantUnionError) (v_)
| InvalidTypeError_TypeVariableShadowingInForall v_ => (typeVariableShadowingInForallError) (v_)
| InvalidTypeError_UndefinedTypeVariable v_ => (undefinedTypeVariableError) (v_)
| InvalidTypeError_VoidInNonBottomPosition v_ => (voidInNonBottomPositionError) (v_)
end) (e)).
Definition undefinedFieldError : forall (_ : UndefinedFieldError) , string := fun (e : UndefinedFieldError) => let fname := (fun r_ => (undefinedFieldError_fieldName) (r_)) (e) in let tname := (fun r_ => (undefinedFieldError_typeName) (r_)) (e) in (strings.cat) ((cons) ("no such field """%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (""" in type """%string) ((cons) ((fun w_ => w_) (tname)) ((cons) (""""%string) (nil)))))).
Definition unexpectedTermVariantError : forall (_ : UnexpectedTermVariantError) , string := fun (e : UnexpectedTermVariantError) => let actual := (fun r_ => (unexpectedTermVariantError_actualTerm) (r_)) (e) in let expected := (fun r_ => (unexpectedTermVariantError_expectedVariant) (r_)) (e) in (strings.cat) ((cons) ("expected "%string) ((cons) ((hydra.show.variants.termVariant) (expected)) ((cons) (" term but found "%string) ((cons) ((hydra.show.core.term) (actual)) (nil))))).
Definition unexpectedTypeVariantError : forall (_ : UnexpectedTypeVariantError) , string := fun (e : UnexpectedTypeVariantError) => let actual := (fun r_ => (unexpectedTypeVariantError_actualType) (r_)) (e) in let expected := (fun r_ => (unexpectedTypeVariantError_expectedVariant) (r_)) (e) in (strings.cat) ((cons) ("expected "%string) ((cons) ((hydra.show.variants.typeVariant) (expected)) ((cons) (" type but found "%string) ((cons) ((hydra.show.core.type) (actual)) (nil))))).

