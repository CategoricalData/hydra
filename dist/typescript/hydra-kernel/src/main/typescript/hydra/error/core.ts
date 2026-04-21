// Note: this is an automatically generated file. Do not edit.

/**
 * Error types for core type and term validation
 */



import * as Core from "../core.js";
import * as Paths from "../paths.js";
import * as Variants from "../variants.js";

export interface DuplicateBindingError {
  readonly location: Paths.SubtermPath;
  readonly name: Core.Name;
}

export interface DuplicateFieldError {
  readonly location: Paths.SubtermPath;
  readonly name: Core.Name;
}

export interface UndefinedFieldError {
  readonly fieldName: Core.Name;
  readonly typeName: Core.Name;
}

export interface UnexpectedTermVariantError {
  readonly expectedVariant: Variants.TermVariant;
  readonly actualTerm: Core.Term;
}

export interface UnexpectedTypeVariantError {
  readonly expectedVariant: Variants.TypeVariant;
  readonly actualType: Core.Type;
}

export interface ConstantConditionError {
  readonly location: Paths.SubtermPath;
  readonly value: boolean;
}

export interface EmptyCaseStatementError {
  readonly location: Paths.SubtermPath;
  readonly typeName: Core.Name;
}

export interface EmptyLetBindingsError {
  readonly location: Paths.SubtermPath;
}

export interface EmptyTermAnnotationError {
  readonly location: Paths.SubtermPath;
}

export interface EmptyTypeNameInTermError {
  readonly location: Paths.SubtermPath;
}

export interface InvalidLambdaParameterNameError {
  readonly location: Paths.SubtermPath;
  readonly name: Core.Name;
}

export interface InvalidLetBindingNameError {
  readonly location: Paths.SubtermPath;
  readonly name: Core.Name;
}

export interface InvalidTypeLambdaParameterNameError {
  readonly location: Paths.SubtermPath;
  readonly name: Core.Name;
}

export interface NestedTermAnnotationError {
  readonly location: Paths.SubtermPath;
}

export interface RedundantWrapUnwrapError {
  readonly location: Paths.SubtermPath;
  readonly typeName: Core.Name;
}

export interface SelfApplicationError {
  readonly location: Paths.SubtermPath;
  readonly name: Core.Name;
}

export interface TermVariableShadowingError {
  readonly location: Paths.SubtermPath;
  readonly name: Core.Name;
}

export interface TypeVariableShadowingInTypeLambdaError {
  readonly location: Paths.SubtermPath;
  readonly name: Core.Name;
}

export interface UndefinedTermVariableError {
  readonly location: Paths.SubtermPath;
  readonly name: Core.Name;
}

export interface UndefinedTypeVariableInBindingTypeError {
  readonly location: Paths.SubtermPath;
  readonly name: Core.Name;
}

export interface UndefinedTypeVariableInLambdaDomainError {
  readonly location: Paths.SubtermPath;
  readonly name: Core.Name;
}

export interface UndefinedTypeVariableInTypeApplicationError {
  readonly location: Paths.SubtermPath;
  readonly name: Core.Name;
}

export interface UnknownPrimitiveNameError {
  readonly location: Paths.SubtermPath;
  readonly name: Core.Name;
}

export interface UnnecessaryIdentityApplicationError {
  readonly location: Paths.SubtermPath;
}

export interface UntypedTermVariableError {
  readonly location: Paths.SubtermPath;
  readonly name: Core.Name;
}

export type InvalidTermError =
  | { readonly tag: "constantCondition"; readonly value: ConstantConditionError }
  | { readonly tag: "duplicateBinding"; readonly value: DuplicateBindingError }
  | { readonly tag: "duplicateField"; readonly value: DuplicateFieldError }
  | { readonly tag: "emptyCaseStatement"; readonly value: EmptyCaseStatementError }
  | { readonly tag: "emptyLetBindings"; readonly value: EmptyLetBindingsError }
  | { readonly tag: "emptyTermAnnotation"; readonly value: EmptyTermAnnotationError }
  | { readonly tag: "emptyTypeNameInTerm"; readonly value: EmptyTypeNameInTermError }
  | { readonly tag: "invalidLambdaParameterName"; readonly value: InvalidLambdaParameterNameError }
  | { readonly tag: "invalidLetBindingName"; readonly value: InvalidLetBindingNameError }
  | { readonly tag: "invalidTypeLambdaParameterName"; readonly value: InvalidTypeLambdaParameterNameError }
  | { readonly tag: "nestedTermAnnotation"; readonly value: NestedTermAnnotationError }
  | { readonly tag: "redundantWrapUnwrap"; readonly value: RedundantWrapUnwrapError }
  | { readonly tag: "selfApplication"; readonly value: SelfApplicationError }
  | { readonly tag: "termVariableShadowing"; readonly value: TermVariableShadowingError }
  | { readonly tag: "typeVariableShadowingInTypeLambda"; readonly value: TypeVariableShadowingInTypeLambdaError }
  | { readonly tag: "undefinedTermVariable"; readonly value: UndefinedTermVariableError }
  | { readonly tag: "undefinedTypeVariableInBindingType"; readonly value: UndefinedTypeVariableInBindingTypeError }
  | { readonly tag: "undefinedTypeVariableInLambdaDomain"; readonly value: UndefinedTypeVariableInLambdaDomainError }
  | { readonly tag: "undefinedTypeVariableInTypeApplication"; readonly value: UndefinedTypeVariableInTypeApplicationError }
  | { readonly tag: "unknownPrimitiveName"; readonly value: UnknownPrimitiveNameError }
  | { readonly tag: "unnecessaryIdentityApplication"; readonly value: UnnecessaryIdentityApplicationError }
  | { readonly tag: "untypedTermVariable"; readonly value: UntypedTermVariableError };

export interface DuplicateRecordTypeFieldNamesError {
  readonly location: Paths.SubtermPath;
  readonly name: Core.Name;
}

export interface DuplicateUnionTypeFieldNamesError {
  readonly location: Paths.SubtermPath;
  readonly name: Core.Name;
}

export interface EmptyRecordTypeError {
  readonly location: Paths.SubtermPath;
}

export interface EmptyTypeAnnotationError {
  readonly location: Paths.SubtermPath;
}

export interface EmptyUnionTypeError {
  readonly location: Paths.SubtermPath;
}

export interface InvalidForallParameterNameError {
  readonly location: Paths.SubtermPath;
  readonly name: Core.Name;
}

export interface InvalidTypeSchemeVariableNameError {
  readonly location: Paths.SubtermPath;
  readonly name: Core.Name;
}

export interface NestedTypeAnnotationError {
  readonly location: Paths.SubtermPath;
}

export interface NonComparableMapKeyTypeError {
  readonly location: Paths.SubtermPath;
  readonly keyType: Core.Type;
}

export interface NonComparableSetElementTypeError {
  readonly location: Paths.SubtermPath;
  readonly elementType: Core.Type;
}

export interface SingleVariantUnionError {
  readonly location: Paths.SubtermPath;
  readonly fieldName: Core.Name;
}

export interface TypeVariableShadowingInForallError {
  readonly location: Paths.SubtermPath;
  readonly name: Core.Name;
}

export interface UndefinedTypeVariableError {
  readonly location: Paths.SubtermPath;
  readonly name: Core.Name;
}

export interface VoidInNonBottomPositionError {
  readonly location: Paths.SubtermPath;
}

export type InvalidTypeError =
  | { readonly tag: "duplicateRecordTypeFieldNames"; readonly value: DuplicateRecordTypeFieldNamesError }
  | { readonly tag: "duplicateUnionTypeFieldNames"; readonly value: DuplicateUnionTypeFieldNamesError }
  | { readonly tag: "emptyRecordType"; readonly value: EmptyRecordTypeError }
  | { readonly tag: "emptyTypeAnnotation"; readonly value: EmptyTypeAnnotationError }
  | { readonly tag: "emptyUnionType"; readonly value: EmptyUnionTypeError }
  | { readonly tag: "invalidForallParameterName"; readonly value: InvalidForallParameterNameError }
  | { readonly tag: "invalidTypeSchemeVariableName"; readonly value: InvalidTypeSchemeVariableNameError }
  | { readonly tag: "nestedTypeAnnotation"; readonly value: NestedTypeAnnotationError }
  | { readonly tag: "nonComparableMapKeyType"; readonly value: NonComparableMapKeyTypeError }
  | { readonly tag: "nonComparableSetElementType"; readonly value: NonComparableSetElementTypeError }
  | { readonly tag: "singleVariantUnion"; readonly value: SingleVariantUnionError }
  | { readonly tag: "typeVariableShadowingInForall"; readonly value: TypeVariableShadowingInForallError }
  | { readonly tag: "undefinedTypeVariable"; readonly value: UndefinedTypeVariableError }
  | { readonly tag: "voidInNonBottomPosition"; readonly value: VoidInNonBottomPositionError };
