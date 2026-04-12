package hydra.error.core

import hydra.core.*

import hydra.paths.*

import hydra.variants.*

import hydra.core

import hydra.paths

import hydra.variants

case class DuplicateBindingError(location: hydra.paths.SubtermPath, name: hydra.core.Name)

case class DuplicateFieldError(location: hydra.paths.SubtermPath, name: hydra.core.Name)

case class UndefinedFieldError(fieldName: hydra.core.Name, typeName: hydra.core.Name)

case class UnexpectedTermVariantError(expectedVariant: hydra.variants.TermVariant, actualTerm: hydra.core.Term)

case class UnexpectedTypeVariantError(expectedVariant: hydra.variants.TypeVariant, actualType: hydra.core.Type)

case class ConstantConditionError(location: hydra.paths.SubtermPath, value: Boolean)

case class EmptyCaseStatementError(location: hydra.paths.SubtermPath, typeName: hydra.core.Name)

case class EmptyLetBindingsError(location: hydra.paths.SubtermPath)

case class EmptyTermAnnotationError(location: hydra.paths.SubtermPath)

case class EmptyTypeNameInTermError(location: hydra.paths.SubtermPath)

case class InvalidLambdaParameterNameError(location: hydra.paths.SubtermPath, name: hydra.core.Name)

case class InvalidLetBindingNameError(location: hydra.paths.SubtermPath, name: hydra.core.Name)

case class InvalidTypeLambdaParameterNameError(location: hydra.paths.SubtermPath, name: hydra.core.Name)

case class NestedTermAnnotationError(location: hydra.paths.SubtermPath)

case class RedundantWrapUnwrapError(location: hydra.paths.SubtermPath, typeName: hydra.core.Name)

case class SelfApplicationError(location: hydra.paths.SubtermPath, name: hydra.core.Name)

case class TermVariableShadowingError(location: hydra.paths.SubtermPath, name: hydra.core.Name)

case class TypeVariableShadowingInTypeLambdaError(location: hydra.paths.SubtermPath, name: hydra.core.Name)

case class UndefinedTermVariableError(location: hydra.paths.SubtermPath, name: hydra.core.Name)

case class UndefinedTypeVariableInBindingTypeError(location: hydra.paths.SubtermPath, name: hydra.core.Name)

case class UndefinedTypeVariableInLambdaDomainError(location: hydra.paths.SubtermPath, name: hydra.core.Name)

case class UndefinedTypeVariableInTypeApplicationError(location: hydra.paths.SubtermPath, name: hydra.core.Name)

case class UnknownPrimitiveNameError(location: hydra.paths.SubtermPath, name: hydra.core.Name)

case class UnnecessaryIdentityApplicationError(location: hydra.paths.SubtermPath)

case class UntypedTermVariableError(location: hydra.paths.SubtermPath, name: hydra.core.Name)

enum InvalidTermError :
   case constantCondition(value: hydra.error.core.ConstantConditionError) extends InvalidTermError
   case duplicateBinding(value: hydra.error.core.DuplicateBindingError) extends InvalidTermError
   case duplicateField(value: hydra.error.core.DuplicateFieldError) extends InvalidTermError
   case emptyCaseStatement(value: hydra.error.core.EmptyCaseStatementError) extends InvalidTermError
   case emptyLetBindings(value: hydra.error.core.EmptyLetBindingsError) extends InvalidTermError
   case emptyTermAnnotation(value: hydra.error.core.EmptyTermAnnotationError) extends InvalidTermError
   case emptyTypeNameInTerm(value: hydra.error.core.EmptyTypeNameInTermError) extends InvalidTermError
   case invalidLambdaParameterName(value: hydra.error.core.InvalidLambdaParameterNameError) extends InvalidTermError
   case invalidLetBindingName(value: hydra.error.core.InvalidLetBindingNameError) extends InvalidTermError
   case invalidTypeLambdaParameterName(value: hydra.error.core.InvalidTypeLambdaParameterNameError) extends InvalidTermError
   case nestedTermAnnotation(value: hydra.error.core.NestedTermAnnotationError) extends InvalidTermError
   case redundantWrapUnwrap(value: hydra.error.core.RedundantWrapUnwrapError) extends InvalidTermError
   case selfApplication(value: hydra.error.core.SelfApplicationError) extends InvalidTermError
   case termVariableShadowing(value: hydra.error.core.TermVariableShadowingError) extends InvalidTermError
   case typeVariableShadowingInTypeLambda(value: hydra.error.core.TypeVariableShadowingInTypeLambdaError) extends InvalidTermError
   case undefinedTermVariable(value: hydra.error.core.UndefinedTermVariableError) extends InvalidTermError
   case undefinedTypeVariableInBindingType(value: hydra.error.core.UndefinedTypeVariableInBindingTypeError) extends InvalidTermError
   case undefinedTypeVariableInLambdaDomain(value: hydra.error.core.UndefinedTypeVariableInLambdaDomainError) extends InvalidTermError
   case undefinedTypeVariableInTypeApplication(value: hydra.error.core.UndefinedTypeVariableInTypeApplicationError) extends InvalidTermError
   case unknownPrimitiveName(value: hydra.error.core.UnknownPrimitiveNameError) extends InvalidTermError
   case unnecessaryIdentityApplication(value: hydra.error.core.UnnecessaryIdentityApplicationError) extends InvalidTermError
   case untypedTermVariable(value: hydra.error.core.UntypedTermVariableError) extends InvalidTermError

case class DuplicateRecordTypeFieldNamesError(location: hydra.paths.SubtermPath, name: hydra.core.Name)

case class DuplicateUnionTypeFieldNamesError(location: hydra.paths.SubtermPath, name: hydra.core.Name)

case class EmptyRecordTypeError(location: hydra.paths.SubtermPath)

case class EmptyTypeAnnotationError(location: hydra.paths.SubtermPath)

case class EmptyUnionTypeError(location: hydra.paths.SubtermPath)

case class InvalidForallParameterNameError(location: hydra.paths.SubtermPath, name: hydra.core.Name)

case class InvalidTypeSchemeVariableNameError(location: hydra.paths.SubtermPath, name: hydra.core.Name)

case class NestedTypeAnnotationError(location: hydra.paths.SubtermPath)

case class NonComparableMapKeyTypeError(location: hydra.paths.SubtermPath, keyType: hydra.core.Type)

case class NonComparableSetElementTypeError(location: hydra.paths.SubtermPath, elementType: hydra.core.Type)

case class SingleVariantUnionError(location: hydra.paths.SubtermPath, fieldName: hydra.core.Name)

case class TypeVariableShadowingInForallError(location: hydra.paths.SubtermPath, name: hydra.core.Name)

case class UndefinedTypeVariableError(location: hydra.paths.SubtermPath, name: hydra.core.Name)

case class VoidInNonBottomPositionError(location: hydra.paths.SubtermPath)

enum InvalidTypeError :
   case duplicateRecordTypeFieldNames(value: hydra.error.core.DuplicateRecordTypeFieldNamesError) extends InvalidTypeError
   case duplicateUnionTypeFieldNames(value: hydra.error.core.DuplicateUnionTypeFieldNamesError) extends InvalidTypeError
   case emptyRecordType(value: hydra.error.core.EmptyRecordTypeError) extends InvalidTypeError
   case emptyTypeAnnotation(value: hydra.error.core.EmptyTypeAnnotationError) extends InvalidTypeError
   case emptyUnionType(value: hydra.error.core.EmptyUnionTypeError) extends InvalidTypeError
   case invalidForallParameterName(value: hydra.error.core.InvalidForallParameterNameError) extends InvalidTypeError
   case invalidTypeSchemeVariableName(value: hydra.error.core.InvalidTypeSchemeVariableNameError) extends InvalidTypeError
   case nestedTypeAnnotation(value: hydra.error.core.NestedTypeAnnotationError) extends InvalidTypeError
   case nonComparableMapKeyType(value: hydra.error.core.NonComparableMapKeyTypeError) extends InvalidTypeError
   case nonComparableSetElementType(value: hydra.error.core.NonComparableSetElementTypeError) extends InvalidTypeError
   case singleVariantUnion(value: hydra.error.core.SingleVariantUnionError) extends InvalidTypeError
   case typeVariableShadowingInForall(value: hydra.error.core.TypeVariableShadowingInForallError) extends InvalidTypeError
   case undefinedTypeVariable(value: hydra.error.core.UndefinedTypeVariableError) extends InvalidTypeError
   case voidInNonBottomPosition(value: hydra.error.core.VoidInNonBottomPositionError) extends InvalidTypeError
