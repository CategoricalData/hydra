module Hydra.Sources.Kernel.Types.Error.Core where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Paths as Paths
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Variants as Variants


ns :: Namespace
ns = Namespace "hydra.error.core"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns (map toTypeDef definitions) [Paths.ns, Core.ns, Variants.ns] [Paths.ns, Core.ns, Variants.ns] $
    Just "Error types for core type and term validation"
  where
    definitions = [
      -- Existing error types (pre-linter)
      duplicateBindingError,
      duplicateFieldError,
      undefinedFieldError,
      unexpectedTermVariantError,
      unexpectedTypeVariantError,

      -- Term validation errors
      constantConditionError,
      emptyCaseStatementError,
      emptyLetBindingsError,
      emptyTermAnnotationError,
      emptyTypeNameInTermError,
      invalidLambdaParameterNameError,
      invalidLetBindingNameError,
      invalidTypeLambdaParameterNameError,
      nestedTermAnnotationError,
      redundantWrapUnwrapError,
      selfApplicationError,
      termVariableShadowingError,
      typeVariableShadowingInTypeLambdaError,
      undefinedTermVariableError,
      undefinedTypeVariableInBindingTypeError,
      undefinedTypeVariableInLambdaDomainError,
      undefinedTypeVariableInTypeApplicationError,
      unknownPrimitiveNameError,
      unnecessaryIdentityApplicationError,
      untypedTermVariableError,
      invalidTermError,

      -- Type validation errors
      duplicateRecordTypeFieldNamesError,
      duplicateUnionTypeFieldNamesError,
      emptyRecordTypeError,
      emptyTypeAnnotationError,
      emptyUnionTypeError,
      invalidForallParameterNameError,
      invalidTypeSchemeVariableNameError,
      nestedTypeAnnotationError,
      nonComparableMapKeyTypeError,
      nonComparableSetElementTypeError,
      singleVariantUnionError,
      typeVariableShadowingInForallError,
      undefinedTypeVariableError,
      voidInNonBottomPositionError,
      invalidTypeError]

-- ============================================================================
-- Existing error types (pre-linter)
-- ============================================================================

duplicateBindingError :: Binding
duplicateBindingError = define "DuplicateBindingError" $
  doc "A duplicate binding name in a let expression" $
  T.record [
    "location">:
      doc "The path to the duplicate binding within the term" $
      Paths.subtermPath,
    "name">:
      doc "The duplicated binding name" $
      Core.name]

duplicateFieldError :: Binding
duplicateFieldError = define "DuplicateFieldError" $
  doc "A duplicate field name in a record or union type" $
  T.record [
    "location">:
      doc "The path to the duplicate field within the term" $
      Paths.subtermPath,
    "name">:
      doc "The duplicated field name" $
      Core.name]

undefinedFieldError :: Binding
undefinedFieldError = define "UndefinedFieldError" $
  doc "A reference to a field that does not exist in the given type" $
  T.record [
    "fieldName">:
      doc "The name of the undefined field" $
      Core.name,
    "typeName">:
      doc "The name of the type in which the field was expected" $
      Core.name]

unexpectedTermVariantError :: Binding
unexpectedTermVariantError = define "UnexpectedTermVariantError" $
  doc "An unexpected term variant was encountered" $
  T.record [
    "expectedVariant">:
      doc "The expected term variant" $
      Variants.termVariant,
    "actualTerm">:
      doc "The actual term that was encountered" $
      Core.term]

unexpectedTypeVariantError :: Binding
unexpectedTypeVariantError = define "UnexpectedTypeVariantError" $
  doc "An unexpected type variant was encountered" $
  T.record [
    "expectedVariant">:
      doc "The expected type variant" $
      Variants.typeVariant,
    "actualType">:
      doc "The actual type that was encountered" $
      Core.type_]

-- ============================================================================
-- Term validation errors
-- ============================================================================

-- T1. EmptyLetBindingsError (optional)
emptyLetBindingsError :: Binding
emptyLetBindingsError = define "EmptyLetBindingsError" $
  doc "A let expression with an empty list of bindings (optional)" $
  T.record [
    "location">:
      doc "The path to the empty let expression within the term" $
      Paths.subtermPath]

-- T2. DuplicateLetBindingNamesError — covered by existing DuplicateBindingError

-- T3. DuplicateRecordFieldNamesError — covered by existing DuplicateFieldError (for terms)

-- T4. DuplicateCaseStatementFieldNamesError — covered by existing DuplicateFieldError (for terms)

-- T5. EmptyTypeNameInTermError (optional)
emptyTypeNameInTermError :: Binding
emptyTypeNameInTermError = define "EmptyTypeNameInTermError" $
  doc "A record, injection, projection, or case statement with an empty type name (optional)" $
  T.record [
    "location">:
      doc "The path to the term with the empty type name" $
      Paths.subtermPath]

-- T6. EmptyCaseStatementError (optional)
emptyCaseStatementError :: Binding
emptyCaseStatementError = define "EmptyCaseStatementError" $
  doc "A case statement with no cases and no default (optional)" $
  T.record [
    "location">:
      doc "The path to the empty case statement within the term" $
      Paths.subtermPath,
    "typeName">:
      doc "The name of the union type being matched" $
      Core.name]

-- T7. UndefinedTermVariableError (replaces UndefinedTermError)
undefinedTermVariableError :: Binding
undefinedTermVariableError = define "UndefinedTermVariableError" $
  doc "A variable reference to a term name that is not bound in scope" $
  T.record [
    "location">:
      doc "The path to the undefined variable within the term" $
      Paths.subtermPath,
    "name">:
      doc "The name of the undefined variable" $
      Core.name]

-- T8. UndefinedTypeVariableInLambdaDomainError
undefinedTypeVariableInLambdaDomainError :: Binding
undefinedTypeVariableInLambdaDomainError = define "UndefinedTypeVariableInLambdaDomainError" $
  doc "A type variable in a lambda domain annotation that is not bound in scope" $
  T.record [
    "location">:
      doc "The path to the lambda within the term" $
      Paths.subtermPath,
    "name">:
      doc "The name of the undefined type variable" $
      Core.name]

-- T9. UndefinedTypeVariableInTypeApplicationError
undefinedTypeVariableInTypeApplicationError :: Binding
undefinedTypeVariableInTypeApplicationError = define "UndefinedTypeVariableInTypeApplicationError" $
  doc "A type variable in a type application term that is not bound in scope" $
  T.record [
    "location">:
      doc "The path to the type application within the term" $
      Paths.subtermPath,
    "name">:
      doc "The name of the undefined type variable" $
      Core.name]

-- T10. UndefinedTypeVariableInBindingTypeError
undefinedTypeVariableInBindingTypeError :: Binding
undefinedTypeVariableInBindingTypeError = define "UndefinedTypeVariableInBindingTypeError" $
  doc "A type variable in a let binding's type scheme that is not bound by the scheme or enclosing scope" $
  T.record [
    "location">:
      doc "The path to the binding within the term" $
      Paths.subtermPath,
    "name">:
      doc "The name of the undefined type variable" $
      Core.name]

-- T11. TermVariableShadowingError (optional)
termVariableShadowingError :: Binding
termVariableShadowingError = define "TermVariableShadowingError" $
  doc "A lambda parameter or let binding name that shadows a variable already in scope (optional)" $
  T.record [
    "location">:
      doc "The path to the shadowing binding within the term" $
      Paths.subtermPath,
    "name">:
      doc "The name of the shadowed variable" $
      Core.name]

-- T12. TypeVariableShadowingInTypeLambdaError (optional)
typeVariableShadowingInTypeLambdaError :: Binding
typeVariableShadowingInTypeLambdaError = define "TypeVariableShadowingInTypeLambdaError" $
  doc "A type lambda parameter that shadows a type variable already in scope (optional)" $
  T.record [
    "location">:
      doc "The path to the type lambda within the term" $
      Paths.subtermPath,
    "name">:
      doc "The name of the shadowed type variable" $
      Core.name]

-- T13. ConstantConditionError (optional)
constantConditionError :: Binding
constantConditionError = define "ConstantConditionError" $
  doc "An application of ifElse where the condition is a literal boolean, creating a dead branch (optional)" $
  T.record [
    "location">:
      doc "The path to the constant condition within the term" $
      Paths.subtermPath,
    "value">:
      doc "The constant boolean value of the condition" $
      T.boolean]

-- T14. RedundantWrapUnwrapError (optional)
redundantWrapUnwrapError :: Binding
redundantWrapUnwrapError = define "RedundantWrapUnwrapError" $
  doc "An unwrap elimination applied to a wrap term of the same type, forming a no-op round-trip (optional)" $
  T.record [
    "location">:
      doc "The path to the redundant wrap/unwrap within the term" $
      Paths.subtermPath,
    "typeName">:
      doc "The type name of the wrapper" $
      Core.name]

-- T15. SelfApplicationError (optional)
selfApplicationError :: Binding
selfApplicationError = define "SelfApplicationError" $
  doc "A variable applied to itself, which is almost always a mistake in Hydra's type system (optional)" $
  T.record [
    "location">:
      doc "The path to the self-application within the term" $
      Paths.subtermPath,
    "name">:
      doc "The name of the variable applied to itself" $
      Core.name]

-- T16. UnnecessaryIdentityApplicationError (optional)
unnecessaryIdentityApplicationError :: Binding
unnecessaryIdentityApplicationError = define "UnnecessaryIdentityApplicationError" $
  doc "An application of an identity lambda to an argument, which simplifies to the argument (optional)" $
  T.record [
    "location">:
      doc "The path to the identity application within the term" $
      Paths.subtermPath]

-- T17. InvalidLambdaParameterNameError (optional)
invalidLambdaParameterNameError :: Binding
invalidLambdaParameterNameError = define "InvalidLambdaParameterNameError" $
  doc "A lambda parameter name that violates naming conventions (optional)" $
  T.record [
    "location">:
      doc "The path to the lambda within the term" $
      Paths.subtermPath,
    "name">:
      doc "The invalid parameter name" $
      Core.name]

-- T18. InvalidLetBindingNameError (optional)
invalidLetBindingNameError :: Binding
invalidLetBindingNameError = define "InvalidLetBindingNameError" $
  doc "A let binding name that violates naming conventions (optional)" $
  T.record [
    "location">:
      doc "The path to the binding within the term" $
      Paths.subtermPath,
    "name">:
      doc "The invalid binding name" $
      Core.name]

-- T19. InvalidTypeLambdaParameterNameError (optional)
invalidTypeLambdaParameterNameError :: Binding
invalidTypeLambdaParameterNameError = define "InvalidTypeLambdaParameterNameError" $
  doc "A type lambda parameter name that violates naming conventions (optional)" $
  T.record [
    "location">:
      doc "The path to the type lambda within the term" $
      Paths.subtermPath,
    "name">:
      doc "The invalid type lambda parameter name" $
      Core.name]

-- T20. NestedTermAnnotationError (optional)
nestedTermAnnotationError :: Binding
nestedTermAnnotationError = define "NestedTermAnnotationError" $
  doc "A term annotation directly wrapping another term annotation; annotations should be merged (optional)" $
  T.record [
    "location">:
      doc "The path to the outer annotation within the term" $
      Paths.subtermPath]

-- T21. EmptyTermAnnotationError (optional)
emptyTermAnnotationError :: Binding
emptyTermAnnotationError = define "EmptyTermAnnotationError" $
  doc "A term annotation with an empty annotation map (optional)" $
  T.record [
    "location">:
      doc "The path to the empty annotation within the term" $
      Paths.subtermPath]

-- T22. UnknownPrimitiveNameError
unknownPrimitiveNameError :: Binding
unknownPrimitiveNameError = define "UnknownPrimitiveNameError" $
  doc "A primitive function reference to a name not in the known primitive registry" $
  T.record [
    "location">:
      doc "The path to the primitive reference within the term" $
      Paths.subtermPath,
    "name">:
      doc "The unknown primitive name" $
      Core.name]

-- UntypedTermVariableError (replaces UndefinedTypeError)
untypedTermVariableError :: Binding
untypedTermVariableError = define "UntypedTermVariableError" $
  doc "A term variable whose type is not known in the current scope" $
  T.record [
    "location">:
      doc "The path to the untyped variable within the term" $
      Paths.subtermPath,
    "name">:
      doc "The name of the untyped variable" $
      Core.name]

-- InvalidTermError: the union of all term validation errors
invalidTermError :: Binding
invalidTermError = define "InvalidTermError" $
  doc "An error indicating that a term is invalid" $
  T.union [
    "constantCondition">:
      doc "An ifElse with a literal boolean condition (optional)" $
      constantConditionError,
    "duplicateBinding">:
      doc "A duplicate binding name in a let expression" $
      duplicateBindingError,
    "duplicateField">:
      doc "A duplicate field name in a record or case statement" $
      duplicateFieldError,
    "emptyCaseStatement">:
      doc "A case statement with no cases and no default (optional)" $
      emptyCaseStatementError,
    "emptyLetBindings">:
      doc "A let expression with no bindings (optional)" $
      emptyLetBindingsError,
    "emptyTermAnnotation">:
      doc "A term annotation with an empty annotation map (optional)" $
      emptyTermAnnotationError,
    "emptyTypeNameInTerm">:
      doc "A term with an empty type name (optional)" $
      emptyTypeNameInTermError,
    "invalidLambdaParameterName">:
      doc "A lambda parameter name violating naming conventions (optional)" $
      invalidLambdaParameterNameError,
    "invalidLetBindingName">:
      doc "A let binding name violating naming conventions (optional)" $
      invalidLetBindingNameError,
    "invalidTypeLambdaParameterName">:
      doc "A type lambda parameter name violating naming conventions (optional)" $
      invalidTypeLambdaParameterNameError,
    "nestedTermAnnotation">:
      doc "Nested term annotations that should be merged (optional)" $
      nestedTermAnnotationError,
    "redundantWrapUnwrap">:
      doc "A no-op unwrap-of-wrap round-trip (optional)" $
      redundantWrapUnwrapError,
    "selfApplication">:
      doc "A variable applied to itself (optional)" $
      selfApplicationError,
    "termVariableShadowing">:
      doc "A binding that shadows a variable already in scope (optional)" $
      termVariableShadowingError,
    "typeVariableShadowingInTypeLambda">:
      doc "A type lambda parameter that shadows a type variable in scope (optional)" $
      typeVariableShadowingInTypeLambdaError,
    "undefinedTermVariable">:
      doc "A variable reference to an unbound term name" $
      undefinedTermVariableError,
    "undefinedTypeVariableInBindingType">:
      doc "An unbound type variable in a let binding's type scheme" $
      undefinedTypeVariableInBindingTypeError,
    "undefinedTypeVariableInLambdaDomain">:
      doc "An unbound type variable in a lambda domain annotation" $
      undefinedTypeVariableInLambdaDomainError,
    "undefinedTypeVariableInTypeApplication">:
      doc "An unbound type variable in a type application term" $
      undefinedTypeVariableInTypeApplicationError,
    "unknownPrimitiveName">:
      doc "A reference to an unknown primitive function" $
      unknownPrimitiveNameError,
    "unnecessaryIdentityApplication">:
      doc "An identity lambda applied to an argument (optional)" $
      unnecessaryIdentityApplicationError,
    "untypedTermVariable">:
      doc "A term variable whose type is not known" $
      untypedTermVariableError]

-- ============================================================================
-- Type validation errors
-- ============================================================================

-- Y1. EmptyRecordTypeError (optional)
emptyRecordTypeError :: Binding
emptyRecordTypeError = define "EmptyRecordTypeError" $
  doc "A record type with no fields; TypeUnit is preferred for the unit-like case (optional)" $
  T.record [
    "location">:
      doc "The path to the empty record type" $
      Paths.subtermPath]

-- Y2. EmptyUnionTypeError (optional)
emptyUnionTypeError :: Binding
emptyUnionTypeError = define "EmptyUnionTypeError" $
  doc "A union type with no alternatives; TypeVoid is preferred (optional)" $
  T.record [
    "location">:
      doc "The path to the empty union type" $
      Paths.subtermPath]

-- Y3. SingleVariantUnionError (optional)
singleVariantUnionError :: Binding
singleVariantUnionError = define "SingleVariantUnionError" $
  doc "A union type with exactly one field; could be a wrapped type or record instead (optional)" $
  T.record [
    "location">:
      doc "The path to the single-variant union type" $
      Paths.subtermPath,
    "fieldName">:
      doc "The name of the single field" $
      Core.name]

-- Y4. DuplicateRecordTypeFieldNamesError
duplicateRecordTypeFieldNamesError :: Binding
duplicateRecordTypeFieldNamesError = define "DuplicateRecordTypeFieldNamesError" $
  doc "A record type with duplicate field names" $
  T.record [
    "location">:
      doc "The path to the record type with duplicate fields" $
      Paths.subtermPath,
    "name">:
      doc "The duplicated field name" $
      Core.name]

-- Y5. DuplicateUnionTypeFieldNamesError
duplicateUnionTypeFieldNamesError :: Binding
duplicateUnionTypeFieldNamesError = define "DuplicateUnionTypeFieldNamesError" $
  doc "A union type with duplicate field names" $
  T.record [
    "location">:
      doc "The path to the union type with duplicate fields" $
      Paths.subtermPath,
    "name">:
      doc "The duplicated field name" $
      Core.name]

-- Y6. UndefinedTypeVariableError
undefinedTypeVariableError :: Binding
undefinedTypeVariableError = define "UndefinedTypeVariableError" $
  doc "A type variable reference to a name that is not bound in scope" $
  T.record [
    "location">:
      doc "The path to the undefined type variable" $
      Paths.subtermPath,
    "name">:
      doc "The name of the undefined type variable" $
      Core.name]

-- Y7. TypeVariableShadowingInForallError (optional)
typeVariableShadowingInForallError :: Binding
typeVariableShadowingInForallError = define "TypeVariableShadowingInForallError" $
  doc "A forall type parameter that shadows a type variable already in scope (optional)" $
  T.record [
    "location">:
      doc "The path to the shadowing forall type" $
      Paths.subtermPath,
    "name">:
      doc "The name of the shadowed type variable" $
      Core.name]

-- Y8. NestedTypeAnnotationError (optional)
nestedTypeAnnotationError :: Binding
nestedTypeAnnotationError = define "NestedTypeAnnotationError" $
  doc "A type annotation directly wrapping another type annotation; annotations should be merged (optional)" $
  T.record [
    "location">:
      doc "The path to the outer annotation" $
      Paths.subtermPath]

-- Y9. EmptyTypeAnnotationError (optional)
emptyTypeAnnotationError :: Binding
emptyTypeAnnotationError = define "EmptyTypeAnnotationError" $
  doc "A type annotation with an empty annotation map (optional)" $
  T.record [
    "location">:
      doc "The path to the empty annotation" $
      Paths.subtermPath]

-- Y10. VoidInNonBottomPositionError (optional)
voidInNonBottomPositionError :: Binding
voidInNonBottomPositionError = define "VoidInNonBottomPositionError" $
  doc "TypeVoid appearing in a position where no value can be constructed, such as a record field, list element, map key/value, set element, pair component, or function codomain (optional)" $
  T.record [
    "location">:
      doc "The path to the void type in a non-bottom position" $
      Paths.subtermPath]

-- Y11. NonComparableMapKeyTypeError
nonComparableMapKeyTypeError :: Binding
nonComparableMapKeyTypeError = define "NonComparableMapKeyTypeError" $
  doc "A map type whose key type is or directly contains a function type, which cannot be compared for equality" $
  T.record [
    "location">:
      doc "The path to the map type" $
      Paths.subtermPath,
    "keyType">:
      doc "The non-comparable key type" $
      Core.type_]

-- Y12. NonComparableSetElementTypeError
nonComparableSetElementTypeError :: Binding
nonComparableSetElementTypeError = define "NonComparableSetElementTypeError" $
  doc "A set type whose element type is or directly contains a function type, which cannot be compared for equality" $
  T.record [
    "location">:
      doc "The path to the set type" $
      Paths.subtermPath,
    "elementType">:
      doc "The non-comparable element type" $
      Core.type_]

-- Y13. InvalidForallParameterNameError (optional)
invalidForallParameterNameError :: Binding
invalidForallParameterNameError = define "InvalidForallParameterNameError" $
  doc "A forall type parameter name that violates type variable naming conventions (optional)" $
  T.record [
    "location">:
      doc "The path to the forall type" $
      Paths.subtermPath,
    "name">:
      doc "The invalid parameter name" $
      Core.name]

-- Y14. InvalidTypeSchemeVariableNameError (optional)
invalidTypeSchemeVariableNameError :: Binding
invalidTypeSchemeVariableNameError = define "InvalidTypeSchemeVariableNameError" $
  doc "A type scheme variable name that violates type variable naming conventions (optional)" $
  T.record [
    "location">:
      doc "The path to the type scheme" $
      Paths.subtermPath,
    "name">:
      doc "The invalid variable name" $
      Core.name]

-- InvalidTypeError: the union of all type validation errors
invalidTypeError :: Binding
invalidTypeError = define "InvalidTypeError" $
  doc "An error indicating that a type is invalid" $
  T.union [
    "duplicateRecordTypeFieldNames">:
      doc "A record type with duplicate field names" $
      duplicateRecordTypeFieldNamesError,
    "duplicateUnionTypeFieldNames">:
      doc "A union type with duplicate field names" $
      duplicateUnionTypeFieldNamesError,
    "emptyRecordType">:
      doc "A record type with no fields (optional)" $
      emptyRecordTypeError,
    "emptyTypeAnnotation">:
      doc "A type annotation with an empty annotation map (optional)" $
      emptyTypeAnnotationError,
    "emptyUnionType">:
      doc "A union type with no alternatives (optional)" $
      emptyUnionTypeError,
    "invalidForallParameterName">:
      doc "A forall parameter name violating naming conventions (optional)" $
      invalidForallParameterNameError,
    "invalidTypeSchemeVariableName">:
      doc "A type scheme variable name violating naming conventions (optional)" $
      invalidTypeSchemeVariableNameError,
    "nestedTypeAnnotation">:
      doc "Nested type annotations that should be merged (optional)" $
      nestedTypeAnnotationError,
    "nonComparableMapKeyType">:
      doc "A map with a non-comparable key type" $
      nonComparableMapKeyTypeError,
    "nonComparableSetElementType">:
      doc "A set with a non-comparable element type" $
      nonComparableSetElementTypeError,
    "singleVariantUnion">:
      doc "A union type with only one variant (optional)" $
      singleVariantUnionError,
    "typeVariableShadowingInForall">:
      doc "A forall parameter that shadows a type variable in scope (optional)" $
      typeVariableShadowingInForallError,
    "undefinedTypeVariable">:
      doc "A type variable reference to an unbound name" $
      undefinedTypeVariableError,
    "voidInNonBottomPosition">:
      doc "TypeVoid in a position where no value can be constructed (optional)" $
      voidInNonBottomPositionError]

-- ============================================================================
-- Deferred term checks (require more than O(1) analysis)
-- ============================================================================

-- T-D1. UnusedLetBindingError (optional)
--   Requires free-variable computation over the let body and sibling bindings.

-- T-D2. EmptyCollectionFoldError (optional)
--   Requires recognizing primitive fold application patterns across the
--   application spine (variable number of curried arguments).

-- T-D3. DeeplyNestedApplicationError (optional)
--   Requires depth tracking across chains of application terms.

-- ============================================================================
-- Deferred type checks (require more than O(1) analysis)
-- ============================================================================

-- Y-D1. UnusedForallTypeVariableError (optional)
--   Requires free type variable computation over the forall body.

-- Y-D2. NonCanonicalTypeFormError (optional)
--   Requires recognizing beta-reducible type applications (type-level lambda
--   applied to an argument).

-- Y-D3. ExcessiveTypeNestingError (optional)
--   Requires depth tracking across chains of function, application, or
--   forall types.
