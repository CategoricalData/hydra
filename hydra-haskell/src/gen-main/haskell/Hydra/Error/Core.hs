-- Note: this is an automatically generated file. Do not edit.

-- | Error types for core type and term validation

module Hydra.Error.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Paths as Paths
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A duplicate binding name in a let expression
data DuplicateBindingError =
  DuplicateBindingError {
    -- | The path to the duplicate binding within the term
    duplicateBindingErrorLocation :: Paths.SubtermPath,
    -- | The duplicated binding name
    duplicateBindingErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_DuplicateBindingError = Core.Name "hydra.error.core.DuplicateBindingError"

_DuplicateBindingError_location = Core.Name "location"

_DuplicateBindingError_name = Core.Name "name"

-- | A duplicate field name in a record or union type
data DuplicateFieldError =
  DuplicateFieldError {
    -- | The path to the duplicate field within the term
    duplicateFieldErrorLocation :: Paths.SubtermPath,
    -- | The duplicated field name
    duplicateFieldErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_DuplicateFieldError = Core.Name "hydra.error.core.DuplicateFieldError"

_DuplicateFieldError_location = Core.Name "location"

_DuplicateFieldError_name = Core.Name "name"

-- | A reference to a field that does not exist in the given type
data UndefinedFieldError =
  UndefinedFieldError {
    -- | The name of the undefined field
    undefinedFieldErrorFieldName :: Core.Name,
    -- | The name of the type in which the field was expected
    undefinedFieldErrorTypeName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_UndefinedFieldError = Core.Name "hydra.error.core.UndefinedFieldError"

_UndefinedFieldError_fieldName = Core.Name "fieldName"

_UndefinedFieldError_typeName = Core.Name "typeName"

-- | An unexpected term variant was encountered
data UnexpectedTermVariantError =
  UnexpectedTermVariantError {
    -- | The expected term variant
    unexpectedTermVariantErrorExpectedVariant :: Variants.TermVariant,
    -- | The actual term that was encountered
    unexpectedTermVariantErrorActualTerm :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_UnexpectedTermVariantError = Core.Name "hydra.error.core.UnexpectedTermVariantError"

_UnexpectedTermVariantError_expectedVariant = Core.Name "expectedVariant"

_UnexpectedTermVariantError_actualTerm = Core.Name "actualTerm"

-- | An unexpected type variant was encountered
data UnexpectedTypeVariantError =
  UnexpectedTypeVariantError {
    -- | The expected type variant
    unexpectedTypeVariantErrorExpectedVariant :: Variants.TypeVariant,
    -- | The actual type that was encountered
    unexpectedTypeVariantErrorActualType :: Core.Type}
  deriving (Eq, Ord, Read, Show)

_UnexpectedTypeVariantError = Core.Name "hydra.error.core.UnexpectedTypeVariantError"

_UnexpectedTypeVariantError_expectedVariant = Core.Name "expectedVariant"

_UnexpectedTypeVariantError_actualType = Core.Name "actualType"

-- | An application of ifElse where the condition is a literal boolean, creating a dead branch (optional)
data ConstantConditionError =
  ConstantConditionError {
    -- | The path to the constant condition within the term
    constantConditionErrorLocation :: Paths.SubtermPath,
    -- | The constant boolean value of the condition
    constantConditionErrorValue :: Bool}
  deriving (Eq, Ord, Read, Show)

_ConstantConditionError = Core.Name "hydra.error.core.ConstantConditionError"

_ConstantConditionError_location = Core.Name "location"

_ConstantConditionError_value = Core.Name "value"

-- | A case statement with no cases and no default (optional)
data EmptyCaseStatementError =
  EmptyCaseStatementError {
    -- | The path to the empty case statement within the term
    emptyCaseStatementErrorLocation :: Paths.SubtermPath,
    -- | The name of the union type being matched
    emptyCaseStatementErrorTypeName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_EmptyCaseStatementError = Core.Name "hydra.error.core.EmptyCaseStatementError"

_EmptyCaseStatementError_location = Core.Name "location"

_EmptyCaseStatementError_typeName = Core.Name "typeName"

-- | A let expression with an empty list of bindings (optional)
data EmptyLetBindingsError =
  EmptyLetBindingsError {
    -- | The path to the empty let expression within the term
    emptyLetBindingsErrorLocation :: Paths.SubtermPath}
  deriving (Eq, Ord, Read, Show)

_EmptyLetBindingsError = Core.Name "hydra.error.core.EmptyLetBindingsError"

_EmptyLetBindingsError_location = Core.Name "location"

-- | A term annotation with an empty annotation map (optional)
data EmptyTermAnnotationError =
  EmptyTermAnnotationError {
    -- | The path to the empty annotation within the term
    emptyTermAnnotationErrorLocation :: Paths.SubtermPath}
  deriving (Eq, Ord, Read, Show)

_EmptyTermAnnotationError = Core.Name "hydra.error.core.EmptyTermAnnotationError"

_EmptyTermAnnotationError_location = Core.Name "location"

-- | A record, injection, projection, or case statement with an empty type name (optional)
data EmptyTypeNameInTermError =
  EmptyTypeNameInTermError {
    -- | The path to the term with the empty type name
    emptyTypeNameInTermErrorLocation :: Paths.SubtermPath}
  deriving (Eq, Ord, Read, Show)

_EmptyTypeNameInTermError = Core.Name "hydra.error.core.EmptyTypeNameInTermError"

_EmptyTypeNameInTermError_location = Core.Name "location"

-- | A lambda parameter name that violates naming conventions (optional)
data InvalidLambdaParameterNameError =
  InvalidLambdaParameterNameError {
    -- | The path to the lambda within the term
    invalidLambdaParameterNameErrorLocation :: Paths.SubtermPath,
    -- | The invalid parameter name
    invalidLambdaParameterNameErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_InvalidLambdaParameterNameError = Core.Name "hydra.error.core.InvalidLambdaParameterNameError"

_InvalidLambdaParameterNameError_location = Core.Name "location"

_InvalidLambdaParameterNameError_name = Core.Name "name"

-- | A let binding name that violates naming conventions (optional)
data InvalidLetBindingNameError =
  InvalidLetBindingNameError {
    -- | The path to the binding within the term
    invalidLetBindingNameErrorLocation :: Paths.SubtermPath,
    -- | The invalid binding name
    invalidLetBindingNameErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_InvalidLetBindingNameError = Core.Name "hydra.error.core.InvalidLetBindingNameError"

_InvalidLetBindingNameError_location = Core.Name "location"

_InvalidLetBindingNameError_name = Core.Name "name"

-- | A type lambda parameter name that violates naming conventions (optional)
data InvalidTypeLambdaParameterNameError =
  InvalidTypeLambdaParameterNameError {
    -- | The path to the type lambda within the term
    invalidTypeLambdaParameterNameErrorLocation :: Paths.SubtermPath,
    -- | The invalid type lambda parameter name
    invalidTypeLambdaParameterNameErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_InvalidTypeLambdaParameterNameError = Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"

_InvalidTypeLambdaParameterNameError_location = Core.Name "location"

_InvalidTypeLambdaParameterNameError_name = Core.Name "name"

-- | A term annotation directly wrapping another term annotation; annotations should be merged (optional)
data NestedTermAnnotationError =
  NestedTermAnnotationError {
    -- | The path to the outer annotation within the term
    nestedTermAnnotationErrorLocation :: Paths.SubtermPath}
  deriving (Eq, Ord, Read, Show)

_NestedTermAnnotationError = Core.Name "hydra.error.core.NestedTermAnnotationError"

_NestedTermAnnotationError_location = Core.Name "location"

-- | An unwrap elimination applied to a wrap term of the same type, forming a no-op round-trip (optional)
data RedundantWrapUnwrapError =
  RedundantWrapUnwrapError {
    -- | The path to the redundant wrap/unwrap within the term
    redundantWrapUnwrapErrorLocation :: Paths.SubtermPath,
    -- | The type name of the wrapper
    redundantWrapUnwrapErrorTypeName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_RedundantWrapUnwrapError = Core.Name "hydra.error.core.RedundantWrapUnwrapError"

_RedundantWrapUnwrapError_location = Core.Name "location"

_RedundantWrapUnwrapError_typeName = Core.Name "typeName"

-- | A variable applied to itself, which is almost always a mistake in Hydra's type system (optional)
data SelfApplicationError =
  SelfApplicationError {
    -- | The path to the self-application within the term
    selfApplicationErrorLocation :: Paths.SubtermPath,
    -- | The name of the variable applied to itself
    selfApplicationErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_SelfApplicationError = Core.Name "hydra.error.core.SelfApplicationError"

_SelfApplicationError_location = Core.Name "location"

_SelfApplicationError_name = Core.Name "name"

-- | A lambda parameter or let binding name that shadows a variable already in scope (optional)
data TermVariableShadowingError =
  TermVariableShadowingError {
    -- | The path to the shadowing binding within the term
    termVariableShadowingErrorLocation :: Paths.SubtermPath,
    -- | The name of the shadowed variable
    termVariableShadowingErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_TermVariableShadowingError = Core.Name "hydra.error.core.TermVariableShadowingError"

_TermVariableShadowingError_location = Core.Name "location"

_TermVariableShadowingError_name = Core.Name "name"

-- | A type lambda parameter that shadows a type variable already in scope (optional)
data TypeVariableShadowingInTypeLambdaError =
  TypeVariableShadowingInTypeLambdaError {
    -- | The path to the type lambda within the term
    typeVariableShadowingInTypeLambdaErrorLocation :: Paths.SubtermPath,
    -- | The name of the shadowed type variable
    typeVariableShadowingInTypeLambdaErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_TypeVariableShadowingInTypeLambdaError = Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"

_TypeVariableShadowingInTypeLambdaError_location = Core.Name "location"

_TypeVariableShadowingInTypeLambdaError_name = Core.Name "name"

-- | A variable reference to a term name that is not bound in scope
data UndefinedTermVariableError =
  UndefinedTermVariableError {
    -- | The path to the undefined variable within the term
    undefinedTermVariableErrorLocation :: Paths.SubtermPath,
    -- | The name of the undefined variable
    undefinedTermVariableErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_UndefinedTermVariableError = Core.Name "hydra.error.core.UndefinedTermVariableError"

_UndefinedTermVariableError_location = Core.Name "location"

_UndefinedTermVariableError_name = Core.Name "name"

-- | A type variable in a let binding's type scheme that is not bound by the scheme or enclosing scope
data UndefinedTypeVariableInBindingTypeError =
  UndefinedTypeVariableInBindingTypeError {
    -- | The path to the binding within the term
    undefinedTypeVariableInBindingTypeErrorLocation :: Paths.SubtermPath,
    -- | The name of the undefined type variable
    undefinedTypeVariableInBindingTypeErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_UndefinedTypeVariableInBindingTypeError = Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"

_UndefinedTypeVariableInBindingTypeError_location = Core.Name "location"

_UndefinedTypeVariableInBindingTypeError_name = Core.Name "name"

-- | A type variable in a lambda domain annotation that is not bound in scope
data UndefinedTypeVariableInLambdaDomainError =
  UndefinedTypeVariableInLambdaDomainError {
    -- | The path to the lambda within the term
    undefinedTypeVariableInLambdaDomainErrorLocation :: Paths.SubtermPath,
    -- | The name of the undefined type variable
    undefinedTypeVariableInLambdaDomainErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_UndefinedTypeVariableInLambdaDomainError = Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"

_UndefinedTypeVariableInLambdaDomainError_location = Core.Name "location"

_UndefinedTypeVariableInLambdaDomainError_name = Core.Name "name"

-- | A type variable in a type application term that is not bound in scope
data UndefinedTypeVariableInTypeApplicationError =
  UndefinedTypeVariableInTypeApplicationError {
    -- | The path to the type application within the term
    undefinedTypeVariableInTypeApplicationErrorLocation :: Paths.SubtermPath,
    -- | The name of the undefined type variable
    undefinedTypeVariableInTypeApplicationErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_UndefinedTypeVariableInTypeApplicationError = Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"

_UndefinedTypeVariableInTypeApplicationError_location = Core.Name "location"

_UndefinedTypeVariableInTypeApplicationError_name = Core.Name "name"

-- | A primitive function reference to a name not in the known primitive registry
data UnknownPrimitiveNameError =
  UnknownPrimitiveNameError {
    -- | The path to the primitive reference within the term
    unknownPrimitiveNameErrorLocation :: Paths.SubtermPath,
    -- | The unknown primitive name
    unknownPrimitiveNameErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_UnknownPrimitiveNameError = Core.Name "hydra.error.core.UnknownPrimitiveNameError"

_UnknownPrimitiveNameError_location = Core.Name "location"

_UnknownPrimitiveNameError_name = Core.Name "name"

-- | An application of an identity lambda to an argument, which simplifies to the argument (optional)
data UnnecessaryIdentityApplicationError =
  UnnecessaryIdentityApplicationError {
    -- | The path to the identity application within the term
    unnecessaryIdentityApplicationErrorLocation :: Paths.SubtermPath}
  deriving (Eq, Ord, Read, Show)

_UnnecessaryIdentityApplicationError = Core.Name "hydra.error.core.UnnecessaryIdentityApplicationError"

_UnnecessaryIdentityApplicationError_location = Core.Name "location"

-- | A term variable whose type is not known in the current scope
data UntypedTermVariableError =
  UntypedTermVariableError {
    -- | The path to the untyped variable within the term
    untypedTermVariableErrorLocation :: Paths.SubtermPath,
    -- | The name of the untyped variable
    untypedTermVariableErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_UntypedTermVariableError = Core.Name "hydra.error.core.UntypedTermVariableError"

_UntypedTermVariableError_location = Core.Name "location"

_UntypedTermVariableError_name = Core.Name "name"

-- | An error indicating that a term is invalid
data InvalidTermError =
  -- | An ifElse with a literal boolean condition (optional)
  InvalidTermErrorConstantCondition ConstantConditionError |
  -- | A duplicate binding name in a let expression
  InvalidTermErrorDuplicateBinding DuplicateBindingError |
  -- | A duplicate field name in a record or case statement
  InvalidTermErrorDuplicateField DuplicateFieldError |
  -- | A case statement with no cases and no default (optional)
  InvalidTermErrorEmptyCaseStatement EmptyCaseStatementError |
  -- | A let expression with no bindings (optional)
  InvalidTermErrorEmptyLetBindings EmptyLetBindingsError |
  -- | A term annotation with an empty annotation map (optional)
  InvalidTermErrorEmptyTermAnnotation EmptyTermAnnotationError |
  -- | A term with an empty type name (optional)
  InvalidTermErrorEmptyTypeNameInTerm EmptyTypeNameInTermError |
  -- | A lambda parameter name violating naming conventions (optional)
  InvalidTermErrorInvalidLambdaParameterName InvalidLambdaParameterNameError |
  -- | A let binding name violating naming conventions (optional)
  InvalidTermErrorInvalidLetBindingName InvalidLetBindingNameError |
  -- | A type lambda parameter name violating naming conventions (optional)
  InvalidTermErrorInvalidTypeLambdaParameterName InvalidTypeLambdaParameterNameError |
  -- | Nested term annotations that should be merged (optional)
  InvalidTermErrorNestedTermAnnotation NestedTermAnnotationError |
  -- | A no-op unwrap-of-wrap round-trip (optional)
  InvalidTermErrorRedundantWrapUnwrap RedundantWrapUnwrapError |
  -- | A variable applied to itself (optional)
  InvalidTermErrorSelfApplication SelfApplicationError |
  -- | A binding that shadows a variable already in scope (optional)
  InvalidTermErrorTermVariableShadowing TermVariableShadowingError |
  -- | A type lambda parameter that shadows a type variable in scope (optional)
  InvalidTermErrorTypeVariableShadowingInTypeLambda TypeVariableShadowingInTypeLambdaError |
  -- | A variable reference to an unbound term name
  InvalidTermErrorUndefinedTermVariable UndefinedTermVariableError |
  -- | An unbound type variable in a let binding's type scheme
  InvalidTermErrorUndefinedTypeVariableInBindingType UndefinedTypeVariableInBindingTypeError |
  -- | An unbound type variable in a lambda domain annotation
  InvalidTermErrorUndefinedTypeVariableInLambdaDomain UndefinedTypeVariableInLambdaDomainError |
  -- | An unbound type variable in a type application term
  InvalidTermErrorUndefinedTypeVariableInTypeApplication UndefinedTypeVariableInTypeApplicationError |
  -- | A reference to an unknown primitive function
  InvalidTermErrorUnknownPrimitiveName UnknownPrimitiveNameError |
  -- | An identity lambda applied to an argument (optional)
  InvalidTermErrorUnnecessaryIdentityApplication UnnecessaryIdentityApplicationError |
  -- | A term variable whose type is not known
  InvalidTermErrorUntypedTermVariable UntypedTermVariableError
  deriving (Eq, Ord, Read, Show)

_InvalidTermError = Core.Name "hydra.error.core.InvalidTermError"

_InvalidTermError_constantCondition = Core.Name "constantCondition"

_InvalidTermError_duplicateBinding = Core.Name "duplicateBinding"

_InvalidTermError_duplicateField = Core.Name "duplicateField"

_InvalidTermError_emptyCaseStatement = Core.Name "emptyCaseStatement"

_InvalidTermError_emptyLetBindings = Core.Name "emptyLetBindings"

_InvalidTermError_emptyTermAnnotation = Core.Name "emptyTermAnnotation"

_InvalidTermError_emptyTypeNameInTerm = Core.Name "emptyTypeNameInTerm"

_InvalidTermError_invalidLambdaParameterName = Core.Name "invalidLambdaParameterName"

_InvalidTermError_invalidLetBindingName = Core.Name "invalidLetBindingName"

_InvalidTermError_invalidTypeLambdaParameterName = Core.Name "invalidTypeLambdaParameterName"

_InvalidTermError_nestedTermAnnotation = Core.Name "nestedTermAnnotation"

_InvalidTermError_redundantWrapUnwrap = Core.Name "redundantWrapUnwrap"

_InvalidTermError_selfApplication = Core.Name "selfApplication"

_InvalidTermError_termVariableShadowing = Core.Name "termVariableShadowing"

_InvalidTermError_typeVariableShadowingInTypeLambda = Core.Name "typeVariableShadowingInTypeLambda"

_InvalidTermError_undefinedTermVariable = Core.Name "undefinedTermVariable"

_InvalidTermError_undefinedTypeVariableInBindingType = Core.Name "undefinedTypeVariableInBindingType"

_InvalidTermError_undefinedTypeVariableInLambdaDomain = Core.Name "undefinedTypeVariableInLambdaDomain"

_InvalidTermError_undefinedTypeVariableInTypeApplication = Core.Name "undefinedTypeVariableInTypeApplication"

_InvalidTermError_unknownPrimitiveName = Core.Name "unknownPrimitiveName"

_InvalidTermError_unnecessaryIdentityApplication = Core.Name "unnecessaryIdentityApplication"

_InvalidTermError_untypedTermVariable = Core.Name "untypedTermVariable"

-- | A record type with duplicate field names
data DuplicateRecordTypeFieldNamesError =
  DuplicateRecordTypeFieldNamesError {
    -- | The path to the record type with duplicate fields
    duplicateRecordTypeFieldNamesErrorLocation :: Paths.SubtermPath,
    -- | The duplicated field name
    duplicateRecordTypeFieldNamesErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_DuplicateRecordTypeFieldNamesError = Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"

_DuplicateRecordTypeFieldNamesError_location = Core.Name "location"

_DuplicateRecordTypeFieldNamesError_name = Core.Name "name"

-- | A union type with duplicate field names
data DuplicateUnionTypeFieldNamesError =
  DuplicateUnionTypeFieldNamesError {
    -- | The path to the union type with duplicate fields
    duplicateUnionTypeFieldNamesErrorLocation :: Paths.SubtermPath,
    -- | The duplicated field name
    duplicateUnionTypeFieldNamesErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_DuplicateUnionTypeFieldNamesError = Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"

_DuplicateUnionTypeFieldNamesError_location = Core.Name "location"

_DuplicateUnionTypeFieldNamesError_name = Core.Name "name"

-- | A record type with no fields; TypeUnit is preferred for the unit-like case (optional)
data EmptyRecordTypeError =
  EmptyRecordTypeError {
    -- | The path to the empty record type
    emptyRecordTypeErrorLocation :: Paths.SubtermPath}
  deriving (Eq, Ord, Read, Show)

_EmptyRecordTypeError = Core.Name "hydra.error.core.EmptyRecordTypeError"

_EmptyRecordTypeError_location = Core.Name "location"

-- | A type annotation with an empty annotation map (optional)
data EmptyTypeAnnotationError =
  EmptyTypeAnnotationError {
    -- | The path to the empty annotation
    emptyTypeAnnotationErrorLocation :: Paths.SubtermPath}
  deriving (Eq, Ord, Read, Show)

_EmptyTypeAnnotationError = Core.Name "hydra.error.core.EmptyTypeAnnotationError"

_EmptyTypeAnnotationError_location = Core.Name "location"

-- | A union type with no alternatives; TypeVoid is preferred (optional)
data EmptyUnionTypeError =
  EmptyUnionTypeError {
    -- | The path to the empty union type
    emptyUnionTypeErrorLocation :: Paths.SubtermPath}
  deriving (Eq, Ord, Read, Show)

_EmptyUnionTypeError = Core.Name "hydra.error.core.EmptyUnionTypeError"

_EmptyUnionTypeError_location = Core.Name "location"

-- | A forall type parameter name that violates type variable naming conventions (optional)
data InvalidForallParameterNameError =
  InvalidForallParameterNameError {
    -- | The path to the forall type
    invalidForallParameterNameErrorLocation :: Paths.SubtermPath,
    -- | The invalid parameter name
    invalidForallParameterNameErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_InvalidForallParameterNameError = Core.Name "hydra.error.core.InvalidForallParameterNameError"

_InvalidForallParameterNameError_location = Core.Name "location"

_InvalidForallParameterNameError_name = Core.Name "name"

-- | A type scheme variable name that violates type variable naming conventions (optional)
data InvalidTypeSchemeVariableNameError =
  InvalidTypeSchemeVariableNameError {
    -- | The path to the type scheme
    invalidTypeSchemeVariableNameErrorLocation :: Paths.SubtermPath,
    -- | The invalid variable name
    invalidTypeSchemeVariableNameErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_InvalidTypeSchemeVariableNameError = Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"

_InvalidTypeSchemeVariableNameError_location = Core.Name "location"

_InvalidTypeSchemeVariableNameError_name = Core.Name "name"

-- | A type annotation directly wrapping another type annotation; annotations should be merged (optional)
data NestedTypeAnnotationError =
  NestedTypeAnnotationError {
    -- | The path to the outer annotation
    nestedTypeAnnotationErrorLocation :: Paths.SubtermPath}
  deriving (Eq, Ord, Read, Show)

_NestedTypeAnnotationError = Core.Name "hydra.error.core.NestedTypeAnnotationError"

_NestedTypeAnnotationError_location = Core.Name "location"

-- | A map type whose key type is or directly contains a function type, which cannot be compared for equality
data NonComparableMapKeyTypeError =
  NonComparableMapKeyTypeError {
    -- | The path to the map type
    nonComparableMapKeyTypeErrorLocation :: Paths.SubtermPath,
    -- | The non-comparable key type
    nonComparableMapKeyTypeErrorKeyType :: Core.Type}
  deriving (Eq, Ord, Read, Show)

_NonComparableMapKeyTypeError = Core.Name "hydra.error.core.NonComparableMapKeyTypeError"

_NonComparableMapKeyTypeError_location = Core.Name "location"

_NonComparableMapKeyTypeError_keyType = Core.Name "keyType"

-- | A set type whose element type is or directly contains a function type, which cannot be compared for equality
data NonComparableSetElementTypeError =
  NonComparableSetElementTypeError {
    -- | The path to the set type
    nonComparableSetElementTypeErrorLocation :: Paths.SubtermPath,
    -- | The non-comparable element type
    nonComparableSetElementTypeErrorElementType :: Core.Type}
  deriving (Eq, Ord, Read, Show)

_NonComparableSetElementTypeError = Core.Name "hydra.error.core.NonComparableSetElementTypeError"

_NonComparableSetElementTypeError_location = Core.Name "location"

_NonComparableSetElementTypeError_elementType = Core.Name "elementType"

-- | A union type with exactly one field; could be a wrapped type or record instead (optional)
data SingleVariantUnionError =
  SingleVariantUnionError {
    -- | The path to the single-variant union type
    singleVariantUnionErrorLocation :: Paths.SubtermPath,
    -- | The name of the single field
    singleVariantUnionErrorFieldName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_SingleVariantUnionError = Core.Name "hydra.error.core.SingleVariantUnionError"

_SingleVariantUnionError_location = Core.Name "location"

_SingleVariantUnionError_fieldName = Core.Name "fieldName"

-- | A forall type parameter that shadows a type variable already in scope (optional)
data TypeVariableShadowingInForallError =
  TypeVariableShadowingInForallError {
    -- | The path to the shadowing forall type
    typeVariableShadowingInForallErrorLocation :: Paths.SubtermPath,
    -- | The name of the shadowed type variable
    typeVariableShadowingInForallErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_TypeVariableShadowingInForallError = Core.Name "hydra.error.core.TypeVariableShadowingInForallError"

_TypeVariableShadowingInForallError_location = Core.Name "location"

_TypeVariableShadowingInForallError_name = Core.Name "name"

-- | A type variable reference to a name that is not bound in scope
data UndefinedTypeVariableError =
  UndefinedTypeVariableError {
    -- | The path to the undefined type variable
    undefinedTypeVariableErrorLocation :: Paths.SubtermPath,
    -- | The name of the undefined type variable
    undefinedTypeVariableErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_UndefinedTypeVariableError = Core.Name "hydra.error.core.UndefinedTypeVariableError"

_UndefinedTypeVariableError_location = Core.Name "location"

_UndefinedTypeVariableError_name = Core.Name "name"

-- | TypeVoid appearing in a position where no value can be constructed, such as a record field, list element, map key/value, set element, pair component, or function codomain (optional)
data VoidInNonBottomPositionError =
  VoidInNonBottomPositionError {
    -- | The path to the void type in a non-bottom position
    voidInNonBottomPositionErrorLocation :: Paths.SubtermPath}
  deriving (Eq, Ord, Read, Show)

_VoidInNonBottomPositionError = Core.Name "hydra.error.core.VoidInNonBottomPositionError"

_VoidInNonBottomPositionError_location = Core.Name "location"

-- | An error indicating that a type is invalid
data InvalidTypeError =
  -- | A record type with duplicate field names
  InvalidTypeErrorDuplicateRecordTypeFieldNames DuplicateRecordTypeFieldNamesError |
  -- | A union type with duplicate field names
  InvalidTypeErrorDuplicateUnionTypeFieldNames DuplicateUnionTypeFieldNamesError |
  -- | A record type with no fields (optional)
  InvalidTypeErrorEmptyRecordType EmptyRecordTypeError |
  -- | A type annotation with an empty annotation map (optional)
  InvalidTypeErrorEmptyTypeAnnotation EmptyTypeAnnotationError |
  -- | A union type with no alternatives (optional)
  InvalidTypeErrorEmptyUnionType EmptyUnionTypeError |
  -- | A forall parameter name violating naming conventions (optional)
  InvalidTypeErrorInvalidForallParameterName InvalidForallParameterNameError |
  -- | A type scheme variable name violating naming conventions (optional)
  InvalidTypeErrorInvalidTypeSchemeVariableName InvalidTypeSchemeVariableNameError |
  -- | Nested type annotations that should be merged (optional)
  InvalidTypeErrorNestedTypeAnnotation NestedTypeAnnotationError |
  -- | A map with a non-comparable key type
  InvalidTypeErrorNonComparableMapKeyType NonComparableMapKeyTypeError |
  -- | A set with a non-comparable element type
  InvalidTypeErrorNonComparableSetElementType NonComparableSetElementTypeError |
  -- | A union type with only one variant (optional)
  InvalidTypeErrorSingleVariantUnion SingleVariantUnionError |
  -- | A forall parameter that shadows a type variable in scope (optional)
  InvalidTypeErrorTypeVariableShadowingInForall TypeVariableShadowingInForallError |
  -- | A type variable reference to an unbound name
  InvalidTypeErrorUndefinedTypeVariable UndefinedTypeVariableError |
  -- | TypeVoid in a position where no value can be constructed (optional)
  InvalidTypeErrorVoidInNonBottomPosition VoidInNonBottomPositionError
  deriving (Eq, Ord, Read, Show)

_InvalidTypeError = Core.Name "hydra.error.core.InvalidTypeError"

_InvalidTypeError_duplicateRecordTypeFieldNames = Core.Name "duplicateRecordTypeFieldNames"

_InvalidTypeError_duplicateUnionTypeFieldNames = Core.Name "duplicateUnionTypeFieldNames"

_InvalidTypeError_emptyRecordType = Core.Name "emptyRecordType"

_InvalidTypeError_emptyTypeAnnotation = Core.Name "emptyTypeAnnotation"

_InvalidTypeError_emptyUnionType = Core.Name "emptyUnionType"

_InvalidTypeError_invalidForallParameterName = Core.Name "invalidForallParameterName"

_InvalidTypeError_invalidTypeSchemeVariableName = Core.Name "invalidTypeSchemeVariableName"

_InvalidTypeError_nestedTypeAnnotation = Core.Name "nestedTypeAnnotation"

_InvalidTypeError_nonComparableMapKeyType = Core.Name "nonComparableMapKeyType"

_InvalidTypeError_nonComparableSetElementType = Core.Name "nonComparableSetElementType"

_InvalidTypeError_singleVariantUnion = Core.Name "singleVariantUnion"

_InvalidTypeError_typeVariableShadowingInForall = Core.Name "typeVariableShadowingInForall"

_InvalidTypeError_undefinedTypeVariable = Core.Name "undefinedTypeVariable"

_InvalidTypeError_voidInNonBottomPosition = Core.Name "voidInNonBottomPosition"
