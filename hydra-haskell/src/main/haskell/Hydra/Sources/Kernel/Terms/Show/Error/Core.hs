
module Hydra.Sources.Kernel.Terms.Show.Error.Core where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms hiding (
  binding, elimination, field, fields, fieldType, floatType, floatValue, function, injection, integerType,
  integerValue, lambda, literal, literalType, term, type_, typeScheme)
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Variants as ShowVariants


ns :: Namespace
ns = Namespace "hydra.show.error.core"

module_ :: Module
module_ = Module ns elements
    [ShowCore.ns, ShowVariants.ns]
    kernelTypesNamespaces $
    Just "String representations of hydra.error.core types"
  where
   elements = [
     toDefinition constantConditionError,
     toDefinition duplicateBindingError,
     toDefinition duplicateFieldError,
     toDefinition duplicateRecordTypeFieldNamesError,
     toDefinition duplicateUnionTypeFieldNamesError,
     toDefinition emptyCaseStatementError,
     toDefinition emptyLetBindingsError,
     toDefinition emptyRecordTypeError,
     toDefinition emptyTermAnnotationError,
     toDefinition emptyTypeAnnotationError,
     toDefinition emptyTypeNameInTermError,
     toDefinition emptyUnionTypeError,
     toDefinition invalidForallParameterNameError,
     toDefinition invalidLambdaParameterNameError,
     toDefinition invalidLetBindingNameError,
     toDefinition invalidTermError,
     toDefinition invalidTypeError,
     toDefinition invalidTypeLambdaParameterNameError,
     toDefinition invalidTypeSchemeVariableNameError,
     toDefinition nestedTermAnnotationError,
     toDefinition nestedTypeAnnotationError,
     toDefinition nonComparableMapKeyTypeError,
     toDefinition nonComparableSetElementTypeError,
     toDefinition redundantWrapUnwrapError,
     toDefinition selfApplicationError,
     toDefinition singleVariantUnionError,
     toDefinition termVariableShadowingError,
     toDefinition typeVariableShadowingInForallError,
     toDefinition typeVariableShadowingInTypeLambdaError,
     toDefinition undefinedFieldError,
     toDefinition undefinedTermVariableError,
     toDefinition undefinedTypeVariableError,
     toDefinition undefinedTypeVariableInBindingTypeError,
     toDefinition undefinedTypeVariableInLambdaDomainError,
     toDefinition undefinedTypeVariableInTypeApplicationError,
     toDefinition unexpectedTermVariantError,
     toDefinition unexpectedTypeVariantError,
     toDefinition unknownPrimitiveNameError,
     toDefinition unnecessaryIdentityApplicationError,
     toDefinition untypedTermVariableError,
     toDefinition voidInNonBottomPositionError]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

-- ============================================================================
-- Existing error types
-- ============================================================================

duplicateBindingError :: TTermDefinition (DuplicateBindingError -> String)
duplicateBindingError = define "duplicateBindingError" $
  doc "Show a duplicate binding error as a string" $
  "e" ~> Strings.cat $ list [
    string "duplicate binding: ",
    Core.unName $ project _DuplicateBindingError _DuplicateBindingError_name @@ var "e"]

duplicateFieldError :: TTermDefinition (DuplicateFieldError -> String)
duplicateFieldError = define "duplicateFieldError" $
  doc "Show a duplicate field error as a string" $
  "e" ~> Strings.cat $ list [
    string "duplicate field: ",
    Core.unName $ project _DuplicateFieldError _DuplicateFieldError_name @@ var "e"]

undefinedFieldError :: TTermDefinition (UndefinedFieldError -> String)
undefinedFieldError = define "undefinedFieldError" $
  doc "Show an undefined field error as a string" $
  "e" ~>
  "fname" <~ project _UndefinedFieldError _UndefinedFieldError_fieldName @@ var "e" $
  "tname" <~ project _UndefinedFieldError _UndefinedFieldError_typeName @@ var "e" $
  Strings.cat $ list [
    string "no such field \"",
    Core.unName $ var "fname",
    string "\" in type \"",
    Core.unName $ var "tname",
    string "\""]

unexpectedTermVariantError :: TTermDefinition (UnexpectedTermVariantError -> String)
unexpectedTermVariantError = define "unexpectedTermVariantError" $
  doc "Show an unexpected term variant error as a string" $
  "e" ~>
  "expected" <~ project _UnexpectedTermVariantError _UnexpectedTermVariantError_expectedVariant @@ var "e" $
  "actual" <~ project _UnexpectedTermVariantError _UnexpectedTermVariantError_actualTerm @@ var "e" $
  Strings.cat $ list [
    string "expected ",
    ShowVariants.termVariant @@ var "expected",
    string " term but found ",
    ShowCore.term @@ var "actual"]

unexpectedTypeVariantError :: TTermDefinition (UnexpectedTypeVariantError -> String)
unexpectedTypeVariantError = define "unexpectedTypeVariantError" $
  doc "Show an unexpected type variant error as a string" $
  "e" ~>
  "expected" <~ project _UnexpectedTypeVariantError _UnexpectedTypeVariantError_expectedVariant @@ var "e" $
  "actual" <~ project _UnexpectedTypeVariantError _UnexpectedTypeVariantError_actualType @@ var "e" $
  Strings.cat $ list [
    string "expected ",
    ShowVariants.typeVariant @@ var "expected",
    string " type but found ",
    ShowCore.type_ @@ var "actual"]

-- ============================================================================
-- Term validation error show functions
-- ============================================================================

constantConditionError :: TTermDefinition (ConstantConditionError -> String)
constantConditionError = define "constantConditionError" $
  doc "Show a constant condition error as a string" $
  "e" ~> Strings.cat $ list [
    string "constant condition: ifElse with literal ",
    Literals.showBoolean $ project _ConstantConditionError _ConstantConditionError_value @@ var "e"]

emptyCaseStatementError :: TTermDefinition (EmptyCaseStatementError -> String)
emptyCaseStatementError = define "emptyCaseStatementError" $
  doc "Show an empty case statement error as a string" $
  "e" ~> Strings.cat $ list [
    string "empty case statement for type: ",
    Core.unName $ project _EmptyCaseStatementError _EmptyCaseStatementError_typeName @@ var "e"]

emptyLetBindingsError :: TTermDefinition (EmptyLetBindingsError -> String)
emptyLetBindingsError = define "emptyLetBindingsError" $
  doc "Show an empty let bindings error as a string" $
  "e" ~> string "let expression with no bindings"

emptyTermAnnotationError :: TTermDefinition (EmptyTermAnnotationError -> String)
emptyTermAnnotationError = define "emptyTermAnnotationError" $
  doc "Show an empty term annotation error as a string" $
  "e" ~> string "term annotation with empty annotation map"

emptyTypeNameInTermError :: TTermDefinition (EmptyTypeNameInTermError -> String)
emptyTypeNameInTermError = define "emptyTypeNameInTermError" $
  doc "Show an empty type name in term error as a string" $
  "e" ~> string "term with empty type name"

invalidLambdaParameterNameError :: TTermDefinition (InvalidLambdaParameterNameError -> String)
invalidLambdaParameterNameError = define "invalidLambdaParameterNameError" $
  doc "Show an invalid lambda parameter name error as a string" $
  "e" ~> Strings.cat $ list [
    string "invalid lambda parameter name: ",
    Core.unName $ project _InvalidLambdaParameterNameError _InvalidLambdaParameterNameError_name @@ var "e"]

invalidLetBindingNameError :: TTermDefinition (InvalidLetBindingNameError -> String)
invalidLetBindingNameError = define "invalidLetBindingNameError" $
  doc "Show an invalid let binding name error as a string" $
  "e" ~> Strings.cat $ list [
    string "invalid let binding name: ",
    Core.unName $ project _InvalidLetBindingNameError _InvalidLetBindingNameError_name @@ var "e"]

invalidTypeLambdaParameterNameError :: TTermDefinition (InvalidTypeLambdaParameterNameError -> String)
invalidTypeLambdaParameterNameError = define "invalidTypeLambdaParameterNameError" $
  doc "Show an invalid type lambda parameter name error as a string" $
  "e" ~> Strings.cat $ list [
    string "invalid type lambda parameter name: ",
    Core.unName $ project _InvalidTypeLambdaParameterNameError _InvalidTypeLambdaParameterNameError_name @@ var "e"]

nestedTermAnnotationError :: TTermDefinition (NestedTermAnnotationError -> String)
nestedTermAnnotationError = define "nestedTermAnnotationError" $
  doc "Show a nested term annotation error as a string" $
  "e" ~> string "nested term annotations should be merged"

redundantWrapUnwrapError :: TTermDefinition (RedundantWrapUnwrapError -> String)
redundantWrapUnwrapError = define "redundantWrapUnwrapError" $
  doc "Show a redundant wrap/unwrap error as a string" $
  "e" ~> Strings.cat $ list [
    string "redundant wrap/unwrap for type: ",
    Core.unName $ project _RedundantWrapUnwrapError _RedundantWrapUnwrapError_typeName @@ var "e"]

selfApplicationError :: TTermDefinition (SelfApplicationError -> String)
selfApplicationError = define "selfApplicationError" $
  doc "Show a self-application error as a string" $
  "e" ~> Strings.cat $ list [
    string "self-application of variable: ",
    Core.unName $ project _SelfApplicationError _SelfApplicationError_name @@ var "e"]

termVariableShadowingError :: TTermDefinition (TermVariableShadowingError -> String)
termVariableShadowingError = define "termVariableShadowingError" $
  doc "Show a term variable shadowing error as a string" $
  "e" ~> Strings.cat $ list [
    string "variable shadowing: ",
    Core.unName $ project _TermVariableShadowingError _TermVariableShadowingError_name @@ var "e"]

typeVariableShadowingInTypeLambdaError :: TTermDefinition (TypeVariableShadowingInTypeLambdaError -> String)
typeVariableShadowingInTypeLambdaError = define "typeVariableShadowingInTypeLambdaError" $
  doc "Show a type variable shadowing in type lambda error as a string" $
  "e" ~> Strings.cat $ list [
    string "type variable shadowing in type lambda: ",
    Core.unName $ project _TypeVariableShadowingInTypeLambdaError _TypeVariableShadowingInTypeLambdaError_name @@ var "e"]

undefinedTermVariableError :: TTermDefinition (UndefinedTermVariableError -> String)
undefinedTermVariableError = define "undefinedTermVariableError" $
  doc "Show an undefined term variable error as a string" $
  "e" ~> Strings.cat $ list [
    string "undefined term variable: ",
    Core.unName $ project _UndefinedTermVariableError _UndefinedTermVariableError_name @@ var "e"]

undefinedTypeVariableInLambdaDomainError :: TTermDefinition (UndefinedTypeVariableInLambdaDomainError -> String)
undefinedTypeVariableInLambdaDomainError = define "undefinedTypeVariableInLambdaDomainError" $
  doc "Show an undefined type variable in lambda domain error as a string" $
  "e" ~> Strings.cat $ list [
    string "undefined type variable in lambda domain: ",
    Core.unName $ project _UndefinedTypeVariableInLambdaDomainError _UndefinedTypeVariableInLambdaDomainError_name @@ var "e"]

undefinedTypeVariableInTypeApplicationError :: TTermDefinition (UndefinedTypeVariableInTypeApplicationError -> String)
undefinedTypeVariableInTypeApplicationError = define "undefinedTypeVariableInTypeApplicationError" $
  doc "Show an undefined type variable in type application error as a string" $
  "e" ~> Strings.cat $ list [
    string "undefined type variable in type application: ",
    Core.unName $ project _UndefinedTypeVariableInTypeApplicationError _UndefinedTypeVariableInTypeApplicationError_name @@ var "e"]

undefinedTypeVariableInBindingTypeError :: TTermDefinition (UndefinedTypeVariableInBindingTypeError -> String)
undefinedTypeVariableInBindingTypeError = define "undefinedTypeVariableInBindingTypeError" $
  doc "Show an undefined type variable in binding type error as a string" $
  "e" ~> Strings.cat $ list [
    string "undefined type variable in binding type: ",
    Core.unName $ project _UndefinedTypeVariableInBindingTypeError _UndefinedTypeVariableInBindingTypeError_name @@ var "e"]

unknownPrimitiveNameError :: TTermDefinition (UnknownPrimitiveNameError -> String)
unknownPrimitiveNameError = define "unknownPrimitiveNameError" $
  doc "Show an unknown primitive name error as a string" $
  "e" ~> Strings.cat $ list [
    string "unknown primitive: ",
    Core.unName $ project _UnknownPrimitiveNameError _UnknownPrimitiveNameError_name @@ var "e"]

unnecessaryIdentityApplicationError :: TTermDefinition (UnnecessaryIdentityApplicationError -> String)
unnecessaryIdentityApplicationError = define "unnecessaryIdentityApplicationError" $
  doc "Show an unnecessary identity application error as a string" $
  "e" ~> string "unnecessary application of identity lambda"

untypedTermVariableError :: TTermDefinition (UntypedTermVariableError -> String)
untypedTermVariableError = define "untypedTermVariableError" $
  doc "Show an untyped term variable error as a string" $
  "e" ~> Strings.cat $ list [
    string "untyped term variable: ",
    Core.unName $ project _UntypedTermVariableError _UntypedTermVariableError_name @@ var "e"]

invalidTermError :: TTermDefinition (InvalidTermError -> String)
invalidTermError = define "invalidTermError" $
  doc "Show an invalid term error as a string" $
  "e" ~> Strings.cat2 (string "invalid term: ") $
    cases _InvalidTermError (var "e") Nothing [
      _InvalidTermError_constantCondition>>: constantConditionError,
      _InvalidTermError_duplicateBinding>>: duplicateBindingError,
      _InvalidTermError_duplicateField>>: duplicateFieldError,
      _InvalidTermError_emptyCaseStatement>>: emptyCaseStatementError,
      _InvalidTermError_emptyLetBindings>>: emptyLetBindingsError,
      _InvalidTermError_emptyTermAnnotation>>: emptyTermAnnotationError,
      _InvalidTermError_emptyTypeNameInTerm>>: emptyTypeNameInTermError,
      _InvalidTermError_invalidLambdaParameterName>>: invalidLambdaParameterNameError,
      _InvalidTermError_invalidLetBindingName>>: invalidLetBindingNameError,
      _InvalidTermError_invalidTypeLambdaParameterName>>: invalidTypeLambdaParameterNameError,
      _InvalidTermError_nestedTermAnnotation>>: nestedTermAnnotationError,
      _InvalidTermError_redundantWrapUnwrap>>: redundantWrapUnwrapError,
      _InvalidTermError_selfApplication>>: selfApplicationError,
      _InvalidTermError_termVariableShadowing>>: termVariableShadowingError,
      _InvalidTermError_typeVariableShadowingInTypeLambda>>: typeVariableShadowingInTypeLambdaError,
      _InvalidTermError_undefinedTermVariable>>: undefinedTermVariableError,
      _InvalidTermError_undefinedTypeVariableInBindingType>>: undefinedTypeVariableInBindingTypeError,
      _InvalidTermError_undefinedTypeVariableInLambdaDomain>>: undefinedTypeVariableInLambdaDomainError,
      _InvalidTermError_undefinedTypeVariableInTypeApplication>>: undefinedTypeVariableInTypeApplicationError,
      _InvalidTermError_unknownPrimitiveName>>: unknownPrimitiveNameError,
      _InvalidTermError_unnecessaryIdentityApplication>>: unnecessaryIdentityApplicationError,
      _InvalidTermError_untypedTermVariable>>: untypedTermVariableError]

-- ============================================================================
-- Type validation error show functions
-- ============================================================================

duplicateRecordTypeFieldNamesError :: TTermDefinition (DuplicateRecordTypeFieldNamesError -> String)
duplicateRecordTypeFieldNamesError = define "duplicateRecordTypeFieldNamesError" $
  doc "Show a duplicate record type field names error as a string" $
  "e" ~> Strings.cat $ list [
    string "duplicate field in record type: ",
    Core.unName $ project _DuplicateRecordTypeFieldNamesError _DuplicateRecordTypeFieldNamesError_name @@ var "e"]

duplicateUnionTypeFieldNamesError :: TTermDefinition (DuplicateUnionTypeFieldNamesError -> String)
duplicateUnionTypeFieldNamesError = define "duplicateUnionTypeFieldNamesError" $
  doc "Show a duplicate union type field names error as a string" $
  "e" ~> Strings.cat $ list [
    string "duplicate field in union type: ",
    Core.unName $ project _DuplicateUnionTypeFieldNamesError _DuplicateUnionTypeFieldNamesError_name @@ var "e"]

emptyRecordTypeError :: TTermDefinition (EmptyRecordTypeError -> String)
emptyRecordTypeError = define "emptyRecordTypeError" $
  doc "Show an empty record type error as a string" $
  "e" ~> string "record type with no fields (use TypeUnit instead)"

emptyTypeAnnotationError :: TTermDefinition (EmptyTypeAnnotationError -> String)
emptyTypeAnnotationError = define "emptyTypeAnnotationError" $
  doc "Show an empty type annotation error as a string" $
  "e" ~> string "type annotation with empty annotation map"

emptyUnionTypeError :: TTermDefinition (EmptyUnionTypeError -> String)
emptyUnionTypeError = define "emptyUnionTypeError" $
  doc "Show an empty union type error as a string" $
  "e" ~> string "union type with no alternatives (use TypeVoid instead)"

invalidForallParameterNameError :: TTermDefinition (InvalidForallParameterNameError -> String)
invalidForallParameterNameError = define "invalidForallParameterNameError" $
  doc "Show an invalid forall parameter name error as a string" $
  "e" ~> Strings.cat $ list [
    string "invalid forall parameter name: ",
    Core.unName $ project _InvalidForallParameterNameError _InvalidForallParameterNameError_name @@ var "e"]

invalidTypeSchemeVariableNameError :: TTermDefinition (InvalidTypeSchemeVariableNameError -> String)
invalidTypeSchemeVariableNameError = define "invalidTypeSchemeVariableNameError" $
  doc "Show an invalid type scheme variable name error as a string" $
  "e" ~> Strings.cat $ list [
    string "invalid type scheme variable name: ",
    Core.unName $ project _InvalidTypeSchemeVariableNameError _InvalidTypeSchemeVariableNameError_name @@ var "e"]

nestedTypeAnnotationError :: TTermDefinition (NestedTypeAnnotationError -> String)
nestedTypeAnnotationError = define "nestedTypeAnnotationError" $
  doc "Show a nested type annotation error as a string" $
  "e" ~> string "nested type annotations should be merged"

nonComparableMapKeyTypeError :: TTermDefinition (NonComparableMapKeyTypeError -> String)
nonComparableMapKeyTypeError = define "nonComparableMapKeyTypeError" $
  doc "Show a non-comparable map key type error as a string" $
  "e" ~> Strings.cat $ list [
    string "map key type contains a function type: ",
    ShowCore.type_ @@ (project _NonComparableMapKeyTypeError _NonComparableMapKeyTypeError_keyType @@ var "e")]

nonComparableSetElementTypeError :: TTermDefinition (NonComparableSetElementTypeError -> String)
nonComparableSetElementTypeError = define "nonComparableSetElementTypeError" $
  doc "Show a non-comparable set element type error as a string" $
  "e" ~> Strings.cat $ list [
    string "set element type contains a function type: ",
    ShowCore.type_ @@ (project _NonComparableSetElementTypeError _NonComparableSetElementTypeError_elementType @@ var "e")]

singleVariantUnionError :: TTermDefinition (SingleVariantUnionError -> String)
singleVariantUnionError = define "singleVariantUnionError" $
  doc "Show a single variant union error as a string" $
  "e" ~> Strings.cat $ list [
    string "union type with single variant: ",
    Core.unName $ project _SingleVariantUnionError _SingleVariantUnionError_fieldName @@ var "e"]

typeVariableShadowingInForallError :: TTermDefinition (TypeVariableShadowingInForallError -> String)
typeVariableShadowingInForallError = define "typeVariableShadowingInForallError" $
  doc "Show a type variable shadowing in forall error as a string" $
  "e" ~> Strings.cat $ list [
    string "type variable shadowing in forall: ",
    Core.unName $ project _TypeVariableShadowingInForallError _TypeVariableShadowingInForallError_name @@ var "e"]

undefinedTypeVariableError :: TTermDefinition (UndefinedTypeVariableError -> String)
undefinedTypeVariableError = define "undefinedTypeVariableError" $
  doc "Show an undefined type variable error as a string" $
  "e" ~> Strings.cat $ list [
    string "undefined type variable: ",
    Core.unName $ project _UndefinedTypeVariableError _UndefinedTypeVariableError_name @@ var "e"]

voidInNonBottomPositionError :: TTermDefinition (VoidInNonBottomPositionError -> String)
voidInNonBottomPositionError = define "voidInNonBottomPositionError" $
  doc "Show a void in non-bottom position error as a string" $
  "e" ~> string "TypeVoid in a position where no value can be constructed"

invalidTypeError :: TTermDefinition (InvalidTypeError -> String)
invalidTypeError = define "invalidTypeError" $
  doc "Show an invalid type error as a string" $
  "e" ~> Strings.cat2 (string "invalid type: ") $
    cases _InvalidTypeError (var "e") Nothing [
      _InvalidTypeError_duplicateRecordTypeFieldNames>>: duplicateRecordTypeFieldNamesError,
      _InvalidTypeError_duplicateUnionTypeFieldNames>>: duplicateUnionTypeFieldNamesError,
      _InvalidTypeError_emptyRecordType>>: emptyRecordTypeError,
      _InvalidTypeError_emptyTypeAnnotation>>: emptyTypeAnnotationError,
      _InvalidTypeError_emptyUnionType>>: emptyUnionTypeError,
      _InvalidTypeError_invalidForallParameterName>>: invalidForallParameterNameError,
      _InvalidTypeError_invalidTypeSchemeVariableName>>: invalidTypeSchemeVariableNameError,
      _InvalidTypeError_nestedTypeAnnotation>>: nestedTypeAnnotationError,
      _InvalidTypeError_nonComparableMapKeyType>>: nonComparableMapKeyTypeError,
      _InvalidTypeError_nonComparableSetElementType>>: nonComparableSetElementTypeError,
      _InvalidTypeError_singleVariantUnion>>: singleVariantUnionError,
      _InvalidTypeError_typeVariableShadowingInForall>>: typeVariableShadowingInForallError,
      _InvalidTypeError_undefinedTypeVariable>>: undefinedTypeVariableError,
      _InvalidTypeError_voidInNonBottomPosition>>: voidInNonBottomPositionError]
