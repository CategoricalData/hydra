-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for core term and type validation

module Hydra.Test.Validate.Core where

import qualified Hydra.Accessors as Accessors
import qualified Hydra.Core as Core
import qualified Hydra.Error.Core as Core_
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Test cases for core term and type validation
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "validate.core",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        duplicateBindingsTests,
        duplicateFieldsTests,
        emptyLetBindingsTests,
        identityApplicationTests,
        variableShadowingTests],
      Testing.testGroupCases = []}

duplicateBindingsTests :: Testing.TestGroup
duplicateBindingsTests =
    Testing.TestGroup {
      Testing.testGroupName = "duplicate bindings",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "no bindings (literal)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseValidateCoreTerm (Testing.ValidateCoreTermTestCase {
            Testing.validateCoreTermTestCaseTyped = False,
            Testing.validateCoreTermTestCaseInput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
            Testing.validateCoreTermTestCaseOutput = Nothing})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseValidateCoreTerm (Testing.ValidateCoreTermTestCase {
            Testing.validateCoreTermTestCaseTyped = False,
            Testing.validateCoreTermTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})),
            Testing.validateCoreTermTestCaseOutput = Nothing})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "distinct bindings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseValidateCoreTerm (Testing.ValidateCoreTermTestCase {
            Testing.validateCoreTermTestCaseTyped = False,
            Testing.validateCoreTermTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})),
            Testing.validateCoreTermTestCaseOutput = Nothing})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate bindings at top level",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseValidateCoreTerm (Testing.ValidateCoreTermTestCase {
            Testing.validateCoreTermTestCaseTyped = False,
            Testing.validateCoreTermTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})),
            Testing.validateCoreTermTestCaseOutput = (Just (Core_.InvalidTermErrorDuplicateBinding (Core_.DuplicateBindingError {
              Core_.duplicateBindingErrorLocation = (Accessors.AccessorPath []),
              Core_.duplicateBindingErrorName = (Core.Name "x")})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate bindings in lambda body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseValidateCoreTerm (Testing.ValidateCoreTermTestCase {
            Testing.validateCoreTermTestCaseTyped = False,
            Testing.validateCoreTermTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "f"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "a"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                    Core.bindingType = Nothing},
                  Core.Binding {
                    Core.bindingName = (Core.Name "a"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermVariable (Core.Name "a"))}))}))),
            Testing.validateCoreTermTestCaseOutput = (Just (Core_.InvalidTermErrorDuplicateBinding (Core_.DuplicateBindingError {
              Core_.duplicateBindingErrorLocation = (Accessors.AccessorPath [
                Accessors.TermAccessorLambdaBody]),
              Core_.duplicateBindingErrorName = (Core.Name "a")})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate bindings in let body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseValidateCoreTerm (Testing.ValidateCoreTermTestCase {
            Testing.validateCoreTermTestCaseTyped = False,
            Testing.validateCoreTermTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    Core.bindingType = Nothing},
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermVariable (Core.Name "y"))}))})),
            Testing.validateCoreTermTestCaseOutput = (Just (Core_.InvalidTermErrorDuplicateBinding (Core_.DuplicateBindingError {
              Core_.duplicateBindingErrorLocation = (Accessors.AccessorPath [
                Accessors.TermAccessorLetBody]),
              Core_.duplicateBindingErrorName = (Core.Name "y")})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate bindings in let binding value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseValidateCoreTerm (Testing.ValidateCoreTermTestCase {
            Testing.validateCoreTermTestCaseTyped = False,
            Testing.validateCoreTermTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "a"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                        Core.bindingType = Nothing},
                      Core.Binding {
                        Core.bindingName = (Core.Name "a"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "a"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})),
            Testing.validateCoreTermTestCaseOutput = (Just (Core_.InvalidTermErrorDuplicateBinding (Core_.DuplicateBindingError {
              Core_.duplicateBindingErrorLocation = (Accessors.AccessorPath [
                Accessors.TermAccessorLetBinding (Core.Name "x")]),
              Core_.duplicateBindingErrorName = (Core.Name "a")})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "same name in different scopes is valid",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseValidateCoreTerm (Testing.ValidateCoreTermTestCase {
            Testing.validateCoreTermTestCaseTyped = False,
            Testing.validateCoreTermTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
            Testing.validateCoreTermTestCaseOutput = Nothing})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

duplicateFieldsTests :: Testing.TestGroup
duplicateFieldsTests =
    Testing.TestGroup {
      Testing.testGroupName = "duplicate fields",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "no fields (literal)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseValidateCoreTerm (Testing.ValidateCoreTermTestCase {
            Testing.validateCoreTermTestCaseTyped = False,
            Testing.validateCoreTermTestCaseInput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
            Testing.validateCoreTermTestCaseOutput = Nothing})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "distinct record fields",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseValidateCoreTerm (Testing.ValidateCoreTermTestCase {
            Testing.validateCoreTermTestCaseTyped = False,
            Testing.validateCoreTermTestCaseInput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "Point"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "x"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))},
                Core.Field {
                  Core.fieldName = (Core.Name "y"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}]})),
            Testing.validateCoreTermTestCaseOutput = Nothing})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate record fields at top level",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseValidateCoreTerm (Testing.ValidateCoreTermTestCase {
            Testing.validateCoreTermTestCaseTyped = False,
            Testing.validateCoreTermTestCaseInput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "Point"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "x"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))},
                Core.Field {
                  Core.fieldName = (Core.Name "x"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}]})),
            Testing.validateCoreTermTestCaseOutput = (Just (Core_.InvalidTermErrorDuplicateField (Core_.DuplicateFieldError {
              Core_.duplicateFieldErrorLocation = (Accessors.AccessorPath []),
              Core_.duplicateFieldErrorName = (Core.Name "x")})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate fields in record inside lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseValidateCoreTerm (Testing.ValidateCoreTermTestCase {
            Testing.validateCoreTermTestCaseTyped = False,
            Testing.validateCoreTermTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "f"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "Point"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "x"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))},
                  Core.Field {
                    Core.fieldName = (Core.Name "x"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}]}))}))),
            Testing.validateCoreTermTestCaseOutput = (Just (Core_.InvalidTermErrorDuplicateField (Core_.DuplicateFieldError {
              Core_.duplicateFieldErrorLocation = (Accessors.AccessorPath [
                Accessors.TermAccessorLambdaBody]),
              Core_.duplicateFieldErrorName = (Core.Name "x")})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate fields in record inside let body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseValidateCoreTerm (Testing.ValidateCoreTermTestCase {
            Testing.validateCoreTermTestCaseTyped = False,
            Testing.validateCoreTermTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "r"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "Point"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "x"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))},
                  Core.Field {
                    Core.fieldName = (Core.Name "x"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}]}))})),
            Testing.validateCoreTermTestCaseOutput = (Just (Core_.InvalidTermErrorDuplicateField (Core_.DuplicateFieldError {
              Core_.duplicateFieldErrorLocation = (Accessors.AccessorPath [
                Accessors.TermAccessorLetBody]),
              Core_.duplicateFieldErrorName = (Core.Name "x")})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

emptyLetBindingsTests :: Testing.TestGroup
emptyLetBindingsTests =
    Testing.TestGroup {
      Testing.testGroupName = "empty let bindings",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "let with bindings is valid",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseValidateCoreTerm (Testing.ValidateCoreTermTestCase {
            Testing.validateCoreTermTestCaseTyped = False,
            Testing.validateCoreTermTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})),
            Testing.validateCoreTermTestCaseOutput = Nothing})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty let bindings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseValidateCoreTerm (Testing.ValidateCoreTermTestCase {
            Testing.validateCoreTermTestCaseTyped = False,
            Testing.validateCoreTermTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [],
              Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
            Testing.validateCoreTermTestCaseOutput = (Just (Core_.InvalidTermErrorEmptyLetBindings (Core_.EmptyLetBindingsError {
              Core_.emptyLetBindingsErrorLocation = (Accessors.AccessorPath [])})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

identityApplicationTests :: Testing.TestGroup
identityApplicationTests =
    Testing.TestGroup {
      Testing.testGroupName = "identity application",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "non-identity lambda application is valid",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseValidateCoreTerm (Testing.ValidateCoreTermTestCase {
            Testing.validateCoreTermTestCaseTyped = False,
            Testing.validateCoreTermTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
            Testing.validateCoreTermTestCaseOutput = Nothing})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "identity lambda application",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseValidateCoreTerm (Testing.ValidateCoreTermTestCase {
            Testing.validateCoreTermTestCaseTyped = False,
            Testing.validateCoreTermTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.validateCoreTermTestCaseOutput = (Just (Core_.InvalidTermErrorUnnecessaryIdentityApplication (Core_.UnnecessaryIdentityApplicationError {
              Core_.unnecessaryIdentityApplicationErrorLocation = (Accessors.AccessorPath [])})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

variableShadowingTests :: Testing.TestGroup
variableShadowingTests =
    Testing.TestGroup {
      Testing.testGroupName = "variable shadowing",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda with fresh variable is valid",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseValidateCoreTerm (Testing.ValidateCoreTermTestCase {
            Testing.validateCoreTermTestCaseTyped = False,
            Testing.validateCoreTermTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
            Testing.validateCoreTermTestCaseOutput = Nothing})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda shadows outer lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseValidateCoreTerm (Testing.ValidateCoreTermTestCase {
            Testing.validateCoreTermTestCaseTyped = False,
            Testing.validateCoreTermTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))),
            Testing.validateCoreTermTestCaseOutput = Nothing})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "let binding shadows lambda parameter",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseValidateCoreTerm (Testing.ValidateCoreTermTestCase {
            Testing.validateCoreTermTestCaseTyped = False,
            Testing.validateCoreTermTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))),
            Testing.validateCoreTermTestCaseOutput = Nothing})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
