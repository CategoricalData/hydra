-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for core term and type validation

module Hydra.Test.Validate.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Paths as Paths
import qualified Hydra.Show.Error.Core as ShowErrorCore
import qualified Hydra.Test.TestGraph as TestGraph
import qualified Hydra.Testing as Testing
import qualified Hydra.Validate.Core as ValidateCore
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

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
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (ValidateCore.term False TestGraph.testGraph (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (ValidateCore.term False TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "distinct bindings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (ValidateCore.term False TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate bindings at top level",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (ValidateCore.term False TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (Just (ErrorCore.InvalidTermErrorDuplicateBinding (ErrorCore.DuplicateBindingError {
              ErrorCore.duplicateBindingErrorLocation = (Paths.SubtermPath []),
              ErrorCore.duplicateBindingErrorName = (Core.Name "x")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate bindings in lambda body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (ValidateCore.term False TestGraph.testGraph (Core.TermLambda (Core.Lambda {
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
                Core.letBody = (Core.TermVariable (Core.Name "a"))}))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (Just (ErrorCore.InvalidTermErrorDuplicateBinding (ErrorCore.DuplicateBindingError {
              ErrorCore.duplicateBindingErrorLocation = (Paths.SubtermPath [
                Paths.SubtermStepLambdaBody]),
              ErrorCore.duplicateBindingErrorName = (Core.Name "a")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate bindings in let body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (ValidateCore.term False TestGraph.testGraph (Core.TermLet (Core.Let {
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
                Core.letBody = (Core.TermVariable (Core.Name "y"))}))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (Just (ErrorCore.InvalidTermErrorDuplicateBinding (ErrorCore.DuplicateBindingError {
              ErrorCore.duplicateBindingErrorLocation = (Paths.SubtermPath [
                Paths.SubtermStepLetBody]),
              ErrorCore.duplicateBindingErrorName = (Core.Name "y")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate bindings in let binding value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (ValidateCore.term False TestGraph.testGraph (Core.TermLet (Core.Let {
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
              Core.letBody = (Core.TermVariable (Core.Name "x"))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (Just (ErrorCore.InvalidTermErrorDuplicateBinding (ErrorCore.DuplicateBindingError {
              ErrorCore.duplicateBindingErrorLocation = (Paths.SubtermPath [
                Paths.SubtermStepLetBinding (Core.Name "x")]),
              ErrorCore.duplicateBindingErrorName = (Core.Name "a")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "same name in different scopes is valid",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (ValidateCore.term False TestGraph.testGraph (Core.TermLet (Core.Let {
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
                Core.letBody = (Core.TermVariable (Core.Name "x"))}))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) Nothing)})),
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
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (ValidateCore.term False TestGraph.testGraph (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "distinct record fields",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (ValidateCore.term False TestGraph.testGraph (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "Point"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "x"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))},
                Core.Field {
                  Core.fieldName = (Core.Name "y"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}]})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate record fields at top level",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (ValidateCore.term False TestGraph.testGraph (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "Point"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "x"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))},
                Core.Field {
                  Core.fieldName = (Core.Name "x"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}]})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (Just (ErrorCore.InvalidTermErrorDuplicateField (ErrorCore.DuplicateFieldError {
              ErrorCore.duplicateFieldErrorLocation = (Paths.SubtermPath []),
              ErrorCore.duplicateFieldErrorName = (Core.Name "x")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate fields in record inside lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (ValidateCore.term False TestGraph.testGraph (Core.TermLambda (Core.Lambda {
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
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}]}))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (Just (ErrorCore.InvalidTermErrorDuplicateField (ErrorCore.DuplicateFieldError {
              ErrorCore.duplicateFieldErrorLocation = (Paths.SubtermPath [
                Paths.SubtermStepLambdaBody]),
              ErrorCore.duplicateFieldErrorName = (Core.Name "x")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate fields in record inside let body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (ValidateCore.term False TestGraph.testGraph (Core.TermLet (Core.Let {
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
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}]}))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (Just (ErrorCore.InvalidTermErrorDuplicateField (ErrorCore.DuplicateFieldError {
              ErrorCore.duplicateFieldErrorLocation = (Paths.SubtermPath [
                Paths.SubtermStepLetBody]),
              ErrorCore.duplicateFieldErrorName = (Core.Name "x")}))))})),
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
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (ValidateCore.term False TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty let bindings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (ValidateCore.term False TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [],
              Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (Just (ErrorCore.InvalidTermErrorEmptyLetBindings (ErrorCore.EmptyLetBindingsError {
              ErrorCore.emptyLetBindingsErrorLocation = (Paths.SubtermPath [])}))))})),
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
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (ValidateCore.term False TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "identity lambda application",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (ValidateCore.term False TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (Just (ErrorCore.InvalidTermErrorUnnecessaryIdentityApplication (ErrorCore.UnnecessaryIdentityApplicationError {
              ErrorCore.unnecessaryIdentityApplicationErrorLocation = (Paths.SubtermPath [])}))))})),
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
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (ValidateCore.term False TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda shadows outer lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (ValidateCore.term False TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "let binding shadows lambda parameter",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) (ValidateCore.term False TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermVariable (Core.Name "x"))}))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorCore.invalidTermError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
