-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for core term and type validation

module Hydra.Test.Validate.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Error.Core as Core_
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Paths as Paths
import qualified Hydra.Show.Error.Core as Core__
import qualified Hydra.Test.TestGraph as TestGraph
import qualified Hydra.Testing as Testing
import qualified Hydra.Validate.Core as Core___
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
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Core___.term False TestGraph.testGraph (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Core___.term False TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "distinct bindings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Core___.term False TestGraph.testGraph (Core.TermLet (Core.Let {
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
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate bindings at top level",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Core___.term False TestGraph.testGraph (Core.TermLet (Core.Let {
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
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Just (Core_.InvalidTermErrorDuplicateBinding (Core_.DuplicateBindingError {
              Core_.duplicateBindingErrorLocation = (Paths.SubtermPath []),
              Core_.duplicateBindingErrorName = (Core.Name "x")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate bindings in lambda body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Core___.term False TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                Core.letBody = (Core.TermVariable (Core.Name "a"))}))}))))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Just (Core_.InvalidTermErrorDuplicateBinding (Core_.DuplicateBindingError {
              Core_.duplicateBindingErrorLocation = (Paths.SubtermPath [
                Paths.SubtermStepLambdaBody]),
              Core_.duplicateBindingErrorName = (Core.Name "a")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate bindings in let body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Core___.term False TestGraph.testGraph (Core.TermLet (Core.Let {
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
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Just (Core_.InvalidTermErrorDuplicateBinding (Core_.DuplicateBindingError {
              Core_.duplicateBindingErrorLocation = (Paths.SubtermPath [
                Paths.SubtermStepLetBody]),
              Core_.duplicateBindingErrorName = (Core.Name "y")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate bindings in let binding value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Core___.term False TestGraph.testGraph (Core.TermLet (Core.Let {
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
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Just (Core_.InvalidTermErrorDuplicateBinding (Core_.DuplicateBindingError {
              Core_.duplicateBindingErrorLocation = (Paths.SubtermPath [
                Paths.SubtermStepLetBinding (Core.Name "x")]),
              Core_.duplicateBindingErrorName = (Core.Name "a")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "same name in different scopes is valid",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Core___.term False TestGraph.testGraph (Core.TermLet (Core.Let {
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
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) Nothing)})),
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
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Core___.term False TestGraph.testGraph (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "distinct record fields",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Core___.term False TestGraph.testGraph (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "Point"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "x"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))},
                Core.Field {
                  Core.fieldName = (Core.Name "y"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}]})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate record fields at top level",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Core___.term False TestGraph.testGraph (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "Point"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "x"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))},
                Core.Field {
                  Core.fieldName = (Core.Name "x"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}]})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Just (Core_.InvalidTermErrorDuplicateField (Core_.DuplicateFieldError {
              Core_.duplicateFieldErrorLocation = (Paths.SubtermPath []),
              Core_.duplicateFieldErrorName = (Core.Name "x")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate fields in record inside lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Core___.term False TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}]}))}))))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Just (Core_.InvalidTermErrorDuplicateField (Core_.DuplicateFieldError {
              Core_.duplicateFieldErrorLocation = (Paths.SubtermPath [
                Paths.SubtermStepLambdaBody]),
              Core_.duplicateFieldErrorName = (Core.Name "x")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate fields in record inside let body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Core___.term False TestGraph.testGraph (Core.TermLet (Core.Let {
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
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Just (Core_.InvalidTermErrorDuplicateField (Core_.DuplicateFieldError {
              Core_.duplicateFieldErrorLocation = (Paths.SubtermPath [
                Paths.SubtermStepLetBody]),
              Core_.duplicateFieldErrorName = (Core.Name "x")}))))})),
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
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Core___.term False TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty let bindings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Core___.term False TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [],
              Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Just (Core_.InvalidTermErrorEmptyLetBindings (Core_.EmptyLetBindingsError {
              Core_.emptyLetBindingsErrorLocation = (Paths.SubtermPath [])}))))})),
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
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Core___.term False TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "identity lambda application",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Core___.term False TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Just (Core_.InvalidTermErrorUnnecessaryIdentityApplication (Core_.UnnecessaryIdentityApplicationError {
              Core_.unnecessaryIdentityApplicationErrorLocation = (Paths.SubtermPath [])}))))})),
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
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Core___.term False TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda shadows outer lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Core___.term False TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "let binding shadows lambda parameter",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) (Core___.term False TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> Core__.invalidTermError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
