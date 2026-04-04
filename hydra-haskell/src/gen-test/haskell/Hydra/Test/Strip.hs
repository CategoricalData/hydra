-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for annotation and type stripping operations

module Hydra.Test.Strip where

import qualified Hydra.Core as Core
import qualified Hydra.Show.Core as Core_
import qualified Hydra.Strip as Strip
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

-- | Test cases for annotation and type stripping operations
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "strip",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "deannotateTerm",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unannotated literal unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Strip.deannotateTerm (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unannotated variable unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Strip.deannotateTerm (Core.TermVariable (Core.Name "x")))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermVariable (Core.Name "x")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unannotated lambda unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Strip.deannotateTerm (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single annotation stripped",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Strip.deannotateTerm (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.annotatedTermAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested annotations stripped",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Strip.deannotateTerm (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                    Core.annotatedTermAnnotation = M.empty})),
                  Core.annotatedTermAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "annotated lambda stripped",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Strip.deannotateTerm (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.annotatedTermAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "annotated application stripped",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Strip.deannotateTerm (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                  Core.annotatedTermAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "deannotateType",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unannotated primitive type unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Strip.deannotateType (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unannotated string type unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Strip.deannotateType (Core.TypeLiteral Core.LiteralTypeString))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unannotated function type unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Strip.deannotateType (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single annotation stripped",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Strip.deannotateType (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.annotatedTypeAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested annotations stripped",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Strip.deannotateType (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeAnnotated (Core.AnnotatedType {
                    Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.annotatedTypeAnnotation = M.empty})),
                  Core.annotatedTypeAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "annotated list type stripped",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Strip.deannotateType (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                  Core.annotatedTypeAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "annotated function type stripped",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Strip.deannotateType (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})),
                  Core.annotatedTypeAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
