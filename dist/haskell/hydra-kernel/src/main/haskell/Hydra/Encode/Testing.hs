-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.testing

module Hydra.Encode.Testing where
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
tag :: Testing.Tag -> Core.Term
tag x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.testing.Tag"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Testing.unTag x))})
testCase :: Testing.TestCase -> Core.Term
testCase x =
    case x of
      Testing.TestCaseUniversal v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "universal"),
          Core.fieldTerm = (universalTestCase v0)}})
testCaseWithMetadata :: Testing.TestCaseWithMetadata -> Core.Term
testCaseWithMetadata x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Testing.testCaseWithMetadataName x))},
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (testCase (Testing.testCaseWithMetadataCase x))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) opt)) (Testing.testCaseWithMetadataDescription x))},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map tag xs)) (Testing.testCaseWithMetadataTags x))}]})
testGroup :: Testing.TestGroup -> Core.Term
testGroup x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Testing.testGroupName x))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) opt)) (Testing.testGroupDescription x))},
        Core.Field {
          Core.fieldName = (Core.Name "subgroups"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map testGroup xs)) (Testing.testGroupSubgroups x))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map testCaseWithMetadata xs)) (Testing.testGroupCases x))}]})
universalTestCase :: Testing.UniversalTestCase -> Core.Term
universalTestCase x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.UniversalTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Testing.universalTestCaseActual x))},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Testing.universalTestCaseExpected x))}]})
