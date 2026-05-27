-- Note: this is an automatically generated file. Do not edit.
-- | Test cases for Ord instance comparisons on complex Hydra types

module Hydra.Test.Ordering where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Equality as Equality
import qualified Hydra.Haskell.Lib.Literals as Literals
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Query as Query
import qualified Hydra.Reduction as Reduction
import qualified Hydra.Relational as Relational
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Show.Util as ShowUtil
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Test.TestTypes as TestTypes
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Test cases for Ord instance comparisons on complex Hydra types
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "ordering",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "Name comparison",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "name less than (alphabetic)",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "apple"))})) (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "banana"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonLessThan)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "name equal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hello"))})) (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hello"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonEqualTo)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "name greater than",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "zebra"))})) (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "apple"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonGreaterThan)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "qualified name less than",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))})) (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonLessThan)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "qualified name equal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))})) (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonEqualTo)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "name equality true",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Equality.equal (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "foo"))})) (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "foo"))})))),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "name equality false",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Equality.equal (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "foo"))})) (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "bar"))})))),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Literal comparison",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "int32 literal less than",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10))) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20))))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonLessThan)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "int32 literal equal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))) (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonEqualTo)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string literal less than",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermLiteral (Core.LiteralString "aaa")) (Core.TermLiteral (Core.LiteralString "bbb")))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonLessThan)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "boolean false < true",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermLiteral (Core.LiteralBoolean False)) (Core.TermLiteral (Core.LiteralBoolean True)))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonLessThan)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "boolean true == true",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermLiteral (Core.LiteralBoolean True)) (Core.TermLiteral (Core.LiteralBoolean True)))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonEqualTo)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Record comparison (monomorphic)",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "person less than by firstName",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypePersonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})) (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypePersonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Bob"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonLessThan)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "person less than by lastName",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypePersonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Jones"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})) (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypePersonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonLessThan)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "person less than by age",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypePersonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]})) (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypePersonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonLessThan)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "person equal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypePersonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})) (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypePersonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonEqualTo)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "latLon less than by lat",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeLatLonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "lat"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 10.0)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lon"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 20.0)))}]})) (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeLatLonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "lat"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 15.0)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lon"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 20.0)))}]})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonLessThan)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "latLon less than by lon",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeLatLonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "lat"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 10.0)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lon"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 20.0)))}]})) (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeLatLonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "lat"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 10.0)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lon"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 25.0)))}]})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonLessThan)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "latLon equal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeLatLonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "lat"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 10.0)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lon"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 20.0)))}]})) (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeLatLonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "lat"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 10.0)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lon"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 20.0)))}]})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonEqualTo)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "person equality true",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Equality.equal (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypePersonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})) (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypePersonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})))),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "person equality false",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Equality.equal (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypePersonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})) (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypePersonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Bob"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})))),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Polymorphic type comparison",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "LatLonPoly Int32 less than by lat",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})) (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 15)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonLessThan)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "LatLonPoly Int32 less than by lon",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})) (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonLessThan)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "LatLonPoly Int32 equal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})) (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonEqualTo)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "LatLonPoly String less than",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "10N"))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "20W"))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})) (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "15N"))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "20W"))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonLessThan)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "LatLonPoly String equal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "10N"))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "20W"))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})) (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "10N"))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "20W"))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonEqualTo)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "PersonOrSomething person vs person",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermInject (Core.Injection {
                    Core.injectionTypeName = TestTypes.testTypePersonOrSomethingName,
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "person"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = TestTypes.testTypePersonName,
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "firstName"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "lastName"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "age"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))}})),
                  Core.typeApplicationTermType = (Core.TypeList (Core.TypeVariable TestTypes.testTypePersonName))})) (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermInject (Core.Injection {
                    Core.injectionTypeName = TestTypes.testTypePersonOrSomethingName,
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "person"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = TestTypes.testTypePersonName,
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "firstName"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Bob"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "lastName"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "age"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))}})),
                  Core.typeApplicationTermType = (Core.TypeList (Core.TypeVariable TestTypes.testTypePersonName))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonLessThan)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "PersonOrSomething person equal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermInject (Core.Injection {
                    Core.injectionTypeName = TestTypes.testTypePersonOrSomethingName,
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "person"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = TestTypes.testTypePersonName,
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "firstName"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "lastName"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "age"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))}})),
                  Core.typeApplicationTermType = (Core.TypeList (Core.TypeVariable TestTypes.testTypePersonName))})) (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermInject (Core.Injection {
                    Core.injectionTypeName = TestTypes.testTypePersonOrSomethingName,
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "person"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = TestTypes.testTypePersonName,
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "firstName"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "lastName"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "age"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))}})),
                  Core.typeApplicationTermType = (Core.TypeList (Core.TypeVariable TestTypes.testTypePersonName))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonEqualTo)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "LatLonPoly Int32 equality true",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Equality.equal (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})) (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "LatLonPoly Int32 equality false",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Equality.equal (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})) (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Union comparison",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "Number int variant less than",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermInject (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "int"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}})) (Core.TermInject (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "int"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonLessThan)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "Number int variant equal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermInject (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "int"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}})) (Core.TermInject (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "int"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonEqualTo)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "Number float variant less than",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermInject (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "float"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 1.5)))}})) (Core.TermInject (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "float"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 2.5)))}})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonLessThan)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "Number float vs int (variant name comparison)",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowUtil.comparison (Equality.compare (Core.TermInject (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "float"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 100.0)))}})) (Core.TermInject (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "int"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}})))),
                Testing.universalTestCaseExpected = (\_ -> ShowUtil.comparison Util.ComparisonLessThan)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "Number int equality true",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Equality.equal (Core.TermInject (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "int"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}})) (Core.TermInject (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "int"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}})))),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "Number int equality false (different value)",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Equality.equal (Core.TermInject (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "int"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}})) (Core.TermInject (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "int"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 43)))}})))),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "Number equality false (different variant)",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Equality.equal (Core.TermInject (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "int"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}})) (Core.TermInject (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "float"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 42.0)))}})))),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
