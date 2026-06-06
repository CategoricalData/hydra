-- Note: this is an automatically generated file. Do not edit.
-- | Test cases for type and term substitution operations

module Hydra.Test.Substitution where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Substitution as Substitution
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Test cases for type and term substitution operations
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "substitution",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "substInType",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty substitution returns type unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.type_ (Substitution.substInType (Typing.TypeSubst (Maps.fromList [])) (Core.TypeLiteral Core.LiteralTypeString))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "substitute type variable with int32",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.type_ (Substitution.substInType (Typing.TypeSubst (Maps.fromList [
                  (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])) (Core.TypeVariable (Core.Name "a")))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "non-matching variable unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.type_ (Substitution.substInType (Typing.TypeSubst (Maps.fromList [
                  (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])) (Core.TypeVariable (Core.Name "b")))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.type_ (Core.TypeVariable (Core.Name "b")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "substitute in function domain",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.type_ (Substitution.substInType (Typing.TypeSubst (Maps.fromList [
                  (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])) (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "substitute in function codomain",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.type_ (Substitution.substInType (Typing.TypeSubst (Maps.fromList [
                  (Core.Name "a", (Core.TypeLiteral Core.LiteralTypeString))])) (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "substitute in list element type",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.type_ (Substitution.substInType (Typing.TypeSubst (Maps.fromList [
                  (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])) (Core.TypeList (Core.TypeVariable (Core.Name "a"))))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.type_ (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "substitute in optional type",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.type_ (Substitution.substInType (Typing.TypeSubst (Maps.fromList [
                  (Core.Name "a", (Core.TypeLiteral Core.LiteralTypeString))])) (Core.TypeOptional (Core.TypeVariable (Core.Name "a"))))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.type_ (Core.TypeOptional (Core.TypeLiteral Core.LiteralTypeString)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "substitute in pair type both sides",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.type_ (Substitution.substInType (Typing.TypeSubst (Maps.fromList [
                  (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])) (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeVariable (Core.Name "a")),
                  Core.pairTypeSecond = (Core.TypeVariable (Core.Name "a"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.type_ (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "substitute in either type",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.type_ (Substitution.substInType (Typing.TypeSubst (Maps.fromList [
                  (Core.Name "a", (Core.TypeLiteral Core.LiteralTypeString))])) (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "a")),
                  Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.type_ (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                  Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "substitute in map key type",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.type_ (Substitution.substInType (Typing.TypeSubst (Maps.fromList [
                  (Core.Name "k", (Core.TypeLiteral Core.LiteralTypeString))])) (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
                  Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.type_ (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
                  Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "substitute in set type",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.type_ (Substitution.substInType (Typing.TypeSubst (Maps.fromList [
                  (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])) (Core.TypeSet (Core.TypeVariable (Core.Name "a"))))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.type_ (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested substitution in list of pairs",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.type_ (Substitution.substInType (Typing.TypeSubst (Maps.fromList [
                  (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])) (Core.TypeList (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeVariable (Core.Name "a")),
                  Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.type_ (Core.TypeList (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple substitutions",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.type_ (Substitution.substInType (Typing.TypeSubst (Maps.fromList [
                  (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                  (Core.Name "b", (Core.TypeLiteral Core.LiteralTypeString))])) (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeVariable (Core.Name "a")),
                  Core.pairTypeSecond = (Core.TypeVariable (Core.Name "b"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.type_ (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "forAll bound variable not substituted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.type_ (Substitution.substInType (Typing.TypeSubst (Maps.fromList [
                  (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])) (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.Name "a"),
                  Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.type_ (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.Name "a"),
                  Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "forAll free variable substituted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.type_ (Substitution.substInType (Typing.TypeSubst (Maps.fromList [
                  (Core.Name "b", (Core.TypeLiteral Core.LiteralTypeString))])) (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.Name "a"),
                  Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))}))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.type_ (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.Name "a"),
                  Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                    Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "substInTypeScheme",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "quantified variable shadows substitution",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.type_ (Core.typeSchemeBody (Substitution.substInTypeScheme (Typing.TypeSubst (Maps.fromList [
                  (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])) (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "a"],
                  Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                  Core.typeSchemeConstraints = Nothing})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                  Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "free variable in body is substituted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.type_ (Core.typeSchemeBody (Substitution.substInTypeScheme (Typing.TypeSubst (Maps.fromList [
                  (Core.Name "b", (Core.TypeLiteral Core.LiteralTypeString))])) (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "a"],
                  Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))})),
                  Core.typeSchemeConstraints = Nothing})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "mixed: free substituted, quantified shadowed",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.type_ (Core.typeSchemeBody (Substitution.substInTypeScheme (Typing.TypeSubst (Maps.fromList [
                  (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                  (Core.Name "b", (Core.TypeLiteral Core.LiteralTypeString))])) (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "a"],
                  Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))})),
                  Core.typeSchemeConstraints = Nothing})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple quantifiers shadow substitution",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.type_ (Core.typeSchemeBody (Substitution.substInTypeScheme (Typing.TypeSubst (Maps.fromList [
                  (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                  (Core.Name "b", (Core.TypeLiteral Core.LiteralTypeString))])) (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "a",
                    (Core.Name "b")],
                  Core.typeSchemeBody = (Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (Core.TypeVariable (Core.Name "a")),
                    Core.pairTypeSecond = (Core.TypeVariable (Core.Name "b"))})),
                  Core.typeSchemeConstraints = Nothing})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.type_ (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeVariable (Core.Name "a")),
                  Core.pairTypeSecond = (Core.TypeVariable (Core.Name "b"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty quantifiers: normal substitution",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.type_ (Core.typeSchemeBody (Substitution.substInTypeScheme (Typing.TypeSubst (Maps.fromList [
                  (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])) (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeVariable (Core.Name "a")),
                  Core.typeSchemeConstraints = Nothing})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
