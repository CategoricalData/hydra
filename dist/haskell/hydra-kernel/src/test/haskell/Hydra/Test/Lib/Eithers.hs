-- Note: this is an automatically generated file. Do not edit.
-- | Test cases for hydra.lib.eithers primitives

module Hydra.Test.Lib.Eithers where
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
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Equality as Equality
import qualified Hydra.Haskell.Lib.Literals as Literals
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Math as Math
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Query as Query
import qualified Hydra.Reduction as Reduction
import qualified Hydra.Relational as Relational
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Test cases for hydra.lib.eithers primitives
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "hydra.lib.eithers primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "bind",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "bind Right with success",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\e -> ShowCore.either (\n -> Literals.showInt32 n) (\n -> Literals.showInt32 n) e) (Eithers.bind (Right "ab") (\s -> Logic.ifElse (Strings.null s) (Left 0) (Right (Strings.length s))))),
                Testing.universalTestCaseExpected = (\_ -> (\e -> ShowCore.either (\n -> Literals.showInt32 n) (\n -> Literals.showInt32 n) e) (Right 2))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "bind Right with failure",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\e -> ShowCore.either (\n -> Literals.showInt32 n) (\n -> Literals.showInt32 n) e) (Eithers.bind (Right "") (\s -> Logic.ifElse (Strings.null s) (Left 0) (Right (Strings.length s))))),
                Testing.universalTestCaseExpected = (\_ -> (\e -> ShowCore.either (\n -> Literals.showInt32 n) (\n -> Literals.showInt32 n) e) (Left 0))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "bind Left returns Left unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\e -> ShowCore.either (\n -> Literals.showInt32 n) (\n -> Literals.showInt32 n) e) (Eithers.bind (Left 42) (\s -> Logic.ifElse (Strings.null s) (Left 0) (Right (Strings.length s))))),
                Testing.universalTestCaseExpected = (\_ -> (\e -> ShowCore.either (\n -> Literals.showInt32 n) (\n -> Literals.showInt32 n) e) (Left 42))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "bimap",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "map left value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\e -> ShowCore.either (\n -> Literals.showInt32 n) (\n -> Literals.showInt32 n) e) (Eithers.bimap (\x -> Math.mul x 2) (\s -> Strings.length s) (Left 5))),
                Testing.universalTestCaseExpected = (\_ -> (\e -> ShowCore.either (\n -> Literals.showInt32 n) (\n -> Literals.showInt32 n) e) (Left 10))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "map right value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\e -> ShowCore.either (\n -> Literals.showInt32 n) (\n -> Literals.showInt32 n) e) (Eithers.bimap (\x -> Math.mul x 2) (\s -> Strings.length s) (Right "ab"))),
                Testing.universalTestCaseExpected = (\_ -> (\e -> ShowCore.either (\n -> Literals.showInt32 n) (\n -> Literals.showInt32 n) e) (Right 2))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "isLeft",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "left value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Eithers.isLeft (Left 42))),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "right value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Eithers.isLeft (Right "test"))),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "isRight",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "right value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Eithers.isRight (Right "test"))),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "left value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Eithers.isRight (Left 42))),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "fromLeft",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "extract left",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\n -> Literals.showInt32 n) (Eithers.fromLeft 99 (Left 42))),
                Testing.universalTestCaseExpected = (\_ -> (\n -> Literals.showInt32 n) 42)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "use default for right",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\n -> Literals.showInt32 n) (Eithers.fromLeft 99 (Right "test"))),
                Testing.universalTestCaseExpected = (\_ -> (\n -> Literals.showInt32 n) 99)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "fromRight",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "extract right",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\s -> Literals.showString s) (Eithers.fromRight "default" (Right "test"))),
                Testing.universalTestCaseExpected = (\_ -> (\s -> Literals.showString s) "test")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "use default for left",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\s -> Literals.showString s) (Eithers.fromRight "default" (Left 42))),
                Testing.universalTestCaseExpected = (\_ -> (\s -> Literals.showString s) "default")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "either",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "apply left function",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\n -> Literals.showInt32 n) (Eithers.either (\x -> Math.mul x 2) (\s -> Strings.length s) (Left 5))),
                Testing.universalTestCaseExpected = (\_ -> (\n -> Literals.showInt32 n) 10)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "apply right function",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\n -> Literals.showInt32 n) (Eithers.either (\x -> Math.mul x 2) (\s -> Strings.length s) (Right "ab"))),
                Testing.universalTestCaseExpected = (\_ -> (\n -> Literals.showInt32 n) 2)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "lefts",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "filter left values",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (Eithers.lefts [
                  Left 1,
                  (Right "a"),
                  (Left 2),
                  (Right "b")])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) [
                  1,
                  2])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "all lefts",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (Eithers.lefts [
                  Left 1,
                  (Left 2)])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) [
                  1,
                  2])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "all rights",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (Eithers.lefts [
                  Right "a",
                  (Right "b")])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (Eithers.lefts [])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "rights",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "filter right values",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) (Eithers.rights [
                  Left 1,
                  (Right "a"),
                  (Left 2),
                  (Right "b")])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) [
                  "a",
                  "b"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "all rights",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) (Eithers.rights [
                  Right "a",
                  (Right "b")])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) [
                  "a",
                  "b"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "all lefts",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) (Eithers.rights [
                  Left 1,
                  (Left 2)])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) (Eithers.rights [])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\s -> Literals.showString s) xs) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "partitionEithers",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "partition mixed",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\p -> ShowCore.pair (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (\xs -> ShowCore.list (\s -> Literals.showString s) xs) p) (Eithers.partitionEithers [
                  Left 1,
                  (Right "a"),
                  (Left 2),
                  (Right "b")])),
                Testing.universalTestCaseExpected = (\_ -> (\p -> ShowCore.pair (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (\xs -> ShowCore.list (\s -> Literals.showString s) xs) p) (
                  [
                    1,
                    2],
                  [
                    "a",
                    "b"]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "all lefts",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\p -> ShowCore.pair (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (\xs -> ShowCore.list (\s -> Literals.showString s) xs) p) (Eithers.partitionEithers [
                  Left 1,
                  (Left 2)])),
                Testing.universalTestCaseExpected = (\_ -> (\p -> ShowCore.pair (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (\xs -> ShowCore.list (\s -> Literals.showString s) xs) p) (
                  [
                    1,
                    2],
                  []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "all rights",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\p -> ShowCore.pair (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (\xs -> ShowCore.list (\s -> Literals.showString s) xs) p) (Eithers.partitionEithers [
                  Right "a",
                  (Right "b")])),
                Testing.universalTestCaseExpected = (\_ -> (\p -> ShowCore.pair (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (\xs -> ShowCore.list (\s -> Literals.showString s) xs) p) (
                  [],
                  [
                    "a",
                    "b"]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\p -> ShowCore.pair (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (\xs -> ShowCore.list (\s -> Literals.showString s) xs) p) (Eithers.partitionEithers [])),
                Testing.universalTestCaseExpected = (\_ -> (\p -> ShowCore.pair (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (\xs -> ShowCore.list (\s -> Literals.showString s) xs) p) ([], []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "map",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "map right value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\e -> ShowCore.either (\n -> Literals.showInt32 n) (\n -> Literals.showInt32 n) e) (Eithers.map (\x -> Math.mul x 2) (Right 5))),
                Testing.universalTestCaseExpected = (\_ -> (\e -> ShowCore.either (\n -> Literals.showInt32 n) (\n -> Literals.showInt32 n) e) (Right 10))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "preserve left",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\e -> ShowCore.either (\n -> Literals.showInt32 n) (\n -> Literals.showInt32 n) e) (Eithers.map (\x -> Math.mul x 2) (Left 99))),
                Testing.universalTestCaseExpected = (\_ -> (\e -> ShowCore.either (\n -> Literals.showInt32 n) (\n -> Literals.showInt32 n) e) (Left 99))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "mapList",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "all succeed",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\e -> ShowCore.either (\s -> Literals.showString s) (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) e) (Eithers.mapList (\x -> Logic.ifElse (Equality.equal x 0) (Left "zero") (Right (Math.mul x 2))) [
                  1,
                  2,
                  3])),
                Testing.universalTestCaseExpected = (\_ -> (\e -> ShowCore.either (\s -> Literals.showString s) (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) e) (Right [
                  2,
                  4,
                  6]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "first fails",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\e -> ShowCore.either (\s -> Literals.showString s) (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) e) (Eithers.mapList (\x -> Logic.ifElse (Equality.equal x 0) (Left "zero") (Right (Math.mul x 2))) [
                  1,
                  0,
                  3])),
                Testing.universalTestCaseExpected = (\_ -> (\e -> ShowCore.either (\s -> Literals.showString s) (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) e) (Left "zero"))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\e -> ShowCore.either (\s -> Literals.showString s) (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) e) (Eithers.mapList (\x -> Logic.ifElse (Equality.equal x 0) (Left "zero") (Right (Math.mul x 2))) [])),
                Testing.universalTestCaseExpected = (\_ -> (\e -> ShowCore.either (\s -> Literals.showString s) (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) e) (Right []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "mapMaybe",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "just succeeds",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\e -> ShowCore.either (\s -> Literals.showString s) (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) e) (Eithers.mapMaybe (\x -> Logic.ifElse (Equality.equal x 0) (Left "zero") (Right (Math.mul x 2))) (Just 5))),
                Testing.universalTestCaseExpected = (\_ -> (\e -> ShowCore.either (\s -> Literals.showString s) (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) e) (Right (Just 10)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "just fails",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\e -> ShowCore.either (\s -> Literals.showString s) (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) e) (Eithers.mapMaybe (\x -> Logic.ifElse (Equality.equal x 0) (Left "zero") (Right (Math.mul x 2))) (Just 0))),
                Testing.universalTestCaseExpected = (\_ -> (\e -> ShowCore.either (\s -> Literals.showString s) (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) e) (Left "zero"))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nothing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\e -> ShowCore.either (\s -> Literals.showString s) (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) e) (Eithers.mapMaybe (\x -> Logic.ifElse (Equality.equal x 0) (Left "zero") (Right (Math.mul x 2))) Nothing)),
                Testing.universalTestCaseExpected = (\_ -> (\e -> ShowCore.either (\s -> Literals.showString s) (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) e) (Right Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
