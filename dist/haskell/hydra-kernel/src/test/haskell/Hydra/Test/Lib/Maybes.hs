-- Note: this is an automatically generated file. Do not edit.
-- | Test cases for hydra.lib.maybes primitives

module Hydra.Test.Lib.Maybes where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Equality as Equality
import qualified Hydra.Haskell.Lib.Literals as Literals
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Math as Math
import qualified Hydra.Haskell.Lib.Maybes as Maybes
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
-- | Test cases for hydra.lib.maybes primitives
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "hydra.lib.maybes primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "apply",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "both just",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) (Maybes.apply (Just (\x -> Math.add 3 x)) (Just 5))),
                Testing.universalTestCaseExpected = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) (Just 8))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nothing function",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) (Maybes.apply Nothing (Just 5))),
                Testing.universalTestCaseExpected = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) Nothing)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nothing value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) (Maybes.apply (Just (\x -> Math.add 3 x)) Nothing)),
                Testing.universalTestCaseExpected = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) Nothing)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "bind",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "just to just",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) (Maybes.bind (Just 5) (\x -> Just (Math.mul x 2)))),
                Testing.universalTestCaseExpected = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) (Just 10))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nothing to nothing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) (Maybes.bind Nothing (\x -> Just (Math.mul x 2)))),
                Testing.universalTestCaseExpected = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) Nothing)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "cases",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "just applies function",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\n -> Literals.showInt32 n) (Maybes.cases (Just 5) 0 (\x -> Math.mul x 2))),
                Testing.universalTestCaseExpected = (\_ -> (\n -> Literals.showInt32 n) 10)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nothing returns default",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\n -> Literals.showInt32 n) (Maybes.cases Nothing 99 (\x -> Math.mul x 2))),
                Testing.universalTestCaseExpected = (\_ -> (\n -> Literals.showInt32 n) 99)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "cat",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "filters nothings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (Maybes.cat [
                  Just 1,
                  Nothing,
                  (Just 2)])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) [
                  1,
                  2])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "all justs",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (Maybes.cat [
                  Just 1,
                  (Just 2)])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) [
                  1,
                  2])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "all nothings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (Maybes.cat [
                  Nothing,
                  Nothing])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (Maybes.cat [])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "compose",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "both succeed",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) (Maybes.compose (\x -> Logic.ifElse (Equality.lte x 5) (Just (Math.add x 1)) Nothing) (\y -> Logic.ifElse (Equality.gte y 5) (Just (Math.mul y 2)) Nothing) 5)),
                Testing.universalTestCaseExpected = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) (Just 12))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "first fails",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) (Maybes.compose (\x -> Logic.ifElse (Equality.lte x 5) (Just (Math.add x 1)) Nothing) (\y -> Logic.ifElse (Equality.gte y 5) (Just (Math.mul y 2)) Nothing) 10)),
                Testing.universalTestCaseExpected = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) Nothing)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "second fails",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) (Maybes.compose (\x -> Logic.ifElse (Equality.lte x 5) (Just (Math.add x 1)) Nothing) (\y -> Logic.ifElse (Equality.gte y 5) (Just (Math.mul y 2)) Nothing) 3)),
                Testing.universalTestCaseExpected = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) Nothing)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "fromMaybe",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "just value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\n -> Literals.showInt32 n) (Maybes.fromMaybe 0 (Just 42))),
                Testing.universalTestCaseExpected = (\_ -> (\n -> Literals.showInt32 n) 42)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nothing with default",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\n -> Literals.showInt32 n) (Maybes.fromMaybe 99 Nothing)),
                Testing.universalTestCaseExpected = (\_ -> (\n -> Literals.showInt32 n) 99)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "isJust",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "just value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Maybes.isJust (Just 42))),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nothing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Maybes.isJust Nothing)),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "isNothing",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "just value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Maybes.isNothing (Just 42))),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nothing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\b -> Literals.showBoolean b) (Maybes.isNothing Nothing)),
                Testing.universalTestCaseExpected = (\_ -> (\b -> Literals.showBoolean b) True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "map",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "maps just value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) (Maybes.map (\x -> Math.mul x 2) (Just 5))),
                Testing.universalTestCaseExpected = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) (Just 10))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nothing unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) (Maybes.map (\x -> Math.mul x 2) Nothing)),
                Testing.universalTestCaseExpected = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) Nothing)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "mapMaybe",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "filter and transform",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (Maybes.mapMaybe (\x -> Logic.ifElse (Equality.gt x 2) (Just (Math.mul x 2)) Nothing) [
                  1,
                  2,
                  3,
                  4,
                  5])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) [
                  6,
                  8,
                  10])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty result",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (Maybes.mapMaybe (\x -> Logic.ifElse (Equality.gt x 2) (Just (Math.mul x 2)) Nothing) [
                  1,
                  2])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty input",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (Maybes.mapMaybe (\x -> Logic.ifElse (Equality.gt x 2) (Just (Math.mul x 2)) Nothing) [])),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "maybe",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "just value applies function",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\n -> Literals.showInt32 n) (Maybes.maybe 0 (\x -> Math.mul x 2) (Just 5))),
                Testing.universalTestCaseExpected = (\_ -> (\n -> Literals.showInt32 n) 10)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nothing returns default",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\n -> Literals.showInt32 n) (Maybes.maybe 99 (\x -> Math.mul x 2) Nothing)),
                Testing.universalTestCaseExpected = (\_ -> (\n -> Literals.showInt32 n) 99)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "pure",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "wraps integer",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) (Maybes.pure 42)),
                Testing.universalTestCaseExpected = (\_ -> (\mx -> ShowCore.maybe (\n -> Literals.showInt32 n) mx) (Just 42))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "wraps string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\mx -> ShowCore.maybe (\s -> Literals.showString s) mx) (Maybes.pure "hello")),
                Testing.universalTestCaseExpected = (\_ -> (\mx -> ShowCore.maybe (\s -> Literals.showString s) mx) (Just "hello"))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "toList",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "just value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (Maybes.toList (Just 42))),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) [
                  42])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nothing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) (Maybes.toList Nothing)),
                Testing.universalTestCaseExpected = (\_ -> (\xs -> ShowCore.list (\n -> Literals.showInt32 n) xs) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
