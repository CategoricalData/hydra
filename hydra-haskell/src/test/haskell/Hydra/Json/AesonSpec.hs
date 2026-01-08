-- | Cross-implementation round-trip tests for JSON serialization/parsing.
--   Tests that the new Hydra-native JSON implementation is compatible with
--   the existing Aeson-based implementation.

{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.Json.AesonSpec.spec
-}

module Hydra.Json.AesonSpec where

import Hydra.Json.Model (Value)
import Hydra.Testing
import Hydra.Parsing (ParseResult(..), ParseSuccess(..), ParseError(..))
import qualified Hydra.Json.Writer as NewSerde
import qualified Hydra.Json.Parser as NewParser
import qualified Hydra.Staging.Json.Serde as AesonSerde
import qualified Hydra.Test.Json.Writer as WriterTests
import qualified Hydra.Test.Json.Parser as ParserTests

import qualified Test.Hspec as H
import qualified Data.List as L


-- | Extract all JSON values from writer test cases (inputs)
extractWriterValues :: TestGroup -> [(String, Value)]
extractWriterValues group = concatMap extractFromCase (testGroupCases group)
    ++ concatMap extractWriterValues (testGroupSubgroups group)
  where
    extractFromCase tc = case testCaseWithMetadataCase tc of
      TestCaseJsonWriter (WriterTestCase input _) ->
        [(testCaseWithMetadataName tc, input)]
      _ -> []

-- | Extract all JSON values from successful parser test cases (expected outputs)
extractParserValues :: TestGroup -> [(String, Value)]
extractParserValues group = concatMap extractFromCase (testGroupCases group)
    ++ concatMap extractParserValues (testGroupSubgroups group)
  where
    extractFromCase tc = case testCaseWithMetadataCase tc of
      TestCaseJsonParser (ParserTestCase _ (ParseResultSuccess success)) ->
        [(testCaseWithMetadataName tc, parseSuccessValue success)]
      _ -> []

-- | All JSON values from both writer and parser test suites
allJsonValues :: [(String, Value)]
allJsonValues = L.nubBy (\(n1, _) (n2, _) -> n1 == n2) $
    extractWriterValues WriterTests.allTests
    ++ extractParserValues ParserTests.allTests

-- | Test: Aeson writer -> New parser -> Compare
aesonWriterNewParserRoundTrip :: H.SpecWith ()
aesonWriterNewParserRoundTrip = H.describe "Aeson writer -> New parser round trip" $ do
  mapM_ makeTest allJsonValues
  where
    makeTest (name, value) = H.it name $ do
      let serialized = AesonSerde.jsonValueToString value
      case NewParser.parseJson serialized of
        ParseResultSuccess success ->
          parseSuccessValue success `H.shouldBe` value
        ParseResultFailure err ->
          H.expectationFailure $ "Parse failed: " ++ parseErrorMessage err
            ++ " at: " ++ take 50 (parseErrorRemainder err)

-- | Test: New writer -> Aeson parser -> Compare
newWriterAesonParserRoundTrip :: H.SpecWith ()
newWriterAesonParserRoundTrip = H.describe "New writer -> Aeson parser round trip" $ do
  mapM_ makeTest allJsonValues
  where
    makeTest (name, value) = H.it name $ do
      let serialized = NewSerde.printJson value
      case AesonSerde.stringToJsonValue serialized of
        Right parsed -> parsed `H.shouldBe` value
        Left err -> H.expectationFailure $ "Parse failed: " ++ err

spec :: H.Spec
spec = do
  aesonWriterNewParserRoundTrip
  newWriterAesonParserRoundTrip
