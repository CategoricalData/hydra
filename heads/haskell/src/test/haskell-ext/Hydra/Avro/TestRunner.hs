-- | Test runner for AvroTestCase. Dispatches on the test case variant and executes the check.

module Hydra.Avro.TestRunner (
  runAvroTestCase,
  runAvroTestCases,
) where

import Hydra.Kernel
import qualified Hydra.Core as Core
import qualified Hydra.Avro.Schema as Schema
import qualified Hydra.Json.Model as Json
import qualified Hydra.Avro.Coder as AvroCoder
import qualified Hydra.Avro.Encoder as Encoder
import qualified Hydra.Avro.SchemaJson as SchemaJson
import qualified Hydra.Avro.Testing as T
import qualified Hydra.Util as Util

import qualified Test.Hspec as H
import qualified Data.Map as M


-- | The context used for all tests
testContext :: Context
testContext = emptyContext

-- | Run all test cases under a single HSpec describe block
runAvroTestCases :: String -> [T.AvroTestCase] -> H.SpecWith ()
runAvroTestCases groupName cases =
  H.describe groupName $ mapM_ runAvroTestCase cases

-- | Run a single AvroTestCase, creating an HSpec `it` block with the test description
runAvroTestCase :: T.AvroTestCase -> H.SpecWith ()
runAvroTestCase tc = case tc of
  T.AvroTestCaseTypeLevelForward c -> runTypeLevelForward c
  T.AvroTestCaseTypeLevelReverse c -> runTypeLevelReverse c
  T.AvroTestCaseTypeLevelRoundTripAvro c -> runTypeLevelRoundTripAvro c
  T.AvroTestCaseTypeLevelRoundTripHydra c -> runTypeLevelRoundTripHydra c
  T.AvroTestCaseTermLevelForward c -> runTermLevelForward c
  T.AvroTestCaseTermLevelReverse c -> runTermLevelReverse c
  T.AvroTestCaseTermLevelRoundTripJson c -> runTermLevelRoundTripJson c
  T.AvroTestCaseTermLevelRoundTripTerm c -> runTermLevelRoundTripTerm c
  T.AvroTestCaseUnion c -> runUnion c
  T.AvroTestCaseNameMapping c -> runNameMapping c
  T.AvroTestCaseLossiness c -> runLossiness c
  T.AvroTestCaseSchemaSerialization c -> runSchemaSerialization c

-- Forward adapter helpers

forwardAdapt :: Schema.Schema -> Either String Core.Type
forwardAdapt schema = case AvroCoder.avroHydraAdapter testContext schema AvroCoder.emptyAvroEnvironment of
  Left err -> Left (show err)
  Right (adapter, _env) -> Right (Util.adapterTarget adapter)

forwardAdapter :: Schema.Schema -> Either String (Util.Adapter Schema.Schema Core.Type Json.Value Core.Term)
forwardAdapter schema = case AvroCoder.avroHydraAdapter testContext schema AvroCoder.emptyAvroEnvironment of
  Left err -> Left (show err)
  Right (adapter, _env) -> Right adapter

-- Reverse adapter helpers (use empty type map for simple/anonymous types)

reverseAdapt :: Core.Type -> Either String Schema.Schema
reverseAdapt typ = case Encoder.hydraAvroAdapter testContext M.empty typ of
  Left err -> Left (show err)
  Right adapter -> Right (Util.adapterTarget adapter)

reverseAdapter :: Core.Type -> Either String (Util.Adapter Core.Type Schema.Schema Core.Term Json.Value)
reverseAdapter typ = case Encoder.hydraAvroAdapter testContext M.empty typ of
  Left err -> Left (show err)
  Right adapter -> Right adapter


-- Category 1: Type-level forward (Avro Schema -> Hydra Type)
runTypeLevelForward :: T.TypeLevelForwardTestCase -> H.SpecWith ()
runTypeLevelForward tc =
  H.it (T.typeLevelForwardTestCaseDescription tc) $ do
    let result = forwardAdapt (T.typeLevelForwardTestCaseSchema tc)
    result `H.shouldBe` Right (T.typeLevelForwardTestCaseType tc)

-- Category 2: Type-level reverse (Hydra Type -> Avro Schema)
runTypeLevelReverse :: T.TypeLevelReverseTestCase -> H.SpecWith ()
runTypeLevelReverse tc =
  H.it (T.typeLevelReverseTestCaseDescription tc) $ do
    let result = reverseAdapt (T.typeLevelReverseTestCaseType tc)
    result `H.shouldBe` Right (T.typeLevelReverseTestCaseSchema tc)

-- Category 3: Type-level round-trip (Avro -> Hydra -> Avro)
runTypeLevelRoundTripAvro :: T.TypeLevelRoundTripAvroTestCase -> H.SpecWith ()
runTypeLevelRoundTripAvro tc =
  H.it (T.typeLevelRoundTripAvroTestCaseDescription tc) $ do
    case forwardAdapt (T.typeLevelRoundTripAvroTestCaseSchema tc) of
      Left err -> H.expectationFailure $ "forward adapt failed: " ++ err
      Right hydraType -> do
        let result = reverseAdapt hydraType
        result `H.shouldBe` Right (T.typeLevelRoundTripAvroTestCaseExpectedSchema tc)

-- Category 4: Type-level round-trip (Hydra -> Avro -> Hydra)
runTypeLevelRoundTripHydra :: T.TypeLevelRoundTripHydraTestCase -> H.SpecWith ()
runTypeLevelRoundTripHydra tc =
  H.it (T.typeLevelRoundTripHydraTestCaseDescription tc) $ do
    case reverseAdapt (T.typeLevelRoundTripHydraTestCaseType tc) of
      Left err -> H.expectationFailure $ "reverse adapt failed: " ++ err
      Right avroSchema -> do
        let result = forwardAdapt avroSchema
        result `H.shouldBe` Right (T.typeLevelRoundTripHydraTestCaseExpectedType tc)

-- Category 5: Term-level forward (JSON -> Hydra Term)
runTermLevelForward :: T.TermLevelForwardTestCase -> H.SpecWith ()
runTermLevelForward tc =
  H.it (T.termLevelForwardTestCaseDescription tc) $ do
    case forwardAdapter (T.termLevelForwardTestCaseSchema tc) of
      Left err -> H.expectationFailure $ "adapter creation failed: " ++ err
      Right adapter -> do
        let result = Util.coderEncode (Util.adapterCoder adapter) testContext (T.termLevelForwardTestCaseJson tc)
        case result of
          Left err -> H.expectationFailure $ "encode failed: " ++ show err
          Right term -> term `H.shouldBe` T.termLevelForwardTestCaseTerm tc

-- Category 6: Term-level reverse (Hydra Term -> JSON)
runTermLevelReverse :: T.TermLevelReverseTestCase -> H.SpecWith ()
runTermLevelReverse tc =
  H.it (T.termLevelReverseTestCaseDescription tc) $ do
    case forwardAdapter (T.termLevelReverseTestCaseSchema tc) of
      Left err -> H.expectationFailure $ "adapter creation failed: " ++ err
      Right adapter -> do
        let result = Util.coderDecode (Util.adapterCoder adapter) testContext (T.termLevelReverseTestCaseTerm tc)
        case result of
          Left err -> H.expectationFailure $ "decode failed: " ++ show err
          Right json -> json `H.shouldBe` T.termLevelReverseTestCaseJson tc

-- Category 7: Term-level round-trip (JSON -> Term -> JSON)
runTermLevelRoundTripJson :: T.TermLevelRoundTripJsonTestCase -> H.SpecWith ()
runTermLevelRoundTripJson tc =
  H.it (T.termLevelRoundTripJsonTestCaseDescription tc) $ do
    case forwardAdapter (T.termLevelRoundTripJsonTestCaseSchema tc) of
      Left err -> H.expectationFailure $ "adapter creation failed: " ++ err
      Right adapter -> do
        let coder = Util.adapterCoder adapter
        case Util.coderEncode coder testContext (T.termLevelRoundTripJsonTestCaseJson tc) of
          Left err -> H.expectationFailure $ "encode failed: " ++ show err
          Right term -> do
            case Util.coderDecode coder testContext term of
              Left err -> H.expectationFailure $ "decode failed: " ++ show err
              Right json -> json `H.shouldBe` T.termLevelRoundTripJsonTestCaseExpectedJson tc

-- Category 8: Term-level round-trip (Term -> JSON -> Term)
runTermLevelRoundTripTerm :: T.TermLevelRoundTripTermTestCase -> H.SpecWith ()
runTermLevelRoundTripTerm tc =
  H.it (T.termLevelRoundTripTermTestCaseDescription tc) $ do
    case reverseAdapter (T.termLevelRoundTripTermTestCaseType tc) of
      Left err -> H.expectationFailure $ "reverse adapter creation failed: " ++ err
      Right adapter -> do
        let coder = Util.adapterCoder adapter
        case Util.coderEncode coder testContext (T.termLevelRoundTripTermTestCaseTerm tc) of
          Left err -> H.expectationFailure $ "encode (term->json) failed: " ++ show err
          Right json -> do
            case Util.coderDecode coder testContext json of
              Left err -> H.expectationFailure $ "decode (json->term) failed: " ++ show err
              Right term -> term `H.shouldBe` T.termLevelRoundTripTermTestCaseExpectedTerm tc

-- Category 9: Union-specific tests
runUnion :: T.UnionTestCase -> H.SpecWith ()
runUnion tc =
  H.it (T.unionTestCaseDescription tc) $ do
    -- Test the reverse direction: Hydra type -> Avro schema
    case reverseAdapter (T.unionTestCaseHydraType tc) of
      Left err -> H.expectationFailure $ "reverse adapter creation failed: " ++ err
      Right adapter -> do
        -- Check each term/json pair
        mapM_ (checkTermPair adapter) (T.unionTestCaseTermPairs tc)
  where
    checkTermPair adapter (term, expectedJson) = do
      case Util.coderEncode (Util.adapterCoder adapter) testContext term of
        Left err -> H.expectationFailure $ "encode failed: " ++ show err
        Right json -> json `H.shouldBe` expectedJson

-- Category 10: Name mapping tests
runNameMapping :: T.NameMappingTestCase -> H.SpecWith ()
runNameMapping tc =
  H.it (T.nameMappingTestCaseDescription tc) $ do
    let hydraName = T.nameMappingTestCaseHydraName tc
    let qname = AvroCoder.parseAvroName (T.nameMappingTestCaseAvroNamespace tc) (T.nameMappingTestCaseAvroName tc)
    let result = AvroCoder.avroNameToHydraName qname
    result `H.shouldBe` hydraName

-- Category 11: Lossiness tests
runLossiness :: T.LossinessTestCase -> H.SpecWith ()
runLossiness tc =
  H.it (T.lossinessTestCaseDescription tc) $ do
    case reverseAdapter (T.lossinessTestCaseHydraType tc) of
      Left err -> H.expectationFailure $ "reverse adapter creation failed: " ++ err
      Right adapter -> do
        Util.adapterIsLossy adapter `H.shouldBe` T.lossinessTestCaseIsLossy tc

-- Category 12: Schema serialization tests
runSchemaSerialization :: T.SchemaSerializationTestCase -> H.SpecWith ()
runSchemaSerialization tc =
  H.it (T.schemaSerializationTestCaseDescription tc) $ do
    let result = SchemaJson.encodeSchema (T.schemaSerializationTestCaseSchema tc)
    result `H.shouldBe` T.schemaSerializationTestCaseJson tc
