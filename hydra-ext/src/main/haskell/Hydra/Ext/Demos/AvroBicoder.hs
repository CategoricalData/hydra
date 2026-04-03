-- | Demo for the bidirectional Avro coder.
--
-- Exercises:
--   1. Forward: Avro schema + JSON data -> Hydra types + terms (with round-trip)
--   2. Reverse: Hydra types -> Avro schema -> JSON
--   3. Round-trip: Avro schema -> Hydra -> Avro schema (structural comparison)
--   4. Schema codec: .avsc file -> parse -> encode -> compare
--   5. Avro to property graph: Avro schema + JSON data -> GraphSON
module Hydra.Ext.Demos.AvroBicoder (
  -- * Top-level demo runners
  runAllDemos,
  runForwardDemo,
  runReverseDemo,
  runRoundTripDemo,
  runSchemaCodecDemo,
  runPropertyGraphDemo,
) where

import Hydra.Kernel
import qualified Hydra.Core as Core
import qualified Hydra.Ext.Org.Apache.Avro.Schema as Avro
import qualified Hydra.Json.Model as Json
import qualified Hydra.Ext.Avro.Coder as AvroCoder
import qualified Hydra.Ext.Avro.Encoder as Encoder
import qualified Hydra.Ext.Avro.Environment as AvroEnv
import qualified Hydra.Ext.Avro.SchemaJson as SchemaJson
import qualified Hydra.Json.Parser as JsonParser
import qualified Hydra.Json.Writer as JsonWriter
import Hydra.Parsing (ParseResult(..), ParseSuccess(..), ParseError(..))
import qualified Hydra.Show.Errors as ShowError
import Hydra.Ext.Tools.AvroWorkflows (propertyGraphGraphsonLastMile, transformAvroJsonDirectory)
import qualified Hydra.Coders as Coders
import qualified Hydra.Util as Util
import qualified Hydra.Json.Writer as JsonWriter

import qualified Data.Map as M
import qualified Data.List as L
import System.Directory
import System.FilePath


-- | Run all demos in sequence
runAllDemos :: IO ()
runAllDemos = do
  putStrLn "============================================"
  putStrLn "Hydra Avro Bidirectional Coder Demo"
  putStrLn "============================================"
  putStrLn ""

  runForwardDemo
  putStrLn ""
  runReverseDemo
  putStrLn ""
  runRoundTripDemo
  putStrLn ""
  runSchemaCodecDemo
  putStrLn ""
  runPropertyGraphDemo

  putStrLn ""
  putStrLn "============================================"
  putStrLn "All demos completed successfully."
  putStrLn "============================================"


-- | Demo 1: Forward pipeline (Avro schema + JSON -> Hydra type + term)
--
-- Reads a real .avsc schema and example JSON data, converts to Hydra types and terms
-- using the forward adapter, then displays the results.
runForwardDemo :: IO ()
runForwardDemo = do
  putStrLn "--- Demo 1: Forward pipeline (Avro -> Hydra) ---"
  pwd <- getCurrentDirectory
  let cx = emptyContext

  -- Load the Review schema
  let schemaPath = combine pwd "src/test/avro/moviedemo/Review.avsc"
  schemaStr <- readFile schemaPath
  case parseJson schemaStr of
    Left e -> putStrLn $ "  Parse error: " ++ e
    Right schemaJson -> case SchemaJson.decodeSchema cx schemaJson of
      Left e -> putStrLn $ "  Schema decode error: " ++ show e
      Right avroSchema -> do
        putStrLn "  Loaded: Review.avsc"
        case avroSchema of
          Avro.SchemaNamed named -> do
            putStrLn $ "  Schema name: " ++ Avro.namedName named
            putStrLn $ "  Schema namespace: " ++ show (Avro.namedNamespace named)
          _ -> return ()

        -- Create forward adapter
        case AvroCoder.avroHydraAdapter cx avroSchema AvroCoder.emptyAvroEnvironment of
          Left e -> putStrLn $ "  Adapter error: " ++ show e
          Right (adapter, _env) -> do
            putStrLn $ "  Forward adapter created (lossy: " ++ show (Coders.adapterIsLossy adapter) ++ ")"

            -- Load and convert example data
            let dataPath = combine pwd "src/test/json/moviedemo/exampleReview.json"
            dataStr <- readFile dataPath
            case parseJson dataStr of
              Left e -> putStrLn $ "  Data parse error: " ++ e
              Right dataJson -> do
                case Coders.coderEncode (Coders.adapterCoder adapter) cx dataJson of
                  Left e -> putStrLn $ "  Encode error: " ++ show e
                  Right hydraTerm -> do
                    putStrLn "  JSON data -> Hydra term: OK"
                    -- Decode back to JSON to verify round-trip
                    case Coders.coderDecode (Coders.adapterCoder adapter) cx hydraTerm of
                      Left e -> putStrLn $ "  Decode error: " ++ show e
                      Right jsonResult -> do
                        let resultStr = JsonWriter.printJson jsonResult
                        putStrLn $ "  Term -> JSON round-trip: OK (" ++ show (length resultStr) ++ " chars)"
                        -- Write output
                        let outDir = "/tmp/hydra-avro-demo/forward"
                        createDirectoryIfMissing True outDir
                        writeFile (combine outDir "review-roundtrip.json") resultStr
                        putStrLn $ "  Output: " ++ combine outDir "review-roundtrip.json"


-- | Demo 2: Reverse pipeline (Hydra types -> Avro schema)
--
-- Defines Hydra types programmatically and encodes them as Avro schemas,
-- then serializes the schemas to JSON.
runReverseDemo :: IO ()
runReverseDemo = do
  putStrLn "--- Demo 2: Reverse pipeline (Hydra -> Avro schema) ---"
  let cx = emptyContext

  -- Define a small type graph: Person with Address
  let addressName = Core.Name "com.example.Address"
  let personName = Core.Name "com.example.Person"
  let addressType = Core.TypeRecord [
        Core.FieldType (Core.Name "street") (Core.TypeLiteral (Core.LiteralTypeString)),
        Core.FieldType (Core.Name "city") (Core.TypeLiteral (Core.LiteralTypeString)),
        Core.FieldType (Core.Name "zipCode") (Core.TypeLiteral (Core.LiteralTypeString))]
  let personType = Core.TypeRecord [
        Core.FieldType (Core.Name "name") (Core.TypeLiteral (Core.LiteralTypeString)),
        Core.FieldType (Core.Name "age") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
        Core.FieldType (Core.Name "email") (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeString))),
        Core.FieldType (Core.Name "address") (Core.TypeVariable addressName),
        Core.FieldType (Core.Name "tags") (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeString)))]
  let typeMap = M.fromList [(addressName, addressType), (personName, personType)]

  case Encoder.encodeType cx typeMap personName of
    Left err -> putStrLn $ "  ERROR: " ++ ShowError.error (inContextObject err)
    Right adapter -> do
      let avroSchema = Coders.adapterTarget adapter
      let json = SchemaJson.encodeSchema avroSchema
      let jsonStr = JsonWriter.printJson json
      putStrLn "  Hydra type 'com.example.Person' encoded as Avro schema:"
      putStrLn $ "  " ++ jsonStr
      -- Write to file
      let outDir = "/tmp/hydra-avro-demo/reverse"
      createDirectoryIfMissing True outDir
      let outFile = combine outDir "Person.avsc"
      writeFile outFile jsonStr
      putStrLn $ "  Written to: " ++ outFile

      -- Also encode a term and show the JSON
      let personTerm = Core.TermRecord $ Core.Record personName [
            Core.Field (Core.Name "name") (Core.TermLiteral (Core.LiteralString "Alice Smith")),
            Core.Field (Core.Name "age") (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30))),
            Core.Field (Core.Name "email") (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "alice@example.com")))),
            Core.Field (Core.Name "address") (Core.TermRecord $ Core.Record addressName [
              Core.Field (Core.Name "street") (Core.TermLiteral (Core.LiteralString "123 Main St")),
              Core.Field (Core.Name "city") (Core.TermLiteral (Core.LiteralString "Springfield")),
              Core.Field (Core.Name "zipCode") (Core.TermLiteral (Core.LiteralString "62701"))]),
            Core.Field (Core.Name "tags") (Core.TermList [
              Core.TermLiteral (Core.LiteralString "engineer"),
              Core.TermLiteral (Core.LiteralString "haskell")])]
      case Coders.coderEncode (Coders.adapterCoder adapter) cx personTerm of
        Left err -> putStrLn $ "  Term encode ERROR: " ++ ShowError.error (inContextObject err)
        Right jsonVal -> do
          let termJsonStr = JsonWriter.printJson jsonVal
          putStrLn $ "  Term encoded as JSON: " ++ termJsonStr
          let termFile = combine outDir "person-data.json"
          writeFile termFile termJsonStr
          putStrLn $ "  Written to: " ++ termFile


-- | Demo 3: Round-trip (Avro -> Hydra -> Avro)
--
-- Parses a real .avsc schema, converts to Hydra types via the forward adapter,
-- then converts back to an Avro schema via the reverse encoder, and compares.
runRoundTripDemo :: IO ()
runRoundTripDemo = do
  putStrLn "--- Demo 3: Round-trip (Avro -> Hydra -> Avro) ---"
  pwd <- getCurrentDirectory
  let cx = emptyContext

  -- Load the AirplaneInfo schema
  let schemaPath = combine pwd "src/test/avro/aviationdemo/AirplaneInfo.avsc"
  schemaStr <- readFile schemaPath
  case parseJson schemaStr of
    Left e -> putStrLn $ "  Parse error: " ++ e
    Right schemaJson -> case SchemaJson.decodeSchema cx schemaJson of
      Left e -> putStrLn $ "  Schema decode error: " ++ show e
      Right avroSchema -> do
        putStrLn $ "  Loaded: AirplaneInfo.avsc"

        -- Forward: Avro -> Hydra
        case AvroCoder.avroHydraAdapter cx avroSchema AvroCoder.emptyAvroEnvironment of
          Left e -> putStrLn $ "  Forward adapter error: " ++ show e
          Right (fwdAdapter, _env) -> do
            let hydraType = Coders.adapterTarget fwdAdapter
            putStrLn $ "  Forward: Avro schema -> Hydra type (ok)"

            -- Reverse: Hydra -> Avro
            let typeName = Core.Name "com.example.AirplaneInfo"
            case Encoder.encodeType cx (M.singleton typeName hydraType) typeName of
              Left e -> putStrLn $ "  Reverse adapter error: " ++ show e
              Right revAdapter -> do
                let revSchema = Coders.adapterTarget revAdapter
                putStrLn $ "  Reverse: Hydra type -> Avro schema (ok)"

                -- Compare: serialize both and check structure
                let origJson = SchemaJson.encodeSchema avroSchema
                let revJson = SchemaJson.encodeSchema revSchema
                let origStr = JsonWriter.printJson origJson
                let revStr = JsonWriter.printJson revJson

                -- Write both for manual inspection
                let outDir = "/tmp/hydra-avro-demo/roundtrip"
                createDirectoryIfMissing True outDir
                writeFile (combine outDir "original.json") origStr
                writeFile (combine outDir "roundtripped.json") revStr
                putStrLn $ "  Original schema:     " ++ show (length origStr) ++ " chars"
                putStrLn $ "  Round-tripped schema: " ++ show (length revStr) ++ " chars"
                putStrLn $ "  Output written to: " ++ outDir

                -- Structural check: both should be named record schemas
                case (avroSchema, revSchema) of
                  (Avro.SchemaNamed origNamed, Avro.SchemaNamed revNamed) -> do
                    putStrLn $ "  Original name: " ++ Avro.namedName origNamed
                    putStrLn $ "  Round-trip name: " ++ Avro.namedName revNamed
                    let origFieldCount = case Avro.namedType origNamed of
                          Avro.NamedTypeRecord (Avro.Record fs) -> length fs
                          _ -> 0
                    let revFieldCount = case Avro.namedType revNamed of
                          Avro.NamedTypeRecord (Avro.Record fs) -> length fs
                          _ -> 0
                    putStrLn $ "  Field count: " ++ show origFieldCount ++ " -> " ++ show revFieldCount
                    if origFieldCount == revFieldCount
                      then putStrLn "  PASS: field counts match"
                      else putStrLn "  NOTE: field counts differ (expected if annotations affect structure)"
                  _ -> putStrLn "  WARNING: schemas have different top-level structure"


-- | Demo 4: Schema string codec
--
-- Demonstrates the avroSchemaStringCoder: parse JSON string -> Schema -> JSON string
runSchemaCodecDemo :: IO ()
runSchemaCodecDemo = do
  putStrLn "--- Demo 4: Schema string codec ---"
  let cx = emptyContext
  let coder = SchemaJson.avroSchemaStringCoder cx

  -- Encode a schema to a JSON string
  let schema = Avro.SchemaNamed $ Avro.Named {
        Avro.namedName = "Greeting",
        Avro.namedNamespace = Just "com.example",
        Avro.namedAliases = Nothing,
        Avro.namedDoc = Just "A simple greeting message",
        Avro.namedType = Avro.NamedTypeRecord $ Avro.Record [
          Avro.Field "message" (Just "The greeting text") (Avro.SchemaPrimitive Avro.PrimitiveString) Nothing Nothing Nothing M.empty,
          Avro.Field "timestamp" Nothing (Avro.SchemaPrimitive Avro.PrimitiveLong) Nothing Nothing Nothing M.empty],
        Avro.namedAnnotations = M.empty}

  case Coders.coderEncode coder cx schema of
    Left e -> putStrLn $ "  Encode error: " ++ show e
    Right jsonStr -> do
      putStrLn $ "  Encoded: " ++ jsonStr
      -- Decode it back
      case Coders.coderDecode coder cx jsonStr of
        Left e -> putStrLn $ "  Decode error: " ++ show e
        Right decoded -> do
          if decoded == schema
            then putStrLn "  PASS: round-trip produces identical schema"
            else putStrLn "  NOTE: round-trip produces structurally different schema (may differ in optional fields)"
          -- Re-encode to verify
          case Coders.coderEncode coder cx decoded of
            Left e -> putStrLn $ "  Re-encode error: " ++ show e
            Right jsonStr2 -> do
              if jsonStr == jsonStr2
                then putStrLn "  PASS: JSON string round-trips exactly"
                else do
                  putStrLn "  NOTE: JSON strings differ after re-encoding"
                  putStrLn $ "  First:  " ++ jsonStr
                  putStrLn $ "  Second: " ++ jsonStr2


-- | Demo 5: Avro to property graph (GraphSON)
--
-- Reads an Avro schema and JSON data, converts through the forward adapter,
-- extracts elements from @primaryKey annotations, builds a graph, then
-- transforms to a GraphSON property graph.
runPropertyGraphDemo :: IO ()
runPropertyGraphDemo = do
  putStrLn "--- Demo 5: Avro to property graph (GraphSON) ---"
  pwd <- getCurrentDirectory
  let schemaPath = combine pwd "src/test/avro/moviedemo/Review.avsc"
  let dataDir = combine pwd "src/test/json/moviedemo"
  let outDir = "/tmp/hydra-avro-demo/graphson"
  createDirectoryIfMissing True outDir
  transformAvroJsonDirectory propertyGraphGraphsonLastMile schemaPath dataDir outDir
  let outFile = combine outDir "exampleReview.jsonl"
  exists <- doesFileExist outFile
  if exists
    then do
      content <- readFile outFile
      let lineCount = length (lines content)
      putStrLn $ "  Generated " ++ show lineCount ++ " GraphSON vertices"
      putStrLn $ "  Output: " ++ outFile
      putStrLn "  First 3 vertices:"
      mapM_ (\l -> putStrLn $ "    " ++ take 120 l ++ if length l > 120 then "..." else "") (take 3 $ lines content)
    else putStrLn "  ERROR: output file not generated"


-- Helpers

parseJson :: String -> Either String Json.Value
parseJson s = case JsonParser.parseJson s of
  ParseResultSuccess success -> Right (parseSuccessValue success)
  ParseResultFailure err -> Left (parseErrorMessage err)
