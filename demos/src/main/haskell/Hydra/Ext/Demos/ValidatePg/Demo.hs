-- | Haskell driver for the PG validation translingual demo.
--
-- Reads a schema JSON file and graph JSON files (produced by GenerateData using
-- hydra.encode.pg.model), validates each graph against the schema using
-- Hydra.Validate.Pg, and prints the results.
--
-- Usage: runhaskell ValidateDemo.hs <data-directory>

module Hydra.Ext.Demos.ValidatePg.Demo where

import qualified Hydra.Core as Core
import qualified Hydra.Pg.Model as Pg
import qualified Hydra.Validate.Pg as Validation
import qualified Hydra.Error.Pg as Err
import qualified Hydra.Json.Model as Json

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Key as AK
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as M
import qualified Data.Scientific as SC
import qualified Data.Text as T
import qualified Data.Vector as V

import System.CPUTime (getCPUTime)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, stdout, hPutStrLn, stderr)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [dataDir] -> runDemo dataDir
    _ -> do
      hPutStrLn stderr "Usage: ValidateDemo <data-directory>"
      exitFailure

runDemo :: FilePath -> IO ()
runDemo dataDir = do
  -- Load data (I/O, not timed)
  schemaJson <- loadJsonFile (dataDir </> "schema.json")
  let schema = decodeGraphSchema schemaJson
  graphData <- mapM (loadGraph dataDir) graphNames
  let loaded = [(n, g) | (n, Just g) <- zip graphNames graphData]

  -- Validate (timed: Hydra computation only)
  startTime <- getCPUTime
  mapM_ (uncurry (printResult schema)) loaded
  endTime <- getCPUTime

  let elapsedPs = endTime - startTime
      elapsedMs = fromIntegral elapsedPs / 1e9 :: Double
  hPutStrLn stderr $ "HYDRA_TIME_MS=" ++ show elapsedMs

loadGraph :: FilePath -> String -> IO (Maybe (Pg.Graph Core.Literal))
loadGraph dataDir name = do
  let path = dataDir </> name ++ ".json"
  exists <- doesFileExist path
  if exists
    then do
      graphJson <- loadJsonFile path
      return (Just (decodeGraph graphJson))
    else return Nothing

printResult :: Pg.GraphSchema Core.LiteralType -> String -> Pg.Graph Core.Literal -> IO ()
printResult schema name graph = do
  let result = Validation.validateGraph checkLiteral schema graph
  case result of
    Nothing  -> log $ "Graph \"" ++ name ++ "\": VALID"
    Just err -> log $ "Graph \"" ++ name ++ "\": INVALID - " ++ show err
  where
    log msg = putStrLn msg >> hFlush stdout

graphNames :: [String]
graphNames =
  [ "valid_social_network"
  , "missing_required_property"
  , "wrong_id_type"
  , "unknown_edge_endpoint"
  , "unexpected_vertex_label"
  , "unexpected_edge_label"
  , "property_value_type_mismatch"
  , "unexpected_property_key"
  , "wrong_in_vertex_label"
  , "wrong_out_vertex_label"
  , "missing_required_edge_property"
  ]

-- | Check that a literal value matches a literal type.
checkLiteral :: Core.LiteralType -> Core.Literal -> Maybe Err.InvalidValueError
checkLiteral lt lv = if showLiteralType lt == literalFamily lv
  then Nothing
  else Just $ Err.InvalidValueError {
    Err.invalidValueErrorExpectedType = showLiteralType lt,
    Err.invalidValueErrorValue = showLiteral lv }

showLiteralType :: Core.LiteralType -> String
showLiteralType lt = case lt of
  Core.LiteralTypeBinary    -> "binary"
  Core.LiteralTypeBoolean   -> "boolean"
  Core.LiteralTypeString    -> "string"
  Core.LiteralTypeFloat ft  -> "float:" ++ showFloatType ft
  Core.LiteralTypeInteger it -> "integer:" ++ showIntegerType it

showFloatType :: Core.FloatType -> String
showFloatType ft = case ft of
  Core.FloatTypeBigfloat -> "bigfloat"
  Core.FloatTypeFloat32  -> "float32"
  Core.FloatTypeFloat64  -> "float64"

showIntegerType :: Core.IntegerType -> String
showIntegerType it = case it of
  Core.IntegerTypeBigint -> "bigint"
  Core.IntegerTypeInt8   -> "int8"
  Core.IntegerTypeInt16  -> "int16"
  Core.IntegerTypeInt32  -> "int32"
  Core.IntegerTypeInt64  -> "int64"
  Core.IntegerTypeUint8  -> "uint8"
  Core.IntegerTypeUint16 -> "uint16"
  Core.IntegerTypeUint32 -> "uint32"
  Core.IntegerTypeUint64 -> "uint64"

showLiteral :: Core.Literal -> String
showLiteral l = case l of
  Core.LiteralBinary _    -> "binary:..."
  Core.LiteralBoolean b   -> "boolean:" ++ show b
  Core.LiteralString s    -> "string:\"" ++ s ++ "\""
  Core.LiteralFloat fv    -> showFloatValue fv
  Core.LiteralInteger iv  -> showIntegerValue iv

showFloatValue :: Core.FloatValue -> String
showFloatValue fv = case fv of
  Core.FloatValueBigfloat d -> "float:bigfloat:" ++ show d
  Core.FloatValueFloat32 f  -> "float:float32:" ++ show f
  Core.FloatValueFloat64 d  -> "float:float64:" ++ show d

showIntegerValue :: Core.IntegerValue -> String
showIntegerValue iv = case iv of
  Core.IntegerValueBigint n  -> "integer:bigint:" ++ show n
  Core.IntegerValueInt8 n    -> "integer:int8:" ++ show n
  Core.IntegerValueInt16 n   -> "integer:int16:" ++ show n
  Core.IntegerValueInt32 n   -> "integer:int32:" ++ show n
  Core.IntegerValueInt64 n   -> "integer:int64:" ++ show n
  Core.IntegerValueUint8 n   -> "integer:uint8:" ++ show n
  Core.IntegerValueUint16 n  -> "integer:uint16:" ++ show n
  Core.IntegerValueUint32 n  -> "integer:uint32:" ++ show n
  Core.IntegerValueUint64 n  -> "integer:uint64:" ++ show n

literalFamily :: Core.Literal -> String
literalFamily l = case l of
  Core.LiteralBinary _    -> "binary"
  Core.LiteralBoolean _   -> "boolean"
  Core.LiteralString _    -> "string"
  Core.LiteralFloat fv    -> "float:" ++ floatFamily fv
  Core.LiteralInteger iv  -> "integer:" ++ integerFamily iv
  where
    floatFamily fv = case fv of
      Core.FloatValueBigfloat _ -> "bigfloat"
      Core.FloatValueFloat32 _  -> "float32"
      Core.FloatValueFloat64 _  -> "float64"
    integerFamily iv = case iv of
      Core.IntegerValueBigint _  -> "bigint"
      Core.IntegerValueInt8 _    -> "int8"
      Core.IntegerValueInt16 _   -> "int16"
      Core.IntegerValueInt32 _   -> "int32"
      Core.IntegerValueInt64 _   -> "int64"
      Core.IntegerValueUint8 _   -> "uint8"
      Core.IntegerValueUint16 _  -> "uint16"
      Core.IntegerValueUint32 _  -> "uint32"
      Core.IntegerValueUint64 _  -> "uint64"


-- ============================================================================
-- JSON parsing (Aeson → Hydra JSON)
-- ============================================================================

loadJsonFile :: FilePath -> IO Json.Value
loadJsonFile fp = do
  content <- BS.readFile fp
  case A.eitherDecode content of
    Left err -> fail $ "JSON parse error in " ++ fp ++ ": " ++ err
    Right v  -> return (aesonToHydra v)

aesonToHydra :: A.Value -> Json.Value
aesonToHydra v = case v of
  A.Object km -> Json.ValueObject $ M.fromList [(AK.toString k, aesonToHydra v') | (k, v') <- AKM.toList km]
  A.Array a   -> Json.ValueArray [aesonToHydra x | x <- V.toList a]
  A.String t  -> Json.ValueString (T.unpack t)
  A.Number s  -> Json.ValueNumber (SC.toRealFloat s)
  A.Bool b    -> Json.ValueBoolean b
  A.Null      -> Json.ValueNull


-- ============================================================================
-- JSON → PG model decoders
-- ============================================================================

decodeGraphSchema :: Json.Value -> Pg.GraphSchema Core.LiteralType
decodeGraphSchema json = Pg.GraphSchema
  (decodeMap (requireField obj "vertices") (Pg.VertexLabel . expectString) decodeVertexType)
  (decodeMap (requireField obj "edges") (Pg.EdgeLabel . expectString) decodeEdgeType)
  where obj = expectObject json

decodeGraph :: Json.Value -> Pg.Graph Core.Literal
decodeGraph json = Pg.Graph
  (decodeMap (requireField obj "vertices") decodeLiteral decodeVertex)
  (decodeMap (requireField obj "edges") decodeLiteral decodeEdge)
  where obj = expectObject json

decodeVertexType :: Json.Value -> Pg.VertexType Core.LiteralType
decodeVertexType json = Pg.VertexType
  (Pg.VertexLabel $ expectString $ requireField obj "label")
  (decodeLiteralType $ requireField obj "id")
  (decodeList (requireField obj "properties") decodePropertyType)
  where obj = expectObject json

decodeEdgeType :: Json.Value -> Pg.EdgeType Core.LiteralType
decodeEdgeType json = Pg.EdgeType
  (Pg.EdgeLabel $ expectString $ requireField obj "label")
  (decodeLiteralType $ requireField obj "id")
  (Pg.VertexLabel $ expectString $ requireField obj "out")
  (Pg.VertexLabel $ expectString $ requireField obj "in")
  (decodeList (requireField obj "properties") decodePropertyType)
  where obj = expectObject json

decodePropertyType :: Json.Value -> Pg.PropertyType Core.LiteralType
decodePropertyType json = Pg.PropertyType
  (Pg.PropertyKey $ expectString $ requireField obj "key")
  (decodeLiteralType $ requireField obj "value")
  (expectBoolean $ requireField obj "required")
  where obj = expectObject json

decodeVertex :: Json.Value -> Pg.Vertex Core.Literal
decodeVertex json = Pg.Vertex
  (Pg.VertexLabel $ expectString $ requireField obj "label")
  (decodeLiteral $ requireField obj "id")
  (decodeMap (requireField obj "properties") (Pg.PropertyKey . expectString) decodeLiteral)
  where obj = expectObject json

decodeEdge :: Json.Value -> Pg.Edge Core.Literal
decodeEdge json = Pg.Edge
  (Pg.EdgeLabel $ expectString $ requireField obj "label")
  (decodeLiteral $ requireField obj "id")
  (decodeLiteral $ requireField obj "out")
  (decodeLiteral $ requireField obj "in")
  (decodeMap (requireField obj "properties") (Pg.PropertyKey . expectString) decodeLiteral)
  where obj = expectObject json

decodeLiteralType :: Json.Value -> Core.LiteralType
decodeLiteralType json
  | hasField obj "binary"  = Core.LiteralTypeBinary
  | hasField obj "boolean" = Core.LiteralTypeBoolean
  | hasField obj "string"  = Core.LiteralTypeString
  | hasField obj "float"   = Core.LiteralTypeFloat $ decodeFloatType (requireField obj "float")
  | hasField obj "integer" = Core.LiteralTypeInteger $ decodeIntegerType (requireField obj "integer")
  | otherwise = error $ "Unknown literal type"
  where obj = expectObject json

decodeFloatType :: Json.Value -> Core.FloatType
decodeFloatType json
  | hasField obj "bigfloat" = Core.FloatTypeBigfloat
  | hasField obj "float32"  = Core.FloatTypeFloat32
  | hasField obj "float64"  = Core.FloatTypeFloat64
  | otherwise = error "Unknown float type"
  where obj = expectObject json

decodeIntegerType :: Json.Value -> Core.IntegerType
decodeIntegerType json
  | hasField obj "bigint" = Core.IntegerTypeBigint
  | hasField obj "int8"   = Core.IntegerTypeInt8
  | hasField obj "int16"  = Core.IntegerTypeInt16
  | hasField obj "int32"  = Core.IntegerTypeInt32
  | hasField obj "int64"  = Core.IntegerTypeInt64
  | hasField obj "uint8"  = Core.IntegerTypeUint8
  | hasField obj "uint16" = Core.IntegerTypeUint16
  | hasField obj "uint32" = Core.IntegerTypeUint32
  | hasField obj "uint64" = Core.IntegerTypeUint64
  | otherwise = error "Unknown integer type"
  where obj = expectObject json

decodeLiteral :: Json.Value -> Core.Literal
decodeLiteral json
  | hasField obj "binary"  = Core.LiteralBinary $ BS8.pack $ expectString (requireField obj "binary")
  | hasField obj "boolean" = Core.LiteralBoolean $ expectBoolean (requireField obj "boolean")
  | hasField obj "string"  = Core.LiteralString $ expectString (requireField obj "string")
  | hasField obj "float"   = Core.LiteralFloat $ decodeFloatValue (requireField obj "float")
  | hasField obj "integer" = Core.LiteralInteger $ decodeIntegerValue (requireField obj "integer")
  | otherwise = error "Unknown literal"
  where obj = expectObject json

decodeFloatValue :: Json.Value -> Core.FloatValue
decodeFloatValue json
  | hasField obj "bigfloat" = Core.FloatValueBigfloat $ expectNumber (requireField obj "bigfloat")
  | hasField obj "float32"  = Core.FloatValueFloat32 $ realToFrac $ expectNumber (requireField obj "float32")
  | hasField obj "float64"  = Core.FloatValueFloat64 $ expectNumber (requireField obj "float64")
  | otherwise = error "Unknown float value"
  where obj = expectObject json

decodeIntegerValue :: Json.Value -> Core.IntegerValue
decodeIntegerValue json
  | hasField obj "bigint" = Core.IntegerValueBigint $ round $ expectNumber (requireField obj "bigint")
  | hasField obj "int8"   = Core.IntegerValueInt8 $ round $ expectNumber (requireField obj "int8")
  | hasField obj "int16"  = Core.IntegerValueInt16 $ round $ expectNumber (requireField obj "int16")
  | hasField obj "int32"  = Core.IntegerValueInt32 $ round $ expectNumber (requireField obj "int32")
  | hasField obj "int64"  = Core.IntegerValueInt64 $ round $ expectNumber (requireField obj "int64")
  | hasField obj "uint8"  = Core.IntegerValueUint8 $ round $ expectNumber (requireField obj "uint8")
  | hasField obj "uint16" = Core.IntegerValueUint16 $ round $ expectNumber (requireField obj "uint16")
  | hasField obj "uint32" = Core.IntegerValueUint32 $ round $ expectNumber (requireField obj "uint32")
  | hasField obj "uint64" = Core.IntegerValueUint64 $ round $ expectNumber (requireField obj "uint64")
  | otherwise = error "Unknown integer value"
  where obj = expectObject json


-- ============================================================================
-- JSON helpers
-- ============================================================================

decodeMap :: Ord k => Json.Value -> (Json.Value -> k) -> (Json.Value -> v) -> M.Map k v
decodeMap json decodeKey decodeValue = M.fromList $ fmap decodePair entries
  where
    entries = expectArray json
    decodePair entry = (decodeKey (requireField obj "@key"), decodeValue (requireField obj "@value"))
      where obj = expectObject entry

decodeList :: Json.Value -> (Json.Value -> a) -> [a]
decodeList json decode = fmap decode (expectArray json)

expectObject :: Json.Value -> M.Map String Json.Value
expectObject (Json.ValueObject m) = m
expectObject _ = error "Expected JSON object"

expectArray :: Json.Value -> [Json.Value]
expectArray (Json.ValueArray a) = a
expectArray _ = error "Expected JSON array"

expectString :: Json.Value -> String
expectString (Json.ValueString s) = s
expectString _ = error "Expected JSON string"

expectBoolean :: Json.Value -> Bool
expectBoolean (Json.ValueBoolean b) = b
expectBoolean _ = error "Expected JSON boolean"

expectNumber :: Json.Value -> Double
expectNumber (Json.ValueNumber n) = n
expectNumber _ = error "Expected JSON number"

requireField :: M.Map String Json.Value -> String -> Json.Value
requireField obj name = case M.lookup name obj of
  Just v  -> v
  Nothing -> error $ "Missing required field: " ++ name

hasField :: M.Map String Json.Value -> String -> Bool
hasField obj name = M.member name obj
