-- | Haskell driver for the Neo4j validation translingual demo (JSON-artifact variant).
--
-- Reads the shared schema.json and each graph JSON file (produced by the Java
-- GenerateData from DSL definitions), decodes them back into hydra.neo4j.model
-- values, and validates each graph against the schema with
-- Hydra.Validate.Neo4j.validateGraph. The same validator -- generated from one
-- Hydra source -- runs identically in the Java and Python counterparts; because
-- every host reads the same JSON files, the data and the logic are identical
-- across languages by construction.
--
-- Usage: runhaskell JsonDemo.hs <data-directory>

module Hydra.Demos.Neo4jValidation.JsonDemo where

import qualified Hydra.Neo4j.Model as M4
import qualified Hydra.Validate.Neo4j as Validate
import qualified Hydra.Error.Neo4j as Err
import qualified Hydra.Validation as V
import qualified Hydra.Json.Model as Json

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Key as AK
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Scientific as SC
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as Vec

import Control.Monad (filterM)
import System.Directory (doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, stdout, hPutStrLn, stderr)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [dataDir] -> runDemo dataDir
    _ -> hPutStrLn stderr "Usage: JsonDemo <data-directory>" >> exitFailure

graphNames :: [String]
graphNames =
  [ "valid"
  , "missing_required_property"
  , "wrong_property_type"
  , "missing_required_released"
  , "endpoint_mismatch"
  , "valid_richer"
  , "valid_likes_person"
  , "multiple_violations"
  , "optional_wrong_type"
  , "acted_in_wrong_direction"
  , "undeclared_label_open_world" ]

runDemo :: FilePath -> IO ()
runDemo dataDir = do
  schema <- decodeGraphType <$> loadJsonFile (dataDir </> "schema.json")
  let profile = reportAllProfile
  names <- graphNamesFor dataDir
  mapM_ (validateOne dataDir schema profile) names

-- | The canonical fixture list when present, otherwise every *.json except schema.json
-- (so a Cypher-ingested graph dropped in is picked up automatically).
graphNamesFor :: FilePath -> IO [String]
graphNamesFor dataDir = do
  present <- filterM (\n -> doesFileExist (dataDir </> n ++ ".json")) graphNames
  if not (null present)
    then return present
    else do
      entries <- listDirectory dataDir
      return $ L.sort
        [ take (length f - 5) f
        | f <- entries, ".json" `L.isSuffixOf` f, f /= "schema.json" ]

validateOne :: FilePath -> M4.GraphType -> V.ValidationProfile -> String -> IO ()
validateOne dataDir schema profile name = do
  let path = dataDir </> name ++ ".json"
  exists <- doesFileExist path
  if not exists then return () else do
    json <- loadJsonFile path
    let nodes = decodeNodes json
        rels = decodeRelationships json
        vr = Validate.validateGraph profile schema nodes rels
        errs = V.validationResultErrors vr
    if null errs
      then out $ "Graph \"" ++ name ++ "\": VALID"
      else do
        out $ "Graph \"" ++ name ++ "\": INVALID (" ++ show (length errs) ++ " violation(s))"
        mapM_ (\e -> out $ "  - " ++ describe e) errs
  where out msg = putStrLn msg >> hFlush stdout

-- | Open-world profile that reports all violations (default bounds are maxErrors=1).
reportAllProfile :: V.ValidationProfile
reportAllProfile = base { V.validationProfileMaxErrors = 1000, V.validationProfileMaxWarnings = 1000 }
  where base = Validate.defaultNeo4jProfile

-- ============================================================================
-- Human-readable rendering of a structured graph-validation error
-- ============================================================================

describe :: Err.InvalidGraphError -> String
describe (Err.InvalidGraphErrorNode n) =
  "node " ++ M4.unElementId (Err.invalidGraphNodeErrorId n) ++ ": "
    ++ describeNode (Err.invalidGraphNodeErrorError n)
describe (Err.InvalidGraphErrorRelationship r) =
  "relationship " ++ M4.unElementId (Err.invalidGraphRelationshipErrorId r) ++ ": "
    ++ describeRel (Err.invalidGraphRelationshipErrorError r)

describeNode :: Err.InvalidNodeError -> String
describeNode e = case e of
  Err.InvalidNodeErrorMissingProperty x ->
    "missing required property '" ++ M4.unKey (Err.propertyExistenceErrorKey x) ++ "'"
  Err.InvalidNodeErrorWrongPropertyType x ->
    "property '" ++ M4.unKey (Err.propertyTypeErrorKey x) ++ "' has the wrong type (expected "
      ++ valueTypeName (Err.propertyTypeErrorExpectedType x) ++ ")"
  Err.InvalidNodeErrorMissingImpliedLabel x ->
    "missing implied label '" ++ M4.unNodeLabel (Err.missingLabelErrorLabel x) ++ "'"
  Err.InvalidNodeErrorNoSuchLabel _ -> "no node element type matches the node's labels"
  _ -> showCtor e

describeRel :: Err.InvalidRelationshipError -> String
describeRel e = case e of
  Err.InvalidRelationshipErrorNoMatchingPattern _ ->
    "endpoints match no declared pattern for this relationship type"
  Err.InvalidRelationshipErrorMissingProperty x ->
    "missing required property '" ++ M4.unKey (Err.propertyExistenceErrorKey x) ++ "'"
  Err.InvalidRelationshipErrorWrongPropertyType x ->
    "property '" ++ M4.unKey (Err.propertyTypeErrorKey x) ++ "' has the wrong type (expected "
      ++ valueTypeName (Err.propertyTypeErrorExpectedType x) ++ ")"
  Err.InvalidRelationshipErrorNoSuchType _ -> "no relationship element type has this type"
  _ -> showCtor e

-- A last-resort label for variants the fixtures never produce.
showCtor :: Show a => a -> String
showCtor = takeWhile (/= ' ') . show

valueTypeName :: M4.ValueType -> String
valueTypeName vt = case vt of
  M4.ValueTypeBoolean -> "BOOLEAN"
  M4.ValueTypeString -> "STRING"
  M4.ValueTypeInteger -> "INTEGER"
  M4.ValueTypeFloat -> "FLOAT"
  _ -> showCtor vt

-- ============================================================================
-- JSON parsing (Aeson -> Hydra JSON)
-- ============================================================================

loadJsonFile :: FilePath -> IO Json.Value
loadJsonFile fp = do
  content <- BS.readFile fp
  case A.eitherDecode content of
    Left err -> fail $ "JSON parse error in " ++ fp ++ ": " ++ err
    Right v  -> return (aesonToHydra v)

aesonToHydra :: A.Value -> Json.Value
aesonToHydra v = case v of
  A.Object km -> Json.ValueObject [(AK.toString k, aesonToHydra v') | (k, v') <- AKM.toList km]
  A.Array a   -> Json.ValueArray [aesonToHydra x | x <- Vec.toList a]
  A.String t  -> Json.ValueString (T.unpack t)
  A.Number s  -> Json.ValueNumber s
  A.Bool b    -> Json.ValueBoolean b
  A.Null      -> Json.ValueNull

-- ============================================================================
-- JSON -> Neo4j model decoders
-- ============================================================================

decodeGraphType :: Json.Value -> M4.GraphType
decodeGraphType json = M4.GraphType
  (decodeList (requireField obj "nodes") decodeNodeElementType)
  (decodeList (requireField obj "relationships") decodeRelElementType)
  where obj = expectObject json

decodeNodeElementType :: Json.Value -> M4.NodeElementType
decodeNodeElementType json = M4.NodeElementType
  (M4.NodeLabel $ expectString $ requireField obj "identifyingLabel")
  (S.fromList $ decodeList (requireField obj "impliedLabels") (M4.NodeLabel . expectString))
  (decodeList (requireField obj "constraints") decodeConstraintDef)
  where obj = expectObject json

decodeRelElementType :: Json.Value -> M4.RelationshipElementType
decodeRelElementType json = M4.RelationshipElementType
  (M4.RelationshipType $ expectString $ requireField obj "type")
  (M4.NodeLabel $ expectString $ requireField obj "startLabel")
  (M4.NodeLabel $ expectString $ requireField obj "endLabel")
  (decodeList (requireField obj "constraints") decodeConstraintDef)
  where obj = expectObject json

-- The fixtures leave constraint names unset (encoded as null); model them as Nothing.
decodeConstraintDef :: Json.Value -> M4.ConstraintDefinition
decodeConstraintDef json = M4.ConstraintDefinition Nothing (decodeConstraint (requireField obj "body"))
  where obj = expectObject json

decodeConstraint :: Json.Value -> M4.Constraint
decodeConstraint json
  | hasField obj "propertyExistence" =
      let c = expectObject (requireField obj "propertyExistence")
      in M4.ConstraintPropertyExistence $ M4.PropertyExistenceConstraint
           (M4.Key $ expectString $ requireField c "property")
  | hasField obj "propertyType" =
      let c = expectObject (requireField obj "propertyType")
      in M4.ConstraintPropertyType $ M4.PropertyTypeConstraint
           (M4.Key $ expectString $ requireField c "property")
           (decodeValueType (requireField c "type"))
  | otherwise = error "Unsupported constraint in demo fixtures"
  where obj = expectObject json

decodeValueType :: Json.Value -> M4.ValueType
decodeValueType json
  | hasField obj "boolean" = M4.ValueTypeBoolean
  | hasField obj "string"  = M4.ValueTypeString
  | hasField obj "integer" = M4.ValueTypeInteger
  | hasField obj "float"   = M4.ValueTypeFloat
  | otherwise = error "Unsupported value type in demo fixtures"
  where obj = expectObject json

decodeNodes :: Json.Value -> [M4.Node]
decodeNodes json = decodeList (requireField (expectObject json) "nodes") decodeNode

decodeRelationships :: Json.Value -> [M4.Relationship]
decodeRelationships json = decodeList (requireField (expectObject json) "relationships") decodeRelationship

decodeNode :: Json.Value -> M4.Node
decodeNode json = M4.Node
  (M4.ElementId $ expectString $ requireField obj "id")
  (S.fromList $ decodeList (requireField obj "labels") (M4.NodeLabel . expectString))
  (decodeProperties (requireField obj "properties"))
  where obj = expectObject json

decodeRelationship :: Json.Value -> M4.Relationship
decodeRelationship json = M4.Relationship
  (M4.ElementId $ expectString $ requireField obj "id")
  (decodeProperties (requireField obj "properties"))
  (M4.RelationshipType $ expectString $ requireField obj "type")
  (M4.ElementId $ expectString $ requireField obj "start")
  (M4.ElementId $ expectString $ requireField obj "end")
  where obj = expectObject json

decodeProperties :: Json.Value -> M.Map M4.Key M4.Value
decodeProperties json = M.fromList $ fmap decodePair (expectArray json)
  where
    decodePair entry =
      ( M4.Key $ expectString $ requireField o "key"
      , decodeValue (requireField o "value") )
      where o = expectObject entry

decodeValue :: Json.Value -> M4.Value
decodeValue json
  | hasField obj "boolean" = M4.ValueBoolean $ expectBoolean (requireField obj "boolean")
  | hasField obj "string"  = M4.ValueString $ expectString (requireField obj "string")
  | hasField obj "integer" = M4.ValueInteger $ round $ expectNumber (requireField obj "integer")
  | hasField obj "float"   = M4.ValueFloat $ expectNumber (requireField obj "float")
  | hasField obj "list"    = M4.ValueList $ decodeList (requireField obj "list") decodeValue
  | otherwise = error "Unsupported value in demo fixtures"
  where obj = expectObject json

-- ============================================================================
-- JSON helpers
-- ============================================================================

decodeList :: Json.Value -> (Json.Value -> a) -> [a]
decodeList json decode = fmap decode (expectArray json)

expectObject :: Json.Value -> M.Map String Json.Value
expectObject (Json.ValueObject m) = M.fromList m
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

-- 64-bit integers are encoded as JSON strings; accept either string or number.
expectNumber :: Json.Value -> Double
expectNumber (Json.ValueNumber n) = SC.toRealFloat n
expectNumber (Json.ValueString s) = read s
expectNumber _ = error "Expected JSON number"

requireField :: M.Map String Json.Value -> String -> Json.Value
requireField obj name = case M.lookup name obj of
  Just v  -> v
  Nothing -> error $ "Missing required field: " ++ name

hasField :: M.Map String Json.Value -> String -> Bool
hasField obj name = M.member name obj
