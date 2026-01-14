module Hydra.Ext.Demos.GenPG.Demo where

import Hydra.Kernel
import Hydra.Ext.Demos.GenPG.ExampleDatabaseSchema
import Hydra.Ext.Demos.GenPG.ExampleGraphSchema
import Hydra.Ext.Demos.GenPG.ExampleMapping
import Hydra.Ext.Demos.GenPG.Generated.DatabaseSchema
import Hydra.Ext.Demos.GenPG.Generated.GraphSchema
import Hydra.Ext.Demos.GenPG.Generated.Mapping
import Hydra.Dsl.Tabular
import Hydra.Ext.Dsl.Pg.Mappings
import Hydra.Lib.Literals
import Hydra.Pg.Graphson.Utils
import qualified Hydra.Demos.Genpg.Transform as Transform
import Hydra.Ext.Staging.Pg.Printing
import Hydra.Ext.Staging.Pg.Utils
import Hydra.Sources.Kernel.Types.Core
import Hydra.Tools.Monads
import qualified Hydra.Json.Writer as JsonWriter
import qualified Hydra.Pg.Model as Pg
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Dsl.Terms as Terms

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import System.IO (hFlush, stdout)


generateExampleGraphSON :: IO ()
generateExampleGraphSON = generateGraphSON
  "data/genpg/sources/sales"
  salesTableSchemas
  salesGraph
  "data/genpg/sales.json"

generateCopilotGraphSON :: IO ()
generateCopilotGraphSON = generateGraphSON
  "data/genpg/sources/health"
  generatedTableSchemas
  generatedGraphMapping
  "data/genpg/copilot.json"

generateGraphSON :: FilePath -> [TableType] -> Pg.LazyGraph Term -> FilePath -> IO ()
generateGraphSON sourceRoot tableSchemas graphMapping outputPath = do
  log $ "Reading CSV files from " ++ sourceRoot ++ "/"
  log $ "  Tables: " ++ L.intercalate ", " (fmap (unRelationName . tableTypeName) tableSchemas)
  g <- transformTables sourceRoot tableSchemas graphMapping
  let els = lazyGraphToElements g
  let (vertices, edges) = L.partition isVertex els
  log $ "Transforming to property graph..."
  log $ "  Vertices: " ++ show (L.length vertices)
  log $ "  Edges: " ++ show (L.length edges)
  log $ "Writing GraphSON to " ++ outputPath
  jsonResult <- flowToIo hydraCoreGraph (pgElementsToGraphson encodeTermValue els)
  writeFile outputPath (jsonValuesToString jsonResult)
  log $ "Done. Output written to " ++ outputPath
  where
    log msg = putStrLn msg >> hFlush stdout
    jsonValuesToString = L.intercalate "\n" . fmap JsonWriter.printJson
    isVertex el = case el of
      Pg.ElementVertex _ -> True
      Pg.ElementEdge _ -> False
    unRelationName (RelationName n) = n


--------------------------------------------------------------------------------
-- Table transformation (I/O)

-- | Transform a table by reading from a file and applying vertex/edge specifications
transformTable :: TableType -> FilePath -> [Pg.Vertex Term] -> [Pg.Edge Term] -> IO ([Pg.Vertex Term], [Pg.Edge Term])
transformTable tableType@(TableType (RelationName tableName) _) path vspecs especs = do
    (Table _ rows) <- decodeTableIo tableType path
    pairs <- flowToIo hydraCoreGraph $ withTrace ("transforming " ++ filePath) $
      CM.mapM (\row -> Transform.transformRecord vspecs especs (Transform.termRowToRecord tableType row)) rows
    return $ L.foldl addRow ([], []) pairs
  where
    filePath = tableName
    addRow (vertices, edges) (v, e) = (vertices ++ v, edges ++ e)

-- | Transform multiple tables according to a graph mapping specification
transformTables :: FilePath -> [TableType] -> Pg.LazyGraph Term -> IO (Pg.LazyGraph Term)
transformTables fileRoot tableTypes spec = do
    transform <- case (Transform.elementSpecsByTable spec) of
      Left err -> fail $ "Error in mapping specification: " ++ err
      Right t -> return t
    pairs <- CM.mapM forTable $ M.toList transform
    let (vertices, edges) = L.foldl addRow ([], []) pairs
    return $ Pg.LazyGraph vertices edges
  where
    addRow (vertices, edges) (v, e) = (vertices ++ v, edges ++ e)
    forTable (tname, (vspecs, especs)) = case M.lookup (RelationName tname) tableTypesByName of
        Nothing -> fail $ "Table specified in mapping does not exist: " ++ tname
        Just tableType -> do
          (vertices, edges) <- transformTable tableType path vspecs especs
          return (vertices, edges)
      where
        path = fileRoot ++ "/" ++ tname
    tableTypesByName = M.fromList $ fmap (\t -> (tableTypeName t, t)) tableTypes


--------------------------------------------------------------------------------
-- Table reading

decodeTable :: TableType -> Table String -> Either String (Table Term)
decodeTable (TableType _ colTypes) (Table mheader rows) = do
    drows <- CM.zipWithM decodeRow rows [1..]
    return $ Table mheader $ fmap DataRow drows
  where
    decodeRow (DataRow row) lineno = CM.zipWithM decodeCell colTypes row
      where
        decodeCell (ColumnType (ColumnName cname) typ) mvalue = case mvalue of
          Nothing -> Right Nothing
          Just value -> case typ of
              TypeLiteral lt -> case lt of
                LiteralTypeBoolean -> readValue Terms.boolean readBoolean value
                LiteralTypeFloat ft -> case ft of
                   FloatTypeBigfloat -> readValue Terms.bigfloat readFloat64 value
                   FloatTypeFloat32 -> readValue Terms.float32 readFloat32 value
                   FloatTypeFloat64 -> readValue Terms.float64 readFloat64 value
                LiteralTypeInteger it -> case it of
                  IntegerTypeInt32 -> readValue Terms.int32 readInt32 value
                  IntegerTypeInt64 -> readValue Terms.int64 readInt64 value
                  _ -> unsupported
                LiteralTypeString -> readValue Terms.string Just value
              _ -> unsupported
            where
              toEither mv = case mv of
                Nothing -> Left $ "Invalid value of type " ++ ShowCore.type_ typ ++ " for column " ++ show cname
                  ++ " on line " ++ show lineno ++ ": " ++ value
                Just v -> Right v
              unsupported = Left $ "Unsupported type for column " ++ show cname ++ ": " ++ ShowCore.type_ typ
              readValue cons read value = (Just . cons) <$> (toEither $ read value)

decodeTableIo :: TableType -> FilePath -> IO (Table Term)
decodeTableIo tableType path = do
    table <- readTable True path
    case decodeTable tableType table of
      Left err -> fail err
      Right t -> return t

-- Note: LLM-generated function. Not thoroughly tested.
parseCsvLine :: String -> Either String [Maybe String]
parseCsvLine = go [] [] False False
  where
    go acc field inQuotes escape [] =
      finalize acc field inQuotes

    go acc field inQuotes escape (c:cs)
      | c == '"' =
          if inQuotes
            then case cs of
              ('"':cs') -> go acc (field ++ ['"']) True False cs'  -- Escaped quote
              _         -> go acc field False False cs             -- End quote
          else if null field
            then go acc field True False cs                       -- Start quote
          else Left "Unexpected quote inside unquoted field"

      | c == ',' && not inQuotes =
          go (acc ++ [normalize field]) [] False False cs

      | otherwise =
          go acc (field ++ [c]) inQuotes False cs

    finalize acc field inQuotes
      | inQuotes  = Left "Unclosed quoted field"
      | otherwise = Right (acc ++ [normalize field])

    normalize "" = Nothing
    normalize s  = Just s

readTable :: Bool -> FilePath -> IO (Table String)
readTable hasHeader path = do
  rawLines <- fmap lines $ readFile path
  case CM.zipWithM parseLine rawLines [1..] of
    Left err -> fail $ "CSV read error in " ++ show path ++ ": " ++ err
    Right rows0 -> do
      let rows = Y.catMaybes rows0
      if hasHeader
        then do
          let headerRow = L.head rows
          if any Y.isNothing headerRow
            then fail $ "null header column(s) in " ++ show path
            else pure $ Table (Just $ HeaderRow $ Y.catMaybes headerRow) $ fmap DataRow $ L.tail rows
        else pure $ Table Nothing $ fmap DataRow rows
  where
    parseLine line number = if L.null trimmed
        then pure Nothing
        else case parseCsvLine trimmed of
          Left err -> Left $ "CSV error in " ++ show path ++ ", line " ++ show number ++ ": " ++ err
          Right row -> pure $ Just row
      where
        trimmed = stripLeadingAndTrailingWhitespace line