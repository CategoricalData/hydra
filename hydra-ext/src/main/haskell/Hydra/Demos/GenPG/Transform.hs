module Hydra.Demos.GenPG.Transform where

import Hydra.Kernel
import qualified Hydra.Pg.Model as Pg
import Hydra.Dsl.Pg.Mappings
import Hydra.Ext.Tabular
import Hydra.Dsl.Ext.Tabular
import Hydra.Lib.Io
import Hydra.Lib.Literals
import Hydra.Tools.Monads
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Tier0.Core (hydraCoreGraph)

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


type PgTransform = M.Map String ([Pg.Vertex Term], [Pg.Edge Term])


evaluate :: Term -> Flow Graph Term
evaluate = reduceTerm True M.empty

evaluateEdge :: Pg.Edge Term -> Term -> Flow Graph (Maybe (Pg.Edge Term))
evaluateEdge (Pg.Edge label idSpec outSpec inSpec propSpecs) term = do
    id <- evaluate $ Terms.apply idSpec term
    mOutId <- evaluate (Terms.apply outSpec term) >>= (ExtractCore.optional pure)
    mInId <- evaluate (Terms.apply inSpec term) >>= (ExtractCore.optional pure)
    props <- evaluateProperties propSpecs term
    return $ case mOutId of
      Nothing -> Nothing
      Just outId -> case mInId of
        Nothing -> Nothing
        Just inId -> Just $ Pg.Edge label id outId inId props

evaluateProperties :: M.Map Pg.PropertyKey Term -> Term -> Flow Graph (M.Map Pg.PropertyKey Term)
evaluateProperties specs record = M.fromList . Y.catMaybes <$> (CM.mapM forPair $ M.toList specs)
  where
    forPair (k, spec) = do
      value <- (evaluate $ Terms.apply spec record)
      case fullyStripTerm value of
        TermOptional mv -> case mv of
          Nothing -> return Nothing
          Just v -> return $ Just (k, v)
        _ -> fail $ "expected an optional value for property " ++ Pg.unPropertyKey k ++ " but got " ++ showTerm value

evaluateVertex :: Pg.Vertex Term -> Term -> Flow Graph (Maybe (Pg.Vertex Term))
evaluateVertex (Pg.Vertex label idSpec propSpecs) record = do
  mId <- evaluate (Terms.apply idSpec record) >>= (ExtractCore.optional pure)
  props <- evaluateProperties propSpecs record
  return $ case mId of
    Nothing -> Nothing
    Just id -> Just $ Pg.Vertex label id props

findTablesInTerm :: Term -> S.Set String
findTablesInTerm = foldOverTerm TraversalOrderPre f S.empty
  where
    f names term = case term of
      TermFunction (FunctionElimination (EliminationRecord (Projection tname _))) -> S.insert (unName tname) names
      _ -> names

findTablesInTerms :: [Term] -> S.Set String
findTablesInTerms terms = S.unions $ fmap findTablesInTerm terms

elementSpecsByTable :: LazyGraph Term -> Either String PgTransform
elementSpecsByTable (LazyGraph vertices edges) = do
    vertexPairs <- CM.mapM vertexPair vertices
    edgePairs <- CM.mapM edgePair edges
    return $ L.foldl addEdgePair (L.foldl addVertexPair M.empty vertexPairs) edgePairs
  where
    vertexPair v = case tableForVertex v of
          Left err -> Left err
          Right table -> Right (table, v)
    edgePair e = case tableForEdge e of
          Left err -> Left err
          Right table -> Right (table, e)
    addVertexPair m (table, v) = M.insert table (v : vertices, edges) m
      where
        (vertices, edges) = Y.fromMaybe ([], []) $ M.lookup table m
    addEdgePair m (table, e) = M.insert table (vertices, e : edges) m
      where
        (vertices, edges) = Y.fromMaybe ([], []) $ M.lookup table m

tableForEdge :: Pg.Edge Term -> Either String String
tableForEdge (Pg.Edge label id outId inId props) = if S.size tables == 1
    then Right $ L.head $ S.toList tables
    else Left $ "Specification for " ++ Pg.unEdgeLabel label ++ " edges has wrong number of tables: "
      ++ show tables
  where
    tables = findTablesInTerms $ [id, outId, inId] ++ M.elems props

tableForVertex :: Pg.Vertex Term -> Either String String
tableForVertex (Pg.Vertex label id props) = if S.size tables == 1
    then Right $ L.head $ S.toList tables
    else Left $ "Specification for " ++ Pg.unVertexLabel label ++ " vertices has wrong number of tables: "
      ++ show tables
  where
    tables = findTablesInTerms $ [id] ++ M.elems props

termRowToRecord :: TableType -> DataRow Term -> Term
termRowToRecord (TableType (TableName tname) colTypes) (DataRow cells) = TermRecord $ Record (Name tname) $
    L.zipWith toField colTypes cells
  where
    toField (ColumnType (ColumnName cname) _) mvalue = Field (Name cname) $ TermOptional mvalue

transformRecord :: [Pg.Vertex Term] -> [Pg.Edge Term] -> Term -> Flow Graph ([Pg.Vertex Term], [Pg.Edge Term])
transformRecord vspecs especs term = do
  vertices <- CM.mapM (\s -> evaluateVertex s term) vspecs
  edges <- CM.mapM (\s -> evaluateEdge s term) especs
  return (Y.catMaybes vertices, Y.catMaybes edges)

transformTable :: TableType -> FilePath -> [Pg.Vertex Term] -> [Pg.Edge Term] -> IO ([Pg.Vertex Term], [Pg.Edge Term])
transformTable tableType@(TableType (TableName tableName) _) path vspecs especs = do
    (Table _ rows) <- decodeTableIo tableType path
    pairs <- fromFlowIo hydraCoreGraph $ withTrace ("transforming " ++ filePath) $
      CM.mapM (transformRecord vspecs especs . termRowToRecord tableType) rows
    return $ L.foldl addRow ([], []) pairs
  where
    filePath = tableName
    addRow (vertices, edges) (v, e) = (vertices ++ v, edges ++ e)

transformTables :: FilePath -> [TableType] -> LazyGraph Term -> IO (LazyGraph Term)
transformTables fileRoot tableTypes spec = do
    transform <- case (elementSpecsByTable spec) of
      Left err -> fail $ "Error in mapping specification: " ++ err
      Right t -> return t
    pairs <- CM.mapM forTable $ M.toList transform
    let (vertices, edges) = L.foldl addRow ([], []) pairs
    return $ LazyGraph vertices edges
  where
    addRow (vertices, edges) (v, e) = (vertices ++ v, edges ++ e)
    forTable (tname, (vspecs, especs)) = case M.lookup (TableName tname) tableTypesByName of
        Nothing -> fail $ "Table specified in mapping does not exist: " ++ tname
        Just tableType -> do
          (vertices, edges) <- transformTable tableType path vspecs especs
          return (vertices, edges)
      where
        path = fileRoot ++ "/" ++ tname
    tableTypesByName = M.fromList $ fmap (\t -> (tableTypeName t, t)) tableTypes


--------------------------------------------------------------------------------
-- Table reading (consider making this into a separate module)

decodeTable :: TableType -> Table String -> Either String (Table Term)
decodeTable (TableType _ colTypes) (Table mheader rows) = do
    -- TODO: check header names against the table schema
    -- TODO: check that all rows have the same length as the header
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
                Nothing -> Left $ "Invalid value of type " ++ showType typ ++ " for column " ++ show cname
                  ++ " on line " ++ show lineno ++ ": " ++ value
                Just v -> Right v
              unsupported = Left $ "Unsupported type for column " ++ show cname ++ ": " ++ showType typ
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
