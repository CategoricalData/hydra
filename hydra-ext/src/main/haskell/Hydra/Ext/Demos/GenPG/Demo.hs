module Hydra.Ext.Demos.GenPG.Demo where

import Hydra.Kernel
import Hydra.Ext.Demos.GenPG.Examples.Sales.DatabaseSchema
import Hydra.Ext.Demos.GenPG.Examples.Sales.GraphSchema
import Hydra.Ext.Demos.GenPG.Examples.Sales.Mapping
import Hydra.Ext.Demos.GenPG.Examples.Health.DatabaseSchema
import Hydra.Ext.Demos.GenPG.Examples.Health.GraphSchema
import Hydra.Ext.Demos.GenPG.Examples.Health.Mapping
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
import qualified Hydra.Tabular as Tab

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import System.IO (hFlush, stdout)


generateSalesGraphSON :: IO ()
generateSalesGraphSON = generateGraphSON
  "demos/genpg/data/sources/sales"
  salesTableSchemas
  salesGraph
  "demos/genpg/output/sales.jsonl"

generateHealthGraphSON :: IO ()
generateHealthGraphSON = generateGraphSON
  "demos/genpg/data/sources/health"
  healthTableSchemas
  healthGraph
  "demos/genpg/output/health.jsonl"

generateGraphSON :: FilePath -> [TableType] -> Pg.LazyGraph Term -> FilePath -> IO ()
generateGraphSON sourceRoot tableSchemas graphMapping outputPath = do
  log $ "Reading CSV files from " ++ sourceRoot ++ "/"
  log $ "  Tables: " ++ L.intercalate ", " (fmap (unRelationName . tableTypeName) tableSchemas)
  g <- transformTables sourceRoot tableSchemas graphMapping
  let els = lazyGraphToElements g
  let (vertices, edges) = L.partition Transform.elementIsVertex els
  log $ "Transforming to property graph..."
  log $ "  Vertices: " ++ show (L.length vertices)
  log $ "  Edges: " ++ show (L.length edges)
  log $ "Writing GraphSON to " ++ outputPath
  jsonResult <- flowToIo hydraCoreGraph (pgElementsToGraphson encodeTermValue els)
  writeFile outputPath (jsonValuesToString jsonResult)
  log $ "Done. Output written to " ++ outputPath
  where
    log msg = putStrLn msg >> hFlush stdout
    jsonValuesToString vals = L.intercalate "\n" (fmap JsonWriter.printJson vals) ++ "\n"
    unRelationName (RelationName n) = n


--------------------------------------------------------------------------------
-- Table transformation (I/O)

-- | Transform a table by reading from a file and applying vertex/edge specifications
transformTable :: TableType -> FilePath -> [Pg.Vertex Term] -> [Pg.Edge Term] -> IO ([Pg.Vertex Term], [Pg.Edge Term])
transformTable tableType@(TableType (RelationName tableName) _) path vspecs especs = do
    (Table _ rows) <- decodeTableIo tableType path
    flowToIo hydraCoreGraph $ withTrace ("transforming " ++ tableName) $
      Transform.transformTableRows vspecs especs tableType rows

-- | Transform multiple tables according to a graph mapping specification
transformTables :: FilePath -> [TableType] -> Pg.LazyGraph Term -> IO (Pg.LazyGraph Term)
transformTables fileRoot tableTypes spec = do
    transform <- case (Transform.elementSpecsByTable spec) of
      Left err -> fail $ "Error in mapping specification: " ++ err
      Right t -> return t
    pairs <- CM.mapM forTable $ M.toList transform
    let (vertices, edges) = L.foldl Transform.concatPairs ([], []) pairs
    return $ Transform.makeLazyGraph vertices edges
  where
    forTable (tname, (vspecs, especs)) = case M.lookup (RelationName tname) tblTypesByName of
        Nothing -> fail $ "Table specified in mapping does not exist: " ++ tname
        Just tableType -> transformTable tableType path vspecs especs
      where
        path = fileRoot ++ "/" ++ tname
    tblTypesByName = Transform.tableTypesByName tableTypes


--------------------------------------------------------------------------------
-- Table reading

decodeTableIo :: TableType -> FilePath -> IO (Table Term)
decodeTableIo tableType path = do
    rawLines <- fmap lines $ readFile path
    table <- case Transform.parseTableLines True rawLines of
      Left err -> fail $ "CSV read error in " ++ show path ++ ": " ++ err
      Right t -> return t
    case Transform.decodeTable tableType table of
      Left err -> fail err
      Right t -> return t