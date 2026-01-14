module Hydra.Ext.Demos.GenPG.Demo where

import Hydra.Kernel
import Hydra.Ext.Demos.GenPG.ExampleDatabaseSchema
import Hydra.Ext.Demos.GenPG.ExampleGraphSchema
import Hydra.Ext.Demos.GenPG.ExampleMapping
import Hydra.Ext.Demos.GenPG.Generated.DatabaseSchema
import Hydra.Ext.Demos.GenPG.Generated.GraphSchema
import Hydra.Ext.Demos.GenPG.Generated.Mapping
import Hydra.Ext.Demos.GenPG.Transform
import Hydra.Dsl.Tabular
import Hydra.Ext.Dsl.Pg.Mappings
import qualified Hydra.Json.Writer as JsonWriter
import qualified Hydra.Pg.Model as Pg
import Hydra.Ext.Staging.Pg.Graphson.Utils
import Hydra.Ext.Staging.Pg.Printing
import Hydra.Sources.Kernel.Types.Core
import Hydra.Tools.Monads

import qualified Data.List as L
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

generateGraphSON :: FilePath -> [TableType] -> LazyGraph Term -> FilePath -> IO ()
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
