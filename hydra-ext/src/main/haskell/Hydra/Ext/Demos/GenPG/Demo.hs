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
import Hydra.Ext.Staging.Pg.Graphson.Utils
import Hydra.Ext.Staging.Pg.Printing
import Hydra.Sources.Kernel.Types.Core
import Hydra.Tools.Monads

import qualified Data.List as L

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
  g <- transformTables sourceRoot tableSchemas graphMapping
  let els = lazyGraphToElements g
  jsonResult <- flowToIo hydraCoreGraph (pgElementsToGraphson termGraphsonContext els)
  writeFile outputPath (jsonValuesToString jsonResult)
  where
    jsonValuesToString = L.intercalate "\n" . fmap JsonWriter.printJson
