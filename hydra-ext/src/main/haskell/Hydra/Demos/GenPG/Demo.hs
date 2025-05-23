module Hydra.Demos.GenPG.Demo where

import Hydra.Demos.GenPG.ExampleDatabaseSchema
import Hydra.Demos.GenPG.ExampleGraphSchema
import Hydra.Demos.GenPG.ExampleMapping
import Hydra.Demos.GenPG.Generated.DatabaseSchema
import Hydra.Demos.GenPG.Generated.GraphSchema
import Hydra.Demos.GenPG.Generated.Mapping
import Hydra.Demos.GenPG.Transform
import Hydra.Dsl.Ext.Tabular
import Hydra.Dsl.Pg.Mappings
import Hydra.Ext.Json.Serde
import Hydra.Ext.Pg.Graphson.Utils
import Hydra.Ext.Pg.Printing
import Hydra.Kernel
import Hydra.Lib.Io
import Hydra.Sources.Tier0.Core
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
  jsonResult <- fromFlowIo hydraCoreGraph (pgElementsToGraphson termGraphsonContext els)
  writeFile outputPath (jsonValuesToString jsonResult)
