-- | Python code generation for the GenPG demo.
--
-- This module provides functions to generate all Python modules needed for
-- the GenPG demo, including:
--   - hydra.pg.model, hydra.pg.mapping
--   - hydra.pg.graphson.* (coder, construct, syntax, utils)
--   - hydra.encode.pg.*, hydra.decode.pg.*
--   - hydra.demos.genpg.transform
--   - hydra.demos.genpg.sales (sales database/graph schemas and mapping)
--   - hydra.demos.genpg.health (health database/graph schemas and mapping)
--
-- Usage from GHCI:
--   > import Hydra.Ext.Demos.GenPG.GeneratePython
--   > generatePythonModules

module Hydra.Ext.Demos.GenPG.GeneratePython where

import Hydra.Kernel
import Hydra.Ext.Generation
import Hydra.Sources.All (kernelModules)
import Hydra.Ext.Sources.All (hydraExtModules, genpgModules)
import Hydra.Ext.Demos.GenPG.Examples.Sales.DatabaseSchema as Sales
import Hydra.Ext.Demos.GenPG.Examples.Sales.GraphSchema as Sales
import Hydra.Ext.Demos.GenPG.Examples.Sales.Mapping as Sales
import Hydra.Ext.Demos.GenPG.Examples.Health.DatabaseSchema as Health
import Hydra.Ext.Demos.GenPG.Examples.Health.GraphSchema as Health
import Hydra.Ext.Demos.GenPG.Examples.Health.Mapping as Health
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Encode.Pg.Model as EncodePg
import qualified Hydra.Encode.Tabular as EncodeTabular
import System.Directory (createDirectoryIfMissing)
import System.IO (hFlush, stdout)


-- | Generate all Python modules for the GenPG demo.
--
-- This generates to hydra-ext/src/gen-main/python:
--   - hydra.pg.* modules
--   - hydra.demos.genpg.transform
--   - hydra.demos.genpg.sales
--   - hydra.demos.genpg.health
generatePythonModules :: IO ()
generatePythonModules = do
  let outputDir = "src/gen-main/python"
  createDirectoryIfMissing True outputDir

  putStrLn "=== Generate Python GenPG Modules ==="
  putStrLn ""
  putStrLn $ "Output directory: " ++ outputDir
  putStrLn ""
  putStrLn "Generating modules:"
  putStrLn "  - hydra.pg.model"
  putStrLn "  - hydra.pg.mapping"
  putStrLn "  - hydra.pg.graphson.* (coder, construct, syntax, utils)"
  putStrLn "  - hydra.encode.pg.*, hydra.decode.pg.*"
  putStrLn "  - hydra.demos.genpg.transform"
  putStrLn "  - hydra.demos.genpg.sales"
  putStrLn "  - hydra.demos.genpg.health"
  putStrLn ""
  hFlush stdout

  -- Universe includes kernel and hydra-ext modules for dependency resolution
  -- We generate genpgModules plus the sales and health modules
  let universeModules = kernelModules ++ hydraExtModules ++ [salesModule, healthModule]
  let modulesToGenerate = genpgModules ++ [salesModule, healthModule]
  writePython outputDir universeModules modulesToGenerate

  putStrLn ""
  putStrLn "=== Done! ==="


-- | The sales module containing sales demo schemas and mapping.
salesModule :: Module
salesModule = Module {
  moduleNamespace = salesNamespace,
  moduleElements = [
    -- salesDatabaseSchema: list[TableType]
    Binding {
      bindingName = Name "hydra.demos.genpg.sales.salesDatabaseSchema",
      bindingTerm = TermList $ fmap EncodeTabular.tableType Sales.salesTableSchemas,
      bindingType = Just listTableTypeScheme
    },
    -- salesGraphSchema: GraphSchema[Type]
    Binding {
      bindingName = Name "hydra.demos.genpg.sales.salesGraphSchema",
      bindingTerm = EncodePg.graphSchema EncodeCore.type_ Sales.salesGraphSchema,
      bindingType = Just graphSchemaTypeScheme
    },
    -- salesMapping: LazyGraph[Term]
    Binding {
      bindingName = Name "hydra.demos.genpg.sales.salesMapping",
      bindingTerm = EncodePg.lazyGraph EncodeCore.term Sales.salesGraph,
      bindingType = Just lazyGraphTermScheme
    }
  ],
  moduleTermDependencies = [
    Namespace "hydra.tabular",
    Namespace "hydra.relational",
    Namespace "hydra.pg.model",
    Namespace "hydra.core"
  ],
  moduleTypeDependencies = [
    Namespace "hydra.tabular",
    Namespace "hydra.relational",
    Namespace "hydra.pg.model",
    Namespace "hydra.core"
  ],
  moduleDescription = Just "GenPG schemas for the sales demo"
}


-- | Namespace for the generated sales module
salesNamespace :: Namespace
salesNamespace = Namespace "hydra.demos.genpg.sales"


-- | The health module containing health demo schemas and mapping.
healthModule :: Module
healthModule = Module {
  moduleNamespace = healthNamespace,
  moduleElements = [
    -- healthDatabaseSchema: list[TableType]
    Binding {
      bindingName = Name "hydra.demos.genpg.health.healthDatabaseSchema",
      bindingTerm = TermList $ fmap EncodeTabular.tableType Health.healthTableSchemas,
      bindingType = Just listTableTypeScheme
    },
    -- healthGraphSchema: GraphSchema[Type]
    Binding {
      bindingName = Name "hydra.demos.genpg.health.healthGraphSchema",
      bindingTerm = EncodePg.graphSchema EncodeCore.type_ Health.healthGraphSchema,
      bindingType = Just graphSchemaTypeScheme
    },
    -- healthMapping: LazyGraph[Term]
    Binding {
      bindingName = Name "hydra.demos.genpg.health.healthMapping",
      bindingTerm = EncodePg.lazyGraph EncodeCore.term Health.healthGraph,
      bindingType = Just lazyGraphTermScheme
    }
  ],
  moduleTermDependencies = [
    Namespace "hydra.tabular",
    Namespace "hydra.relational",
    Namespace "hydra.pg.model",
    Namespace "hydra.core"
  ],
  moduleTypeDependencies = [
    Namespace "hydra.tabular",
    Namespace "hydra.relational",
    Namespace "hydra.pg.model",
    Namespace "hydra.core"
  ],
  moduleDescription = Just "GenPG schemas for the health demo"
}


-- | Namespace for the generated health module
healthNamespace :: Namespace
healthNamespace = Namespace "hydra.demos.genpg.health"


-- | Type scheme for list of TableType (no type variables)
listTableTypeScheme :: TypeScheme
listTableTypeScheme = TypeScheme {
  typeSchemeVariables = [],
  typeSchemeType = TypeList (TypeVariable (Name "hydra.tabular.TableType")),
  typeSchemeConstraints = Nothing
}


-- | Type scheme for GraphSchema Type (no type variables)
graphSchemaTypeScheme :: TypeScheme
graphSchemaTypeScheme = TypeScheme {
  typeSchemeVariables = [],
  typeSchemeType = TypeApplication (ApplicationType
    (TypeVariable (Name "hydra.pg.model.GraphSchema"))
    (TypeVariable (Name "hydra.core.Type"))),
  typeSchemeConstraints = Nothing
}


-- | Type scheme for LazyGraph Term (no type variables)
lazyGraphTermScheme :: TypeScheme
lazyGraphTermScheme = TypeScheme {
  typeSchemeVariables = [],
  typeSchemeType = TypeApplication (ApplicationType
    (TypeVariable (Name "hydra.pg.model.LazyGraph"))
    (TypeVariable (Name "hydra.core.Term"))),
  typeSchemeConstraints = Nothing
}
