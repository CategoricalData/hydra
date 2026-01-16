-- | Python code generation for the GenPG demo.
--
-- This module provides functions to generate all Python modules needed for
-- the GenPG demo, including:
--   - hydra.pg.model, hydra.pg.mapping
--   - hydra.pg.graphson.* (coder, construct, syntax, utils)
--   - hydra.encode.pg.*, hydra.decode.pg.*
--   - hydra.demos.genpg.transform
--   - hydra.demos.genpg.example (schemas and mapping)
--
-- Usage from GHCI:
--   > import Hydra.Ext.Demos.GenPG.GeneratePython
--   > generatePythonModules

module Hydra.Ext.Demos.GenPG.GeneratePython where

import Hydra.Kernel
import Hydra.Ext.Generation
import Hydra.Sources.All (kernelModules)
import Hydra.Ext.Sources.All (hydraExtModules, genpgModules)
import Hydra.Ext.Demos.GenPG.Examples.Sales.DatabaseSchema
import Hydra.Ext.Demos.GenPG.Examples.Sales.GraphSchema
import Hydra.Ext.Demos.GenPG.Examples.Sales.Mapping
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
--   - hydra.demos.genpg.example
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
  putStrLn "  - hydra.demos.genpg.example"
  putStrLn ""
  hFlush stdout

  -- Universe includes kernel and hydra-ext modules for dependency resolution
  -- We generate genpgModules plus the example module
  let universeModules = kernelModules ++ hydraExtModules ++ [exampleModule]
  let modulesToGenerate = genpgModules ++ [exampleModule]
  writePython outputDir universeModules modulesToGenerate

  putStrLn ""
  putStrLn "=== Done! ==="


-- | The example module containing sales demo schemas and mapping.
exampleModule :: Module
exampleModule = Module {
  moduleNamespace = exampleNamespace,
  moduleElements = [
    -- exampleDatabaseSchema: list[TableType]
    Binding {
      bindingName = Name "hydra.demos.genpg.example.exampleDatabaseSchema",
      bindingTerm = TermList $ fmap EncodeTabular.tableType salesTableSchemas,
      bindingType = Just listTableTypeScheme
    },
    -- exampleGraphSchema: GraphSchema[Type]
    Binding {
      bindingName = Name "hydra.demos.genpg.example.exampleGraphSchema",
      bindingTerm = EncodePg.graphSchema EncodeCore.type_ salesGraphSchema,
      bindingType = Just graphSchemaTypeScheme
    },
    -- exampleMapping: LazyGraph[Term]
    Binding {
      bindingName = Name "hydra.demos.genpg.example.exampleMapping",
      bindingTerm = EncodePg.lazyGraph EncodeCore.term salesGraph,
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
  moduleDescription = Just "Example GenPG schemas for the sales demo"
}


-- | Namespace for the generated example module
exampleNamespace :: Namespace
exampleNamespace = Namespace "hydra.demos.genpg.example"


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
