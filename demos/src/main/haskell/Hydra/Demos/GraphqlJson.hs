-- | GraphQL JSON demo: generates a GraphQL schema from Hydra kernel types, then demonstrates
--   querying Hydra kernel modules through that schema.
--
--   The pipeline:
--   1. Hydra types  ->  GraphQL schema (via the GraphQL coder)
--   2. Hydra terms  ->  JSON data      (via term encoders + JSON encoder)
--   3. GraphQL queries against the JSON data (via a companion Python script)
--
--   This demo uses the actual Hydra kernel modules as the dataset.

module Hydra.Demos.GraphqlJson (
  demoGraphqlJson,
  generateGraphqlSchema,
) where

import Hydra.Kernel
import Hydra.ExtGeneration
import Hydra.Generation (moduleAsBindings)
import Hydra.Dsl.Bootstrap (bootstrapGraph)

import qualified Hydra.Graphql.Coder as GraphqlCoder
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Context as Context

import qualified Data.Map as M
import qualified System.Directory as SD
import qualified System.FilePath as FP


-- | The kernel type modules that define the schema we want to query.
-- Includes hydra.util for the Pair/Either fallback types referenced by other types.
schemaModules :: [Module]
schemaModules = filterModulesByNamespace ["hydra.module", "hydra.util"] kernelTypesModules

filterModulesByNamespace :: [String] -> [Module] -> [Module]
filterModulesByNamespace names mods =
  [m | m <- mods, unNamespace (moduleNamespace m) `elem` names]

-- | All kernel modules: both type modules and term modules.
allKernelModules :: [Module]
allKernelModules = kernelModules

-- | Generate the GraphQL schema by calling the coder directly, bypassing language adaptation.
generateGraphqlSchema :: FilePath -> IO Int
generateGraphqlSchema outputDir = do
  putStrLn $ "Generating GraphQL schema to: " ++ outputDir

  -- Build the schema graph from kernel type modules. elementsToGraph still
  -- operates in terms of Bindings, so we convert each module's definitions
  -- via the kernel-aligned helper before feeding it the graph.
  let allMods = kernelTypesModules
      schemaElements = concatMap moduleAsBindings allMods
      schemaGraph = Lexical.elementsToGraph bootstrapGraph M.empty schemaElements
      cx = Context.Context [] [] M.empty

  -- For each schema module, hand the coder the type definitions directly.
  let results = do
        mod <- schemaModules
        let defs = [d | d@(DefinitionType _) <- moduleDefinitions mod]
        case GraphqlCoder.moduleToGraphql mod defs cx schemaGraph of
          Left ic -> [(unNamespace (moduleNamespace mod), Left $ show ic)]
          Right files -> [(path, Right content) | (path, content) <- M.toList files]

  -- Write output files
  let successes = [(path, content) | (path, Right content) <- results]
      failures = [(path, err) | (path, Left err) <- results]

  mapM_ (\(path, err) -> putStrLn $ "  Warning: " ++ path ++ ": " ++ err) failures

  mapM_ (\(path, content) -> do
    let fullPath = FP.combine outputDir path
    SD.createDirectoryIfMissing True $ FP.takeDirectory fullPath
    writeFile fullPath (content ++ "\n")
    putStrLn $ "  Wrote: " ++ fullPath
    ) successes

  return (length successes)

-- | Run the full demo
demoGraphqlJson :: IO ()
demoGraphqlJson = do
  let demoDir = "demos/graphql-json"
  let outputDir = FP.combine demoDir "output"
  let jsonDir = "../../dist/json/hydra-kernel/src/main/json"

  putStrLn "=== GraphQL JSON Demo ==="
  putStrLn ""

  -- Step 1: Generate GraphQL schema
  putStrLn "--- Step 1: Generate GraphQL schema from Hydra types ---"
  nFiles <- generateGraphqlSchema outputDir
  putStrLn $ "  Generated " ++ show nFiles ++ " schema file(s)"
  putStrLn ""

  putStrLn "--- Done ---"
  putStrLn ""
  putStrLn "Generated files:"
  putStrLn $ "  Schema:  " ++ FP.combine outputDir "hydra/module.graphql"
  putStrLn $ "  Data:    " ++ jsonDir ++ "/hydra/*.json (existing kernel JSON)"
  putStrLn ""
  putStrLn "To run GraphQL queries:"
  putStrLn $ "  cd " ++ demoDir
  putStrLn "  pip install graphql-core"
  putStrLn "  python3 query.py"
