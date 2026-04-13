-- Diagnostic: trace where exactly it hangs
module Main where

import Hydra.Kernel
import Hydra.Generation (modulesToGraph)
import Hydra.Dsl.Bootstrap (bootstrapGraph)
import Hydra.Codegen (moduleTermDepsTransitive, moduleTypeDepsTransitive, generateSourceFiles)
import Hydra.Lexical (elementsToGraph, graphToBindings, buildGraph)
import Hydra.Inference (inferGraphTypes)
import Hydra.Adapt (dataGraphToDefinitions)
import qualified Hydra.Environment as Environment
import Hydra.Sources.All (mainModules)
import Hydra.Sources.Ext (hydraExtModules)
import Hydra.Sources.All
import Hydra.Haskell.Language (haskellLanguage)
import Hydra.Haskell.Coder (moduleToHaskell)
import qualified Hydra.Coders as Coders
import qualified Hydra.Show.Errors as ShowError

import qualified Hydra.Sources.Cpp.Names as CppNames

import qualified Data.Map as M
import qualified Data.List as L
import System.IO (hSetBuffering, stdout, BufferMode(..), hFlush)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  let uni = mainModules ++ hydraExtModules
  let modToGen = CppNames.module_
  let cx = emptyContext

  -- Step 1: Build namespace map
  putStrLn "Step 1: Building namespace map..."
  hFlush stdout
  let namespaceMap = M.fromList [(moduleNamespace m, m) | m <- uni ++ [modToGen]]
  putStrLn $ "  Namespace map size: " ++ show (M.size namespaceMap)

  -- Step 2: Compute dependencies
  putStrLn "Step 2: Computing dependencies..."
  hFlush stdout
  let termDeps = moduleTermDepsTransitive namespaceMap [modToGen]
  putStrLn $ "  Term dep modules: " ++ show (length termDeps)
  let dataElements = concatMap moduleBindings termDeps
  putStrLn $ "  Data elements: " ++ show (length dataElements)

  let typeDeps = moduleTypeDepsTransitive namespaceMap [modToGen]
  putStrLn $ "  Type dep modules: " ++ show (length typeDeps)
  let schemaElements = filter isNativeType $ concatMap moduleBindings (typeDeps ++ [modToGen])
  putStrLn $ "  Schema elements: " ++ show (length schemaElements)

  -- Step 3: Build graphs
  putStrLn "Step 3: Building graphs..."
  hFlush stdout
  let bsGraph = bootstrapGraph
  putStrLn $ "  Bootstrap graph primitives: " ++ show (M.size $ graphPrimitives bsGraph)
  let schemaGraph = elementsToGraph bsGraph M.empty schemaElements
  let schemaTypes = case Environment.schemaGraphToTypingEnvironment schemaGraph of
        Left _ -> M.empty
        Right r -> r
  putStrLn $ "  Schema types: " ++ show (M.size schemaTypes)
  let dataGraph = elementsToGraph bsGraph schemaTypes dataElements
  putStrLn $ "  Data graph built"

  -- Step 4: Get bindings
  putStrLn "Step 4: Getting bindings..."
  hFlush stdout
  let bins0 = graphToBindings dataGraph
  putStrLn $ "  Bindings: " ++ show (length bins0)

  -- Step 5: Rebuild graph (as dataGraphToDefinitions does)
  putStrLn "Step 5: Rebuilding graph..."
  hFlush stdout
  let g0 = dataGraph
  let rebuildGraph bindings = let bg = buildGraph bindings M.empty (graphPrimitives g0)
                              in Graph {
                                graphBoundTerms = graphBoundTerms bg,
                                graphBoundTypes = graphBoundTypes bg,
                                graphClassConstraints = graphClassConstraints bg,
                                graphLambdaVariables = graphLambdaVariables bg,
                                graphMetadata = graphMetadata bg,
                                graphPrimitives = graphPrimitives bg,
                                graphSchemaTypes = graphSchemaTypes g0,
                                graphTypeVariables = graphTypeVariables bg
                              }
  let g1 = rebuildGraph bins0
  putStrLn $ "  Rebuilt bound terms: " ++ show (M.size $ graphBoundTerms g1)
  putStrLn $ "  Rebuilt primitives: " ++ show (M.size $ graphPrimitives g1)

  -- Step 6: Inference
  putStrLn "Step 6: Calling inferGraphTypes..."
  hFlush stdout
  case inferGraphTypes cx bins0 g1 of
    Left err -> putStrLn $ "  ERROR: " ++ ShowError.error err
    Right ((g2, bs), cx2) -> do
      putStrLn $ "  OK: " ++ show (length bs) ++ " bindings"

  -- Step 7: Full generateSourceFiles
  putStrLn "Step 7: Calling generateSourceFiles..."
  hFlush stdout
  case generateSourceFiles moduleToHaskell haskellLanguage True False False False bootstrapGraph uni [modToGen] cx of
    Left err -> putStrLn $ "  ERROR: " ++ ShowError.error err
