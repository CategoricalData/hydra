module Hydra.Impl.Haskell.GraphIO (
  generateHydraHaskell,
  generateHydraScala,
) where

import Hydra.Core
import Hydra.Errors
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Extras
import Hydra.Ext.Haskell.Serde
import Hydra.Ext.Scala.Serde
import Hydra.Util.Codetree.Print
import Hydra.Impl.Haskell.Sources.Core
import Hydra.Impl.Haskell.Sources.Errors
import Hydra.Impl.Haskell.Sources.Graph
import Hydra.Impl.Haskell.Sources.Basics
import Hydra.Impl.Haskell.Sources.Adapters.Utils
import Hydra.Impl.Haskell.Sources.Ext.Haskell.Ast
import Hydra.Impl.Haskell.Sources.Ext.Json.Json
import Hydra.Impl.Haskell.Sources.Ext.Pegasus.Pdl
import Hydra.Impl.Haskell.Sources.Ext.Tinkerpop.V3
import Hydra.Impl.Haskell.Sources.Ext.Yaml.Model
import Hydra.Impl.Haskell.Sources.Util.Codetree.Ast
import Hydra.Util.Formatting
import Hydra.Impl.Haskell.Dsl.Standard
import Hydra.Impl.Haskell.Sources.Libraries
import qualified Hydra.Lib.Strings as Strings

import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Directory as SD


generateHydraHaskell :: FP.FilePath -> IO ()
generateHydraHaskell hydraHome = generateSources "haskell" nameToHaskellFileName dataGraphToHaskellString (FP.combine hydraHome "hydra-haskell")
  where
    nameToHaskellFileName name = L.intercalate "/" (capitalize <$> Strings.splitOn "/" name) ++ ".hs"

generateHydraScala :: FP.FilePath -> IO ()
generateHydraScala hydraHome = generateSources "scala" nameToScalaFileName dataGraphToScalaString (FP.combine hydraHome "hydra-scala")
  where
    nameToScalaFileName name = L.intercalate "/" (capitalize <$> Strings.splitOn "/" name) ++ ".scala"

generateSources :: String -> (GraphName -> FP.FilePath) -> (Context Meta -> Graph Meta -> Qualified String) -> FP.FilePath -> IO ()
generateSources langName toFile serialize baseDir = do
    writeDataGraph hydraCore []
    writeDataGraph hydraErrors []
    writeDataGraph hydraGraph [hydraCore]
    writeDataGraph basicsGraph []
    writeDataGraph adaptersUtilsGraph [basicsGraph]
    writeDataGraph haskellAst []
    writeDataGraph jsonJson []
    writeDataGraph pegasusPdl [jsonJson]
    writeDataGraph yamlModel []
    writeDataGraph codetreeAst []
    writeDataGraph tinkerpopV3 [hydraCore]
  where
    writeDataGraph g deps = do
      let cx = setContextElements (g:deps) $ standardContext {
             contextGraphs = GraphSet (M.fromList [
               (graphName g, g),
               ("hydra/core", hydraCore)]) (graphName g),
             contextFunctions = M.fromList $ fmap (\p -> (primitiveFunctionName p, p)) standardPrimitives}
      writeGraph serialize cx g $ Just $ FP.combine baseDir
        $ "src/gen-main/" ++ langName ++ "/" ++ toFile (graphName g)

writeGraph :: (Default m, Eq m, Ord m, Read m, Show m)
  => (Context m -> Graph m -> Qualified String)
  -> Context m -> Graph m -> Maybe FilePath -> IO ()
writeGraph serialize cx g path = do
  case serialize cx g of
    Qualified Nothing warnings -> putStrLn $ "Transformation failed: " ++ indent (unlines warnings)
    Qualified (Just s) warnings -> do
      if not (L.null warnings)
        then putStrLn $ "Warnings: " ++ indent (unlines warnings) ++ "\n"
        else pure ()
      case path of
        Nothing -> putStrLn s
        Just p -> do
          SD.createDirectoryIfMissing True $ FP.takeDirectory p
          writeFile p s
