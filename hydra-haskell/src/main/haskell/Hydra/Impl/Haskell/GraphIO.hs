module Hydra.Impl.Haskell.GraphIO where

import Hydra.Errors
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Extras
import Hydra.Ext.Haskell.Serde
import Hydra.Util.Codetree.Print
import Hydra.Impl.Haskell.Sources.CoreGraph
import Hydra.Impl.Haskell.Sources.Basics
import Hydra.Impl.Haskell.Sources.Adapters.Utils
import Hydra.Util.Formatting
import Hydra.Impl.Haskell.Dsl.Standard
import Hydra.Impl.Haskell.Sources.Libraries
import qualified Hydra.Lib.Strings as Strings

import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Directory as SD


graphToHaskell :: (Default a, Eq a, Ord a, Read a, Show a) => Context a -> Graph a -> Maybe FilePath -> IO ()
graphToHaskell cx g path = do
  case dataGraphToHaskellString cx g of
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

generateHydraHaskell :: FP.FilePath -> IO ()
generateHydraHaskell hydraHome = do
    writeDataGraph basicsGraph
    writeDataGraph adaptersUtilsGraph
  where
    baseDir = FP.combine hydraHome "hydra-haskell"
    writeDataGraph g = do
      let cx = standardContext {
             contextGraphs = GraphSet (M.fromList [(graphName g, g), ("hydra/core", hydraCoreGraph)]) (graphName g),
             contextFunctions = M.fromList $ fmap (\p -> (primitiveFunctionName p, p)) standardPrimitives,
             contextElements = M.empty}
      graphToHaskell cx g $ Just $ FP.combine baseDir $ "src/gen-main/haskell/" ++ nameToHaskellFileName (graphName g)
    nameToHaskellFileName name = L.intercalate "/" (capitalize <$> Strings.splitOn "/" name) ++ ".hs"
