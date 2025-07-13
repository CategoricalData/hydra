-- | Entry point for Hydra code generation utilities

module Hydra.Generation where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Ext.Haskell.Coder
import Hydra.Ext.Org.Json.Coder
import Hydra.Staging.Yaml.Modules
import Hydra.Sources.Libraries

import qualified Control.Monad as CM
import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Directory as SD
import qualified Data.Maybe as Y


generateSources :: (Module -> Flow Graph (M.Map FilePath String)) -> FilePath -> [Module] -> IO ()
generateSources printModule basePath mods = do
    mfiles <- runFlow bootstrapGraph generateFiles
    case mfiles of
      Nothing -> fail "Transformation failed"
      Just files -> mapM_ writePair files
  where
    generateFiles = do
      withTrace "generate files" $ do
        withState (modulesToGraph mods) $ do
          g <- getState
          g1 <- inferGraphTypes g
          withState g1 $ do
              maps <- CM.mapM forModule $ refreshModule (graphElements g1) <$> mods
              return $ L.concat (M.toList <$> maps)
            where
              refreshModule els mod = mod {
                moduleElements = Y.catMaybes ((\e -> M.lookup (elementName e) els) <$> moduleElements mod)}

    writePair (path, s) = do
        let fullPath = FP.combine basePath path
        SD.createDirectoryIfMissing True $ FP.takeDirectory fullPath
        writeFile fullPath withNewline
      where
        withNewline = if L.isSuffixOf "\n" s then s else s ++ "\n"

    forModule mod = withTrace ("module " ++ unNamespace (moduleNamespace mod)) $ printModule mod

-- TODO: move into the kernel
modulesToGraph :: [Module] -> Graph
modulesToGraph mods = elementsToGraph parent (Just schemaGraph) dataElements
  where
    parent = bootstrapGraph
    dataElements = L.concat (moduleElements <$> closedMods)
    schemaElements = L.concat (moduleElements <$> (L.concat (moduleTypeDependencies <$> closedMods)))
    schemaGraph = elementsToGraph bootstrapGraph Nothing schemaElements
    closedMods = L.concat (close <$> mods)
    close mod = mod:(L.concat (close <$> moduleTermDependencies mod))

printTrace :: Bool -> Trace -> IO ()
printTrace isError t = do
  CM.unless (L.null $ traceMessages t) $ do
      putStrLn $ if isError then "Flow failed. Messages:" else "Messages:"
      putStrLn $ indentLines $ traceSummary t

runFlow :: s -> Flow s a -> IO (Maybe a)
runFlow s f = do
    printTrace (Y.isNothing v) t
    return v
  where
    FlowState v _ t = unFlow f s emptyTrace

writeHaskell :: FilePath -> [Module] -> IO ()
writeHaskell = generateSources moduleToHaskell

-- writeJson :: FP.FilePath -> [Module] -> IO ()
-- writeJson = generateSources Json.printModule

writeYaml :: FP.FilePath -> [Module] -> IO ()
writeYaml = generateSources moduleToYaml
