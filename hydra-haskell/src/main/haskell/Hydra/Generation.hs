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


-- TODO: deprecated
generateSources :: (Module -> Flow Graph (M.Map FilePath String)) -> FilePath -> [Module] -> IO ()
generateSources printModule basePath mods = do
    mfiles <- runFlow bootstrapGraph generateFiles
    case mfiles of
      Nothing -> fail "Transformation failed"
      Just files -> mapM_ writePair files
  where
    generateFiles = withTrace "generate files" $ withState (modulesToGraph mods) $ do
      g <- getState
      g1 <- inferGraphTypes g
      withState g1 $ do
          maps <- CM.mapM forModule $ refreshModule (graphElements g1) <$> mods
          return $ L.concat (M.toList <$> maps)
        where
          refreshModule els mod = mod {
            moduleElements = Y.catMaybes ((\e -> M.lookup (bindingName e) els) <$> moduleElements mod)}

    writePair (path, s) = do
        let fullPath = FP.combine basePath path
        SD.createDirectoryIfMissing True $ FP.takeDirectory fullPath
        writeFile fullPath withNewline
      where
        withNewline = if L.isSuffixOf "\n" s then s else s ++ "\n"

    forModule mod = withTrace ("module " ++ unNamespace (moduleNamespace mod)) $ printModule mod

generateSourcesSimple :: (Module -> [Definition] -> Flow Graph (M.Map FilePath String)) -> Language -> Bool
                      -> FilePath -> [Module] -> IO ()
generateSourcesSimple printDefinitions lang doExpand basePath mods = do
    mschemaFiles <- runFlow bootstrapGraph generateSchemaFiles
    case mschemaFiles of
      Nothing -> fail "Failed to generate schema files"
      Just files -> mapM_ writePair files
    mdataFiles <- runFlow bootstrapGraph generateDataFiles
    case mdataFiles of
      Nothing -> fail "Failed to generate data files"
      Just files -> mapM_ writePair files
  where
    constraints = languageConstraints lang
    isTypeElement el = case deannotateTerm (bindingTerm el) of
      TermUnion inj -> injectionTypeName inj == _Type
      _ -> False
    -- Note: we assume that no module contains both type-level and term-level elements
    isSchemaModule mod = not $ L.null $ L.filter isTypeElement $ moduleElements mod
    (schemaModules, dataModules) = L.partition isSchemaModule mods

    generateSchemaFiles = withTrace "generate schema files" $ do
        (tmap, defLists) <- schemaGraphToDefinitions constraints g0 nameLists
        withState g0 $ do
          maps <- CM.zipWithM forEachModule schemaModules defLists
          return $ L.concat (M.toList <$> maps)
      where
        g0 = modulesToGraph schemaModules
        nameLists = fmap (fmap bindingName . moduleElements) schemaModules
        forEachModule mod defs = withTrace ("schema module " ++ unNamespace (moduleNamespace mod)) $
          printDefinitions mod (fmap DefinitionType defs)

    generateDataFiles = withTrace "generate data files" $ do
        (g1, defLists) <- dataGraphToDefinitions constraints doExpand g0 nameLists
        withState g1 $ do
          maps <- CM.zipWithM forEachModule dataModules defLists
          return $ L.concat (M.toList <$> maps)
      where
        g0 = modulesToGraph dataModules
        nameLists = fmap (fmap bindingName . moduleElements) dataModules
        forEachModule mod defs = withTrace ("data module " ++ unNamespace (moduleNamespace mod)) $
          printDefinitions mod (fmap DefinitionTerm defs)

    writePair (path, s) = do
        let fullPath = FP.combine basePath path
        SD.createDirectoryIfMissing True $ FP.takeDirectory fullPath
        writeFile fullPath withNewline
      where
        withNewline = if L.isSuffixOf "\n" s then s else s ++ "\n"

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
