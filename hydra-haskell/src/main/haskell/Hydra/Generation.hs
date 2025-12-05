-- | Entry point for Hydra code generation utilities

module Hydra.Generation where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Ext.Haskell.Coder
import Hydra.Ext.Haskell.Language
import Hydra.Ext.Org.Json.Coder
import Hydra.Staging.Yaml.Modules
import Hydra.Staging.Yaml.Language
import Hydra.Sources.Libraries
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Inference as Inference
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Sources.All as Sources

import qualified Control.Monad as CM
import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Directory as SD
import qualified Data.Maybe as Y


generateSources
  :: (Module -> [Definition] -> Flow Graph (M.Map FilePath String))
  -> Language
  -> Bool
  -> FilePath
  -> [Module]
  -> IO ()
generateSources printDefinitions lang doExpand basePath mods = do
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
    -- Note: we assume that no module contains both type-level and term-level elements
    isSchemaModule mod = not $ L.null $ L.filter isNativeType $ moduleElements mod
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

    generateDataFiles = if L.null dataModules
        then pure []  -- No data modules to process
        else withTrace "generate data files" $ do
          (g1, defLists) <- dataGraphToDefinitions constraints doExpand g0 nameLists
          withState g1 $ do
            -- Refresh modules with elements from the inferred graph (which have type annotations)
            let refreshedMods = refreshModule (graphElements g1) <$> dataModules
            maps <- CM.zipWithM forEachModule refreshedMods defLists
            return $ L.concat (M.toList <$> maps)
      where
        g0 = modulesToGraph dataModules
        nameLists = fmap (fmap bindingName . moduleElements) dataModules
        forEachModule mod defs = withTrace ("data module " ++ unNamespace (moduleNamespace mod)) $
          printDefinitions mod (fmap DefinitionTerm defs)
        refreshModule els mod = mod {
          moduleElements = Y.catMaybes ((\e -> M.lookup (bindingName e) els) <$> moduleElements mod)}

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
writeHaskell = generateSources moduleToHaskell haskellLanguage False

-- writeJson :: FP.FilePath -> [Module] -> IO ()
-- writeJson = generateSources Json.printModule

-- | YAML generation - only processes data modules (term definitions), skips schema modules
writeYaml :: FP.FilePath -> [Module] -> IO ()
writeYaml basePath mods = do
    mfiles <- runFlow bootstrapGraph generateFiles
    case mfiles of
      Nothing -> fail "Failed to generate YAML files"
      Just files -> mapM_ writePair files
  where
    constraints = languageConstraints yamlLanguage
    -- Only process data modules (modules without native types)
    dataModules = L.filter (not . hasNativeTypes) mods
    hasNativeTypes mod = not $ L.null $ L.filter isNativeType $ moduleElements mod

    generateFiles = if L.null dataModules
        then pure []
        else withTrace "generate YAML files" $ do
          (g1, defLists) <- dataGraphToDefinitions constraints True g0 nameLists
          withState g1 $ do
            maps <- CM.zipWithM forEachModule dataModules defLists
            return $ L.concat (M.toList <$> maps)
      where
        g0 = modulesToGraph dataModules
        nameLists = fmap (fmap bindingName . moduleElements) dataModules
        forEachModule mod defs = withTrace ("data module " ++ unNamespace (moduleNamespace mod)) $
          moduleToYaml mod (fmap DefinitionTerm defs)

    writePair (path, contents) = do
      let fullPath = basePath FP.</> path
      SD.createDirectoryIfMissing True $ FP.takeDirectory fullPath
      writeFile fullPath contents

-- | Generate the lexicon content from a graph
generateLexicon :: Graph -> Flow Graph String
generateLexicon graph = do
  let bindings = M.elems $ graphElements graph
      primitives = M.elems $ graphPrimitives graph
      (typeBindings, termBindings) = L.partition isNativeType bindings
      sortedPrimitives = L.sortBy comparePrimitiveNames primitives
      sortedTypes = L.sortBy compareBindingNames typeBindings
      sortedTerms = L.sortBy compareBindingNames termBindings
  typeLines <- CM.mapM formatTypeBinding sortedTypes
  let termLines = fmap formatTermBinding sortedTerms
      primitiveLines = fmap formatPrimitive sortedPrimitives
  return $ "Primitives:\n" ++ unlines primitiveLines ++
    "\nTypes:\n" ++ unlines typeLines ++
    "\nTerms:\n" ++ unlines termLines
  where
    compareBindingNames a b = compare (bindingName a) (bindingName b)
    comparePrimitiveNames a b = compare (primitiveName a) (primitiveName b)

-- | Format a type binding for the lexicon
formatTypeBinding :: Binding -> Flow Graph String
formatTypeBinding binding = do
  let name = unName $ bindingName binding
  typ <- DecodeCore.type_ (bindingTerm binding)
  let typeStr = ShowCore.type_ typ
  return $ "  " ++ name ++ " = " ++ typeStr

-- | Format a term binding for the lexicon
formatTermBinding :: Binding -> String
formatTermBinding binding =
  let name = unName $ bindingName binding
      typeStr = case bindingType binding of
        Just scheme -> ShowCore.typeScheme scheme
        Nothing -> "?"
  in "  " ++ name ++ " : " ++ typeStr

-- | Format a primitive for the lexicon
formatPrimitive :: Primitive -> String
formatPrimitive prim =
  let name = unName $ primitiveName prim
      typeStr = ShowCore.typeScheme (primitiveType prim)
  in "  " ++ name ++ " : " ++ typeStr

-- | Generate and write the lexicon file
writeLexicon :: FilePath -> IO ()
writeLexicon path = do
  let g0 = modulesToGraph Sources.kernelModules
  mg1 <- runFlow g0 (Inference.inferGraphTypes g0)
  case mg1 of
    Nothing -> fail "Type inference failed"
    Just g1 -> do
      mcontent <- runFlow g1 (generateLexicon g1)
      case mcontent of
        Nothing -> fail "Lexicon generation failed"
        Just content -> do
          writeFile path content
          putStrLn $ "Lexicon written to " ++ path

-- | Generate the lexicon to the standard location
writeLexiconToStandardPath :: IO ()
writeLexiconToStandardPath = writeLexicon "../docs/hydra-lexicon.txt"
