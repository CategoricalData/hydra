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
-- import qualified Hydra.Encoding as Encoding  -- Temporarily disabled for regeneration
import qualified Hydra.Inference as Inference
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Sources.All as Sources
import qualified Hydra.Sources.Kernel.Types.Core as CoreTypes

import qualified Control.Monad as CM
import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified System.Directory as SD
import qualified Data.Maybe as Y


generateSources
  :: (Module -> [Definition] -> Flow Graph (M.Map FilePath String))
  -> Language
  -> Bool
  -> FilePath
  -> [Module]  -- ^ Universe: all modules for type/term resolution
  -> [Module]  -- ^ Modules to transform and generate
  -> IO ()
generateSources printDefinitions lang doExpand basePath universeModules modulesToGenerate =
    generateSourcesFor modulesToGenerate
  where
    -- Build namespace -> module map from universe and modules to generate
    namespaceMap = M.fromList [(moduleNamespace m, m) | m <- universeModules ++ modulesToGenerate]

    -- Schema modules: transitive closure of moduleTypeDependencies from modules to generate
    schemaMods = moduleTypeDependenciesTransitive namespaceMap modulesToGenerate
    schemaElements = L.filter isNativeType $ L.concat (moduleElements <$> (schemaMods ++ typeModulesToGenerate))

    -- Data modules: transitive closure of moduleTermDependencies from modules to generate
    -- Plus the modules to generate themselves (they contain the terms we want to generate)
    dataMods = moduleTermDependenciesTransitive namespaceMap modulesToGenerate
    dataElements = L.filter (not . isNativeType) $ L.concat (moduleElements <$> dataMods)

    -- Build the schema graph (types only)
    schemaGraph = elementsToGraph bootstrapGraph Nothing schemaElements

    -- Build the complete graph with schema and data elements
    dataGraph = elementsToGraph bootstrapGraph (Just schemaGraph) dataElements

    generateSourcesFor mods = do
--      fail $ "schema modules: " ++ show (unNamespace . moduleNamespace <$> schemaMods)
--      fail $ "data modules: " ++ show (unNamespace . moduleNamespace <$> dataMods)
--      fail $ "type modules: " ++ show (unNamespace . moduleNamespace <$> typeModulesToGenerate)
--      fail $ "term modules: " ++ show (unNamespace . moduleNamespace <$> termModulesToGenerate)
--      fail $ "schema elements: " ++ show (unName . bindingName <$> schemaElements)
--      fail $ "data elements: " ++ show (unName . bindingName <$> dataElements)

      mschemaFiles <- runFlow bootstrapGraph (generateTypeModules mods)
      case mschemaFiles of
        Nothing -> fail "Failed to generate schema files"
        Just files -> mapM_ writePair files
      mdataFiles <- runFlow bootstrapGraph (generateTermModules mods)
      case mdataFiles of
        Nothing -> fail "Failed to generate data files"
        Just files -> mapM_ writePair files

    constraints = languageConstraints lang

    isTypeModule mod = not $ L.null $ L.filter isNativeType $ moduleElements mod
    
    (typeModulesToGenerate, termModulesToGenerate) = L.partition isTypeModule modulesToGenerate

    generateTypeModules _ = withTrace "generate type modules" $ do
        if L.null typeModulesToGenerate
          then return []
          else do
            -- Only include type binding names, not term bindings that may be in mixed modules
            let nameLists = fmap (fmap bindingName . L.filter isNativeType . moduleElements) typeModulesToGenerate
            (tmap, defLists) <- schemaGraphToDefinitions constraints schemaGraph nameLists
            withState schemaGraph $ do
              maps <- CM.zipWithM forEachModule typeModulesToGenerate defLists
              return $ L.concat (M.toList <$> maps)
      where
        forEachModule m defs = withTrace ("type module " ++ unNamespace (moduleNamespace m)) $
          printDefinitions m (fmap DefinitionType defs)

    generateTermModules _ = do
        if L.null termModulesToGenerate
          then pure []
          else withTrace "generate term modules" $ do
            let nameLists = fmap (fmap bindingName . moduleElements) termModulesToGenerate
            (g1, defLists) <- dataGraphToDefinitions constraints doExpand dataGraph nameLists
            withState g1 $ do
              -- Refresh modules with elements from the inferred graph (which have type annotations)
              let refreshedMods = refreshModule (graphElements g1) <$> termModulesToGenerate
              maps <- CM.zipWithM forEachModule refreshedMods defLists
              return $ L.concat (M.toList <$> maps)
      where
        forEachModule m defs = withTrace ("term module " ++ unNamespace (moduleNamespace m)) $
          printDefinitions m (fmap DefinitionTerm defs)
        refreshModule els m = m {
          moduleElements = Y.catMaybes ((\e -> M.lookup (bindingName e) els) <$> moduleElements m)}

    writePair (path, s) = do
        let fullPath = FP.combine basePath path
        SD.createDirectoryIfMissing True $ FP.takeDirectory fullPath
        writeFile fullPath withNewline
      where
        withNewline = if L.isSuffixOf "\n" s then s else s ++ "\n"

moduleTermDependenciesTransitive :: M.Map Namespace Module -> [Module] -> [Module]
moduleTermDependenciesTransitive mapping modules = Y.catMaybes $ fmap (\n -> M.lookup n mapping) $ S.toList $ S.union
  (transitiveClosure moduleTermDependencies mapping modules)
  (S.fromList $ moduleNamespace <$> modules)

moduleTypeDependenciesTransitive :: M.Map Namespace Module -> [Module] -> [Module]
moduleTypeDependenciesTransitive mapping modules = Y.catMaybes $ fmap (\n -> M.lookup n mapping) $ S.toList $
  transitiveClosure moduleTypeDependencies mapping modules

-- | Build a graph from a list of modules.
-- Elements are partitioned into schema (type definitions) and data (term definitions)
-- based on the isNativeType predicate applied at the element level.
modulesToGraph :: [Module] -> [Module] -> Graph
modulesToGraph universeModules modules = elementsToGraph bootstrapGraph (Just schemaGraph) dataElements
  where
    universe = M.fromList [(moduleNamespace m, m) | m <- universeModules ++ modules]
    schemaModules = moduleTypeDependenciesTransitive universe modules
    dataModules = moduleTermDependenciesTransitive universe modules
    schemaElements = L.filter isNativeType $ L.concat (moduleElements <$> schemaModules)
    dataElements = L.filter (not . isNativeType) $ L.concat (moduleElements <$> dataModules)
    schemaGraph = elementsToGraph bootstrapGraph Nothing schemaElements

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

-- Compute transitive closure of dependencies
-- Excludes self-references (a module listing itself as a dependency),
-- but includes other start modules that are legitimate dependencies
transitiveClosure :: (Module -> [Namespace]) -> M.Map Namespace Module -> [Module] -> S.Set Namespace
transitiveClosure getDeps namespaceMap startMods = go initialDeps S.empty
  where
    -- Start with dependencies, excluding self-references
    initialDeps = S.fromList $ concat
      [L.filter (/= moduleNamespace m) (getDeps m) | m <- startMods]
    go pending visited
      | S.null pending = visited
      | otherwise =
          let newVisited = S.union visited pending
              nextDeps = S.fromList $ concat
                [getDeps m | ns <- S.toList pending, Just m <- [M.lookup ns namespaceMap]]
              newPending = S.difference nextDeps newVisited
          in go newPending newVisited

-- | Generate Haskell source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeHaskell :: FilePath -> [Module] -> [Module] -> IO ()
writeHaskell = generateSources moduleToHaskell haskellLanguage False

-- writeJson :: FP.FilePath -> [Module] -> IO ()
-- writeJson = generateSources Json.printModule

-- | YAML generation - only processes data modules (term definitions), skips schema modules
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeYaml :: FP.FilePath -> [Module] -> [Module] -> IO ()
writeYaml basePath universeModules modulesToGenerate = do
    mfiles <- runFlow bootstrapGraph (generateFiles modulesToGenerate)
    case mfiles of
      Nothing -> fail "Failed to generate YAML files"
      Just files -> mapM_ writePair files
  where
    constraints = languageConstraints yamlLanguage
    hasNativeTypes mod = not $ L.null $ L.filter isNativeType $ moduleElements mod

    -- Build the complete universe by computing transitive closure of dependencies
    namespaceMap = M.fromList [(moduleNamespace m, m) | m <- universeModules ++ modulesToGenerate]

    transitiveClosure :: [Module] -> S.Set Namespace
    transitiveClosure startMods = go (S.fromList $ moduleNamespace <$> startMods) S.empty
      where
        go pending visited
          | S.null pending = visited
          | otherwise =
              let newVisited = S.union visited pending
                  nextDeps = S.fromList $ concat
                    [moduleTermDependencies m ++ moduleTypeDependencies m
                    | ns <- S.toList pending
                    , Just m <- [M.lookup ns namespaceMap]]
                  newPending = S.difference nextDeps newVisited
              in go newPending newVisited

    allNeededNamespaces = transitiveClosure modulesToGenerate
    completeUniverse = [m | ns <- S.toList allNeededNamespaces, Just m <- [M.lookup ns namespaceMap]]
                    ++ modulesToGenerate

    generateFiles mods = do
        -- Only process data modules (modules without native types)
        let dataModules = L.filter (not . hasNativeTypes) mods
        if L.null dataModules
          then pure []
          else withTrace "generate YAML files" $ do
            let g0 = modulesToGraph completeUniverse completeUniverse  -- Use complete universe for full dependency resolution
                nameLists = fmap (fmap bindingName . moduleElements) dataModules
            (g1, defLists) <- dataGraphToDefinitions constraints True g0 nameLists
            withState g1 $ do
              maps <- CM.zipWithM forEachModule dataModules defLists
              return $ L.concat (M.toList <$> maps)
      where
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
  let g0 = modulesToGraph Sources.kernelModules Sources.kernelModules
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

-- | Generate encoder modules for a list of type modules
-- For each type module, generates an encoder module with functions to encode values to Terms
-- NOTE: Temporarily disabled for Module type change regeneration
-- generateEncoderModules :: [Module] -> Flow Graph [Module]
-- generateEncoderModules mods = do
--   results <- CM.mapM Encoding.encodeModule mods
--   return $ Y.catMaybes results

-- | Write encoder modules as Haskell to the given path
-- NOTE: Temporarily disabled for Module type change regeneration
-- writeEncoderHaskell :: FilePath -> [Module] -> IO ()
-- writeEncoderHaskell basePath typeMods = do
--   let graph = modulesToGraph typeMods
--   mresult <- runFlow graph (generateEncoderModules typeMods)
--   case mresult of
--     Nothing -> fail "Failed to generate encoder modules"
--     Just encoderMods -> do
--       -- Add core types module to each encoder module's type dependencies
--       -- since the encoders reference hydra.core.Term, hydra.core.Injection, etc.
--       let coreModule = CoreTypes.module_
--           withCoreDeps = fmap (addTypeDep coreModule) encoderMods
--       writeHaskell basePath withCoreDeps
--   where
--     addTypeDep dep mod = mod { moduleTypeDependencies = dep : moduleTypeDependencies mod }
