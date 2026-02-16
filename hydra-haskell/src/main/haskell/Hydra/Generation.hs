-- | Entry point for Hydra code generation utilities

module Hydra.Generation where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Ext.Haskell.Coder
import Hydra.Ext.Haskell.Language
import Hydra.Module (_Module)
import qualified Hydra.Json.Model as Json
import Hydra.Staging.Yaml.Modules
import Hydra.Staging.Yaml.Language
import Hydra.Sources.Libraries
import qualified Hydra.Decoding as Decoding
import qualified Hydra.Encoding as Encoding
import qualified Hydra.Sources.All as Sources
import qualified Hydra.Sources.Kernel.Types.Core as CoreTypes
import qualified Hydra.CodeGeneration as Generated

import qualified Control.Monad as CM
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Key as AK
import qualified Data.ByteString.Lazy as BS
import qualified Data.Scientific as SC
import qualified Data.Vector as V
import Data.Word (Word8)
import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified System.Directory as SD
import qualified Data.Maybe as Y


-- | Pure core of code generation. Delegated to the generated Hydra.CodeGeneration module.
generateSourceFiles
  :: (Module -> [Definition] -> Flow Graph (M.Map FilePath String))
  -> Language -> Bool -> Bool -> Bool
  -> Graph -> [Module] -> [Module]
  -> Flow Graph [(FilePath, String)]
generateSourceFiles = Generated.generateSourceFiles

-- Original staging code (preserved as reference):
-- generateSourceFiles printDefinitions lang doExpand doHoistCaseStatements doHoistPolymorphicLetBindings bsGraph universeModules modulesToGenerate = do
--     schemaFiles <- generateTypeModules
--     termFiles <- generateTermModules
--     return $ schemaFiles ++ termFiles
--   where
--     namespaceMap = M.fromList [(moduleNamespace m, m) | m <- universeModules ++ modulesToGenerate]
--     schemaMods = moduleTypeDependenciesTransitive namespaceMap modulesToGenerate
--     schemaElements = L.filter isNativeType $ L.concat (moduleElements <$> (schemaMods ++ typeModulesToGenerate))
--     dataMods = moduleTermDependenciesTransitive namespaceMap modulesToGenerate
--     dataElements = L.concat (moduleElements <$> dataMods)
--     schemaGraph = elementsToGraph bsGraph Nothing schemaElements
--     dataGraph = elementsToGraph bsGraph (Just schemaGraph) dataElements
--     constraints = languageConstraints lang
--     isTypeModule mod = not $ L.null $ L.filter isNativeType $ moduleElements mod
--     (typeModulesToGenerate, termModulesToGenerate) = L.partition isTypeModule modulesToGenerate
--     generateTypeModules = withTrace "generate type modules" $ do
--         if L.null typeModulesToGenerate
--           then return []
--           else do
--             let nameLists = fmap (fmap bindingName . L.filter isNativeType . moduleElements) typeModulesToGenerate
--             (tmap, defLists) <- schemaGraphToDefinitions constraints schemaGraph nameLists
--             withState schemaGraph $ do
--               maps <- CM.zipWithM forEachModule typeModulesToGenerate defLists
--               return $ L.concat (M.toList <$> maps)
--       where
--         forEachModule m defs = withTrace ("type module " ++ unNamespace (moduleNamespace m)) $
--           printDefinitions m (fmap DefinitionType defs)
--     generateTermModules = do
--         if L.null termModulesToGenerate
--           then pure []
--           else withTrace "generate term modules" $ do
--             let namespaces = fmap moduleNamespace termModulesToGenerate
--             (g1, defLists) <- dataGraphToDefinitions constraints doExpand doHoistCaseStatements doHoistPolymorphicLetBindings dataGraph namespaces
--             withState g1 $ do
--               let refreshedMods = refreshModule (graphElements g1) <$> termModulesToGenerate
--               maps <- CM.zipWithM forEachModule refreshedMods defLists
--               return $ L.concat (M.toList <$> maps)
--       where
--         forEachModule m defs = withTrace ("term module " ++ unNamespace (moduleNamespace m)) $
--           printDefinitions m (fmap DefinitionTerm defs)
--         refreshModule els m = m {
--           moduleElements = Y.catMaybes ((\e -> L.find (\b -> bindingName b == bindingName e) els) <$> moduleElements m)}

-- | Generate source files and write them to disk.
-- This is a thin I/O wrapper around 'generateSourceFiles'.
generateSources
  :: (Module -> [Definition] -> Flow Graph (M.Map FilePath String))
  -> Language
  -> Bool  -- ^ doExpand
  -> Bool  -- ^ doHoistCaseStatements
  -> Bool  -- ^ doHoistPolymorphicLetBindings
  -> FilePath
  -> [Module]  -- ^ Universe
  -> [Module]  -- ^ Modules to generate
  -> IO ()
generateSources printDefinitions lang doExpand doHoistCaseStatements doHoistPolymorphicLetBindings basePath universeModules modulesToGenerate = do
    mfiles <- runFlow bootstrapGraph $
      generateSourceFiles printDefinitions lang doExpand doHoistCaseStatements doHoistPolymorphicLetBindings bootstrapGraph universeModules modulesToGenerate
    case mfiles of
      Nothing -> fail "Failed to generate source files"
      Just files -> mapM_ writePair files
  where
    writePair (path, s) = do
        let fullPath = FP.combine basePath path
        SD.createDirectoryIfMissing True $ FP.takeDirectory fullPath
        writeFile fullPath withNewline
      where
        withNewline = if L.isSuffixOf "\n" s then s else s ++ "\n"

-- | Delegated to Generated.moduleTermDepsTransitive
moduleTermDependenciesTransitive :: M.Map Namespace Module -> [Module] -> [Module]
moduleTermDependenciesTransitive = Generated.moduleTermDepsTransitive

-- Original staging code (preserved as reference):
-- moduleTermDependenciesTransitive mapping modules = Y.catMaybes $ fmap (\n -> M.lookup n mapping) $ S.toList $ S.union
--   (transitiveClosure moduleTermDependencies mapping modules)
--   (S.fromList $ moduleNamespace <$> modules)

-- | Delegated to Generated.moduleTypeDepsTransitive
moduleTypeDependenciesTransitive :: M.Map Namespace Module -> [Module] -> [Module]
moduleTypeDependenciesTransitive = Generated.moduleTypeDepsTransitive

-- Original staging code (preserved as reference):
-- moduleTypeDependenciesTransitive mapping modules = Y.catMaybes $ fmap (\n -> M.lookup n mapping) typeNamespaces
--   where
--     termMods = moduleTermDependenciesTransitive mapping modules
--     typeNamespaces = S.toList $ transitiveClosure moduleTypeDependencies mapping termMods

-- | Build a graph from a list of modules, using an explicit bootstrap graph.
-- Delegated to Generated.modulesToGraph.
modulesToGraphWith :: Graph -> [Module] -> [Module] -> Graph
modulesToGraphWith = Generated.modulesToGraph

-- Original staging code (preserved as reference):
-- modulesToGraphWith bsGraph universeModules modules = elementsToGraph bsGraph (Just schemaGraph) dataElements
--   where
--     universe = M.fromList [(moduleNamespace m, m) | m <- universeModules ++ modules]
--     schemaModules = moduleTypeDependenciesTransitive universe modules
--     dataModules = moduleTermDependenciesTransitive universe modules
--     schemaElements = L.filter isNativeType $ L.concat (moduleElements <$> (schemaModules ++ modules))
--     dataElements = L.filter (not . isNativeType) $ L.concat (moduleElements <$> dataModules)
--     schemaGraph = elementsToGraph bsGraph Nothing schemaElements

-- | Build a graph from a list of modules using the Haskell bootstrapGraph.
-- Thin wrapper around modulesToGraphWith.
modulesToGraph :: [Module] -> [Module] -> Graph
modulesToGraph = modulesToGraphWith bootstrapGraph

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

-- | Delegated to Generated.transitiveDeps
transitiveClosure :: (Module -> [Namespace]) -> M.Map Namespace Module -> [Module] -> S.Set Namespace
transitiveClosure = Generated.transitiveDeps

-- Original staging code (preserved as reference):
-- transitiveClosure getDeps namespaceMap startMods = go initialDeps S.empty
--   where
--     initialDeps = S.fromList $ concat
--       [L.filter (/= moduleNamespace m) (getDeps m) | m <- startMods]
--     go pending visited
--       | S.null pending = visited
--       | otherwise =
--           let newVisited = S.union visited pending
--               nextDeps = S.fromList $ concat
--                 [getDeps m | ns <- S.toList pending, Just m <- [M.lookup ns namespaceMap]]
--               newPending = S.difference nextDeps newVisited
--           in go newPending newVisited

-- | Generate Haskell source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeHaskell :: FilePath -> [Module] -> [Module] -> IO ()
writeHaskell = generateSources moduleToHaskell haskellLanguage False False False

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
                namespaces = fmap moduleNamespace dataModules
            -- Infer types on the data graph before adaptation (eta expansion requires types)
            g0' <- inferGraphTypes g0
            (g1, defLists) <- dataGraphToDefinitions constraints True False False g0' namespaces
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

-- | Generate the lexicon content from a graph. Delegated to Generated.generateLexicon.
generateLexicon :: Graph -> Flow Graph String
generateLexicon = Generated.generateLexicon

-- Original staging code (preserved as reference):
-- generateLexicon graph = do
--   let bindings = graphElements graph
--       primitives = M.elems $ graphPrimitives graph
--       (typeBindings, termBindings) = L.partition isNativeType bindings
--       sortedPrimitives = L.sortBy comparePrimitiveNames primitives
--       sortedTypes = L.sortBy compareBindingNames typeBindings
--       sortedTerms = L.sortBy compareBindingNames termBindings
--   typeLines <- CM.mapM formatTypeBinding sortedTypes
--   let termLines = fmap formatTermBinding sortedTerms
--       primitiveLines = fmap formatPrimitive sortedPrimitives
--   return $ "Primitives:\n" ++ unlines primitiveLines ++
--     "\nTypes:\n" ++ unlines typeLines ++
--     "\nTerms:\n" ++ unlines termLines
--   where
--     compareBindingNames a b = compare (bindingName a) (bindingName b)
--     comparePrimitiveNames a b = compare (primitiveName a) (primitiveName b)

-- | Format a type binding for the lexicon. Delegated to Generated.formatTypeBinding.
formatTypeBinding :: Binding -> Flow Graph String
formatTypeBinding = Generated.formatTypeBinding

-- Original staging code (preserved as reference):
-- formatTypeBinding binding = do
--   g <- Monads.getState
--   typ <- Monads.eitherToFlow Util.unDecodingError $ DecodeCore.type_ g (bindingTerm binding)
--   return $ "  " ++ unName (bindingName binding) ++ " = " ++ ShowCore.type_ typ

-- | Format a term binding for the lexicon. Delegated to Generated.formatTermBinding.
formatTermBinding :: Binding -> String
formatTermBinding = Generated.formatTermBinding

-- Original staging code (preserved as reference):
-- formatTermBinding binding =
--   let name = unName $ bindingName binding
--       typeStr = Y.maybe "?" ShowCore.typeScheme (bindingType binding)
--   in "  " ++ name ++ " : " ++ typeStr

-- | Format a primitive for the lexicon. Delegated to Generated.formatPrimitive.
formatPrimitive :: Primitive -> String
formatPrimitive = Generated.formatPrimitive

-- Original staging code (preserved as reference):
-- formatPrimitive prim =
--   let name = unName $ primitiveName prim
--       typeStr = ShowCore.typeScheme (primitiveType prim)
--   in "  " ++ name ++ " : " ++ typeStr

-- | Perform type inference and generate the lexicon. Delegated to Generated.inferAndGenerateLexicon.
inferAndGenerateLexicon :: Graph -> [Module] -> Flow Graph String
inferAndGenerateLexicon = Generated.inferAndGenerateLexicon

-- | Generate and write the lexicon file (IO wrapper).
writeLexicon :: FilePath -> IO ()
writeLexicon path = do
  mcontent <- runFlow bootstrapGraph
    (Generated.inferAndGenerateLexicon bootstrapGraph Sources.kernelModules)
  case mcontent of
    Nothing -> fail "Lexicon generation failed"
    Just content -> do
      writeFile path content
      putStrLn $ "Lexicon written to " ++ path

-- | Generate the lexicon to the standard location
writeLexiconToStandardPath :: IO ()
writeLexiconToStandardPath = writeLexicon "../docs/hydra-lexicon.txt"


-- | Convert a generated Module into a Source module. Delegated to Generated.moduleToSourceModule.
moduleToSourceModule :: Module -> Module
moduleToSourceModule = Generated.moduleToSourceModule

-- Original staging code (preserved as reference):
-- moduleToSourceModule m = Module {
--     moduleNamespace = sourceNamespace,
--     moduleElements = [moduleBinding],
--     moduleTermDependencies = [ModuleTypes.ns],
--     moduleTypeDependencies = [ModuleTypes.ns],
--     moduleDescription = Just $ "Source module for " ++ unNamespace (moduleNamespace m)
--   }
--   where
--     sourceNamespace = Namespace $ "hydra.sources." ++
--       L.intercalate "." (drop 1 $ LS.splitOn "." $ unNamespace $ moduleNamespace m)
--     moduleBinding = Binding {
--       bindingName = Name $ unNamespace sourceNamespace ++ ".module_",
--       bindingTerm = EncodeModule.module_ m,
--       bindingType = Nothing
--     }

----------------------------------------

-- | Pure Flow-based coder module generation. Delegated to Generated.generateCoderModules.
generateCoderModules :: (Module -> Flow Graph (Maybe Module)) -> Graph -> [Module] -> [Module] -> Flow Graph [Module]
generateCoderModules = Generated.generateCoderModules

-- Original staging code (preserved as reference):
-- generateCoderModules codec bsGraph universeModules typeModules =
--     let graph = modulesToGraph bsGraph universeModules universeModules
--     in case graphSchema graph of
--       Nothing -> Flows.fail "No schema graph available"
--       Just schemaGraph -> Monads.withState schemaGraph $
--         fmap catMaybes $ CM.mapM codec typeModules

-- | IO wrapper for generateCoderModules. Evaluates the Flow and handles errors.
generateCoderModulesIO :: (Module -> Flow Graph (Maybe Module)) -> String -> [Module] -> [Module] -> IO [Module]
generateCoderModulesIO codec label universeModules typeModules = do
    let graph = modulesToGraph universeModules universeModules
    case graphSchema graph of
      Nothing -> fail "No schema graph available"
      Just schemaGraph -> do
        mresult <- runFlow schemaGraph (Generated.generateCoderModules codec bootstrapGraph universeModules typeModules)
        case mresult of
          Nothing -> fail $ "Failed to generate " ++ label ++ " modules"
          Just results -> return results

generateDecoderModules :: [Module] -> [Module] -> IO [Module]
generateDecoderModules = generateCoderModulesIO Decoding.decodeModule "decoder"

generateEncoderModules :: [Module] -> [Module] -> IO [Module]
generateEncoderModules = generateCoderModulesIO Encoding.encodeModule "encoder"

----------------------------------------

-- | Generate encoder/decoder Source modules for a list of type modules.
-- These are Source modules that define `module_` bindings containing the encoder Modules as Terms.
generateCoderSourceModules :: ([Module] -> [Module] -> IO [Module]) -> [Module] -> [Module] -> IO [Module]
generateCoderSourceModules generate universeModules typeModules = do
  sourceMods <- generate universeModules typeModules
  return $ fmap moduleToSourceModule sourceMods

generateDecoderSourceModules :: [Module] -> [Module] -> IO [Module]
generateDecoderSourceModules = generateCoderSourceModules generateDecoderModules

generateEncoderSourceModules :: [Module] -> [Module] -> IO [Module]
generateEncoderSourceModules = generateCoderSourceModules generateEncoderModules

----------------------------------------

writeCoderSourceHaskell :: ([Module] -> [Module] -> IO [Module]) -> FilePath -> [Module] -> [Module] -> IO ()
writeCoderSourceHaskell generate basePath universeModules typeModules = do
  sourceMods <- generateCoderSourceModules generate universeModules typeModules
  -- The source modules need the Module encoder/decoder and Core types
  writeHaskell basePath (universeModules ++ sourceMods) sourceMods

-- | Write decoder Source modules as Haskell to the given path.
-- These typically go to src/gen-main/haskell/Hydra/Sources/Decode/
writeDecoderSourceHaskell :: FilePath -> [Module] -> [Module] -> IO ()
writeDecoderSourceHaskell = writeCoderSourceHaskell generateDecoderModules

-- | Write encoder Source modules as Haskell to the given path.
-- These typically go to src/gen-main/haskell/Hydra/Sources/Encode/
writeEncoderSourceHaskell :: FilePath -> [Module] -> [Module] -> IO ()
writeEncoderSourceHaskell = writeCoderSourceHaskell generateEncoderModules

----------------------------------------

-- | Write encoder/decoder modules as Haskell to the given path.
-- First argument: generator function for encoder or decoder modules
-- Second argument: output directory
-- Third argument: universe modules (all modules for type/term resolution)
-- Fourth argument: type modules to generate encoders/decoders for
-- Note: This function bypasses type inference; for efficiency, we generate type signatures directly.
writeCoderHaskell :: ([Module] -> [Module] -> IO [Module]) -> FilePath -> [Module] -> [Module] -> IO ()
writeCoderHaskell generate basePath universeModules typeModules = do
    coderMods <- generate universeModules typeModules
    -- Add core types namespace to each encoder/decoder module's type dependencies
    -- since the encoders/decoders reference hydra.core.Term, hydra.core.Injection, etc.
    let withCoreDeps = fmap addCoreDep coderMods
    writeHaskell basePath universeModules withCoreDeps
  where
    addCoreDep m = m { moduleTypeDependencies = CoreTypes.ns : moduleTypeDependencies m }

writeDecoderHaskell :: FilePath -> [Module] -> [Module] -> IO ()
writeDecoderHaskell = writeCoderHaskell generateDecoderModules

writeEncoderHaskell :: FilePath -> [Module] -> [Module] -> IO ()
writeEncoderHaskell = writeCoderHaskell generateEncoderModules

----------------------------------------
-- Module Inference
----------------------------------------

-- | Pure Flow-based type inference on modules. Delegated to Generated.inferModules.
inferModules :: Graph -> [Module] -> [Module] -> Flow Graph [Module]
inferModules = Generated.inferModules

-- Original staging code (preserved as reference):
-- inferModules bsGraph universeMods targetMods = do
--   let g0 = modulesToGraph universeMods universeMods
--   g1 <- Inference.inferGraphTypes g0
--   let inferredElements = graphElements g1
--       isTypeModule mod = all isNativeType (moduleElements mod)
--       refreshModule m
--         | isTypeModule m = m
--         | otherwise = m { moduleElements = catMaybes
--             ((\e -> find (\b -> bindingName b == bindingName e) inferredElements)
--               <$> moduleElements m) }
--   return $ fmap refreshModule targetMods

-- | IO wrapper for inferModules. Evaluates the Flow and handles errors.
inferModulesIO :: [Module] -> [Module] -> IO [Module]
inferModulesIO universeMods targetMods = do
  let g0 = modulesToGraph universeMods universeMods
  mresult <- runFlow g0 (Generated.inferModules bootstrapGraph universeMods targetMods)
  case mresult of
    Nothing -> fail "Type inference failed on modules"
    Just mods -> return mods

----------------------------------------
-- JSON Module Export
----------------------------------------

-- | Convert a Module to a JSON string. Delegated to Generated.moduleToJson.
moduleToJson :: Module -> Either String String
moduleToJson = Generated.moduleToJson

-- Original staging code (preserved as reference):
-- moduleToJson mod =
--     let term = EncodeModule.module_ mod
--     in case JsonEncode.toJson term of
--       Left err -> Left err
--       Right json -> Right $ JsonWriter.printJson json

-- | Write a single module to a JSON file.
-- The file path is derived from the module namespace.
writeModuleJson :: FilePath -> Module -> IO ()
writeModuleJson basePath mod = do
    case moduleToJson mod of
      Left err -> fail $ "Failed to convert module to JSON: " ++ unNamespace (moduleNamespace mod) ++ ": " ++ err
      Right jsonStr -> do
        let filePath = basePath FP.</> namespaceToPath (moduleNamespace mod) ++ ".json"
        SD.createDirectoryIfMissing True $ FP.takeDirectory filePath
        writeFile filePath (jsonStr ++ "\n")
        putStrLn $ "Wrote: " ++ filePath

-- | Write multiple modules to JSON files.
-- Each module is written to basePath/<namespace-path>.json
-- If doInfer is True, type inference is performed on the modules first.
-- The universe modules are used for type inference context (may include more modules
-- than those being written). If not inferring, the universe is ignored.
writeModulesJson :: Bool -> FilePath -> [Module] -> [Module] -> IO ()
writeModulesJson doInfer basePath universeMods mods = do
  mods' <- if doInfer then inferModulesIO universeMods mods else return mods
  mapM_ (writeModuleJson basePath) mods'

-- | Convert a namespace to a file path (e.g., "hydra.monads" -> "hydra/monads").
-- Delegated to Generated.namespaceToPath.
namespaceToPath :: Namespace -> FilePath
namespaceToPath = Generated.namespaceToPath

-- Original staging code (preserved as reference):
-- namespaceToPath ns = L.intercalate "/" $ LS.splitOn "." $ unNamespace ns

----------------------------------------
-- JSON Module Import
----------------------------------------

-- | Convert an Aeson JSON value to a Hydra JSON value.
aesonToHydra :: A.Value -> Json.Value
aesonToHydra v = case v of
  A.Object km -> Json.ValueObject $ M.fromList (mapPair <$> AKM.toList km)
    where
      mapPair (k, v') = (AK.toString k, aesonToHydra v')
  A.Array a -> Json.ValueArray (aesonToHydra <$> V.toList a)
  A.String t -> Json.ValueString $ T.unpack t
  A.Number s -> Json.ValueNumber $ SC.toRealFloat s
  A.Bool b -> Json.ValueBoolean b
  A.Null -> Json.ValueNull

-- | Parse a JSON file using Aeson and convert to Hydra JSON.
-- Pre-processes the content to escape control characters that the Hydra JSON writer
-- doesn't escape (e.g. null bytes in string literals).
parseJsonFile :: FilePath -> IO (Either String Json.Value)
parseJsonFile fp = do
  content <- BS.readFile fp
  let escaped = escapeControlCharsInJson content
  return $ aesonToHydra <$> A.eitherDecode escaped

-- | Escape unescaped control characters (< 0x20) inside JSON string literals.
-- Thin ByteString wrapper around Generated.escapeControlCharsInJson (which operates on [Int]).
escapeControlCharsInJson :: BS.ByteString -> BS.ByteString
escapeControlCharsInJson input =
  BS.pack $ fmap fromIntegral $ Generated.escapeControlCharsInJson $ fmap fromIntegral $ BS.unpack input

-- Original staging code (preserved as reference):
-- escapeControlCharsInJson input = BS.pack $ go False False (BS.unpack input)
--   where
--     go _ _ [] = []
--     go inStr esc (b:bs)
--       | esc = b : go inStr False bs
--       | b == 0x5C && inStr = b : go inStr True bs
--       | b == 0x22 = b : go (not inStr) False bs
--       | inStr && b < 0x20 = escapeToUnicode b ++ go inStr False bs
--       | otherwise = b : go inStr False bs
--     escapeToUnicode b =
--       [0x5C, 0x75, 0x30, 0x30, hexDigit (b `div` 16), hexDigit (b `mod` 16)]
--     hexDigit n
--       | n < 10 = 0x30 + n
--       | otherwise = 0x61 + n - 10

-- | Build a schema map (Name -> Type) from a graph's schema. Delegated to Generated.buildSchemaMap.
buildSchemaMap :: Graph -> M.Map Name Type
buildSchemaMap = Generated.buildSchemaMap

-- Original staging code (preserved as reference):
-- buildSchemaMap g = case graphSchema g of
--   Nothing -> M.empty
--   Just schemaGraph -> M.fromList $ Y.catMaybes $ fmap (extractType schemaGraph) $ graphElements schemaGraph
--   where
--     extractType sg binding = case DecodeCore.type_ sg (bindingTerm binding) of
--       Right typ -> Just (bindingName binding, Rewriting.deannotateType typ)
--       Left _ -> Nothing

-- | Strip TypeSchemes from term bindings in a module, preserving type binding TypeSchemes.
-- Delegated to Generated.stripModuleTypeSchemes.
stripModuleTypeSchemes :: Module -> Module
stripModuleTypeSchemes = Generated.stripModuleTypeSchemes

-- Original staging code (preserved as reference):
-- stripModuleTypeSchemes m = m { moduleElements = fmap stripIfTerm (moduleElements m) }
--   where
--     stripIfTerm b
--       | isNativeType b = b  -- Preserve TypeSchemes on type bindings
--       | otherwise = b { bindingType = Nothing }

-- | Decode a single module from a JSON value. Delegated to Generated.decodeModuleFromJson.
decodeModuleFromJson :: Graph -> [Module] -> Bool -> Json.Value -> Either String Module
decodeModuleFromJson = Generated.decodeModuleFromJson

-- Original staging code (preserved as reference):
-- decodeModuleFromJson bsGraph universeModules doStripTypeSchemes jsonVal =
--   let graph = modulesToGraph bsGraph universeModules universeModules
--       schemaMap = buildSchemaMap graph
--       modType = TypeVariable _Module
--       postProcess = if doStripTypeSchemes then stripModuleTypeSchemes else id
--   in case JsonDecode.fromJson schemaMap modType jsonVal of
--     Left err -> Left err
--     Right term -> case DecodeModule.module_ graph term of
--       Left (DecodingError err) -> Left (unDecodingError (DecodingError err))
--       Right mod -> Right (postProcess mod)

-- | Load modules from JSON files.
-- Takes a base path and a list of namespaces to load.
-- The universeModules are used to build the graph for type resolution.
-- When doStripTypeSchemes is True, TypeSchemes are stripped from term bindings
-- because they may contain stale types (e.g., bigfloat) that cause inference
-- errors after adaptation. The inference engine will reconstruct correct TypeSchemes.
-- When False, TypeSchemes are preserved (useful for ext modules that don't need
-- type adaptation and where stripping would cause inference to loop on recursive types).
loadModulesFromJson :: Bool -> FilePath -> [Module] -> [Namespace] -> IO [Module]
loadModulesFromJson doStripTypeSchemes basePath universeModules namespaces = do
  CM.forM namespaces $ \ns -> do
    let filePath = basePath FP.</> namespaceToPath ns ++ ".json"
    parseResult <- parseJsonFile filePath
    case parseResult of
      Left err -> fail $ "JSON parse error for " ++ unNamespace ns ++ ": " ++ err
      Right jsonVal -> case decodeModuleFromJson bootstrapGraph universeModules doStripTypeSchemes jsonVal of
        Left err -> fail $ "Module decode error for " ++ unNamespace ns ++ ": " ++ err
        Right mod -> do
          putStrLn $ "  Loaded: " ++ unNamespace ns
          return mod

-- | Discover all JSON module files in a directory and load them.
-- Scans the directory tree for .json files, converts paths to namespaces,
-- then loads all discovered modules.
-- TypeSchemes are stripped by default (suitable for main/kernel modules).
loadAllModulesFromJsonDir :: FilePath -> [Module] -> IO [Module]
loadAllModulesFromJsonDir = loadAllModulesFromJsonDirWith True

-- | Like loadAllModulesFromJsonDir but with control over TypeScheme stripping.
loadAllModulesFromJsonDirWith :: Bool -> FilePath -> [Module] -> IO [Module]
loadAllModulesFromJsonDirWith doStripTypeSchemes basePath universeModules = do
  namespaces <- discoverJsonNamespaces basePath
  putStrLn $ "  Discovered " ++ show (length namespaces) ++ " modules in " ++ basePath
  loadModulesFromJson doStripTypeSchemes basePath universeModules namespaces

-- | Discover namespaces from JSON files in a directory tree.
-- Converts file paths like "hydra/core.json" to Namespace "hydra.core".
discoverJsonNamespaces :: FilePath -> IO [Namespace]
discoverJsonNamespaces basePath = do
  exists <- SD.doesDirectoryExist basePath
  if not exists
    then return []
    else do
      files <- findJsonFiles basePath basePath
      return $ L.sort files
  where
    findJsonFiles root dir = do
      entries <- SD.listDirectory dir
      results <- CM.forM entries $ \entry -> do
        let path = dir FP.</> entry
        isDir <- SD.doesDirectoryExist path
        if isDir
          then findJsonFiles root path
          else if FP.takeExtension entry == ".json"
            then do
              let relPath = FP.makeRelative root path
                  ns = Namespace $ L.intercalate "." $ LS.splitOn "/" $ FP.dropExtension relPath
              return [ns]
            else return []
      return $ L.concat results
