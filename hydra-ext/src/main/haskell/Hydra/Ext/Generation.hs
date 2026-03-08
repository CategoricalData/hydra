-- | Entry point for code generation in hydra-ext; provides additional sources and coders not found in hydra-haskell.

module Hydra.Ext.Generation (
  module Hydra.Ext.Generation,
  module Hydra.Ext.Sources.All,
  module Hydra.Generation,
  module Hydra.Sources.All,
) where

import Hydra.Kernel
import Hydra.Generation
import Hydra.Ext.Sources.All
import Hydra.Sources.All

import Hydra.Ext.Cpp.Language (cppLanguage)
import Hydra.Ext.Java.Language
import Hydra.Ext.Org.Json.Schema.Language (jsonSchemaLanguage)
import Hydra.Ext.Protobuf.Language (protobufLanguage)
import Hydra.Ext.Python.Language
import Hydra.Ext.Staging.Pegasus.Language (pdlLanguage)
import Hydra.Ext.Staging.Scala.Language (scalaLanguage)
import Hydra.Ext.Staging.Cpp.Coder
import Hydra.Ext.Staging.Graphql.Coder
import Hydra.Ext.Staging.Graphql.Language (graphqlLanguage)
import Hydra.Ext.Java.Coder (moduleToJava)
import Hydra.Ext.Staging.Json.Schema.Coder
import Hydra.Ext.Staging.Pegasus.Coder
import Hydra.Ext.Staging.Protobuf.Coder
import Hydra.Ext.Python.Coder (moduleToPython)
import Hydra.Ext.Rust.Coder (moduleToRust)
import Hydra.Ext.Rust.Language (rustLanguage)
import Hydra.Ext.Staging.Scala.Coder
import Hydra.Ext.Staging.Yaml.Modules
import Hydra.Ext.Staging.Yaml.Language

import qualified Hydra.Json.Model as Json
import qualified Hydra.Json.Writer as JsonWriter

import qualified Hydra.Context as Context
import qualified Hydra.Error as Error
import qualified Hydra.Monads as Monads

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified System.Directory as SD
import qualified System.FilePath as FP


-- | Write a manifest.json listing ext module namespaces.
-- This mirrors writeManifestJson in Hydra.Generation but for hydra-ext module lists.
writeExtManifestJson :: FilePath -> IO ()
writeExtManifestJson basePath = do
    let jsonVal = Json.ValueObject $ M.fromList [
            ("hydraCoderModules", namespacesJson hydraCoderModules),
            ("hydraExtJavaModules", namespacesJson hydraExtJavaModules),
            ("hydraExtModules", namespacesJson hydraExtJsonModules)]
        jsonStr = JsonWriter.printJson jsonVal
        filePath = basePath FP.</> "manifest.json"
    writeFile filePath (jsonStr ++ "\n")
    putStrLn $ "Wrote manifest: " ++ filePath
  where
    namespacesJson mods = Json.ValueArray $ fmap (Json.ValueString . unNamespace . moduleNamespace) mods

-- | Generate C++ source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeCpp :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeCpp = generateSources moduleToCpp cppLanguage True False False False

-- | Generate GraphQL source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeGraphql :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeGraphql = generateSources moduleToGraphql graphqlLanguage True False False False

-- | Generate Java source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
-- Note: Java uses doHoistPolymorphicLetBindings=True to hoist polymorphic let bindings to class level
writeJava :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeJava = generateSources moduleToJava javaLanguage True True False True

-- | Generate JSON Schema files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeJsonSchema :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeJsonSchema = generateSources (moduleToJsonSchema (JsonSchemaOptions True)) jsonSchemaLanguage True False False False

-- | Generate PDL (Pegasus) source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writePdl :: FP.FilePath -> [Module] -> [Module] -> IO Int
writePdl = generateSources moduleToPdl pdlLanguage True False False False

-- | Generate Protocol Buffers source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeProtobuf :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeProtobuf = generateSources moduleToProtobuf protobufLanguage True False False False

-- | Generate Python source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
-- Note: Python uses doHoistCaseStatements=True to hoist case statements to let bindings
writePython :: FP.FilePath -> [Module] -> [Module] -> IO Int
writePython = generateSources moduleToPython pythonLanguage True True True False

-- | Generate Rust source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeRust :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeRust = generateSources moduleToRust rustLanguage True False False False

-- | Generate Scala source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeScala :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeScala = generateSources moduleToScala scalaLanguage True True False False

-- | YAML generation - only processes data modules (term definitions), skips schema modules
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeYaml :: FP.FilePath -> [Module] -> [Module] -> IO ()
writeYaml basePath universeModules modulesToGenerate =
    case generateFiles modulesToGenerate of
      Left ic -> fail $ "Failed to generate YAML files: " ++ formatError ic
      Right files -> mapM_ writePair files
  where
    cx = Monads.emptyContext
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

    generateFiles mods =
        let dataModules = L.filter (not . hasNativeTypes) mods
        in if L.null dataModules
          then Right []
          else do
            let g0 = modulesToGraph completeUniverse completeUniverse
                namespaces = fmap moduleNamespace dataModules
                dataElements = L.filter (not . isNativeType) $ L.concatMap moduleElements completeUniverse
            -- Infer types on the data graph before adaptation (eta expansion requires types)
            ((g0', _inferredBindings), _cx1) <- inferGraphTypes cx dataElements g0
            (g1, defLists) <- wrapStringError $ dataGraphToDefinitions constraints True True False False dataElements g0' namespaces cx
            L.concat . fmap M.toList <$> CM.zipWithM (forEachModule g1) dataModules defLists

    forEachModule g1 mod defs = moduleToYaml mod (fmap DefinitionTerm defs) cx g1

    wrapStringError :: Either String a -> Either (Context.InContext Error.OtherError) a
    wrapStringError (Left err) = Left $ Context.InContext (Error.OtherError err) cx
    wrapStringError (Right a) = Right a

    writePair (path, contents) = do
      let fullPath = basePath FP.</> path
      SD.createDirectoryIfMissing True $ FP.takeDirectory fullPath
      writeFile fullPath contents
