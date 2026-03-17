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
import Hydra.Ext.Pegasus.Language (pdlLanguage)
import Hydra.Ext.Scala.Language (scalaLanguage)
import Hydra.Ext.Cpp.Coder (moduleToCpp)
import Hydra.Ext.Graphql.Coder (moduleToGraphql)
import Hydra.Ext.Graphql.Language (graphqlLanguage)
import Hydra.Ext.Java.Coder (moduleToJava)
import Hydra.Ext.Json.Schema.Coder
import Hydra.Ext.Pegasus.Coder (moduleToPdl)
import Hydra.Ext.Protobuf.Coder (moduleToProtobuf)
import Hydra.Ext.Python.Coder (moduleToPython)
import Hydra.Ext.Rust.Coder (moduleToRust)
import Hydra.Ext.Rust.Language (rustLanguage)
import Hydra.Ext.Scala.Coder (moduleToScala)


import qualified Hydra.Json.Model as Json
import qualified Hydra.Json.Writer as JsonWriter

import qualified Data.Map as M
import qualified System.FilePath as FP


-- | Options for JSON Schema code generation (was previously in Staging module)
data JsonSchemaOptions = JsonSchemaOptions {
  jsonSchemaOptionsShortNames :: Bool
}

-- | Write a manifest.json listing ext module namespaces.
-- This mirrors writeManifestJson in Hydra.Generation but for hydra-ext module lists.
writeExtManifestJson :: FilePath -> IO ()
writeExtManifestJson basePath = do
    let jsonVal = Json.ValueObject $ M.fromList [
            ("hydraBootstrapCoderModules", namespacesJson hydraBootstrapCoderModules),
            ("hydraExtDemoModules", namespacesJson hydraExtDemoModules),
            ("hydraExtEssentialModules", namespacesJson hydraExtEssentialModules),
            ("hydraExtJavaModules", namespacesJson hydraExtJavaModules),  -- legacy alias
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
