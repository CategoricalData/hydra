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
import Hydra.Ext.Staging.Java.Coder
import Hydra.Ext.Staging.Json.Schema.Coder
import Hydra.Ext.Staging.Pegasus.Coder
import Hydra.Ext.Staging.Protobuf.Coder
import Hydra.Ext.Python.Coder (moduleToPython)
import Hydra.Ext.Staging.Scala.Coder

import qualified System.FilePath as FP


-- | Generate C++ source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeCpp :: FP.FilePath -> [Module] -> [Module] -> IO ()
writeCpp = generateSources moduleToCpp cppLanguage False False False

-- | Generate GraphQL source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeGraphql :: FP.FilePath -> [Module] -> [Module] -> IO ()
writeGraphql = generateSources moduleToGraphql graphqlLanguage False False False

-- | Generate Java source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
-- Note: Java uses doHoistPolymorphicLetBindings=True to hoist polymorphic let bindings to class level
writeJava :: FP.FilePath -> [Module] -> [Module] -> IO ()
writeJava = generateSources moduleToJava javaLanguage True False True

-- | Generate JSON Schema files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeJsonSchema :: FP.FilePath -> [Module] -> [Module] -> IO ()
writeJsonSchema = generateSources (moduleToJsonSchema (JsonSchemaOptions True)) jsonSchemaLanguage False False False

-- | Generate PDL (Pegasus) source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writePdl :: FP.FilePath -> [Module] -> [Module] -> IO ()
writePdl = generateSources moduleToPdl pdlLanguage False False False

-- | Generate Protocol Buffers source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeProtobuf :: FP.FilePath -> [Module] -> [Module] -> IO ()
writeProtobuf = generateSources moduleToProtobuf protobufLanguage False False False

-- | Generate Python source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
-- Note: Python uses doHoistCaseStatements=True to hoist case statements to let bindings
writePython :: FP.FilePath -> [Module] -> [Module] -> IO ()
writePython = generateSources moduleToPython pythonLanguage True True False

-- | Generate Scala source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeScala :: FP.FilePath -> [Module] -> [Module] -> IO ()
writeScala = generateSources moduleToScala scalaLanguage True False False
