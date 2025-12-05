-- | Entry point for code generation in hydra-ext; provides additional sources and coders not found in hydra-haskell.

module Hydra.Ext.Generation (
  module Hydra.Ext.Generation,
  module Hydra.Ext.Sources.All,
  module Hydra.Sources.All,
) where

import Hydra.Kernel
import Hydra.Generation
import Hydra.Ext.Sources.All
import Hydra.Sources.All

import Hydra.Ext.Cpp.Language (cppLanguage)
import Hydra.Ext.Java.Language
import Hydra.Ext.Protobuf.Language (protobufLanguage)
import Hydra.Ext.Python.Language
import Hydra.Ext.Staging.Pegasus.Language (pdlLanguage)
import Hydra.Ext.Staging.Cpp.Coder
import Hydra.Ext.Staging.Graphql.Coder
import Hydra.Ext.Staging.Graphql.Language (graphqlLanguage)
import Hydra.Ext.Staging.Java.Coder
import Hydra.Ext.Staging.Json.Schema.Coder
import Hydra.Ext.Staging.Pegasus.Coder
import Hydra.Ext.Staging.Protobuf.Coder
import Hydra.Ext.Staging.Python.Coder
import Hydra.Ext.Staging.Scala.Coder

import qualified System.FilePath as FP


writeCpp :: FP.FilePath -> [Module] -> IO ()
writeCpp = generateSourcesSimple moduleToCpp cppLanguage False

writeGraphql :: FP.FilePath -> [Module] -> IO ()
writeGraphql = generateSourcesSimple moduleToGraphql graphqlLanguage False

writeJava :: FP.FilePath -> [Module] -> IO ()
writeJava = generateSourcesSimple moduleToJava javaLanguage True

writeJsonSchema :: FP.FilePath -> [Module] -> IO ()
writeJsonSchema = generateSources (moduleToJsonSchemaFiles (JsonSchemaOptions True))

writePdl :: FP.FilePath -> [Module] -> IO ()
writePdl = generateSourcesSimple moduleToPdl pdlLanguage False

writeProtobuf :: FP.FilePath -> [Module] -> IO ()
writeProtobuf = generateSourcesSimple moduleToProtobuf protobufLanguage False

writePython :: FP.FilePath -> [Module] -> IO ()
writePython = generateSourcesSimple moduleToPython pythonLanguage True

writeScala :: FP.FilePath -> [Module] -> IO ()
writeScala = generateSources moduleToScala
