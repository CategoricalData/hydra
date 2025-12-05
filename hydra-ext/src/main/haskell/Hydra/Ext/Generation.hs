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
import Hydra.Ext.Staging.Python.Coder
import Hydra.Ext.Staging.Scala.Coder

import qualified System.FilePath as FP


writeCpp :: FP.FilePath -> [Module] -> IO ()
writeCpp = generateSources moduleToCpp cppLanguage False

writeGraphql :: FP.FilePath -> [Module] -> IO ()
writeGraphql = generateSources moduleToGraphql graphqlLanguage False

writeJava :: FP.FilePath -> [Module] -> IO ()
writeJava = generateSources moduleToJava javaLanguage True

writeJsonSchema :: FP.FilePath -> [Module] -> IO ()
writeJsonSchema = generateSources (moduleToJsonSchema (JsonSchemaOptions True)) jsonSchemaLanguage False

writePdl :: FP.FilePath -> [Module] -> IO ()
writePdl = generateSources moduleToPdl pdlLanguage False

writeProtobuf :: FP.FilePath -> [Module] -> IO ()
writeProtobuf = generateSources moduleToProtobuf protobufLanguage False

writePython :: FP.FilePath -> [Module] -> IO ()
writePython = generateSources moduleToPython pythonLanguage True

writeScala :: FP.FilePath -> [Module] -> IO ()
writeScala = generateSources moduleToScala scalaLanguage True
