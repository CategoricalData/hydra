-- | Entry point for code generation in hydra-ext; provides additional sources and coders not found in hydra-haskell.

module Hydra.Ext.Generation (
  module Hydra.Ext.Generation,
  module Hydra.Ext.Sources.All,
) where

import Hydra.Kernel
import Hydra.Generation
import Hydra.Ext.Sources.All

import Hydra.Ext.Staging.Cpp.Coder
import Hydra.Ext.Staging.Graphql.Coder
import Hydra.Ext.Staging.Java.Coder
import Hydra.Ext.Staging.Pegasus.Coder
import Hydra.Ext.Staging.Protobuf.Coder
import Hydra.Ext.Staging.Python.Coder
import Hydra.Ext.Staging.Scala.Coder

import qualified System.FilePath as FP


writeCpp :: FP.FilePath -> [Module] -> IO ()
writeCpp = generateSources moduleToCpp

writeGraphql :: FP.FilePath -> [Module] -> IO ()
writeGraphql = generateSources moduleToGraphql

writeJava :: FP.FilePath -> [Module] -> IO ()
writeJava = generateSources moduleToJava

writePdl :: FP.FilePath -> [Module] -> IO ()
writePdl = generateSources moduleToPdl

writeProtobuf :: FP.FilePath -> [Module] -> IO ()
writeProtobuf = generateSources moduleToProtobuf

writePython :: FP.FilePath -> [Module] -> IO ()
writePython = generateSources moduleToPython

writeScala :: FP.FilePath -> [Module] -> IO ()
writeScala = generateSources moduleToScala
