-- | Entry point for "extension" models and coders which are not included in hydra-haskell

module Hydra.Extensions where

import Hydra.Kernel
import Hydra.Codegen

import Hydra.Ext.Cpp.Coder
import Hydra.Ext.Delta.Src.Parquet
import Hydra.Ext.Gql.OpenGql
import Hydra.Ext.Gql.PathAlgebra.Expressions
import Hydra.Ext.Gql.PathAlgebra.Syntax
import Hydra.Ext.Graphql.Coder
import Hydra.Ext.Graphviz.Src.Dot
import Hydra.Ext.Java.Coder
import Hydra.Ext.Kusto.Src.Kql
import Hydra.Ext.Other.Atlas
import Hydra.Ext.Other.AzureDtld
import Hydra.Ext.Other.Coq
import Hydra.Ext.Other.Datalog
import Hydra.Ext.Other.GeoJson
import Hydra.Ext.Other.IanaRelations
import Hydra.Ext.Other.Osv
import Hydra.Ext.Other.StacItems
import Hydra.Ext.Owl.Src.Syntax
import Hydra.Ext.Parquet.Src.Format
import Hydra.Ext.Pegasus.Coder
import Hydra.Ext.Protobuf.Coder
import Hydra.Ext.Python.Coder
import Hydra.Ext.Scala.Coder
import Hydra.Ext.Shex.Src.Syntax
import Hydra.Ext.Sql.Src.Ansi
import Hydra.Ext.Tinkerpop.Src.Features
import Hydra.Ext.Tinkerpop.Src.Gremlin
import Hydra.Ext.Xml.Src.Schema

import qualified Control.Monad as CM
import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Directory as SD
import qualified Data.Maybe as Y


hydraExtModules :: [Module]
hydraExtModules = [
  atlasModelModule,
  coqSyntaxModule,
  datalogSyntaxModule,
  deltaParquetModule,
  dotModule,
  dtldModule,
  geoJsonModule,
  gremlinModule,
  ianaRelationsModule,
  kqlModule,
  openGqlModule,
  osvSchemaModule,
  owlSyntaxModule,
  parquetFormatModule,
  shexSyntaxModule,
  sqlModule,
  stacModule,
  tinkerpopFeaturesModule,
  xmlSchemaModule] ++ gqlModules

gqlModules = [
  pathAlgebraExpressionsModule,
  pathAlgebraSyntaxModule]

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
