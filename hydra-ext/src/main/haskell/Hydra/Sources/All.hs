-- | Entry point for "extension" models and coders which are not included in hydra-haskell

module Hydra.Sources.All where -- TODO: move to Hydra.Sources.Ext.All

import Hydra.Kernel
import Hydra.Codegen

import Hydra.Ext.Cpp.Coder
import Hydra.Sources.Ext.Delta.Parquet
import Hydra.Sources.Ext.Gql.OpenGql
import Hydra.Sources.Ext.Gql.PathAlgebra.Expressions
import Hydra.Sources.Ext.Gql.PathAlgebra.Syntax
import Hydra.Ext.Graphql.Coder
import Hydra.Sources.Ext.Graphviz.Dot
import Hydra.Ext.Java.Coder
import Hydra.Sources.Ext.Kusto.Kql
import Hydra.Sources.Ext.Other.Atlas
import Hydra.Sources.Ext.Other.AzureDtld
import Hydra.Sources.Ext.Other.Coq
import Hydra.Sources.Ext.Other.Datalog
import Hydra.Sources.Ext.Other.GeoJson
import Hydra.Sources.Ext.Other.IanaRelations
import Hydra.Sources.Ext.Other.Osv
import Hydra.Sources.Ext.Other.StacItems
import Hydra.Sources.Ext.Owl.Syntax
import Hydra.Sources.Ext.Parquet.Format
import Hydra.Ext.Pegasus.Coder
import Hydra.Ext.Protobuf.Coder
import Hydra.Ext.Python.Coder
import Hydra.Ext.Scala.Coder
import Hydra.Sources.Ext.Shex.Syntax
import Hydra.Sources.Ext.Sql.Ansi
import Hydra.Sources.Ext.Tinkerpop.Features
import Hydra.Sources.Ext.Tinkerpop.Gremlin
import Hydra.Sources.Ext.Xml.Schema

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
