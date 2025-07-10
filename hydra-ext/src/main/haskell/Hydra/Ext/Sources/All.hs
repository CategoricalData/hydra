-- | Entry point for "extension" models and coders which are not included in hydra-haskell

module Hydra.Ext.Sources.All where

import Hydra.Kernel
import Hydra.Codegen

import Hydra.Ext.Staging.Cpp.Coder
import Hydra.Ext.Staging.Graphql.Coder
import Hydra.Ext.Staging.Java.Coder
import Hydra.Ext.Staging.Pegasus.Coder
import Hydra.Ext.Staging.Protobuf.Coder
import Hydra.Ext.Staging.Python.Coder
import Hydra.Ext.Staging.Scala.Coder

import Hydra.Ext.Sources.Avro.Schema
import Hydra.Ext.Sources.Cpp.Language
import Hydra.Ext.Sources.Cpp.Syntax
import Hydra.Ext.Sources.Csharp.Language
import Hydra.Ext.Sources.Csharp.Syntax
import Hydra.Ext.Sources.Cypher.Features
import Hydra.Ext.Sources.Cypher.OpenCypher
import Hydra.Ext.Sources.Delta.Parquet
import Hydra.Ext.Sources.Gql.OpenGql
import Hydra.Ext.Sources.Gql.PathAlgebra.Expressions
import Hydra.Ext.Sources.Gql.PathAlgebra.Syntax
import Hydra.Ext.Sources.Graphql.Syntax
import Hydra.Ext.Sources.Graphviz.Dot
import Hydra.Ext.Sources.Java.Language
import Hydra.Ext.Sources.Java.Syntax
import Hydra.Ext.Sources.Kusto.Kql
import Hydra.Ext.Sources.Other.Atlas
import Hydra.Ext.Sources.Other.AzureDtld
import Hydra.Ext.Sources.Other.Coq
import Hydra.Ext.Sources.Other.Datalog
import Hydra.Ext.Sources.Other.GeoJson
import Hydra.Ext.Sources.Other.IanaRelations
import Hydra.Ext.Sources.Other.Osv
import Hydra.Ext.Sources.Other.StacItems
import Hydra.Ext.Sources.Owl.Syntax
import Hydra.Ext.Sources.Parquet.Format
import Hydra.Ext.Sources.Pegasus.Pdl
import Hydra.Ext.Sources.Pg.Graphson.Syntax
import Hydra.Ext.Sources.Pg.Mapping
import Hydra.Ext.Sources.Pg.Model
import Hydra.Ext.Sources.Pg.Query
import Hydra.Ext.Sources.Pg.Validation
import Hydra.Ext.Sources.Protobuf.Any
import Hydra.Ext.Sources.Protobuf.Language
import Hydra.Ext.Sources.Protobuf.Proto3
import Hydra.Ext.Sources.Protobuf.SourceContext
import Hydra.Ext.Sources.Python.Language
import Hydra.Ext.Sources.Python.Syntax
import Hydra.Ext.Sources.Rdf.Syntax
import Hydra.Ext.Sources.Scala.Meta
import Hydra.Ext.Sources.Shacl.Model
import Hydra.Ext.Sources.Shex.Syntax
import Hydra.Ext.Sources.Sql.Ansi
import Hydra.Ext.Sources.Tinkerpop.Features
import Hydra.Ext.Sources.Tinkerpop.Gremlin
import Hydra.Ext.Sources.TypeScript.Language
import Hydra.Ext.Sources.TypeScript.Model
import Hydra.Ext.Sources.Xml.Schema

import qualified Control.Monad as CM
import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Directory as SD
import qualified Data.Maybe as Y


hydraExtModules :: [Module]
hydraExtModules = otherModules ++ gqlModules
  where
    otherModules = [
      atlasModelModule,
      avroSchemaModule,
      coqSyntaxModule,
      cppLanguageModule,
      cppSyntaxModule,
      csharpLanguageModule,
      csharpSyntaxModule,
      datalogSyntaxModule,
      deltaParquetModule,
      dotModule,
      dtldModule,
      geoJsonModule,
      graphqlSyntaxModule,
      graphsonSyntaxModule,
      gremlinModule,
      ianaRelationsModule,
      javaLanguageModule,
      javaSyntaxModule,
      kqlModule,
      openCypherFeaturesModule,
      openCypherModule,
      osvSchemaModule,
      owlSyntaxModule,
      parquetFormatModule,
      pegasusPdlModule,
      pgMappingModule,
      pgModelModule,
      pgQueryModule,
      pgValidationModule,
      proto3Module,
      protobufAnyModule,
      protobufLanguageModule,
      protobufSourceContextModule,
      pythonLanguageModule,
      pythonSyntaxModule,
      rdfSyntaxModule,
      scalaMetaModule,
      shaclModelModule,
      shexSyntaxModule,
      sqlModule,
      stacModule,
      tinkerpopFeaturesModule,
      typeScriptLanguageModule,
      typeScriptModelModule,
      xmlSchemaModule]

gqlModules = [
  openGqlModule,
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
