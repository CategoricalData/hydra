-- | Entry point for "extension" models and coders which are not included in hydra-haskell

module Hydra.Sources.All where -- TODO: move to Hydra.Sources.Ext.All

import Hydra.Kernel
import Hydra.Codegen

import Hydra.Ext.Cpp.Coder
import Hydra.Ext.Graphql.Coder
import Hydra.Ext.Java.Coder
import Hydra.Ext.Pegasus.Coder
import Hydra.Ext.Protobuf.Coder
import Hydra.Ext.Python.Coder
import Hydra.Ext.Scala.Coder

import Hydra.Sources.Ext.Avro.Schema
import Hydra.Sources.Ext.Cpp.Language
import Hydra.Sources.Ext.Cpp.Syntax
import Hydra.Sources.Ext.Csharp.Language
import Hydra.Sources.Ext.Csharp.Syntax
import Hydra.Sources.Ext.Cypher.Features
import Hydra.Sources.Ext.Cypher.OpenCypher
import Hydra.Sources.Ext.Delta.Parquet
import Hydra.Sources.Ext.Gql.OpenGql
import Hydra.Sources.Ext.Gql.PathAlgebra.Expressions
import Hydra.Sources.Ext.Gql.PathAlgebra.Syntax
import Hydra.Sources.Ext.Graphql.Syntax
import Hydra.Sources.Ext.Graphviz.Dot
import Hydra.Sources.Ext.Java.Language
import Hydra.Sources.Ext.Java.Syntax
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
import Hydra.Sources.Ext.Pegasus.Pdl
import Hydra.Sources.Ext.Pg.Graphson.Syntax
import Hydra.Sources.Ext.Pg.Mapping
import Hydra.Sources.Ext.Pg.Model
import Hydra.Sources.Ext.Pg.Query
import Hydra.Sources.Ext.Pg.Validation
import Hydra.Sources.Ext.Protobuf.Any
import Hydra.Sources.Ext.Protobuf.Language
import Hydra.Sources.Ext.Protobuf.Proto3
import Hydra.Sources.Ext.Protobuf.SourceContext
import Hydra.Sources.Ext.Python.Language
import Hydra.Sources.Ext.Python.Syntax
import Hydra.Sources.Ext.Rdf.Syntax
import Hydra.Sources.Ext.Scala.Meta
import Hydra.Sources.Ext.Shacl.Model
import Hydra.Sources.Ext.Shex.Syntax
import Hydra.Sources.Ext.Sql.Ansi
import Hydra.Sources.Ext.Tinkerpop.Features
import Hydra.Sources.Ext.Tinkerpop.Gremlin
import Hydra.Sources.Ext.TypeScript.Language
import Hydra.Sources.Ext.TypeScript.Model
import Hydra.Sources.Ext.Xml.Schema

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
