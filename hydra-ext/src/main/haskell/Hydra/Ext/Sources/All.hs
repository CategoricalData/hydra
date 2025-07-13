-- | A collection of all Hydra sources provided in hydra-ext

module Hydra.Ext.Sources.All (
  module Hydra.Ext.Sources.All,
  module Hydra.Sources.All,
) where

import Hydra.Kernel
import Hydra.Sources.All

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
import qualified Hydra.Ext.Sources.Json.Schema as JsonSchema
import qualified Hydra.Ext.Sources.Json.Schema.Language as JsonSchemaLanguage
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
      JsonSchema.module_,
      JsonSchemaLanguage.module_,
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
