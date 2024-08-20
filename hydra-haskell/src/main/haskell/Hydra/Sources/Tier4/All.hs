module Hydra.Sources.Tier4.All(
  module Hydra.Sources.Tier4.All,
  module Hydra.Sources.Tier3.All,
  -- Note: individual Tier-4 modules are not exported, as they are currently not guaranteed to be free of name collisions
) where

import Hydra.Sources.Tier3.All
import Hydra.Sources.Tier4.Langs.Avro.Schema
import Hydra.Sources.Tier4.Langs.Cypher.Features
import Hydra.Sources.Tier4.Langs.Cypher.OpenCypher
import Hydra.Sources.Tier4.Langs.Delta.Parquet
import Hydra.Sources.Tier4.Langs.Graphql.Syntax
import Hydra.Sources.Tier4.Langs.Haskell.Ast
import Hydra.Sources.Tier4.Langs.Java.Language
import Hydra.Sources.Tier4.Langs.Java.Syntax
import Hydra.Sources.Tier4.Langs.Json.Decoding
import Hydra.Sources.Tier4.Langs.Kusto.Kql
import Hydra.Sources.Tier4.Langs.Owl.Syntax
import Hydra.Sources.Tier4.Langs.Parquet.Format
import Hydra.Sources.Tier4.Langs.Pegasus.Pdl
import Hydra.Sources.Tier4.Langs.Protobuf.Any
import Hydra.Sources.Tier4.Langs.Protobuf.Language
import Hydra.Sources.Tier4.Langs.Protobuf.Proto3
import Hydra.Sources.Tier4.Langs.Protobuf.SourceContext
import Hydra.Sources.Tier4.Langs.Python.Python3
import Hydra.Sources.Tier4.Langs.Rdf.Syntax
import Hydra.Sources.Tier4.Langs.RelationalModel
import Hydra.Sources.Tier4.Langs.Scala.Meta
import Hydra.Sources.Tier4.Langs.Shacl.Model
import Hydra.Sources.Tier4.Langs.Shex.Syntax
import Hydra.Sources.Tier4.Langs.Sql.Ansi
import Hydra.Sources.Tier4.Langs.Tabular
import Hydra.Sources.Tier4.Langs.Tinkerpop.Features
import Hydra.Sources.Tier4.Langs.Tinkerpop.Gremlin
import Hydra.Sources.Tier4.Langs.Tinkerpop.Mappings
import Hydra.Sources.Tier4.Langs.Tinkerpop.PropertyGraph
import Hydra.Sources.Tier4.Langs.Tinkerpop.Queries
import Hydra.Sources.Tier4.Langs.Tinkerpop.Validate
import Hydra.Sources.Tier4.Langs.Xml.Schema
import Hydra.Sources.Tier4.Langs.Yaml.Model
import Hydra.Sources.Tier4.Test.TestSuite


allModules :: [Module]
allModules = mainModules ++ testModules

mainModules :: [Module]
mainModules = kernelModules ++ tier4LangModules

testModules :: [Module]
testModules = [
  testSuiteModule]

tier4LangModules :: [Module]
tier4LangModules = [
  avroSchemaModule,
  deltaParquetModule,
  graphqlSyntaxModule,
  gremlinModule,
  haskellAstModule,
  javaLanguageModule,
  javaSyntaxModule,
  jsonDecodingModule,
  kqlModule,
  openCypherModule,
  openCypherFeaturesModule,
  owlSyntaxModule,
  parquetFormatModule,
  pegasusPdlModule,
  proto3Module,
  protobufAnyModule,
  protobufLanguageModule,
  protobufSourceContextModule,
--  python3Module,
  rdfSyntaxModule,
  relationalModelModule,
  scalaMetaModule,
  shaclModelModule,
  shexSyntaxModule,
  sqlModule,
  tabularModule,
  tinkerpopFeaturesModule,
  tinkerpopMappingsModule,
  tinkerpopPropertyGraphModule,
  propertyGraphQueriesModule,
  tinkerpopValidateModule,
  xmlSchemaModule,
  yamlModelModule]
