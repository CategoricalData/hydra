module Hydra.Sources.Tier4.All(
  module Hydra.Sources.Tier4.All,
  module Hydra.Sources.Tier3.All,
  -- Note: individual Tier-4 modules are not exported, as they are currently not guaranteed to be free of name collisions
) where

import Hydra.Sources.Tier3.All
import Hydra.Sources.Tier4.Ext.Avro.Schema
import Hydra.Sources.Tier4.Ext.Cypher.Features
import Hydra.Sources.Tier4.Ext.Cypher.OpenCypher
import Hydra.Sources.Tier4.Ext.Graphql.Syntax
import Hydra.Sources.Tier4.Ext.Haskell.Ast
import Hydra.Sources.Tier4.Ext.Java.Language
import Hydra.Sources.Tier4.Ext.Java.Syntax
import Hydra.Sources.Tier4.Ext.Json.Decoding
import Hydra.Sources.Tier4.Ext.Pegasus.Pdl
import Hydra.Sources.Tier4.Ext.Protobuf.Any
import Hydra.Sources.Tier4.Ext.Protobuf.Language
import Hydra.Sources.Tier4.Ext.Protobuf.Proto3
import Hydra.Sources.Tier4.Ext.Protobuf.SourceContext
import Hydra.Sources.Tier4.Ext.Rdf.Syntax
import Hydra.Sources.Tier4.Ext.RelationalModel
import Hydra.Sources.Tier4.Ext.Scala.Meta
import Hydra.Sources.Tier4.Ext.Shacl.Model
import Hydra.Sources.Tier4.Ext.Tabular
import Hydra.Sources.Tier4.Ext.Tinkerpop.Features
import Hydra.Sources.Tier4.Ext.Tinkerpop.Gremlin
import Hydra.Sources.Tier4.Ext.Tinkerpop.Mappings
import Hydra.Sources.Tier4.Ext.Tinkerpop.PropertyGraph
import Hydra.Sources.Tier4.Ext.Tinkerpop.Queries
import Hydra.Sources.Tier4.Ext.Tinkerpop.Validate
import Hydra.Sources.Tier4.Ext.Yaml.Model
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
  graphqlSyntaxModule,
  gremlinModule,
  haskellAstModule,
  javaLanguageModule,
  javaSyntaxModule,
  jsonDecodingModule,
  openCypherModule,
  openCypherFeaturesModule,
  pegasusPdlModule,
  proto3Module,
  protobufAnyModule,
  protobufLanguageModule,
  protobufSourceContextModule,
  rdfSyntaxModule,
  relationalModelModule,
  scalaMetaModule,
  shaclModelModule,
  tabularModule,
  tinkerpopFeaturesModule,
  tinkerpopMappingsModule,
  tinkerpopPropertyGraphModule,
  propertyGraphQueriesModule,
  tinkerpopValidateModule,
  yamlModelModule]
