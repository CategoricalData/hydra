module Hydra.Sources.Tier3.All(
  module Hydra.Sources.Tier3.All,
  module Hydra.Sources.Tier2.All,
  -- Note: individual Tier-3 modules are not exported, as they are currently not guaranteed to be free of name collisions
) where

import Hydra.Sources.Tier2.All
import Hydra.Sources.Tier3.Ext.Avro.Schema
import Hydra.Sources.Tier3.Ext.Csharp.Syntax
import Hydra.Sources.Tier3.Ext.Cypher.Features
import Hydra.Sources.Tier3.Ext.Cypher.OpenCypher
import Hydra.Sources.Tier3.Ext.Gql.OpenGql
import Hydra.Sources.Tier3.Ext.Graphql.Syntax
import Hydra.Sources.Tier3.Ext.Haskell.Ast
import Hydra.Sources.Tier3.Ext.Java.Language
import Hydra.Sources.Tier3.Ext.Java.Syntax
import Hydra.Sources.Tier3.Ext.Json.Decoding
import Hydra.Sources.Tier3.Ext.Json.Schema
import Hydra.Sources.Tier3.Ext.Pegasus.Pdl
import Hydra.Sources.Tier3.Ext.Pg.Graphson.Syntax
import Hydra.Sources.Tier3.Ext.Pg.Mapping
import Hydra.Sources.Tier3.Ext.Pg.Model
import Hydra.Sources.Tier3.Ext.Pg.Query
import Hydra.Sources.Tier3.Ext.Pg.Validation
import Hydra.Sources.Tier3.Ext.Protobuf.Any
import Hydra.Sources.Tier3.Ext.Protobuf.Language
import Hydra.Sources.Tier3.Ext.Protobuf.Proto3
import Hydra.Sources.Tier3.Ext.Protobuf.SourceContext
import Hydra.Sources.Tier3.Ext.Python.Language
import Hydra.Sources.Tier3.Ext.Python.Syntax
import Hydra.Sources.Tier3.Ext.Rdf.Syntax
import Hydra.Sources.Tier3.Ext.RelationalModel
import Hydra.Sources.Tier3.Ext.Scala.Meta
import Hydra.Sources.Tier3.Ext.Shacl.Model
import Hydra.Sources.Tier3.Ext.Tabular
import Hydra.Sources.Tier3.Ext.TypeScript.Model
import Hydra.Sources.Tier3.Ext.Yaml.Model
import Hydra.Sources.Tier3.Test.TestGraph
import Hydra.Sources.Tier3.Test.TestSuite

allModules :: [Module]
allModules = mainModules ++ testModules

mainModules :: [Module]
mainModules = kernelModules ++ tier3ExtModules

testModules :: [Module]
testModules = [
  testGraphModule,
  testSuiteModule]

tier3ExtModules :: [Module]
tier3ExtModules = [
  avroSchemaModule,
  csharpSyntaxModule,
  graphqlSyntaxModule,
  haskellAstModule,
  javaLanguageModule,
  javaSyntaxModule,
  jsonDecodingModule,
  jsonSchemaModule,
  openCypherModule,
  openCypherFeaturesModule,
  openGqlModule,
  pegasusPdlModule,
  proto3Module,
  protobufAnyModule,
  protobufLanguageModule,
  protobufSourceContextModule,
  pythonLanguageModule,
  pythonSyntaxModule,
  rdfSyntaxModule,
  relationalModelModule,
  scalaMetaModule,
  shaclModelModule,
  tabularModule,
  pgMappingModule,
  pgModelModule,
  pgQueryModule,
  pgValidationModule,
  graphsonSyntaxModule,
  typeScriptModelModule,
  yamlModelModule]
