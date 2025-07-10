module Hydra.Sources.Ext.All(
  module Hydra.Sources.Ext.All,
  module Hydra.Sources.Kernel.Terms.All,
  -- Note: individual Tier-3 modules are not exported, as they are currently not guaranteed to be free of name collisions
) where

import Hydra.Kernel
import Hydra.Sources.Kernel.Terms.All
import Hydra.Sources.Ext.Avro.Schema
import Hydra.Sources.Ext.Cpp.Language
import Hydra.Sources.Ext.Cpp.Syntax
import Hydra.Sources.Ext.Csharp.Language
import Hydra.Sources.Ext.Csharp.Syntax
import Hydra.Sources.Ext.Cypher.Features
import Hydra.Sources.Ext.Cypher.OpenCypher
import Hydra.Sources.Ext.Graphql.Syntax
import Hydra.Sources.Ext.Haskell.Ast
import Hydra.Sources.Ext.Haskell.Ast
import Hydra.Sources.Ext.Haskell.Coder
import Hydra.Sources.Ext.Haskell.Language
import Hydra.Sources.Ext.Haskell.Operators
import Hydra.Sources.Ext.Haskell.Serde
import Hydra.Sources.Ext.Haskell.Utils
import Hydra.Sources.Ext.Java.Language
import Hydra.Sources.Ext.Java.Syntax
import Hydra.Sources.Ext.Json.Decoding
import Hydra.Sources.Ext.Json.Schema
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
import Hydra.Sources.Ext.TypeScript.Language
import Hydra.Sources.Ext.TypeScript.Model
import Hydra.Sources.Ext.Yaml.Model


mainModules :: [Module]
mainModules = kernelModules ++ tier3ExtModules

tier3ExtModules :: [Module]
tier3ExtModules = [
  avroSchemaModule,
  cppLanguageModule,
  cppSyntaxModule,
  csharpLanguageModule,
  csharpSyntaxModule,
  graphqlSyntaxModule,
  haskellAstModule,
  haskellCoderModule,
  haskellLanguageModule,
  haskellOperatorsModule,
  haskellSerdeModule,
  haskellUtilsModule,
  javaLanguageModule,
  javaSyntaxModule,
  jsonDecodingModule,
  jsonSchemaModule,
  openCypherModule,
  openCypherFeaturesModule,
  pegasusPdlModule,
  proto3Module,
  protobufAnyModule,
  protobufLanguageModule,
  protobufSourceContextModule,
  pythonLanguageModule,
  pythonSyntaxModule,
  rdfSyntaxModule,
  scalaMetaModule,
  shaclModelModule,
  pgMappingModule,
  pgModelModule,
  pgQueryModule,
  pgValidationModule,
  graphsonSyntaxModule,
  typeScriptLanguageModule,
  typeScriptModelModule,
  yamlModelModule]
