-- | A collection of all Hydra sources provided in hydra-ext

module Hydra.Ext.Sources.All (
  module Hydra.Ext.Sources.All,
  module Hydra.Sources.All,
) where

import Hydra.Kernel
import Hydra.Sources.All

import Hydra.Ext.Sources.Cpp.Language
import Hydra.Ext.Sources.Csharp.Language
import Hydra.Ext.Sources.Graphql.Syntax
import Hydra.Ext.Sources.Other.Datalog
import Hydra.Ext.Sources.Protobuf.Language
import Hydra.Ext.Sources.Python.Language
import Hydra.Ext.Sources.Shex.Syntax
import Hydra.Ext.Sources.Sql.Ansi
import Hydra.Ext.Sources.TypeScript.Language
import qualified Hydra.Ext.Sources.Avro.Schema                 as AvroSchema
import qualified Hydra.Ext.Sources.Cpp.Syntax                  as CppSyntax
import qualified Hydra.Ext.Sources.Csharp.Syntax               as CsharpSyntax
import qualified Hydra.Ext.Sources.Cypher.Features             as CypherFeatures
import qualified Hydra.Ext.Sources.Cypher.OpenCypher           as OpenCypher
import qualified Hydra.Ext.Sources.Delta.Parquet               as DeltaParquet
import qualified Hydra.Ext.Sources.Demos.GenPG.Transform       as GenPGTransform
import qualified Hydra.Ext.Sources.Go.Language                 as GoLanguage
import qualified Hydra.Ext.Sources.Go.Serde                    as GoSerde
import qualified Hydra.Ext.Sources.Go.Syntax                   as GoSyntax
import qualified Hydra.Ext.Sources.Gql.OpenGql                 as OpenGql
import qualified Hydra.Ext.Sources.Gql.PathAlgebra.Expressions as PathAlgebraExpressions
import qualified Hydra.Ext.Sources.Gql.PathAlgebra.Syntax      as PathAlgebraSyntax
import qualified Hydra.Ext.Sources.Graphviz.Dot                as Dot
import qualified Hydra.Ext.Sources.Java.Helpers                as JavaHelpers
import qualified Hydra.Ext.Sources.Java.Language               as JavaLanguage
import qualified Hydra.Ext.Sources.Java.Names                  as JavaNames
import qualified Hydra.Ext.Sources.Java.Serde                  as JavaSerde
import qualified Hydra.Ext.Sources.Java.Syntax                 as JavaSyntax
import qualified Hydra.Ext.Sources.Java.Utils                  as JavaUtils
import qualified Hydra.Ext.Sources.Java.Coder                 as JavaCoder
import qualified Hydra.Ext.Sources.JavaScript.Language         as JavaScriptLanguage
import qualified Hydra.Ext.Sources.JavaScript.Operators        as JavaScriptOperators
import qualified Hydra.Ext.Sources.JavaScript.Serde            as JavaScriptSerde
import qualified Hydra.Ext.Sources.JavaScript.Syntax           as JavaScriptSyntax
import qualified Hydra.Ext.Sources.Json.Schema                 as JsonSchema
import qualified Hydra.Ext.Sources.Json.Schema.Language        as JsonSchemaLanguage
import qualified Hydra.Ext.Sources.Kusto.Kql                   as Kql
import qualified Hydra.Ext.Sources.Other.Atlas                 as Atlas
import qualified Hydra.Ext.Sources.Other.AzureDtld             as AzureDtld
import qualified Hydra.Ext.Sources.Other.Coq                   as Coq
import qualified Hydra.Ext.Sources.Other.GeoJson               as GeoJson
import qualified Hydra.Ext.Sources.Other.IanaRelations         as IanaRelations
import qualified Hydra.Ext.Sources.Other.Osv                   as Osv
import qualified Hydra.Ext.Sources.Other.StacItems             as StacItems
import qualified Hydra.Ext.Sources.Owl.Syntax                  as OwlSyntax
import qualified Hydra.Ext.Sources.Parquet.Format              as ParquetFormat
import qualified Hydra.Ext.Sources.Pegasus.Pdl                 as Pdl
import qualified Hydra.Ext.Sources.Pg.Graphson.Coder           as GraphsonCoder
import qualified Hydra.Ext.Sources.Pg.Graphson.Construct       as GraphsonConstruct
import qualified Hydra.Ext.Sources.Pg.Graphson.Syntax          as GraphsonSyntax
import qualified Hydra.Ext.Sources.Pg.Graphson.Utils           as GraphsonUtils
import qualified Hydra.Ext.Sources.Pg.Mapping                  as PgMapping
import qualified Hydra.Ext.Sources.Pg.Model                    as PgModel
import qualified Hydra.Ext.Sources.Pg.Query                    as PgQuery
import qualified Hydra.Ext.Sources.Pg.Validation               as PgValidation
import qualified Hydra.Ext.Sources.Protobuf.Any                as ProtobufAny
import qualified Hydra.Ext.Sources.Protobuf.Proto3             as Proto3
import qualified Hydra.Ext.Sources.Protobuf.SourceContext      as ProtobufSourceContext
import qualified Hydra.Ext.Sources.Python.Coder                as PythonCoder
import qualified Hydra.Ext.Sources.Python.Helpers              as PythonHelpers
import qualified Hydra.Ext.Sources.Python.Names                as PythonNames
import qualified Hydra.Ext.Sources.Python.Serde                as PythonSerde
import qualified Hydra.Ext.Sources.Python.Syntax               as PythonSyntax
import qualified Hydra.Ext.Sources.Python.Utils                as PythonUtils
import qualified Hydra.Ext.Sources.Rdf.Syntax                  as RdfSyntax
import qualified Hydra.Ext.Sources.Rust.Language               as RustLanguage
import qualified Hydra.Ext.Sources.Rust.Operators              as RustOperators
import qualified Hydra.Ext.Sources.Rust.Serde                  as RustSerde
import qualified Hydra.Ext.Sources.Rust.Syntax                 as RustSyntax
import qualified Hydra.Ext.Sources.Scala.Meta                  as ScalaMeta
import qualified Hydra.Ext.Sources.Shacl.Model                 as ShaclModel
import qualified Hydra.Ext.Sources.Tinkerpop.Features          as TinkerpopFeatures
import qualified Hydra.Ext.Sources.Tinkerpop.Gremlin           as Gremlin
import qualified Hydra.Ext.Sources.TypeScript.Model            as TypeScriptModel
import qualified Hydra.Ext.Sources.Xml.Schema                  as XmlSchema

import qualified Hydra.Sources.Decode.Pg.Mapping               as DecodePgMapping
import qualified Hydra.Sources.Decode.Pg.Model                 as DecodePgModel

import qualified Hydra.Sources.Encode.Pg.Mapping               as EncodePgMapping
import qualified Hydra.Sources.Encode.Pg.Model                 as EncodePgModel


hydraExtModules :: [Module]
hydraExtModules = otherExtModules
  ++ cppModules
  ++ csharpModules
  ++ goModules
  ++ gqlModules
  ++ graphsonModules
  ++ javaModules
  ++ javaScriptModules
  ++ jsonSchemaModules
  ++ pgModules
  ++ protobufModules
  ++ pythonModules
  ++ rdfModules
  ++ rustModules
  ++ typescriptModules

otherExtModules :: [Module]
otherExtModules = [
  Atlas.module_,
  AvroSchema.module_,
  AzureDtld.module_,
  Coq.module_,
  datalogSyntaxModule,
  DeltaParquet.module_,
  Dot.module_,
  GeoJson.module_,
  graphqlSyntaxModule,
  IanaRelations.module_,
  Kql.module_,
  Osv.module_,
  ParquetFormat.module_,
  Pdl.module_,
  ScalaMeta.module_,
  sqlModule,
  StacItems.module_,
  XmlSchema.module_,
  GenPGTransform.module_]

cppModules :: [Module]
cppModules = [
  cppLanguageModule,
  CppSyntax.module_]

csharpModules :: [Module]
csharpModules = [
  csharpLanguageModule,
  CsharpSyntax.module_]

goModules :: [Module]
goModules = [
  GoLanguage.module_,
  -- GoSerde.module_, -- WIP, incomplete module
  GoSyntax.module_]

gqlModules = [
  OpenGql.module_,
  PathAlgebraExpressions.module_,
  PathAlgebraSyntax.module_]

graphsonModules :: [Module]
graphsonModules = [
  GraphsonCoder.module_,
  GraphsonConstruct.module_,
  GraphsonSyntax.module_,
  GraphsonUtils.module_]

javaModules :: [Module]
javaModules = [
  JavaHelpers.module_,
  JavaLanguage.module_,
  JavaNames.module_,
  JavaSerde.module_,
  JavaSyntax.module_,
  JavaUtils.module_,
  JavaCoder.module_]

javaScriptModules :: [Module]
javaScriptModules = [
  JavaScriptLanguage.module_,
  JavaScriptOperators.module_,
  JavaScriptSerde.module_,
  JavaScriptSyntax.module_]

jsonSchemaModules :: [Module]
jsonSchemaModules = [
  JsonSchema.module_,
  JsonSchemaLanguage.module_]

pgModules :: [Module]
pgModules = [
  CypherFeatures.module_,
  Gremlin.module_,
  OpenCypher.module_,
  PgMapping.module_,
  PgModel.module_,
  PgQuery.module_,
  PgValidation.module_,
  TinkerpopFeatures.module_]

protobufModules :: [Module]
protobufModules = [
  Proto3.module_,
  ProtobufAny.module_,
  protobufLanguageModule,
  ProtobufSourceContext.module_]

pythonModules :: [Module]
pythonModules = [
  PythonHelpers.module_,
  pythonLanguageModule,
  PythonNames.module_,
  PythonSerde.module_,
  PythonSyntax.module_,
  PythonUtils.module_,
  PythonCoder.module_]

rdfModules :: [Module]
rdfModules = [
  OwlSyntax.module_,
  RdfSyntax.module_,
  ShaclModel.module_,
  shexSyntaxModule]

rustModules :: [Module]
rustModules = [
  RustLanguage.module_,
  RustOperators.module_,
  RustSerde.module_,
  RustSyntax.module_]

typescriptModules :: [Module]
typescriptModules = [
  typeScriptLanguageModule,
  TypeScriptModel.module_]

{-
  :set +m
  writeDecoderSourceHaskell "src/gen-main/haskell" (kernelModules <> hydraExtModules) [
    Hydra.Ext.Sources.Pg.Mapping.module_,
    Hydra.Ext.Sources.Pg.Model.module_]
  writeEncoderSourceHaskell "src/gen-main/haskell" (kernelModules <> hydraExtModules) [
    Hydra.Ext.Sources.Pg.Mapping.module_,
    Hydra.Ext.Sources.Pg.Model.module_]
-}
hydraExtDecodingModules = [
  DecodePgMapping.module_,
  DecodePgModel.module_]
hydraExtEncodingModules = [
  EncodePgMapping.module_,
  EncodePgModel.module_]

-- All hydra-ext modules for the GenPG demo
genpgModules :: [Module]
genpgModules = [
    GraphsonCoder.module_,
    GraphsonConstruct.module_,
    GraphsonSyntax.module_,
    GraphsonUtils.module_,
    PgMapping.module_,
    PgModel.module_,
    GenPGTransform.module_,
    DecodePgMapping.module_,
    DecodePgModel.module_,
    EncodePgMapping.module_,
    EncodePgModel.module_]
