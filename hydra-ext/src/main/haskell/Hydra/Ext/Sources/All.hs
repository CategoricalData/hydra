-- | A collection of all Hydra sources provided in hydra-ext

module Hydra.Ext.Sources.All (
  module Hydra.Ext.Sources.All,
  module Hydra.Sources.All,
) where

import Hydra.Kernel
import Hydra.Sources.All

import qualified Hydra.Ext.Sources.Cpp.Language               as CppLanguage
import qualified Hydra.Ext.Sources.Csharp.Language             as CsharpLanguage
import qualified Hydra.Ext.Sources.Graphql.Syntax              as GraphqlSyntax
import qualified Hydra.Ext.Sources.Other.Datalog               as Datalog
import qualified Hydra.Ext.Sources.Protobuf.Language           as ProtobufLanguage
import qualified Hydra.Ext.Sources.Python.Language             as PythonLanguage
import qualified Hydra.Ext.Sources.Shex.Syntax                 as ShexSyntax
import qualified Hydra.Ext.Sources.Sql.Ansi                    as SqlAnsi
import qualified Hydra.Ext.Sources.TypeScript.Language          as TypeScriptLanguage
import qualified Hydra.Ext.Sources.Avro.Coder                  as AvroCoder
import qualified Hydra.Ext.Sources.Avro.Encoder                 as AvroEncoder
import qualified Hydra.Ext.Sources.Avro.Environment             as AvroEnvironment
import qualified Hydra.Ext.Sources.Avro.Language               as AvroLanguage
import qualified Hydra.Ext.Sources.Avro.Schema                 as AvroSchema
import qualified Hydra.Ext.Sources.Avro.SchemaJson             as AvroSchemaJson
import qualified Hydra.Ext.Sources.Avro.Testing                as AvroTesting
import qualified Hydra.Ext.Sources.Cpp.Coder                  as CppCoder
import qualified Hydra.Ext.Sources.Cpp.Environment             as CppEnvironment
import qualified Hydra.Ext.Sources.Cpp.Names                  as CppNames
import qualified Hydra.Ext.Sources.Cpp.Serde                  as CppSerde
import qualified Hydra.Ext.Sources.Cpp.Syntax                  as CppSyntax
import qualified Hydra.Ext.Sources.Cpp.Utils                   as CppUtils
import qualified Hydra.Ext.Sources.Csharp.Syntax               as CsharpSyntax
import qualified Hydra.Ext.Sources.Cypher.Features             as CypherFeatures
import qualified Hydra.Ext.Sources.Cypher.OpenCypher           as OpenCypher
import qualified Hydra.Ext.Sources.Delta.Parquet               as DeltaParquet
import qualified Hydra.Ext.Sources.Demos.GenPG.Transform       as GenPGTransform
import qualified Hydra.Ext.Sources.Graphql.Coder               as GraphqlCoder
import qualified Hydra.Ext.Sources.Graphql.Language             as GraphqlLanguage
import qualified Hydra.Ext.Sources.Graphql.Serde               as GraphqlSerde
import qualified Hydra.Ext.Sources.Go.Language                 as GoLanguage
import qualified Hydra.Ext.Sources.Go.Serde                    as GoSerde
import qualified Hydra.Ext.Sources.Go.Syntax                   as GoSyntax
import qualified Hydra.Ext.Sources.Gql.OpenGql                 as OpenGql
import qualified Hydra.Ext.Sources.Gql.PathAlgebra.Expressions as PathAlgebraExpressions
import qualified Hydra.Ext.Sources.Gql.PathAlgebra.Syntax      as PathAlgebraSyntax
import qualified Hydra.Ext.Sources.Graphviz.Coder              as GraphvizCoder
import qualified Hydra.Ext.Sources.Graphviz.Dot                as Dot
import qualified Hydra.Ext.Sources.Graphviz.Serde              as GraphvizSerde
import qualified Hydra.Ext.Sources.Java.Environment            as JavaEnvironment
import qualified Hydra.Ext.Sources.Java.Language               as JavaLanguage
import qualified Hydra.Ext.Sources.Java.Names                  as JavaNames
import qualified Hydra.Ext.Sources.Java.Serde                  as JavaSerde
import qualified Hydra.Ext.Sources.Java.Syntax                 as JavaSyntax
import qualified Hydra.Ext.Sources.Java.Utils                  as JavaUtils
import qualified Hydra.Ext.Sources.Java.Coder                 as JavaCoder
import qualified Hydra.Ext.Sources.Java.Testing              as JavaTesting
import qualified Hydra.Ext.Sources.JavaScript.Language         as JavaScriptLanguage
import qualified Hydra.Ext.Sources.JavaScript.Operators        as JavaScriptOperators
import qualified Hydra.Ext.Sources.JavaScript.Serde            as JavaScriptSerde
import qualified Hydra.Ext.Sources.JavaScript.Syntax           as JavaScriptSyntax
import qualified Hydra.Ext.Sources.Json.Schema                 as JsonSchema
import qualified Hydra.Ext.Sources.Json.Schema.Coder           as JsonSchemaCoder
import qualified Hydra.Ext.Sources.Json.Schema.Language        as JsonSchemaLanguage
import qualified Hydra.Ext.Sources.Json.Schema.Serde           as JsonSchemaSerde
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
import qualified Hydra.Ext.Sources.Pegasus.Coder               as PegasusCoder
import qualified Hydra.Ext.Sources.Pegasus.Language             as PegasusLanguage
import qualified Hydra.Ext.Sources.Pegasus.Pdl                 as Pdl
import qualified Hydra.Ext.Sources.Pegasus.Serde               as PegasusSerde
import qualified Hydra.Ext.Sources.Pg.Coder                    as PgCoder
import qualified Hydra.Ext.Sources.Pg.Rdf.Environment          as PgRdfEnvironment
import qualified Hydra.Ext.Sources.Pg.Rdf.Mappings             as PgRdfMappings
import qualified Hydra.Ext.Sources.Pg.Graphson.Coder           as GraphsonCoder
import qualified Hydra.Ext.Sources.Pg.Graphson.Construct       as GraphsonConstruct
import qualified Hydra.Ext.Sources.Pg.Graphson.Syntax          as GraphsonSyntax
import qualified Hydra.Ext.Sources.Pg.Graphson.Utils           as GraphsonUtils
import qualified Hydra.Ext.Sources.Pg.Mapping                  as PgMapping
import qualified Hydra.Ext.Sources.Pg.Model                    as PgModel
import qualified Hydra.Ext.Sources.Pg.Printing                 as PgPrinting
import qualified Hydra.Ext.Sources.Pg.Query                    as PgQuery
import qualified Hydra.Ext.Sources.Pg.TermsToElements          as PgTermsToElements
import qualified Hydra.Ext.Sources.Pg.Utils                    as PgUtils
import qualified Hydra.Ext.Sources.Error.Pg                    as ErrorPg
import qualified Hydra.Ext.Sources.Validate.Pg                 as ValidatePg
import qualified Hydra.Ext.Sources.Protobuf.Any                as ProtobufAny
import qualified Hydra.Ext.Sources.Protobuf.Proto3             as Proto3
import qualified Hydra.Ext.Sources.Protobuf.Coder              as ProtobufCoder
import qualified Hydra.Ext.Sources.Protobuf.Environment        as ProtobufEnvironment
import qualified Hydra.Ext.Sources.Protobuf.Serde              as ProtobufSerde
import qualified Hydra.Ext.Sources.Protobuf.SourceContext      as ProtobufSourceContext
import qualified Hydra.Ext.Sources.Python.Coder                as PythonCoder
import qualified Hydra.Ext.Sources.Python.Testing            as PythonTesting
import qualified Hydra.Ext.Sources.Python.Environment          as PythonEnvironment
import qualified Hydra.Ext.Sources.Python.Names                as PythonNames
import qualified Hydra.Ext.Sources.Python.Serde                as PythonSerde
import qualified Hydra.Ext.Sources.Python.Syntax               as PythonSyntax
import qualified Hydra.Ext.Sources.Python.Utils                as PythonUtils
import qualified Hydra.Ext.Sources.Lisp.Coder                  as LispCoder
import qualified Hydra.Ext.Sources.Lisp.Language               as LispLanguage
import qualified Hydra.Ext.Sources.Lisp.Serde                  as LispSerde
import qualified Hydra.Ext.Sources.Lisp.Syntax                 as LispSyntax
import qualified Hydra.Ext.Sources.Rdf.Serde                   as RdfSerde
import qualified Hydra.Ext.Sources.Rdf.Syntax                  as RdfSyntax
import qualified Hydra.Ext.Sources.Rdf.Utils                   as RdfUtils
import qualified Hydra.Ext.Sources.Rust.Coder                 as RustCoder
import qualified Hydra.Ext.Sources.Rust.Language               as RustLanguage
import qualified Hydra.Ext.Sources.Rust.Operators              as RustOperators
import qualified Hydra.Ext.Sources.Rust.Serde                  as RustSerde
import qualified Hydra.Ext.Sources.Rust.Syntax                 as RustSyntax
import qualified Hydra.Ext.Sources.Scala.Coder                  as ScalaCoder
import qualified Hydra.Ext.Sources.Scala.Language               as ScalaLanguage
import qualified Hydra.Ext.Sources.Scala.Syntax                  as ScalaSyntax
import qualified Hydra.Ext.Sources.Scala.Serde                 as ScalaSerde
import qualified Hydra.Ext.Sources.Scala.Utils                 as ScalaUtils
import qualified Hydra.Ext.Sources.Shacl.Coder                as ShaclCoder
import qualified Hydra.Ext.Sources.Shacl.Language               as ShaclLanguage
import qualified Hydra.Ext.Sources.Shacl.Model                 as ShaclModel
import qualified Hydra.Ext.Sources.Tinkerpop.Features          as TinkerpopFeatures
import qualified Hydra.Ext.Sources.Tinkerpop.Gremlin           as Gremlin
import qualified Hydra.Ext.Sources.Tinkerpop.Language          as TinkerpopLanguage
import qualified Hydra.Ext.Sources.TypeScript.Model            as TypeScriptModel
import qualified Hydra.Ext.Sources.Xml.Schema                  as XmlSchema
import qualified Hydra.Ext.Sources.Workflow                     as Workflow
import qualified Hydra.Ext.Sources.Yaml.Coder                  as YamlCoder
import qualified Hydra.Ext.Sources.Yaml.Language                as YamlLanguage
import qualified Hydra.Ext.Sources.Yaml.Serde                   as YamlSerde

import qualified Hydra.Sources.Decode.Pg.Mapping               as DecodePgMapping
import qualified Hydra.Sources.Decode.Pg.Model                 as DecodePgModel

import qualified Hydra.Sources.Encode.Pg.Mapping               as EncodePgMapping
import qualified Hydra.Sources.Encode.Pg.Model                 as EncodePgModel

import qualified Data.List as L


-- | Coder modules for the bootstrap-relevant languages: Haskell, Java, Python, and Lisp.
--   Each list includes the coder itself plus its dependencies (helpers, names, serde, syntax, utils, language).
hydraBootstrapCoderModules :: [Module]
hydraBootstrapCoderModules = haskellModules ++ javaModules ++ pythonModules ++ scalaModules ++ lispModules

-- | Essential hydra-ext modules: the Java and Python coder families.
hydraExtEssentialModules :: [Module]
hydraExtEssentialModules = javaModules ++ pythonModules

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
  ++ lispModules
  ++ pgModules
  ++ protobufModules
  ++ pythonModules
  ++ rdfModules
  ++ rustModules
  ++ scalaModules
  ++ typescriptModules
  ++ yamlModules

-- | All modules that should be exported to JSON, including decode/encode modules
--   that are not part of hydraExtModules (since they have their own Haskell gen-main).
hydraExtJsonModules :: [Module]
hydraExtJsonModules = hydraExtModules
  ++ hydraExtDecodingModules
  ++ hydraExtEncodingModules

-- | Ext modules whose generated code is checked into hydra-ext/src/gen-main/ for each
--   target language (Haskell, Java, Python). These are the modules needed by hydra-ext
--   demos and other target-language code. Not to be confused with language coder modules.
hydraExtDemoModules :: [Module]
hydraExtDemoModules = L.nub $ L.concat [pgModules, genpgModules, rdfModules]

-- | Legacy alias.
hydraExtJavaModules :: [Module]
hydraExtJavaModules = hydraExtDemoModules

otherExtModules :: [Module]
otherExtModules = [
  Atlas.module_,
  AvroCoder.module_,
  AvroEncoder.module_,
  AvroEnvironment.module_,
  AvroLanguage.module_,
  AvroSchema.module_,
  AvroSchemaJson.module_,
  AvroTesting.module_,
  AzureDtld.module_,
  Coq.module_,
  Datalog.module_,
  DeltaParquet.module_,
  Dot.module_,
  GeoJson.module_,
  GraphqlLanguage.module_,
  -- GraphqlCoder.module_,
  -- GraphqlSerde.module_,
  GraphqlSyntax.module_,
  GraphvizCoder.module_,
  GraphvizSerde.module_,
  IanaRelations.module_,
  Kql.module_,
  Osv.module_,
  ParquetFormat.module_,
  PegasusLanguage.module_,
  PegasusCoder.module_,
  PegasusSerde.module_,
  Pdl.module_,
  SqlAnsi.module_,
  StacItems.module_,
  Workflow.module_,
  GenPGTransform.module_]

cppModules :: [Module]
cppModules = [
  CppCoder.module_,
  CppEnvironment.module_,
  CppLanguage.module_,
  CppNames.module_,
  CppSerde.module_,
  CppSyntax.module_,
  CppUtils.module_]

csharpModules :: [Module]
csharpModules = [
  CsharpLanguage.module_,
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
  JavaEnvironment.module_,
  JavaLanguage.module_,
  JavaNames.module_,
  JavaSerde.module_,
  JavaSyntax.module_,
  JavaUtils.module_,
  JavaCoder.module_,
  JavaTesting.module_]

javaScriptModules :: [Module]
javaScriptModules = [
  JavaScriptLanguage.module_,
  JavaScriptOperators.module_,
  JavaScriptSerde.module_,
  JavaScriptSyntax.module_]

jsonSchemaModules :: [Module]
jsonSchemaModules = [
  JsonSchema.module_,
  JsonSchemaCoder.module_,
  JsonSchemaLanguage.module_,
  JsonSchemaSerde.module_]

lispModules :: [Module]
lispModules = [
  LispCoder.module_,
  LispLanguage.module_,
  LispSerde.module_,
  LispSyntax.module_]

pgModules :: [Module]
pgModules = [
  CypherFeatures.module_,
  DecodePgMapping.module_,
  DecodePgModel.module_,
  EncodePgMapping.module_,
  EncodePgModel.module_,
  ErrorPg.module_,
  Gremlin.module_,
  OpenCypher.module_,
  PgCoder.module_,
  PgMapping.module_,
  PgModel.module_,
  PgPrinting.module_,
  PgQuery.module_,
  PgRdfEnvironment.module_,
  PgRdfMappings.module_,
  PgTermsToElements.module_,
  PgUtils.module_,
  TinkerpopFeatures.module_,
  TinkerpopLanguage.module_,
  ValidatePg.module_]

protobufModules :: [Module]
protobufModules = [
  Proto3.module_,
  ProtobufAny.module_,
  ProtobufCoder.module_,
  ProtobufEnvironment.module_,
  ProtobufLanguage.module_,
  ProtobufSerde.module_,
  ProtobufSourceContext.module_]

pythonModules :: [Module]
pythonModules = [
  PythonEnvironment.module_,
  PythonLanguage.module_,
  PythonNames.module_,
  PythonSerde.module_,
  PythonSyntax.module_,
  PythonUtils.module_,
  PythonCoder.module_,
  PythonTesting.module_]

rdfModules :: [Module]
rdfModules = [
  OwlSyntax.module_,
  RdfSerde.module_,
  RdfSyntax.module_,
  RdfUtils.module_,
  ShaclCoder.module_,
  ShaclLanguage.module_,
  ShaclModel.module_,
  ShexSyntax.module_,
  XmlSchema.module_]

rustModules :: [Module]
rustModules = [
  RustCoder.module_,
  RustLanguage.module_,
  RustOperators.module_,
  RustSerde.module_,
  RustSyntax.module_]

scalaModules :: [Module]
scalaModules = [
  ScalaCoder.module_,
  ScalaLanguage.module_,
  ScalaSyntax.module_,
  ScalaSerde.module_,
  ScalaUtils.module_]

typescriptModules :: [Module]
typescriptModules = [
  TypeScriptLanguage.module_,
  TypeScriptModel.module_]

yamlModules :: [Module]
yamlModules = [
  YamlLanguage.module_,
  YamlCoder.module_,
  YamlSerde.module_]

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

-- | Modules promoted from staging to Sources DSL (#267) and generating correctly.
hydraExtRecentlyPromotedModules :: [Module]
hydraExtRecentlyPromotedModules = [
  AvroCoder.module_,
  AvroEncoder.module_,
  AvroEnvironment.module_,
  AvroLanguage.module_,
  AvroSchemaJson.module_,
  CppCoder.module_,
  CppEnvironment.module_,
  CppNames.module_,
  CppSerde.module_,
  CppUtils.module_,
  GraphqlLanguage.module_,
  GraphvizCoder.module_,
  GraphvizSerde.module_,
  JsonSchemaCoder.module_,
  JsonSchemaSerde.module_,
  PegasusCoder.module_,
  PegasusLanguage.module_,
  PegasusSerde.module_,
  PgCoder.module_,
  PgPrinting.module_,
  PgTermsToElements.module_,
  PgUtils.module_,
  ProtobufCoder.module_,
  ProtobufEnvironment.module_,
  ProtobufSerde.module_,
  RdfSerde.module_,
  RdfUtils.module_,
  ScalaCoder.module_,
  ScalaSerde.module_,
  ScalaUtils.module_,
  ShaclCoder.module_,
  ShaclLanguage.module_,
  TinkerpopLanguage.module_,
  YamlSerde.module_]

-- All hydra-ext modules for the GenPG demo
genpgModules :: [Module]
genpgModules = graphsonModules ++ pgModules ++ [GenPGTransform.module_]
