-- | Package manifest for hydra-ext.
--
-- Owns DSL sources for the "truly-ext" coders and formats: Avro, Protobuf,
-- GraphQL, Pegasus/PDL, Yaml, Atlas, Cpp, Csharp, Json Schema, Rust,
-- and various miscellaneous domain modules. See
-- feature_290_packaging-plan.md, "Sync system redesign / Package manifests".
--
-- Note: Go DSL sources moved out of hydra-ext into the hydra-go package
-- as part of issue #289 (Go head). TypeScript moved out into hydra-typescript
-- as part of issue #126.

module Hydra.Sources.Ext.Manifest (
  mainModules,
  testModules,
  dslTypeModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Avro.Coder as AvroCoder
import qualified Hydra.Sources.Avro.Encoder as AvroEncoder
import qualified Hydra.Sources.Avro.Environment as AvroEnvironment
import qualified Hydra.Sources.Avro.Language as AvroLanguage
import qualified Hydra.Sources.Avro.Schema as AvroSchema
import qualified Hydra.Sources.Avro.SchemaJson as AvroSchemaJson
import qualified Hydra.Sources.Avro.Testing as AvroTesting
import qualified Hydra.Sources.Cpp.Coder as CppCoder
import qualified Hydra.Sources.Cpp.Environment as CppEnvironment
import qualified Hydra.Sources.Cpp.Language as CppLanguageSource
import qualified Hydra.Sources.Cpp.Names as CppNames
import qualified Hydra.Sources.Cpp.Serde as CppSerde
import qualified Hydra.Sources.Cpp.Syntax as CppSyntax
import qualified Hydra.Sources.Cpp.Utils as CppUtils
import qualified Hydra.Sources.Csharp.Language as CsharpLanguage
import qualified Hydra.Sources.Csharp.Syntax as CsharpSyntax
import qualified Hydra.Sources.Delta.Parquet as DeltaParquet
import qualified Hydra.Sources.Graphql.Coder as GraphqlCoder
import qualified Hydra.Sources.Graphql.Language as GraphqlLanguage
import qualified Hydra.Sources.Graphql.Serde as GraphqlSerde
import qualified Hydra.Sources.Graphql.Syntax as GraphqlSyntax
import qualified Hydra.Sources.Json.Schema as JsonSchema
import qualified Hydra.Sources.Json.Schema.Coder as JsonSchemaCoder
import qualified Hydra.Sources.Json.Schema.Language as JsonSchemaLanguage
import qualified Hydra.Sources.Json.Schema.Serde as JsonSchemaSerde
import qualified Hydra.Sources.Kusto.Kql as Kql
import qualified Hydra.Sources.Other.Atlas as Atlas
import qualified Hydra.Sources.Other.AzureDtld as AzureDtld
import qualified Hydra.Sources.Other.Datalog as Datalog
import qualified Hydra.Sources.Other.GeoJson as GeoJson
import qualified Hydra.Sources.Other.IanaRelations as IanaRelations
import qualified Hydra.Sources.Other.Osv as Osv
import qualified Hydra.Sources.Other.StacItems as StacItems
import qualified Hydra.Sources.Parquet.Format as ParquetFormat
import qualified Hydra.Sources.Pegasus.Coder as PegasusCoder
import qualified Hydra.Sources.Pegasus.Language as PegasusLanguageSource
import qualified Hydra.Sources.Pegasus.Pdl as PdlSyntax
import qualified Hydra.Sources.Pegasus.Serde as PegasusSerdeSource
import qualified Hydra.Sources.Protobuf.Any as ProtobufAny
import qualified Hydra.Sources.Protobuf.Coder as ProtobufCoder
import qualified Hydra.Sources.Protobuf.Environment as ProtobufEnvironment
import qualified Hydra.Sources.Protobuf.Language as ProtobufLanguageSource
import qualified Hydra.Sources.Protobuf.Proto3 as Proto3Syntax
import qualified Hydra.Sources.Protobuf.Serde as ProtobufSerdeSource
import qualified Hydra.Sources.Protobuf.SourceContext as ProtobufSourceContext
import qualified Hydra.Sources.Rust.Coder as RustCoder
import qualified Hydra.Sources.Rust.Language as RustLanguageSource
import qualified Hydra.Sources.Rust.Operators as RustOperators
import qualified Hydra.Sources.Rust.Serde as RustSerdeSource
import qualified Hydra.Sources.Rust.Syntax as RustSyntax
import qualified Hydra.Sources.Sql.Ansi as SqlAnsi
import qualified Hydra.Sources.Workflow as Workflow
import qualified Hydra.Sources.Yaml.Coder as YamlCoder
import qualified Hydra.Sources.Yaml.Language as YamlLanguage
import qualified Hydra.Sources.Yaml.Serde as YamlSerde

mainModules :: [Module]
mainModules = [
  Atlas.module_,
  AvroCoder.module_,
  AvroEncoder.module_,
  AvroEnvironment.module_,
  AvroLanguage.module_,
  AvroSchema.module_,
  AvroSchemaJson.module_,
  AvroTesting.module_,
  AzureDtld.module_,
  CppCoder.module_,
  CppEnvironment.module_,
  CppLanguageSource.module_,
  CppNames.module_,
  CppSerde.module_,
  CppSyntax.module_,
  CppUtils.module_,
  CsharpLanguage.module_,
  CsharpSyntax.module_,
  Datalog.module_,
  DeltaParquet.module_,
  GeoJson.module_,
  GraphqlCoder.module_,
  GraphqlLanguage.module_,
  GraphqlSerde.module_,
  GraphqlSyntax.module_,
  IanaRelations.module_,
  JsonSchema.module_,
  JsonSchemaCoder.module_,
  JsonSchemaLanguage.module_,
  JsonSchemaSerde.module_,
  Kql.module_,
  Osv.module_,
  ParquetFormat.module_,
  PdlSyntax.module_,
  PegasusCoder.module_,
  PegasusLanguageSource.module_,
  PegasusSerdeSource.module_,
  Proto3Syntax.module_,
  ProtobufAny.module_,
  ProtobufCoder.module_,
  ProtobufEnvironment.module_,
  ProtobufLanguageSource.module_,
  ProtobufSerdeSource.module_,
  ProtobufSourceContext.module_,
  RustCoder.module_,
  RustLanguageSource.module_,
  RustOperators.module_,
  RustSerdeSource.module_,
  RustSyntax.module_,
  SqlAnsi.module_,
  StacItems.module_,
  Workflow.module_,
  YamlCoder.module_,
  YamlLanguage.module_,
  YamlSerde.module_]

-- | Modules in this package whose type definitions should produce derived
-- DSL wrapper modules. Empty today — many derived `dist/haskell/hydra-ext/
-- .../Hydra/Dsl/*.hs` files are tracked in git from earlier broader
-- generation passes, but none of them are imported by any non-test
-- source. Most hydra-ext modules are experimental schema / syntax
-- models, not yet used in an application or demo. Add entries
-- explicitly per module when downstream consumers materialize.
dslTypeModules :: [Module]
dslTypeModules = []

testModules :: [Module]
testModules = []
