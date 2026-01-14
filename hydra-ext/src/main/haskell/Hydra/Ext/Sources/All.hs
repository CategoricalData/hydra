-- | A collection of all Hydra sources provided in hydra-ext

module Hydra.Ext.Sources.All (
  module Hydra.Ext.Sources.All,
  module Hydra.Sources.All,
) where

import Hydra.Kernel
import Hydra.Sources.All

import qualified Hydra.Ext.Sources.Avro.Schema as AvroSchema
import Hydra.Ext.Sources.Cpp.Language
import qualified Hydra.Ext.Sources.Cpp.Syntax as CppSyntax
import Hydra.Ext.Sources.Csharp.Language
import qualified Hydra.Ext.Sources.Csharp.Syntax as CsharpSyntax
import qualified Hydra.Ext.Sources.Cypher.Features as CypherFeatures
import qualified Hydra.Ext.Sources.Cypher.OpenCypher as OpenCypher
import qualified Hydra.Ext.Sources.Delta.Parquet as DeltaParquet
import qualified Hydra.Ext.Sources.Gql.OpenGql as OpenGql
import qualified Hydra.Ext.Sources.Gql.PathAlgebra.Expressions as PathAlgebraExpressions
import qualified Hydra.Ext.Sources.Gql.PathAlgebra.Syntax as PathAlgebraSyntax
import Hydra.Ext.Sources.Graphql.Syntax
import qualified Hydra.Ext.Sources.Graphviz.Dot as Dot
import Hydra.Ext.Sources.Java.Language
import qualified Hydra.Ext.Sources.Java.Syntax as JavaSyntax
import qualified Hydra.Ext.Sources.Json.Schema as JsonSchema
import qualified Hydra.Ext.Sources.Json.Schema.Language as JsonSchemaLanguage
import qualified Hydra.Ext.Sources.Kusto.Kql as Kql
import qualified Hydra.Ext.Sources.Other.Atlas as Atlas
import qualified Hydra.Ext.Sources.Other.AzureDtld as AzureDtld
import qualified Hydra.Ext.Sources.Other.Coq as Coq
import Hydra.Ext.Sources.Other.Datalog
import qualified Hydra.Ext.Sources.Other.GeoJson as GeoJson
import qualified Hydra.Ext.Sources.Other.IanaRelations as IanaRelations
import qualified Hydra.Ext.Sources.Other.Osv as Osv
import qualified Hydra.Ext.Sources.Other.StacItems as StacItems
import qualified Hydra.Ext.Sources.Owl.Syntax as OwlSyntax
import qualified Hydra.Ext.Sources.Parquet.Format as ParquetFormat
import qualified Hydra.Ext.Sources.Pegasus.Pdl as Pdl
import qualified Hydra.Ext.Sources.Pg.Graphson.Coder as GraphsonCoder
import qualified Hydra.Ext.Sources.Pg.Graphson.Construct as GraphsonConstruct
import qualified Hydra.Ext.Sources.Pg.Graphson.Syntax as GraphsonSyntax
import qualified Hydra.Ext.Sources.Pg.Graphson.Utils as GraphsonUtils
import qualified Hydra.Ext.Sources.Pg.Mapping as PgMapping
import qualified Hydra.Ext.Sources.Pg.Model as PgModel
import qualified Hydra.Ext.Sources.Pg.Query as PgQuery
import Hydra.Ext.Sources.Pg.Validation
import qualified Hydra.Ext.Sources.Protobuf.Any as ProtobufAny
import Hydra.Ext.Sources.Protobuf.Language
import qualified Hydra.Ext.Sources.Protobuf.Proto3 as Proto3
import qualified Hydra.Ext.Sources.Protobuf.SourceContext as ProtobufSourceContext
import Hydra.Ext.Sources.Python.Language
import qualified Hydra.Ext.Sources.Python.Syntax as PythonSyntax
import qualified Hydra.Ext.Sources.Rdf.Syntax as RdfSyntax
import qualified Hydra.Ext.Sources.Scala.Meta as ScalaMeta
import qualified Hydra.Ext.Sources.Shacl.Model as ShaclModel
import Hydra.Ext.Sources.Shex.Syntax
import Hydra.Ext.Sources.Sql.Ansi
import qualified Hydra.Ext.Sources.Tinkerpop.Features as TinkerpopFeatures
import qualified Hydra.Ext.Sources.Tinkerpop.Gremlin as Gremlin
import Hydra.Ext.Sources.TypeScript.Language
import qualified Hydra.Ext.Sources.TypeScript.Model as TypeScriptModel
import qualified Hydra.Ext.Sources.Xml.Schema as XmlSchema


hydraExtModules :: [Module]
hydraExtModules = otherModules ++ gqlModules
  where
    otherModules = [
      Atlas.module_,
      AvroSchema.module_,
      Coq.module_,
      cppLanguageModule,
      CppSyntax.module_,
      csharpLanguageModule,
      CsharpSyntax.module_,
      datalogSyntaxModule,
      DeltaParquet.module_,
      Dot.module_,
      AzureDtld.module_,
      GeoJson.module_,
      graphqlSyntaxModule,
      GraphsonCoder.module_,
      GraphsonConstruct.module_,
      GraphsonSyntax.module_,
      GraphsonUtils.module_,
      Gremlin.module_,
      IanaRelations.module_,
      javaLanguageModule,
      JavaSyntax.module_,
      JsonSchema.module_,
      JsonSchemaLanguage.module_,
      Kql.module_,
      CypherFeatures.module_,
      OpenCypher.module_,
      Osv.module_,
      OwlSyntax.module_,
      ParquetFormat.module_,
      Pdl.module_,
      PgMapping.module_,
      PgModel.module_,
      PgQuery.module_,
      pgValidationModule,
      Proto3.module_,
      ProtobufAny.module_,
      protobufLanguageModule,
      ProtobufSourceContext.module_,
      pythonLanguageModule,
      PythonSyntax.module_,
      RdfSyntax.module_,
      ScalaMeta.module_,
      ShaclModel.module_,
      shexSyntaxModule,
      sqlModule,
      StacItems.module_,
      TinkerpopFeatures.module_,
      typeScriptLanguageModule,
      TypeScriptModel.module_,
      XmlSchema.module_]

gqlModules = [
  OpenGql.module_,
  PathAlgebraExpressions.module_,
  PathAlgebraSyntax.module_]
