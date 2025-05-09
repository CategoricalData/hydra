module Hydra.Extensions where

import Hydra.Kernel

import Hydra.Ext.Delta.Src.Parquet
import Hydra.Ext.Graphviz.Src.Dot
import Hydra.Ext.Kusto.Src.Kql
import Hydra.Ext.Owl.Src.Syntax
import Hydra.Ext.Parquet.Src.Format
import Hydra.Ext.Shex.Src.Syntax
import Hydra.Ext.Sql.Src.Ansi
import Hydra.Ext.Tinkerpop.Src.Features
import Hydra.Ext.Tinkerpop.Src.Gremlin
import Hydra.Ext.Xml.Src.Schema

import Hydra.Ext.Other.Atlas
import Hydra.Ext.Other.AzureDtld
import Hydra.Ext.Other.Coq
import Hydra.Ext.Other.Datalog
import Hydra.Ext.Other.GeoJson
import Hydra.Ext.Other.IanaRelations
import Hydra.Ext.Other.Osv
import Hydra.Ext.Other.PathAlgebra
import Hydra.Ext.Other.StacItems


hydraExtModules :: [Module]
hydraExtModules = [
  atlasModelModule,
  coqSyntaxModule,
  datalogSyntaxModule,
  deltaParquetModule,
  dotModule,
  dtldModule,
  geoJsonModule,
  gremlinModule,
  ianaRelationsModule,
  kqlModule,
  osvSchemaModule,
  owlSyntaxModule,
  parquetFormatModule,
  pathAlgebraModule,
  shexSyntaxModule,
  sqlModule,
  stacModule,
  tinkerpopFeaturesModule,
  xmlSchemaModule]
