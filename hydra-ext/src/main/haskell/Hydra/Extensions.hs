module Hydra.Extensions where

import Hydra.Kernel

import Hydra.Ext.Delta.Src.Parquet
import Hydra.Ext.Graphviz.Src.Dot
import Hydra.Ext.Kusto.Src.Kql
import Hydra.Ext.Owl.Src.Syntax
import Hydra.Ext.Parquet.Src.Format
import Hydra.Ext.Python.Src.Python3
import Hydra.Ext.Shex.Src.Syntax
import Hydra.Ext.Sql.Src.Ansi
import Hydra.Ext.Xml.Src.Schema

import Hydra.Models.Atlas
import Hydra.Models.AzureDtld
import Hydra.Models.Coq
import Hydra.Models.Datalog
import Hydra.Models.GeoJson
import Hydra.Models.IanaRelations
import Hydra.Models.Osv
import Hydra.Models.StacItems


hydraExtModules :: [Module]
hydraExtModules = [
  atlasModelModule,
  coqSyntaxModule,
  datalogSyntaxModule,
  deltaParquetModule,
  dotModule,
  dtldModule,
  geoJsonModule,
  ianaRelationsModule,
  kqlModule,
  osvSchemaModule,
  owlSyntaxModule,
  parquetFormatModule,
  -- python3Module,
  shexSyntaxModule,
  sqlModule,
  stacModule,
  xmlSchemaModule]
