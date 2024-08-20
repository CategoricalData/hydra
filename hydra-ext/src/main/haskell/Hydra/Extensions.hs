module Hydra.Extensions where

import Hydra.Kernel

import Hydra.Ext.Delta.Src.Parquet
import Hydra.Ext.Graphviz.Src.Dot

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
  osvSchemaModule,
  stacModule]
