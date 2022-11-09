module Hydra.Extensions where

import Hydra.All

import Hydra.Models.Atlas
import Hydra.Models.AzureDtld
import Hydra.Models.Coq
import Hydra.Models.Datalog
import Hydra.Models.GeoJson
import Hydra.Models.IanaRelations
import Hydra.Models.Osv
import Hydra.Models.StacItems


hydraExtensionsModules :: [Module Meta]
hydraExtensionsModules = [
  atlasModelModule,
  coqSyntaxModule,
  datalogSyntaxModule,
  dtldModule,
  geoJsonModule,
  ianaRelationsModule,
  osvSchemaModule,
  stacModule]
