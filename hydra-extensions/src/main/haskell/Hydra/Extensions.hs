module Hydra.Extensions where

import Hydra.All

import Hydra.Models.Atlas
import Hydra.Models.AzureDtld
import Hydra.Models.GeoJson
import Hydra.Models.IanaRelations
import Hydra.Models.Osv
import Hydra.Models.StacItems


hydraExtensionsModules :: [Module Meta]
hydraExtensionsModules = [
  atlasModelModule,
  dtldModule,
  geoJsonModule,
  ianaRelationsModule,
  osvSchemaModule,
  stacModule]
