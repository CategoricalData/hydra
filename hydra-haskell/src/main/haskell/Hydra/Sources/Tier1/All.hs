module Hydra.Sources.Tier1.All(
  module Hydra.Sources.Tier1.All,
  module Hydra.Sources.Tier0.All,
  module Hydra.Sources.Tier1.Qnames,
  module Hydra.Sources.Tier1.Tier1,
) where

import Hydra.Sources.Tier0.All
import Hydra.Sources.Tier1.Tier1
import Hydra.Sources.Tier1.Qnames

tier1Modules :: [Module]
tier1Modules = [
  hydraQnamesModule,
  hydraTier1Module]
