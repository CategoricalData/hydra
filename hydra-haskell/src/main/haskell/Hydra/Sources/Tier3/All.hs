module Hydra.Sources.Tier3.All(
  module Hydra.Sources.Tier3.All,
  module Hydra.Sources.Tier2.All,
) where

import Hydra.Sources.Tier2.All

kernelModules :: [Module]
kernelModules = [hydraCoreModule] ++ tier1Modules ++ tier2Modules ++ tier3Modules

tier3Modules :: [Module]
tier3Modules = [
  ]
