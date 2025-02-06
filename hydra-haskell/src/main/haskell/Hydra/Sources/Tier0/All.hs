
module Hydra.Sources.Tier0.All(
  module Hydra.Sources.Tier0.All,
  module Hydra.Sources.Core,
) where

import Hydra.Sources.Core

tier0Modules :: [Module]
tier0Modules = [hydraCoreModule]
