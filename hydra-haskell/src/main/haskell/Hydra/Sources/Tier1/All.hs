module Hydra.Sources.Tier1.All(
  module Hydra.Sources.Tier1.All,
  module Hydra.Sources.Tier0.All,
  module Hydra.Sources.Tier1.Flows,
  module Hydra.Sources.Tier1.Qnames,
  module Hydra.Sources.Tier1.Rewriting,
  module Hydra.Sources.Tier1.Variants,
) where

import Hydra.Sources.Tier0.All
import Hydra.Sources.Tier1.Flows
import Hydra.Sources.Tier1.Qnames
import Hydra.Sources.Tier1.Rewriting
import Hydra.Sources.Tier1.Variants

tier1Modules :: [Module]
tier1Modules = [
  hydraFlowsModule,
  hydraQnamesModule,
  hydraRewritingModule,
  hydraVariantsModule]
