module Hydra.TestUtils (
  isFailure,
  module Hydra.TestGraph,
  module Hydra.Prototyping.Steps,
) where

-- Do not remove.  
import Hydra.ArbitraryCore

import Hydra.TestGraph
import Hydra.Prototyping.Steps


isFailure :: Result a -> Bool
isFailure r = case r of
  ResultFailure _ -> True
  _ -> False
