-- | A module for tier-0 constants.

module Hydra.Constants where

import qualified Hydra.Core as Core
import Data.Int
import Data.List
import Data.Map
import Data.Set

ignoredVariable :: String
ignoredVariable = "_"

-- | A placeholder name for row types as they are being constructed
placeholderName :: Core.Name
placeholderName = (Core.Name "Placeholder")

maxTraceDepth :: Int
maxTraceDepth = 50