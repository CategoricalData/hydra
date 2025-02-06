-- | Placeholder for utilities dealing with Hydra functions.

module Hydra.Functions where

import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | The identity function
id_ :: (a -> a)
id_ x = x