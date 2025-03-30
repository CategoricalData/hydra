-- | Various general-purpose helper functions.

module Hydra.Functions where

import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Optionals as Optionals
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

id_ :: (t0 -> t0)
id_ any_ = any_

optionalToList :: (Maybe t0 -> [t0])
optionalToList mx = (Optionals.maybe [] Lists.pure mx)
