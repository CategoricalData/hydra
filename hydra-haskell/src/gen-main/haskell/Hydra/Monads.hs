-- Note: this is an automatically generated file. Do not edit.

-- | Miscellaneous helper functions.

module Hydra.Monads where

import qualified Hydra.Context as Context
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | An empty context with no trace, messages, or other data
emptyContext :: Context.Context
emptyContext = Context.Context {
  Context.contextTrace = [],
  Context.contextMessages = [],
  Context.contextOther = Maps.empty}

-- | Converts an optional value either to an empty list (if nothing) or a singleton list (if just).
maybeToList :: (Maybe t0 -> [t0])
maybeToList mx = (Maybes.maybe [] Lists.pure mx)
