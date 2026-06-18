-- | Haskell implementations of hydra.lib.effects primitives

module Hydra.Haskell.Lib.Effects where

import Prelude hiding (fail, map, pure)
import qualified Prelude as P


-- | Sequence two effectful computations.
bind :: IO a -> (a -> IO b) -> IO b
bind = (P.>>=)

-- | Create an effectful computation which fails with a message.
fail :: String -> IO a
fail = P.ioError . P.userError

-- | Map a pure function over the result of an effect.
map :: (a -> b) -> IO a -> IO b
map = P.fmap

-- | Lift a pure value into an effect.
pure :: a -> IO a
pure = P.pure
