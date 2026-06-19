-- | Haskell implementations of hydra.lib.effects primitives

module Hydra.Haskell.Lib.Effects where

import qualified Control.Monad as CM
import Prelude hiding (foldl, map, pure)
import qualified Prelude as P


-- | Applicative apply for effects.
apply :: IO (a -> b) -> IO a -> IO b
apply = (P.<*>)

-- | Sequence two effectful computations.
bind :: IO a -> (a -> IO b) -> IO b
bind = (P.>>=)

-- | Kleisli composition for effects.
compose :: (a -> IO b) -> (b -> IO c) -> a -> IO c
compose f g = \x -> f x P.>>= g

-- | Left-fold over a list with an effect-returning function.
foldl :: (a -> b -> IO a) -> a -> [b] -> IO a
foldl = CM.foldM

-- | Map a pure function over the result of an effect.
map :: (a -> b) -> IO a -> IO b
map = P.fmap

-- | Map an effect-returning function over a list.
mapList :: (a -> IO b) -> [a] -> IO [b]
mapList = CM.mapM

-- | Map an effect-returning function over an optional.
mapOptional :: (a -> IO b) -> Maybe a -> IO (Maybe b)
mapOptional = CM.mapM

-- | Lift a pure value into an effect.
pure :: a -> IO a
pure = P.pure
