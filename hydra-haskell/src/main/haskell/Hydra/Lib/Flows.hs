-- | Haskell implementations of hydra.lib.flows primitives. These are simply wrappers around hydra.flows functions.

module Hydra.Lib.Flows where

import Hydra.Compute
import qualified Hydra.Monads as Monads

import qualified Control.Monad as CM

-- Haskell-specific helpers

instance Functor (Flow s) where
  fmap = CM.liftM
instance Applicative (Flow s) where
  pure = Monads.pure
  (<*>) = CM.ap
instance Monad (Flow s) where
  (>>=) = Monads.bind
instance MonadFail (Flow s) where
  fail = Monads.fail

-- Primitive functions

apply :: Flow s (x -> y) -> Flow s x -> Flow s y
apply = (<*>)

bind :: Flow s x -> (x -> Flow s y) -> Flow s y
bind = Monads.bind

fail :: String -> Flow s x
fail = Monads.fail

map :: (x -> y) -> Flow s x -> Flow s y
map = Monads.map

mapList :: (x -> Flow s y) -> [x] -> Flow s [y]
mapList = CM.mapM

pure :: x -> Flow s x
pure = Monads.pure

sequence :: [Flow s x] -> Flow s [x]
sequence = CM.sequence

traverseOptional :: (x -> Flow s y) -> Maybe x -> Flow s (Maybe y)
traverseOptional = traverse
