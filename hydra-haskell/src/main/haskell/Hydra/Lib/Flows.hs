-- | Haskell implementations of hydra.lib.flows primitives. These are simply wrappers around hydra.flows functions.

module Hydra.Lib.Flows where

import Hydra.Compute
import qualified Hydra.Flows as Flows

import qualified Control.Monad as CM

-- Haskell-specific helpers

instance Functor (Flow s) where
  fmap = CM.liftM
instance Applicative (Flow s) where
  pure = Flows.pure
  (<*>) = CM.ap
instance Monad (Flow s) where
  (>>=) = Flows.bind
instance MonadFail (Flow s) where
  fail = Flows.fail

-- Primitive functions

apply :: Flow s (x -> y) -> Flow s x -> Flow s y
apply = (<*>)

bind :: Flow s x -> (x -> Flow s y) -> Flow s y
bind = Flows.bind

fail :: String -> Flow s x
fail = Flows.fail

map :: (x -> y) -> Flow s x -> Flow s y
map = Flows.map

mapList :: (x -> Flow s y) -> [x] -> Flow s [y]
mapList = CM.mapM

pure :: x -> Flow s x
pure = Flows.pure

sequence :: [Flow s x] -> Flow s [x]
sequence = CM.sequence

traverseOptional :: (x -> Flow s y) -> Maybe x -> Flow s (Maybe y)
traverseOptional = traverse
