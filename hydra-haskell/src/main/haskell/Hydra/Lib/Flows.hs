-- | Haskell implementations of hydra/lib/flows primitives

module Hydra.Lib.Flows where

import Hydra.Kernel


apply :: Flow s (x -> y) -> Flow s x -> Flow s y
apply = (<*>)

bind :: Flow s x -> (x -> Flow s y) -> Flow s y
bind = (>>=)

map :: (x -> y) -> Flow s x -> Flow s y
map = fmap

pure :: x -> Flow s x
pure = return
