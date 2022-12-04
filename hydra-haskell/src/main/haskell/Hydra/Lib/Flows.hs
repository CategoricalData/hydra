module Hydra.Lib.Flows where

import Hydra.Kernel


apply :: Flow s (a -> b) -> Flow s a -> Flow s b
apply = (<*>)

bind :: Flow s a -> (a -> Flow s b) -> Flow s b
bind = (>>=)

map :: (a -> b) -> Flow s a -> Flow s b
map = fmap

pure :: a -> Flow s a
pure = return
