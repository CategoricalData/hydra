module Hydra.Lib.Optionals where

import qualified Data.Maybe as Y


apply :: Y.Maybe (a -> b) -> Y.Maybe a -> Y.Maybe b
apply = (<*>)

bind :: Y.Maybe a -> (a -> Y.Maybe b) -> Y.Maybe b
bind = (>>=)

map :: (a -> b) -> Y.Maybe a -> Y.Maybe b
map = fmap

pure :: a -> Y.Maybe a
pure = Just
