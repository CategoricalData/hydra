-- | Haskell implementations of hydra/lib/optionals primitives

module Hydra.Lib.Optionals where

import qualified Data.Maybe as Y


apply :: Y.Maybe (x -> y) -> Y.Maybe x -> Y.Maybe y
apply = (<*>)

bind :: Y.Maybe x -> (x -> Y.Maybe y) -> Y.Maybe y
bind = (>>=)

map :: (x -> y) -> Y.Maybe x -> Y.Maybe y
map = fmap

pure :: x -> Y.Maybe x
pure = Just
