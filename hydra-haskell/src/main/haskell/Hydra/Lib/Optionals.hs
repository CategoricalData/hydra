-- | Haskell implementations of hydra/lib/optionals primitives

module Hydra.Lib.Optionals where

import qualified Data.Maybe as Y


apply :: Y.Maybe (a -> b) -> Y.Maybe a -> Y.Maybe b
apply = (<*>)

bind :: Y.Maybe a -> (a -> Y.Maybe b) -> Y.Maybe b
bind = (>>=)

cat :: [Y.Maybe a] -> [a]
cat = Y.catMaybes

isJust :: Y.Maybe a -> Bool
isJust = Y.isJust

isNothing :: Y.Maybe a -> Bool
isNothing = Y.isNothing

map :: (a -> b) -> Y.Maybe a -> Y.Maybe b
map = fmap

pure :: a -> Y.Maybe a
pure = Just
