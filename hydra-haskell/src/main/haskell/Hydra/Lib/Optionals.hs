-- | Haskell implementations of hydra.lib.optionals primitives

module Hydra.Lib.Optionals where

import qualified Data.Maybe as Y


apply :: Y.Maybe (a -> b) -> Y.Maybe a -> Y.Maybe b
apply = (<*>)

bind :: Y.Maybe a -> (a -> Y.Maybe b) -> Y.Maybe b
bind = (>>=)

cat :: [Y.Maybe a] -> [a]
cat = Y.catMaybes

compose :: (a -> Y.Maybe b) -> (b -> Y.Maybe c) -> (a -> Y.Maybe c)
compose f g = \x -> f x >>= g

fromJust :: Y.Maybe a -> a
fromJust = Y.fromJust

fromMaybe :: a -> Y.Maybe a -> a
fromMaybe = Y.fromMaybe

isJust :: Y.Maybe a -> Bool
isJust = Y.isJust

isNothing :: Y.Maybe a -> Bool
isNothing = Y.isNothing

map :: (a -> b) -> Y.Maybe a -> Y.Maybe b
map = fmap

mapMaybe :: (a -> Y.Maybe b) -> [a] -> [b]
mapMaybe = Y.mapMaybe

maybe :: b -> (a -> b) -> Y.Maybe a -> b
maybe = Y.maybe

pure :: a -> Y.Maybe a
pure = Just
