-- | Haskell implementations of hydra.lib.maybes primitives

module Hydra.Lib.Maybes where

import qualified Data.Maybe as Y


-- | Apply a function to an argument (applicative).
apply :: Y.Maybe (a -> b) -> Y.Maybe a -> Y.Maybe b
apply = (<*>)

-- | Chain operations on optional values, handling Nothing cases automatically.
bind :: Y.Maybe a -> (a -> Y.Maybe b) -> Y.Maybe b
bind = (>>=)

-- | Handle an optional value with different parameter order than maybe.
-- Arguments are: the Maybe value, the default for Nothing, and the function to apply for Just.
cases :: Y.Maybe a -> b -> (a -> b) -> b
cases m n j = Y.maybe n j m

-- | Filter out Nothing values from a list.
cat :: [Y.Maybe a] -> [a]
cat = Y.catMaybes

-- | Compose two Maybe-returning functions (Kleisli composition).
compose :: (a -> Y.Maybe b) -> (b -> Y.Maybe c) -> (a -> Y.Maybe c)
compose f g = \x -> f x >>= g

-- | Extract value from Maybe, assuming it's Just (unsafe).
-- TODO: partial function. See https://github.com/CategoricalData/hydra/issues/201
fromJust :: Y.Maybe a -> a
fromJust = Y.fromJust

-- | Get a value from an optional value, or return a default value.
fromMaybe :: a -> Y.Maybe a -> a
fromMaybe = Y.fromMaybe

-- | Check if a value is Just.
isJust :: Y.Maybe a -> Bool
isJust = Y.isJust

-- | Check if a value is Nothing.
isNothing :: Y.Maybe a -> Bool
isNothing = Y.isNothing

-- | Map a function over an optional value.
map :: (a -> b) -> Y.Maybe a -> Y.Maybe b
map = fmap

-- | Map a function over a list and collect Just results.
mapMaybe :: (a -> Y.Maybe b) -> [a] -> [b]
mapMaybe = Y.mapMaybe

-- | Handle an optional value, with transformation.
maybe :: b -> (a -> b) -> Y.Maybe a -> b
maybe = Y.maybe

-- | Lift a value into the Maybe type.
pure :: a -> Y.Maybe a
pure = Just
