-- | Haskell implementations of hydra.lib.optionals primitives

module Hydra.Overlay.Haskell.Lib.Optionals where

import Prelude hiding (map, pure)
import qualified Control.Monad as CM
import qualified Data.Maybe as Y
import qualified Data.Set as S


-- | Apply a function to an argument (applicative).
apply :: Y.Maybe (a -> b) -> Y.Maybe a -> Y.Maybe b
apply = (<*>)

-- | Chain operations on optional values, handling absent cases automatically.
bind :: Y.Maybe a -> (a -> Y.Maybe b) -> Y.Maybe b
bind = (>>=)

-- | Handle an optional value with the absent-case value as the first argument.
cases :: Y.Maybe a -> b -> (a -> b) -> b
cases m n j = Y.maybe n j m

-- | Filter out absent values from a list.
cat :: [Y.Maybe a] -> [a]
cat = Y.catMaybes

-- | Compose two optional-returning functions (Kleisli composition).
compose :: (a -> Y.Maybe b) -> (b -> Y.Maybe c) -> (a -> Y.Maybe c)
compose f g = \x -> f x >>= g

-- | Left-fold over a list with an optional-returning function, short-circuiting on none.
foldList :: (a -> b -> Y.Maybe a) -> a -> [b] -> Y.Maybe a
foldList = CM.foldM

-- | Get a value from an optional value, or return a default value.
fromOptional :: a -> Y.Maybe a -> a
fromOptional = Y.fromMaybe

-- | Filter out absent values from a list. (Alias of cat.)
givens :: [Y.Maybe a] -> [a]
givens = Y.catMaybes

-- | Check if a value is present.
isGiven :: Y.Maybe a -> Bool
isGiven = Y.isJust

-- | Check if a value is absent.
isNone :: Y.Maybe a -> Bool
isNone = Y.isNothing

-- | Map a function over an optional value.
map :: (a -> b) -> Y.Maybe a -> Y.Maybe b
map = fmap

-- | Traverse a list in the optional monad.
mapList :: (a -> Y.Maybe b) -> [a] -> Y.Maybe [b]
mapList = CM.mapM

-- | Map a function over a list and collect present results.
mapOptional :: (a -> Y.Maybe b) -> [a] -> [b]
mapOptional = Y.mapMaybe

-- | Traverse a set in the optional monad.
mapSet :: Ord b => (a -> Y.Maybe b) -> S.Set a -> Y.Maybe (S.Set b)
mapSet f s = fmap S.fromList (CM.mapM f (S.toList s))

-- | Lift a value into the optional type.
pure :: a -> Y.Maybe a
pure = Just

-- | Convert an optional value to a list: a present value becomes [x], an absent value becomes [].
toList :: Y.Maybe a -> [a]
toList = Y.maybeToList

-- | Get a value from an optional value, or return a default value. (Alias of fromOptional.)
withDefault :: a -> Y.Maybe a -> a
withDefault = Y.fromMaybe
