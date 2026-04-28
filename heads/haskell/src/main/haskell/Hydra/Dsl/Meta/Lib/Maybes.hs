{-# LANGUAGE FlexibleContexts #-}

-- | Phantom-typed term DSL for the hydra.lib.maybes library

module Hydra.Dsl.Meta.Lib.Maybes where

import Hydra.Phantoms
import Hydra.Dsl.AsTerm
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


-- | Apply a function to an argument (applicative).
apply :: TTerm (Maybe (a -> b)) -> TTerm (Maybe a) -> TTerm (Maybe b)
apply = primitive2 _maybes_apply

-- | Chain operations on optional values, handling Nothing cases automatically.
bind :: TTerm (Maybe a) -> TTerm (a -> Maybe b) -> TTerm (Maybe b)
bind = primitive2 _maybes_bind

-- | Handle an optional value with the maybe value as the first argument.
cases :: TTerm (Maybe a) -> TTerm b -> TTerm (a -> b) -> TTerm b
cases = primitive3 _maybes_cases

-- | Filter out Nothing values from a list.
cat :: TTerm [Maybe a] -> TTerm [a]
cat = primitive1 _maybes_cat

-- | Compose two Maybe-returning functions (Kleisli composition).
compose :: TTerm (a -> Maybe b) -> TTerm (b -> Maybe c) -> TTerm (a -> Maybe c)
compose = primitive2 _maybes_compose

-- | Get a value from an optional value, or return a default value.
fromMaybe :: TTerm a -> TTerm (Maybe a) -> TTerm a
fromMaybe = primitive2 _maybes_fromMaybe

-- | Check if a value is Just.
isJust :: TTerm (Maybe a) -> TTerm Bool
isJust = primitive1 _maybes_isJust

-- | Check if a value is Nothing.
isNothing :: TTerm (Maybe a) -> TTerm Bool
isNothing = primitive1 _maybes_isNothing

-- | Map a function over an optional value.
map :: AsTerm f (a -> b) => f -> TTerm (Maybe a) -> TTerm (Maybe b)
map f = primitive2 _maybes_map (asTerm f)

-- | Map a function over a list and collect Just results.
mapMaybe :: TTerm (a -> Maybe b) -> TTerm [a] -> TTerm [b]
mapMaybe = primitive2 _maybes_mapMaybe

-- | Eliminate an optional value with a default and a function.
maybe :: (AsTerm t1 b, AsTerm t2 (a -> b)) => t1 -> t2 -> TTerm (Maybe a) -> TTerm b
maybe def f = primitive3 _maybes_maybe (asTerm def) (asTerm f)

-- | Lift a value into the Maybe type.
pure :: TTerm a -> TTerm (Maybe a)
pure = primitive1 _maybes_pure

-- | Convert a Maybe to a list: Just x becomes [x], Nothing becomes [].
toList :: TTerm (Maybe a) -> TTerm [a]
toList = primitive1 _maybes_toList
