{-# LANGUAGE FlexibleContexts #-}

-- | Phantom-typed term DSL for the hydra.lib.maybes library

module Hydra.Dsl.Meta.Lib.Maybes where

import Hydra.Typed
import Hydra.Dsl.AsTerm
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


-- | Apply a function to an argument (applicative).
apply :: TypedTerm (Maybe (a -> b)) -> TypedTerm (Maybe a) -> TypedTerm (Maybe b)
apply = primitive2 _maybes_apply

-- | Chain operations on optional values, handling Nothing cases automatically.
bind :: TypedTerm (Maybe a) -> TypedTerm (a -> Maybe b) -> TypedTerm (Maybe b)
bind = primitive2 _maybes_bind

-- | Handle an optional value with the maybe value as the first argument.
-- The default and function arguments accept anything coercible to a term (AsTerm),
-- mirroring the ergonomics of the former 'maybe' eliminator.
cases :: (AsTerm t1 b, AsTerm t2 (a -> b)) => TypedTerm (Maybe a) -> t1 -> t2 -> TypedTerm b
cases m def f = primitive3 _maybes_cases m (asTerm def) (asTerm f)

-- | Filter out Nothing values from a list.
cat :: TypedTerm [Maybe a] -> TypedTerm [a]
cat = primitive1 _maybes_cat

-- | Compose two Maybe-returning functions (Kleisli composition).
compose :: TypedTerm (a -> Maybe b) -> TypedTerm (b -> Maybe c) -> TypedTerm (a -> Maybe c)
compose = primitive2 _maybes_compose

-- | Get a value from an optional value, or return a default value.
fromMaybe :: TypedTerm a -> TypedTerm (Maybe a) -> TypedTerm a
fromMaybe = primitive2 _maybes_fromMaybe

-- | Check if a value is Just.
isJust :: TypedTerm (Maybe a) -> TypedTerm Bool
isJust = primitive1 _maybes_isJust

-- | Check if a value is Nothing.
isNothing :: TypedTerm (Maybe a) -> TypedTerm Bool
isNothing = primitive1 _maybes_isNothing

-- | Map a function over an optional value.
map :: AsTerm f (a -> b) => f -> TypedTerm (Maybe a) -> TypedTerm (Maybe b)
map f = primitive2 _maybes_map (asTerm f)

-- | Map a function over a list and collect Just results.
mapMaybe :: TypedTerm (a -> Maybe b) -> TypedTerm [a] -> TypedTerm [b]
mapMaybe = primitive2 _maybes_mapMaybe

-- | Lift a value into the Maybe type.
pure :: TypedTerm a -> TypedTerm (Maybe a)
pure = primitive1 _maybes_pure

-- | Convert a Maybe to a list: Just x becomes [x], Nothing becomes [].
toList :: TypedTerm (Maybe a) -> TypedTerm [a]
toList = primitive1 _maybes_toList
