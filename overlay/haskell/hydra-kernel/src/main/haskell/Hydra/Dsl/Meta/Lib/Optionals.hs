{-# LANGUAGE FlexibleContexts #-}

-- | Phantom-typed term DSL for the hydra.lib.optionals library

module Hydra.Dsl.Meta.Lib.Optionals where

import Hydra.Typed
import Hydra.Dsl.AsTerm
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Prims as Prims
import qualified Hydra.Lib.Optionals as DefOptionals


-- | Apply a function to an argument (applicative).
apply :: TypedTerm (Maybe (a -> b)) -> TypedTerm (Maybe a) -> TypedTerm (Maybe b)
apply = primitive2 DefOptionals.apply

-- | Chain operations on optional values, handling absent cases automatically.
bind :: TypedTerm (Maybe a) -> TypedTerm (a -> Maybe b) -> TypedTerm (Maybe b)
bind = primitive2 DefOptionals.bind

-- | Handle an optional value with the optional value as the first argument.
-- The default and function arguments accept anything coercible to a term (AsTerm),
-- mirroring the ergonomics of the former 'maybe' eliminator.
cases :: (AsTerm t1 b, AsTerm t2 (a -> b)) => TypedTerm (Maybe a) -> t1 -> t2 -> TypedTerm b
cases m def f = primitive3 DefOptionals.cases m (asTerm def) (asTerm f)

-- | Filter out absent values from a list.
cat :: TypedTerm [Maybe a] -> TypedTerm [a]
cat = primitive1 DefOptionals.cat

-- | Compose two optional-returning functions (Kleisli composition).
compose :: TypedTerm (a -> Maybe b) -> TypedTerm (b -> Maybe c) -> TypedTerm (a -> Maybe c)
compose = primitive2 DefOptionals.compose

-- | Get a value from an optional value, or return a default value.
fromOptional :: TypedTerm a -> TypedTerm (Maybe a) -> TypedTerm a
fromOptional = primitive2 DefOptionals.fromOptional

-- | Check if a value is present.
isGiven :: TypedTerm (Maybe a) -> TypedTerm Bool
isGiven = primitive1 DefOptionals.isGiven

-- | Check if a value is absent.
isNone :: TypedTerm (Maybe a) -> TypedTerm Bool
isNone = primitive1 DefOptionals.isNone

-- | Map a function over an optional value.
map :: AsTerm f (a -> b) => f -> TypedTerm (Maybe a) -> TypedTerm (Maybe b)
map f = primitive2 DefOptionals.map (asTerm f)

-- | Map a function over a list and collect present results.
mapOptional :: TypedTerm (a -> Maybe b) -> TypedTerm [a] -> TypedTerm [b]
mapOptional = primitive2 DefOptionals.mapOptional

-- | Lift a value into the optional type.
pure :: TypedTerm a -> TypedTerm (Maybe a)
pure = primitive1 DefOptionals.pure

-- | Convert an optional value to a list: a present value becomes [x], an absent value becomes [].
toList :: TypedTerm (Maybe a) -> TypedTerm [a]
toList = primitive1 DefOptionals.toList
