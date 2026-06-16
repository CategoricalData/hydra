-- | Phantom-typed term DSL for the hydra.lib.equality library

{-# LANGUAGE FlexibleContexts #-}

module Hydra.Dsl.Meta.Lib.Equality where

import Hydra.Dsl.AsTerm
import Hydra.Dsl.Meta.Phantoms
import Hydra.Typed
import Hydra.Util
import qualified Hydra.Dsl.Terms as Terms

import Data.Int
import qualified Hydra.Dsl.Prims as Prims
import qualified Hydra.Lib.Equality as DefEquality


-- | Compare two values and return a Comparison.
compare :: AsTerm t2 a => TypedTerm a -> t2 -> TypedTerm Comparison
compare t1 t2 = primitive2 DefEquality.compare t1 (asTerm t2)

-- | Check if two values are equal.
equal :: AsTerm t2 a => TypedTerm a -> t2 -> TypedTerm Bool
equal t1 t2 = primitive2 DefEquality.equal t1 (asTerm t2)

-- | Check if first value is greater than second.
gt :: AsTerm t2 a => TypedTerm a -> t2 -> TypedTerm Bool
gt t1 t2 = primitive2 DefEquality.gt t1 (asTerm t2)

-- | Check if first value is greater than or equal to second.
gte :: AsTerm t2 a => TypedTerm a -> t2 -> TypedTerm Bool
gte t1 t2 = primitive2 DefEquality.gte t1 (asTerm t2)

-- | Return a value unchanged.
identity :: TypedTerm a -> TypedTerm a
identity = primitive1 DefEquality.identity

-- | Check if first value is less than second.
lt :: AsTerm t2 a => TypedTerm a -> t2 -> TypedTerm Bool
lt t1 t2 = primitive2 DefEquality.lt t1 (asTerm t2)

-- | Check if first value is less than or equal to second.
lte :: AsTerm t2 a => TypedTerm a -> t2 -> TypedTerm Bool
lte t1 t2 = primitive2 DefEquality.lte t1 (asTerm t2)

-- | Return the maximum of two values.
max :: AsTerm t2 a => TypedTerm a -> t2 -> TypedTerm a
max t1 t2 = primitive2 DefEquality.max t1 (asTerm t2)

-- | Return the minimum of two values.
min :: AsTerm t2 a => TypedTerm a -> t2 -> TypedTerm a
min t1 t2 = primitive2 DefEquality.min t1 (asTerm t2)
