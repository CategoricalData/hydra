-- | Phantom-typed term DSL for the hydra.lib.equality library

{-# LANGUAGE FlexibleContexts #-}

module Hydra.Dsl.Meta.Lib.Equality where

import Hydra.Dsl.AsTerm
import Hydra.Dsl.Meta.Phantoms
import Hydra.Phantoms
import Hydra.Util
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries

import Data.Int


compare :: AsTerm t2 a => TTerm a -> t2 -> TTerm Comparison
compare t1 t2 = primitive2 _equality_compare t1 (asTerm t2)

equal :: AsTerm t2 a => TTerm a -> t2 -> TTerm Bool
equal t1 t2 = primitive2 _equality_equal t1 (asTerm t2)

gt :: AsTerm t2 a => TTerm a -> t2 -> TTerm Bool
gt t1 t2 = primitive2 _equality_gt t1 (asTerm t2)

gte :: AsTerm t2 a => TTerm a -> t2 -> TTerm Bool
gte t1 t2 = primitive2 _equality_gte t1 (asTerm t2)

identity :: TTerm a -> TTerm a
identity = primitive1 _equality_identity

lt :: AsTerm t2 a => TTerm a -> t2 -> TTerm Bool
lt t1 t2 = primitive2 _equality_lt t1 (asTerm t2)

lte :: AsTerm t2 a => TTerm a -> t2 -> TTerm Bool
lte t1 t2 = primitive2 _equality_lte t1 (asTerm t2)

max :: AsTerm t2 a => TTerm a -> t2 -> TTerm a
max t1 t2 = primitive2 _equality_max t1 (asTerm t2)

min :: AsTerm t2 a => TTerm a -> t2 -> TTerm a
min t1 t2 = primitive2 _equality_min t1 (asTerm t2)
