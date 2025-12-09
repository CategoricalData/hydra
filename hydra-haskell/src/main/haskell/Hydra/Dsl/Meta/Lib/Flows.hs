{-# LANGUAGE FlexibleContexts #-}

-- | Phantom-typed term DSL for the hydra.lib.flows library

module Hydra.Dsl.Meta.Lib.Flows where

import Hydra.Compute
import Hydra.Phantoms
import Hydra.Dsl.AsTerm
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Libraries

import qualified Data.Map as M
import qualified Data.Set as S


apply :: TTerm (Flow s (x -> y)) -> TTerm (Flow s x) -> TTerm (Flow s y)
apply = primitive2 _flows_apply

bind :: (AsTerm t (Flow s x), AsTerm f (x -> Flow s y)) => t -> f -> TTerm (Flow s y)
bind fx f = primitive2 _flows_bind (asTerm fx) (asTerm f)

fail :: AsTerm f String => f -> TTerm (Flow s x)
fail = primitive1 _flows_fail . asTerm

foldl :: TTerm (x -> y -> Flow s x) -> TTerm x -> TTerm [y] -> TTerm (Flow s x)
foldl = primitive3 _flows_foldl

map :: (AsTerm f (x -> y), AsTerm t (Flow s x)) => f -> t -> TTerm (Flow s y)
map f fx = primitive2 _flows_map (asTerm f) (asTerm fx)

mapElems :: TTerm (v1 -> Flow s v2) -> TTerm (M.Map k v1) -> TTerm (Flow s (M.Map k v2))
mapElems = primitive2 _flows_mapElems

mapKeys :: TTerm (k1 -> Flow s k2) -> TTerm (M.Map k1 v) -> TTerm (Flow s (M.Map k2 v))
mapKeys = primitive2 _flows_mapKeys

mapList :: AsTerm f (x -> Flow s y) => f -> TTerm [x] -> TTerm (Flow s [y])
mapList f = primitive2 _flows_mapList (asTerm f)

mapMaybe :: AsTerm f (x -> Flow s y) => f -> TTerm (Maybe x) -> TTerm (Flow s (Maybe y))
mapMaybe f = primitive2 _flows_mapMaybe (asTerm f)

mapSet :: TTerm (x -> Flow s y) -> TTerm (S.Set x) -> TTerm (Flow s (S.Set y))
mapSet = primitive2 _flows_mapSet

pure :: AsTerm t x => t -> TTerm (Flow s x)
pure x = primitive1 _flows_pure (asTerm x)

sequence :: TTerm [Flow s a] -> TTerm (Flow s [a])
sequence = primitive1 _flows_sequence
