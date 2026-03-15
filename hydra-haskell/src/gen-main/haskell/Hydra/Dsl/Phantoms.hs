-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.phantoms

module Hydra.Dsl.Phantoms where

import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

tBinding :: (Core.Name -> Phantoms.TTerm t0 -> Phantoms.TBinding t0)
tBinding name term = Phantoms.TBinding {
  Phantoms.tBindingName = name,
  Phantoms.tBindingTerm = term}

tBindingName :: (Phantoms.TBinding t0 -> Core.Name)
tBindingName = Phantoms.tBindingName

tBindingTerm :: (Phantoms.TBinding t0 -> Phantoms.TTerm t0)
tBindingTerm = Phantoms.tBindingTerm

tBindingWithName :: (Phantoms.TBinding t0 -> Core.Name -> Phantoms.TBinding t0)
tBindingWithName original newVal = Phantoms.TBinding {
  Phantoms.tBindingName = newVal,
  Phantoms.tBindingTerm = (Phantoms.tBindingTerm original)}

tBindingWithTerm :: (Phantoms.TBinding t0 -> Phantoms.TTerm t1 -> Phantoms.TBinding t1)
tBindingWithTerm original newVal = Phantoms.TBinding {
  Phantoms.tBindingName = (Phantoms.tBindingName original),
  Phantoms.tBindingTerm = newVal}

tTerm :: (Core.Term -> Phantoms.TTerm t0)
tTerm x = (Phantoms.TTerm x)

unTTerm :: (Phantoms.TTerm t0 -> Core.Term)
unTTerm = Phantoms.unTTerm
