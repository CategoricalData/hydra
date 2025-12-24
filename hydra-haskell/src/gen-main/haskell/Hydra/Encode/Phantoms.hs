-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.phantoms

module Hydra.Encode.Phantoms where

import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as Core_
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

tBinding :: (t0 -> Phantoms.TBinding t1 -> Core.Term)
tBinding a x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.phantoms.TBinding"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core_.name (Phantoms.tBindingName x))},
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (tTerm a (Phantoms.tBindingTerm x))}]}))

tTerm :: (t0 -> Phantoms.TTerm t1 -> Core.Term)
tTerm a x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.phantoms.TTerm"),
  Core.wrappedTermBody = (Core_.term (Phantoms.unTTerm x))}))
