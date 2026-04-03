-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.classes

module Hydra.Dsl.Classes where

import qualified Hydra.Classes as Classes
import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

typeClassEquality :: Phantoms.TTerm Classes.TypeClass
typeClassEquality =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.classes.TypeClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equality"),
        Core.fieldTerm = Core.TermUnit}}))

typeClassOrdering :: Phantoms.TTerm Classes.TypeClass
typeClassOrdering =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.classes.TypeClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ordering"),
        Core.fieldTerm = Core.TermUnit}}))
