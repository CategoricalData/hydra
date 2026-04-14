-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.protobuf.environment

module Hydra.Dsl.Protobuf.Environment where

import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Protobuf.Environment as Environment
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

structuralTypeRefEither :: Phantoms.TTerm (Core.Type, Core.Type) -> Phantoms.TTerm Environment.StructuralTypeRef
structuralTypeRefEither x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.protobuf.environment.StructuralTypeRef"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "either"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

structuralTypeRefPair :: Phantoms.TTerm (Core.Type, Core.Type) -> Phantoms.TTerm Environment.StructuralTypeRef
structuralTypeRefPair x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.protobuf.environment.StructuralTypeRef"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
