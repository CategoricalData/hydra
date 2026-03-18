-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.protobuf.environment

module Hydra.Dsl.Ext.Protobuf.Environment where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Protobuf.Environment as Environment
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

structuralTypeRefEither :: Phantoms.TTerm (Core.Type, Core.Type) -> Phantoms.TTerm Environment.StructuralTypeRef
structuralTypeRefEither x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.environment.StructuralTypeRef"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "either"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

structuralTypeRefPair :: Phantoms.TTerm (Core.Type, Core.Type) -> Phantoms.TTerm Environment.StructuralTypeRef
structuralTypeRefPair x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.protobuf.environment.StructuralTypeRef"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
