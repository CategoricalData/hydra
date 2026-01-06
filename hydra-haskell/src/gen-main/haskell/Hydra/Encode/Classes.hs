-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.classes

module Hydra.Encode.Classes where

import qualified Hydra.Classes as Classes
import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

typeClass :: (Classes.TypeClass -> Core.Term)
typeClass x = case x of
  Classes.TypeClassEquality -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.classes.TypeClass"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "equality"),
      Core.fieldTerm = Core.TermUnit}}))
  Classes.TypeClassOrdering -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.classes.TypeClass"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "ordering"),
      Core.fieldTerm = Core.TermUnit}}))
