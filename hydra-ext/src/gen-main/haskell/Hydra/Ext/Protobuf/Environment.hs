-- Note: this is an automatically generated file. Do not edit.

-- | Type definitions for the Protobuf code generation environment

module Hydra.Ext.Protobuf.Environment where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A reference to a structural type (Either or Pair) with its component types
data StructuralTypeRef =
  -- | An Either type with left and right component types
  StructuralTypeRefEither (Core.Type, Core.Type) |
  -- | A Pair type with first and second component types
  StructuralTypeRefPair (Core.Type, Core.Type)
  deriving (Eq, Ord, Read, Show)

_StructuralTypeRef = Core.Name "hydra.ext.protobuf.environment.StructuralTypeRef"

_StructuralTypeRef_either = Core.Name "either"

_StructuralTypeRef_pair = Core.Name "pair"
