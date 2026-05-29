-- Note: this is an automatically generated file. Do not edit.
-- | Type definitions for the Protobuf code generation environment

module Hydra.Protobuf.Environment where
import qualified Hydra.Core as Core
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | State threaded through Protobuf encoding: inference context plus field-index counter
data EncoderState =
  EncoderState {
    -- | Inference context used by kernel utilities (annotations, dependency analysis)
    encoderStateContext :: Typing.InferenceContext,
    -- | Counter used to assign the next Protobuf field index within the current message
    encoderStateFieldIndex :: Int}
  deriving (Eq, Ord, Read, Show)
_EncoderState = Core.Name "hydra.protobuf.environment.EncoderState"
_EncoderState_context = Core.Name "context"
_EncoderState_fieldIndex = Core.Name "fieldIndex"
-- | A reference to a structural type (Either or Pair) with its component types
data StructuralTypeRef =
  -- | An Either type with left and right component types
  StructuralTypeRefEither (Core.Type, Core.Type) |
  -- | A Pair type with first and second component types
  StructuralTypeRefPair (Core.Type, Core.Type)
  deriving (Eq, Ord, Read, Show)
_StructuralTypeRef = Core.Name "hydra.protobuf.environment.StructuralTypeRef"
_StructuralTypeRef_either = Core.Name "either"
_StructuralTypeRef_pair = Core.Name "pair"
