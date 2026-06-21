-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.time

module Hydra.Encode.Time where
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Time as Time
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Encoder for hydra.time.Timespec
timespec :: Time.Timespec -> Core.Term
timespec x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.time.Timespec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "seconds"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 x2))) (Time.timespecSeconds x))},
        Core.Field {
          Core.fieldName = (Core.Name "nanoseconds"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 x2))) (Time.timespecNanoseconds x))}]})
