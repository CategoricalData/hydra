-- Note: this is an automatically generated file. Do not edit.

-- | A model for points in time

module Hydra.Time where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Int as I

-- | The POSIX struct timespec, with the same semantics: an instant in time as a number of seconds and nanoseconds since the Unix Epoch (1970-01-01T00:00:00Z). The actual resolution is implementation- and filesystem-defined. See <time.h> (https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/time.h.html) and XBD section 4.19, "Seconds Since the Epoch" (https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap04.html#tag_04_19).
data Timespec =
  Timespec {
    -- | Whole seconds since the Unix Epoch; signed, so instants in the far past or distant future are representable
    timespecSeconds :: I.Int64,
    -- | Nanoseconds within the second, in the range [0, 999999999]; unsigned, as the value is never negative
    timespecNanoseconds :: I.Int64}
  deriving (Eq, Ord, Read, Show)

_Timespec = Core.Name "hydra.time.Timespec"

_Timespec_seconds = Core.Name "seconds"

_Timespec_nanoseconds = Core.Name "nanoseconds"
