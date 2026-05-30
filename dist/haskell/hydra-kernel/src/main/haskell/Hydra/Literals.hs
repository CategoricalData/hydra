-- Note: this is an automatically generated file. Do not edit.
-- | Conversion functions for literal values.

module Hydra.Literals where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Literals as Literals
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Convert a bigint to an integer value of a given type (note: lossy)
bigintToIntegerValue :: Core.IntegerType -> Integer -> Core.IntegerValue
bigintToIntegerValue it bi =
    case it of
      Core.IntegerTypeBigint -> Core.IntegerValueBigint bi
      Core.IntegerTypeInt8 -> Core.IntegerValueInt8 (Literals.bigintToInt8 bi)
      Core.IntegerTypeInt16 -> Core.IntegerValueInt16 (Literals.bigintToInt16 bi)
      Core.IntegerTypeInt32 -> Core.IntegerValueInt32 (Literals.bigintToInt32 bi)
      Core.IntegerTypeInt64 -> Core.IntegerValueInt64 (Literals.bigintToInt64 bi)
      Core.IntegerTypeUint8 -> Core.IntegerValueUint8 (Literals.bigintToUint8 bi)
      Core.IntegerTypeUint16 -> Core.IntegerValueUint16 (Literals.bigintToUint16 bi)
      Core.IntegerTypeUint32 -> Core.IntegerValueUint32 (Literals.bigintToUint32 bi)
      Core.IntegerTypeUint64 -> Core.IntegerValueUint64 (Literals.bigintToUint64 bi)
-- | Convert an integer value of any precision to a bigint
integerValueToBigint :: Core.IntegerValue -> Integer
integerValueToBigint x =
    case x of
      Core.IntegerValueBigint v0 -> v0
      Core.IntegerValueInt8 v0 -> Literals.int8ToBigint v0
      Core.IntegerValueInt16 v0 -> Literals.int16ToBigint v0
      Core.IntegerValueInt32 v0 -> Literals.int32ToBigint v0
      Core.IntegerValueInt64 v0 -> Literals.int64ToBigint v0
      Core.IntegerValueUint8 v0 -> Literals.uint8ToBigint v0
      Core.IntegerValueUint16 v0 -> Literals.uint16ToBigint v0
      Core.IntegerValueUint32 v0 -> Literals.uint32ToBigint v0
      Core.IntegerValueUint64 v0 -> Literals.uint64ToBigint v0
