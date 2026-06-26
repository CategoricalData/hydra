-- Note: this is an automatically generated file. Do not edit.
-- | Term decoders for hydra.time

module Hydra.Decode.Time where
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Overlay.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Time as Time
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Decoder for hydra.time.Timespec
timespec :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Time.Timespec
timespec cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "seconds" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt64 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int64 value")
            _ -> Left (Errors.DecodingError "expected int64 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_seconds -> Eithers.bind (ExtractCore.requireField "nanoseconds" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueUint32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected uint32 value")
            _ -> Left (Errors.DecodingError "expected uint32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_nanoseconds -> Right (Time.Timespec {
          Time.timespecSeconds = field_seconds,
          Time.timespecNanoseconds = field_nanoseconds}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.time.Timespec")) (ExtractCore.stripWithDecodingError cx raw)
