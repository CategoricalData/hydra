-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.compute

module Hydra.Decode.Compute where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

flowState :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> (Graph.Graph -> Core.Term -> Either Util.DecodingError t1) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Compute.FlowState t0 t1))
flowState s v cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "value" (Helpers.decodeMaybe v) fieldMap cx) (\field_value -> Eithers.bind (Helpers.requireField "state" s fieldMap cx) (\field_state -> Eithers.bind (Helpers.requireField "trace" trace fieldMap cx) (\field_trace -> Right (Compute.FlowState {
      Compute.flowStateValue = field_value,
      Compute.flowStateState = field_state,
      Compute.flowStateTrace = field_trace})))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.compute.FlowState"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

trace :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Compute.Trace)
trace cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "stack" (Helpers.decodeList (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))) fieldMap cx) (\field_stack -> Eithers.bind (Helpers.requireField "messages" (Helpers.decodeList (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))) fieldMap cx) (\field_messages -> Eithers.bind (Helpers.requireField "other" (Helpers.decodeMap Core_.name Core_.term) fieldMap cx) (\field_other -> Right (Compute.Trace {
      Compute.traceStack = field_stack,
      Compute.traceMessages = field_messages,
      Compute.traceOther = field_other})))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.compute.Trace"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
