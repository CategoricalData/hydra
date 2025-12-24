-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.compute

module Hydra.Decode.Compute where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

flowState :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> (Graph.Graph -> Core.Term -> Either Util.DecodingError t1) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Compute.FlowState t0 t1))
flowState s v cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\value -> Eithers.either (\err -> Left err) (\state -> Eithers.either (\err -> Left err) (\trace -> Right (Compute.FlowState {
      Compute.flowStateValue = value,
      Compute.flowStateState = state,
      Compute.flowStateTrace = trace})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "trace",
      " in record"]))) (\fieldTerm -> trace cx fieldTerm) (Maps.lookup (Core.Name "trace") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "state",
      " in record"]))) (\fieldTerm -> s cx fieldTerm) (Maps.lookup (Core.Name "state") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "value",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMaybe v2 -> (Eithers.mapMaybe (v cx) v2)
      _ -> (Left (Util.DecodingError "expected optional value"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "value") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.compute.FlowState"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

trace :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Compute.Trace)
trace cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\stack -> Eithers.either (\err -> Left err) (\messages -> Eithers.either (\err -> Left err) (\other -> Right (Compute.Trace {
      Compute.traceStack = stack,
      Compute.traceMessages = messages,
      Compute.traceOther = other})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "other",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMap v2 ->  
        let pairs = (Maps.toList v2) 
            decodePair = (\kv ->  
                    let k = (Pairs.first kv) 
                        v = (Pairs.second kv)
                    in (Eithers.either (\err -> Left err) (\k2 -> Eithers.either (\err2 -> Left err2) (\v2 -> Right (k2, v2)) (Core_.term cx v)) (Core_.name cx k)))
        in (Eithers.either (\err -> Left err) (\decodedPairs -> Right (Maps.fromList decodedPairs)) (Eithers.mapList decodePair pairs))
      _ -> (Left (Util.DecodingError "expected map"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "other") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "messages",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (\raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
        Core.TermLiteral v3 -> ((\x -> case x of
          Core.LiteralString v4 -> (Right v4)
          _ -> (Left (Util.DecodingError "expected string literal"))) v3)
        _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v2)
      _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "messages") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "stack",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (\raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
        Core.TermLiteral v3 -> ((\x -> case x of
          Core.LiteralString v4 -> (Right v4)
          _ -> (Left (Util.DecodingError "expected string literal"))) v3)
        _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v2)
      _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "stack") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.compute.Trace"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
