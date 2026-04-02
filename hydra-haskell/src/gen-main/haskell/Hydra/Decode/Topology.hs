-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.topology

module Hydra.Decode.Topology where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Topology as Topology
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

graph :: Graph.Graph -> Core.Term -> Either Errors.DecodingError (M.Map Int [Int])
graph = Core_.decodeMap vertex (Core_.decodeList vertex)

tarjanState :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Topology.TarjanState
tarjanState cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "counter" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_counter -> Eithers.bind (Core_.requireField "indices" (Core_.decodeMap vertex (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw))) fieldMap cx) (\field_indices -> Eithers.bind (Core_.requireField "lowLinks" (Core_.decodeMap vertex (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw))) fieldMap cx) (\field_lowLinks -> Eithers.bind (Core_.requireField "stack" (Core_.decodeList vertex) fieldMap cx) (\field_stack -> Eithers.bind (Core_.requireField "onStack" (Core_.decodeSet vertex) fieldMap cx) (\field_onStack -> Eithers.bind (Core_.requireField "sccs" (Core_.decodeList (Core_.decodeList vertex)) fieldMap cx) (\field_sccs -> Right (Topology.TarjanState {
          Topology.tarjanStateCounter = field_counter,
          Topology.tarjanStateIndices = field_indices,
          Topology.tarjanStateLowLinks = field_lowLinks,
          Topology.tarjanStateStack = field_stack,
          Topology.tarjanStateOnStack = field_onStack,
          Topology.tarjanStateSccs = field_sccs}))))))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

vertex :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Int
vertex cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermLiteral v0 -> case v0 of
        Core.LiteralInteger v1 -> case v1 of
          Core.IntegerValueInt32 v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected int32 value")
        _ -> Left (Errors.DecodingError "expected int32 literal")
      _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)
