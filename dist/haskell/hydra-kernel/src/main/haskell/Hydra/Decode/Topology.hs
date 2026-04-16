-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.topology

module Hydra.Decode.Topology where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Topology as Topology
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M

graph :: Graph.Graph -> Core.Term -> Either Errors.DecodingError (M.Map Int [Int])
graph = ExtractCore.decodeMap vertex (ExtractCore.decodeList vertex)

tarjanState :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Topology.TarjanState
tarjanState cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "counter" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_counter -> Eithers.bind (ExtractCore.requireField "indices" (ExtractCore.decodeMap vertex (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_indices -> Eithers.bind (ExtractCore.requireField "lowLinks" (ExtractCore.decodeMap vertex (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_lowLinks -> Eithers.bind (ExtractCore.requireField "stack" (ExtractCore.decodeList vertex) fieldMap cx) (\field_stack -> Eithers.bind (ExtractCore.requireField "onStack" (ExtractCore.decodeSet vertex) fieldMap cx) (\field_onStack -> Eithers.bind (ExtractCore.requireField "sccs" (ExtractCore.decodeList (ExtractCore.decodeList vertex)) fieldMap cx) (\field_sccs -> Right (Topology.TarjanState {
          Topology.tarjanStateCounter = field_counter,
          Topology.tarjanStateIndices = field_indices,
          Topology.tarjanStateLowLinks = field_lowLinks,
          Topology.tarjanStateStack = field_stack,
          Topology.tarjanStateOnStack = field_onStack,
          Topology.tarjanStateSccs = field_sccs}))))))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

vertex :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Int
vertex cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermLiteral v0 -> case v0 of
        Core.LiteralInteger v1 -> case v1 of
          Core.IntegerValueInt32 v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected int32 value")
        _ -> Left (Errors.DecodingError "expected int32 literal")
      _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw)
