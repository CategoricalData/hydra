-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.topology

module Hydra.Decode.Topology where

import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Extract.Helpers as Helpers
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

graph :: Graph.Graph -> Core.Term -> Either Error.DecodingError (M.Map Int [Int])
graph = Helpers.decodeMap vertex (Helpers.decodeList vertex)

tarjanState :: Graph.Graph -> Core.Term -> Either Error.DecodingError Topology.TarjanState
tarjanState cx raw =
    Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->  
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "counter" (\cx -> \raw -> Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Error.DecodingError "expected int32 value")
            _ -> Left (Error.DecodingError "expected int32 literal")
          _ -> Left (Error.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_counter -> Eithers.bind (Helpers.requireField "indices" (Helpers.decodeMap vertex (\cx -> \raw -> Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Error.DecodingError "expected int32 value")
            _ -> Left (Error.DecodingError "expected int32 literal")
          _ -> Left (Error.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw))) fieldMap cx) (\field_indices -> Eithers.bind (Helpers.requireField "lowLinks" (Helpers.decodeMap vertex (\cx -> \raw -> Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Error.DecodingError "expected int32 value")
            _ -> Left (Error.DecodingError "expected int32 literal")
          _ -> Left (Error.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw))) fieldMap cx) (\field_lowLinks -> Eithers.bind (Helpers.requireField "stack" (Helpers.decodeList vertex) fieldMap cx) (\field_stack -> Eithers.bind (Helpers.requireField "onStack" (Helpers.decodeSet vertex) fieldMap cx) (\field_onStack -> Eithers.bind (Helpers.requireField "sccs" (Helpers.decodeList (Helpers.decodeList vertex)) fieldMap cx) (\field_sccs -> Right (Topology.TarjanState {
          Topology.tarjanStateCounter = field_counter,
          Topology.tarjanStateIndices = field_indices,
          Topology.tarjanStateLowLinks = field_lowLinks,
          Topology.tarjanStateStack = field_stack,
          Topology.tarjanStateOnStack = field_onStack,
          Topology.tarjanStateSccs = field_sccs}))))))))
      _ -> Left (Error.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

vertex :: Graph.Graph -> Core.Term -> Either Error.DecodingError Int
vertex cx raw =
    Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> case stripped of
      Core.TermLiteral v0 -> case v0 of
        Core.LiteralInteger v1 -> case v1 of
          Core.IntegerValueInt32 v2 -> Right v2
          _ -> Left (Error.DecodingError "expected int32 value")
        _ -> Left (Error.DecodingError "expected int32 literal")
      _ -> Left (Error.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)
