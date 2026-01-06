-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.topology

module Hydra.Decode.Topology where

import qualified Hydra.Core as Core
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Topology as Topology
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

graph :: (Graph.Graph -> Core.Term -> Either Util.DecodingError (M.Map Int [Int]))
graph = (Helpers.decodeMap vertex (Helpers.decodeList vertex))

tarjanState :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Topology.TarjanState)
tarjanState cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "counter" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralInteger v3 -> ((\x -> case x of
          Core.IntegerValueInt32 v4 -> (Right v4)
          _ -> (Left (Util.DecodingError "expected int32 value"))) v3)
        _ -> (Left (Util.DecodingError "expected int32 literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_counter -> Eithers.bind (Helpers.requireField "indices" (Helpers.decodeMap vertex (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralInteger v3 -> ((\x -> case x of
          Core.IntegerValueInt32 v4 -> (Right v4)
          _ -> (Left (Util.DecodingError "expected int32 value"))) v3)
        _ -> (Left (Util.DecodingError "expected int32 literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))) fieldMap cx) (\field_indices -> Eithers.bind (Helpers.requireField "lowLinks" (Helpers.decodeMap vertex (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralInteger v3 -> ((\x -> case x of
          Core.IntegerValueInt32 v4 -> (Right v4)
          _ -> (Left (Util.DecodingError "expected int32 value"))) v3)
        _ -> (Left (Util.DecodingError "expected int32 literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))) fieldMap cx) (\field_lowLinks -> Eithers.bind (Helpers.requireField "stack" (Helpers.decodeList vertex) fieldMap cx) (\field_stack -> Eithers.bind (Helpers.requireField "onStack" (Helpers.decodeSet vertex) fieldMap cx) (\field_onStack -> Eithers.bind (Helpers.requireField "sccs" (Helpers.decodeList (Helpers.decodeList vertex)) fieldMap cx) (\field_sccs -> Right (Topology.TarjanState {
      Topology.tarjanStateCounter = field_counter,
      Topology.tarjanStateIndices = field_indices,
      Topology.tarjanStateLowLinks = field_lowLinks,
      Topology.tarjanStateStack = field_stack,
      Topology.tarjanStateOnStack = field_onStack,
      Topology.tarjanStateSccs = field_sccs}))))))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.topology.TarjanState"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

vertex :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Int)
vertex cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermLiteral v1 -> ((\x -> case x of
    Core.LiteralInteger v2 -> ((\x -> case x of
      Core.IntegerValueInt32 v3 -> (Right v3)
      _ -> (Left (Util.DecodingError "expected int32 value"))) v2)
    _ -> (Left (Util.DecodingError "expected int32 literal"))) v1)
  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
