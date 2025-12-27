-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.topology

module Hydra.Decode.Topology where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Topology as Topology
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

graph :: (Graph.Graph -> Core.Term -> Either Util.DecodingError (M.Map Int [Int]))
graph cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermMap v1 ->  
    let pairs = (Maps.toList v1) 
        decodePair = (\kv ->  
                let rawKey = (Pairs.first kv) 
                    rawVal = (Pairs.second kv)
                in (Eithers.either (\err -> Left err) (\k2 -> Eithers.either (\err2 -> Left err2) (\v2 -> Right (k2, v2)) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermList v2 -> (Eithers.mapList (vertex cx) v2)
                  _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx rawVal))) (vertex cx rawKey)))
    in (Eithers.either (\err -> Left err) (\decodedPairs -> Right (Maps.fromList decodedPairs)) (Eithers.mapList decodePair pairs))
  _ -> (Left (Util.DecodingError "expected map"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

tarjanState :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Topology.TarjanState)
tarjanState cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_counter -> Eithers.either (\err -> Left err) (\field_indices -> Eithers.either (\err -> Left err) (\field_lowLinks -> Eithers.either (\err -> Left err) (\field_stack -> Eithers.either (\err -> Left err) (\field_onStack -> Eithers.either (\err -> Left err) (\field_sccs -> Right (Topology.TarjanState {
      Topology.tarjanStateCounter = field_counter,
      Topology.tarjanStateIndices = field_indices,
      Topology.tarjanStateLowLinks = field_lowLinks,
      Topology.tarjanStateStack = field_stack,
      Topology.tarjanStateOnStack = field_onStack,
      Topology.tarjanStateSccs = field_sccs})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "sccs",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (\raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
        Core.TermList v3 -> (Eithers.mapList (vertex cx) v3)
        _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v2)
      _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "sccs") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "onStack",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermSet v2 ->  
        let elements = (Sets.toList v2)
        in (Eithers.either (\err -> Left err) (\decodedElems -> Right (Sets.fromList decodedElems)) (Eithers.mapList (vertex cx) elements))
      _ -> (Left (Util.DecodingError "expected set"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "onStack") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "stack",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (vertex cx) v2)
      _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "stack") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "lowLinks",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMap v2 ->  
        let pairs = (Maps.toList v2) 
            decodePair = (\kv ->  
                    let rawKey = (Pairs.first kv) 
                        rawVal = (Pairs.second kv)
                    in (Eithers.either (\err -> Left err) (\k2 -> Eithers.either (\err2 -> Left err2) (\v2 -> Right (k2, v2)) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                      Core.TermLiteral v3 -> ((\x -> case x of
                        Core.LiteralInteger v4 -> ((\x -> case x of
                          Core.IntegerValueInt32 v5 -> (Right v5)
                          _ -> (Left (Util.DecodingError "expected int32 value"))) v4)
                        _ -> (Left (Util.DecodingError "expected int32 literal"))) v3)
                      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx rawVal))) (vertex cx rawKey)))
        in (Eithers.either (\err -> Left err) (\decodedPairs -> Right (Maps.fromList decodedPairs)) (Eithers.mapList decodePair pairs))
      _ -> (Left (Util.DecodingError "expected map"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "lowLinks") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "indices",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMap v2 ->  
        let pairs = (Maps.toList v2) 
            decodePair = (\kv ->  
                    let rawKey = (Pairs.first kv) 
                        rawVal = (Pairs.second kv)
                    in (Eithers.either (\err -> Left err) (\k2 -> Eithers.either (\err2 -> Left err2) (\v2 -> Right (k2, v2)) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                      Core.TermLiteral v3 -> ((\x -> case x of
                        Core.LiteralInteger v4 -> ((\x -> case x of
                          Core.IntegerValueInt32 v5 -> (Right v5)
                          _ -> (Left (Util.DecodingError "expected int32 value"))) v4)
                        _ -> (Left (Util.DecodingError "expected int32 literal"))) v3)
                      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx rawVal))) (vertex cx rawKey)))
        in (Eithers.either (\err -> Left err) (\decodedPairs -> Right (Maps.fromList decodedPairs)) (Eithers.mapList decodePair pairs))
      _ -> (Left (Util.DecodingError "expected map"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "indices") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "counter",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralInteger v3 -> ((\x -> case x of
          Core.IntegerValueInt32 v4 -> (Right v4)
          _ -> (Left (Util.DecodingError "expected int32 value"))) v3)
        _ -> (Left (Util.DecodingError "expected int32 literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "counter") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.topology.TarjanState"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

vertex :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Int)
vertex cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermLiteral v1 -> ((\x -> case x of
    Core.LiteralInteger v2 -> ((\x -> case x of
      Core.IntegerValueInt32 v3 -> (Right v3)
      _ -> (Left (Util.DecodingError "expected int32 value"))) v2)
    _ -> (Left (Util.DecodingError "expected int32 literal"))) v1)
  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
