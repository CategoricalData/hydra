-- Note: this is an automatically generated file. Do not edit.

-- | Helper functions for decoding terms to domain types

module Hydra.Extract.Helpers where

import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Decode an Either value using the provided left and right decoders
decodeEither :: ((Graph.Graph -> Core.Term -> Either Error.DecodingError t0) -> (Graph.Graph -> Core.Term -> Either Error.DecodingError t1) -> Graph.Graph -> Core.Term -> Either Error.DecodingError (Either t0 t1))
decodeEither leftDecoder rightDecoder g term = (Eithers.bind (Eithers.bimap (\x -> Error.DecodingError x) (\x -> x) (Lexical.stripAndDereferenceTermEither g term)) (\stripped -> (\x -> case x of
  Core.TermEither v1 -> (Eithers.either (\lv -> Eithers.map (\x -> Left x) (leftDecoder g lv)) (\rv -> Eithers.map (\x -> Right x) (rightDecoder g rv)) v1)
  _ -> (Left (Error.DecodingError "expected either value"))) stripped))

-- | Decode a list of elements using the provided element decoder
decodeList :: ((Graph.Graph -> Core.Term -> Either Error.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Error.DecodingError [t0])
decodeList elemDecoder g term = (Eithers.bind (Eithers.bimap (\x -> Error.DecodingError x) (\x -> x) (Lexical.stripAndDereferenceTermEither g term)) (\stripped -> (\x -> case x of
  Core.TermList v1 -> (Eithers.mapList (elemDecoder g) v1)
  _ -> (Left (Error.DecodingError "expected list"))) stripped))

-- | Decode a Map using the provided key and value decoders
decodeMap :: Ord t0 => ((Graph.Graph -> Core.Term -> Either Error.DecodingError t0) -> (Graph.Graph -> Core.Term -> Either Error.DecodingError t1) -> Graph.Graph -> Core.Term -> Either Error.DecodingError (M.Map t0 t1))
decodeMap keyDecoder valDecoder g term = (Eithers.bind (Eithers.bimap (\x -> Error.DecodingError x) (\x -> x) (Lexical.stripAndDereferenceTermEither g term)) (\stripped -> (\x -> case x of
  Core.TermMap v1 -> (Eithers.map Maps.fromList (Eithers.mapList (\kv -> Eithers.bind (keyDecoder g (Pairs.first kv)) (\k -> Eithers.map (\v -> (k, v)) (valDecoder g (Pairs.second kv)))) (Maps.toList v1)))
  _ -> (Left (Error.DecodingError "expected map"))) stripped))

-- | Decode a Maybe value using the provided element decoder
decodeMaybe :: ((Graph.Graph -> Core.Term -> Either Error.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Error.DecodingError (Maybe t0))
decodeMaybe elemDecoder g term = (Eithers.bind (Eithers.bimap (\x -> Error.DecodingError x) (\x -> x) (Lexical.stripAndDereferenceTermEither g term)) (\stripped -> (\x -> case x of
  Core.TermMaybe v1 -> (Eithers.mapMaybe (elemDecoder g) v1)
  _ -> (Left (Error.DecodingError "expected optional value"))) stripped))

-- | Decode a Pair using the provided first and second decoders
decodePair :: ((Graph.Graph -> Core.Term -> Either Error.DecodingError t0) -> (Graph.Graph -> Core.Term -> Either Error.DecodingError t1) -> Graph.Graph -> Core.Term -> Either Error.DecodingError (t0, t1))
decodePair firstDecoder secondDecoder g term = (Eithers.bind (Eithers.bimap (\x -> Error.DecodingError x) (\x -> x) (Lexical.stripAndDereferenceTermEither g term)) (\stripped -> (\x -> case x of
  Core.TermPair v1 -> (Eithers.bind (firstDecoder g (Pairs.first v1)) (\f -> Eithers.map (\s -> (f, s)) (secondDecoder g (Pairs.second v1))))
  _ -> (Left (Error.DecodingError "expected pair"))) stripped))

-- | Decode a Set using the provided element decoder
decodeSet :: Ord t0 => ((Graph.Graph -> Core.Term -> Either Error.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Error.DecodingError (S.Set t0))
decodeSet elemDecoder g term = (Eithers.bind (Eithers.bimap (\x -> Error.DecodingError x) (\x -> x) (Lexical.stripAndDereferenceTermEither g term)) (\stripped -> (\x -> case x of
  Core.TermSet v1 -> (Eithers.map Sets.fromList (Eithers.mapList (elemDecoder g) (Sets.toList v1)))
  _ -> (Left (Error.DecodingError "expected set"))) stripped))

-- | Decode a unit value
decodeUnit :: (Graph.Graph -> Core.Term -> Either Error.DecodingError ())
decodeUnit g term = (Eithers.bind (Eithers.bimap (\x -> Error.DecodingError x) (\x -> x) (Lexical.stripAndDereferenceTermEither g term)) (\stripped -> (\x -> case x of
  Core.TermUnit -> (Right ())
  _ -> (Left (Error.DecodingError "expected a unit value"))) stripped))

-- | Decode a wrapped value using the provided body decoder
decodeWrapped :: ((Graph.Graph -> Core.Term -> Either Error.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Error.DecodingError t0)
decodeWrapped bodyDecoder g term = (Eithers.bind (Eithers.bimap (\x -> Error.DecodingError x) (\x -> x) (Lexical.stripAndDereferenceTermEither g term)) (\stripped -> (\x -> case x of
  Core.TermWrap v1 -> (bodyDecoder g (Core.wrappedTermBody v1))
  _ -> (Left (Error.DecodingError "expected wrapped value"))) stripped))

-- | Require a field from a record's field map and decode it
requireField :: (String -> (t0 -> t1 -> Either Error.DecodingError t2) -> M.Map Core.Name t1 -> t0 -> Either Error.DecodingError t2)
requireField fieldName decoder fieldMap g = (Maybes.maybe (Left (Error.DecodingError (Strings.cat [
  "missing field ",
  fieldName,
  " in record"]))) (\fieldTerm -> decoder g fieldTerm) (Maps.lookup (Core.Name fieldName) fieldMap))

-- | Convert a Record's fields to a Map from Name to Term
toFieldMap :: (Core.Record -> M.Map Core.Name Core.Term)
toFieldMap record = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields record)))
