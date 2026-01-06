{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Kernel.Terms.Extract.Helpers where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (literalType, matchRecord, matchUnion)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
--import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Dsl.Meta.DeepCore as DC
import           Hydra.Dsl.Meta.DeepCore ((@@@))


ns :: Namespace
ns = Namespace "hydra.extract.helpers"

module_ :: Module
module_ = Module ns elements
    [Lexical.ns]
    kernelTypesNamespaces $
    Just "Helper functions for decoding terms to domain types"
  where
    elements = [
      toBinding decodeEither,
      toBinding decodeList,
      toBinding decodeMap,
      toBinding decodeMaybe,
      toBinding decodePair,
      toBinding decodeSet,
      toBinding decodeUnit,
      toBinding decodeWrapped,
      toBinding requireField,
      toBinding toFieldMap]

define :: String -> TTerm x -> TBinding x
define = definitionInModule module_

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- | Helper to convert Either String Term to Either DecodingError Term
stripWithDecodingError :: TTerm Graph -> TTerm Term -> TTerm (Either DecodingError Term)
stripWithDecodingError g term = Eithers.bimap
  (unaryFunction Util.decodingError)
  ("x" ~> var "x")
  (Lexical.stripAndDereferenceTermEither @@ g @@ term)

--------------------------------------------------------------------------------
-- Main decoder functions
--------------------------------------------------------------------------------

-- | Decode an Either value using the provided left and right decoders
decodeEither :: TBinding ((Graph -> Term -> Either DecodingError a) -> (Graph -> Term -> Either DecodingError b) -> Graph -> Term -> Either DecodingError (Either a b))
decodeEither = define "decodeEither" $
  doc "Decode an Either value using the provided left and right decoders" $
  "leftDecoder" ~> "rightDecoder" ~> "g" ~> "term" ~>
  Eithers.bind
    (stripWithDecodingError (var "g") (var "term"))
    ("stripped" ~> cases _Term (var "stripped")
      (Just $ left $ Util.decodingError $ string "expected either value") [
      _Term_either>>: "e" ~>
        Eithers.either_
          -- Left case: decode the left value, then wrap result in Left
          ("lv" ~> Eithers.map ("x" ~> left (var "x")) (var "leftDecoder" @@ var "g" @@ var "lv"))
          -- Right case: decode the right value, then wrap result in Right
          ("rv" ~> Eithers.map ("x" ~> right (var "x")) (var "rightDecoder" @@ var "g" @@ var "rv"))
          (var "e")])

-- | Decode a list of elements using the provided element decoder
decodeList :: TBinding ((Graph -> Term -> Either DecodingError a) -> Graph -> Term -> Either DecodingError [a])
decodeList = define "decodeList" $
  doc "Decode a list of elements using the provided element decoder" $
  "elemDecoder" ~> "g" ~> "term" ~>
  Eithers.bind
    (stripWithDecodingError (var "g") (var "term"))
    ("stripped" ~> cases _Term (var "stripped")
      (Just $ left $ Util.decodingError $ string "expected list") [
      _Term_list>>: "els" ~> Eithers.mapList (var "elemDecoder" @@ var "g") $ var "els"])

-- | Decode a Map using the provided key and value decoders
decodeMap :: TBinding ((Graph -> Term -> Either DecodingError k) -> (Graph -> Term -> Either DecodingError v) -> Graph -> Term -> Either DecodingError (M.Map k v))
decodeMap = define "decodeMap" $
  doc "Decode a Map using the provided key and value decoders" $
  "keyDecoder" ~> "valDecoder" ~> "g" ~> "term" ~>
  Eithers.bind
    (stripWithDecodingError (var "g") (var "term"))
    ("stripped" ~> cases _Term (var "stripped")
      (Just $ left $ Util.decodingError $ string "expected map") [
      _Term_map>>: "m" ~>
        Eithers.map (unaryFunction Maps.fromList)
          (Eithers.mapList
            ("kv" ~>
              Eithers.bind (var "keyDecoder" @@ var "g" @@ (Pairs.first $ var "kv"))
                ("k" ~> Eithers.map ("v" ~> pair (var "k") (var "v"))
                  (var "valDecoder" @@ var "g" @@ (Pairs.second $ var "kv"))))
            (Maps.toList $ var "m"))])

-- | Decode a Maybe value using the provided element decoder
decodeMaybe :: TBinding ((Graph -> Term -> Either DecodingError a) -> Graph -> Term -> Either DecodingError (Maybe a))
decodeMaybe = define "decodeMaybe" $
  doc "Decode a Maybe value using the provided element decoder" $
  "elemDecoder" ~> "g" ~> "term" ~>
  Eithers.bind
    (stripWithDecodingError (var "g") (var "term"))
    ("stripped" ~> cases _Term (var "stripped")
      (Just $ left $ Util.decodingError $ string "expected optional value") [
      _Term_maybe>>: "opt" ~> Eithers.mapMaybe (var "elemDecoder" @@ var "g") $ var "opt"])

-- | Decode a Pair using the provided first and second decoders
decodePair :: TBinding ((Graph -> Term -> Either DecodingError a) -> (Graph -> Term -> Either DecodingError b) -> Graph -> Term -> Either DecodingError (a, b))
decodePair = define "decodePair" $
  doc "Decode a Pair using the provided first and second decoders" $
  "firstDecoder" ~> "secondDecoder" ~> "g" ~> "term" ~>
  Eithers.bind
    (stripWithDecodingError (var "g") (var "term"))
    ("stripped" ~> cases _Term (var "stripped")
      (Just $ left $ Util.decodingError $ string "expected pair") [
      _Term_pair>>: "p" ~>
        Eithers.bind (var "firstDecoder" @@ var "g" @@ (Pairs.first $ var "p"))
          ("f" ~> Eithers.map ("s" ~> pair (var "f") (var "s"))
            (var "secondDecoder" @@ var "g" @@ (Pairs.second $ var "p")))])

-- | Decode a Set using the provided element decoder
decodeSet :: TBinding ((Graph -> Term -> Either DecodingError a) -> Graph -> Term -> Either DecodingError (S.Set a))
decodeSet = define "decodeSet" $
  doc "Decode a Set using the provided element decoder" $
  "elemDecoder" ~> "g" ~> "term" ~>
  Eithers.bind
    (stripWithDecodingError (var "g") (var "term"))
    ("stripped" ~> cases _Term (var "stripped")
      (Just $ left $ Util.decodingError $ string "expected set") [
      _Term_set>>: "s" ~>
        Eithers.map (unaryFunction Sets.fromList)
          (Eithers.mapList (var "elemDecoder" @@ var "g") (Sets.toList $ var "s"))])

-- | Decode a unit value
decodeUnit :: TBinding (Graph -> Term -> Either DecodingError ())
decodeUnit = define "decodeUnit" $
  doc "Decode a unit value" $
  "g" ~> "term" ~>
  Eithers.bind
    (stripWithDecodingError (var "g") (var "term"))
    ("stripped" ~> cases _Term (var "stripped")
      (Just $ left $ Util.decodingError $ string "expected a unit value") [
      _Term_unit>>: constant $ right unit])

-- | Decode a wrapped value using the provided body decoder
decodeWrapped :: TBinding ((Graph -> Term -> Either DecodingError a) -> Graph -> Term -> Either DecodingError a)
decodeWrapped = define "decodeWrapped" $
  doc "Decode a wrapped value using the provided body decoder" $
  "bodyDecoder" ~> "g" ~> "term" ~>
  Eithers.bind
    (stripWithDecodingError (var "g") (var "term"))
    ("stripped" ~> cases _Term (var "stripped")
      (Just $ left $ Util.decodingError $ string "expected wrapped value") [
      _Term_wrap>>: "wt" ~>
        var "bodyDecoder" @@ var "g" @@ (Core.wrappedTermBody (var "wt"))])

-- | Require a field from a field map and decode it using the provided decoder
-- Returns Left with a "missing field" error if the field is not present
requireField :: TBinding (String -> (Graph -> Term -> Either DecodingError a) -> M.Map Name Term -> Graph -> Either DecodingError a)
requireField = define "requireField" $
  doc "Require a field from a record's field map and decode it" $
  "fieldName" ~> "decoder" ~> "fieldMap" ~> "g" ~>
  Maybes.maybe
    (left $ Util.decodingError $ Strings.cat $ list [string "missing field ", var "fieldName", string " in record"])
    ("fieldTerm" ~> var "decoder" @@ var "g" @@ var "fieldTerm")
    (Maps.lookup (Phantoms.wrap _Name $ var "fieldName") $ var "fieldMap")

-- | Convert a Record to a Map from field Name to Term
toFieldMap :: TBinding (Record -> M.Map Name Term)
toFieldMap = define "toFieldMap" $
  doc "Convert a Record's fields to a Map from Name to Term" $
  "record" ~>
  Maps.fromList $
    Lists.map
      ("f" ~> pair (Core.fieldName $ var "f") (Core.fieldTerm $ var "f"))
      (Core.recordFields $ var "record")
