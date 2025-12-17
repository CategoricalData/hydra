
module Hydra.Sources.Eval.Lib.Maps where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (map)
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
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), map)
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore


ns :: Namespace
ns = Namespace "hydra.eval.lib.maps"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [ExtractCore.module_, Monads.module_, ShowCore.module_]
    kernelTypesModules $
    Just ("Evaluation-level implementations of Map functions for the Hydra interpreter.")
  where
    elements = [
      toBinding alter_,
      toBinding bimap_,
      toBinding filter_,
      toBinding filterWithKey_,
      toBinding map_,
      toBinding mapKeys_]

-- | Interpreter-friendly alter for Map terms.
-- Applies funTerm to the current value (or Nothing) and updates accordingly.
alter_ :: TBinding (Term -> Term -> Term -> Flow s Term)
alter_ = define "alter" $
  doc "Interpreter-friendly alter for Map terms." $
  "funTerm" ~> "keyTerm" ~> "mapTerm" ~>
  cases _Term (var "mapTerm")
    (Just (Monads.unexpected @@ string "map value" @@ (ShowCore.term @@ var "mapTerm"))) [
    _Term_map>>: "m" ~>
      -- Get current value: lookup key m
      "currentVal" <~ Maps.lookup (var "keyTerm") (var "m") $
      -- Apply function to get new value: funTerm (Just v) or funTerm Nothing
      "newVal" <~ Core.termApplication (Core.application
        (var "funTerm")
        (Core.termMaybe (var "currentVal"))) $
      -- Result depends on newVal:
      -- If newVal is Nothing, delete the key
      -- If newVal is Just v', insert/update with v'
      produce $ Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termApplication $ Core.application
            (Core.termFunction $ Core.functionPrimitive $ wrap _Name $ string "hydra.lib.maybes.maybe")
            -- default: delete key from map
            (Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termFunction $ Core.functionPrimitive $ wrap _Name $ string "hydra.lib.maps.delete")
                (var "keyTerm"))
              (var "mapTerm")))
          -- function: insert new value (as a term-level lambda)
          (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "newV") nothing $
            Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termApplication $ Core.application
                  (Core.termFunction $ Core.functionPrimitive $ wrap _Name $ string "hydra.lib.maps.insert")
                  (var "keyTerm"))
                (Core.termVariable $ wrap _Name $ string "newV"))
              (var "mapTerm")))
        (var "newVal")]

-- | Interpreter-friendly bimap for Map terms.
-- Applies keyFun to each key and valFun to each value.
bimap_ :: TBinding (Term -> Term -> Term -> Flow s Term)
bimap_ = define "bimap" $
  doc "Interpreter-friendly bimap for Map terms." $
  "keyFun" ~> "valFun" ~> "mapTerm" ~>
  cases _Term (var "mapTerm")
    (Just (Monads.unexpected @@ string "map value" @@ (ShowCore.term @@ var "mapTerm"))) [
    _Term_map>>: "m" ~>
      -- m is Map Term Term, convert to list of pairs
      "pairs" <~ Maps.toList (var "m") $
      -- Build: fromList (map (\(k,v) -> (keyFun k, valFun v)) pairs)
      produce $ Core.termMap $ Maps.fromList $ Lists.map
        ("p" ~>
          "k" <~ Pairs.first (var "p") $
          "v" <~ Pairs.second (var "p") $
          pair
            (Core.termApplication $ Core.application (var "keyFun") (var "k"))
            (Core.termApplication $ Core.application (var "valFun") (var "v")))
        (var "pairs")]

-- | Interpreter-friendly filter for Map terms.
-- Keeps entries where valPred returns true for the value.
filter_ :: TBinding (Term -> Term -> Flow s Term)
filter_ = define "filter" $
  doc "Interpreter-friendly filter for Map terms." $
  "valPred" ~> "mapTerm" ~>
  cases _Term (var "mapTerm")
    (Just (Monads.unexpected @@ string "map value" @@ (ShowCore.term @@ var "mapTerm"))) [
    _Term_map>>: "m" ~>
      "pairs" <~ Maps.toList (var "m") $
      -- Build: fromList (concat (map (\(k,v) -> if valPred v then [(k,v)] else []) pairs))
      produce $ Core.termApplication $ Core.application
        (Core.termFunction $ Core.functionPrimitive $ wrap _Name $ string "hydra.lib.maps.fromList")
        (Core.termApplication $ Core.application
          (Core.termFunction $ Core.functionPrimitive $ wrap _Name $ string "hydra.lib.lists.concat")
          (Core.termList $ Lists.map
            ("p" ~>
              "v" <~ Pairs.second (var "p") $
              Core.termApplication $ Core.application
                (Core.termApplication $ Core.application
                  (Core.termApplication $ Core.application
                    (Core.termFunction $ Core.functionPrimitive $ wrap _Name $ string "hydra.lib.logic.ifElse")
                    (Core.termApplication $ Core.application (var "valPred") (var "v")))
                  (Core.termList $ Lists.pure $ Core.termPair $ pair (Pairs.first $ var "p") (var "v")))
                (Core.termList $ list ([] :: [TTerm Term])))
            (var "pairs")))]

-- | Interpreter-friendly filterWithKey for Map terms.
-- Keeps entries where pred returns true for the key and value.
filterWithKey_ :: TBinding (Term -> Term -> Flow s Term)
filterWithKey_ = define "filterWithKey" $
  doc "Interpreter-friendly filterWithKey for Map terms." $
  "pred" ~> "mapTerm" ~>
  cases _Term (var "mapTerm")
    (Just (Monads.unexpected @@ string "map value" @@ (ShowCore.term @@ var "mapTerm"))) [
    _Term_map>>: "m" ~>
      "pairs" <~ Maps.toList (var "m") $
      -- Build: fromList (concat (map (\(k,v) -> if pred k v then [(k,v)] else []) pairs))
      produce $ Core.termApplication $ Core.application
        (Core.termFunction $ Core.functionPrimitive $ wrap _Name $ string "hydra.lib.maps.fromList")
        (Core.termApplication $ Core.application
          (Core.termFunction $ Core.functionPrimitive $ wrap _Name $ string "hydra.lib.lists.concat")
          (Core.termList $ Lists.map
            ("p" ~>
              "k" <~ Pairs.first (var "p") $
              "v" <~ Pairs.second (var "p") $
              Core.termApplication $ Core.application
                (Core.termApplication $ Core.application
                  (Core.termApplication $ Core.application
                    (Core.termFunction $ Core.functionPrimitive $ wrap _Name $ string "hydra.lib.logic.ifElse")
                    (Core.termApplication $ Core.application
                      (Core.termApplication $ Core.application (var "pred") (var "k"))
                      (var "v")))
                  (Core.termList $ Lists.pure $ Core.termPair $ pair (var "k") (var "v")))
                (Core.termList $ list ([] :: [TTerm Term])))
            (var "pairs")))]

-- | Interpreter-friendly map for Map terms.
-- Applies valFun to each value.
map_ :: TBinding (Term -> Term -> Flow s Term)
map_ = define "map" $
  doc "Interpreter-friendly map for Map terms." $
  "valFun" ~> "mapTerm" ~>
  cases _Term (var "mapTerm")
    (Just (Monads.unexpected @@ string "map value" @@ (ShowCore.term @@ var "mapTerm"))) [
    _Term_map>>: "m" ~>
      "pairs" <~ Maps.toList (var "m") $
      -- Build: fromList (map (\(k,v) -> (k, valFun v)) pairs)
      produce $ Core.termMap $ Maps.fromList $ Lists.map
        ("p" ~>
          "k" <~ Pairs.first (var "p") $
          "v" <~ Pairs.second (var "p") $
          pair (var "k") (Core.termApplication $ Core.application (var "valFun") (var "v")))
        (var "pairs")]

-- | Interpreter-friendly mapKeys for Map terms.
-- Applies keyFun to each key.
mapKeys_ :: TBinding (Term -> Term -> Flow s Term)
mapKeys_ = define "mapKeys" $
  doc "Interpreter-friendly mapKeys for Map terms." $
  "keyFun" ~> "mapTerm" ~>
  cases _Term (var "mapTerm")
    (Just (Monads.unexpected @@ string "map value" @@ (ShowCore.term @@ var "mapTerm"))) [
    _Term_map>>: "m" ~>
      "pairs" <~ Maps.toList (var "m") $
      -- Build: fromList (map (\(k,v) -> (keyFun k, v)) pairs)
      produce $ Core.termMap $ Maps.fromList $ Lists.map
        ("p" ~>
          "k" <~ Pairs.first (var "p") $
          "v" <~ Pairs.second (var "p") $
          pair (Core.termApplication $ Core.application (var "keyFun") (var "k")) (var "v"))
        (var "pairs")]
