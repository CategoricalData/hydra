
module Hydra.Sources.Eval.Lib.Lists where

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
import qualified Hydra.Dsl.Meta.Literals as MetaLiterals
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
ns = Namespace "hydra.eval.lib.lists"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [ExtractCore.ns, Monads.ns, ShowCore.ns]
    kernelTypesNamespaces $
    Just ("Evaluation-level implementations of List functions for the Hydra interpreter.")
  where
    elements = [
      toBinding apply_,
      toBinding bind_,
      toBinding dropWhile_,
      toBinding filter_,
      toBinding foldl_,
      toBinding map_,
      toBinding sortOn_,
      toBinding span_,
      toBinding zipWith_]

-- | Interpreter-friendly applicative apply for List terms.
-- Applies each function in funsTerm to each argument in argsTerm.
apply_ :: TBinding (Term -> Term -> Flow s Term)
apply_ = define "apply" $
  doc "Interpreter-friendly applicative apply for List terms." $
  "funsTerm" ~> "argsTerm" ~>
  "funs" <<~ ExtractCore.list @@ var "funsTerm" $
  "arguments" <<~ ExtractCore.list @@ var "argsTerm" $
  "applyOne" <~ ("f" ~> Lists.map
    ("arg" ~> Core.termApplication $ Core.application (var "f") (var "arg"))
    (var "arguments")) $
  produce $ Core.termList $ Lists.concat $ Lists.map (var "applyOne") (var "funs")

-- | Interpreter-friendly monadic bind for List terms.
-- Applies funTerm to each element and concatenates the results.
bind_ :: TBinding (Term -> Term -> Flow s Term)
bind_ = define "bind" $
  doc "Interpreter-friendly monadic bind for List terms." $
  "listTerm" ~> "funTerm" ~>
  "elements" <<~ ExtractCore.list @@ var "listTerm" $
  produce $ Core.termApplication $ Core.application
    (Core.termFunction $ Core.functionPrimitive $ encodedName _lists_concat)
    (Core.termList $ Lists.map
      ("el" ~> Core.termApplication $ Core.application (var "funTerm") (var "el"))
      (var "elements"))

-- | Interpreter-friendly dropWhile for List terms.
-- Drops elements from the front while predTerm returns true.
dropWhile_ :: TBinding (Term -> Term -> Flow s Term)
dropWhile_ = define "dropWhile" $
  doc "Interpreter-friendly dropWhile for List terms." $
  "predTerm" ~> "listTerm" ~>
  -- Build: snd (span predTerm listTerm) - delegate to span primitive
  produce $ Core.termApplication $ Core.application
    (Core.termFunction $ Core.functionPrimitive $ encodedName _pairs_second)
    (Core.termApplication $ Core.application
      (Core.termApplication $ Core.application
        (Core.termFunction $ Core.functionPrimitive $ encodedName _lists_span)
        (var "predTerm"))
      (var "listTerm"))

-- | Interpreter-friendly filter for List terms.
-- Keeps elements where predTerm returns true.
filter_ :: TBinding (Term -> Term -> Flow s Term)
filter_ = define "filter" $
  doc "Interpreter-friendly filter for List terms." $
  "predTerm" ~> "listTerm" ~>
  "elements" <<~ ExtractCore.list @@ var "listTerm" $
  -- Build: concat (map (\el -> ifElse (pred el) [el] []) elements)
  produce $ Core.termApplication $ Core.application
    (Core.termFunction $ Core.functionPrimitive $ encodedName _lists_concat)
    (Core.termList $ Lists.map
      ("el" ~> Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termApplication $ Core.application
            (Core.termFunction $ Core.functionPrimitive $ encodedName _logic_ifElse)
            (Core.termApplication $ Core.application (var "predTerm") (var "el")))
          (Core.termList $ Lists.pure (var "el")))
        (Core.termList $ list ([] :: [TTerm Term])))
      (var "elements"))

-- | Interpreter-friendly left fold for List terms.
-- Folds from the left: foldl f init [e1,e2,e3] = f (f (f init e1) e2) e3
foldl_ :: TBinding (Term -> Term -> Term -> Flow s Term)
foldl_ = define "foldl" $
  doc "Interpreter-friendly left fold for List terms." $
  "funTerm" ~> "initTerm" ~> "listTerm" ~>
  "elements" <<~ ExtractCore.list @@ var "listTerm" $
  -- Build nested applications: f (f (f init e1) e2) e3
  produce $ Lists.foldl
    ("acc" ~> "el" ~> Core.termApplication $ Core.application
      (Core.termApplication $ Core.application (var "funTerm") (var "acc"))
      (var "el"))
    (var "initTerm")
    (var "elements")

-- | Interpreter-friendly map for List terms.
-- Applies funTerm to each element of listTerm.
-- Note: builds result directly using foldl to avoid recursive primitive calls.
map_ :: TBinding (Term -> Term -> Flow s Term)
map_ = define "map" $
  doc "Interpreter-friendly map for List terms." $
  "funTerm" ~> "listTerm" ~>
  "elements" <<~ ExtractCore.list @@ var "listTerm" $
  -- Build the mapped list by folding over elements and accumulating applications
  -- This avoids calling lists.map recursively
  produce $ Core.termList $ Lists.reverse $ Lists.foldl
    ("acc" ~> "el" ~> Lists.cons
      (Core.termApplication $ Core.application (var "funTerm") (var "el"))
      (var "acc"))
    (list ([] :: [TTerm Term]))
    (var "elements")

-- | Interpreter-friendly sortOn for List terms.
-- Sorts elements by comparing the results of applying projTerm to each.
-- Uses insertion sort: for each element, use span to find insertion point.
sortOn_ :: TBinding (Term -> Term -> Flow s Term)
sortOn_ = define "sortOn" $
  doc "Interpreter-friendly sortOn for List terms." $
  "projTerm" ~> "listTerm" ~>
  "elements" <<~ ExtractCore.list @@ var "listTerm" $
  -- Build: foldl (\sorted x -> insert x sorted) [] elements
  -- where insert x sorted = let (before, after) = span (\y -> lte (proj y) (proj x)) sorted
  --                         in concat [before, [x], after]
  produce $ Lists.foldl
    ("sorted" ~> "x" ~>
      -- Build the split using span with predicate: \y -> lte (proj y) (proj x)
      "splitResult" <~ (Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termFunction $ Core.functionPrimitive $ encodedName _lists_span)
          -- predicate lambda: \y -> lte (proj y) (proj x) -- use lte for stable sort
          (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "y") nothing $
            Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termFunction $ Core.functionPrimitive $ encodedName _equality_lte)
                (Core.termApplication $ Core.application
                  (var "projTerm")
                  (Core.termVariable $ wrap _Name $ string "y")))
              (Core.termApplication $ Core.application (var "projTerm") (var "x"))))
        (var "sorted")) $
      -- Build: concat [before, [x], after]
      "before" <~ (Core.termApplication $ Core.application
        (Core.termFunction $ Core.functionPrimitive $ encodedName _pairs_first)
        (var "splitResult")) $
      "after" <~ (Core.termApplication $ Core.application
        (Core.termFunction $ Core.functionPrimitive $ encodedName _pairs_second)
        (var "splitResult")) $
      Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termFunction $ Core.functionPrimitive $ encodedName _lists_concat2)
          (var "before"))
        (Core.termApplication $ Core.application
          (Core.termApplication $ Core.application
            (Core.termFunction $ Core.functionPrimitive $ encodedName _lists_cons)
            (var "x"))
          (var "after")))
    (Core.termList $ list ([] :: [TTerm Term]))
    (var "elements")

-- | Interpreter-friendly span for List terms.
-- Splits the list into (takeWhile pred list, dropWhile pred list).
-- Uses foldl with state ((stillTaking, left), right) to track the split point.
span_ :: TBinding (Term -> Term -> Flow s Term)
span_ = define "span" $
  doc "Interpreter-friendly span for List terms." $
  "predTerm" ~> "listTerm" ~>
  "elements" <<~ ExtractCore.list @@ var "listTerm" $
  -- State: ((taking, left), right) as nested pairs
  -- Initial: ((true, []), [])
  -- Step: ifElse (and taking (pred el))
  --         ((true, append left [el]), right)
  --         ((false, left), append right [el])
  -- Result: (snd (fst result), snd result)
  "initialState" <~ (Core.termPair $ pair
    (Core.termPair $ pair
      (Core.termLiteral $ Core.literalBoolean $ MetaLiterals.boolean True)
      (Core.termList $ list ([] :: [TTerm Term])))
    (Core.termList $ list ([] :: [TTerm Term]))) $
  "finalState" <~ (Lists.foldl
    ("acc" ~> "el" ~>
      -- Extract state components using term-level pairs
      "takingLeft" <~ (Core.termApplication $ Core.application
        (Core.termFunction $ Core.functionPrimitive $ encodedName _pairs_first)
        (var "acc")) $
      "right" <~ (Core.termApplication $ Core.application
        (Core.termFunction $ Core.functionPrimitive $ encodedName _pairs_second)
        (var "acc")) $
      "taking" <~ (Core.termApplication $ Core.application
        (Core.termFunction $ Core.functionPrimitive $ encodedName _pairs_first)
        (var "takingLeft")) $
      "left" <~ (Core.termApplication $ Core.application
        (Core.termFunction $ Core.functionPrimitive $ encodedName _pairs_second)
        (var "takingLeft")) $
      -- Build ifElse (and taking (pred el)) trueCase falseCase
      Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termApplication $ Core.application
            (Core.termFunction $ Core.functionPrimitive $ encodedName _logic_ifElse)
            -- condition: and taking (pred el)
            (Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termFunction $ Core.functionPrimitive $ encodedName _logic_and)
                (var "taking"))
              (Core.termApplication $ Core.application (var "predTerm") (var "el"))))
          -- true branch: ((true, append left [el]), right)
          (Core.termPair $ pair
            (Core.termPair $ pair
              (Core.termLiteral $ Core.literalBoolean $ MetaLiterals.boolean True)
              (Core.termApplication $ Core.application
                (Core.termApplication $ Core.application
                  (Core.termFunction $ Core.functionPrimitive $ encodedName _lists_concat2)
                  (var "left"))
                (Core.termList $ list [var "el"])))
            (var "right")))
        -- false branch: ((false, left), append right [el])
        (Core.termPair $ pair
          (Core.termPair $ pair
            (Core.termLiteral $ Core.literalBoolean $ MetaLiterals.boolean False)
            (var "left"))
          (Core.termApplication $ Core.application
            (Core.termApplication $ Core.application
              (Core.termFunction $ Core.functionPrimitive $ encodedName _lists_concat2)
              (var "right"))
            (Core.termList $ list [var "el"]))))
    (var "initialState")
    (var "elements")) $
  -- Extract result: (snd (fst finalState), snd finalState)
  produce $ Core.termPair $ pair
    (Core.termApplication $ Core.application
      (Core.termFunction $ Core.functionPrimitive $ encodedName _pairs_second)
      (Core.termApplication $ Core.application
        (Core.termFunction $ Core.functionPrimitive $ encodedName _pairs_first)
        (var "finalState")))
    (Core.termApplication $ Core.application
      (Core.termFunction $ Core.functionPrimitive $ encodedName _pairs_second)
      (var "finalState"))

-- | Interpreter-friendly zipWith for List terms.
-- Applies funTerm to corresponding pairs of elements.
zipWith_ :: TBinding (Term -> Term -> Term -> Flow s Term)
zipWith_ = define "zipWith" $
  doc "Interpreter-friendly zipWith for List terms." $
  "funTerm" ~> "listTerm1" ~> "listTerm2" ~>
  "elements1" <<~ ExtractCore.list @@ var "listTerm1" $
  "elements2" <<~ ExtractCore.list @@ var "listTerm2" $
  -- Build: [f a1 b1, f a2 b2, ...]
  produce $ Core.termList $ Lists.map
    ("p" ~>
      "a" <~ Pairs.first (var "p") $
      "b" <~ Pairs.second (var "p") $
      Core.termApplication $ Core.application
        (Core.termApplication $ Core.application (var "funTerm") (var "a"))
        (var "b"))
    (Lists.zip (var "elements1") (var "elements2"))
