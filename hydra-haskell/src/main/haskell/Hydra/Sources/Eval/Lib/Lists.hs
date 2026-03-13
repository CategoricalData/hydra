
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

import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Meta.Error        as Error
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore

ns :: Namespace
ns = Namespace "hydra.eval.lib.lists"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [ExtractCore.ns, ShowCore.ns]
    kernelTypesNamespaces $
    Just ("Evaluation-level implementations of List functions for the Hydra interpreter.")
  where
    elements = [
      toBinding apply_,
      toBinding bind_,
      toBinding dropWhile_,
      toBinding filter_,
      toBinding find_,
      toBinding foldl_,
      toBinding foldr_,
      toBinding map_,
      toBinding partition_,
      toBinding sortOn_,
      toBinding span_,
      toBinding zipWith_]

-- | Interpreter-friendly applicative apply for List terms.
-- Applies each function in funsTerm to each argument in argsTerm.
apply_ :: TBinding (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)
apply_ = define "apply" $
  doc "Interpreter-friendly applicative apply for List terms." $
  "cx" ~> "g" ~>
  "funsTerm" ~> "argsTerm" ~>
  "funs" <<~ (ExtractCore.list @@ var "cx" @@ var "g" @@ var "funsTerm") $
  "arguments" <<~ (ExtractCore.list @@ var "cx" @@ var "g" @@ var "argsTerm") $
  "applyOne" <~ ("f" ~> Lists.map
    ("arg" ~> Core.termApplication $ Core.application (var "f") (var "arg"))
    (var "arguments")) $
  right $ Core.termList $ Lists.concat $ Lists.map (var "applyOne") (var "funs")

-- | Interpreter-friendly monadic bind for List terms.
-- Applies funTerm to each element and concatenates the results.
bind_ :: TBinding (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)
bind_ = define "bind" $
  doc "Interpreter-friendly monadic bind for List terms." $
  "cx" ~> "g" ~>
  "listTerm" ~> "funTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "cx" @@ var "g" @@ var "listTerm") $
  right $ Core.termApplication $ Core.application
    (Core.termFunction $ Core.functionPrimitive $ encodedName _lists_concat)
    (Core.termList $ Lists.map
      ("el" ~> Core.termApplication $ Core.application (var "funTerm") (var "el"))
      (var "elements"))

-- | Interpreter-friendly dropWhile for List terms.
-- Drops elements from the front while predTerm returns true.
dropWhile_ :: TBinding (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)
dropWhile_ = define "dropWhile" $
  doc "Interpreter-friendly dropWhile for List terms." $
  "cx" ~> "g" ~>
  "predTerm" ~> "listTerm" ~>
  -- Build: snd (span predTerm listTerm) - delegate to span primitive
  right $ Core.termApplication $ Core.application
    (Core.termFunction $ Core.functionPrimitive $ encodedName _pairs_second)
    (Core.termApplication $ Core.application
      (Core.termApplication $ Core.application
        (Core.termFunction $ Core.functionPrimitive $ encodedName _lists_span)
        (var "predTerm"))
      (var "listTerm"))

-- | Interpreter-friendly filter for List terms.
-- Keeps elements where predTerm returns true.
filter_ :: TBinding (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)
filter_ = define "filter" $
  doc "Interpreter-friendly filter for List terms." $
  "cx" ~> "g" ~>
  "predTerm" ~> "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "cx" @@ var "g" @@ var "listTerm") $
  -- Build: concat (map (\el -> ifElse (pred el) [el] []) elements)
  right $ Core.termApplication $ Core.application
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

-- | Interpreter-friendly find for List terms.
-- Returns the first element where predTerm returns true, or Nothing if none found.
find_ :: TBinding (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)
find_ = define "find" $
  doc "Interpreter-friendly find for List terms." $
  "cx" ~> "g" ~>
  "predTerm" ~> "listTerm" ~>
  -- Build: safeHead (filter predTerm listTerm) - delegate to filter and safeHead
  right $ Core.termApplication $ Core.application
    (Core.termFunction $ Core.functionPrimitive $ encodedName _lists_safeHead)
    (Core.termApplication $ Core.application
      (Core.termApplication $ Core.application
        (Core.termFunction $ Core.functionPrimitive $ encodedName _lists_filter)
        (var "predTerm"))
      (var "listTerm"))

-- | Interpreter-friendly left fold for List terms.
-- Folds from the left: foldl f init [e1,e2,e3] = f (f (f init e1) e2) e3
foldl_ :: TBinding (Context -> Graph -> Term -> Term -> Term -> Either (InContext Error) Term)
foldl_ = define "foldl" $
  doc "Interpreter-friendly left fold for List terms." $
  "cx" ~> "g" ~>
  "funTerm" ~> "initTerm" ~> "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "cx" @@ var "g" @@ var "listTerm") $
  -- Build nested applications: f (f (f init e1) e2) e3
  right $ Lists.foldl
    ("acc" ~> "el" ~> Core.termApplication $ Core.application
      (Core.termApplication $ Core.application (var "funTerm") (var "acc"))
      (var "el"))
    (var "initTerm")
    (var "elements")

-- | Interpreter-friendly right fold for List terms.
-- Folds from the right: foldr f init [e1,e2,e3] = f e1 (f e2 (f e3 init))
foldr_ :: TBinding (Context -> Graph -> Term -> Term -> Term -> Either (InContext OtherError) Term)
foldr_ = define "foldr" $
  doc "Interpreter-friendly right fold for List terms." $
  "cx" ~> "g" ~>
  "funTerm" ~> "initTerm" ~> "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "cx" @@ var "g" @@ var "listTerm") $
  -- Build nested applications: f e1 (f e2 (f e3 init))
  right $ Lists.foldr
    ("el" ~> "acc" ~> Core.termApplication $ Core.application
      (Core.termApplication $ Core.application (var "funTerm") (var "el"))
      (var "acc"))
    (var "initTerm")
    (var "elements")

-- | Interpreter-friendly map for List terms.
-- Applies funTerm to each element of listTerm.
-- Note: builds result directly using foldl to avoid recursive primitive calls.
map_ :: TBinding (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)
map_ = define "map" $
  doc "Interpreter-friendly map for List terms." $
  "cx" ~> "g" ~>
  "funTerm" ~> "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "cx" @@ var "g" @@ var "listTerm") $
  -- Build the mapped list by folding over elements and accumulating applications
  -- This avoids calling lists.map recursively
  right $ Core.termList $ Lists.reverse $ Lists.foldl
    ("acc" ~> "el" ~> Lists.cons
      (Core.termApplication $ Core.application (var "funTerm") (var "el"))
      (var "acc"))
    (list ([] :: [TTerm Term]))
    (var "elements")

-- | Interpreter-friendly partition for List terms.
-- Partitions elements into (satisfying predicate, not satisfying predicate).
-- Unlike span, partition checks ALL elements, not just the prefix.
partition_ :: TBinding (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)
partition_ = define "partition" $
  doc "Interpreter-friendly partition for List terms." $
  "cx" ~> "g" ~>
  "predTerm" ~> "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "cx" @@ var "g" @@ var "listTerm") $
  -- State: (yeses, nos) - two accumulators
  -- Initial: ([], [])
  -- Step: ifElse (pred el) (append yeses [el], nos) (yeses, append nos [el])
  -- Result: (yeses, nos) - already in correct order due to foldl + concat2
  "initialState" <~ (Core.termPair $ pair
    (Core.termList $ list ([] :: [TTerm Term]))
    (Core.termList $ list ([] :: [TTerm Term]))) $
  "finalState" <~ (Lists.foldl
    ("acc" ~> "el" ~>
      -- Extract state components
      "yeses" <~ (Core.termApplication $ Core.application
        (Core.termFunction $ Core.functionPrimitive $ encodedName _pairs_first)
        (var "acc")) $
      "nos" <~ (Core.termApplication $ Core.application
        (Core.termFunction $ Core.functionPrimitive $ encodedName _pairs_second)
        (var "acc")) $
      -- Build ifElse (pred el) trueCase falseCase
      Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termApplication $ Core.application
            (Core.termFunction $ Core.functionPrimitive $ encodedName _logic_ifElse)
            -- condition: pred el
            (Core.termApplication $ Core.application (var "predTerm") (var "el")))
          -- true branch: (append yeses [el], nos)
          (Core.termPair $ pair
            (Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termFunction $ Core.functionPrimitive $ encodedName _lists_concat2)
                (var "yeses"))
              (Core.termList $ list [var "el"]))
            (var "nos")))
        -- false branch: (yeses, append nos [el])
        (Core.termPair $ pair
          (var "yeses")
          (Core.termApplication $ Core.application
            (Core.termApplication $ Core.application
              (Core.termFunction $ Core.functionPrimitive $ encodedName _lists_concat2)
              (var "nos"))
            (Core.termList $ list [var "el"]))))
    (var "initialState")
    (var "elements")) $
  -- Return the final state directly (it's already a pair)
  right $ var "finalState"

-- | Interpreter-friendly sortOn for List terms.
-- Sorts elements by comparing the results of applying projTerm to each.
-- Uses insertion sort: for each element, use span to find insertion point.
sortOn_ :: TBinding (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)
sortOn_ = define "sortOn" $
  doc "Interpreter-friendly sortOn for List terms." $
  "cx" ~> "g" ~>
  "projTerm" ~> "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "cx" @@ var "g" @@ var "listTerm") $
  -- Build: foldl (\sorted x -> insert x sorted) [] elements
  -- where insert x sorted = let (before, after) = span (\y -> lte (proj y) (proj x)) sorted
  --                         in concat [before, [x], after]
  right $ Lists.foldl
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
span_ :: TBinding (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)
span_ = define "span" $
  doc "Interpreter-friendly span for List terms." $
  "cx" ~> "g" ~>
  "predTerm" ~> "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "cx" @@ var "g" @@ var "listTerm") $
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
  right $ Core.termPair $ pair
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
zipWith_ :: TBinding (Context -> Graph -> Term -> Term -> Term -> Either (InContext Error) Term)
zipWith_ = define "zipWith" $
  doc "Interpreter-friendly zipWith for List terms." $
  "cx" ~> "g" ~>
  "funTerm" ~> "listTerm1" ~> "listTerm2" ~>
  "elements1" <<~ (ExtractCore.list @@ var "cx" @@ var "g" @@ var "listTerm1") $
  "elements2" <<~ (ExtractCore.list @@ var "cx" @@ var "g" @@ var "listTerm2") $
  -- Build: [f a1 b1, f a2 b2, ...]
  right $ Core.termList $ Lists.map
    ("p" ~>
      "a" <~ Pairs.first (var "p") $
      "b" <~ Pairs.second (var "p") $
      Core.termApplication $ Core.application
        (Core.termApplication $ Core.application (var "funTerm") (var "a"))
        (var "b"))
    (Lists.zip (var "elements1") (var "elements2"))
