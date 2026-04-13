
module Hydra.Sources.Eval.Lib.Lists where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (map)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths     as Paths
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Util       as Util
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Json.Model          as Json
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
import qualified Hydra.Dsl.Packaging        as Packaging
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Meta.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import qualified Hydra.Dsl.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), map)
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Errors       as Error
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Reduction as Reduction
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore

ns :: Namespace
ns = Namespace "hydra.eval.lib.lists"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns definitions
    [ExtractCore.ns, Reduction.ns, ShowCore.ns]
    kernelTypesNamespaces $
    Just ("Evaluation-level implementations of List functions for the Hydra interpreter.")
  where
    definitions = [
      toDefinition apply_,
      toDefinition bind_,
      toDefinition concat2_,
      toDefinition dropWhile_,
      toDefinition elem_,
      toDefinition filter_,
      toDefinition find_,
      toDefinition foldl_,
      toDefinition foldr_,
      toDefinition group_,
      toDefinition intercalate_,
      toDefinition intersperse_,
      toDefinition map_,
      toDefinition maybeHead_,
      toDefinition nub_,
      toDefinition partition_,
      toDefinition pure_,
      toDefinition replicate_,
      toDefinition safeHead_,
      toDefinition singleton_,
      toDefinition sort_,
      toDefinition sortOn_,
      toDefinition span_,
      toDefinition zipWith_]

-- | Interpreter-friendly applicative apply for List terms.
-- Applies each function in funsTerm to each argument in argsTerm.
apply_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either Error Term)
apply_ = define "apply" $
  doc "Interpreter-friendly applicative apply for List terms." $
  "cx" ~> "g" ~>
  "funsTerm" ~> "argsTerm" ~>
  "funs" <<~ (ExtractCore.list @@ var "g" @@ var "funsTerm") $
  "arguments" <<~ (ExtractCore.list @@ var "g" @@ var "argsTerm") $
  "applyOne" <~ ("f" ~> Lists.map
    ("arg" ~> Core.termApplication $ Core.application (var "f") (var "arg"))
    (var "arguments")) $
  right $ Core.termList $ Lists.concat $ Lists.map (var "applyOne") (var "funs")

-- | Interpreter-friendly monadic bind for List terms.
-- Applies funTerm to each element and concatenates the results.
bind_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either Error Term)
bind_ = define "bind" $
  doc "Interpreter-friendly monadic bind for List terms." $
  "cx" ~> "g" ~>
  "listTerm" ~> "funTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm") $
  right $ Core.termApplication $ Core.application
    (Core.termVariable $ encodedName _lists_concat)
    (Core.termList $ Lists.map
      ("el" ~> Core.termApplication $ Core.application (var "funTerm") (var "el"))
      (var "elements"))

-- | Interpreter-friendly concat2 for List terms.
-- Concatenates two lists.
concat2_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either Error Term)
concat2_ = define "concat2" $
  doc "Interpreter-friendly concat2 for List terms." $
  "cx" ~> "g" ~>
  "list1" ~> "list2" ~>
  -- Build: concat [list1, list2]
  right $ Core.termApplication $ Core.application
    (Core.termVariable $ encodedName _lists_concat)
    (Core.termList $ list [var "list1", var "list2"])

-- | Interpreter-friendly dropWhile for List terms.
-- Drops elements from the front while predTerm returns true.
dropWhile_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either Error Term)
dropWhile_ = define "dropWhile" $
  doc "Interpreter-friendly dropWhile for List terms." $
  "cx" ~> "g" ~>
  "predTerm" ~> "listTerm" ~>
  -- Build: snd (span predTerm listTerm) - delegate to span primitive
  right $ Core.termApplication $ Core.application
    (Core.termVariable $ encodedName _pairs_second)
    (Core.termApplication $ Core.application
      (Core.termApplication $ Core.application
        (Core.termVariable $ encodedName _lists_span)
        (var "predTerm"))
      (var "listTerm"))

-- | Interpreter-friendly filter for List terms.
-- Keeps elements where predTerm returns true.
filter_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either Error Term)
filter_ = define "filter" $
  doc "Interpreter-friendly filter for List terms." $
  "cx" ~> "g" ~>
  "predTerm" ~> "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm") $
  -- Build: concat (map (\el -> ifElse (pred el) [el] []) elements)
  right $ Core.termApplication $ Core.application
    (Core.termVariable $ encodedName _lists_concat)
    (Core.termList $ Lists.map
      ("el" ~> Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termApplication $ Core.application
            (Core.termVariable $ encodedName _logic_ifElse)
            (Core.termApplication $ Core.application (var "predTerm") (var "el")))
          (Core.termList $ Lists.pure (var "el")))
        (Core.termList $ list ([] :: [TTerm Term])))
      (var "elements"))

-- | Interpreter-friendly find for List terms.
-- Returns the first element where predTerm returns true, or Nothing if none found.
find_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either Error Term)
find_ = define "find" $
  doc "Interpreter-friendly find for List terms." $
  "cx" ~> "g" ~>
  "predTerm" ~> "listTerm" ~>
  -- Build: safeHead (filter predTerm listTerm) - delegate to filter and safeHead
  right $ Core.termApplication $ Core.application
    (Core.termVariable $ encodedName _lists_safeHead)
    (Core.termApplication $ Core.application
      (Core.termApplication $ Core.application
        (Core.termVariable $ encodedName _lists_filter)
        (var "predTerm"))
      (var "listTerm"))

-- | Interpreter-friendly left fold for List terms.
-- Folds from the left: foldl f init [e1,e2,e3] = f (f (f init e1) e2) e3
-- Each step is reduced through the interpreter so that the accumulator is always a value.
foldl_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Term -> Either Error Term)
foldl_ = define "foldl" $
  doc "Interpreter-friendly left fold for List terms." $
  "cx" ~> "g" ~>
  "funTerm" ~> "initTerm" ~> "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm") $
  -- Fold with reduction at each step: reduce f(acc, el) before next iteration.
  -- The accumulator is Either Error Term to thread errors through.
  Lists.foldl
    ("acc" ~> "el" ~>
      Eithers.bind (var "acc") $
        "reducedAcc" ~>
          Reduction.reduceTerm @@ var "cx" @@ var "g" @@ MetaLiterals.boolean True @@
            (Core.termApplication $ Core.application
              (Core.termApplication $ Core.application (var "funTerm") (var "reducedAcc"))
              (var "el")))
    (right $ var "initTerm")
    (var "elements")

-- | Interpreter-friendly right fold for List terms.
-- Folds from the right: foldr f init [e1,e2,e3] = f e1 (f e2 (f e3 init))
-- Each step is reduced through the interpreter so that the accumulator is always a value.
foldr_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Term -> Either Error Term)
foldr_ = define "foldr" $
  doc "Interpreter-friendly right fold for List terms." $
  "cx" ~> "g" ~>
  "funTerm" ~> "initTerm" ~> "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm") $
  -- Fold with reduction at each step
  Lists.foldr
    ("el" ~> "acc" ~>
      Eithers.bind (var "acc") $
        "reducedAcc" ~>
          Reduction.reduceTerm @@ var "cx" @@ var "g" @@ MetaLiterals.boolean True @@
            (Core.termApplication $ Core.application
              (Core.termApplication $ Core.application (var "funTerm") (var "el"))
              (var "reducedAcc")))
    (right $ var "initTerm")
    (var "elements")

-- | Interpreter-friendly map for List terms.
-- Applies funTerm to each element of listTerm.
-- Note: builds result directly using foldl to avoid recursive primitive calls.
map_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either Error Term)
map_ = define "map" $
  doc "Interpreter-friendly map for List terms." $
  "cx" ~> "g" ~>
  "funTerm" ~> "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm") $
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
partition_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either Error Term)
partition_ = define "partition" $
  doc "Interpreter-friendly partition for List terms." $
  "cx" ~> "g" ~>
  "predTerm" ~> "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm") $
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
        (Core.termVariable $ encodedName _pairs_first)
        (var "acc")) $
      "nos" <~ (Core.termApplication $ Core.application
        (Core.termVariable $ encodedName _pairs_second)
        (var "acc")) $
      -- Build ifElse (pred el) trueCase falseCase
      Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termApplication $ Core.application
            (Core.termVariable $ encodedName _logic_ifElse)
            -- condition: pred el
            (Core.termApplication $ Core.application (var "predTerm") (var "el")))
          -- true branch: (append yeses [el], nos)
          (Core.termPair $ pair
            (Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termVariable $ encodedName _lists_concat2)
                (var "yeses"))
              (Core.termList $ list [var "el"]))
            (var "nos")))
        -- false branch: (yeses, append nos [el])
        (Core.termPair $ pair
          (var "yeses")
          (Core.termApplication $ Core.application
            (Core.termApplication $ Core.application
              (Core.termVariable $ encodedName _lists_concat2)
              (var "nos"))
            (Core.termList $ list [var "el"]))))
    (var "initialState")
    (var "elements")) $
  -- Return the final state directly (it's already a pair)
  right $ var "finalState"

-- | Interpreter-friendly sortOn for List terms.
-- Sorts elements by comparing the results of applying projTerm to each.
-- Uses insertion sort: for each element, use span to find insertion point.
sortOn_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either Error Term)
sortOn_ = define "sortOn" $
  doc "Interpreter-friendly sortOn for List terms." $
  "cx" ~> "g" ~>
  "projTerm" ~> "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm") $
  -- Build: foldl (\sorted x -> insert x sorted) [] elements
  -- where insert x sorted = let (before, after) = span (\y -> lte (proj y) (proj x)) sorted
  --                         in concat [before, [x], after]
  right $ Lists.foldl
    ("sorted" ~> "x" ~>
      -- Build the split using span with predicate: \y -> lte (proj y) (proj x)
      "splitResult" <~ (Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termVariable $ encodedName _lists_span)
          -- predicate lambda: \y -> lte (proj y) (proj x) -- use lte for stable sort
          (Core.termLambda $ Core.lambda (wrap _Name $ string "y") nothing $
            Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termVariable $ encodedName _equality_lte)
                (Core.termApplication $ Core.application
                  (var "projTerm")
                  (Core.termVariable $ wrap _Name $ string "y")))
              (Core.termApplication $ Core.application (var "projTerm") (var "x"))))
        (var "sorted")) $
      -- Build: concat [before, [x], after]
      "before" <~ (Core.termApplication $ Core.application
        (Core.termVariable $ encodedName _pairs_first)
        (var "splitResult")) $
      "after" <~ (Core.termApplication $ Core.application
        (Core.termVariable $ encodedName _pairs_second)
        (var "splitResult")) $
      Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termVariable $ encodedName _lists_concat2)
          (var "before"))
        (Core.termApplication $ Core.application
          (Core.termApplication $ Core.application
            (Core.termVariable $ encodedName _lists_cons)
            (var "x"))
          (var "after")))
    (Core.termList $ list ([] :: [TTerm Term]))
    (var "elements")

-- | Interpreter-friendly span for List terms.
-- Splits the list into (takeWhile pred list, dropWhile pred list).
-- Uses foldl with state ((stillTaking, left), right) to track the split point.
span_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either Error Term)
span_ = define "span" $
  doc "Interpreter-friendly span for List terms." $
  "cx" ~> "g" ~>
  "predTerm" ~> "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm") $
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
        (Core.termVariable $ encodedName _pairs_first)
        (var "acc")) $
      "right" <~ (Core.termApplication $ Core.application
        (Core.termVariable $ encodedName _pairs_second)
        (var "acc")) $
      "taking" <~ (Core.termApplication $ Core.application
        (Core.termVariable $ encodedName _pairs_first)
        (var "takingLeft")) $
      "left" <~ (Core.termApplication $ Core.application
        (Core.termVariable $ encodedName _pairs_second)
        (var "takingLeft")) $
      -- Build ifElse (and taking (pred el)) trueCase falseCase
      Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termApplication $ Core.application
            (Core.termVariable $ encodedName _logic_ifElse)
            -- condition: and taking (pred el)
            (Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termVariable $ encodedName _logic_and)
                (var "taking"))
              (Core.termApplication $ Core.application (var "predTerm") (var "el"))))
          -- true branch: ((true, append left [el]), right)
          (Core.termPair $ pair
            (Core.termPair $ pair
              (Core.termLiteral $ Core.literalBoolean $ MetaLiterals.boolean True)
              (Core.termApplication $ Core.application
                (Core.termApplication $ Core.application
                  (Core.termVariable $ encodedName _lists_concat2)
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
              (Core.termVariable $ encodedName _lists_concat2)
              (var "right"))
            (Core.termList $ list [var "el"]))))
    (var "initialState")
    (var "elements")) $
  -- Extract result: (snd (fst finalState), snd finalState)
  right $ Core.termPair $ pair
    (Core.termApplication $ Core.application
      (Core.termVariable $ encodedName _pairs_second)
      (Core.termApplication $ Core.application
        (Core.termVariable $ encodedName _pairs_first)
        (var "finalState")))
    (Core.termApplication $ Core.application
      (Core.termVariable $ encodedName _pairs_second)
      (var "finalState"))

-- | Interpreter-friendly zipWith for List terms.
-- Applies funTerm to corresponding pairs of elements.
zipWith_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Term -> Either Error Term)
zipWith_ = define "zipWith" $
  doc "Interpreter-friendly zipWith for List terms." $
  "cx" ~> "g" ~>
  "funTerm" ~> "listTerm1" ~> "listTerm2" ~>
  "elements1" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm1") $
  "elements2" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm2") $
  -- Build: [f a1 b1, f a2 b2, ...]
  right $ Core.termList $ Lists.map
    ("p" ~>
      "a" <~ Pairs.first (var "p") $
      "b" <~ Pairs.second (var "p") $
      Core.termApplication $ Core.application
        (Core.termApplication $ Core.application (var "funTerm") (var "a"))
        (var "b"))
    (Lists.zip (var "elements1") (var "elements2"))

-- | Interpreter-friendly elem for List terms.
-- Tests whether an element is in the list: elem x xs = isJust (find (equal x) xs)
elem_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either Error Term)
elem_ = define "elem" $
  doc "Interpreter-friendly elem for List terms." $
  "cx" ~> "g" ~>
  "x" ~> "listTerm" ~>
  -- Build: isJust (find (equal x) listTerm)
  right $ Core.termApplication $ Core.application
    (Core.termVariable $ encodedName _maybes_isJust)
    (Core.termApplication $ Core.application
      (Core.termApplication $ Core.application
        (Core.termVariable $ encodedName _lists_find)
        (Core.termApplication $ Core.application
          (Core.termVariable $ encodedName _equality_equal)
          (var "x")))
      (var "listTerm"))

-- | Interpreter-friendly group for List terms.
-- Groups consecutive equal elements: group [1,1,2,2,2,3] = [[1,1],[2,2,2],[3]]
-- Returns a term that delegates to the lists.foldl primitive with a term-level lambda.
-- The interpreter's native foldl uses functionWithReduce, which reduces each step
-- via reduceTerm — so the accumulator is always a value, not an unreduced expression.
-- Uses safeHead+maybe instead of null+head+ifElse to avoid eager evaluation of head on empty lists.
group_ :: TTermDefinition (Context -> Graph -> Term -> Either Error Term)
group_ = define "group" $
  doc "Interpreter-friendly group for List terms." $
  "cx" ~> "g" ~>
  "listTerm" ~>
  right $ Core.termApplication $ Core.application flushFn foldExpr
  where
    -- Helper: build a primitive application
    prim1 n x = Core.termApplication $ Core.application (Core.termVariable $ encodedName n) x
    prim2 n x y = Core.termApplication $ Core.application (Core.termApplication $ Core.application (Core.termVariable $ encodedName n) x) y
    prim3 n x y z = Core.termApplication $ Core.application (Core.termApplication $ Core.application (Core.termApplication $ Core.application (Core.termVariable $ encodedName n) x) y) z
    -- Helper: term-level variable
    tv s = Core.termVariable $ wrap _Name $ string s
    -- Helper: term-level lambda
    lam s body = Core.termLambda $ Core.lambda (wrap _Name $ string s) nothing body

    -- stepFn: \acc el -> maybe ([el], snd acc) (\h -> ifElse (equal el h) (extend) (flush)) (safeHead (fst acc))
    stepFn = lam "acc" $ lam "el" $
      prim3 _maybes_maybe
        -- Nothing (empty group): start new with [el]
        (Core.termPair $ pair
          (Core.termList $ list [tv "el"])
          (prim1 _pairs_second (tv "acc")))
        -- Just h: check equality
        (lam "h" $
          prim3 _logic_ifElse
            (prim2 _equality_equal (tv "el") (tv "h"))
            -- Same: extend current group
            (Core.termPair $ pair
              (prim2 _lists_concat2 (prim1 _pairs_first (tv "acc")) (Core.termList $ list [tv "el"]))
              (prim1 _pairs_second (tv "acc")))
            -- Different: flush and start new
            (Core.termPair $ pair
              (Core.termList $ list [tv "el"])
              (prim2 _lists_concat2
                (prim1 _pairs_second (tv "acc"))
                (Core.termList $ list [prim1 _pairs_first (tv "acc")]))))
        -- safeHead (fst acc)
        (prim1 _lists_safeHead (prim1 _pairs_first (tv "acc")))

    initState = Core.termPair $ pair
      (Core.termList $ list ([] :: [TTerm Term]))
      (Core.termList $ list ([] :: [TTerm Term]))

    foldExpr = prim3 _lists_foldl stepFn initState (var "listTerm")

    -- Flush: \foldResult -> ifElse (null (fst r)) (snd r) (concat2 (snd r) [fst r])
    flushFn = lam "foldResult" $
      prim3 _logic_ifElse
        (prim1 _lists_null (prim1 _pairs_first (tv "foldResult")))
        (prim1 _pairs_second (tv "foldResult"))
        (prim2 _lists_concat2
          (prim1 _pairs_second (tv "foldResult"))
          (Core.termList $ list [prim1 _pairs_first (tv "foldResult")]))

-- | Interpreter-friendly intercalate for List terms.
-- intercalate sep xss = concat (intersperse sep xss)
intercalate_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either Error Term)
intercalate_ = define "intercalate" $
  doc "Interpreter-friendly intercalate for List terms." $
  "cx" ~> "g" ~>
  "sep" ~> "listsTerm" ~>
  -- Build: concat (intersperse sep listsTerm)
  right $ Core.termApplication $ Core.application
    (Core.termVariable $ encodedName _lists_concat)
    (Core.termApplication $ Core.application
      (Core.termApplication $ Core.application
        (Core.termVariable $ encodedName _lists_intersperse)
        (var "sep"))
      (var "listsTerm"))

-- | Interpreter-friendly intersperse for List terms.
-- intersperse sep [a,b,c] = [a,sep,b,sep,c]
intersperse_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either Error Term)
intersperse_ = define "intersperse" $
  doc "Interpreter-friendly intersperse for List terms." $
  "cx" ~> "g" ~>
  "sep" ~> "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm") $
  right $ Logic.ifElse
    (Lists.null (var "elements"))
    (Core.termList $ list ([] :: [TTerm Term]))
    (Core.termList $ Lists.cons
      (Lists.head (var "elements"))
      (Lists.concat $ Lists.map
        ("el" ~> list [var "sep", var "el"])
        (Lists.tail (var "elements"))))

-- | Interpreter-friendly maybeHead for List terms.
-- maybeHead xs = if null xs then Nothing else Just (head xs)
maybeHead_ :: TTermDefinition (Context -> Graph -> Term -> Either Error Term)
maybeHead_ = define "maybeHead" $
  doc "Interpreter-friendly maybeHead for List terms." $
  "cx" ~> "g" ~>
  "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm") $
  right $ Logic.ifElse
    (Lists.null (var "elements"))
    (Core.termMaybe nothing)
    (Core.termMaybe $ just $ Lists.head (var "elements"))

-- | Interpreter-friendly nub for List terms.
-- Removes duplicates using equality. nub xs = foldl (\acc x -> ifElse (elem x acc) acc (concat2 acc [x])) [] xs
nub_ :: TTermDefinition (Context -> Graph -> Term -> Either Error Term)
nub_ = define "nub" $
  doc "Interpreter-friendly nub for List terms." $
  "cx" ~> "g" ~>
  "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm") $
  -- Build: foldl (\acc x -> ifElse (elem x acc) acc (concat2 acc [x])) [] xs
  -- This must be entirely at the term level since we need runtime equality checks
  right $ Core.termApplication $ Core.application
    (Core.termApplication $ Core.application
      (Core.termApplication $ Core.application
        (Core.termVariable $ encodedName _lists_foldl)
        -- fold function: \acc x -> ifElse (elem x acc) acc (concat2 acc [x])
        (Core.termLambda $ Core.lambda (wrap _Name $ string "acc") nothing $
          Core.termLambda $ Core.lambda (wrap _Name $ string "x") nothing $
            Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termApplication $ Core.application
                  (Core.termVariable $ encodedName _logic_ifElse)
                  (Core.termApplication $ Core.application
                    (Core.termApplication $ Core.application
                      (Core.termVariable $ encodedName _lists_elem)
                      (Core.termVariable $ wrap _Name $ string "x"))
                    (Core.termVariable $ wrap _Name $ string "acc")))
                (Core.termVariable $ wrap _Name $ string "acc"))
              (Core.termApplication $ Core.application
                (Core.termApplication $ Core.application
                  (Core.termVariable $ encodedName _lists_concat2)
                  (Core.termVariable $ wrap _Name $ string "acc"))
                (Core.termList $ list [Core.termVariable $ wrap _Name $ string "x"]))))
      -- initial: []
      (Core.termList $ list ([] :: [TTerm Term])))
    -- list
    (var "listTerm")

-- | Interpreter-friendly pure for List terms.
-- Wraps a single element in a list: pure x = [x]
pure_ :: TTermDefinition (Context -> Graph -> Term -> Either Error Term)
pure_ = define "pure" $
  doc "Interpreter-friendly pure for List terms." $
  "cx" ~> "g" ~>
  "x" ~>
  right $ Core.termList $ list [var "x"]

-- | Interpreter-friendly replicate for List terms.
-- replicate n x = map (const x) (range 0 n)
replicate_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either Error Term)
replicate_ = define "replicate" $
  doc "Interpreter-friendly replicate for List terms." $
  "cx" ~> "g" ~>
  "n" ~> "x" ~>
  -- Build: map (\_ -> x) (range 1 n)  -- range is inclusive, so range 1 n has n elements
  right $ Core.termApplication $ Core.application
    (Core.termApplication $ Core.application
      (Core.termVariable $ encodedName _lists_map)
      (Core.termLambda $ Core.lambda (wrap _Name $ string "_") nothing $
        var "x"))
    (Core.termApplication $ Core.application
      (Core.termApplication $ Core.application
        (Core.termVariable $ encodedName _math_range)
        (Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ MetaLiterals.int32 1))
      (var "n"))

-- | Interpreter-friendly safeHead for List terms.
-- safeHead xs = if null xs then Nothing else Just (head xs)
safeHead_ :: TTermDefinition (Context -> Graph -> Term -> Either Error Term)
safeHead_ = define "safeHead" $
  doc "Interpreter-friendly safeHead for List terms." $
  "cx" ~> "g" ~>
  "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm") $
  right $ Logic.ifElse
    (Lists.null (var "elements"))
    (Core.termMaybe nothing)
    (Core.termMaybe $ just $ Lists.head (var "elements"))

-- | Interpreter-friendly singleton for List terms.
-- singleton x = [x]
singleton_ :: TTermDefinition (Context -> Graph -> Term -> Either Error Term)
singleton_ = define "singleton" $
  doc "Interpreter-friendly singleton for List terms." $
  "cx" ~> "g" ~>
  "x" ~>
  right $ Core.termList $ list [var "x"]

-- | Interpreter-friendly sort for List terms.
-- sort xs = sortOn identity xs
sort_ :: TTermDefinition (Context -> Graph -> Term -> Either Error Term)
sort_ = define "sort" $
  doc "Interpreter-friendly sort for List terms." $
  "cx" ~> "g" ~>
  "listTerm" ~>
  -- Build: sortOn identity listTerm
  right $ Core.termApplication $ Core.application
    (Core.termApplication $ Core.application
      (Core.termVariable $ encodedName _lists_sortOn)
      (Core.termVariable $ encodedName _equality_identity))
    (var "listTerm")
