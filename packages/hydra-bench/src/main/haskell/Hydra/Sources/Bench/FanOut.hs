-- | Synthetic workload — fan-out tree — for cross-host inference benchmarking.
--
-- One of several "series" under @Hydra.Sources.Bench@. Where 'LinearChain'
-- has each walker call exactly one predecessor, this series has each walker
-- call up to three smaller walkers in different case branches. Closer to
-- real codegen / term-rewriter shape: branchy traversal with multiple
-- recursive paths.
--
-- For walker_k (with k > 0), the case branches recurse on three different
-- predecessors:
--
--   * @_Term_application@ branch -> calls walker_(k - 1)
--   * @_Term_lambda@      branch -> calls walker_(k - max 1 (k `div` 2))
--   * @_Term_let@         branch -> calls walker_(k - max 1 (k `div` 3))
--
-- This produces a fan-out DAG with overlapping subtrees. Inference cost
-- depends on how the host caches across multiple references to the same
-- predecessor; chains with high reuse benefit from memoization.
--
-- Adjust 'numFanWalkers' or 'fanWalkerBody' and re-run the kernel-haskell
-- sync to regenerate the JSON used by all hosts.
module Hydra.Sources.Bench.FanOut where

import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Sources.Kernel.Terms.Strip as Strip

import qualified Data.List                   as L
import           Prelude                     hiding ((++))

ns :: ModuleName
ns = ModuleName "hydra.bench.fanOut"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
  moduleName = ns,
  moduleDefinitions = definitions,
  moduleDependencies = unqualifiedDep <$> ([Strip.ns] `L.union` kernelTypesModuleNames),
  moduleMetadata = descriptionMetadata (Just "Fan-out inference benchmark. Each fanWalker_K branches to three smaller fanWalkers via _Term cases — closer to real codegen DAG shape than LinearChain.")
  }
  where
    definitions = [toDefinition (mkFanWalker k) | k <- [0 .. numFanWalkers - 1]]

-- | Indices of the three predecessors a level-k fanWalker recurses on.
-- All three are clamped to @[0, k-1]@.
fanPredecessors :: Int -> (Int, Int, Int)
fanPredecessors k =
  let p1 = max 0 (k - 1)
      p2 = max 0 (k - max 1 (k `div` 2))
      p3 = max 0 (k - max 1 (k `div` 3))
  in (p1, p2, p3)

-- | Body of @fanWalker_K@. Case on @_Term@; each branch recurses on a
-- different predecessor.
fanWalkerBody :: Int -> TypedTerm (Term -> Maybe Term)
fanWalkerBody 0 =
  "t" ~> just (var "t")
fanWalkerBody k =
  let (p1, p2, p3) = fanPredecessors k
  in "t" ~>
       "stripped" <~ (Strip.deannotateTerm @@ var "t") $
       cases _Term (var "stripped") (Just $ just (var "stripped")) [
         _Term_application>>: "app" ~>
           "fun" <~ Core.applicationFunction (var "app") $
           "arg" <~ Core.applicationArgument (var "app") $
           Maybes.cases (fanWalkerRef p1 @@ var "fun") (fanWalkerRef p1 @@ var "arg") ("_" ~> fanWalkerRef p1 @@ var "fun"),
         _Term_lambda>>: "lam" ~>
           "body" <~ Core.lambdaBody (var "lam") $
           fanWalkerRef p2 @@ var "body",
         _Term_let>>: "le" ~>
           "body" <~ Core.letBody (var "le") $
           Maybes.cases (fanWalkerRef p3 @@ var "body") nothing ("inner" ~> fanWalkerRef p3 @@ var "inner"),
         _Term_variable>>: constant $ just (var "stripped"),
         _Term_literal>>:  constant $ just (var "stripped")]

-- | Hydra-level fully-qualified name for the @k@th fanWalker.
fanWalkerName :: Int -> Name
fanWalkerName k = Name $ L.concat ["hydra.bench.fanOut.fanWalker", show k]

-- | Reference to the @k@th fanWalker.
fanWalkerRef :: Int -> TypedTerm (Term -> Maybe Term)
fanWalkerRef k = TypedTerm $ TermVariable (fanWalkerName k)

mkFanWalker :: Int -> TypedTermDefinition (Term -> Maybe Term)
mkFanWalker k =
  let (p1, p2, p3) = fanPredecessors k
  in define (L.concat ["fanWalker", show k])
       $ doc (L.concat [
           "Fan-out walker level ", show k,
           "; branches to fanWalker", show p1,
           ", fanWalker", show p2,
           ", fanWalker", show p3, "."])
       $ fanWalkerBody k

-- | Number of fanWalker definitions in the tree.
numFanWalkers :: Int
numFanWalkers = 400
