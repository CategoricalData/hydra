-- | Synthetic workload — linear chain — for cross-host inference benchmarking.
--
-- One of several "series" under @Hydra.Sources.Bench@. Each series stresses
-- inference along a different axis; runners can target any series via its
-- namespace (@--series linearChain@, @--series polymorphicChain@, etc.).
--
-- This series defines @walker0@ through @walker(N-1)@. Each walker cases on
-- a subset of @_Term@ variants and recurses on @walker(k-1)@ via a
-- fully-qualified Hydra-level reference. Per-host bench runners load this
-- module's JSON, take the first N defs, build a synthetic test module, and
-- time @inferModulesGiven@ on it.
--
-- Each walker exercises four patterns that historically stressed inference:
--
--   1. Let-nesting (the @stripped@ binding in each def)
--   2. Cases on an enum-like union (subset of @_Term@ variants)
--   3. Lazy-alternative @Maybes.maybe@ (so eager evaluation in let-bindings is exposed)
--   4. Cross-def recursion (the @walkerK -> walkerK-1@ chain)
--
-- This is the simplest series — the chain is artificially deep (100 levels)
-- and per-def work is light. Use @polymorphicChain@ to add type-scheme
-- pressure or @fanOut@ for a more realistic codegen-shape workload.
--
-- Adjust 'numWalkers' or 'walkerBody' and re-run the kernel-haskell sync to
-- regenerate the JSON used by all hosts.
module Hydra.Sources.Bench.LinearChain where

import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Sources.Kernel.Terms.Strip as Strip

import qualified Data.List                   as L
import           Prelude                     hiding ((++))

ns :: Namespace
ns = Namespace "hydra.bench.linearChain"

-- | Number of walker definitions in the chain. Bench runners take prefixes
-- (e.g. first 10 / 25 / 50 / 100) to build scaling curves.
numWalkers :: Int
numWalkers = 800

-- | Hydra-level fully-qualified name for the @k@th walker. Used both as the
-- definition name and as a reference target from @walker(k+1)@.
walkerName :: Int -> Name
walkerName k = Name $ L.concat ["hydra.bench.linearChain.walker", show k]

-- | Reference to the @k@th walker as a TTerm, suitable for application.
walkerRef :: Int -> TTerm (Term -> Maybe Term)
walkerRef k = TTerm $ TermVariable (walkerName k)

-- | Body of @walkerK@.
--
-- Base case @k = 0@: identity wrapped in @Just@.
-- Inductive case @k > 0@: case on @_Term@ and recurse via @walkerRef (k-1)@.
walkerBody :: Int -> TTerm (Term -> Maybe Term)
walkerBody 0 =
  "t" ~> just (var "t")
walkerBody k =
  "t" ~>
    -- Pattern 1: let-nesting
    "stripped" <~ (Strip.deannotateTerm @@ var "t") $
    -- Pattern 2: cases on an enum-like union (slim subset of _Term variants)
    cases _Term (var "stripped") (Just $ just (var "stripped")) [
      _Term_application>>: "app" ~>
        "fun" <~ Core.applicationFunction (var "app") $
        "arg" <~ Core.applicationArgument (var "app") $
        -- Pattern 3 + 4: lazy-alternative Maybes.maybe + recursion via walkerRef
        Maybes.maybe
          (walkerRef (k-1) @@ var "arg")
          ("_" ~> walkerRef (k-1) @@ var "fun")
          (walkerRef (k-1) @@ var "fun"),
      _Term_lambda>>: "lam" ~>
        "body" <~ Core.lambdaBody (var "lam") $
        walkerRef (k-1) @@ var "body",
      _Term_let>>: "le" ~>
        "body" <~ Core.letBody (var "le") $
        Maybes.maybe
          nothing
          ("inner" ~> walkerRef (k-1) @@ var "inner")
          (walkerRef (k-1) @@ var "body"),
      _Term_variable>>: constant $ just (var "stripped"),
      _Term_literal>>:  constant $ just (var "stripped")]

-- | Build the kth walker definition.
mkWalker :: Int -> TTermDefinition (Term -> Maybe Term)
mkWalker k = define (L.concat ["walker", show k])
  $ doc (L.concat ["Term walker level ", show k, "; recurses to walker", show (max 0 (k-1)), "."])
  $ walkerBody k

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module {
  moduleNamespace = ns,
  moduleDefinitions = definitions,
  moduleDependencies = [Strip.ns] `L.union` kernelTypesNamespaces,
  moduleDescription = Just "Linear-chain inference benchmark. walkerK cases on _Term variants and recurses to walker(K-1) — depth-N type-resolution stress test."
  }
  where
    definitions = [toDefinition (mkWalker k) | k <- [0 .. numWalkers - 1]]
