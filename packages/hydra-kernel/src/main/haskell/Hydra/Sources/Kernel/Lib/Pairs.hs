-- | Primitive declarations for the hydra.lib.pairs namespace.
{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Lib.Pairs where

import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.lib.pairs"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.pairs module.")}
  where
    definitions = [bimap, first, Hydra.Sources.Kernel.Lib.Pairs.pair, second]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

defineWithDefault :: String -> String -> TermSignature -> [String] -> TypedTerm a -> PrimitiveDefinition
defineWithDefault = primitiveWithDefaultInModule module_

bimap :: PrimitiveDefinition
bimap = defineWithDefault "bimap" "Map over both elements of a pair."
  (sigWithParams [("f", "the function applied to the first element"),
                  ("g", "the function applied to the second element"),
                  ("p", "the pair to map over")] $ TypeScheme [Name "a", Name "b", Name "c", Name "d"]
    ((Types.var "a" Types.~> Types.var "c") Types.~>
     (Types.var "b" Types.~> Types.var "d") Types.~>
     Types.pair (Types.var "a") (Types.var "b") Types.~>
     Types.pair (Types.var "c") (Types.var "d"))
    mempty)
  ["bimap(f, g, p) returns a new pair (f(first(p)), g(second(p))). The bifunctor map for pairs.",
   "Total. Corresponds to Haskell's Data.Bifunctor.bimap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)."]
  ("f" ~> "g" ~> "p" ~> Phantoms.pair
    (var "f" @@ Pairs.first (var "p"))
    (var "g" @@ Pairs.second (var "p")))

first :: PrimitiveDefinition
first = define "first" "Get the first element of a pair."
  (sigWithParams [("p", "the pair whose first element is returned")] $ TypeScheme [Name "a", Name "b"]
    (Types.pair (Types.var "a") (Types.var "b") Types.~> Types.var "a")
    mempty)
  ["first(p) returns the first component of the pair p.",
   "Total. Corresponds to Haskell's fst :: (a, b) -> a."]

pair :: PrimitiveDefinition
pair = define "pair" "Construct a pair from two values."
  (sigWithParams [("x", "the first element"), ("y", "the second element")] $ TypeScheme [Name "a", Name "b"]
    (Types.var "a" Types.~> Types.var "b" Types.~> Types.pair (Types.var "a") (Types.var "b"))
    mempty)
  ["pair(x, y) returns the pair (x, y).",
   "The type-free pair constructor, useful point-free (e.g. zipWith(pair, xs, ys)).",
   "Total. Corresponds to Haskell's (,) :: a -> b -> (a, b)."]

second :: PrimitiveDefinition
second = define "second" "Get the second element of a pair."
  (sigWithParams [("p", "the pair whose second element is returned")] $ TypeScheme [Name "a", Name "b"]
    (Types.pair (Types.var "a") (Types.var "b") Types.~> Types.var "b")
    mempty)
  ["second(p) returns the second component of the pair p.",
   "Total. Corresponds to Haskell's snd :: (a, b) -> b."]
