-- | Primitive declarations for the hydra.lib.equality namespace.
{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Lib.Equality where

import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap          as Bootstrap
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Logic    as Logic
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms hiding (identity)
import qualified Hydra.Overlay.Haskell.Dsl.Types              as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), compare, max, min)


ns :: ModuleName
ns = ModuleName "hydra.lib.equality"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.equality module.")}
  where
    definitions = [compare, equal, gt, gte, identity, lt, lte, max, min]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

defineWithDefault :: String -> String -> TermSignature -> [String] -> TypedTerm a -> PrimitiveDefinition
defineWithDefault = primitiveWithDefaultInModule module_

-- Shared type variable
tx :: Type
tx = Types.var "x"

compare :: PrimitiveDefinition
compare = define "compare" "Compare two values and return a Comparison."
  (sig $ Types.polyConstrained [("x", [Name "ordering"])]
    (tx Types.~> tx Types.~> Types.var "hydra.util.Comparison"))
  ["compare(x, y) returns the hydra.util.Comparison value that classifies the relationship between x\
  \ and y under the type's ordering: LessThan if x < y, EqualTo if x == y, GreaterThan if x > y.",
   "The result type's three-valued tag is the canonical primitive comparison; the boolean comparators\
   \ (lt/lte/gt/gte) and equal are derivable from it.",
   "Requires an 'ordering' type-class constraint on the argument type, which is the closest Hydra\
   \ equivalent to Haskell's Ord instance.",
   "Total. Corresponds to Haskell's compare :: Ord a => a -> a -> Ordering."]

equal :: PrimitiveDefinition
equal = define "equal" "Check whether two values are equal."
  (sig $ Types.polyConstrained [("x", [Name "equality"])]
    (tx Types.~> tx Types.~> Types.boolean))
  ["equal(x, y) returns true if x and y are structurally equal under the type's notion of equality.",
   "Requires an 'equality' type-class constraint on the argument type, which is the closest Hydra\
   \ equivalent to Haskell's Eq instance.",
   "Equality is reflexive, symmetric, and transitive (no NaN-style exception on floating-point:\
   \ floating-point equality is provided per the underlying host's IEEE 754 comparison rules, so\
   \ NaN /= NaN at the level of float64/float32).",
   "Total. Corresponds to Haskell's (==) :: Eq a => a -> a -> Bool."]

gt :: PrimitiveDefinition
gt = define "gt" "Check whether the first value is greater than the second."
  (sig $ Types.polyConstrained [("x", [Name "ordering"])] (tx Types.~> tx Types.~> Types.boolean))
  ["gt(x, y) returns true iff x > y under the type's ordering.",
   "Requires an 'ordering' constraint on the argument type.",
   "Total. Corresponds to Haskell's (>) :: Ord a => a -> a -> Bool."]

gte :: PrimitiveDefinition
gte = define "gte" "Check whether the first value is greater than or equal to the second."
  (sig $ Types.polyConstrained [("x", [Name "ordering"])] (tx Types.~> tx Types.~> Types.boolean))
  ["gte(x, y) returns true iff x >= y under the type's ordering.",
   "Requires an 'ordering' constraint on the argument type.",
   "Total. Corresponds to Haskell's (>=) :: Ord a => a -> a -> Bool."]

identity :: PrimitiveDefinition
identity = defineWithDefault "identity" "Return a value unchanged."
  (sig $ TypeScheme [Name "x"] (tx Types.~> tx) Nothing)
  ["identity(x) = x. The polymorphic identity function.",
   "Total. Corresponds to Haskell's id :: a -> a."]
  ("x" ~> var "x")

lt :: PrimitiveDefinition
lt = define "lt" "Check whether the first value is less than the second."
  (sig $ Types.polyConstrained [("x", [Name "ordering"])] (tx Types.~> tx Types.~> Types.boolean))
  ["lt(x, y) returns true iff x < y under the type's ordering.",
   "Requires an 'ordering' constraint on the argument type.",
   "Total. Corresponds to Haskell's (<) :: Ord a => a -> a -> Bool."]

lte :: PrimitiveDefinition
lte = define "lte" "Check whether the first value is less than or equal to the second."
  (sig $ Types.polyConstrained [("x", [Name "ordering"])] (tx Types.~> tx Types.~> Types.boolean))
  ["lte(x, y) returns true iff x <= y under the type's ordering.",
   "Requires an 'ordering' constraint on the argument type.",
   "Total. Corresponds to Haskell's (<=) :: Ord a => a -> a -> Bool."]

max :: PrimitiveDefinition
max = defineWithDefault "max" "Return the maximum of two values."
  (sig $ Types.polyConstrained [("x", [Name "ordering"])] (tx Types.~> tx Types.~> tx))
  ["max(x, y) returns the larger of x and y under the type's ordering; if x == y, it returns y\
  \ (matching Haskell's convention).",
   "Requires an 'ordering' constraint on the argument type.",
   "Total. Corresponds to Haskell's max :: Ord a => a -> a -> a."]
  ("x" ~> "y" ~> Logic.ifElse (Equality.gte (var "x") (var "y")) (var "x" :: TypedTerm a) (var "y"))

min :: PrimitiveDefinition
min = defineWithDefault "min" "Return the minimum of two values."
  (sig $ Types.polyConstrained [("x", [Name "ordering"])] (tx Types.~> tx Types.~> tx))
  ["min(x, y) returns the smaller of x and y under the type's ordering; if x == y, it returns x\
  \ (matching Haskell's convention).",
   "Requires an 'ordering' constraint on the argument type.",
   "Total. Corresponds to Haskell's min :: Ord a => a -> a -> a."]
  ("x" ~> "y" ~> Logic.ifElse (Equality.lte (var "x") (var "y")) (var "x" :: TypedTerm a) (var "y"))
