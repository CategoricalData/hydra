-- | Primitive declarations for the hydra.core.lib.ordering namespace.
{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Lib.Ordering where

import Hydra.Kernel
import qualified Hydra.Core.Overlay.Haskell.Bootstrap          as Bootstrap
import qualified Hydra.Core.Dsl.Lib.Logic    as Logic
import qualified Hydra.Core.Dsl.Lib.Ordering as Ordering
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms      as Phantoms hiding (compare, max, min)
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types              as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), compare, max, min)


ns :: ModuleName
ns = ModuleName "hydra.core.lib.ordering"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.core.lib.ordering module.")}
  where
    definitions = [compare, gt, gte, lt, lte, max, min]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

defineWithDefault :: String -> String -> TermSignature -> [String] -> TypedTerm a -> PrimitiveDefinition
defineWithDefault = primitiveWithDefaultInModule module_

-- Shared type variable
tx :: Type
tx = Types.var "x"

compare :: PrimitiveDefinition
compare = withProvisions [
  requirement "threeWayClassification"
    "compare(x, y) returns lessThan, equalTo, or greaterThan classifying x relative to y under\
    \ the type's ordering, for integers, strings (lexicographic, by Unicode code point), floats,\
    \ lists (lexicographic, order-sensitive), sets, and maps (canonical sorted-order comparison,\
    \ independent of construction/insertion order).",
  requirement "decimalScaleTiebreak"
    "compare on decimals orders primarily by numeric value; when two decimals are numerically\
    \ equal but have different scale, compare treats them as distinct (not equalTo) and breaks\
    \ the tie by scale (smaller scale orders first), including transitively across more than two\
    \ scale levels."]
  $ define "compare" "Compare two values and return a Comparison."
  (sigWithParams [("x", "the first value to compare"), ("y", "the second value to compare")] $ Types.polyConstrained [("x", [Name "ordering"])]
    (tx Types.~> tx Types.~> Types.var "hydra.core.util.Comparison"))
  ["compare(x, y) returns the hydra.core.util.Comparison value that classifies the relationship between x\
  \ and y under the type's ordering: LessThan if x < y, EqualTo if x == y, GreaterThan if x > y.",
   "The result type's three-valued tag is the canonical primitive comparison; the boolean comparators\
   \ (lt/lte/gt/gte) are derivable from it.",
   "Requires an 'ordering' type-class constraint on the argument type, which is the closest Hydra\
   \ equivalent to Haskell's Ord instance.",
   "Since: 0.18 (moved from hydra.core.lib.equality.compare).",
   "Total. Corresponds to Haskell's compare :: Ord a => a -> a -> Ordering."]

gt :: PrimitiveDefinition
gt = withProvisions [
  requirement "strictGreaterThan"
    "gt(x, y) returns true exactly when x is strictly greater than y under the type's ordering,\
    \ for integers, floats, and strings; equal values and lesser values both return false."]
  $ define "gt" "Check whether the first value is greater than the second."
  (sigWithParams [("x", "the first value to compare"), ("y", "the second value to compare")] $ Types.polyConstrained [("x", [Name "ordering"])] (tx Types.~> tx Types.~> Types.boolean))
  ["gt(x, y) returns true iff x > y under the type's ordering.",
   "Requires an 'ordering' constraint on the argument type.",
   "Since: 0.18 (moved from hydra.core.lib.equality.gt).",
   "Total. Corresponds to Haskell's (>) :: Ord a => a -> a -> Bool."]

gte :: PrimitiveDefinition
gte = withProvisions [
  requirement "greaterThanOrEqual"
    "gte(x, y) returns true when x is greater than or strictly equal to y under the type's\
    \ ordering, for integers, floats, strings, and decimals -- including the decimal\
    \ scale-tiebreak rule (a numerically-equal, smaller-scale decimal is treated as lesser, so\
    \ gte is true in that direction)."]
  $ define "gte" "Check whether the first value is greater than or equal to the second."
  (sigWithParams [("x", "the first value to compare"), ("y", "the second value to compare")] $ Types.polyConstrained [("x", [Name "ordering"])] (tx Types.~> tx Types.~> Types.boolean))
  ["gte(x, y) returns true iff x >= y under the type's ordering.",
   "Requires an 'ordering' constraint on the argument type.",
   "Since: 0.18 (moved from hydra.core.lib.equality.gte).",
   "Total. Corresponds to Haskell's (>=) :: Ord a => a -> a -> Bool."]

lt :: PrimitiveDefinition
lt = withProvisions [
  requirement "strictLessThan"
    "lt(x, y) returns true exactly when x is strictly less than y under the type's ordering, for\
    \ integers, floats, and strings; equal values and greater values both return false."]
  $ define "lt" "Check whether the first value is less than the second."
  (sigWithParams [("x", "the first value to compare"), ("y", "the second value to compare")] $ Types.polyConstrained [("x", [Name "ordering"])] (tx Types.~> tx Types.~> Types.boolean))
  ["lt(x, y) returns true iff x < y under the type's ordering.",
   "Requires an 'ordering' constraint on the argument type.",
   "Since: 0.18 (moved from hydra.core.lib.equality.lt).",
   "Total. Corresponds to Haskell's (<) :: Ord a => a -> a -> Bool."]

lte :: PrimitiveDefinition
lte = withProvisions [
  requirement "lessThanOrEqual"
    "lte(x, y) returns true when x is less than or strictly equal to y under the type's\
    \ ordering, for integers, floats, strings, and decimals -- including the decimal\
    \ scale-tiebreak rule."]
  $ define "lte" "Check whether the first value is less than or equal to the second."
  (sigWithParams [("x", "the first value to compare"), ("y", "the second value to compare")] $ Types.polyConstrained [("x", [Name "ordering"])] (tx Types.~> tx Types.~> Types.boolean))
  ["lte(x, y) returns true iff x <= y under the type's ordering.",
   "Requires an 'ordering' constraint on the argument type.",
   "Since: 0.18 (moved from hydra.core.lib.equality.lte).",
   "Total. Corresponds to Haskell's (<=) :: Ord a => a -> a -> Bool."]

max :: PrimitiveDefinition
max = withProvisions [
  requirement "returnsLargerOperand"
    "max(x, y) returns whichever of x and y is larger under the type's ordering; when x and y\
    \ are equal, max returns y (matching Haskell's tie-breaking convention) -- holds for\
    \ integers, strings, floats, and decimals."]
  $ defineWithDefault "max" "Return the maximum of two values."
  (sigWithParams [("x", "the first value to compare"), ("y", "the second value to compare")] $ Types.polyConstrained [("x", [Name "ordering"])] (tx Types.~> tx Types.~> tx))
  ["max(x, y) returns the larger of x and y under the type's ordering; if x == y, it returns y\
  \ (matching Haskell's convention).",
   "Requires an 'ordering' constraint on the argument type.",
   "Since: 0.18 (moved from hydra.core.lib.equality.max).",
   "Total. Corresponds to Haskell's max :: Ord a => a -> a -> a."]
  ("x" ~> "y" ~> Logic.ifElse (Ordering.gte (var "x") (var "y")) (var "x" :: TypedTerm a) (var "y"))

min :: PrimitiveDefinition
min = withProvisions [
  requirement "returnsSmallerOperand"
    "min(x, y) returns whichever of x and y is smaller under the type's ordering; when x and y\
    \ are equal, min returns x (matching Haskell's tie-breaking convention) -- holds for\
    \ integers, strings, floats, and decimals."]
  $ defineWithDefault "min" "Return the minimum of two values."
  (sigWithParams [("x", "the first value to compare"), ("y", "the second value to compare")] $ Types.polyConstrained [("x", [Name "ordering"])] (tx Types.~> tx Types.~> tx))
  ["min(x, y) returns the smaller of x and y under the type's ordering; if x == y, it returns x\
  \ (matching Haskell's convention).",
   "Requires an 'ordering' constraint on the argument type.",
   "Since: 0.18 (moved from hydra.core.lib.equality.min).",
   "Total. Corresponds to Haskell's min :: Ord a => a -> a -> a."]
  ("x" ~> "y" ~> Logic.ifElse (Ordering.lte (var "x") (var "y")) (var "x" :: TypedTerm a) (var "y"))
