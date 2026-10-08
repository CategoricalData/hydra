-- | Primitive declarations for the hydra.core.lib.equality namespace.
{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Lib.Equality where

import Hydra.Kernel
import qualified Hydra.Core.Overlay.Haskell.Bootstrap          as Bootstrap
import qualified Hydra.Core.Dsl.Lib.Equality as Equality
import qualified Hydra.Core.Dsl.Lib.Logic    as Logic
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms      as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types              as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.core.lib.equality"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.core.lib.equality module.")}
  where
    definitions = [equal, notEqual]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

defineWithDefault :: String -> String -> TermSignature -> [String] -> TypedTerm a -> PrimitiveDefinition
defineWithDefault = primitiveWithDefaultInModule module_

-- Signatures (derived from Hydra.Core.Overlay.Haskell.Libraries primN declarations).

-- Shared type variable
tx :: Type
tx = Types.var "x"

equal :: PrimitiveDefinition
equal = withProvisions [
  requirement "structuralEquivalence"
    "equal returns true exactly when its two arguments are structurally equivalent under the\
    \ type's equality notion -- identical scalars, or recursively equivalent collection contents\
    \ regardless of construction/insertion order for sets and maps, and in the same order for\
    \ lists.",
  requirement "collectionContentsCompared"
    "equal compares collection (map/set) values structurally, not by a proxy such as a\
    \ stringified representation -- two non-empty collections with different contents are never\
    \ reported equal, including when a decimal value is nested inside a collection rather than\
    \ appearing at the top level.",
  requirement "decimalScaleSensitive"
    "equal treats two decimals of equal numeric value but different scale as unequal -- decimal\
    \ equality is exact (value and scale), not numeric-value equality.",
  requirement "ieee754ExtendedFloatSemantics"
    "equal on float64/float32 follows IEEE 754 extended totalOrder semantics rather than native\
    \ host ==: NaN is equal to itself, and -0.0 is unequal to +0.0; ordinary finite values compare\
    \ by numeric value."]
  $ define "equal" "Check whether two values are equal."
  (sigWithParams [("x", "the first value to compare"), ("y", "the second value to compare")] $ Types.polyConstrained [("x", [Name "equality"])]
    (tx Types.~> tx Types.~> Types.boolean))
  ["equal(x, y) returns true if x and y are structurally equal under the type's notion of equality.",
   "Requires an 'equality' type-class constraint on the argument type, which is the closest Hydra\
   \ equivalent to Haskell's Eq instance.",
   "Equality is reflexive, symmetric, and transitive (no NaN-style exception on floating-point:\
   \ floating-point equality is provided per the underlying host's IEEE 754 comparison rules, so\
   \ NaN /= NaN at the level of float64/float32).",
   "Total. Corresponds to Haskell's (==) :: Eq a => a -> a -> Bool."]

notEqual :: PrimitiveDefinition
notEqual = withProvisions [
  requirement "negatesEqual"
    "notEqual(x, y) = not(equal(x, y)) for scalar values of any type -- integers, strings, and\
    \ floats.",
  requirement "negatesEqualOnCollections"
    "notEqual on collection values (lists, sets, maps) is likewise the negation of equal:\
    \ order-sensitivity for lists and order-independence for sets/maps carry through unchanged."]
  $ defineWithDefault "notEqual" "Check whether two values are unequal."
  (sigWithParams [("x", "the first value to compare"), ("y", "the second value to compare")] $ Types.polyConstrained [("x", [Name "equality"])]
    (tx Types.~> tx Types.~> Types.boolean))
  ["notEqual(x, y) = logic.not(equal(x, y)); this defining equation is the specification, and the\
   \ default implementation.",
   "Requires an 'equality' type-class constraint on the argument type.",
   "Total. Corresponds to Haskell's (/=) :: Eq a => a -> a -> Bool."]
  ("x" ~> "y" ~> Logic.not (Equality.equal (var "x") (var "y")))
