-- | Primitive declarations for the hydra.lib.equality namespace.
{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Lib.Equality where

import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap          as Bootstrap
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Logic    as Logic
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Types              as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.lib.equality"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.equality module.")}
  where
    definitions = [equal, notEqual]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

defineWithDefault :: String -> String -> TermSignature -> [String] -> TypedTerm a -> PrimitiveDefinition
defineWithDefault = primitiveWithDefaultInModule module_

-- Signatures (derived from Hydra.Overlay.Haskell.Libraries primN declarations).

-- Shared type variable
tx :: Type
tx = Types.var "x"

equal :: PrimitiveDefinition
equal = define "equal" "Check whether two values are equal."
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
notEqual = defineWithDefault "notEqual" "Check whether two values are unequal."
  (sigWithParams [("x", "the first value to compare"), ("y", "the second value to compare")] $ Types.polyConstrained [("x", [Name "equality"])]
    (tx Types.~> tx Types.~> Types.boolean))
  ["notEqual(x, y) = logic.not(equal(x, y)); this defining equation is the specification, and the\
   \ default implementation.",
   "Requires an 'equality' type-class constraint on the argument type.",
   "Total. Corresponds to Haskell's (/=) :: Eq a => a -> a -> Bool."]
  ("x" ~> "y" ~> Logic.not (Equality.equal (var "x") (var "y")))
