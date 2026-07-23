-- | Primitive declarations for the hydra.lib.logic namespace.
{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Lib.Logic where

import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Lib.Logic    as Logic
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), and)


ns :: ModuleName
ns = ModuleName "hydra.lib.logic"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.logic module.")}
  where
    definitions = [and, ifElse, not_, or_]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

defineWithDefault :: String -> String -> TermSignature -> [String] -> TypedTerm a -> PrimitiveDefinition
defineWithDefault = primitiveWithDefaultInModule module_

and :: PrimitiveDefinition
and = defineWithDefault "and" "Compute the logical AND of two boolean values."
  (sigWithParams [("p", "the first boolean operand"),
                  ("q", "the second boolean operand")] $ TypeScheme [] (Types.boolean Types.~> Types.boolean Types.~> Types.boolean) Nothing)
  ["and(p, q) returns true iff both p and q are true.",
   "Evaluation is strict in both arguments at the primitive level; for short-circuiting behavior, use\
   \ ifElse.",
   "Total. Corresponds to Haskell's (&&) :: Bool -> Bool -> Bool (but without short-circuit evaluation)."]
  ("a" ~> "b" ~> Logic.ifElse (var "a") (var "b" :: TypedTerm Bool) false)

-- ifElse is lazy in its then/else branches (positions 1, 2): only the
-- selected branch is evaluated. The condition (position 0) is strict.
ifElse :: PrimitiveDefinition
ifElse = define "ifElse" "Compute a conditional expression."
  (markLazyParams [1, 2] $ sigWithParams [("p", "the boolean condition"),
                                          ("t", "the value returned when p is true"),
                                          ("f", "the value returned when p is false")] $ TypeScheme [Name "x"]
    (Types.boolean Types.~> Types.var "x" Types.~> Types.var "x" Types.~> Types.var "x")
    Nothing)
  ["ifElse(p, t, f) returns t if p is true, or f if p is false.",
   "The unselected branch is not necessarily evaluated; ifElse is the standard way to express\
   \ short-circuiting boolean logic and branching in Hydra.",
   "Total. Corresponds to Haskell's if/then/else."]

not_ :: PrimitiveDefinition
not_ = defineWithDefault "not" "Compute the logical NOT of a boolean value."
  (sigWithParams [("p", "the boolean value to negate")] $ TypeScheme [] (Types.boolean Types.~> Types.boolean) Nothing)
  ["not(p) returns false if p is true, or true if p is false.",
   "Total. Corresponds to Haskell's not :: Bool -> Bool."]
  ("a" ~> Logic.ifElse (var "a") false true)

or_ :: PrimitiveDefinition
or_ = defineWithDefault "or" "Compute the logical OR of two boolean values."
  (sigWithParams [("p", "the first boolean operand"),
                  ("q", "the second boolean operand")] $ TypeScheme [] (Types.boolean Types.~> Types.boolean Types.~> Types.boolean) Nothing)
  ["or(p, q) returns true iff at least one of p and q is true.",
   "Evaluation is strict in both arguments at the primitive level; for short-circuiting behavior, use\
   \ ifElse.",
   "Total. Corresponds to Haskell's (||) :: Bool -> Bool -> Bool (but without short-circuit evaluation)."]
  ("a" ~> "b" ~> Logic.ifElse (var "a") true (var "b" :: TypedTerm Bool))
