-- | Primitive declarations for the hydra.lib.functions namespace.
{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Lib.Functions where

import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap          as Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms hiding (compose, const, flip, identity)
import qualified Hydra.Overlay.Haskell.Dsl.Types              as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), const, flip)


ns :: ModuleName
ns = ModuleName "hydra.lib.functions"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.functions module.")}
  where
    definitions = [compose, const, flip, identity]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

defineWithDefault :: String -> String -> TermSignature -> [String] -> TypedTerm a -> PrimitiveDefinition
defineWithDefault = primitiveWithDefaultInModule module_

-- Shared type variables
t1, t2, t3 :: Type
t1 = Types.var "t1"
t2 = Types.var "t2"
t3 = Types.var "t3"

compose :: PrimitiveDefinition
compose = defineWithDefault "compose" "Compose two functions."
  (sigWithParams [("g", "the outer function, applied to the result of f"),
                  ("f", "the inner function, applied to x first"),
                  ("x", "the argument to which f is applied")] $ TypeScheme [Name "t1", Name "t2", Name "t3"]
    ((t2 Types.~> t3) Types.~> (t1 Types.~> t2) Types.~> t1 Types.~> t3) Nothing)
  ["compose(g, f, x) = g(f(x)); this defining equation is the specification.",
   "The outer function comes first: f is applied to x, and g is applied to the result.",
   "This is ordinary function composition, distinct from the Kleisli compose primitives of the\
   \ monad modules.",
   "Total. Corresponds to Haskell's (.) :: (b -> c) -> (a -> b) -> a -> c."]
  ("g" ~> "f" ~> "x" ~> (var "g" @@ (var "f" @@ var "x")))

const :: PrimitiveDefinition
const = defineWithDefault "const" "Return the first argument, ignoring the second."
  (sigWithParams [("x", "the value to return"),
                  ("y", "the ignored second argument")] $ TypeScheme [Name "t1", Name "t2"]
    (t1 Types.~> t2 Types.~> t1) Nothing)
  ["const(x, y) = x; this defining equation is the specification.",
   "Partially applied, const(x) is the constant function which returns x on every input.",
   "Total. Corresponds to Haskell's const :: a -> b -> a."]
  ("x" ~> "y" ~> var "x")

flip :: PrimitiveDefinition
flip = defineWithDefault "flip" "Swap the argument order of a binary function."
  (sigWithParams [("f", "the binary function whose argument order is swapped"),
                  ("x", "the argument passed as f's second argument"),
                  ("y", "the argument passed as f's first argument")] $ TypeScheme [Name "t1", Name "t2", Name "t3"]
    ((t1 Types.~> t2 Types.~> t3) Types.~> t2 Types.~> t1 Types.~> t3) Nothing)
  ["flip(f, x, y) = f(y, x); this defining equation is the specification.",
   "Total. Corresponds to Haskell's flip :: (a -> b -> c) -> b -> a -> c."]
  ("f" ~> "x" ~> "y" ~> (var "f" @@ var "y" @@ var "x"))

identity :: PrimitiveDefinition
identity = defineWithDefault "identity" "Return the argument unchanged."
  (sigWithParams [("x", "the value to return unchanged")] $ TypeScheme [Name "t1"] (t1 Types.~> t1) Nothing)
  ["identity(x) = x; this defining equation is the specification. The polymorphic identity function.",
   "identity is the unit of compose: compose(identity, f) and compose(f, identity) are both f.",
   "Total. Corresponds to Haskell's id :: a -> a."]
  ("x" ~> var "x")
