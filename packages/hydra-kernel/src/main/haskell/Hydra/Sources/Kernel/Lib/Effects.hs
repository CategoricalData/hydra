-- | Primitive declarations for the hydra.lib.effects namespace.

module Hydra.Sources.Kernel.Lib.Effects where

import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap     as Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms as Phantoms hiding (apply, compose, map)
import qualified Hydra.Overlay.Haskell.Dsl.Types         as Types
import           Hydra.Overlay.Haskell.Dsl.Types         (effect)
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), foldl, map, pure)


ns :: ModuleName
ns = ModuleName "hydra.lib.effects"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.effects module.")}
  where
    definitions = [apply, bind, compose, foldList, map, mapList, mapOptional, mapSet, pure]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = impurePrimitiveInModule module_

-- Shared type variables
tx, ty, tz :: Type
tx = Types.var "x"
ty = Types.var "y"
tz = Types.var "z"

apply :: PrimitiveDefinition
apply = define "apply" "Applicative apply for effects."
  (sigWithParams [("ef", "the effect producing the function to apply"),
                  ("ex", "the effect producing the argument")]
    $ TypeScheme [Name "x", Name "y"]
    (effect (tx Types.~> ty) Types.~> effect tx Types.~> effect ty) Nothing)
  ["apply(ef, ex) describes an effectful computation which first interprets ef to produce a\
  \ function, then interprets ex to produce an argument, and returns the result of applying the\
  \ function to the argument."]

bind :: PrimitiveDefinition
bind = define "bind" "Sequence two effectful computations."
  (sigWithParams [("e", "the effect to interpret first"),
                  ("f", "the function producing the next effect from e's result")]
    $ TypeScheme [Name "x", Name "y"]
    (effect tx Types.~> (tx Types.~> effect ty) Types.~> effect ty) Nothing)
  ["bind(e, f) describes an effectful computation which first interprets e, then passes its\
  \ result to f to obtain the next effect.",
   "Host interpreters provide the operational meaning; Hydra uses the type effect<t> to keep the\
   \ effect boundary explicit."]

compose :: PrimitiveDefinition
compose = define "compose" "Kleisli composition for effects."
  (sigWithParams [("f", "the first effect-returning function"),
                  ("g", "the second effect-returning function"),
                  ("x", "the input to the composed computation")]
    $ TypeScheme [Name "x", Name "y", Name "z"]
    ((tx Types.~> effect ty) Types.~> (ty Types.~> effect tz) Types.~> tx Types.~> effect tz) Nothing)
  ["compose(f, g, x) describes an effectful computation equivalent to bind(f(x), g)."]

foldList :: PrimitiveDefinition
foldList = define "foldList" "Left-fold over a list with an effect-returning function."
  (sigWithParams [("f", "the effect-returning combining function"),
                  ("acc0", "the initial accumulator value"),
                  ("xs", "the list to fold over")]
    $ TypeScheme [Name "x", Name "y"]
    ((tx Types.~> ty Types.~> effect tx) Types.~> tx Types.~> Types.list ty Types.~> effect tx) Nothing)
  ["foldList(f, acc0, xs) describes an effectful left fold over xs, sequencing applications of f from\
  \ left to right."]

map :: PrimitiveDefinition
map = define "map" "Map a pure function over the result of an effect."
  (sigWithParams [("f", "the pure function to apply to the effect's result"),
                  ("e", "the effect to interpret")]
    $ TypeScheme [Name "x", Name "y"]
    ((tx Types.~> ty) Types.~> effect tx Types.~> effect ty) Nothing)
  ["map(f, e) describes an effectful computation which interprets e and applies f to its result.",
   "Equivalent to bind(e, \\x -> pure(f x))."]

mapList :: PrimitiveDefinition
mapList = define "mapList" "Map an effect-returning function over a list."
  (sigWithParams [("f", "the effect-returning function to apply to each element"),
                  ("xs", "the list to map over")]
    $ TypeScheme [Name "x", Name "y"]
    ((tx Types.~> effect ty) Types.~> Types.list tx Types.~> effect (Types.list ty)) Nothing)
  ["mapList(f, xs) describes an effectful computation which applies f to each element of xs from\
  \ left to right and collects the results."]

mapOptional :: PrimitiveDefinition
mapOptional = define "mapOptional" "Map an effect-returning function over an optional."
  (sigWithParams [("f", "the effect-returning function to apply to the contained value"),
                  ("m", "the optional to map over")]
    $ TypeScheme [Name "x", Name "y"]
    ((tx Types.~> effect ty) Types.~> Types.optional tx Types.~> effect (Types.optional ty)) Nothing)
  ["mapOptional(f, m) describes an effectful computation which returns none when m is none, or\
  \ applies f to the contained value and wraps the result in given."]

mapSet :: PrimitiveDefinition
mapSet = define "mapSet" "Map an effect-returning function over a set."
  (sigWithParams [("f", "the effect-returning function to apply to each element"),
                  ("s", "the set to map over")]
    $ Types.polyConstrained [("x", [Name "ordering"]), ("y", [Name "ordering"])]
    ((tx Types.~> effect ty) Types.~> Types.set tx Types.~> effect (Types.set ty)))
  ["mapSet(f, s) describes an effectful computation which applies f to each element of s, sequencing\
  \ the effects in ascending element order (the order is normative, since the effects are observable),\
  \ and collects the results as a set, deduplicated by the result type's ordering.",
   "Requires 'ordering' constraints on both element types (the set type contract)."]

pure :: PrimitiveDefinition
pure = define "pure" "Lift a pure value into an effect."
  (sigWithParams [("x", "the pure value to lift into an effect")]
    $ TypeScheme [Name "x"] (tx Types.~> effect tx) Nothing)
  ["pure(x) describes an effectful computation which succeeds with x without requiring host\
  \ interaction."]
