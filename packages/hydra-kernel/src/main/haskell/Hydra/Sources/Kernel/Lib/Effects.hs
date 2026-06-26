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
    definitions = [apply, bind, compose, foldl, map, mapList, mapOptional, pure]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = impurePrimitiveInModule module_

-- Shared type variables
tx, ty, tz :: Type
tx = Types.var "x"
ty = Types.var "y"
tz = Types.var "z"

apply :: PrimitiveDefinition
apply = define "apply" "Applicative apply for effects."
  (sig $ TypeScheme [Name "x", Name "y"]
    (effect (tx Types.~> ty) Types.~> effect tx Types.~> effect ty) Nothing)
  ["apply(ef, ex) describes an effectful computation which first interprets ef to produce a\
  \ function, then interprets ex to produce an argument, and returns the result of applying the\
  \ function to the argument."]

bind :: PrimitiveDefinition
bind = define "bind" "Sequence two effectful computations."
  (sig $ TypeScheme [Name "x", Name "y"]
    (effect tx Types.~> (tx Types.~> effect ty) Types.~> effect ty) Nothing)
  ["bind(e, f) describes an effectful computation which first interprets e, then passes its\
  \ result to f to obtain the next effect.",
   "Host interpreters provide the operational meaning; Hydra uses the type effect<t> to keep the\
   \ effect boundary explicit."]

compose :: PrimitiveDefinition
compose = define "compose" "Kleisli composition for effects."
  (sig $ TypeScheme [Name "x", Name "y", Name "z"]
    ((tx Types.~> effect ty) Types.~> (ty Types.~> effect tz) Types.~> tx Types.~> effect tz) Nothing)
  ["compose(f, g, x) describes an effectful computation equivalent to bind(f(x), g)."]

foldl :: PrimitiveDefinition
foldl = define "foldl" "Left-fold over a list with an effect-returning function."
  (sig $ TypeScheme [Name "x", Name "y"]
    ((tx Types.~> ty Types.~> effect tx) Types.~> tx Types.~> Types.list ty Types.~> effect tx) Nothing)
  ["foldl(f, acc0, xs) describes an effectful left fold over xs, sequencing applications of f from\
  \ left to right."]

map :: PrimitiveDefinition
map = define "map" "Map a pure function over the result of an effect."
  (sig $ TypeScheme [Name "x", Name "y"]
    ((tx Types.~> ty) Types.~> effect tx Types.~> effect ty) Nothing)
  ["map(f, e) describes an effectful computation which interprets e and applies f to its result.",
   "Equivalent to bind(e, \\x -> pure(f x))."]

mapList :: PrimitiveDefinition
mapList = define "mapList" "Map an effect-returning function over a list."
  (sig $ TypeScheme [Name "x", Name "y"]
    ((tx Types.~> effect ty) Types.~> Types.list tx Types.~> effect (Types.list ty)) Nothing)
  ["mapList(f, xs) describes an effectful computation which applies f to each element of xs from\
  \ left to right and collects the results."]

mapOptional :: PrimitiveDefinition
mapOptional = define "mapOptional" "Map an effect-returning function over an optional."
  (sig $ TypeScheme [Name "x", Name "y"]
    ((tx Types.~> effect ty) Types.~> Types.optional tx Types.~> effect (Types.optional ty)) Nothing)
  ["mapOptional(f, m) describes an effectful computation which returns none when m is none, or\
  \ applies f to the contained value and wraps the result in given."]

pure :: PrimitiveDefinition
pure = define "pure" "Lift a pure value into an effect."
  (sig $ TypeScheme [Name "x"] (tx Types.~> effect tx) Nothing)
  ["pure(x) describes an effectful computation which succeeds with x without requiring host\
  \ interaction."]
