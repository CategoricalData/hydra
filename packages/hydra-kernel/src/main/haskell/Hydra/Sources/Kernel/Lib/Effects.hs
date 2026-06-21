-- | Primitive declarations for the hydra.lib.effects namespace.

module Hydra.Sources.Kernel.Lib.Effects where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import           Hydra.Dsl.Prims         (sig)
import qualified Hydra.Dsl.Types         as Types
import           Hydra.Dsl.Types         (effect)
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), foldl, map, pure)


ns :: ModuleName
ns = ModuleName "hydra.lib.effects"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.effects module.")}
  where
    definitions = [
      primImpure "apply" "Applicative apply for effects." applySig [
        "apply(ef, ex) describes an effectful computation which first interprets ef to produce a\
        \ function, then interprets ex to produce an argument, and returns the result of applying the\
        \ function to the argument."],
      primImpure "bind" "Sequence two effectful computations." bindSig [
        "bind(e, f) describes an effectful computation which first interprets e, then passes its\
        \ result to f to obtain the next effect.",
        "Host interpreters provide the operational meaning; Hydra uses the type effect<t> to keep the\
        \ effect boundary explicit."],
      primImpure "compose" "Kleisli composition for effects." composeSig [
        "compose(f, g, x) describes an effectful computation equivalent to bind(f(x), g)."],
      primImpure "foldl" "Left-fold over a list with an effect-returning function." foldlSig [
        "foldl(f, acc0, xs) describes an effectful left fold over xs, sequencing applications of f from\
        \ left to right."],
      primImpure "map" "Map a pure function over the result of an effect." mapSig [
        "map(f, e) describes an effectful computation which interprets e and applies f to its result.",
        "Equivalent to bind(e, \\x -> pure(f x))."],
      primImpure "mapList" "Map an effect-returning function over a list." mapListSig [
        "mapList(f, xs) describes an effectful computation which applies f to each element of xs from\
        \ left to right and collects the results."],
      primImpure "mapOptional" "Map an effect-returning function over an optional." mapOptionalSig [
        "mapOptional(f, m) describes an effectful computation which returns none when m is none, or\
        \ applies f to the contained value and wraps the result in given."],
      primImpure "pure" "Lift a pure value into an effect." pureSig [
        "pure(x) describes an effectful computation which succeeds with x without requiring host\
        \ interaction."]]

primImpure :: String -> String -> TermSignature -> [String] -> Definition
primImpure = Phantoms.impurePrimitiveInModule ns

tx, ty, tz :: Type
tx = Types.var "x"
ty = Types.var "y"
tz = Types.var "z"

-- apply : forall a b. effect<(a -> b)> -> effect<a> -> effect<b>
applySig :: TermSignature
applySig = sig $ TypeScheme [Name "x", Name "y"]
  (effect (tx Types.~> ty) Types.~> effect tx Types.~> effect ty) Nothing

-- bind : forall a b. effect<a> -> (a -> effect<b>) -> effect<b>
bindSig :: TermSignature
bindSig = sig $ TypeScheme [Name "x", Name "y"]
  (effect tx Types.~> (tx Types.~> effect ty) Types.~> effect ty) Nothing

-- compose : forall a b c. (a -> effect<b>) -> (b -> effect<c>) -> a -> effect<c>
composeSig :: TermSignature
composeSig = sig $ TypeScheme [Name "x", Name "y", Name "z"]
  ((tx Types.~> effect ty) Types.~> (ty Types.~> effect tz) Types.~> tx Types.~> effect tz) Nothing

-- foldl : forall a b. (a -> b -> effect<a>) -> a -> list<b> -> effect<a>
foldlSig :: TermSignature
foldlSig = sig $ TypeScheme [Name "x", Name "y"]
  ((tx Types.~> ty Types.~> effect tx) Types.~> tx Types.~> Types.list ty Types.~> effect tx) Nothing

-- map : forall a b. (a -> b) -> effect<a> -> effect<b>
mapSig :: TermSignature
mapSig = sig $ TypeScheme [Name "x", Name "y"]
  ((tx Types.~> ty) Types.~> effect tx Types.~> effect ty) Nothing

-- mapList : forall a b. (a -> effect<b>) -> list<a> -> effect<list<b>>
mapListSig :: TermSignature
mapListSig = sig $ TypeScheme [Name "x", Name "y"]
  ((tx Types.~> effect ty) Types.~> Types.list tx Types.~> effect (Types.list ty)) Nothing

-- mapOptional : forall a b. (a -> effect<b>) -> optional<a> -> effect<optional<b>>
mapOptionalSig :: TermSignature
mapOptionalSig = sig $ TypeScheme [Name "x", Name "y"]
  ((tx Types.~> effect ty) Types.~> Types.optional tx Types.~> effect (Types.optional ty)) Nothing

-- pure : forall a. a -> effect<a>
pureSig :: TermSignature
pureSig = sig $ TypeScheme [Name "x"]
  (tx Types.~> effect tx) Nothing
