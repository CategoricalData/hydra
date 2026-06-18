-- | Primitive declarations for the hydra.lib.effects namespace.

module Hydra.Sources.Kernel.Lib.Effects where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Types         as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), fail, map, pure)


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
      primImpure "bind" "Sequence two effectful computations." bindSig [
        "bind(e, f) describes an effectful computation which first interprets e, then passes its successful\
        \ result to f to obtain the next effect.",
        "Host interpreters provide the operational meaning; Hydra uses the type effect<T> to keep the\
        \ effect boundary explicit."],
      primImpure "fail" "Create an effectful computation which fails with a message." failSig [
        "fail(message) describes an effectful computation which fails during host interpretation.",
        "The initial error payload is a string; a richer error type may be introduced later."],
      primImpure "map" "Map a pure function over the result of an effect." mapSig [
        "map(f, e) describes an effectful computation which interprets e and applies f to its successful\
        \ result.",
        "Equivalent to bind(e, \\x -> pure(f x))."],
      primImpure "pure" "Lift a pure value into an effect." pureSig [
        "pure(x) describes an effectful computation which succeeds with x without requiring host\
        \ interaction."]]

primImpure :: String -> String -> TermSignature -> [String] -> Definition
primImpure localName description s comments =
  toImpurePrimitive description s (unqualifyName (QualifiedName (Just ns) localName)) comments

sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

effect :: Type -> Type
effect = TypeEffect

tx, ty :: Type
tx = Types.var "x"
ty = Types.var "y"

-- bind : forall a b. effect<a> -> (a -> effect<b>) -> effect<b>
bindSig :: TermSignature
bindSig = sig $ TypeScheme [Name "x", Name "y"]
  (effect tx Types.~> (tx Types.~> effect ty) Types.~> effect ty) Nothing

-- fail : forall a. string -> effect<a>
failSig :: TermSignature
failSig = sig $ TypeScheme [Name "x"]
  (Types.string Types.~> effect tx) Nothing

-- map : forall a b. (a -> b) -> effect<a> -> effect<b>
mapSig :: TermSignature
mapSig = sig $ TypeScheme [Name "x", Name "y"]
  ((tx Types.~> ty) Types.~> effect tx Types.~> effect ty) Nothing

-- pure : forall a. a -> effect<a>
pureSig :: TermSignature
pureSig = sig $ TypeScheme [Name "x"]
  (tx Types.~> effect tx) Nothing
