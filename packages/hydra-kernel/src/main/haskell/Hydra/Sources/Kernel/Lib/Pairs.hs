-- | Primitive declarations for the hydra.lib.pairs namespace.

module Hydra.Sources.Kernel.Lib.Pairs where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.lib.pairs"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModuleName ns

-- Local convenience: build a TermSignature from a TypeScheme.
sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

-- Local convenience: build a no-default primitive Definition from a local name.
primNoDef :: String -> String -> TermSignature -> Definition
primNoDef localName description s =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName))

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleDescription = Just "Primitives in the hydra.lib.pairs namespace."}
  where
    definitions = [
      toPrimitive "Map over both elements of a pair." bimapSig bimap_,
      primNoDef "first" "Get the first element of a pair." firstSig,
      primNoDef "second" "Get the second element of a pair." secondSig]

-- Signatures (derived from Hydra.Sources.Libraries primN declarations).

-- bimap : forall a b c d. (a -> c) -> (b -> d) -> (a, b) -> (c, d)
bimapSig :: TermSignature
bimapSig = sig $ TypeScheme [Name "a", Name "b", Name "c", Name "d"]
  ((Types.var "a" Types.~> Types.var "c") Types.~>
   (Types.var "b" Types.~> Types.var "d") Types.~>
   Types.pair (Types.var "a") (Types.var "b") Types.~>
   Types.pair (Types.var "c") (Types.var "d"))
  Nothing

-- first : forall a b. (a, b) -> a
firstSig :: TermSignature
firstSig = sig $ TypeScheme [Name "a", Name "b"]
  (Types.pair (Types.var "a") (Types.var "b") Types.~> Types.var "a")
  Nothing

-- second : forall a b. (a, b) -> b
secondSig :: TermSignature
secondSig = sig $ TypeScheme [Name "a", Name "b"]
  (Types.pair (Types.var "a") (Types.var "b") Types.~> Types.var "b")
  Nothing

-- Default implementations.

-- bimap f g p = pair (f (first p)) (g (second p))
bimap_ :: TTermDefinition ((a -> c) -> (b -> d) -> (a, b) -> (c, d))
bimap_ = define "bimap" $
  doc "Map over both elements of a pair, defined in terms of first and second." $
  "f" ~> "g" ~> "p" ~> pair
    (var "f" @@ Pairs.first (var "p"))
    (var "g" @@ Pairs.second (var "p"))
