-- | Synthetic workload — polymorphic chain — for cross-host inference benchmarking.
--
-- One of several "series" under @Hydra.Sources.Bench@. Where 'LinearChain'
-- exercises depth at fixed monomorphic types, this series adds parameterised
-- (@forall a@) signatures: each @polyWalker_K@ has type @Maybe a -> Maybe a@,
-- and the chain recurses on the same polymorphic shape. Inference must
-- instantiate the @forall@ at each cross-def call site.
--
-- Patterns exercised in addition to those in 'LinearChain':
--
--   1. Type-scheme polymorphism (every def's signature is @forall a@)
--   2. Type-variable propagation through @Maybes.bind@ and lambdas
--
-- Adjust 'numPolyWalkers' or 'polyWalkerBody' and re-run the kernel-haskell
-- sync to regenerate the JSON used by all hosts.
module Hydra.Sources.Bench.PolymorphicChain where

import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import           Hydra.Sources.Kernel.Types.All

import qualified Data.List                   as L
import           Prelude                     hiding ((++))

ns :: ModuleName
ns = ModuleName "hydra.bench.polymorphicChain"

-- | Number of polyWalker definitions in the chain. Bench runners take prefixes
-- (e.g. first 10 / 25 / 50 / 100) to build scaling curves.
numPolyWalkers :: Int
numPolyWalkers = 400

-- | Hydra-level fully-qualified name for the @k@th polyWalker.
polyWalkerName :: Int -> Name
polyWalkerName k = Name $ L.concat ["hydra.bench.polymorphicChain.polyWalker", show k]

-- | Reference to the @k@th polyWalker. We give it the full polymorphic type;
-- inference will instantiate @a@ at each call site.
polyWalkerRef :: Int -> TTerm (Maybe a -> Maybe a)
polyWalkerRef k = TTerm $ TermVariable (polyWalkerName k)

-- | Body of @polyWalker_K@.
--
-- Base case @k = 0@ is the identity on @Maybe a@.
-- Inductive case @k > 0@ chains through @Maybes.bind@ and @Maybes.maybe@,
-- recursing on @polyWalker_(k-1)@.
polyWalkerBody :: Int -> TTerm (Maybe a -> Maybe a)
polyWalkerBody 0 =
  "m" ~> var "m"
polyWalkerBody k =
  "m" ~>
    -- Pattern: a let-binding that holds a polymorphic Maybe a, then bind
    -- through. The forall a propagates through the bind.
    "stepped" <~ (Maybes.bind (var "m") ("v" ~> polyWalkerRef (k-1) @@ just (var "v"))) $
    Maybes.maybe nothing ("w" ~> just (var "w")) (var "stepped")

-- | Build the kth polyWalker definition.
mkPolyWalker :: Int -> TTermDefinition (Maybe a -> Maybe a)
mkPolyWalker k = define (L.concat ["polyWalker", show k])
  $ doc (L.concat ["Polymorphic walker level ", show k, "; recurses to polyWalker", show (max 0 (k-1)), "."])
  $ polyWalkerBody k

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
  moduleName = ns,
  moduleDefinitions = definitions,
  moduleDependencies = unqualifiedDep <$> (kernelTypesModuleNames),
  moduleDescription = Just "Polymorphic-chain inference benchmark. polyWalker_K :: Maybe a -> Maybe a; chains via Maybes.bind, instantiating forall a at each cross-def call site."
  }
  where
    definitions = [toDefinition (mkPolyWalker k) | k <- [0 .. numPolyWalkers - 1]]
