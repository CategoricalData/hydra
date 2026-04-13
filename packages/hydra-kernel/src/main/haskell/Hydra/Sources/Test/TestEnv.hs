-- | DSL declarations for hydra.test.testEnv.
--
-- This module declares the FQNs hydra.test.testEnv.testGraph and
-- hydra.test.testEnv.testContext so the code generator can resolve
-- references to them (from Meta/Testing.hs) during type inference. It is
-- included in the generator's universe for type resolution but is NOT
-- emitted to src/gen-test/haskell, so the hand-written
-- src/gen-test/haskell/Hydra/Test/TestEnv.hs is the source of truth at
-- the Haskell level.
--
-- The bodies here are stubs (emptyGraph/emptyContext). The real runtime
-- values come from the hand-written Haskell module and include primitives
-- and kernel term bindings, which are inherently host-language specific
-- and cannot be expressed at the DSL level.

module Hydra.Sources.Test.TestEnv where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import Hydra.Sources.Kernel.Types.All


ns :: Namespace
ns = Namespace "hydra.test.testEnv"

module_ :: Module
module_ = Module ns definitions
    [Lexical.ns]
    kernelTypesNamespaces $
    Just ("Type-level declarations for the hand-written Hydra.Test.TestEnv module.")
  where
   definitions = [
     Phantoms.toDefinition testGraph,
     Phantoms.toDefinition testContext]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

-- | Stub: the real testGraph lives in hand-written Hydra.Test.TestEnv
testGraph :: TTermDefinition Graph
testGraph = define "testGraph" $ asTerm Lexical.emptyGraph

-- | Stub: the real testContext lives in hand-written Hydra.Test.TestEnv
testContext :: TTermDefinition Context
testContext = define "testContext" $ asTerm Lexical.emptyContext
