-- | Test cases for type and term substitution operations
module Hydra.Sources.Test.Substitution where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Hydra.Testing


ns :: Namespace
ns = Namespace "hydra.test.substitution"

module_ :: Module
module_ = Module ns elements
    []
    kernelTypesNamespaces
    (Just "Test cases for type and term substitution operations")
  where
    elements = [Phantoms.toBinding allTests]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

-- Helper to build names
nm :: String -> TTerm Name
nm s = Core.name $ Phantoms.string s

-- Helper for building substitution pairs
subst :: [(String, TTerm Type)] -> TTerm [(Name, Type)]
subst pairs = Phantoms.list [Phantoms.pair (nm n) t | (n, t) <- pairs]

-- ============================================================
-- substInType tests
-- ============================================================

substInTypeTests :: TTerm TestGroup
substInTypeTests = subgroup "substInType" [
  -- Empty substitution returns type unchanged
  substInTypeCase "empty substitution returns type unchanged"
    (subst [])
    T.string
    T.string,

  -- Substitute a type variable with a concrete type
  substInTypeCase "substitute type variable with int32"
    (subst [("a", T.int32)])
    (T.var "a")
    T.int32,

  -- Non-matching variable is unchanged
  substInTypeCase "non-matching variable unchanged"
    (subst [("a", T.int32)])
    (T.var "b")
    (T.var "b"),

  -- Substitute in function type
  substInTypeCase "substitute in function domain"
    (subst [("a", T.int32)])
    (T.function (T.var "a") T.string)
    (T.function T.int32 T.string),

  substInTypeCase "substitute in function codomain"
    (subst [("a", T.string)])
    (T.function T.int32 (T.var "a"))
    (T.function T.int32 T.string),

  -- Substitute in list type
  substInTypeCase "substitute in list element type"
    (subst [("a", T.int32)])
    (T.list (T.var "a"))
    (T.list T.int32),

  -- Substitute in optional type
  substInTypeCase "substitute in optional type"
    (subst [("a", T.string)])
    (T.optional (T.var "a"))
    (T.optional T.string),

  -- Substitute in pair type
  substInTypeCase "substitute in pair type both sides"
    (subst [("a", T.int32)])
    (T.pair (T.var "a") (T.var "a"))
    (T.pair T.int32 T.int32),

  -- Substitute in either type
  substInTypeCase "substitute in either type"
    (subst [("a", T.string)])
    (T.either_ (T.var "a") T.int32)
    (T.either_ T.string T.int32),

  -- Substitute in map type
  substInTypeCase "substitute in map key type"
    (subst [("k", T.string)])
    (T.map (T.var "k") T.int32)
    (T.map T.string T.int32),

  -- Substitute in set type
  substInTypeCase "substitute in set type"
    (subst [("a", T.int32)])
    (T.set (T.var "a"))
    (T.set T.int32),

  -- Nested substitution
  substInTypeCase "nested substitution in list of pairs"
    (subst [("a", T.int32)])
    (T.list (T.pair (T.var "a") T.string))
    (T.list (T.pair T.int32 T.string)),

  -- Multiple substitutions
  substInTypeCase "multiple substitutions"
    (subst [("a", T.int32), ("b", T.string)])
    (T.pair (T.var "a") (T.var "b"))
    (T.pair T.int32 T.string),

  -- Forall type: bound variable should not be substituted in body
  substInTypeCase "forAll bound variable not substituted"
    (subst [("a", T.int32)])
    (T.forAll "a" (T.function (T.var "a") (T.var "a")))
    (T.forAll "a" (T.function (T.var "a") (T.var "a"))),

  -- Forall type: free variable in body should be substituted
  substInTypeCase "forAll free variable substituted"
    (subst [("b", T.string)])
    (T.forAll "a" (T.function (T.var "a") (T.var "b")))
    (T.forAll "a" (T.function (T.var "a") T.string))]

-- ============================================================
-- All tests
-- ============================================================

allTests :: TBinding TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for type and term substitution operations" $
    supergroup "substitution" [
      substInTypeTests]
