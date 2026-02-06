-- | Test cases for type and term substitution operations
module Hydra.Sources.Test.Substitution where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Dsl.Meta.Types as Ty
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes


ns :: Namespace
ns = Namespace "hydra.test.substitution"

module_ :: Module
module_ = Module ns elements
    []
    KernelTypes.kernelTypesNamespaces
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
    Ty.string
    Ty.string,

  -- Substitute a type variable with a concrete type
  substInTypeCase "substitute type variable with int32"
    (subst [("a", Ty.int32)])
    (Ty.var "a")
    Ty.int32,

  -- Non-matching variable is unchanged
  substInTypeCase "non-matching variable unchanged"
    (subst [("a", Ty.int32)])
    (Ty.var "b")
    (Ty.var "b"),

  -- Substitute in function type
  substInTypeCase "substitute in function domain"
    (subst [("a", Ty.int32)])
    (Ty.function (Ty.var "a") Ty.string)
    (Ty.function Ty.int32 Ty.string),

  substInTypeCase "substitute in function codomain"
    (subst [("a", Ty.string)])
    (Ty.function Ty.int32 (Ty.var "a"))
    (Ty.function Ty.int32 Ty.string),

  -- Substitute in list type
  substInTypeCase "substitute in list element type"
    (subst [("a", Ty.int32)])
    (Ty.list (Ty.var "a"))
    (Ty.list Ty.int32),

  -- Substitute in optional type
  substInTypeCase "substitute in optional type"
    (subst [("a", Ty.string)])
    (Ty.optional (Ty.var "a"))
    (Ty.optional Ty.string),

  -- Substitute in pair type
  substInTypeCase "substitute in pair type both sides"
    (subst [("a", Ty.int32)])
    (Ty.pair (Ty.var "a") (Ty.var "a"))
    (Ty.pair Ty.int32 Ty.int32),

  -- Substitute in either type
  substInTypeCase "substitute in either type"
    (subst [("a", Ty.string)])
    (Ty.either_ (Ty.var "a") Ty.int32)
    (Ty.either_ Ty.string Ty.int32),

  -- Substitute in map type
  substInTypeCase "substitute in map key type"
    (subst [("k", Ty.string)])
    (Ty.map (Ty.var "k") Ty.int32)
    (Ty.map Ty.string Ty.int32),

  -- Substitute in set type
  substInTypeCase "substitute in set type"
    (subst [("a", Ty.int32)])
    (Ty.set (Ty.var "a"))
    (Ty.set Ty.int32),

  -- Nested substitution
  substInTypeCase "nested substitution in list of pairs"
    (subst [("a", Ty.int32)])
    (Ty.list (Ty.pair (Ty.var "a") Ty.string))
    (Ty.list (Ty.pair Ty.int32 Ty.string)),

  -- Multiple substitutions
  substInTypeCase "multiple substitutions"
    (subst [("a", Ty.int32), ("b", Ty.string)])
    (Ty.pair (Ty.var "a") (Ty.var "b"))
    (Ty.pair Ty.int32 Ty.string),

  -- Forall type: bound variable should not be substituted in body
  substInTypeCase "forAll bound variable not substituted"
    (subst [("a", Ty.int32)])
    (Ty.forAll "a" (Ty.function (Ty.var "a") (Ty.var "a")))
    (Ty.forAll "a" (Ty.function (Ty.var "a") (Ty.var "a"))),

  -- Forall type: free variable in body should be substituted
  substInTypeCase "forAll free variable substituted"
    (subst [("b", Ty.string)])
    (Ty.forAll "a" (Ty.function (Ty.var "a") (Ty.var "b")))
    (Ty.forAll "a" (Ty.function (Ty.var "a") Ty.string))]

-- ============================================================
-- All tests
-- ============================================================

allTests :: TBinding TestGroup
allTests = define "allTests" $
    doc "Test cases for type and term substitution operations" $
    supergroup "substitution" [
      substInTypeTests]
