-- | Test cases for type and term substitution operations
module Hydra.Sources.Test.Substitution where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import           Hydra.Dsl.Meta.Phantoms      as Phantoms hiding ((++))
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Hydra.Testing
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Substitution as SubstitutionModule


ns :: Namespace
ns = Namespace "hydra.test.substitution"

module_ :: Module
module_ = Module ns definitions
    [ShowCore.ns, SubstitutionModule.ns]
    kernelTypesNamespaces
    (Just "Test cases for type and term substitution operations")
  where
    definitions = [Phantoms.toDefinition allTests]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

-- Helper to build names
nm :: String -> TTerm Name
nm s = Core.name $ string s

-- Helper for building substitution pairs
subst :: [(String, TTerm Type)] -> TTerm [(Name, Type)]
subst pairs = list [pair (nm n) t | (n, t) <- pairs]

-- | Apply substInType and show the result as a string
showSubstInType :: [(String, TTerm Type)] -> TTerm Type -> TTerm String
showSubstInType pairs inputType =
  ShowCore.type_ @@ (SubstitutionModule.substInType @@
    (wrap _TypeSubst (Maps.fromList (subst pairs))) @@ inputType)

-- | Universal substInType test case
substInTypeCase :: String -> [(String, TTerm Type)] -> TTerm Type -> TTerm Type -> TTerm TestCaseWithMetadata
substInTypeCase cname pairs input output =
  universalCase cname (showSubstInType pairs input) (ShowCore.type_ @@ output)

-- | Build a TypeScheme DSL value
scheme :: [String] -> TTerm Type -> TTerm TypeScheme
scheme vars body = Core.typeScheme (list [nm v | v <- vars]) body nothing

-- | Apply substInTypeScheme and render just the scheme's body type, which is
-- where the capture-avoidance behavior shows up. The quantifier list is
-- preserved unchanged so we test it separately via showSubstInTypeSchemeVars.
showSubstInTypeSchemeBody :: [(String, TTerm Type)] -> TTerm TypeScheme -> TTerm String
showSubstInTypeSchemeBody pairs inputScheme =
  ShowCore.type_ @@ (Core.typeSchemeType $ SubstitutionModule.substInTypeScheme @@
    (wrap _TypeSubst (Maps.fromList (subst pairs))) @@ inputScheme)

substInTypeSchemeBodyCase :: String -> [(String, TTerm Type)] -> TTerm TypeScheme -> TTerm Type -> TTerm TestCaseWithMetadata
substInTypeSchemeBodyCase cname pairs input expectedBody =
  universalCase cname (showSubstInTypeSchemeBody pairs input)
    (ShowCore.type_ @@ expectedBody)

-- ============================================================
-- substInType tests
-- ============================================================

substInTypeTests :: TTerm TestGroup
substInTypeTests = subgroup "substInType" [
  -- Empty substitution returns type unchanged
  substInTypeCase "empty substitution returns type unchanged"
    []
    T.string
    T.string,

  -- Substitute a type variable with a concrete type
  substInTypeCase "substitute type variable with int32"
    [("a", T.int32)]
    (T.var "a")
    T.int32,

  -- Non-matching variable is unchanged
  substInTypeCase "non-matching variable unchanged"
    [("a", T.int32)]
    (T.var "b")
    (T.var "b"),

  -- Substitute in function type
  substInTypeCase "substitute in function domain"
    [("a", T.int32)]
    (T.function (T.var "a") T.string)
    (T.function T.int32 T.string),

  substInTypeCase "substitute in function codomain"
    [("a", T.string)]
    (T.function T.int32 (T.var "a"))
    (T.function T.int32 T.string),

  -- Substitute in list type
  substInTypeCase "substitute in list element type"
    [("a", T.int32)]
    (T.list (T.var "a"))
    (T.list T.int32),

  -- Substitute in optional type
  substInTypeCase "substitute in optional type"
    [("a", T.string)]
    (T.optional (T.var "a"))
    (T.optional T.string),

  -- Substitute in pair type
  substInTypeCase "substitute in pair type both sides"
    [("a", T.int32)]
    (T.pair (T.var "a") (T.var "a"))
    (T.pair T.int32 T.int32),

  -- Substitute in either type
  substInTypeCase "substitute in either type"
    [("a", T.string)]
    (T.either_ (T.var "a") T.int32)
    (T.either_ T.string T.int32),

  -- Substitute in map type
  substInTypeCase "substitute in map key type"
    [("k", T.string)]
    (T.map (T.var "k") T.int32)
    (T.map T.string T.int32),

  -- Substitute in set type
  substInTypeCase "substitute in set type"
    [("a", T.int32)]
    (T.set (T.var "a"))
    (T.set T.int32),

  -- Nested substitution
  substInTypeCase "nested substitution in list of pairs"
    [("a", T.int32)]
    (T.list (T.pair (T.var "a") T.string))
    (T.list (T.pair T.int32 T.string)),

  -- Multiple substitutions
  substInTypeCase "multiple substitutions"
    [("a", T.int32), ("b", T.string)]
    (T.pair (T.var "a") (T.var "b"))
    (T.pair T.int32 T.string),

  -- Forall type: bound variable should not be substituted in body
  substInTypeCase "forAll bound variable not substituted"
    [("a", T.int32)]
    (T.forAll "a" (T.function (T.var "a") (T.var "a")))
    (T.forAll "a" (T.function (T.var "a") (T.var "a"))),

  -- Forall type: free variable in body should be substituted
  substInTypeCase "forAll free variable substituted"
    [("b", T.string)]
    (T.forAll "a" (T.function (T.var "a") (T.var "b")))
    (T.forAll "a" (T.function (T.var "a") T.string))]

-- ============================================================
-- substInTypeScheme tests: quantifier-shadowing
-- ============================================================

substInTypeSchemeTests :: TTerm TestGroup
substInTypeSchemeTests = subgroup "substInTypeScheme" [
  -- Bound variable in scheme's quantifier list must shadow the substitution.
  -- Without proper shadowing, {t0 -> Foo} applied to `forall [t0]. t0 -> t0`
  -- would incorrectly rewrite the body to `Foo -> Foo`, leaving the scheme
  -- with quantifier [t0] and body Foo -> Foo — a scheme with no free t0
  -- and an escaped Foo. Regression test for the incremental-inference
  -- unification bug (2026-04-24).
  substInTypeSchemeBodyCase "quantified variable shadows substitution"
    [("a", T.int32)]
    (scheme ["a"] (T.function (T.var "a") (T.var "a")))
    (T.function (T.var "a") (T.var "a")),

  -- Free variable in scheme body (not in quantifier list) is substituted.
  substInTypeSchemeBodyCase "free variable in body is substituted"
    [("b", T.string)]
    (scheme ["a"] (T.function (T.var "a") (T.var "b")))
    (T.function (T.var "a") T.string),

  -- Mixed: substitute frees, not quantified
  substInTypeSchemeBodyCase "mixed: free substituted, quantified shadowed"
    [("a", T.int32), ("b", T.string)]
    (scheme ["a"] (T.function (T.var "a") (T.var "b")))
    (T.function (T.var "a") T.string),

  -- Multiple quantifiers: all are shadowed
  substInTypeSchemeBodyCase "multiple quantifiers shadow substitution"
    [("a", T.int32), ("b", T.string)]
    (scheme ["a", "b"] (T.pair (T.var "a") (T.var "b")))
    (T.pair (T.var "a") (T.var "b")),

  -- Empty quantifier list: substitution applies normally
  substInTypeSchemeBodyCase "empty quantifiers: normal substitution"
    [("a", T.int32)]
    (scheme [] (T.var "a"))
    T.int32]

-- ============================================================
-- All tests
-- ============================================================

allTests :: TTermDefinition TestGroup
allTests = define "allTests" $
    doc "Test cases for type and term substitution operations" $
    supergroup "substitution" [
      substInTypeTests,
      substInTypeSchemeTests]
