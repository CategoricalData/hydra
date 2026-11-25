-- | Test cases for type and term substitution operations
module Hydra.Sources.Test.Substitution where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import           Hydra.Core.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                 as Testing
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core          as Core
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms      as Phantoms hiding ((++))
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types         as T
import qualified Hydra.Core.Dsl.Lib.Lists     as Lists
import qualified Hydra.Core.Dsl.Lib.Literals  as Literals
import qualified Hydra.Core.Dsl.Lib.Maps      as Maps
import qualified Hydra.Core.Dsl.Lib.Sets      as Sets
import qualified Hydra.Core.Dsl.Lib.Strings   as Strings
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Hydra.Core.Testing
import qualified Hydra.Sources.Kernel.Terms.Classes as Classes
import qualified Hydra.Sources.Kernel.Terms.Print.Core as PrintCore
import qualified Hydra.Sources.Kernel.Terms.Substitution as Substitution


ns :: ModuleName
ns = ModuleName "hydra.core.test.substitution"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([Classes.ns, PrintCore.ns, Substitution.ns] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata ((Just "Test cases for type and term substitution operations"))}
  where
    definitions = [Phantoms.toDefinition allTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    doc "Test cases for type and term substitution operations" $
    supergroup "substitution" [
      substInTypeTests,
      substInTypeSchemeTests,
      comparabilityTests,
      substInClassConstraintsTests]

-- Helper to build names
nm :: String -> TypedTerm Name
nm s = Core.name $ string s

-- | Build a TypeScheme DSL value
scheme :: [String] -> TypedTerm Type -> TypedTerm TypeScheme
scheme vars body = Core.typeScheme (list [nm v | v <- vars]) body Maps.empty

-- | Apply substInType and show the result as a string
showSubstInType :: [(String, TypedTerm Type)] -> TypedTerm Type -> TypedTerm String
showSubstInType pairs inputType =
  PrintCore.type_ @@ (Substitution.substInType @@
    (wrap _TypeSubst (Maps.fromList (subst pairs))) @@ inputType)

-- | Apply substInTypeScheme and render just the scheme's body type, which is
-- where the capture-avoidance behavior shows up. The quantifier list is
-- preserved unchanged so we test it separately via showSubstInTypeSchemeVars.
showSubstInTypeSchemeBody :: [(String, TypedTerm Type)] -> TypedTerm TypeScheme -> TypedTerm String
showSubstInTypeSchemeBody pairs inputScheme =
  PrintCore.type_ @@ (Core.typeSchemeBody $ Substitution.substInTypeScheme @@
    (wrap _TypeSubst (Maps.fromList (subst pairs))) @@ inputScheme)

-- | A schema of nominal types for the comparability tests (#848)
comparabilitySchema :: TypedTerm (M.Map Name TypeScheme)
comparabilitySchema = Maps.fromList $ list [
  pair (nm "test.Point") (scheme [] $ T.record (nm "test.Point") [fieldOf "x" T.int32, fieldOf "y" T.int32]),
  pair (nm "test.WithFunction") (scheme [] $ T.record (nm "test.WithFunction") [fieldOf "f" (T.function T.int32 T.int32)]),
  pair (nm "test.Box") (scheme ["a"] $ T.wrap (nm "test.Box") (T.var "a")),
  pair (nm "test.Tagged") (scheme ["a"] $ T.wrap (nm "test.Tagged") T.string),
  pair (nm "test.IntList") (scheme [] $ T.union (nm "test.IntList") [
    fieldOf "nil" T.unit,
    fieldOf "cons" (T.pair T.int32 (T.var "test.IntList"))]),
  pair (nm "test.FunctionList") (scheme [] $ T.union (nm "test.FunctionList") [
    fieldOf "nil" T.unit,
    fieldOf "cons" (T.pair (T.function T.int32 T.int32) (T.var "test.FunctionList"))])]

-- | A field of a record or union type
fieldOf :: String -> TypedTerm Type -> (TypedTerm Name, TypedTerm Type)
fieldOf fname ftype = (nm fname, ftype)

-- | Universal isComparableType test case
comparableCase :: String -> TypedTerm Type -> Bool -> TypedTerm TestCaseWithMetadata
comparableCase cname typ expected = universalCase cname
  (Literals.printBoolean $ Classes.isComparableType @@ comparabilitySchema @@ typ)
  (string $ if expected then "true" else "false")

-- | Universal substInClassConstraints test case: substitute into a single class constraint on t0,
-- and show the names of the variables which carry constraints afterward
substInClassConstraintsCase :: String -> String -> [(String, TypedTerm Type)] -> [String] -> TypedTerm TestCaseWithMetadata
substInClassConstraintsCase cname className pairs expected = universalCase cname
  (Strings.join (string ",") $ Lists.map (reify Core.unName) $ Maps.keys $
    Substitution.substInClassConstraints @@ comparabilitySchema
      @@ (wrap _TypeSubst (Maps.fromList (subst pairs)))
      @@ (Maps.singleton (nm "t0") (Core.typeVariableConstraints $ Sets.singleton $ Core.typeClassConstraintSimple $ nm className)))
  (string $ L.intercalate "," expected)

-- Helper for building substitution pairs
subst :: [(String, TypedTerm Type)] -> TypedTerm [(Name, Type)]
subst pairs = list [pair (nm n) t | (n, t) <- pairs]

-- | Universal substInType test case
substInTypeCase :: String -> [(String, TypedTerm Type)] -> TypedTerm Type -> TypedTerm Type -> TypedTerm TestCaseWithMetadata
substInTypeCase cname pairs input output =
  universalCase cname (showSubstInType pairs input) (PrintCore.type_ @@ output)

substInTypeSchemeBodyCase :: String -> [(String, TypedTerm Type)] -> TypedTerm TypeScheme -> TypedTerm Type -> TypedTerm TestCaseWithMetadata
substInTypeSchemeBodyCase cname pairs input expectedBody =
  universalCase cname (showSubstInTypeSchemeBody pairs input)
    (PrintCore.type_ @@ expectedBody)

-- ============================================================
-- Comparability tests (#848)
-- ============================================================

comparabilityTests :: TypedTerm TestGroup
comparabilityTests = subgroup "isComparableType" [
  comparableCase "literal" T.int32 True,
  comparableCase "unit" T.unit True,
  comparableCase "void" T.void True,
  comparableCase "function" (T.function T.int32 T.int32) False,
  comparableCase "effect" (Core.typeEffect T.int32) False,
  comparableCase "list of maps" (T.list $ T.map T.string T.int32) True,
  comparableCase "function in a pair" (T.pair T.int32 (T.function T.int32 T.int32)) False,
  comparableCase "function as a map value" (T.map T.string (T.function T.int32 T.int32)) False,
  comparableCase "effect in a list" (T.list $ Core.typeEffect T.string) False,
  comparableCase "annotated function" (T.annot Maps.empty (T.function T.int32 T.int32)) False,
  comparableCase "type variable" (T.var "t0") True,
  comparableCase "forall over a comparable body" (T.forAll "a" $ T.list $ T.var "a") True,
  comparableCase "forall over a function" (T.forAll "a" $ T.function (T.var "a") (T.var "a")) False,
  comparableCase "nominal record" (T.var "test.Point") True,
  comparableCase "nominal record with a function field" (T.var "test.WithFunction") False,
  comparableCase "nominal type nested in a set" (T.set $ T.var "test.WithFunction") False,
  comparableCase "polymorphic wrapper at a comparable type" (T.apply (T.var "test.Box") T.int32) True,
  comparableCase "polymorphic wrapper at a function type" (T.apply (T.var "test.Box") (T.function T.int32 T.int32)) False,
  comparableCase "phantom parameter at a function type" (T.apply (T.var "test.Tagged") (T.function T.int32 T.int32)) True,
  comparableCase "recursive union" (T.var "test.IntList") True,
  comparableCase "recursive union with a function" (T.var "test.FunctionList") False]

substInClassConstraintsTests :: TypedTerm TestGroup
substInClassConstraintsTests = subgroup "substInClassConstraints" [
  substInClassConstraintsCase "ordering propagates to free variables" "ordering"
    [("t0", T.pair (T.var "t1") T.int32)] ["t1"],
  substInClassConstraintsCase "ordering is discharged by a comparable type" "ordering"
    [("t0", T.var "test.Point")] [],
  substInClassConstraintsCase "ordering is retained on a function type" "ordering"
    [("t0", T.function T.int32 T.int32)] ["t0"],
  substInClassConstraintsCase "ordering is retained on a nominal type with a function field" "ordering"
    [("t0", T.var "test.WithFunction")] ["t0"],
  substInClassConstraintsCase "ordering is retained, not just propagated, on a function with free variables" "ordering"
    [("t0", T.pair (T.var "t1") (T.function T.int32 T.int32))] ["t0", "t1"],
  substInClassConstraintsCase "numeric propagates to a variable" "numeric"
    [("t0", T.var "t1")] ["t1"]]

-- ============================================================
-- substInType tests
-- ============================================================

substInTypeSchemeTests :: TypedTerm TestGroup
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

substInTypeTests :: TypedTerm TestGroup
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
