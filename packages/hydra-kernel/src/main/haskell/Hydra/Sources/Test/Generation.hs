{-# LANGUAGE FlexibleContexts #-}

-- | Test cases for code generation operations, in particular the contract
-- between `inferModules` and `inferModulesGiven`: incremental inference of a
-- subset of the universe must produce the same inferred type schemes for the
-- target modules' term bindings as a full inference run over the same universe.

module Hydra.Sources.Test.Generation where

import Hydra.Kernel hiding (inferModules)
import Hydra.Dsl.Meta.Testing                 as Testing
import qualified Hydra.Dsl.Meta.Terms         as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import           Hydra.Dsl.Meta.Phantoms      as Phantoms hiding ((++))
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Packaging          as Packaging
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Data.List                    as L
import qualified Data.Map                     as M

import qualified Hydra.Sources.Kernel.Terms.Generation as Generation
import qualified Hydra.Sources.Kernel.Terms.Show.Core  as ShowCore


ns :: Namespace
ns = Namespace "hydra.test.generation"

module_ :: Module
module_ = Module ns definitions
    [Generation.ns, ShowCore.ns, TestGraph.ns]
    kernelTypesNamespaces
    (Just "Test cases for code generation operations such as inferModules and inferModulesGiven")
  where
    definitions = [Phantoms.toDefinition allTests]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

-- Local alias for polymorphic application.
(#) :: (AsTerm f (a -> b), AsTerm g a) => f -> g -> TTerm b
(#) = (Phantoms.@@)
infixl 1 #

----------------------------------------
-- Toy modules used as test input.
--
-- Two modules in a fictional `hydra.testInput.*` namespace, deliberately
-- minimal:
--
--   hydra.testInput.a:  idA = \x. x
--   hydra.testInput.b:  useId = idA 42      (depends on hydra.testInput.a)
--
-- The cross-module reference from `useId` to `idA` is the property we care
-- about: it forces inference for `b` to consult information about `a`, which
-- is exactly the path `inferModulesGiven` exercises differently from
-- `inferModules` (the former only infers `targetBindings`, the latter infers
-- the full universe).

nsA :: TTerm Namespace
nsA = Packaging.namespace (Phantoms.string "hydra.testInput.a")

nsB :: TTerm Namespace
nsB = Packaging.namespace (Phantoms.string "hydra.testInput.b")

nameIdA :: TTerm Name
nameIdA = Core.name (Phantoms.string "hydra.testInput.a.idA")

nameUseId :: TTerm Name
nameUseId = Core.name (Phantoms.string "hydra.testInput.b.useId")

-- An untyped term definition (Maybe TypeScheme = nothing) so inference will
-- assign a fresh scheme.
untypedTermDef :: TTerm Name -> TTerm Term -> TTerm Definition
untypedTermDef nm tm = Packaging.definitionTerm
  (Packaging.termDefinition nm tm Phantoms.nothing)

-- A pre-annotated term definition, simulating a universe binding whose type
-- scheme was populated by a prior inference run (the future caching layer).
-- `inferModulesGiven` uses these schemes via `modulesToGraph`'s `boundTypes`
-- seeding to resolve cross-module references without re-inferring the
-- universe.
typedTermDef :: TTerm Name -> TTerm Term -> TTerm TypeScheme -> TTerm Definition
typedTermDef nm tm ts = Packaging.definitionTerm
  (Packaging.termDefinition nm tm (Phantoms.just ts))

-- The scheme carried on `modA.idA`: forall a. a -> a.
idAScheme :: TTerm TypeScheme
idAScheme = T.poly ["a"] (T.function (T.var "a") (T.var "a"))

modA :: TTerm Module
modA = Packaging.module_
  nsA
  (Phantoms.list [
    typedTermDef nameIdA (Terms.lambda "x" (Terms.var "x")) idAScheme])
  (Phantoms.list ([] :: [TTerm Namespace]))
  (Phantoms.list ([] :: [TTerm Namespace]))
  Phantoms.nothing

modB :: TTerm Module
modB = Packaging.module_
  nsB
  (Phantoms.list [
    untypedTermDef nameUseId (Terms.apply (Terms.var "hydra.testInput.a.idA") (Terms.int32 42))])
  (Phantoms.list [nsA])
  (Phantoms.list ([] :: [TTerm Namespace]))
  Phantoms.nothing

universeMods :: TTerm [Module]
universeMods = Phantoms.list [modA, modB]

----------------------------------------
-- Second toy universe: a "clean" module carrying a scheme with vacuous
-- quantifiers (type variables that appear only in the domain, never in the
-- codomain), plus a "stale" target that applies the function. This shape is
-- actually produced by a prior inference run on the real kernel, e.g. the
-- stored scheme for `hydra.eval.lib.eithers.either` after round-tripping
-- through JSON. `inferModules` and `inferModulesGiven` must agree on the
-- rendered inferred modules for any such universe.
--
--   hydra.testInput.v.funky :: forall t0 t1. t0 -> t1 -> int32 -> int32
--     (the body is irrelevant; only the scheme matters for seeding.)
--   hydra.testInput.w.useFunky = funky "foo" 7 100

nsV :: TTerm Namespace
nsV = Packaging.namespace (Phantoms.string "hydra.testInput.v")

nsW :: TTerm Namespace
nsW = Packaging.namespace (Phantoms.string "hydra.testInput.w")

nameFunky :: TTerm Name
nameFunky = Core.name (Phantoms.string "hydra.testInput.v.funky")

nameUseFunky :: TTerm Name
nameUseFunky = Core.name (Phantoms.string "hydra.testInput.w.useFunky")

-- forall t0 t1 t2. t0 -> t1 -> t2 -> t2
-- This is the canonical scheme that inferModules produces when funky is
-- inferred alone (without useFunky constraining it). The body `\x.\y.\z. z`
-- gives 3 free type variables; a caching layer would store exactly this.
funkyScheme :: TTerm TypeScheme
funkyScheme = T.poly ["t0", "t1", "t2"]
  (T.function (T.var "t0")
    (T.function (T.var "t1")
      (T.function (T.var "t2") (T.var "t2"))))

-- Body: `\x. \y. \z. z`. Three args, returns the third.
funkyTerm :: TTerm Term
funkyTerm = Terms.lambda "x" (Terms.lambda "y" (Terms.lambda "z" (Terms.var "z")))

modV :: TTerm Module
modV = Packaging.module_
  nsV
  (Phantoms.list [typedTermDef nameFunky funkyTerm funkyScheme])
  (Phantoms.list ([] :: [TTerm Namespace]))
  (Phantoms.list ([] :: [TTerm Namespace]))
  Phantoms.nothing

-- useFunky = funky "foo" 7 100
modW :: TTerm Module
modW = Packaging.module_
  nsW
  (Phantoms.list [
    untypedTermDef nameUseFunky
      (Terms.apply
        (Terms.apply
          (Terms.apply (Terms.var "hydra.testInput.v.funky")
                       (Terms.string "foo"))
          (Terms.int32 7))
        (Terms.int32 100))])
  (Phantoms.list [nsV])
  (Phantoms.list ([] :: [TTerm Namespace]))
  Phantoms.nothing

vacuousUniverse :: TTerm [Module]
vacuousUniverse = Phantoms.list [modV, modW]

----------------------------------------
-- Show helpers.
--
-- Render a `[Module]` deterministically as the concatenation, in module order,
-- of `"<binding-name> :: <type-scheme>\n"` for every term definition. Type
-- definitions are skipped (they aren't touched by inference).

-- | Render a definition as "<name> :: <scheme> = <term>\n". The term component
-- is included so that two runs producing the same final type but different
-- inferred term bodies (e.g. different TypeApplication wrappers) fail the
-- equality check.
showDef :: TTerm Definition -> TTerm String
showDef d = Phantoms.cases _Definition d Nothing [
    _Definition_type>>: "td" ~> Phantoms.string "",
    _Definition_term>>: "td" ~>
      Strings.concat [
        Core.unName (Packaging.termDefinitionName (var "td")),
        Phantoms.string " :: ",
        Maybes.maybe
          (Phantoms.string "<no scheme>")
          ("ts" ~> ShowCore.typeScheme # var "ts")
          (Packaging.termDefinitionType (var "td")),
        Phantoms.string " = ",
        ShowCore.term # (Packaging.termDefinitionTerm (var "td")),
        Phantoms.string "\n"]]

showModule :: TTerm Module -> TTerm String
showModule m = Strings.cat (Lists.map ("d" ~> showDef (var "d")) (Packaging.moduleDefinitions m))

showModules :: TTerm [Module] -> TTerm String
showModules ms = Strings.cat (Lists.map ("m" ~> showModule (var "m")) ms)

showResult :: TTerm (Either Error [Module]) -> TTerm String
showResult r = Eithers.either_
  ("e" ~> Phantoms.string "<<inference error>>")
  ("ms" ~> showModules (var "ms"))
  r

----------------------------------------
-- Test cases.

-- | Property: incremental inference of a strict subset of the universe
-- produces the same inferred type schemes for the target modules' term
-- bindings as a full inference run over the same universe.
incrementalSubsetCase :: TTerm TestCaseWithMetadata
incrementalSubsetCase = universalCase "incremental inference of subset matches full inference"
    actual
    expected
  where
    target = Phantoms.list [modB]
    actual = showResult (Generation.inferModulesGiven
      # TestGraph.testContext # TestGraph.testGraph # universeMods # target)
    expected = showResult (Generation.inferModules
      # TestGraph.testContext # TestGraph.testGraph # universeMods # target)

-- | Property: when target = universe, `inferModulesGiven` is equivalent to
-- `inferModules`.
incrementalFullCase :: TTerm TestCaseWithMetadata
incrementalFullCase = universalCase "incremental inference of full universe matches full inference"
    actual
    expected
  where
    actual = showResult (Generation.inferModulesGiven
      # TestGraph.testContext # TestGraph.testGraph # universeMods # universeMods)
    expected = showResult (Generation.inferModules
      # TestGraph.testContext # TestGraph.testGraph # universeMods # universeMods)

-- | Regression test for issue #247: when a clean universe module carries a
-- scheme with vacuous quantifiers (variables bound but never appearing in the
-- codomain), `inferModules` and `inferModulesGiven` must produce the same
-- inferred term bodies for any stale target that references it. Without this
-- property, incremental inference can emit term ASTs that differ from
-- non-incremental inference, even when both produce the same type.
vacuousQuantifierCase :: TTerm TestCaseWithMetadata
vacuousQuantifierCase = universalCase
    "incremental inference agrees with full inference on vacuous-quantifier schemes"
    actual
    expected
  where
    target = Phantoms.list [modW]
    actual = showResult (Generation.inferModulesGiven
      # TestGraph.testContext # TestGraph.testGraph # vacuousUniverse # target)
    expected = showResult (Generation.inferModules
      # TestGraph.testContext # TestGraph.testGraph # vacuousUniverse # target)

allTests :: TTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for code generation operations" $
    supergroup "generation" [
      subgroup "inferModulesGiven" [
        incrementalSubsetCase,
        incrementalFullCase,
        vacuousQuantifierCase]]
