{-# LANGUAGE FlexibleContexts #-}

-- | Test cases for code generation operations, in particular the contract
-- between `inferModules` and `inferModulesGiven`: incremental inference of a
-- subset of the universe must produce the same inferred type schemes for the
-- target modules' term bindings as a full inference run over the same universe.

module Hydra.Sources.Test.Generation where

import Hydra.Kernel hiding (inferModules)
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
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
import qualified Hydra.Sources.Kernel.Terms.Scoping    as Scoping
import qualified Hydra.Sources.Kernel.Terms.Show.Core  as ShowCore


-- Local alias for polymorphic application.
(#) :: (AsTerm f (a -> b), AsTerm g a) => f -> g -> TypedTerm b
(#) = (Phantoms.@@)
infixl 1 #

ns :: ModuleName
ns = ModuleName "hydra.test.generation"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([Generation.ns, ShowCore.ns, TestGraph.ns] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata ((Just "Test cases for code generation operations such as inferModules and inferModulesGiven"))}
  where
    definitions = [Phantoms.toDefinition allTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

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

-- The scheme carried on `modA.idA`: forall a. a -> a.
idAScheme :: TypedTerm TypeScheme
idAScheme = T.poly ["a"] (T.function (T.var "a") (T.var "a"))

modA :: TypedTerm Module
modA = Packaging.module_
  nsA
  Phantoms.nothing
  (Phantoms.list ([] :: [TypedTerm ModuleDependency]))
  (Phantoms.list [
    typedTermDef nameIdA (Terms.lambda "x" (Terms.var "x")) idAScheme])

modB :: TypedTerm Module
modB = Packaging.module_
  nsB
  Phantoms.nothing
  (Phantoms.list [Packaging.moduleDependency nsA Phantoms.nothing])
  (Phantoms.list [
    untypedTermDef nameUseId (Terms.apply (Terms.var "hydra.testInput.a.idA") (Terms.int32 42))])

nameIdA :: TypedTerm Name
nameIdA = Core.name (Phantoms.string "hydra.testInput.a.idA")

nameUseId :: TypedTerm Name
nameUseId = Core.name (Phantoms.string "hydra.testInput.b.useId")

nsA :: TypedTerm ModuleName
nsA = Packaging.moduleName2 (Phantoms.string "hydra.testInput.a")

nsB :: TypedTerm ModuleName
nsB = Packaging.moduleName2 (Phantoms.string "hydra.testInput.b")

-- A pre-annotated term definition, simulating a universe binding whose type
-- scheme was populated by a prior inference run (the future caching layer).
-- `inferModulesGiven` uses these schemes via `modulesToGraph`'s `boundTypes`
-- seeding to resolve cross-module references without re-inferring the
-- universe.
typedTermDef :: TypedTerm Name -> TypedTerm Term -> TypedTerm TypeScheme -> TypedTerm Definition
typedTermDef nm tm ts = Packaging.definitionTerm
  (Packaging.termDefinition nm Phantoms.nothing tm (Phantoms.just (Scoping.typeSchemeToTermSignature @@ ts)))

universeMods :: TypedTerm [Module]
universeMods = Phantoms.list [modA, modB]

-- An untyped term definition (Maybe TypeScheme = nothing) so inference will
-- assign a fresh scheme.
untypedTermDef :: TypedTerm Name -> TypedTerm Term -> TypedTerm Definition
untypedTermDef nm tm = Packaging.definitionTerm
  (Packaging.termDefinition nm Phantoms.nothing tm Phantoms.nothing)

----------------------------------------
-- Second toy universe: a "clean" module carrying a scheme with vacuous
-- quantifiers (type variables that appear only in the domain, never in the
-- codomain), plus a "stale" target that applies the function. This shape is
-- actually produced by a prior inference run on the real kernel, e.g. the
-- stored scheme for `hydra.lib.defaults.eithers.either` after round-tripping
-- through JSON. `inferModules` and `inferModulesGiven` must agree on the
-- rendered inferred modules for any such universe.
--
--   hydra.testInput.v.funky :: forall t0 t1. t0 -> t1 -> int32 -> int32
--     (the body is irrelevant; only the scheme matters for seeding.)
--   hydra.testInput.w.useFunky = funky "foo" 7 100

-- forall t0 t1 t2. t0 -> t1 -> t2 -> t2
-- This is the canonical scheme that inferModules produces when funky is
-- inferred alone (without useFunky constraining it). The body `\x.\y.\z. z`
-- gives 3 free type variables; a caching layer would store exactly this.
funkyScheme :: TypedTerm TypeScheme
funkyScheme = T.poly ["t0", "t1", "t2"]
  (T.function (T.var "t0")
    (T.function (T.var "t1")
      (T.function (T.var "t2") (T.var "t2"))))

-- Body: `\x. \y. \z. z`. Three args, returns the third.
funkyTerm :: TypedTerm Term
funkyTerm = Terms.lambda "x" (Terms.lambda "y" (Terms.lambda "z" (Terms.var "z")))

modV :: TypedTerm Module
modV = Packaging.module_
  nsV
  Phantoms.nothing
  (Phantoms.list ([] :: [TypedTerm ModuleDependency]))
  (Phantoms.list [typedTermDef nameFunky funkyTerm funkyScheme])

-- useFunky = funky "foo" 7 100
modW :: TypedTerm Module
modW = Packaging.module_
  nsW
  Phantoms.nothing
  (Phantoms.list [Packaging.moduleDependency nsV Phantoms.nothing])
  (Phantoms.list [
    untypedTermDef nameUseFunky
      (Terms.apply
        (Terms.apply
          (Terms.apply (Terms.var "hydra.testInput.v.funky")
                       (Terms.string "foo"))
          (Terms.int32 7))
        (Terms.int32 100))])

nameFunky :: TypedTerm Name
nameFunky = Core.name (Phantoms.string "hydra.testInput.v.funky")

nameUseFunky :: TypedTerm Name
nameUseFunky = Core.name (Phantoms.string "hydra.testInput.w.useFunky")

nsV :: TypedTerm ModuleName
nsV = Packaging.moduleName2 (Phantoms.string "hydra.testInput.v")

nsW :: TypedTerm ModuleName
nsW = Packaging.moduleName2 (Phantoms.string "hydra.testInput.w")

vacuousUniverse :: TypedTerm [Module]
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
showDef :: TypedTerm Definition -> TypedTerm String
showDef d = Phantoms.cases _Definition d Nothing [
    _Definition_type>>: "td" ~> Phantoms.string "",
    _Definition_term>>: "td" ~>
      Strings.concat [
        Core.unName (Packaging.termDefinitionName (var "td")),
        Phantoms.string " :: ",
        Maybes.maybe
          (Phantoms.string "<no scheme>")
          ("ts" ~> ShowCore.typeScheme # var "ts")
          (Maybes.map Scoping.termSignatureToTypeScheme (Packaging.termDefinitionSignature (var "td"))),
        Phantoms.string " = ",
        ShowCore.term # (Packaging.termDefinitionTerm (var "td")),
        Phantoms.string "\n"],
    _Definition_primitive>>: "pd" ~>
      Strings.concat [
        Core.unName (Packaging.primitiveDefinitionName (var "pd")),
        Phantoms.string " :: <primitive>\n"]]

showModule :: TypedTerm Module -> TypedTerm String
showModule m = Strings.cat (Lists.map ("d" ~> showDef (var "d")) (Packaging.moduleDefinitions m))

showModules :: TypedTerm [Module] -> TypedTerm String
showModules ms = Strings.cat (Lists.map ("m" ~> showModule (var "m")) ms)

showResult :: TypedTerm (Either Error [Module]) -> TypedTerm String
showResult r = Eithers.either_
  ("e" ~> Phantoms.string "<<inference error>>")
  ("ms" ~> showModules (var "ms"))
  r

----------------------------------------
-- Test cases.

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for code generation operations" $
    supergroup "generation" [
      subgroup "inferModulesGiven" [
        incrementalSubsetCase,
        incrementalFullCase,
        vacuousQuantifierCase]]

-- | Property: when target = universe, `inferModulesGiven` is equivalent to
-- `inferModules`.
incrementalFullCase :: TypedTerm TestCaseWithMetadata
incrementalFullCase = universalCase "incremental inference of full universe matches full inference"
    actual
    expected
  where
    actual = showResult (Generation.inferModulesGiven
      # TestGraph.testContext # TestGraph.testGraph # universeMods # universeMods)
    expected = showResult (Generation.inferModules
      # TestGraph.testContext # TestGraph.testGraph # universeMods # universeMods)

-- | Property: incremental inference of a strict subset of the universe
-- produces the same inferred type schemes for the target modules' term
-- bindings as a full inference run over the same universe.
incrementalSubsetCase :: TypedTerm TestCaseWithMetadata
incrementalSubsetCase = universalCase "incremental inference of subset matches full inference"
    actual
    expected
  where
    target = Phantoms.list [modB]
    actual = showResult (Generation.inferModulesGiven
      # TestGraph.testContext # TestGraph.testGraph # universeMods # target)
    expected = showResult (Generation.inferModules
      # TestGraph.testContext # TestGraph.testGraph # universeMods # target)

-- | Property: when a clean universe module carries a pre-inferred scheme,
-- `inferModulesGiven` uses that scheme verbatim rather than re-inferring it.
-- References from stale targets to the clean module are instantiated at the
-- scheme's full quantifier count (routed through `inferTypeOfVariable`'s
-- `graphBoundTypes` branch), even if the scheme has vacuous quantifiers that
-- a fresh inference pass would collapse.
--
-- This is the behaviour that lets `inferModulesGiven` skip inference for
-- clean modules without losing soundness: the cached scheme is the source of
-- truth, and Phase 6 of `inferTypeOfLetNormalized` only rewrites references
-- to currently let-bound names, so there is no quantifier-count mismatch.
--
-- The divergence from `inferModules` (which re-infers the clean binding and
-- may collapse quantifiers) is expected and correct. Byte-identical output
-- against the real kernel JSON at regeneration time is what certifies the
-- partition; this case just pins the per-reference AST shape for the toy
-- vacuous-quantifier universe.
vacuousQuantifierCase :: TypedTerm TestCaseWithMetadata
vacuousQuantifierCase = universalCase
    "incremental inference uses cached scheme verbatim on vacuous-quantifier universe"
    actual
    (Phantoms.string $
      "hydra.testInput.w.useFunky :: (int32) = (" ++
      "hydra.testInput.v.funky⟨string⟩⟨int32⟩⟨int32⟩" ++
      " @ \"foo\" @ 7:int32 @ 100:int32)\n")
  where
    target = Phantoms.list [modW]
    actual = showResult (Generation.inferModulesGiven
      # TestGraph.testContext # TestGraph.testGraph # vacuousUniverse # target)
