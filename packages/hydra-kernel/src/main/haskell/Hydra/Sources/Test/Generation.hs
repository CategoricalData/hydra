{-# LANGUAGE FlexibleContexts #-}

-- | Test cases for code generation operations, in particular the contract
-- between `inferModules` and `inferModulesGiven`: incremental inference of a
-- subset of the universe must produce the same inferred type schemes for the
-- target modules' term bindings as a full inference run over the same universe.

module Hydra.Sources.Test.Generation where

import Hydra.Kernel hiding (inferModules)
import           Hydra.Core.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                 as Testing
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms         as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core          as Core
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms      as Phantoms hiding ((++))
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types         as T
import qualified Hydra.Core.Dsl.Lib.Eithers   as Eithers
import qualified Hydra.Core.Dsl.Lib.Lists     as Lists
import qualified Hydra.Core.Dsl.Lib.Maps      as Maps
import qualified Hydra.Core.Dsl.Lib.Optionals    as Optionals
import qualified Hydra.Core.Dsl.Lib.Strings   as Strings
import qualified Hydra.Core.Dsl.Packaging          as Packaging
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Data.List                    as L
import qualified Data.Map                     as M

import qualified Hydra.Sources.Kernel.Terms.Generation as Generation
import qualified Hydra.Sources.Kernel.Terms.Scoping    as Scoping
import qualified Hydra.Sources.Kernel.Terms.Print.Core  as PrintCore
import qualified Hydra.Sources.Kernel.Terms.Print.Markdown as PrintMarkdown


-- Local alias for polymorphic application.
(#) :: (AsTerm f (a -> b), AsTerm g a) => f -> g -> TypedTerm b
(#) = (Phantoms.@@)
infixl 1 #

ns :: ModuleName
ns = ModuleName "hydra.core.test.generation"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([Generation.ns, PrintCore.ns, PrintMarkdown.ns, TestGraph.ns] ++ kernelTypesModuleNames),
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
  (Packaging.termDefinition nm Phantoms.nothing (Phantoms.just (Scoping.typeSchemeToTermSignature @@ ts)) tm)

universeMods :: TypedTerm [Module]
universeMods = Phantoms.list [modA, modB]

-- An untyped term definition (Maybe TypeScheme = nothing) so inference will
-- assign a fresh scheme.
untypedTermDef :: TypedTerm Name -> TypedTerm Term -> TypedTerm Definition
untypedTermDef nm tm = Packaging.definitionTerm
  (Packaging.termDefinition nm Phantoms.nothing Phantoms.nothing tm)

----------------------------------------
-- Second toy universe: a "clean" module carrying a scheme with vacuous
-- quantifiers (type variables that appear only in the domain, never in the
-- codomain), plus a "stale" target that applies the function. This shape
-- has historically been produced by inference runs on the real kernel
-- (e.g. defaults' eliminator schemes after round-tripping through JSON).
-- `inferModules` and `inferModulesGiven` must agree on the rendered
-- inferred modules for any such universe.
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
showDef d = Phantoms.match _Definition d Nothing [
    _Definition_type>>: "td" ~> Phantoms.string "",
    _Definition_term>>: "td" ~>
      Strings.concat (list [
        Core.unName (Packaging.termDefinitionName (var "td")),
        Phantoms.string " :: ",
        Optionals.match (Optionals.map (asTerm Scoping.termSignatureToTypeScheme) (Packaging.termDefinitionSignature (var "td"))) (Phantoms.string "<no scheme>") ("ts" ~> PrintCore.typeScheme # var "ts"),
        Phantoms.string " = ",
        PrintCore.term # (Packaging.termDefinitionBody (var "td")),
        Phantoms.string "\n"]),
    _Definition_primitive>>: "pd" ~>
      Strings.concat (list [
        Core.unName (Packaging.primitiveDefinitionName (var "pd")),
        Phantoms.string " :: <primitive>\n"])]

showModule :: TypedTerm Module -> TypedTerm String
showModule m = Strings.concat (Lists.map ("d" ~> showDef (var "d")) (Packaging.moduleDefinitions m))

showModules :: TypedTerm [Module] -> TypedTerm String
showModules ms = Strings.concat (Lists.map ("m" ~> showModule (var "m")) ms)

showResult :: TypedTerm (Either Error [Module]) -> TypedTerm String
showResult r = Eithers.either
  ("e" ~> Phantoms.string "<<inference error>>")
  ("ms" ~> showModules (var "ms"))
  r

----------------------------------------
-- Test cases.

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for code generation operations" $
    supergroup "generation" [
      subgroup "generateModuleDoc" [
        generateModuleDocCase,
        multiDefinitionCase,
        primitiveDocCase,
        typeDocCase,
        undocumentedCase],
      subgroup "inferModulesGiven" [
        incrementalSubsetCase,
        incrementalFullCase,
        vacuousQuantifierCase]]

----------------------------------------
-- generateModuleDoc test input and case (#723).
--
-- A minimal hand-built module -- one documented term definition -- exercises
-- generateModuleDoc end-to-end (Module -> hydra.core.markdown.Document) followed by
-- PrintMarkdown.document (Document -> String), asserting the exact rendered
-- Markdown. This is a shrunk stand-in for the eventual convergence test
-- (regenerate + diff against the real committed spec pages), which needs a
-- full sync to run; this test needs only the toy module below.

-- forall a. a -> a
docExampleScheme :: TypedTerm TypeScheme
docExampleScheme = T.poly ["a"] (T.function (T.var "a") (T.var "a"))

-- The single documented term definition under test: hydra.testInput.doc.identity,
-- with a one-line doc string, so the rendered page exercises the doc-string
-- paragraph (not the "*(undocumented)*" fallback -- that fallback path is
-- simple enough to leave for the eventual convergence test to cover
-- end-to-end against real undocumented kernel definitions, if any remain).
docExampleDefinition :: TypedTerm Definition
docExampleDefinition = Packaging.definitionTerm
  (Packaging.termDefinition
    nameDocExample
    (Phantoms.just (Packaging.entityMetadata
      (Phantoms.just (Phantoms.string "The identity function"))
      (Phantoms.list ([] :: [TypedTerm String]))
      (Phantoms.list ([] :: [TypedTerm EntityReference]))
      Phantoms.nothing
      (Phantoms.list ([] :: [TypedTerm Provision]))))
    (Phantoms.just (Scoping.typeSchemeToTermSignature # docExampleScheme))
    (Terms.lambda "x" (Terms.var "x")))

nameDocExample :: TypedTerm Name
nameDocExample = Core.name (Phantoms.string "hydra.testInput.doc.identity")

nsDocExample :: TypedTerm ModuleName
nsDocExample = Packaging.moduleName2 (Phantoms.string "hydra.testInput.doc")

docExampleModule :: TypedTerm Module
docExampleModule = Packaging.module_
  nsDocExample
  Phantoms.nothing
  (Phantoms.list ([] :: [TypedTerm ModuleDependency]))
  (Phantoms.list [docExampleDefinition])

-- | Property: generateModuleDoc renders one H2 section per definition, with
-- the doc string as its own paragraph and the signature as a code-span
-- paragraph, wrapped in an H1 titled after the module.
generateModuleDocCase :: TypedTerm TestCaseWithMetadata
generateModuleDocCase = universalCase
    "generateModuleDoc renders a documented term definition's doc string and signature"
    actual
    expected
  where
    actual = PrintMarkdown.document # (Generation.generateModuleDoc # docExampleModule)
    -- PrintCore.typeScheme renders the scheme's own quantifier as literal ASCII
    -- "forall v1,v2. " (see Print/Core.hs's "fa" binding); only Type-level function
    -- arrows and any NESTED Type-level foralls inside the scheme body render as
    -- Unicode (confirmed against docs/hydra-lexicon.txt, e.g.
    -- "hydra.core.lib.effects.apply : (forall x,y. (effect<(x → y)> → effect<x> → effect<y>))") --
    -- so a simple `forall a. a -> a` scheme (no nested Type-level forall) renders
    -- "(forall a. (a → a))", mixing ASCII (scheme header) and Unicode (body arrow).
    expected = Phantoms.string $
      "# hydra.testInput.doc\n\n" ++
      "<!-- Note: this is an automatically generated file. Do not edit. -->\n\n" ++
      "## hydra.testInput.doc.identity\n\n" ++
      "The identity function\n\n" ++
      "`(forall a. (a → a))`"

-- | Property: an undocumented definition (metadata = nothing) renders the
-- "*(undocumented)*" placeholder paragraph rather than failing, per #723's
-- "missing doc strings render as a visible marker, not a failure."
undocumentedDefinition :: TypedTerm Definition
undocumentedDefinition = Packaging.definitionTerm
  (Packaging.termDefinition
    nameUndocumented
    Phantoms.nothing
    (Phantoms.just (Scoping.typeSchemeToTermSignature # docExampleScheme))
    (Terms.lambda "x" (Terms.var "x")))

nameUndocumented :: TypedTerm Name
nameUndocumented = Core.name (Phantoms.string "hydra.testInput.undoc.identity")

nsUndocumented :: TypedTerm ModuleName
nsUndocumented = Packaging.moduleName2 (Phantoms.string "hydra.testInput.undoc")

undocumentedModule :: TypedTerm Module
undocumentedModule = Packaging.module_
  nsUndocumented
  Phantoms.nothing
  (Phantoms.list ([] :: [TypedTerm ModuleDependency]))
  (Phantoms.list [undocumentedDefinition])

undocumentedCase :: TypedTerm TestCaseWithMetadata
undocumentedCase = universalCase
    "generateModuleDoc renders the undocumented-marker placeholder when metadata is absent"
    actual
    expected
  where
    actual = PrintMarkdown.document # (Generation.generateModuleDoc # undocumentedModule)
    expected = Phantoms.string $
      "# hydra.testInput.undoc\n\n" ++
      "<!-- Note: this is an automatically generated file. Do not edit. -->\n\n" ++
      "## hydra.testInput.undoc.identity\n\n" ++
      "*(undocumented)*\n\n" ++
      "`(forall a. (a → a))`"

-- | Property: a PrimitiveDefinition's signature always renders (never falls back to "?",
-- unlike TermDefinition, since a primitive's signature is "always explicit, never inferred"
-- per its own doc comment). Uses a trivial no-argument-domain, monomorphic signature so the
-- rendered TypeScheme has no `forall` clause at all -- distinguishing this case from
-- generateModuleDocCase's polymorphic one.
-- | The provision under test: a single requirement, with its name already fully composed (as
-- Provision.name always is by the time it reaches this generator -- see provisionParagraph's doc
-- comment in Generation.hs). Uses Packaging.provision/provisionKindRequirement, the generated
-- record/union constructors following this codebase's established <lowerType><Field> /
-- <lowerType><Variant> naming convention (confirmed precedent: Core.typeForall,
-- Packaging.entityMetadata) -- not directly grepped from a prior real call site, since no other
-- code in the tree constructs a Provision term yet (#725 landed only the type shapes + the
-- composeProvisionName helper, no term-level consumer). If this name is wrong, the eventual build
-- will surface it immediately as a single "not in scope" error here.
primitiveExampleProvision :: TypedTerm Provision
primitiveExampleProvision = Packaging.provision
  (Core.name (Phantoms.string "hydra.testInput.prim.example.constantValue"))
  Packaging.provisionKindRequirement
  (Phantoms.string "This primitive always returns the same string value.")

primitiveExampleDefinition :: TypedTerm Definition
primitiveExampleDefinition = Packaging.definitionPrimitive
  (Packaging.primitiveDefinition
    namePrimitiveExample
    (Phantoms.just (Packaging.entityMetadata
      (Phantoms.just (Phantoms.string "A trivial constant primitive"))
      (Phantoms.list ([] :: [TypedTerm String]))
      (Phantoms.list ([] :: [TypedTerm EntityReference]))
      Phantoms.nothing
      (Phantoms.list [primitiveExampleProvision])))
    (Scoping.typeSchemeToTermSignature # Core.typeScheme (Phantoms.list ([] :: [TypedTerm Name])) T.string Maps.empty)
    (Phantoms.boolean True)
    (Phantoms.boolean True)
    Phantoms.nothing)

namePrimitiveExample :: TypedTerm Name
namePrimitiveExample = Core.name (Phantoms.string "hydra.testInput.prim.example")

nsPrimitiveExample :: TypedTerm ModuleName
nsPrimitiveExample = Packaging.moduleName2 (Phantoms.string "hydra.testInput.prim")

primitiveExampleModule :: TypedTerm Module
primitiveExampleModule = Packaging.module_
  nsPrimitiveExample
  Phantoms.nothing
  (Phantoms.list ([] :: [TypedTerm ModuleDependency]))
  (Phantoms.list [primitiveExampleDefinition])

primitiveDocCase :: TypedTerm TestCaseWithMetadata
primitiveDocCase = universalCase
    "generateModuleDoc renders a primitive's monomorphic signature (no forall clause) and its provision"
    actual
    expected
  where
    actual = PrintMarkdown.document # (Generation.generateModuleDoc # primitiveExampleModule)
    -- provisionParagraph bolds ONLY the "[NAME] (kind) " lead-in (a separate Inline.strong),
    -- not the statement -- the statement is a sibling plain Inline.text, concatenated with no
    -- separator (see Print/Markdown.hs's `inlines` -- Strings.concat with no join string).
    expected = Phantoms.string $
      "# hydra.testInput.prim\n\n" ++
      "<!-- Note: this is an automatically generated file. Do not edit. -->\n\n" ++
      "## hydra.testInput.prim.example\n\n" ++
      "A trivial constant primitive\n\n" ++
      "`(string)`\n\n" ++
      "**[HYDRA-TEST-INPUT-PRIM-EXAMPLE-CONSTANT-VALUE] (requirement) **" ++
      "This primitive always returns the same string value."

-- | Property: a TypeDefinition's body renders directly (no Optional-signature fallback,
-- unlike TermDefinition -- a type's body is never absent).
typeExampleDefinition :: TypedTerm Definition
typeExampleDefinition = Packaging.definitionType
  (Packaging.typeDefinition
    nameTypeExample
    (Phantoms.just (Packaging.entityMetadata
      (Phantoms.just (Phantoms.string "A type alias for string"))
      (Phantoms.list ([] :: [TypedTerm String]))
      (Phantoms.list ([] :: [TypedTerm EntityReference]))
      Phantoms.nothing
      (Phantoms.list ([] :: [TypedTerm Provision]))))
    (Core.typeScheme (Phantoms.list ([] :: [TypedTerm Name])) T.string Maps.empty))

nameTypeExample :: TypedTerm Name
nameTypeExample = Core.name (Phantoms.string "hydra.testInput.typ.Example")

nsTypeExample :: TypedTerm ModuleName
nsTypeExample = Packaging.moduleName2 (Phantoms.string "hydra.testInput.typ")

typeExampleModule :: TypedTerm Module
typeExampleModule = Packaging.module_
  nsTypeExample
  Phantoms.nothing
  (Phantoms.list ([] :: [TypedTerm ModuleDependency]))
  (Phantoms.list [typeExampleDefinition])

typeDocCase :: TypedTerm TestCaseWithMetadata
typeDocCase = universalCase
    "generateModuleDoc renders a type definition's body directly, with no Optional unwrap"
    actual
    expected
  where
    actual = PrintMarkdown.document # (Generation.generateModuleDoc # typeExampleModule)
    expected = Phantoms.string $
      "# hydra.testInput.typ\n\n" ++
      "<!-- Note: this is an automatically generated file. Do not edit. -->\n\n" ++
      "## hydra.testInput.typ.Example\n\n" ++
      "A type alias for string\n\n" ++
      "`(string)`"

-- | Property: a module with multiple definitions renders one Section per definition,
-- joined by a blank line in the module's own definition order (Packaging.moduleDefinitions'
-- order is preserved, not re-sorted -- unlike generateLexicon, which sorts for lexicon
-- readability; generateModuleDoc is per-module, so declaration order is the natural order).
--
-- A second, undocumented sibling definition under the SAME hydra.testInput.doc namespace
-- (real modules' definitions all share their module's namespace as prefix; reusing the
-- separate hydra.testInput.undoc.identity definition here would test the same rendering
-- logic but with an unrealistic cross-namespace module, so this case gets its own sibling).
docExampleSiblingDefinition :: TypedTerm Definition
docExampleSiblingDefinition = Packaging.definitionTerm
  (Packaging.termDefinition
    nameDocExampleSibling
    Phantoms.nothing
    (Phantoms.just (Scoping.typeSchemeToTermSignature # docExampleScheme))
    (Terms.lambda "x" (Terms.var "x")))

nameDocExampleSibling :: TypedTerm Name
nameDocExampleSibling = Core.name (Phantoms.string "hydra.testInput.doc.identity2")

multiDefinitionModule :: TypedTerm Module
multiDefinitionModule = Packaging.module_
  nsDocExample
  Phantoms.nothing
  (Phantoms.list ([] :: [TypedTerm ModuleDependency]))
  (Phantoms.list [docExampleDefinition, docExampleSiblingDefinition])

multiDefinitionCase :: TypedTerm TestCaseWithMetadata
multiDefinitionCase = universalCase
    "generateModuleDoc renders multiple definitions as separate sections in declaration order"
    actual
    expected
  where
    actual = PrintMarkdown.document # (Generation.generateModuleDoc # multiDefinitionModule)
    expected = Phantoms.string $
      "# hydra.testInput.doc\n\n" ++
      "<!-- Note: this is an automatically generated file. Do not edit. -->\n\n" ++
      "## hydra.testInput.doc.identity\n\n" ++
      "The identity function\n\n" ++
      "`(forall a. (a → a))`\n\n" ++
      "## hydra.testInput.doc.identity2\n\n" ++
      "*(undocumented)*\n\n" ++
      "`(forall a. (a → a))`"

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
