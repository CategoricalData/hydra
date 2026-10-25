-- | Round-trip test cases for the textual syntax (#497/#710), focused on term-signature printing
-- and parsing (hydra.core.print.model's termSignature / hydra.core.parse.core's termSignature).
module Hydra.Sources.Test.Syntax where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import           Hydra.Core.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                 as Testing
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core          as Core
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms      as Phantoms hiding ((++))
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types         as T
import qualified Hydra.Core.Dsl.Typing       as Typing
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import qualified Hydra.Core.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Core.Dsl.Lib.Maps     as Maps
import qualified Hydra.Core.Dsl.Packaging     as Packaging
import qualified Hydra.Sources.Kernel.Terms.Print.Core as PrintCore
import qualified Hydra.Sources.Kernel.Terms.Print.Packaging as PrintPackaging
import qualified Hydra.Sources.Kernel.Terms.Parse.Core as ParseCore
import qualified Hydra.Sources.Kernel.Terms.Parse.Packaging as ParsePackaging

import Hydra.Core.Testing


ns :: ModuleName
ns = ModuleName "hydra.core.test.syntax"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([TestGraph.ns, PrintCore.ns, PrintPackaging.ns, ParseCore.ns, ParsePackaging.ns] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata ((Just "Round-trip test cases for the textual syntax"))}
  where
    definitions = [
      Phantoms.toDefinition allTests,
      Phantoms.toDefinition lazyParameterRoundtripGroup,
      Phantoms.toDefinition moduleAliasGroup]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    doc "Test cases for the textual syntax (#497/#710)" $
    Testing.testGroup (string "textual syntax") nothing (list subgroups) (list ([] :: [TypedTerm TestCaseWithMetadata]))
  where
    subgroups = [lazyParameterRoundtripGroup, moduleAliasGroup]

-- | Print a term signature, then parse the printed text back, and show the round-tripped
-- signature. Compares against the original signature's own printed form, so the test both
-- confirms the 'lazy' keyword is emitted (via the printed text matching the expected string
-- elsewhere) and that it survives the round trip (parameterIsLazy flags intact).
signatureRoundtripTest :: String -> TypedTerm TermSignature -> TypedTerm TestCaseWithMetadata
signatureRoundtripTest testName sig = universalCase testName actual expected
  where
    printed = PrintCore.termSignature @@ sig
    actual = Eithers.either
      (Phantoms.lambda "e" $ Phantoms.var "e")
      (Phantoms.lambda "sig2" $ PrintCore.termSignature @@ Phantoms.var "sig2")
      (ParseCore.parseTermSignature @@ printed)
    expected = printed

-- | Print a term signature and compare against an exact expected string -- pins the 'lazy'
-- keyword's exact placement (#800), unlike signatureRoundtripTest which only self-compares.
signaturePrintTest :: String -> TypedTerm TermSignature -> String -> TypedTerm TestCaseWithMetadata
signaturePrintTest testName sig expectedStr = universalCase testName
  (PrintCore.termSignature @@ sig)
  (Phantoms.string expectedStr)

-- | A parameter with no laziness marking.
strictParam :: String -> TypedTerm Type -> TypedTerm Parameter
strictParam n ty = Typing.parameter (Core.name $ Phantoms.string n) nothing ty false

-- | A parameter marked lazy (#800): the position's value is passed thunked at call sites in
-- hosts that distinguish strict from lazy evaluation.
lazyParam :: String -> TypedTerm Type -> TypedTerm Parameter
lazyParam n ty = Typing.parameter (Core.name $ Phantoms.string n) nothing ty true

sigOf :: [TypedTerm Name] -> [TypedTerm Parameter] -> TypedTerm Type -> TypedTerm TermSignature
sigOf typeVars params resultTy = Typing.termSignature
  (Phantoms.list $ (\v -> Typing.typeParameter v (Phantoms.list ([] :: [TypedTerm TypeClassConstraint]))) <$> typeVars)
  (Phantoms.list params)
  (Typing.result nothing resultTy)

-- | ifElse's signature (hydra.core.lib.logic.ifElse): boolean -> lazy t -> lazy t -> t. Both
-- branches (positions 1, 2) are lazy; the condition (position 0) is strict. The original gap
-- this issue (#800) was discovered against -- validated directly per the issue's request.
ifElseSignature :: TypedTerm TermSignature
ifElseSignature = sigOf [Core.name $ Phantoms.string "x"]
  [strictParam "p" T.boolean, lazyParam "t" (T.variable "x"), lazyParam "f" (T.variable "x")]
  (T.variable "x")

-- | findWithDefault's signature (hydra.core.lib.maps.findWithDefault): the default value
-- (position 0) is lazy, evaluated only when the key is absent.
findWithDefaultSignature :: TypedTerm TermSignature
findWithDefaultSignature = sigOf [Core.name $ Phantoms.string "k", Core.name $ Phantoms.string "v"]
  [lazyParam "def" (T.variable "v"), strictParam "key" (T.variable "k"),
   strictParam "m" (T.apply (T.apply (T.variable "Map") (T.variable "k")) (T.variable "v"))]
  (T.variable "v")

-- | optionals.match's signature (hydra.core.lib.optionals.match): the default value
-- (position 1) is lazy, evaluated only when the optional is none.
optionalsMatchSignature :: TypedTerm TermSignature
optionalsMatchSignature = sigOf [Core.name $ Phantoms.string "x", Core.name $ Phantoms.string "y"]
  [strictParam "m" (T.optional $ T.variable "x"), lazyParam "def" (T.variable "y"),
   strictParam "f" (T.function (T.variable "x") (T.variable "y"))]
  (T.variable "y")

-- | A signature with no lazy parameters at all -- confirms the 'lazy' keyword is absent when
-- no parameter needs it (no spurious markers).
noLazyParamsSignature :: TypedTerm TermSignature
noLazyParamsSignature = sigOf [Core.name $ Phantoms.string "x"]
  [strictParam "p" T.boolean, strictParam "q" (T.variable "x")]
  (T.variable "x")

lazyParameterRoundtripGroup :: TypedTermDefinition TestGroup
lazyParameterRoundtripGroup = define "lazyParameterRoundtripGroup" $
  doc "Round-trip and exact-print test cases for lazy parameter syntax (#800)" $
  Testing.testGroup (string "lazy parameter syntax") nothing (list ([] :: [TypedTerm TestGroup])) (list [
    signaturePrintTest "ifElse: lazy then/else branches print with 'lazy' keyword"
      ifElseSignature
      "\8704x.boolean \8594 lazy x \8594 lazy x \8594 x",

    signatureRoundtripTest "ifElse: lazy then/else branches round-trip" ifElseSignature,

    signaturePrintTest "findWithDefault: lazy default value prints with 'lazy' keyword"
      findWithDefaultSignature
      "\8704k,v.lazy v \8594 k \8594 Map k v \8594 v",

    signatureRoundtripTest "findWithDefault: lazy default value round-trips" findWithDefaultSignature,

    signaturePrintTest "optionals.match: lazy default value prints with 'lazy' keyword"
      optionalsMatchSignature
      "\8704x,y.optional<x> \8594 lazy y \8594 (x \8594 y) \8594 y",

    signatureRoundtripTest "optionals.match: lazy default value round-trips" optionalsMatchSignature,

    signaturePrintTest "no lazy parameters: no 'lazy' keyword appears"
      noLazyParamsSignature
      "\8704x.boolean \8594 x \8594 x",

    signatureRoundtripTest "no lazy parameters: round-trips cleanly" noLazyParamsSignature])

-- #803: module-level import-alias printing. A module whose two dependencies share the trailing
-- segment 'file' forces the asymmetric collision rule (syntax.md §6.4) to assign a MULTI-segment
-- alias ('error.file') to one of them, exercising both the printer's alias threading
-- (Print/Core.hs's printNameAliased, via Print/Packaging.hs's module') and the parser's
-- longest-prefix alias expansion (Parse/Packaging.hs's expandAliasedName). Mirrors the confirmed
-- Files.hydra case from the issue.

-- | A monomorphic type scheme (no quantified variables, no constraints) wrapping the given type.
monoScheme :: TypedTerm Type -> TypedTerm TypeScheme
monoScheme ty = Core.typeScheme (Phantoms.list ([] :: [TypedTerm Name])) ty Maps.empty

-- | The #803 example module: 'type Thing = (hydra.core.file.FilePath, hydra.core.error.file.FileError)'
-- with both referenced modules declared as dependencies (error.file listed first so the printed
-- import order is deterministic). Printing must shorten both references via the computed aliases.
aliasExampleModule :: TypedTerm Module
aliasExampleModule = Packaging.module_
  (Packaging.moduleName2 $ Phantoms.string "hydra.example.app")
  nothing
  (Phantoms.list [
    Packaging.moduleDependency (Packaging.moduleName2 $ Phantoms.string "hydra.core.error.file") nothing,
    Packaging.moduleDependency (Packaging.moduleName2 $ Phantoms.string "hydra.core.file") nothing])
  (Phantoms.list [
    Packaging.definitionType (Packaging.typeDefinition
      (Core.name $ Phantoms.string "hydra.example.app.Thing")
      nothing
      (monoScheme (T.pair (T.variable "hydra.core.file.FilePath") (T.variable "hydra.core.error.file.FileError"))))])

-- | Exact-print test: the printed module uses the computed aliases on its import lines AND in the
-- type-definition body ('file.FilePath'/'error.file.FileError'), proving the alias lines are
-- load-bearing (#803).
moduleAliasPrintTest :: TypedTerm TestCaseWithMetadata
moduleAliasPrintTest = universalCase "module prints import aliases and uses them in definition bodies"
  (PrintPackaging.module' @@ aliasExampleModule)
  (Phantoms.string $ L.intercalate "\n" [
    "module hydra.example.app",
    "",
    "import hydra.core.error.file as error.file",
    "import hydra.core.file as file",
    "",
    "type Thing = \8704.(file.FilePath, error.file.FileError)"])

-- | Round-trip test: printing then parsing then re-printing the module yields identical text. This
-- is the load-bearing contract -- the printed aliases (including the multi-segment 'error.file')
-- must re-expand to the original fully-qualified names so the second print matches the first.
moduleAliasRoundtripTest :: TypedTerm TestCaseWithMetadata
moduleAliasRoundtripTest = universalCase "module alias print round-trips (print -> parse -> print)"
  actual expected
  where
    printed = PrintPackaging.module' @@ aliasExampleModule
    actual = Eithers.either
      (Phantoms.lambda "e" $ Phantoms.var "e")
      (Phantoms.lambda "mod2" $ PrintPackaging.module' @@ Phantoms.var "mod2")
      (ParsePackaging.parseModule @@ printed)
    expected = printed

moduleAliasGroup :: TypedTermDefinition TestGroup
moduleAliasGroup = define "moduleAliasGroup" $
  doc "Module-level import-alias printing and round-trip test cases (#803)" $
  Testing.testGroup (string "import aliases") nothing (list ([] :: [TypedTerm TestGroup])) (list [
    moduleAliasPrintTest,
    moduleAliasRoundtripTest])
