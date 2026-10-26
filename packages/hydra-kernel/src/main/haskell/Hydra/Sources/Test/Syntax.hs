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

-- #804: import aliasing is gated by actual LOCAL-NAME collisions, not module-name-suffix
-- collisions. A dependency module is imported BARE (no 'as') iff none of its referenced local
-- names collide with another bare import's (or with the printed module's own declared names); only
-- genuine collisions force the shortest-unique-suffix alias machinery (#803's growAliasesStep).
-- The set of modules left bare is the Maximum-Weight Independent Set of the local-name conflict
-- graph (PrintPackaging.mwisSolve), weight = reference count. These tests exercise the gate.
--
-- NOTE for anyone diffing #803 -> #804: 'moduleAliasPrintTest' below changed its expected output.
-- Under #803, FilePath (hydra.core.file) and FileError (hydra.core.error.file) were wrongly aliased
-- 'file'/'error.file' purely because the two MODULE names share the trailing segment 'file'. But
-- the two LOCAL names never collide, so #804 prints both imports bare and both names unqualified.
-- This is the exact bug #804 fixes -- not a regression against #803.

-- | A monomorphic type scheme (no quantified variables, no constraints) wrapping the given type.
monoScheme :: TypedTerm Type -> TypedTerm TypeScheme
monoScheme ty = Core.typeScheme (Phantoms.list ([] :: [TypedTerm Name])) ty Maps.empty

-- | A module with the given name, dependencies (each a bare ModuleDependency on the given
-- ModuleName string), and type definitions (each a 'type <Local> = <scheme>').
testModule :: String -> [String] -> [(String, TypedTerm TypeScheme)] -> TypedTerm Module
testModule modName deps typeDefs = Packaging.module_
  (Packaging.moduleName2 $ Phantoms.string modName)
  nothing
  (Phantoms.list [Packaging.moduleDependency (Packaging.moduleName2 $ Phantoms.string d) nothing | d <- deps])
  (Phantoms.list [
    Packaging.definitionType (Packaging.typeDefinition
      (Core.name $ Phantoms.string (modName L.++ "." L.++ local)) nothing scheme)
    | (local, scheme) <- typeDefs])

-- | The #804 no-collision example: FilePath (hydra.core.file) and FileError (hydra.core.error.file)
-- have DIFFERENT local names, so even though the two module names share the trailing segment 'file',
-- neither needs an alias. Both imports print bare and both names print unqualified. (Under #803 this
-- same module wrongly produced 'import ... as file' / 'as error.file'.)
aliasExampleModule :: TypedTerm Module
aliasExampleModule = testModule "hydra.example.app"
  ["hydra.core.error.file", "hydra.core.file"]
  [("Thing", monoScheme (T.pair (T.variable "hydra.core.file.FilePath") (T.variable "hydra.core.error.file.FileError")))]

-- | Exact-print test: no collision -> both imports bare, both names unqualified (#804).
moduleAliasPrintTest :: TypedTerm TestCaseWithMetadata
moduleAliasPrintTest = universalCase "no local-name collision: imports print bare, names unqualified"
  (PrintPackaging.module' @@ aliasExampleModule)
  (Phantoms.string $ L.intercalate "\n" [
    "module hydra.example.app",
    "",
    "import hydra.core.error.file",
    "import hydra.core.file",
    "",
    "type Thing = \8704.(FilePath, FileError)"])

-- | A genuine two-module collision: both hydra.a and hydra.b contribute a local name 'Foo'. They
-- conflict, so at most one may be bare. hydra.a's Foo is referenced TWICE (two type defs) vs
-- hydra.b's once, so hydra.a has the higher MWIS weight and wins the bare slot; hydra.b loses and
-- is aliased by the shortest-unique-suffix rule (its distinct suffix 'b'). The printed body shows
-- a bare 'Foo' (from a) and a qualified 'b.Foo' (from b).
collisionModule :: TypedTerm Module
collisionModule = testModule "hydra.app"
  ["hydra.a", "hydra.b"]
  [("T1", monoScheme (T.variable "hydra.a.Foo")),
   ("T2", monoScheme (T.pair (T.variable "hydra.a.Foo") (T.variable "hydra.b.Foo")))]

moduleCollisionPrintTest :: TypedTerm TestCaseWithMetadata
moduleCollisionPrintTest = universalCase "local-name collision: higher-weight module wins the bare slot, loser is aliased"
  (PrintPackaging.module' @@ collisionModule)
  (Phantoms.string $ L.intercalate "\n" [
    "module hydra.app",
    "",
    "import hydra.a",
    "import hydra.b as b",
    "",
    "type T1 = \8704.Foo",
    "",
    "type T2 = \8704.(Foo, b.Foo)"])

-- | MWIS path case A-B-C, the greedy-catcher: three dependency modules where A and C each collide
-- only with B (B shares local name 'X' with A and 'Y' with C; A and C share nothing). Weights:
-- aa=2 (its X is referenced in D1 and D3), bb=2 (X in D1, Y in D2), cc=1 (Y in D2). The independent
-- set {aa, cc} has weight 3, STRICTLY greater than {bb} alone at weight 2 -- so the exact solver
-- leaves aa and cc bare and aliases only bb. A greedy "highest-degree first" heuristic would wrongly
-- take bb (degree 2) and alias both aa and cc, scoring only 2. Asserted via the printed module text
-- (a String, as universalCase requires): the bare vs aliased split shows up directly as bare 'X'/'Y'
-- (from aa/cc) vs qualified 'bb.X'/'bb.Y' (from bb).
pathModule :: TypedTerm Module
pathModule = testModule "hydra.app"
  ["hydra.aa", "hydra.bb", "hydra.cc"]
  -- A (hydra.aa) and B (hydra.bb) collide on 'X'; B and C (hydra.cc) collide on 'Y'; A,C disjoint.
  -- aa.X referenced twice (D1, D3) so {aa,cc} STRICTLY outweighs {bb}.
  [("D1", monoScheme (T.pair (T.variable "hydra.aa.X") (T.variable "hydra.bb.X"))),
   ("D2", monoScheme (T.pair (T.variable "hydra.bb.Y") (T.variable "hydra.cc.Y"))),
   ("D3", monoScheme (T.variable "hydra.aa.X"))]

-- | Expected print of 'pathModule': aa and cc bare, bb aliased (its distinct suffix 'bb'); the
-- bodies therefore show bare X/Y for aa/cc and qualified bb.X/bb.Y for bb.
moduleAliasPathTest :: TypedTerm TestCaseWithMetadata
moduleAliasPathTest = universalCase "MWIS path A-B-C: A+C stay bare, only B is aliased (exact solver beats greedy)"
  (PrintPackaging.module' @@ pathModule)
  (Phantoms.string $ L.intercalate "\n" [
    "module hydra.app",
    "",
    "import hydra.aa",
    "import hydra.bb as bb",
    "import hydra.cc",
    "",
    "type D1 = \8704.(X, bb.X)",
    "",
    "type D2 = \8704.(bb.Y, Y)",
    "",
    "type D3 = \8704.X"])

-- | Round-trip test for the no-collision module: print -> parse -> print yields identical text.
moduleAliasRoundtripTest :: TypedTerm TestCaseWithMetadata
moduleAliasRoundtripTest = universalCase "bare-import module round-trips (print -> parse -> print)"
  (roundtripPrint aliasExampleModule) (PrintPackaging.module' @@ aliasExampleModule)

-- NO collision round-trip test (#804): a module whose collision forces one import bare and the
-- other aliased does NOT survive a TEXTUAL print->parse->print through the single-file parser, and
-- that is a pre-existing PARSER limitation, not a printer bug. Concretely, 'collisionModule' first
-- prints 'import hydra.b as b' with body '(Foo, b.Foo)' (hydra.a bare, hydra.b aliased -- correct,
-- asserted by 'moduleCollisionPrintTest'). On reparse the bare 'Foo' from bare-imported hydra.a
-- cannot be re-qualified to 'hydra.a.Foo': the single-file parser has no access to hydra.a's
-- definitions, so it keeps 'Foo' as a module-less local name (see Parse/Packaging.hs's
-- 'expandAliasedName' note, lines ~254-262). 'b.Foo' DOES re-expand via its alias. The second print
-- then sees only hydra.b referenced (hydra.a's contribution became a module-less name), finds no
-- collision, and prints hydra.b bare too -> '(Foo, Foo)'. Asserting that lossy second print as the
-- expected output would enshrine broken round-trip as correct, so there is deliberately no such
-- assertion here. The no-collision case ('moduleAliasRoundtripTest') round-trips fine because it has
-- no alias to lose. Full semantic (collision) round-trip needs parser bare-name re-qualification
-- against a real module environment -- tracked as a separate follow-up, out of #804's printer scope.

-- | print -> parse -> print of a module (yielding the error message on a parse failure).
roundtripPrint :: TypedTerm Module -> TypedTerm String
roundtripPrint m = Eithers.either
  (Phantoms.lambda "e" $ Phantoms.var "e")
  (Phantoms.lambda "mod2" $ PrintPackaging.module' @@ Phantoms.var "mod2")
  (ParsePackaging.parseModule @@ (PrintPackaging.module' @@ m))

moduleAliasGroup :: TypedTermDefinition TestGroup
moduleAliasGroup = define "moduleAliasGroup" $
  doc "Import-alias gating by local-name collision + MWIS, printing and round-trip (#804)" $
  Testing.testGroup (string "import aliases") nothing (list ([] :: [TypedTerm TestGroup])) (list [
    moduleAliasPrintTest,
    moduleCollisionPrintTest,
    moduleAliasPathTest,
    moduleAliasRoundtripTest])
