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
import qualified Hydra.Core.Dsl.Lib.Lists    as Lists
import qualified Hydra.Core.Dsl.Lib.Literals as LibLiterals
import qualified Hydra.Core.Dsl.Lib.Maps     as Maps
import qualified Hydra.Core.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Core.Dsl.Lib.Strings  as Strings
import qualified Hydra.Core.Dsl.Paths        as Paths
import qualified Hydra.Core.Dsl.Packaging     as Packaging
import qualified Hydra.Sources.Kernel.Terms.Print.Core as PrintCore
import qualified Hydra.Sources.Kernel.Terms.Print.Packaging as PrintPackaging
import qualified Hydra.Sources.Kernel.Terms.Parse.Core as ParseCore
import qualified Hydra.Sources.Kernel.Terms.Parse.Packaging as ParsePackaging
import qualified Hydra.Sources.Kernel.Terms.Positions as Positions

import Hydra.Core.Testing


ns :: ModuleName
ns = ModuleName "hydra.core.test.syntax"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([TestGraph.ns, PrintCore.ns, PrintPackaging.ns, ParseCore.ns, ParsePackaging.ns, Positions.ns] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata ((Just "Round-trip test cases for the textual syntax"))}
  where
    definitions = [
      Phantoms.toDefinition allTests,
      Phantoms.toDefinition doSugarGroup,
      Phantoms.toDefinition lazyParameterRoundtripGroup,
      Phantoms.toDefinition moduleAliasGroup,
      Phantoms.toDefinition nominalSugarGroup,
      Phantoms.toDefinition sourcePositionGroup,
      Phantoms.toDefinition sourceSpanGroup]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    doc "Test cases for the textual syntax (#497/#710)" $
    Testing.testGroup (string "textual syntax") nothing (list subgroups) (list ([] :: [TypedTerm TestCaseWithMetadata]))
  where
    subgroups = [doSugarGroup, lazyParameterRoundtripGroup, moduleAliasGroup, nominalSugarGroup, sourceSpanGroup, sourcePositionGroup]

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

-- #804 introduced bare (unqualified) cross-module imports, gated by actual LOCAL-NAME collisions
-- rather than module-name-suffix collisions: a dependency module was imported BARE (no 'as') iff
-- none of its referenced local names collided with another bare import's (or with the printed
-- module's own declared names), with genuine collisions resolved by the shortest-unique-suffix
-- alias machinery (#803's growAliasesStep) and the bare/alias split chosen by
-- 'PrintPackaging.mwisSolve' (a Maximum-Weight Independent Set over the local-name conflict
-- graph, weight = reference count).
--
-- #804's bare imports turned out to be unparseable in general (#805): a single-file parser has no
-- access to a bare import's own declared names, so it can never re-qualify a bare body reference
-- back to the module it came from -- not in the no-collision case (#711) and not in the
-- split-bare-vs-aliased collision case (#805) #804 itself surfaced. Per Josh's 2026-10-31
-- Decision Desk ruling (card '711-bare-import-roundtrip'), the required contract is
-- 'module -> print -> parse == original module' (structural identity), which a bare cross-module
-- import can never satisfy. The fix (folded into #711): 'moduleAliases' no longer leaves any
-- cross-module dependency bare -- every one gets a real alias from the existing shortest-suffix
-- fixed point; 'mwisSolve' and the bare/alias MWIS split it solved are removed as dead code. The
-- tests below are updated accordingly: no import line is ever bare, and the round-trip test now
-- asserts true module-definitions identity, not just printed-text stability.

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

-- | The former #804 no-collision example: FilePath (hydra.core.file) and FileError
-- (hydra.core.error.file) have DIFFERENT local names, so leaving either bare would have
-- introduced no local-name collision -- but since 2026-10-31 every cross-module import is
-- aliased regardless (see 'PrintPackaging.moduleAliases'), both still get a real alias from the
-- shortest-unique-suffix fixed point: 'hydra.core.file' -> 'file' (its one-segment suffix is
-- already unique), 'hydra.core.error.file' -> 'error.file' (its one-segment suffix 'file'
-- collides with the former, so it grows one segment).
aliasExampleModule :: TypedTerm Module
aliasExampleModule = testModule "hydra.example.app"
  ["hydra.core.error.file", "hydra.core.file"]
  [("Thing", monoScheme (T.pair (T.variable "hydra.core.file.FilePath") (T.variable "hydra.core.error.file.FileError")))]

-- | Exact-print test: every import is aliased, even with no local-name collision (2026-10-31
-- round-trip-identity ruling superseding #804's bare-import case).
moduleAliasPrintTest :: TypedTerm TestCaseWithMetadata
moduleAliasPrintTest = universalCase "no local-name collision: imports are still aliased (round-trip identity)"
  (PrintPackaging.module' @@ aliasExampleModule)
  (Phantoms.string $ L.intercalate "\n" [
    "module hydra.example.app",
    "",
    "import hydra.core.error.file as error.file",
    "import hydra.core.file as file",
    "",
    "type Thing = (file.FilePath, error.file.FileError)"])

-- | A genuine two-module collision: both hydra.a and hydra.b contribute a local name 'Foo'. Under
-- #804 this chose a bare/aliased split by MWIS weight; since 2026-10-31 both are unconditionally
-- aliased by the (unchanged) shortest-unique-suffix rule: 'hydra.a' -> 'a', 'hydra.b' -> 'b'.
collisionModule :: TypedTerm Module
collisionModule = testModule "hydra.app"
  ["hydra.a", "hydra.b"]
  [("T1", monoScheme (T.variable "hydra.a.Foo")),
   ("T2", monoScheme (T.pair (T.variable "hydra.a.Foo") (T.variable "hydra.b.Foo")))]

moduleCollisionPrintTest :: TypedTerm TestCaseWithMetadata
moduleCollisionPrintTest = universalCase "local-name collision: both sides are aliased, never bare"
  (PrintPackaging.module' @@ collisionModule)
  (Phantoms.string $ L.intercalate "\n" [
    "module hydra.app",
    "",
    "import hydra.a as a",
    "import hydra.b as b",
    "",
    "type T1 = a.Foo",
    "",
    "type T2 = (a.Foo, b.Foo)"])

-- | Three dependency modules, formerly exercising #804's MWIS path case (A-B-C: A and C each
-- collide only with B). Since bareness is no longer a printer choice at all, the MWIS weighting
-- that used to decide which side of each collision stayed bare is moot -- all three simply get
-- their shortest-unique-suffix alias ('aa', 'bb', 'cc', none colliding with each other).
pathModule :: TypedTerm Module
pathModule = testModule "hydra.app"
  ["hydra.aa", "hydra.bb", "hydra.cc"]
  [("D1", monoScheme (T.pair (T.variable "hydra.aa.X") (T.variable "hydra.bb.X"))),
   ("D2", monoScheme (T.pair (T.variable "hydra.bb.Y") (T.variable "hydra.cc.Y"))),
   ("D3", monoScheme (T.variable "hydra.aa.X"))]

-- | Expected print of 'pathModule': all three dependencies aliased by their one-segment suffix
-- (none collide with each other), so every body reference is qualified.
moduleAliasPathTest :: TypedTerm TestCaseWithMetadata
moduleAliasPathTest = universalCase "three non-colliding dependencies: all three are still aliased"
  (PrintPackaging.module' @@ pathModule)
  (Phantoms.string $ L.intercalate "\n" [
    "module hydra.app",
    "",
    "import hydra.aa as aa",
    "import hydra.bb as bb",
    "import hydra.cc as cc",
    "",
    "type D1 = (aa.X, bb.X)",
    "",
    "type D2 = (bb.Y, cc.Y)",
    "",
    "type D3 = aa.X"])

-- | Structural round-trip fingerprint for a module: its name plus every definition's LOCAL name
-- and FULLY-QUALIFIED (alias-independent, 'PrintCore.noAliases') type/term text. Deliberately
-- excludes 'moduleDependencies' (the printer may choose different import lines on reprint -- "the
-- import structure need not be preserved", 2026-10-31 ruling) and metadata (doc-comment parsing
-- is a separate, already-tracked gap). Comparing fully-qualified bodies rather than the aliased
-- printed text means this fingerprint is itself immune to any alias-choice difference between two
-- prints of structurally-identical modules -- exactly the identity bar the round-trip tests below
-- must satisfy: 'module -> print -> parse' must yield a module whose fingerprint matches the
-- original's.
moduleFingerprint :: TypedTerm Module -> TypedTerm String
moduleFingerprint m =
  Strings.concat (Phantoms.list [
    Packaging.unModuleName (Packaging.moduleName m), Phantoms.string "\n",
    Strings.join (Phantoms.string "\n") (Lists.map ("d" ~> defFingerprint (Phantoms.var "d")) (Packaging.moduleDefinitions m))])
  where
    defFingerprint :: TypedTerm Definition -> TypedTerm String
    defFingerprint d = Phantoms.match _Definition d Nothing [
      _Definition_type>>: "td" ~> Strings.concat (Phantoms.list [
        Core.unName (Packaging.typeDefinitionName (var "td")), Phantoms.string " = ",
        PrintCore.typeScheme @@ (Packaging.typeDefinitionBody (var "td"))]),
      _Definition_term>>: "td" ~> Strings.concat (Phantoms.list [
        Core.unName (Packaging.termDefinitionName (var "td")), Phantoms.string " = ",
        PrintCore.term @@ (Packaging.termDefinitionBody (var "td"))]),
      _Definition_primitive>>: "pd" ~> Core.unName (Packaging.primitiveDefinitionName (var "pd"))]

-- | print -> parse -> fingerprint of a module (yielding the parse error message on failure).
roundtripFingerprint :: TypedTerm Module -> TypedTerm String
roundtripFingerprint m = Eithers.either
  (Phantoms.lambda "e" $ Phantoms.var "e")
  (Phantoms.lambda "mod2" $ moduleFingerprint (Phantoms.var "mod2"))
  (ParsePackaging.parseModule @@ (PrintPackaging.module' @@ m))

-- | Round-trip identity test for the former #804 no-collision module: 'module -> print -> parse'
-- must yield a module whose fingerprint matches the original -- the 2026-10-31 Decision Desk bar
-- (card '711-bare-import-roundtrip'), folding #711's no-collision symptom into a real fix rather
-- than an accepted gap. Every cross-module import now being aliased (see 'moduleAliasPrintTest')
-- gives the parser a lookup key for every body reference, so this holds unconditionally.
moduleAliasRoundtripTest :: TypedTerm TestCaseWithMetadata
moduleAliasRoundtripTest = universalCase "no-collision module round-trips to an identical module (module->print->parse)"
  (roundtripFingerprint aliasExampleModule)
  (moduleFingerprint aliasExampleModule)

-- | Round-trip identity test for the former #805 collision module: same bar as
-- 'moduleAliasRoundtripTest', now also covering the split-bare-vs-aliased collision case #805
-- found unparseable under #804. Since both sides of a collision are now always aliased (see
-- 'moduleCollisionPrintTest'), the parser has a lookup key for both, and this holds too -- #805 is
-- closed by the same fix, not by a separate parser change.
moduleCollisionRoundtripTest :: TypedTerm TestCaseWithMetadata
moduleCollisionRoundtripTest = universalCase "collision module round-trips to an identical module (module->print->parse, #805)"
  (roundtripFingerprint collisionModule)
  (moduleFingerprint collisionModule)

moduleAliasGroup :: TypedTermDefinition TestGroup
moduleAliasGroup = define "moduleAliasGroup" $
  doc "Import aliasing: every cross-module dependency is aliased, and module->print->parse round-trips to an identical module (#711, #804, #805)" $
  Testing.testGroup (string "import aliases") nothing (list ([] :: [TypedTerm TestGroup])) (list [
    moduleAliasPrintTest,
    moduleCollisionPrintTest,
    moduleAliasPathTest,
    moduleAliasRoundtripTest,
    moduleCollisionRoundtripTest])

----------------------------------------------------------------------------------------------
-- #809: nominal construction/elimination sugar (Wave 1 -- record/injection/wrap + project/unwrap).
-- These are graph-free string round-trips (print -> parse -> print). Note: an injection prints as
-- the SAME one-field record-style construct string 'Name{field}', so its string round-trip passes
-- without the packaging-pass injection correction (which operates at the AST level against a schema
-- graph, exercised separately once Parse/Packaging.hs's correction lands).

-- | Print a term, parse the printed text back, and re-print -- compares to the original printed
-- form (self-comparing string round-trip, mirroring 'signatureRoundtripTest').
termRoundtripTest :: String -> TypedTerm Term -> TypedTerm TestCaseWithMetadata
termRoundtripTest testName t = universalCase testName actual expected
  where
    printed = PrintCore.term @@ t
    actual = Eithers.either
      (Phantoms.lambda "e" $ Phantoms.var "e")
      (Phantoms.lambda "t2" $ PrintCore.term @@ Phantoms.var "t2")
      (ParseCore.parseTerm @@ printed)
    expected = printed

-- | Print a term and compare against an exact expected string -- pins the canonical sugar form.
termPrintTest :: String -> TypedTerm Term -> String -> TypedTerm TestCaseWithMetadata
termPrintTest testName t expectedStr = universalCase testName
  (PrintCore.term @@ t)
  (Phantoms.string expectedStr)

-- | Parse an input string, re-print the parsed term, and compare to the printed form of an expected
-- term. Used for the applied 'case' sugar (#809 Wave 2), which is PARSE-ONLY: 'case T x {…}'
-- desugars to 'application(cases(T){…}) x', and the printer re-emits only that desugared form, so a
-- self-round-trip would not reproduce the 'case' spelling. This checks the desugaring directly:
-- parse('case …') must equal the hand-built applied-'cases' term (compared via their printed forms).
caseSugarParseTest :: String -> String -> TypedTerm Term -> TypedTerm TestCaseWithMetadata
caseSugarParseTest testName input expected = universalCase testName
  (Eithers.either
    (Phantoms.lambda "e" $ Phantoms.var "e")
    (Phantoms.lambda "t2" $ PrintCore.term @@ Phantoms.var "t2")
    (ParseCore.parseTerm @@ Phantoms.string input))
  (PrintCore.term @@ expected)

-- Applied-case desugaring builders (#809 Wave 2).
lamT :: String -> TypedTerm Term -> TypedTerm Term
lamT v body = Core.termLambda (Core.lambda (nm v) nothing body)

varT :: String -> TypedTerm Term
varT v = Core.termVariable (nm v)

appT :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
appT f a = Core.termApplication (Core.application f a)

-- cases(T){alts}, with optional default
casesT :: String -> [TypedTerm Field] -> Maybe (TypedTerm Term) -> TypedTerm Term
casesT tname alts mdef = Core.termCases (Core.caseStatement (nm tname)
  (maybe nothing just mdef)
  (Phantoms.list [Core.caseAlternative (Core.fieldName f) (Core.fieldTerm f) | f <- alts]))

-- Expected desugaring of: case example.Color x { red y = "r", green = "g", _ = "c" }
--   → application(cases(example.Color){ red = λy. "r", green = "g", _default = "c" }) x
caseSugarExpected :: TypedTerm Term
caseSugarExpected = appT
  (casesT "example.Color"
    [fld "red" (lamT "y" (strT "r")), fld "green" (strT "g")]
    (Just (strT "c")))
  (varT "x")

-- | Parse a sugared input string and compare the PRINTED parse result against the printed form of an
-- expected (hand-built, already-desugared) term. This is the test shape for #811 PARSE-ONLY sugar:
-- the printer never emits these forms, so round-trip can't exercise them; instead we assert that
-- 'parse(sugar)' yields exactly the explicit desugared term. Comparing printed strings (rather than
-- the raw 'Either') keeps a parse failure legible (the error message surfaces as the actual value).
termParseTest :: String -> String -> TypedTerm Term -> TypedTerm TestCaseWithMetadata
termParseTest testName input expectedTerm = universalCase testName actual expected
  where
    actual = Eithers.either
      (Phantoms.lambda "e" $ Phantoms.var "e")
      (Phantoms.lambda "t2" $ PrintCore.term @@ Phantoms.var "t2")
      (ParseCore.parseTerm @@ Phantoms.string input)
    expected = PrintCore.term @@ expectedTerm

-- Term builders (shallow DSL) for the sugar fixtures.
nm :: String -> TypedTerm Name
nm s = Core.name $ Phantoms.string s

strT :: String -> TypedTerm Term
strT s = Core.termLiteral (Core.literalString $ Phantoms.string s)

i32T :: Int -> TypedTerm Term
i32T n = Core.termLiteral (Core.literalInteger (Core.integerValueInt32 (Phantoms.int32 n)))

fld :: String -> TypedTerm Term -> TypedTerm Field
fld n t = Core.field (nm n) t

unitFld :: String -> TypedTerm Field
unitFld n = Core.field (nm n) Core.termUnit

-- record Person{firstName = "Alice", age = 30:int32}
recordPersonTerm :: TypedTerm Term
recordPersonTerm = Core.termRecord (Core.record (nm "example.Person")
  (Phantoms.list [fld "firstName" (strT "Alice"), fld "age" (i32T 30)]))

-- record Person{contactInfo, firstName = "Arthur"} -- bareword unit field
recordBarewordTerm :: TypedTerm Term
recordBarewordTerm = Core.termRecord (Core.record (nm "example.Person")
  (Phantoms.list [unitFld "contactInfo", fld "firstName" (strT "Arthur")]))

-- injection Color{red} -- unit-valued variant (prints identically to a one-bare-field record)
injectUnitTerm :: TypedTerm Term
injectUnitTerm = Core.termInject (Core.injection (nm "example.Color") (unitFld "red"))

-- injection Length{meters = 1.0:float64-ish} -- data-carrying variant (use int for simplicity)
injectDataTerm :: TypedTerm Term
injectDataTerm = Core.termInject (Core.injection (nm "example.Length") (fld "meters" (i32T 1)))

-- wrap Meters{100:int32} -- literal body, sugars safely
wrapLiteralTerm :: TypedTerm Term
wrapLiteralTerm = Core.termWrap (Core.wrappedTerm (nm "example.Meters") (i32T 100))

-- wrap Meters{x} -- variable body, must keep keyword form 'wrap(...)'
wrapVariableTerm :: TypedTerm Term
wrapVariableTerm = Core.termWrap (Core.wrappedTerm (nm "example.Meters") (Core.termVariable (nm "x")))

-- project Person[firstName]
projectTermVal :: TypedTerm Term
projectTermVal = Core.termProject (Core.projection (nm "example.Person") (nm "firstName"))

-- unwrap Meters[]
unwrapTermVal :: TypedTerm Term
unwrapTermVal = Core.termUnwrap (nm "example.Meters")

nominalSugarGroup :: TypedTermDefinition TestGroup
nominalSugarGroup = define "nominalSugarGroup" $
  doc "Nominal construction/elimination sugar: print + round-trip (#809)" $
  Testing.testGroup (string "nominal sugar") nothing (list ([] :: [TypedTerm TestGroup])) (list [
    termPrintTest "record sugar: multi-field"
      recordPersonTerm "example.Person{firstName = \"Alice\", age = 30:int32}",
    termRoundtripTest "record sugar: multi-field round-trips" recordPersonTerm,

    termPrintTest "record sugar: bareword unit field"
      recordBarewordTerm "example.Person{contactInfo, firstName = \"Arthur\"}",
    termRoundtripTest "record sugar: bareword unit field round-trips" recordBarewordTerm,

    termPrintTest "injection sugar: unit variant prints as bareword record-style construct"
      injectUnitTerm "example.Color{red}",
    termRoundtripTest "injection sugar: unit variant string round-trips" injectUnitTerm,

    termPrintTest "injection sugar: data variant"
      injectDataTerm "example.Length{meters = 1:int32}",
    termRoundtripTest "injection sugar: data variant string round-trips" injectDataTerm,

    termPrintTest "wrap sugar: literal body"
      wrapLiteralTerm "example.Meters{100:int32}",
    termRoundtripTest "wrap sugar: literal body round-trips" wrapLiteralTerm,

    termPrintTest "wrap: variable body keeps keyword form (ambiguous as record field otherwise)"
      wrapVariableTerm "wrap(example.Meters){x}",
    termRoundtripTest "wrap: variable body keyword form round-trips" wrapVariableTerm,

    termPrintTest "project sugar: Name[field]"
      projectTermVal "example.Person[firstName]",
    termRoundtripTest "project sugar round-trips" projectTermVal,

    termPrintTest "unwrap sugar: Name[]"
      unwrapTermVal "example.Meters[]",
    termRoundtripTest "unwrap sugar round-trips" unwrapTermVal,

    -- #822: raw 'cases' with a '_' default handler must round-trip print -> parse -> print.
    termRoundtripTest "cases: '_' default handler round-trips"
      (casesT "example.Color" [fld "red" (strT "stop"), fld "green" (strT "go")] (Just (strT "caution"))),
    termRoundtripTest "cases: '_' default only round-trips"
      (casesT "example.Color" [] (Just (strT "caution"))),

    -- #809 Wave 2: applied 'case' sugar (parse-only; desugars to an applied 'cases').
    caseSugarParseTest "case sugar: binder branch → λ-handler, nullary branch as-is, default"
      "case example.Color x { red y = \"r\", green = \"g\", _ = \"c\" }"
      caseSugarExpected,

    caseSugarParseTest "case sugar: single nullary branch desugars to applied cases"
      "case example.Color x { red = \"stop\" }"
      (appT (casesT "example.Color" [fld "red" (strT "stop")] Nothing) (varT "x")),

    -- edge: a binder branch alone → λ-handler binding the payload (no default)
    caseSugarParseTest "case sugar: binder branch desugars handler to a lambda over the payload"
      "case example.Length len { meters m = m }"
      (appT (casesT "example.Length" [fld "meters" (lamT "m" (varT "m"))] Nothing) (varT "len")),

    -- edge: a no-binder (nullary) branch keeps the handler as-is, no spurious lambda
    caseSugarParseTest "case sugar: no-binder branch keeps handler as-is"
      "case example.Color c { red = \"r\", green = \"g\" }"
      (appT (casesT "example.Color" [fld "red" (strT "r"), fld "green" (strT "g")] Nothing) (varT "c")),

    -- edge: a compound scrutinee must be parenthesized (scrutinee parsed at postfix level)
    caseSugarParseTest "case sugar: parenthesized compound scrutinee"
      "case example.Color (f y) { red = \"r\" }"
      (appT (casesT "example.Color" [fld "red" (strT "r")] Nothing) (appT (varT "f") (varT "y")))])

-- #811 'do'-block fixtures ------------------------------------------------------------------------
-- (reuses appT/lamT/varT from the #809 Wave 2 builders above; adds refT/bindT)

-- A primitive/variable reference term, e.g. the bind primitive or a plain variable.
refT :: String -> TypedTerm Term
refT s = Core.termVariable (nm s)

-- '<tag>s.bind e k' == app (app bindRef e) k
bindT :: String -> TypedTerm Term -> TypedTerm Term -> TypedTerm Term
bindT bindPrim e k = appT (appT (refT bindPrim) e) k

-- Expected desugaring of 'do either { x := e1, y := e2, r }':
--   eithers.bind e1 (λx. eithers.bind e2 (λy. r))
-- (e1 = readFile, e2 = decode applied to x; kept abstract as variable applications.)
doEitherExpected :: TypedTerm Term
doEitherExpected =
  bindT "hydra.core.lib.eithers.bind" (appT (refT "readFile") (refT "path"))
    (lamT "x"
      (bindT "hydra.core.lib.eithers.bind" (appT (refT "decode") (refT "x"))
        (lamT "y" (refT "y"))))

-- Expected desugaring of a single-bind block 'do optional { x := e, x }':
--   optionals.bind e (λx. x)
doOptionalExpected :: TypedTerm Term
doOptionalExpected =
  bindT "hydra.core.lib.optionals.bind" (refT "e") (lamT "x" (refT "x"))

-- Expected desugaring of a bare (non-binding) non-final clause 'do effect { eff, r }':
--   effects.bind eff (λ_. r)   -- wildcard binder for a sequenced/discarded clause
doEffectSeqExpected :: TypedTerm Term
doEffectSeqExpected =
  bindT "hydra.core.lib.effects.bind" (refT "eff") (lamT "_" (refT "r"))

-- Expected desugaring of a single-clause block 'do list { r }': just the result, no bind.
doListSingleExpected :: TypedTerm Term
doListSingleExpected = refT "r"

doSugarGroup :: TypedTermDefinition TestGroup
doSugarGroup = define "doSugarGroup" $
  doc "Monadic 'do' sugar (#811): parse-only; 'do <tag> { clauses }' desugars to <tag>s.bind chains" $
  Testing.testGroup (string "monadic do sugar") nothing (list ([] :: [TypedTerm TestGroup])) (list [
    termParseTest "do either: two binds + result"
      "do either { x := readFile path, y := decode x, y }" doEitherExpected,
    -- newlines are insignificant: same block, multi-line, must parse identically
    termParseTest "do either: multi-line (newlines insignificant)"
      "do either {\n  x := readFile path,\n  y := decode x,\n  y\n}" doEitherExpected,
    termParseTest "do optional: single bind"
      "do optional { x := e, x }" doOptionalExpected,
    termParseTest "do effect: bare non-final clause uses wildcard binder"
      "do effect { eff, r }" doEffectSeqExpected,
    termParseTest "do list: single result clause (no bind)"
      "do list { r }" doListSingleExpected])

----------------------------------------------------------------------------------------------
-- #815: source spans

-- | Show an optional span as "[start,end)" (or "none")
showSpan :: TypedTerm (Maybe SourceSpan) -> TypedTerm String
showSpan m = Optionals.match m (string "none") (Phantoms.lambda "sp" $ Strings.concat (list [
  string "[",
  LibLiterals.printInt32 (Phantoms.project _SourceSpan _SourceSpan_start Phantoms.@@ Phantoms.var "sp"),
  string ",",
  LibLiterals.printInt32 (Phantoms.project _SourceSpan _SourceSpan_end Phantoms.@@ Phantoms.var "sp"),
  string ")"]))

-- | Parse with spans, resolve a subterm path, and show the resulting span
spanAtPathTest :: String -> String -> [TypedTerm SubtermStep] -> String -> TypedTerm TestCaseWithMetadata
spanAtPathTest testName input steps expectedStr = universalCase testName
  (Eithers.either
    (Phantoms.lambda "e" $ Phantoms.var "e")
    (Phantoms.lambda "t" $ showSpan (Positions.spanAtPath @@ Paths.subtermPath (list steps) @@ Phantoms.var "t"))
    (ParseCore.parseTermWithSpans @@ string input))
  (string expectedStr)

-- | The span-free term recovered from a span-annotated parse prints identically to the plain parse
stripSpansTest :: String -> String -> TypedTerm TestCaseWithMetadata
stripSpansTest testName input = universalCase testName
  (Eithers.either
    (Phantoms.lambda "e" $ Phantoms.var "e")
    (Phantoms.lambda "t" $ PrintCore.term @@ (Positions.stripSpans @@ Phantoms.var "t"))
    (ParseCore.parseTermWithSpans @@ string input))
  (Eithers.either (Phantoms.lambda "e" $ Phantoms.var "e") (Phantoms.lambda "t" $ PrintCore.term @@ Phantoms.var "t")
    (ParseCore.parseTerm @@ string input))

-- | The plain parser leaves no span annotations behind
plainParseHasNoSpansTest :: String -> String -> TypedTerm TestCaseWithMetadata
plainParseHasNoSpansTest testName input = universalCase testName
  (Eithers.either
    (Phantoms.lambda "e" $ Phantoms.var "e")
    (Phantoms.lambda "t" $ showSpan (Positions.termSpan @@ Phantoms.var "t"))
    (ParseCore.parseTerm @@ string input))
  (string "none")

rootSpanTest :: String -> String -> String -> TypedTerm TestCaseWithMetadata
rootSpanTest testName input expectedStr = spanAtPathTest testName input [] expectedStr

sourceSpanGroup :: TypedTermDefinition TestGroup
sourceSpanGroup = define "sourceSpanGroup" $
  doc "Source spans recorded by the term parser, and SubtermPath-to-span resolution (#815)" $
  Testing.testGroup (string "source spans") nothing (list ([] :: [TypedTerm TestGroup])) (list [
    rootSpanTest "literal" "42:int32" "[0,8)",
    rootSpanTest "span excludes leading whitespace" "  x" "[2,3)",
    rootSpanTest "span excludes trailing whitespace" "x   " "[0,1)",
    rootSpanTest "span excludes trailing comment" "x # note\n" "[0,1)",
    rootSpanTest "application spans function through argument" "f x" "[0,3)",
    rootSpanTest "chained application" "f x y" "[0,5)",
    spanAtPathTest "application function" "f x" [inject _SubtermStep _SubtermStep_applicationFunction unit] "[0,1)",
    spanAtPathTest "application argument" "f x" [inject _SubtermStep _SubtermStep_applicationArgument unit] "[2,3)",
    spanAtPathTest "nested application argument" "f g x"
      [inject _SubtermStep _SubtermStep_applicationFunction unit,
       inject _SubtermStep _SubtermStep_applicationArgument unit] "[2,3)",
    spanAtPathTest "list element" "[1:int32, 22:int32]" [inject _SubtermStep _SubtermStep_listElement (Phantoms.int32 1)] "[10,18)",
    spanAtPathTest "lambda body" "\955x . x" [inject _SubtermStep _SubtermStep_lambdaBody unit] "[5,6)",
    spanAtPathTest "path that cannot be followed yields the last node reached" "f x"
      [inject _SubtermStep _SubtermStep_lambdaBody unit] "[0,3)",
    spanAtPathTest "parenthesized term" "(x)" [] "[0,3)",
    rootSpanTest "do block spans the whole construct" "do either { x := f a, g x }" "[0,27)",
    spanAtPathTest "do clause expression keeps its own span" "do either { x := f a, g x }"
      [inject _SubtermStep _SubtermStep_applicationFunction unit,
       inject _SubtermStep _SubtermStep_applicationArgument unit] "[17,20)",
    spanAtPathTest "do result expression keeps its own span" "do either { x := f a, g x }"
      [inject _SubtermStep _SubtermStep_applicationArgument unit,
       inject _SubtermStep _SubtermStep_lambdaBody unit] "[22,25)",
    spanAtPathTest "do bind primitive falls back to the do block's span" "do either { x := f a, g x }"
      [inject _SubtermStep _SubtermStep_applicationFunction unit,
       inject _SubtermStep _SubtermStep_applicationFunction unit] "[0,27)",
    stripSpansTest "strip spans: do block" "do either { x := f a, g x }",
    stripSpansTest "strip spans: application" "f x y",
    stripSpansTest "strip spans: lambda" "\955x . f x",
    stripSpansTest "strip spans: list" "[1:int32, 2:int32]",
    plainParseHasNoSpansTest "plain parse records no spans" "f x"])

-- | Convert an offset in a text to a position and show it as "line:column"
positionTest :: String -> String -> Int -> String -> TypedTerm TestCaseWithMetadata
positionTest testName text offset expectedStr = universalCase testName
  (Strings.concat (list [
    LibLiterals.printInt32 (Phantoms.project _SourcePosition _SourcePosition_line Phantoms.@@ pos),
    string ":",
    LibLiterals.printInt32 (Phantoms.project _SourcePosition _SourcePosition_column Phantoms.@@ pos)]))
  (string expectedStr)
  where
    pos = Positions.offsetToPosition @@ string text @@ Phantoms.int32 offset

sourcePositionGroup :: TypedTermDefinition TestGroup
sourcePositionGroup = define "sourcePositionGroup" $
  doc "Conversion of codepoint offsets to (line, column) positions, with UTF-16 column accounting (#815)" $
  Testing.testGroup (string "source positions") nothing (list ([] :: [TypedTerm TestGroup])) (list [
    positionTest "start of text" "abc" 0 "0:0",
    positionTest "within first line" "abc" 2 "0:2",
    positionTest "end of text" "abc" 3 "0:3",
    positionTest "after LF" "ab\ncd" 3 "1:0",
    positionTest "within second line" "ab\ncd" 4 "1:1",
    positionTest "at the LF itself" "ab\ncd" 2 "0:2",
    positionTest "after CR LF counts one line break" "ab\r\ncd" 4 "1:0",
    positionTest "between CR and LF" "ab\r\ncd" 3 "0:3",
    positionTest "lone CR is a line break" "ab\rcd" 3 "1:0",
    positionTest "blank lines" "a\n\n\nb" 4 "3:0",
    positionTest "BMP non-ASCII is one UTF-16 unit" "\233a" 2 "0:2",
    positionTest "astral codepoint is two UTF-16 units" "\128512a" 2 "0:3",
    positionTest "astral codepoint then next line" "\128512\na" 2 "1:0",
    positionTest "offset beyond end is clamped" "ab" 10 "0:2",
    positionTest "empty text" "" 0 "0:0"])
