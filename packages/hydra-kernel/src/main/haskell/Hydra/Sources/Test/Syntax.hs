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
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms         as Terms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types         as T
import qualified Hydra.Core.Dsl.Typing       as Typing
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M
import qualified Data.Set                     as S

import qualified Hydra.Core.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Core.Dsl.Lib.Lists    as Lists
import qualified Hydra.Core.Dsl.Lib.Sets     as Sets
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
      Phantoms.toDefinition annotationRoundtripGroup,
      Phantoms.toDefinition corpusRoundtripGroup,
      Phantoms.toDefinition doSugarGroup,
      Phantoms.toDefinition forallMultiBinderGroup,
      Phantoms.toDefinition lambdaMultiBinderGroup,
      Phantoms.toDefinition lazyParameterRoundtripGroup,
      Phantoms.toDefinition moduleAliasGroup,
      Phantoms.toDefinition multiLineDocCommentGroup,
      Phantoms.toDefinition nominalSugarGroup,
      Phantoms.toDefinition sourcePositionGroup,
      Phantoms.toDefinition sourceSpanGroup,
      Phantoms.toDefinition termDocCommentGroup,
      Phantoms.toDefinition unqualifiedNamesGroup]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    doc "Test cases for the textual syntax (#497/#710)" $
    Testing.testGroup (string "textual syntax") nothing (list subgroups) (list ([] :: [TypedTerm TestCaseWithMetadata]))
  where
    subgroups = [annotationRoundtripGroup, corpusRoundtripGroup, doSugarGroup, forallMultiBinderGroup,
                 lambdaMultiBinderGroup, lazyParameterRoundtripGroup, moduleAliasGroup, multiLineDocCommentGroup,
                 nominalSugarGroup, sourceSpanGroup, sourcePositionGroup, termDocCommentGroup, unqualifiedNamesGroup]

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

-- | Fixture: 'forall a. a' -- a single-binder forall, unaffected by the multi-binder collapsing
-- (#711); the printer still emits exactly one binder before '.'.
forallSingleBinderType :: TypedTerm Type
forallSingleBinderType = Core.typeForall $ Core.forallType (nm "a") (T.variable "a")

-- | Fixture: 'forall a. forall b. forall c. a' -- three DIRECTLY nested Type.forall
-- constructors, built by hand (not via the parser) to isolate the printer's collapsing logic
-- from its own desugaring.
forallTripleNestedType :: TypedTerm Type
forallTripleNestedType =
  Core.typeForall $ Core.forallType (nm "a")
    (Core.typeForall $ Core.forallType (nm "b")
      (Core.typeForall $ Core.forallType (nm "c")
        (T.variable "a")))

-- | Fixture: 'forall a. (forall b. b)@{...}' -- the inner forall is nested under an annotation,
-- not directly under the outer forall, so collapsing must stop at the outer binder: the printer
-- emits 'forall a. (forall b. b)@{...}', NOT 'forall a, b. b'. The annotation value is an
-- arbitrary unit-valued term map entry; its exact content doesn't matter, only its presence as
-- an intervening non-forall wrapper.
forallNestedUnderAnnotationType :: TypedTerm Type
forallNestedUnderAnnotationType =
  Core.typeForall $ Core.forallType (nm "a")
    (Core.typeAnnotated $ Core.annotatedType
      (Core.typeForall $ Core.forallType (nm "b") (T.variable "b"))
      (Core.termMap $ Maps.fromList $ Phantoms.list [
        Phantoms.pair (Core.termVariable $ nm "tag") (Core.termLiteral $ Core.literalString $ Phantoms.string "x")]))

-- | Multi-binder 'forall' collapsing (#711): a directly-nested Type.forall chain prints as one
-- comma-separated binder list, consistent with typeScheme's existing '∀x,y,z.' notation.
forallMultiBinderGroup :: TypedTermDefinition TestGroup
forallMultiBinderGroup = define "forallMultiBinderGroup" $
  doc "Print/parse/round-trip cases for the multi-binder 'forall a, b.' syntax (#711)" $
  Testing.testGroup (string "forall multi-binder syntax") nothing (list ([] :: [TypedTerm TestGroup])) (list [
    typePrintTest "single binder: prints as 'forall a.' (unaffected by collapsing)"
      forallSingleBinderType
      "forall a. a",

    typeRoundtripTest "single binder: round-trips" forallSingleBinderType,

    typePrintTest "three directly-nested foralls collapse into one 'forall a, b, c.'"
      forallTripleNestedType
      "forall a, b, c. a",

    typeRoundtripTest "three directly-nested foralls round-trip" forallTripleNestedType,

    typePrintTest "forall nested under an annotation does NOT collapse across the annotation"
      forallNestedUnderAnnotationType
      "forall a. (forall b. b)@{tag=\"x\"}",

    typeRoundtripTest "forall nested under an annotation round-trips" forallNestedUnderAnnotationType])

-- | Build a bare (no signature, no pre-existing metadata) TermDefinition for the given body
-- term, for exercising 'PrintPackaging.termDefinition's description-hoisting directly (#711
-- second readability pass) without needing a whole module fixture.
termDefOf :: String -> TypedTerm Term -> TypedTerm TermDefinition
termDefOf localName body = Packaging.termDefinition (nm localName) nothing nothing body

-- | Print a bare TermDefinition (no module context, empty alias map) and compare against an
-- exact expected string.
termDefinitionPrintTest :: String -> TypedTerm TermDefinition -> String -> TypedTerm TestCaseWithMetadata
termDefinitionPrintTest testName td expectedStr = universalCase testName
  (PrintPackaging.termDefinition @@ (Maps.empty :: TypedTerm (M.Map ModuleName String)) @@ td)
  (Phantoms.string expectedStr)

-- | A single-key description annotation: '<body>@{description="<d>"}'.
withDescriptionAnn :: String -> TypedTerm Term -> TypedTerm Term
withDescriptionAnn d body = Core.termAnnotated $ Core.annotatedTerm body
  (Core.termMap $ Maps.fromList $ Phantoms.list [
    Phantoms.pair (Core.termVariable $ nm "description") (strT d)])

-- | A two-key annotation: description plus one other key, to check that only 'description' is
-- hoisted and the other key stays in a trailing '@{...}' (#711).
withDescriptionAndOtherAnn :: String -> String -> TypedTerm Term -> TypedTerm Term
withDescriptionAndOtherAnn d otherVal body = Core.termAnnotated $ Core.annotatedTerm body
  (Core.termMap $ Maps.fromList $ Phantoms.list [
    Phantoms.pair (Core.termVariable $ nm "description") (strT d),
    Phantoms.pair (Core.termVariable $ nm "availableSince") (strT otherVal)])

-- | Term-definition description hoisting (#711, second readability pass): a term definition's
-- trailing '@{description=...}' annotation prints as a leading '## ...' comment instead, the
-- same treatment type definitions already receive. Any OTHER annotation keys alongside
-- 'description' stay in a trailing '@{...}' suffix; only 'description' is hoisted. An inner
-- (non-outermost) description annotation is left untouched.
termDocCommentGroup :: TypedTermDefinition TestGroup
termDocCommentGroup = define "termDocCommentGroup" $
  doc "Print/parse/round-trip cases for term-definition '##' doc comments (#711)" $
  Testing.testGroup (string "term doc comments") nothing (list ([] :: [TypedTerm TestGroup])) (list [
    termDefinitionPrintTest "single-line description hoists to a leading '##'"
      (termDefOf "capitalize" (withDescriptionAnn "Capitalize the first letter of a string" (varT "mapFirstLetter")))
      "## Capitalize the first letter of a string\nterm capitalize = mapFirstLetter",

    termDefinitionPrintTest "no description: no leading '##', body prints unchanged"
      (termDefOf "identity" (varT "x"))
      "term identity = x",

    termDefinitionPrintTest "description plus another key: description hoists, other key stays trailing"
      (termDefOf "charAt" (withDescriptionAndOtherAnn "Get a character by index" "0.17.6" (varT "impl")))
      "## Get a character by index\nterm charAt = impl@{availableSince=\"0.17.6\"}",

    termDefinitionPrintTest "inner (non-outermost) description is NOT hoisted"
      (termDefOf "wrapped" (Core.termApplication $ Core.application (varT "f") (withDescriptionAnn "inner desc" (varT "x"))))
      "term wrapped = f x@{description=\"inner desc\"}"])

-- | Build a bare (no pre-existing metadata) TypeDefinition for the given type scheme body, for
-- exercising 'PrintPackaging.typeDefinition's description-hoisting directly (#711).
typeDefOf :: String -> TypedTerm Type -> TypedTerm TypeDefinition
typeDefOf localName body = Packaging.typeDefinition (nm localName) nothing (monoScheme body)

-- | Print a bare TypeDefinition (no module context, empty alias map) and compare against an
-- exact expected string.
typeDefinitionPrintTest :: String -> TypedTerm TypeDefinition -> String -> TypedTerm TestCaseWithMetadata
typeDefinitionPrintTest testName td expectedStr = universalCase testName
  (PrintPackaging.typeDefinition @@ (Maps.empty :: TypedTerm (M.Map ModuleName String)) @@ td)
  (Phantoms.string expectedStr)

-- | A single-key description annotation on a TYPE: '<body>@{description="<d>"}' (the type-level
-- analog of 'withDescriptionAnn').
withTypeDescriptionAnn :: String -> TypedTerm Type -> TypedTerm Type
withTypeDescriptionAnn d body = Core.typeAnnotated $ Core.annotatedType body
  (Core.termMap $ Maps.fromList $ Phantoms.list [
    Phantoms.pair (Core.termVariable $ nm "description") (strT d)])

-- | Build a ModuleName from a String literal.
nsLit :: String -> TypedTerm ModuleName
nsLit s = Packaging.moduleName2 $ Phantoms.string s

-- | Build a minimal single-definition module with the given module-level description, for
-- exercising 'PrintPackaging.module''s multi-line '##' doc-comment printing directly (#711).
moduleWithDescription :: String -> String -> TypedTerm Module
moduleWithDescription nsStr d = Packaging.module_
  (nsLit nsStr)
  (Phantoms.just (Packaging.entityMetadata
    (Phantoms.just $ Phantoms.string d)
    (Phantoms.list ([] :: [TypedTerm String])) (Phantoms.list ([] :: [TypedTerm EntityReference])) Phantoms.nothing
    (Phantoms.list ([] :: [TypedTerm Provision]))))
  (Phantoms.list ([] :: [TypedTerm ModuleDependency]))
  (Phantoms.list [Packaging.definitionTerm (termDefOf "x" (varT "y"))])

-- | Build a minimal, no-module-metadata module whose sole term definition carries the given
-- multi-line description (hoisted to leading '##' lines by 'termDefinition'), for round-tripping
-- the whole module (#711).
moduleWithTermDescription :: String -> String -> TypedTerm Module
moduleWithTermDescription nsStr d = Packaging.module_
  (nsLit nsStr) Phantoms.nothing
  (Phantoms.list ([] :: [TypedTerm ModuleDependency]))
  (Phantoms.list [Packaging.definitionTerm (termDefOf "impl" (withDescriptionAnn d (varT "y")))])

-- | Build a minimal, no-module-metadata module whose sole type definition carries the given
-- multi-line description (hoisted to leading '##' lines by 'typeDefinition'), for round-tripping
-- the whole module (#711).
moduleWithTypeDescription :: String -> String -> TypedTerm Module
moduleWithTypeDescription nsStr d = Packaging.module_
  (nsLit nsStr) Phantoms.nothing
  (Phantoms.list ([] :: [TypedTerm ModuleDependency]))
  (Phantoms.list [Packaging.definitionType (typeDefOf "Config" (withTypeDescriptionAnn d T.string))])

-- | Print a module and compare against an exact expected string.
modulePrintTest :: String -> TypedTerm Module -> String -> TypedTerm TestCaseWithMetadata
modulePrintTest testName m expectedStr = universalCase testName
  (PrintPackaging.module' @@ m)
  (Phantoms.string expectedStr)

-- | print -> parse -> re-print a module, comparing the RE-PRINTED text against the ORIGINAL
-- printed text (a fixpoint test, like 'typeRoundtripTest' but at the whole-module level and
-- comparing full text rather than a fingerprint -- unlike 'roundtripFingerprint', which
-- deliberately excludes metadata/doc-comment text, this test is specifically about the '##'
-- comment TEXT surviving the round-trip, so the fingerprint approach would not catch a
-- regression here).
moduleRoundtripTest :: String -> TypedTerm Module -> TypedTerm TestCaseWithMetadata
moduleRoundtripTest testName m = universalCase testName actual expected
  where
    printed = PrintPackaging.module' @@ m
    actual = Eithers.either
      (Phantoms.lambda "e" $ Phantoms.var "e")
      (Phantoms.lambda "m2" $ PrintPackaging.module' @@ Phantoms.var "m2")
      (ParsePackaging.parseModule @@ printed)
    expected = printed

-- | Multi-line doc-comment descriptions (#711): a DSL-authored 'doc' string containing embedded
-- '\n's (e.g. a multi-paragraph or bulleted-list description, as in
-- hydra.core.unification's unifyTypeConstraints) must print as one '##'-prefixed line PER source
-- line, not as a single '## ' line with a raw embedded newline -- the latter produces a bare,
-- non-'##' continuation line that the parser's 'docCommentLines' cannot recognize as part of the
-- comment block, breaking the round-trip. Covers all three places a description can appear: a
-- term definition's hoisted '##' (termDefinition), a type definition's hoisted '##'
-- (typeDefinition), and a module's own leading '##' block (entityMetadataDoc). Each case prints
-- as a bare definition/module (an exact-string check against the expected '##' lines) and also
-- round-trips the WHOLE containing module (print -> parse -> re-print, comparing full text), so
-- a regression that only breaks the parser (not the printer) is caught too.
multiLineDocCommentGroup :: TypedTermDefinition TestGroup
multiLineDocCommentGroup = define "multiLineDocCommentGroup" $
  doc "Print/parse/round-trip cases for multi-line '##' doc-comment descriptions (#711)" $
  Testing.testGroup (string "multi-line doc comments") nothing (list ([] :: [TypedTerm TestGroup])) (list [
    termDefinitionPrintTest "multi-line term description hoists to one '##' line per source line"
      (termDefOf "unifyTypeConstraints" (withDescriptionAnn "Robinson's algorithm.\nSee the reference." (varT "impl")))
      "## Robinson's algorithm.\n## See the reference.\nterm unifyTypeConstraints = impl",

    moduleRoundtripTest "multi-line term description round-trips (whole module)"
      (moduleWithTermDescription "test.multiline.term" "Robinson's algorithm.\nSee the reference."),

    typeDefinitionPrintTest "multi-line type description hoists to one '##' line per source line"
      (typeDefOf "Config" (withTypeDescriptionAnn "Configuration options.\nSee the manual." T.string))
      "## Configuration options.\n## See the manual.\ntype Config = string",

    moduleRoundtripTest "multi-line type description round-trips (whole module)"
      (moduleWithTypeDescription "test.multiline.type" "Configuration options.\nSee the manual."),

    modulePrintTest "multi-line module description hoists to one '##' line per source line"
      (moduleWithDescription "test.multiline" "Utilities for X.\nSee the reference.")
      "## Utilities for X.\n## See the reference.\nmodule test.multiline\n\nterm x = y",

    moduleRoundtripTest "multi-line module description round-trips (whole module)"
      (moduleWithDescription "test.multiline.module" "Utilities for X.\nSee the reference.")])

-- | Fixture: 'λa.λb.λc.a' -- three directly-nested Term.lambda constructors, no domain types.
lambdaTripleNestedTerm :: TypedTerm Term
lambdaTripleNestedTerm = lamT "a" (lamT "b" (lamT "c" (varT "a")))

-- | Fixture: 'λa:int32.λb:string.a' -- two directly-nested lambdas, each with its own domain
-- type, to check the printer emits 'λa:int32,b:string.' (not dropping either type).
lambdaTypedNestedTerm :: TypedTerm Term
lambdaTypedNestedTerm = Core.termLambda (Core.lambda (nm "a") (just T.int32)
  (Core.termLambda (Core.lambda (nm "b") (just T.string) (varT "a"))))

-- | Fixture: 'λa.(λb.b)@{...}' -- the inner lambda is nested under an annotation, not directly
-- under the outer lambda, so collapsing must stop at the outer binder.
lambdaNestedUnderAnnotationTerm :: TypedTerm Term
lambdaNestedUnderAnnotationTerm = lamT "a" (withDescriptionAnn "inner" (lamT "b" (varT "b")))

-- | Fixture: 'Λa.Λb.Λc.a' -- three directly-nested Term.typeLambda constructors.
typeLambdaTripleNestedTerm :: TypedTerm Term
typeLambdaTripleNestedTerm = Core.termTypeLambda $ Core.typeLambda (nm "a")
  (Core.termTypeLambda $ Core.typeLambda (nm "b")
    (Core.termTypeLambda $ Core.typeLambda (nm "c") (varT "a")))

-- | Multi-binder λ/Λ collapsing (#711, third readability pass): a directly-nested lambda or
-- type-lambda chain prints as one comma-separated binder list, consistent with forall/∀.
lambdaMultiBinderGroup :: TypedTermDefinition TestGroup
lambdaMultiBinderGroup = define "lambdaMultiBinderGroup" $
  doc "Print/parse/round-trip cases for multi-binder 'λa,b.'/'Λa,b.' syntax (#711)" $
  Testing.testGroup (string "lambda/type-lambda multi-binder syntax") nothing (list ([] :: [TypedTerm TestGroup])) (list [
    termPrintTest "three directly-nested λ's collapse into one 'λa,b,c.'"
      lambdaTripleNestedTerm
      "λa,b,c.a",

    termRoundtripTest "three directly-nested λ's round-trip" lambdaTripleNestedTerm,

    termPrintTest "typed binders collapse with each binder keeping its own domain type"
      lambdaTypedNestedTerm
      "λa:int32,b:string.a",

    termRoundtripTest "typed binders round-trip" lambdaTypedNestedTerm,

    termPrintTest "λ nested under an annotation does NOT collapse across the annotation"
      lambdaNestedUnderAnnotationTerm
      "λa.(λb.b)@{description=\"inner\"}",

    termRoundtripTest "λ nested under an annotation round-trips" lambdaNestedUnderAnnotationTerm,

    termPrintTest "three directly-nested Λ's collapse into one 'Λa,b,c.'"
      typeLambdaTripleNestedTerm
      "Λa,b,c.a",

    termRoundtripTest "three directly-nested Λ's round-trip" typeLambdaTripleNestedTerm])

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

-- | A universe entry: (module name, its full set of declared names). The universe map's values
-- are FULLY QUALIFIED 'Name's ('module.local', matching 'Names.qname's own construction and
-- 'Print.Packaging.moduleAliasesWith'/'Parse.Packaging.resolveNameRefWith's membership checks
-- against it), built here from the module name plus a list of local-name strings for test
-- brevity.
universeEntry :: String -> [String] -> (TypedTerm ModuleName, TypedTerm (S.Set Name))
universeEntry modName localNames = (Packaging.moduleName2 $ Phantoms.string modName,
  Sets.fromList (Phantoms.list [Core.name (Phantoms.string (modName L.++ "." L.++ n)) | n <- localNames]))

-- | Print a module given an explicit universe map (#5/#711).
moduleWithUniversePrintTest :: String -> TypedTerm Module -> [(TypedTerm ModuleName, TypedTerm (S.Set Name))] -> String -> TypedTerm TestCaseWithMetadata
moduleWithUniversePrintTest testName m universeEntries expectedStr = universalCase testName
  (PrintPackaging.moduleDefinitionWith @@ (Maps.fromList $ Phantoms.list [Phantoms.pair k v | (k, v) <- universeEntries]) @@ m)
  (Phantoms.string expectedStr)

-- | print(universe) -> parse(SAME universe) -> fingerprint round-trip test (#5/#711): unlike
-- 'roundtripFingerprint' (which always uses the context-free 'module''/'parseModule'), this drives
-- both sides through the SAME explicit universe map, exactly mirroring how a real caller (the
-- build/sync, loading all modules together) would use 'moduleDefinitionWith'/'parseModuleWith' in
-- tandem.
-- | 'parseModuleWith' is self-contained given 'universe': it scopes its own resolution to the
-- printed text's own recovered import lines (every real dependency, aliased or bare-safe, always
-- has one -- staging's 2026-11-01 ruling), using 'universe' only to look up each import's
-- declared names. So this test helper passes the SAME 'universeMap' to both the printer and the
-- parser -- no narrowing needed, matching how a real caller (which only has the universe, not
-- the original in-memory 'Module') would use it.
moduleWithUniverseRoundtripTest :: String -> TypedTerm Module -> [(TypedTerm ModuleName, TypedTerm (S.Set Name))] -> TypedTerm TestCaseWithMetadata
moduleWithUniverseRoundtripTest testName m universeEntries = universalCase testName actual expected
  where
    universeMap = Maps.fromList $ Phantoms.list [Phantoms.pair k v | (k, v) <- universeEntries]
    printed = PrintPackaging.moduleDefinitionWith @@ universeMap @@ m
    actual = Eithers.either
      (Phantoms.lambda "e" $ Phantoms.var "e")
      (Phantoms.lambda "mod2" $ moduleFingerprint (Phantoms.var "mod2"))
      (ParsePackaging.parseModuleWith @@ universeMap @@ printed)
    expected = moduleFingerprint m

-- | Staging's counterexample (2026-11-11): module A imports 'logic' (referencing only
-- 'logic.ifElse') and 'foo' (an UNRELATED import, never referenced at all, but which happens to
-- ALSO declare a local name 'ifElse'). Under a naive "is the REFERENCED name unique" rule the
-- printer would emit bare 'ifElse' (no collision among what A actually references) -- but the
-- parser, resolving against the imports' FULL declared sets as it must, would then find TWO
-- modules declaring 'ifElse' and hard-error as ambiguous, breaking 'module -> print -> parse'
-- identity. The fix (per staging/Josh, 'option 2' + the universe-symmetric printer) requires the
-- FULL declared set of 'foo' to be known and checked, not just what A references -- so 'ifElse'
-- must print qualified ('logic.ifElse'), even though A itself never touches 'foo.ifElse'.
ambiguousOwnerCounterexampleModule :: TypedTerm Module
ambiguousOwnerCounterexampleModule = testModule "hydra.example.ambiguous"
  ["hydra.logic", "hydra.foo"]
  [("UsesIfElse", monoScheme (T.variable "hydra.logic.ifElse"))]

ambiguousOwnerCounterexampleUniverse :: [(TypedTerm ModuleName, TypedTerm (S.Set Name))]
ambiguousOwnerCounterexampleUniverse = [
  universeEntry "hydra.logic" ["ifElse", "and", "or"],
  universeEntry "hydra.foo" ["ifElse", "bar"]]

ambiguousOwnerCounterexamplePrintTest :: TypedTerm TestCaseWithMetadata
ambiguousOwnerCounterexamplePrintTest = moduleWithUniversePrintTest
  "staging's counterexample: 'ifElse' stays qualified because an UNREFERENCED import also declares it"
  ambiguousOwnerCounterexampleModule
  ambiguousOwnerCounterexampleUniverse
  (L.intercalate "\n" [
    "module hydra.example.ambiguous",
    "",
    "import hydra.logic as logic",
    "",
    "type UsesIfElse = logic.ifElse"])

ambiguousOwnerCounterexampleRoundtripTest :: TypedTerm TestCaseWithMetadata
ambiguousOwnerCounterexampleRoundtripTest = moduleWithUniverseRoundtripTest
  "staging's counterexample round-trips (module->print->parse with the same universe)"
  ambiguousOwnerCounterexampleModule
  ambiguousOwnerCounterexampleUniverse

-- | A dependency module whose local name collides with one of the PRINTED module's own
-- definitions: 'hydra.lib' declares 'Widget' (among others), and the printed module ALSO defines
-- its own 'Widget'. Per 'parseModuleWith's resolution order (local binder, THEN own definitions,
-- THEN imports), a bare 'Widget' reference would resolve to the module's OWN 'Widget' on reparse
-- -- not 'hydra.lib.Widget' -- so the import's reference to 'Widget' must print qualified, even
-- though 'Widget' is otherwise unique among the imports in scope.
ownNameCollisionModule :: TypedTerm Module
ownNameCollisionModule = testModule "hydra.example.ownclash"
  ["hydra.lib"]
  [("Widget", monoScheme T.string),
   ("UsesLibWidget", monoScheme (T.variable "hydra.lib.Widget"))]

ownNameCollisionUniverse :: [(TypedTerm ModuleName, TypedTerm (S.Set Name))]
ownNameCollisionUniverse = [universeEntry "hydra.lib" ["Widget", "Thing"]]

ownNameCollisionPrintTest :: TypedTerm TestCaseWithMetadata
ownNameCollisionPrintTest = moduleWithUniversePrintTest
  "an import's local name colliding with the module's OWN definition forces qualification"
  ownNameCollisionModule
  ownNameCollisionUniverse
  (L.intercalate "\n" [
    "module hydra.example.ownclash",
    "",
    "import hydra.lib as lib",
    "",
    "type Widget = string",
    "",
    "type UsesLibWidget = lib.Widget"])

ownNameCollisionRoundtripTest :: TypedTerm TestCaseWithMetadata
ownNameCollisionRoundtripTest = moduleWithUniverseRoundtripTest
  "own-definition/import local-name collision round-trips, resolving the bare name to the module's own definition"
  ownNameCollisionModule
  ownNameCollisionUniverse

-- | Staging's scope refinement (2026-11-11): the collision check that gates bareness must be
-- scoped to the printed module's own definitions plus its ACTUAL imports, never every module the
-- universe happens to contain -- 'parseModuleWith' only ever resolves a bare name against the
-- module's own definitions and its declared imports, so an UNIMPORTED module sharing a common
-- local name poses no real ambiguity risk. 'hydra.unrelated' here also declares 'ifElse' (same
-- local name as 'hydra.logic.ifElse', the one actually referenced), but since
-- 'hydra.example.unimported' never imports 'hydra.unrelated' at all, that collision must NOT force
-- qualification -- 'ifElse' should print bare.
unimportedCollisionModule :: TypedTerm Module
unimportedCollisionModule = testModule "hydra.example.unimported"
  ["hydra.logic"]
  [("UsesIfElse", monoScheme (T.variable "hydra.logic.ifElse"))]

unimportedCollisionUniverse :: [(TypedTerm ModuleName, TypedTerm (S.Set Name))]
unimportedCollisionUniverse = [
  universeEntry "hydra.logic" ["ifElse", "and", "or"],
  universeEntry "hydra.unrelated" ["ifElse", "somethingElse"]]

unimportedCollisionPrintTest :: TypedTerm TestCaseWithMetadata
unimportedCollisionPrintTest = moduleWithUniversePrintTest
  "a same-local-name module that is NOT imported does not block bareness (scoped collision check)"
  unimportedCollisionModule
  unimportedCollisionUniverse
  (L.intercalate "\n" [
    "module hydra.example.unimported",
    "",
    "import hydra.logic",
    "",
    "type UsesIfElse = ifElse"])

unimportedCollisionRoundtripTest :: TypedTerm TestCaseWithMetadata
unimportedCollisionRoundtripTest = moduleWithUniverseRoundtripTest
  "scoped-collision bare module round-trips (module->print->parse with the same universe)"
  unimportedCollisionModule
  unimportedCollisionUniverse

unqualifiedNamesGroup :: TypedTermDefinition TestGroup
unqualifiedNamesGroup = define "unqualifiedNamesGroup" $
  doc "Unqualified (bare) cross-module name printing when provably safe, and the universe-aware parser resolution that makes it round-trip (#5/#711)" $
  Testing.testGroup (string "unqualified names") nothing (list ([] :: [TypedTerm TestGroup])) (list [
    ambiguousOwnerCounterexamplePrintTest,
    ambiguousOwnerCounterexampleRoundtripTest,
    ownNameCollisionPrintTest,
    ownNameCollisionRoundtripTest,
    unimportedCollisionPrintTest,
    unimportedCollisionRoundtripTest])

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

-- | Print a type and compare against an exact expected string -- pins the canonical sugar form
-- (mirrors 'termPrintTest'). Used for the multi-binder 'forall a, b.' collapsing (#711): a plain
-- round-trip test alone would not catch a regression that collapses too eagerly or not at all,
-- since the self-comparing round-trip only checks print-parse-print stability, not which
-- surface form the printer actually chose.
typePrintTest :: String -> TypedTerm Type -> String -> TypedTerm TestCaseWithMetadata
typePrintTest testName t expectedStr = universalCase testName
  (PrintCore.type_ @@ t)
  (Phantoms.string expectedStr)

-- | Print a type, parse the printed text back, and re-print -- compares to the original printed
-- form (self-comparing string round-trip, mirroring 'termRoundtripTest'/'signatureRoundtripTest').
typeRoundtripTest :: String -> TypedTerm Type -> TypedTerm TestCaseWithMetadata
typeRoundtripTest testName t = universalCase testName actual expected
  where
    printed = PrintCore.type_ @@ t
    actual = Eithers.either
      (Phantoms.lambda "e" $ Phantoms.var "e")
      (Phantoms.lambda "t2" $ PrintCore.type_ @@ Phantoms.var "t2")
      (ParseCore.parseType @@ printed)
    expected = printed

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

----------------------------------------------------------------------------------------------
-- #497 close-out: a general print->parse->print sweep over the existing kernel test-fixture
-- corpus (Hydra.Sources.Test.TestTerms/TestTypes), plus explicit annotation round-trip cases
-- tied to syntax.md #1's named provisions. Scoped term-level only, per the issue's own narrowing
-- (module/packaging-level syntax is separate, future work). Uses the same self-comparing
-- print->parse->print string shape as termRoundtripTest/typeRoundtripTest/signatureRoundtripTest
-- throughout this file, not raw structural equality (universalCase ultimately compares
-- String-typed terms; see those helpers' own docs).

corpusRoundtripGroup :: TypedTermDefinition TestGroup
corpusRoundtripGroup = define "corpusRoundtripGroup" $
  doc "General print->parse->print round-trip sweep over the kernel test-fixture corpus (#497)" $
  Testing.testGroup (string "fixture corpus round-trips") nothing (list ([] :: [TypedTerm TestGroup])) (list [
    -- Terms (Hydra.Sources.Test.TestTerms): a nominal record fixture.
    termRoundtripTest "corpus: testDataArthur (nominal record)" (Phantoms.asTerm TestTerms.testDataArthur),

    -- Types (Hydra.Sources.Test.TestTypes): one representative of each major shape in the
    -- corpus -- record, monomorphic/polymorphic/recursive union, wrap, multi-variable forall,
    -- and the built-in container types (optional/list/set/map/either/function).
    typeRoundtripTest "corpus: testTypePerson (record)" (Phantoms.asTerm TestTypes.testTypePerson),
    typeRoundtripTest "corpus: testTypeUnionMonomorphic (monomorphic union)" (Phantoms.asTerm TestTypes.testTypeUnionMonomorphic),
    typeRoundtripTest "corpus: testTypeLatLonPoly (polymorphic record, single forall)" (Phantoms.asTerm TestTypes.testTypeLatLonPoly),
    typeRoundtripTest "corpus: testTypeTriple (triple forall)" (Phantoms.asTerm TestTypes.testTypeTriple),
    typeRoundtripTest "corpus: testTypeList (self-recursive polymorphic record)" (Phantoms.asTerm TestTypes.testTypeList),
    typeRoundtripTest "corpus: testTypeUnionPolymorphicRecursive (self-recursive polymorphic union)"
      (Phantoms.asTerm TestTypes.testTypeUnionPolymorphicRecursive),
    typeRoundtripTest "corpus: testTypeUnionPolymorphicRecursiveListWrapped (list-wrapped recursive union)"
      (Phantoms.asTerm TestTypes.testTypeUnionPolymorphicRecursiveListWrapped),
    typeRoundtripTest "corpus: testTypeStringAlias (wrap)" (Phantoms.asTerm TestTypes.testTypeStringAlias),
    typeRoundtripTest "corpus: testTypePolymorphicWrapper (forall + wrap + list)" (Phantoms.asTerm TestTypes.testTypePolymorphicWrapper),
    typeRoundtripTest "corpus: testTypeSymmetricTriple (two-variable forall + wrap + applied type)"
      (Phantoms.asTerm TestTypes.testTypeSymmetricTriple),
    typeRoundtripTest "corpus: optionalStringType" (Phantoms.asTerm TestTypes.optionalStringType),
    typeRoundtripTest "corpus: listOfSetOfStringsType (nested containers)" (Phantoms.asTerm TestTypes.listOfSetOfStringsType),
    typeRoundtripTest "corpus: mapOfStringsToIntsType" (Phantoms.asTerm TestTypes.mapOfStringsToIntsType),
    typeRoundtripTest "corpus: eitherStringOrInt8Type" (Phantoms.asTerm TestTypes.eitherStringOrInt8Type),
    typeRoundtripTest "corpus: concatType (curried function type)" (Phantoms.asTerm TestTypes.concatType),
    typeRoundtripTest "corpus: testTypeTimestamp (union with a uint64 variant)" (Phantoms.asTerm TestTypes.testTypeTimestamp)])

-- | An annotation payload matching the spec's own canonical example (syntax.md #3's annot note:
-- "common case: t @{description = \"...\"}"): a one-entry Name->Term map.
descriptionAnnotation :: String -> TypedTerm Term
descriptionAnnotation text = Terms.annotationMapToTerm $ Maps.fromList $ Phantoms.list [
  Phantoms.pair (nm "description") (strT text)]

annotationRoundtripGroup :: TypedTermDefinition TestGroup
annotationRoundtripGroup = define "annotationRoundtripGroup" $
  doc "Annotation round-trip cases tied to syntax.md #1's HYDRA-SYN-ROUND-TRIP / HYDRA-SYN-PRINTER-FAITHFUL provisions (#497)" $
  Testing.testGroup (string "annotation round-trips") nothing (list ([] :: [TypedTerm TestGroup])) (list [
    -- [HYDRA-SYN-PRINTER-FAITHFUL]: a literal, variable, and application each carrying a single
    -- annotation -- the printer must emit the annotation and the parser must recover it, not drop
    -- it. Round-trip only (self-comparing print->parse->print), not an exact-string pin -- see
    -- termRoundtripTest's own doc for why that is the established, safe comparison shape here.
    termRoundtripTest "annotated literal round-trips" (Terms.annotatedTerm (i32T 42) (descriptionAnnotation "the answer")),

    termRoundtripTest "annotated variable round-trips" (Terms.annotatedTerm (varT "x") (descriptionAnnotation "a variable")),

    termRoundtripTest "annotated application round-trips"
      (Terms.annotatedTerm (appT (varT "f") (varT "x")) (descriptionAnnotation "a call")),

    -- [HYDRA-SYN-ROUND-TRIP] "stacked annotations are structure: they print as nested and parse
    -- back as nested" -- x @m1 @m2 == annotated(annotated(x, m1), m2), chains left.
    termRoundtripTest "stacked annotations round-trip (chains left)"
      (Terms.annotatedTerm (Terms.annotatedTerm (varT "x") (descriptionAnnotation "inner")) (descriptionAnnotation "outer")),

    -- An annotation payload that is itself compound (not a bare literal/variable) must print
    -- parenthesized per the postfix-annotation precedence rule, and still round-trip.
    termRoundtripTest "annotation with a compound (non-atomic) payload round-trips"
      (Terms.annotatedTerm (varT "x") (appT (varT "f") (varT "y"))),

    -- The type-level mirror of the same contract: annotated types must round-trip too.
    typeRoundtripTest "annotated type round-trips"
      (T.annot (Maps.fromList $ Phantoms.list [Phantoms.pair (nm "description") (strT "a type")]) T.string),

    typeRoundtripTest "stacked type annotations round-trip (chains left)"
      (T.annot (Maps.fromList $ Phantoms.list [Phantoms.pair (nm "outer") (strT "o")])
        (T.annot (Maps.fromList $ Phantoms.list [Phantoms.pair (nm "inner") (strT "i")]) T.string))])
