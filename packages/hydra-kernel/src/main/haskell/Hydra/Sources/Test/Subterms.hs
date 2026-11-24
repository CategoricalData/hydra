{-# LANGUAGE FlexibleContexts #-}

-- | Test cases for hydra.core.print.paths (step/path printer round-trip), hydra.core.subterms (the term-graph view),
--   and hydra.core.validate.paths (the TermGraph:TypeGraph path-erasure relation).
module Hydra.Sources.Test.Subterms where

-- Standard imports for tests
import Hydra.Kernel
import           Hydra.Core.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                 as Testing
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms                   as Terms hiding ((@@))
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core          as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Phantoms      as Phantoms
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms                ((@@))
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types         as T
import qualified Hydra.Core.Dsl.Paths                               as Paths
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Hydra.Core.Testing

import qualified Hydra.Sources.Kernel.Terms.Print.Core as PrintCore
import qualified Hydra.Sources.Kernel.Terms.Print.Errors as PrintErrors
import qualified Hydra.Sources.Kernel.Terms.Print.Paths as PrintPaths
import qualified Hydra.Sources.Kernel.Terms.Subterms as SubtermsModule
import qualified Hydra.Sources.Kernel.Terms.Validate.Paths as ValidatePaths
import qualified Hydra.Core.Dsl.Lib.Eithers as Eithers
import qualified Hydra.Core.Dsl.Lib.Maps as Maps
import qualified Hydra.Core.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Core.Dsl.Lib.Sets as Sets
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph as Graph


ns :: ModuleName
ns = ModuleName "hydra.core.test.subterms"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([PrintCore.ns, PrintErrors.ns, PrintPaths.ns, SubtermsModule.ns, ValidatePaths.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata (Just "Test cases for path serialization round-trips, term-graph paths, and path erasure")}
  where
    definitions = [Phantoms.toDefinition allTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for path serialization, term-graph paths, and path erasure" $
    supergroup "subterms" [
      subtermStepRoundTripGroup,
      subtypeStepRoundTripGroup,
      erasureGroup,
      elaboratedLeafGroup,
      elaboratedGraphGroup]

-- | Round-trip a printed subterm step: print, parse, print again; the two printings must agree
--   (parse . print = id, observed through the printer).
subtermRoundTrip :: String -> TypedTerm SubtermStep -> TypedTerm TestCaseWithMetadata
subtermRoundTrip cname step = universalCase cname
  (Optionals.withDefault (Phantoms.string "<<parse failed>>")
    (Optionals.map (Phantoms.lambda "s" (PrintPaths.subtermStep @@ Phantoms.var "s"))
      (PrintPaths.parseSubtermStep @@ (PrintPaths.subtermStep @@ step))))
  (PrintPaths.subtermStep @@ step)

subtypeRoundTrip :: String -> TypedTerm SubtypeStep -> TypedTerm TestCaseWithMetadata
subtypeRoundTrip cname step = universalCase cname
  (Optionals.withDefault (Phantoms.string "<<parse failed>>")
    (Optionals.map (Phantoms.lambda "s" (PrintPaths.subtypeStep @@ Phantoms.var "s"))
      (PrintPaths.parseSubtypeStep @@ (PrintPaths.subtypeStep @@ step))))
  (PrintPaths.subtypeStep @@ step)

nm :: String -> TypedTerm Name
nm s = Core.name (Phantoms.string s)

-- | Every SubtermStep variant round-trips through print/parse, including payload-carrying steps.
subtermStepRoundTripGroup :: TypedTerm TestGroup
subtermStepRoundTripGroup = subgroup "subtermStep round-trip" [
    subtermRoundTrip "annotatedAnnotation" Paths.subtermStepAnnotatedAnnotation,
    subtermRoundTrip "annotatedBody" Paths.subtermStepAnnotatedBody,
    subtermRoundTrip "applicationArgument" Paths.subtermStepApplicationArgument,
    subtermRoundTrip "applicationFunction" Paths.subtermStepApplicationFunction,
    subtermRoundTrip "casesCase (named)" (Paths.subtermStepCasesCase (nm "foo")),
    subtermRoundTrip "casesDefault" Paths.subtermStepCasesDefault,
    subtermRoundTrip "eitherLeft" Paths.subtermStepEitherLeft,
    subtermRoundTrip "eitherRight" Paths.subtermStepEitherRight,
    subtermRoundTrip "injectField (named)" (Paths.subtermStepInjectField (nm "bar")),
    subtermRoundTrip "lambdaBody" Paths.subtermStepLambdaBody,
    subtermRoundTrip "letBinding (named)" (Paths.subtermStepLetBinding (nm "x")),
    subtermRoundTrip "letBody" Paths.subtermStepLetBody,
    subtermRoundTrip "listElement (indexed)" (Paths.subtermStepListElement (Phantoms.int32 3)),
    subtermRoundTrip "mapEntry (indexed)" (Paths.subtermStepMapEntry (Phantoms.int32 0)),
    subtermRoundTrip "optionalGiven" Paths.subtermStepOptionalGiven,
    subtermRoundTrip "pairFirst" Paths.subtermStepPairFirst,
    subtermRoundTrip "pairSecond" Paths.subtermStepPairSecond,
    subtermRoundTrip "recordField (named)" (Paths.subtermStepRecordField (nm "field")),
    subtermRoundTrip "setElement (indexed)" (Paths.subtermStepSetElement (Phantoms.int32 2)),
    subtermRoundTrip "typeApplicationBody" Paths.subtermStepTypeApplicationBody,
    subtermRoundTrip "typeLambdaBody" Paths.subtermStepTypeLambdaBody,
    subtermRoundTrip "wrapBody" Paths.subtermStepWrapBody]

-- | Every SubtypeStep variant round-trips through print/parse.
subtypeStepRoundTripGroup :: TypedTerm TestGroup
subtypeStepRoundTripGroup = subgroup "subtypeStep round-trip" [
    subtypeRoundTrip "annotatedBody" Paths.subtypeStepAnnotatedBody,
    subtypeRoundTrip "applicationArgument" Paths.subtypeStepApplicationArgument,
    subtypeRoundTrip "applicationFunction" Paths.subtypeStepApplicationFunction,
    subtypeRoundTrip "effectValue" Paths.subtypeStepEffectValue,
    subtypeRoundTrip "eitherLeft" Paths.subtypeStepEitherLeft,
    subtypeRoundTrip "eitherRight" Paths.subtypeStepEitherRight,
    subtypeRoundTrip "forallBody" Paths.subtypeStepForallBody,
    subtypeRoundTrip "functionCodomain" Paths.subtypeStepFunctionCodomain,
    subtypeRoundTrip "functionDomain" Paths.subtypeStepFunctionDomain,
    subtypeRoundTrip "listElement" Paths.subtypeStepListElement,
    subtypeRoundTrip "mapKeys" Paths.subtypeStepMapKeys,
    subtypeRoundTrip "mapValues" Paths.subtypeStepMapValues,
    subtypeRoundTrip "optionalElement" Paths.subtypeStepOptionalElement,
    subtypeRoundTrip "pairFirst" Paths.subtypeStepPairFirst,
    subtypeRoundTrip "pairSecond" Paths.subtypeStepPairSecond,
    subtypeRoundTrip "recordField (named)" (Paths.subtypeStepRecordField (nm "field")),
    subtypeRoundTrip "setElement" Paths.subtypeStepSetElement,
    subtypeRoundTrip "unionField (named)" (Paths.subtypeStepUnionField (nm "variant")),
    subtypeRoundTrip "wrapBody" Paths.subtypeStepWrapBody]

-- | Erase a subterm path and render the resulting subtype path (or a sentinel when it leaves the data
--   fragment), for comparison in erasureCase.
erasePrinted :: TypedTerm SubtermPath -> TypedTerm String
erasePrinted p = Optionals.withDefault (Phantoms.string "<<no type-side position>>")
  (Optionals.map (Phantoms.lambda "sp" (PrintPaths.subtypePath @@ Phantoms.var "sp"))
    (ValidatePaths.eraseSubtermPath @@ p))

-- | Assert that a subterm path erases to the expected printed subtype path.
erasureCase :: String -> [TypedTerm SubtermStep] -> String -> TypedTerm TestCaseWithMetadata
erasureCase cname steps expected = universalCase cname
  (erasePrinted (Paths.subtermPath (Phantoms.list steps)))
  (Phantoms.string expected)

-- | The TermGraph:TypeGraph path-erasure relation on the data fragment, including the two-step
--   mapEntry·pairFirst ↦ mapKeys and mapEntry·pairSecond ↦ mapValues collapses, and the computation
--   fragment yielding no type-side position.
erasureGroup :: TypedTerm TestGroup
erasureGroup = subgroup "path erasure (TermGraph : TypeGraph)" [
    erasureCase "listElement -> listElement"
      [Paths.subtermStepListElement (Phantoms.int32 0)] "listElement",
    erasureCase "setElement -> setElement"
      [Paths.subtermStepSetElement (Phantoms.int32 0)] "setElement",
    erasureCase "optionalGiven -> optionalElement"
      [Paths.subtermStepOptionalGiven] "optionalElement",
    erasureCase "injectField -> unionField (name preserved)"
      [Paths.subtermStepInjectField (nm "left")] "unionField:left",
    erasureCase "recordField -> recordField (name preserved)"
      [Paths.subtermStepRecordField (nm "age")] "recordField:age",
    erasureCase "mapEntry . pairFirst -> mapKeys"
      [Paths.subtermStepMapEntry (Phantoms.int32 2), Paths.subtermStepPairFirst] "mapKeys",
    erasureCase "mapEntry . pairSecond -> mapValues"
      [Paths.subtermStepMapEntry (Phantoms.int32 2), Paths.subtermStepPairSecond] "mapValues",
    erasureCase "nested: listElement . recordField -> listElement . recordField (name preserved)"
      [Paths.subtermStepListElement (Phantoms.int32 0), Paths.subtermStepRecordField (nm "x")]
      "listElement/recordField:x",
    -- computation fragment: no type-side position
    erasureCase "lambdaBody -> (none)"
      [Paths.subtermStepLambdaBody] "<<no type-side position>>",
    erasureCase "applicationFunction -> (none)"
      [Paths.subtermStepApplicationFunction] "<<no type-side position>>",
    -- a dangling mapEntry (not projected by pairFirst/pairSecond) has no type-side position
    erasureCase "mapEntry (unprojected) -> (none)"
      [Paths.subtermStepMapEntry (Phantoms.int32 0)] "<<no type-side position>>"]

-- Elaborated empty leaves (#853). Inference wraps a synthesis-incomplete leaf in type applications:
-- none<t>, []<t>, {}<t> (set), {}<k><v> (map), and every either value as e<l><r>. The leaf's type comes
-- from those arguments, with no expected type and no schema.

float64T :: TypedTerm Type
float64T = Core.typeLiteral $ Core.literalTypeFloat Core.floatTypeFloat64

int32T :: TypedTerm Type
int32T = Core.typeLiteral $ Core.literalTypeInteger Core.integerTypeInt32

stringT :: TypedTerm Type
stringT = Core.typeLiteral Core.literalTypeString

float64Term :: Double -> TypedTerm Term
float64Term x = Core.termLiteral $ Core.literalFloat $ Core.floatValueFloat64 $ Phantoms.float64 x

tyApp :: TypedTerm Term -> TypedTerm Type -> TypedTerm Term
tyApp body t = Core.termTypeApplication $ Core.typeApplicationTerm body t

noneTerm :: TypedTerm Term
noneTerm = Core.termOptional Phantoms.nothing

-- | A graph with the given bindings and the given types (as schemes with no variables) in its bound types.
fixtureGraph :: [(String, TypedTerm Term)] -> [(String, TypedTerm Type)] -> TypedTerm Graph
fixtureGraph terms types = Graph.graph
    (Maps.fromList (Phantoms.list [Phantoms.pair (nm n) t | (n, t) <- terms]))
    (Maps.fromList (Phantoms.list [Phantoms.pair (nm n) (scheme t) | (n, t) <- types]))
    Maps.empty
    Sets.empty
    Maps.empty
    Maps.empty
    Maps.empty
    Sets.empty
  where
    scheme t = Core.typeScheme (Phantoms.list ([] :: [TypedTerm Name])) t Maps.empty

-- | The printed type synthesized for a term with no expected type, or the printed error.
synthesizedPrinted :: TypedTerm Term -> TypedTerm String
synthesizedPrinted t = Eithers.either
  (Phantoms.lambda "e" (PrintErrors.error_ @@ Phantoms.var "e"))
  (Phantoms.lambda "ty" (PrintCore.type_ @@ Phantoms.var "ty"))
  (SubtermsModule.synthesizeType @@ fixtureGraph [] [] @@ Maps.empty @@ Maps.empty @@ nm "x"
    @@ Phantoms.nothing @@ Paths.subtermPath (Phantoms.list ([] :: [TypedTerm SubtermStep])) @@ t)

synthesisCase :: String -> TypedTerm Term -> TypedTerm Type -> TypedTerm TestCaseWithMetadata
synthesisCase cname t expected = universalCase cname (synthesizedPrinted t) (PrintCore.type_ @@ expected)

elaboratedLeafGroup :: TypedTerm TestGroup
elaboratedLeafGroup = subgroup "synthesizeType on elaborated empty leaves" [
    synthesisCase "none<float64>" (tyApp noneTerm float64T) (Core.typeOptional float64T),
    synthesisCase "[]<string>" (tyApp (Core.termList $ Phantoms.list ([] :: [TypedTerm Term])) stringT) (Core.typeList stringT),
    synthesisCase "empty set<int32>" (tyApp (Core.termSet Sets.empty) int32T) (Core.typeSet int32T),
    synthesisCase "empty map<string><int32>" (tyApp (tyApp (Core.termMap Maps.empty) stringT) int32T)
      (Core.typeMap $ Core.mapType stringT int32T),
    synthesisCase "left(1)<int32><string>"
      (tyApp (tyApp (Core.termEither $ Phantoms.left $ int32Term 1) int32T) stringT)
      (Core.typeEither $ Core.eitherType int32T stringT),
    synthesisCase "right(1.5)<string><float64>"
      (tyApp (tyApp (Core.termEither $ Phantoms.right $ float64Term 1.5) stringT) float64T)
      (Core.typeEither $ Core.eitherType stringT float64T),
    -- the arguments instantiate simultaneously: a left argument naming the right forall's parameter is not captured
    synthesisCase "left(1)<r><int32> (no capture)"
      (tyApp (tyApp (Core.termEither $ Phantoms.left $ int32Term 1) (Core.typeVariable $ nm "r")) int32T)
      (Core.typeEither $ Core.eitherType (Core.typeVariable $ nm "r") int32T),
    synthesisCase "bare none: its polymorphic type" noneTerm
      (Core.typeForall $ Core.forallType (nm "t") (Core.typeOptional $ Core.typeVariable $ nm "t")),
    synthesisCase "partial map<string>: forall over the value type" (tyApp (Core.termMap Maps.empty) stringT)
      (Core.typeForall $ Core.forallType (nm "v") (Core.typeMap $ Core.mapType stringT (Core.typeVariable $ nm "v")))]
  where
    int32Term n = Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ Phantoms.int32 n

-- | The schema of #853: a plain record and a parametric record (through two wrappers), each with an optional field.
exampleSchema :: [(String, TypedTerm Type)]
exampleSchema = [
    ("example.Measurement", Core.typeForall $ Core.forallType (nm "u") $ Core.typeRecord $ Phantoms.list [
      Core.fieldType (nm "unit") (Core.typeVariable $ nm "u"),
      Core.fieldType (nm "value") float64T,
      Core.fieldType (nm "error") (Core.typeOptional float64T)]),
    ("example.TimeUnit", Core.typeUnion $ Phantoms.list [
      Core.fieldType (nm "millisecond") Core.typeUnit,
      Core.fieldType (nm "second") Core.typeUnit]),
    ("example.Duration", Core.typeWrap $ Core.typeApplication $
      Core.applicationType (Core.typeVariable $ nm "example.Measurement") (Core.typeVariable $ nm "example.TimeUnit")),
    ("example.Offset", Core.typeWrap $ Core.typeVariable $ nm "example.Duration"),
    ("example.Span", Core.typeRecord $ Phantoms.list [
      Core.fieldType (nm "start") float64T,
      Core.fieldType (nm "end") (Core.typeOptional float64T)])]

-- | example.Span{start = 1.5, end = <end>}, as inference elaborates it
spanTerm :: TypedTerm Term -> TypedTerm Term
spanTerm end = Core.termRecord $ Core.record (nm "example.Span") $ Phantoms.list [
  Core.field (nm "start") (float64Term 1.5),
  Core.field (nm "end") end]

-- | example.Offset{example.Duration{example.Measurement{unit = second, value = 1.5, error = <err>}}}, as inference
--   elaborates it (the parametric record instantiated at example.TimeUnit)
offsetTerm :: TypedTerm Term -> TypedTerm Term
offsetTerm err = Core.termWrap $ Core.wrappedTerm (nm "example.Offset") $
  Core.termWrap $ Core.wrappedTerm (nm "example.Duration") $
    tyApp (Core.termRecord $ Core.record (nm "example.Measurement") $ Phantoms.list [
      Core.field (nm "unit") (Core.termInject $ Core.injection (nm "example.TimeUnit") (Core.field (nm "second") Core.termUnit)),
      Core.field (nm "value") (float64Term 1.5),
      Core.field (nm "error") err])
    (Core.typeVariable $ nm "example.TimeUnit")

-- | "ok" if graphToTermGraph succeeds on the binding `x = <term>` of type <type> over the example schema, else the error
termGraphCase :: String -> TypedTerm Term -> String -> TypedTerm TestCaseWithMetadata
termGraphCase cname t tname = universalCase cname
  (Eithers.either
    (Phantoms.lambda "e" (PrintErrors.error_ @@ Phantoms.var "e"))
    (Phantoms.constant (Phantoms.string "ok"))
    (SubtermsModule.graphToTermGraph @@ fixtureGraph [("x", t)]
      (exampleSchema L.++ [("x", Core.typeVariable $ nm tname)])))
  (Phantoms.string "ok")

elaboratedGraphGroup :: TypedTerm TestGroup
elaboratedGraphGroup = subgroup "graphToTermGraph on elaborated absent optionals" [
    termGraphCase "plain record, optional given" (spanTerm $ Core.termOptional $ Phantoms.just $ float64Term 2.0) "example.Span",
    termGraphCase "plain record, optional none" (spanTerm $ tyApp noneTerm float64T) "example.Span",
    termGraphCase "parametric record, optional given" (offsetTerm $ Core.termOptional $ Phantoms.just $ float64Term 0.1) "example.Offset",
    termGraphCase "parametric record, optional none" (offsetTerm $ tyApp noneTerm float64T) "example.Offset"]
