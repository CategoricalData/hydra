{-# LANGUAGE FlexibleContexts #-}

-- | Test cases for hydra.print.paths (step/path printer round-trip), hydra.subterms (the term-graph view),
--   and hydra.validate.paths (the TermGraph:TypeGraph path-erasure relation).
module Hydra.Sources.Test.Subterms where

-- Standard imports for tests
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
import Hydra.Overlay.Haskell.Dsl.Typed.Terms                   as Terms hiding ((@@))
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core          as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms                ((@@))
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types         as T
import qualified Hydra.Dsl.Paths                               as Paths
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Hydra.Testing

import qualified Hydra.Sources.Kernel.Terms.Print.Paths as PrintPaths
import qualified Hydra.Sources.Kernel.Terms.Validate.Paths as ValidatePaths
import qualified Hydra.Dsl.Lib.Optionals as Optionals


ns :: ModuleName
ns = ModuleName "hydra.test.subterms"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([PrintPaths.ns, ValidatePaths.ns] L.++ kernelTypesModuleNames),
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
      erasureGroup]

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
