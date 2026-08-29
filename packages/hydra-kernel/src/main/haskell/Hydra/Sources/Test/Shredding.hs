{-# LANGUAGE FlexibleContexts #-}

-- | Test cases for hydra.print.paths (step/path printer round-trip) and hydra.shredding (the link view).
module Hydra.Sources.Test.Shredding where

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
import qualified Hydra.Dsl.Lib.Optionals as Optionals


ns :: ModuleName
ns = ModuleName "hydra.test.shredding"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([PrintPaths.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata (Just "Test cases for path serialization round-trips and graph shredding")}
  where
    definitions = [Phantoms.toDefinition allTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for path serialization and graph shredding" $
    supergroup "shredding" [
      subtermStepRoundTripGroup,
      subtypeStepRoundTripGroup]

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
    subtermRoundTrip "mapKey (indexed)" (Paths.subtermStepMapKey (Phantoms.int32 0)),
    subtermRoundTrip "mapValue (indexed)" (Paths.subtermStepMapValue (Phantoms.int32 1)),
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
