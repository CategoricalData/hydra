{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Terms.Print.Paths where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Overlay.Haskell.Libraries
import qualified Hydra.Dsl.Paths       as Paths
import qualified Hydra.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Lib.Chars    as Chars
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Math     as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Sets     as Sets
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base         as MetaBase
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms        as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular           as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing      as Testing
import qualified Hydra.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Names as Names


ns :: ModuleName
ns = ModuleName "hydra.print.paths"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Names.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just ("Serialization (printing and parsing) of subterm and subtype steps and paths."))}
  where
   definitions = [
     toDefinition parseSubtermPath,
     toDefinition parseSubtermStep,
     toDefinition parseSubtypePath,
     toDefinition parseSubtypeStep,
     toDefinition subtermPath,
     toDefinition subtermStep,
     toDefinition subtypePath,
     toDefinition subtypeStep]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

parseSubtermPath :: TypedTermDefinition (String -> Maybe SubtermPath)
parseSubtermPath = define "parseSubtermPath" $
  doc "Parse a printed subterm path (steps joined by '/') back into a SubtermPath" $
  "s" ~> Optionals.map
    (reify Paths.subtermPath)
    -- sequence-map over the tokens: all must parse, else the whole path fails.
    -- Hand-rolled via foldl+bind (portable) rather than optionals.mapList (no Java/Python/Scala impl).
    (Lists.foldl
      ("macc" ~> "tok" ~> Optionals.bind (var "macc")
        ("acc" ~> Optionals.map ("st" ~> Lists.concat2 (var "acc") (list [var "st"])) (parseSubtermStep @@ var "tok")))
      (just (list ([] :: [TypedTerm SubtermStep])))
      (Strings.splitOn (string "/") (var "s")))

parseSubtermStep :: TypedTermDefinition (String -> Maybe SubtermStep)
parseSubtermStep = define "parseSubtermStep" $
  doc "Parse a printed subterm step token back into a SubtermStep" $
  "tok" ~>
  "segs" <~ Strings.splitOn (string ":") (var "tok") $
  "tag" <~ Optionals.withDefault (var "tok") (Lists.at (int32 0) $ var "segs") $
  "mpayload" <~ Lists.at (int32 1) (var "segs") $
  "name" <~ Optionals.map (reify Core.name) (var "mpayload") $
  "idx" <~ Optionals.bind (var "mpayload") (reify Literals.parseInt32) $
  Logic.ifElse (Equality.equal (var "tag") (string "annotatedAnnotation")) (just Paths.subtermStepAnnotatedAnnotation) $
  Logic.ifElse (Equality.equal (var "tag") (string "annotatedBody")) (just Paths.subtermStepAnnotatedBody) $
  Logic.ifElse (Equality.equal (var "tag") (string "applicationArgument")) (just Paths.subtermStepApplicationArgument) $
  Logic.ifElse (Equality.equal (var "tag") (string "applicationFunction")) (just Paths.subtermStepApplicationFunction) $
  Logic.ifElse (Equality.equal (var "tag") (string "casesCase")) (Optionals.map (reify Paths.subtermStepCasesCase) (var "name")) $
  Logic.ifElse (Equality.equal (var "tag") (string "casesDefault")) (just Paths.subtermStepCasesDefault) $
  Logic.ifElse (Equality.equal (var "tag") (string "eitherLeft")) (just Paths.subtermStepEitherLeft) $
  Logic.ifElse (Equality.equal (var "tag") (string "eitherRight")) (just Paths.subtermStepEitherRight) $
  Logic.ifElse (Equality.equal (var "tag") (string "injectField")) (Optionals.map (reify Paths.subtermStepInjectField) (var "name")) $
  Logic.ifElse (Equality.equal (var "tag") (string "lambdaBody")) (just Paths.subtermStepLambdaBody) $
  Logic.ifElse (Equality.equal (var "tag") (string "letBinding")) (Optionals.map (reify Paths.subtermStepLetBinding) (var "name")) $
  Logic.ifElse (Equality.equal (var "tag") (string "letBody")) (just Paths.subtermStepLetBody) $
  Logic.ifElse (Equality.equal (var "tag") (string "listElement")) (Optionals.map (reify Paths.subtermStepListElement) (var "idx")) $
  Logic.ifElse (Equality.equal (var "tag") (string "mapKey")) (Optionals.map (reify Paths.subtermStepMapKey) (var "idx")) $
  Logic.ifElse (Equality.equal (var "tag") (string "mapValue")) (Optionals.map (reify Paths.subtermStepMapValue) (var "idx")) $
  Logic.ifElse (Equality.equal (var "tag") (string "optionalGiven")) (just Paths.subtermStepOptionalGiven) $
  Logic.ifElse (Equality.equal (var "tag") (string "pairFirst")) (just Paths.subtermStepPairFirst) $
  Logic.ifElse (Equality.equal (var "tag") (string "pairSecond")) (just Paths.subtermStepPairSecond) $
  Logic.ifElse (Equality.equal (var "tag") (string "recordField")) (Optionals.map (reify Paths.subtermStepRecordField) (var "name")) $
  Logic.ifElse (Equality.equal (var "tag") (string "setElement")) (Optionals.map (reify Paths.subtermStepSetElement) (var "idx")) $
  Logic.ifElse (Equality.equal (var "tag") (string "typeApplicationBody")) (just Paths.subtermStepTypeApplicationBody) $
  Logic.ifElse (Equality.equal (var "tag") (string "typeLambdaBody")) (just Paths.subtermStepTypeLambdaBody) $
  Logic.ifElse (Equality.equal (var "tag") (string "wrapBody")) (just Paths.subtermStepWrapBody) $
  nothing

parseSubtypePath :: TypedTermDefinition (String -> Maybe SubtypePath)
parseSubtypePath = define "parseSubtypePath" $
  doc "Parse a printed subtype path (steps joined by '/') back into a SubtypePath" $
  "s" ~> Optionals.map
    (reify Paths.subtypePath)
    -- sequence-map over the tokens (portable foldl+bind; see parseSubtermPath).
    (Lists.foldl
      ("macc" ~> "tok" ~> Optionals.bind (var "macc")
        ("acc" ~> Optionals.map ("st" ~> Lists.concat2 (var "acc") (list [var "st"])) (parseSubtypeStep @@ var "tok")))
      (just (list ([] :: [TypedTerm SubtypeStep])))
      (Strings.splitOn (string "/") (var "s")))

parseSubtypeStep :: TypedTermDefinition (String -> Maybe SubtypeStep)
parseSubtypeStep = define "parseSubtypeStep" $
  doc "Parse a printed subtype step token back into a SubtypeStep" $
  "tok" ~>
  "segs" <~ Strings.splitOn (string ":") (var "tok") $
  "tag" <~ Optionals.withDefault (var "tok") (Lists.at (int32 0) $ var "segs") $
  "mpayload" <~ Lists.at (int32 1) (var "segs") $
  "name" <~ Optionals.map (reify Core.name) (var "mpayload") $
  Logic.ifElse (Equality.equal (var "tag") (string "annotatedBody")) (just Paths.subtypeStepAnnotatedBody) $
  Logic.ifElse (Equality.equal (var "tag") (string "applicationArgument")) (just Paths.subtypeStepApplicationArgument) $
  Logic.ifElse (Equality.equal (var "tag") (string "applicationFunction")) (just Paths.subtypeStepApplicationFunction) $
  Logic.ifElse (Equality.equal (var "tag") (string "effectValue")) (just Paths.subtypeStepEffectValue) $
  Logic.ifElse (Equality.equal (var "tag") (string "eitherLeft")) (just Paths.subtypeStepEitherLeft) $
  Logic.ifElse (Equality.equal (var "tag") (string "eitherRight")) (just Paths.subtypeStepEitherRight) $
  Logic.ifElse (Equality.equal (var "tag") (string "forallBody")) (just Paths.subtypeStepForallBody) $
  Logic.ifElse (Equality.equal (var "tag") (string "functionCodomain")) (just Paths.subtypeStepFunctionCodomain) $
  Logic.ifElse (Equality.equal (var "tag") (string "functionDomain")) (just Paths.subtypeStepFunctionDomain) $
  Logic.ifElse (Equality.equal (var "tag") (string "listElement")) (just Paths.subtypeStepListElement) $
  Logic.ifElse (Equality.equal (var "tag") (string "mapKeys")) (just Paths.subtypeStepMapKeys) $
  Logic.ifElse (Equality.equal (var "tag") (string "mapValues")) (just Paths.subtypeStepMapValues) $
  Logic.ifElse (Equality.equal (var "tag") (string "optionalElement")) (just Paths.subtypeStepOptionalElement) $
  Logic.ifElse (Equality.equal (var "tag") (string "pairFirst")) (just Paths.subtypeStepPairFirst) $
  Logic.ifElse (Equality.equal (var "tag") (string "pairSecond")) (just Paths.subtypeStepPairSecond) $
  Logic.ifElse (Equality.equal (var "tag") (string "recordField")) (Optionals.map (reify Paths.subtypeStepRecordField) (var "name")) $
  Logic.ifElse (Equality.equal (var "tag") (string "setElement")) (just Paths.subtypeStepSetElement) $
  Logic.ifElse (Equality.equal (var "tag") (string "unionField")) (Optionals.map (reify Paths.subtypeStepUnionField) (var "name")) $
  Logic.ifElse (Equality.equal (var "tag") (string "wrapBody")) (just Paths.subtypeStepWrapBody) $
  nothing

subtermPath :: TypedTermDefinition (SubtermPath -> String)
subtermPath = define "subtermPath" $
  doc "Print a subterm path as its steps joined by '/'" $
  "path" ~> Strings.join (string "/") (Lists.map (asTerm subtermStep) (unwrap _SubtermPath @@ var "path"))

subtermStep :: TypedTermDefinition (SubtermStep -> String)
subtermStep = define "subtermStep" $
  doc "Print a subterm step in its round-trippable notation" $
  "step" ~> match _SubtermStep (var "step") Nothing [
    _SubtermStep_annotatedAnnotation>>: constant (string "annotatedAnnotation"),
    _SubtermStep_annotatedBody>>: constant (string "annotatedBody"),
    _SubtermStep_applicationArgument>>: constant (string "applicationArgument"),
    _SubtermStep_applicationFunction>>: constant (string "applicationFunction"),
    _SubtermStep_casesCase>>: "name" ~> Strings.concat2 (string "casesCase:") (Core.unName $ var "name"),
    _SubtermStep_casesDefault>>: constant (string "casesDefault"),
    _SubtermStep_eitherLeft>>: constant (string "eitherLeft"),
    _SubtermStep_eitherRight>>: constant (string "eitherRight"),
    _SubtermStep_injectField>>: "name" ~> Strings.concat2 (string "injectField:") (Core.unName $ var "name"),
    _SubtermStep_lambdaBody>>: constant (string "lambdaBody"),
    _SubtermStep_letBinding>>: "name" ~> Strings.concat2 (string "letBinding:") (Core.unName $ var "name"),
    _SubtermStep_letBody>>: constant (string "letBody"),
    _SubtermStep_listElement>>: "i" ~> Strings.concat2 (string "listElement:") (Literals.printInt32 $ var "i"),
    _SubtermStep_mapKey>>: "i" ~> Strings.concat2 (string "mapKey:") (Literals.printInt32 $ var "i"),
    _SubtermStep_mapValue>>: "i" ~> Strings.concat2 (string "mapValue:") (Literals.printInt32 $ var "i"),
    _SubtermStep_optionalGiven>>: constant (string "optionalGiven"),
    _SubtermStep_pairFirst>>: constant (string "pairFirst"),
    _SubtermStep_pairSecond>>: constant (string "pairSecond"),
    _SubtermStep_recordField>>: "name" ~> Strings.concat2 (string "recordField:") (Core.unName $ var "name"),
    _SubtermStep_setElement>>: "i" ~> Strings.concat2 (string "setElement:") (Literals.printInt32 $ var "i"),
    _SubtermStep_typeApplicationBody>>: constant (string "typeApplicationBody"),
    _SubtermStep_typeLambdaBody>>: constant (string "typeLambdaBody"),
    _SubtermStep_wrapBody>>: constant (string "wrapBody")]

subtypePath :: TypedTermDefinition (SubtypePath -> String)
subtypePath = define "subtypePath" $
  doc "Print a subtype path as its steps joined by '/'" $
  "path" ~> Strings.join (string "/") (Lists.map (asTerm subtypeStep) (unwrap _SubtypePath @@ var "path"))

subtypeStep :: TypedTermDefinition (SubtypeStep -> String)
subtypeStep = define "subtypeStep" $
  doc "Print a subtype step in its round-trippable notation" $
  "step" ~> match _SubtypeStep (var "step") Nothing [
    _SubtypeStep_annotatedBody>>: constant (string "annotatedBody"),
    _SubtypeStep_applicationArgument>>: constant (string "applicationArgument"),
    _SubtypeStep_applicationFunction>>: constant (string "applicationFunction"),
    _SubtypeStep_effectValue>>: constant (string "effectValue"),
    _SubtypeStep_eitherLeft>>: constant (string "eitherLeft"),
    _SubtypeStep_eitherRight>>: constant (string "eitherRight"),
    _SubtypeStep_forallBody>>: constant (string "forallBody"),
    _SubtypeStep_functionCodomain>>: constant (string "functionCodomain"),
    _SubtypeStep_functionDomain>>: constant (string "functionDomain"),
    _SubtypeStep_listElement>>: constant (string "listElement"),
    _SubtypeStep_mapKeys>>: constant (string "mapKeys"),
    _SubtypeStep_mapValues>>: constant (string "mapValues"),
    _SubtypeStep_optionalElement>>: constant (string "optionalElement"),
    _SubtypeStep_pairFirst>>: constant (string "pairFirst"),
    _SubtypeStep_pairSecond>>: constant (string "pairSecond"),
    _SubtypeStep_recordField>>: "name" ~> Strings.concat2 (string "recordField:") (Core.unName $ var "name"),
    _SubtypeStep_setElement>>: constant (string "setElement"),
    _SubtypeStep_unionField>>: "name" ~> Strings.concat2 (string "unionField:") (Core.unName $ var "name"),
    _SubtypeStep_wrapBody>>: constant (string "wrapBody")]
