{-# LANGUAGE ScopedTypeVariables #-}

-- | The TermGraph : TypeGraph conformance relation for hydra.paths: path erasure (a term-side position
--   maps to the type-side position of its type) plus node typing. Partial on the computation fragment —
--   steps that have no type-side analog (application/lambda/let/cases/typeApp/typeLambda/wrap/unwrap)
--   erase to nothing and their subtrees are not checked structurally against the schema.
module Hydra.Sources.Kernel.Terms.Validate.Paths where

import Hydra.Kernel
import Hydra.Overlay.Haskell.Libraries
import qualified Hydra.Dsl.Paths        as Paths
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Strings  as Strings
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.List                   as L
import qualified Data.Map                    as M


ns :: ModuleName
ns = ModuleName "hydra.validate.paths"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just
              ("The TermGraph : TypeGraph conformance relation: path erasure (a subterm position maps to"
               <> " the subtype position of its type) plus node typing, partial on the computation fragment."))}
  where
   definitions = [
     toDefinition eraseSubtermPath,
     toDefinition eraseSubtermStep]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- | Erase a single subterm step to its subtype-side analog, if any. Total on the data fragment; nothing
--   on the computation fragment (application/lambda/let/cases/typeApp/typeLambda/wrap/unwrap/annotated),
--   and on the pair components and mapEntry, which are handled at the path level (see eraseSubtermPath):
--   a map entry's key/value reach the map's key/value types only as the two-step sequence
--   mapEntry i · pairFirst ↦ mapKeys and mapEntry i · pairSecond ↦ mapValues.
eraseSubtermStep :: TypedTermDefinition (SubtermStep -> Maybe SubtypeStep)
eraseSubtermStep = define "eraseSubtermStep" $
  doc "The subtype-side analog of a subterm step on the data fragment, or nothing on the computation fragment" $
  "step" ~>
  match _SubtermStep (var "step") (Just nothing) [
    _SubtermStep_listElement>>: constant (just Paths.subtypeStepListElement),
    _SubtermStep_setElement>>: constant (just Paths.subtypeStepSetElement),
    _SubtermStep_optionalGiven>>: constant (just Paths.subtypeStepOptionalElement),
    _SubtermStep_injectField>>: "n" ~> just (Paths.subtypeStepUnionField $ var "n"),
    _SubtermStep_recordField>>: "n" ~> just (Paths.subtypeStepRecordField $ var "n")]

-- | Erase a subterm path to a subtype path (the type-side position of the term at that path), if the path
--   lies wholly in the data fragment. The one non-1:1 rewrite is the map entry: mapEntry i · pairFirst
--   collapses to mapKeys and mapEntry i · pairSecond to mapValues. A step with no analog (or a bare
--   mapEntry / pair component not part of a map-entry projection) yields nothing (the path leaves the data
--   fragment, so there is no corresponding type-side position).
eraseSubtermPath :: TypedTermDefinition (SubtermPath -> Maybe SubtypePath)
eraseSubtermPath = define "eraseSubtermPath" $
  doc "Erase a subterm path to the subtype path of its type, if wholly within the data fragment" $
  "path" ~>
  "steps" <~ unwrap _SubtermPath @@ var "path" $
  -- Fold over the steps, collapsing the mapEntry·pairFirst/pairSecond sequences. State: Maybe [SubtypeStep];
  -- once nothing (fell off the data fragment) it stays nothing. `pending` carries an in-flight mapEntry.
  "go" <~ ("acc" ~> "step" ~>
    Optionals.match (var "acc")
      nothing
      ("stateP" ~>
        "out" <~ Pairs.first (var "stateP") $
        "pendingEntry" <~ Pairs.second (var "stateP") $
        Logic.ifElse (var "pendingEntry")
          -- previous step was mapEntry: this step must be pairFirst/pairSecond → mapKeys/mapValues
          (match _SubtermStep (var "step") (Just nothing) [
            _SubtermStep_pairFirst>>: constant (just $ pair (Lists.concat2 (var "out") (list [Paths.subtypeStepMapKeys])) false),
            _SubtermStep_pairSecond>>: constant (just $ pair (Lists.concat2 (var "out") (list [Paths.subtypeStepMapValues])) false)])
          -- no pending entry: a mapEntry starts one; otherwise erase the step directly
          (match _SubtermStep (var "step")
            (Just $ Optionals.map ("st" ~> pair (Lists.concat2 (var "out") (list [var "st"])) false)
              (eraseSubtermStep @@ var "step")) [
            _SubtermStep_mapEntry>>: constant (just $ pair (var "out") true)]))) $
  "result" <~ Lists.foldl (var "go")
    (just $ pair (list ([] :: [TypedTerm SubtypeStep])) false)
    (var "steps") $
  -- A path that ends with a pending (un-projected) mapEntry has no type-side position.
  Optionals.bind (var "result")
    ("stateP" ~> Logic.ifElse (Pairs.second $ var "stateP")
      nothing
      (just $ Paths.subtypePath (Pairs.first $ var "stateP")))
