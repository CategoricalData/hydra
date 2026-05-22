module Hydra.Sources.Validate.Pg where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (Edge(..), _Edge, _Edge_in, _Edge_out, Element(..), _Element, Graph(..), _Graph)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Validation                      as Validation
import           Prelude hiding ((++))
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Maybe                                as Y

-- Additional imports
import Hydra.Pg.Model as PG
import Hydra.Error.Pg as Err
import qualified Hydra.Sources.Pg.Model as PgModel
import qualified Hydra.Sources.Error.Pg as ErrorPg


validationDefinition :: String -> TTerm a -> TTermDefinition a
validationDefinition = definitionInModule module_

module_ :: Module
module_ = Module {
            moduleNamespace = (Namespace "hydra.validate.pg"),
            moduleDefinitions = definitions,
            moduleDependencies = [PgModel.ns, ErrorPg.ns, Namespace "hydra.validation", Namespace "hydra.core"],
            moduleDescription = Just "Validation functions for property graphs"}
  where
   definitions = [
     toDefinition appendFindingEdge,
     toDefinition appendFindingGraph,
     toDefinition appendFindingProperty,
     toDefinition appendFindingVertex,
     toDefinition defaultPgProfile,
     toDefinition enabledPg,
     toDefinition validateEdge,
     toDefinition validateGraph,
     toDefinition validateProperties,
     toDefinition validateVertex]

-- ============================================================================
-- Profile helpers (host-side)
-- ============================================================================

-- | Compose a fully qualified rule identifier from a union-type qualified
-- name and a variant local name, joined with '.'. Mirrors the helper of the
-- same name in Validate/Core.hs and Validate/Packaging.hs.
qualifiedRule :: Name -> Name -> Name
qualifiedRule (Name u) (Name v) = Name (L.concat [u, ".", v])

-- | Wrap a leaf-shaped 'Maybe InvalidVertexError' finding with the rule that
-- produced it, gated by the active profile. Same pattern as 'guardedTermRule'
-- in Validate/Core.hs.
guardedVertexRule
  :: TTerm ValidationProfile
  -> Name -- ^ Union-type qualified name (e.g. _InvalidVertexError).
  -> Name -- ^ Variant local name (e.g. _InvalidVertexError_label).
  -> TTerm (Maybe InvalidVertexError)
  -> TTerm (Maybe (Name, InvalidVertexError))
guardedVertexRule profile unionName variantName findingExpr =
  Logic.ifElse (enabledPg @@ profile @@ ruleNameTerm)
    (Maybes.map ("f" ~> pair ruleNameTerm (var "f")) findingExpr)
    nothing
  where
    ruleNameTerm = nameLift (qualifiedRule unionName variantName)

-- | Edge counterpart of 'guardedVertexRule'.
guardedEdgeRule
  :: TTerm ValidationProfile
  -> Name
  -> Name
  -> TTerm (Maybe InvalidEdgeError)
  -> TTerm (Maybe (Name, InvalidEdgeError))
guardedEdgeRule profile unionName variantName findingExpr =
  Logic.ifElse (enabledPg @@ profile @@ ruleNameTerm)
    (Maybes.map ("f" ~> pair ruleNameTerm (var "f")) findingExpr)
    nothing
  where
    ruleNameTerm = nameLift (qualifiedRule unionName variantName)

-- | Property counterpart. The payload is 'InvalidElementPropertyError'
-- (the wrapper that pairs the property key with the inner
-- 'InvalidPropertyError'), but the rule name still refers to the inner
-- 'InvalidPropertyError' variant.
guardedPropertyRule
  :: TTerm ValidationProfile
  -> Name -- ^ _InvalidPropertyError
  -> Name -- ^ Variant local name (e.g. _InvalidPropertyError_missingRequired).
  -> TTerm (Maybe InvalidElementPropertyError)
  -> TTerm (Maybe (Name, InvalidElementPropertyError))
guardedPropertyRule profile unionName variantName findingExpr =
  Logic.ifElse (enabledPg @@ profile @@ ruleNameTerm)
    (Maybes.map ("f" ~> pair ruleNameTerm (var "f")) findingExpr)
    nothing
  where
    ruleNameTerm = nameLift (qualifiedRule unionName variantName)

-- | An empty ValidationResult parameterized by 'e'.
emptyResult :: TTerm (ValidationResult e)
emptyResult = Validation.validationResult
  (list ([] :: [TTerm e]))
  (list ([] :: [TTerm e]))

-- | The full set of PG rule names used by 'defaultPgProfile' and the
-- strict-profile legacy shims. Single source of truth: any rule wired
-- into a 'guarded*Rule' call must appear here so the legacy shims
-- continue to detect it.
--
-- Note: the 'InvalidGraphError.vertex' and 'InvalidGraphError.edge'
-- variants are deliberately excluded. They are not per-rule checks; they
-- are the wrappers used by 'validateGraph' to lift vertex- and
-- edge-level findings into graph-level findings. The lift is
-- unconditional, so including them in a profile would have no effect.
allPgRuleNames :: [Name]
allPgRuleNames = L.concat
  [ fmap (qualifiedRule _InvalidVertexError)
      [ _InvalidVertexError_id
      , _InvalidVertexError_label
      , _InvalidVertexError_property]
  , fmap (qualifiedRule _InvalidEdgeError)
      [ _InvalidEdgeError_id
      , _InvalidEdgeError_inVertexLabel
      , _InvalidEdgeError_inVertexNotFound
      , _InvalidEdgeError_label
      , _InvalidEdgeError_outVertexLabel
      , _InvalidEdgeError_outVertexNotFound
      , _InvalidEdgeError_property]
  , fmap (qualifiedRule _InvalidPropertyError)
      [ _InvalidPropertyError_invalidValue
      , _InvalidPropertyError_missingRequired
      , _InvalidPropertyError_unexpectedKey]]

-- ============================================================================
-- Profile-aware helpers (DSL definitions)
-- ============================================================================

-- | Test whether a rule is active in a profile (in either errorRules or
-- warningRules). Local counterpart of 'enabled' in Validate/Core.hs;
-- named 'enabledPg' to avoid clashing if both modules are imported into
-- the same scope.
enabledPg :: TTermDefinition (ValidationProfile -> Name -> Bool)
enabledPg = validationDefinition "enabledPg" $
  doc "True iff the given rule name appears in the profile's errorRules or warningRules." $
  "p" ~> "ruleName" ~>
  Logic.or
    (Sets.member (var "ruleName") (Validation.validationProfileErrorRules $ var "p"))
    (Sets.member (var "ruleName") (Validation.validationProfileWarningRules $ var "p"))

-- | The default validation profile for hydra.validate.pg. Every PG rule
-- is classified as an error; no warnings; 'maxErrors = 1' preserves the
-- legacy 'first error wins' behaviour; 'maxWarnings = 20' is a small
-- starting cap.
defaultPgProfile :: TTermDefinition ValidationProfile
defaultPgProfile = validationDefinition "defaultPgProfile" $
  doc "The default validation profile for property-graph validation. Every PG rule classified as an error; no warnings; maxErrors=1, maxWarnings=20." $
  Validation.validationProfile
    (Sets.fromList $ list $ nameLift <$> allPgRuleNames)
    Sets.empty
    (int32 1)
    (int32 20)

-- | Classify a rule-tagged 'Maybe (Name, InvalidVertexError)' finding
-- against the active profile and append the payload (without its rule
-- tag) to the appropriate list in the accumulator. Same semantics as
-- 'appendFinding' in Validate/Core.hs.
appendFindingVertex :: TTermDefinition (
  ValidationProfile
  -> ValidationResult InvalidVertexError
  -> Maybe (Name, InvalidVertexError)
  -> ValidationResult InvalidVertexError)
appendFindingVertex = validationDefinition "appendFindingVertex" $
  doc "Append a rule-tagged InvalidVertexError finding to a ValidationResult, classifying as error or warning per the profile and respecting maxErrors/maxWarnings bounds." $
  "p" ~> "acc" ~> "finding" ~>
  Maybes.cases (var "finding")
    (var "acc")
    ("rp" ~>
      "ruleName" <~ Pairs.first (var "rp") $
      "payload" <~ Pairs.second (var "rp") $
      "errs" <~ Validation.validationResultErrors (var "acc") $
      "wrns" <~ Validation.validationResultWarnings (var "acc") $
      Logic.ifElse (Sets.member (var "ruleName") (Validation.validationProfileErrorRules $ var "p"))
        (Logic.ifElse (Equality.lt (Lists.length $ var "errs") (Validation.validationProfileMaxErrors $ var "p"))
          (Validation.validationResult
            (Lists.concat2 (var "errs") (Lists.singleton $ var "payload"))
            (var "wrns"))
          (var "acc"))
        (Logic.ifElse (Sets.member (var "ruleName") (Validation.validationProfileWarningRules $ var "p"))
          (Logic.ifElse (Equality.lt (Lists.length $ var "wrns") (Validation.validationProfileMaxWarnings $ var "p"))
            (Validation.validationResult
              (var "errs")
              (Lists.concat2 (var "wrns") (Lists.singleton $ var "payload")))
            (var "acc"))
          (var "acc")))

-- | Edge counterpart of 'appendFindingVertex'.
appendFindingEdge :: TTermDefinition (
  ValidationProfile
  -> ValidationResult InvalidEdgeError
  -> Maybe (Name, InvalidEdgeError)
  -> ValidationResult InvalidEdgeError)
appendFindingEdge = validationDefinition "appendFindingEdge" $
  doc "Append a rule-tagged InvalidEdgeError finding to a ValidationResult." $
  "p" ~> "acc" ~> "finding" ~>
  Maybes.cases (var "finding")
    (var "acc")
    ("rp" ~>
      "ruleName" <~ Pairs.first (var "rp") $
      "payload" <~ Pairs.second (var "rp") $
      "errs" <~ Validation.validationResultErrors (var "acc") $
      "wrns" <~ Validation.validationResultWarnings (var "acc") $
      Logic.ifElse (Sets.member (var "ruleName") (Validation.validationProfileErrorRules $ var "p"))
        (Logic.ifElse (Equality.lt (Lists.length $ var "errs") (Validation.validationProfileMaxErrors $ var "p"))
          (Validation.validationResult
            (Lists.concat2 (var "errs") (Lists.singleton $ var "payload"))
            (var "wrns"))
          (var "acc"))
        (Logic.ifElse (Sets.member (var "ruleName") (Validation.validationProfileWarningRules $ var "p"))
          (Logic.ifElse (Equality.lt (Lists.length $ var "wrns") (Validation.validationProfileMaxWarnings $ var "p"))
            (Validation.validationResult
              (var "errs")
              (Lists.concat2 (var "wrns") (Lists.singleton $ var "payload")))
            (var "acc"))
          (var "acc")))

-- | Property counterpart. Payload type is 'InvalidElementPropertyError'.
appendFindingProperty :: TTermDefinition (
  ValidationProfile
  -> ValidationResult InvalidElementPropertyError
  -> Maybe (Name, InvalidElementPropertyError)
  -> ValidationResult InvalidElementPropertyError)
appendFindingProperty = validationDefinition "appendFindingProperty" $
  doc "Append a rule-tagged InvalidElementPropertyError finding to a ValidationResult." $
  "p" ~> "acc" ~> "finding" ~>
  Maybes.cases (var "finding")
    (var "acc")
    ("rp" ~>
      "ruleName" <~ Pairs.first (var "rp") $
      "payload" <~ Pairs.second (var "rp") $
      "errs" <~ Validation.validationResultErrors (var "acc") $
      "wrns" <~ Validation.validationResultWarnings (var "acc") $
      Logic.ifElse (Sets.member (var "ruleName") (Validation.validationProfileErrorRules $ var "p"))
        (Logic.ifElse (Equality.lt (Lists.length $ var "errs") (Validation.validationProfileMaxErrors $ var "p"))
          (Validation.validationResult
            (Lists.concat2 (var "errs") (Lists.singleton $ var "payload"))
            (var "wrns"))
          (var "acc"))
        (Logic.ifElse (Sets.member (var "ruleName") (Validation.validationProfileWarningRules $ var "p"))
          (Logic.ifElse (Equality.lt (Lists.length $ var "wrns") (Validation.validationProfileMaxWarnings $ var "p"))
            (Validation.validationResult
              (var "errs")
              (Lists.concat2 (var "wrns") (Lists.singleton $ var "payload")))
            (var "acc"))
          (var "acc")))

-- | Graph counterpart. Polymorphic in the vertex value type 'v'.
appendFindingGraph :: TTermDefinition (
  ValidationProfile
  -> ValidationResult (InvalidGraphError v)
  -> Maybe (Name, InvalidGraphError v)
  -> ValidationResult (InvalidGraphError v))
appendFindingGraph = validationDefinition "appendFindingGraph" $
  doc "Append a rule-tagged InvalidGraphError finding to a ValidationResult." $
  "p" ~> "acc" ~> "finding" ~>
  Maybes.cases (var "finding")
    (var "acc")
    ("rp" ~>
      "ruleName" <~ Pairs.first (var "rp") $
      "payload" <~ Pairs.second (var "rp") $
      "errs" <~ Validation.validationResultErrors (var "acc") $
      "wrns" <~ Validation.validationResultWarnings (var "acc") $
      Logic.ifElse (Sets.member (var "ruleName") (Validation.validationProfileErrorRules $ var "p"))
        (Logic.ifElse (Equality.lt (Lists.length $ var "errs") (Validation.validationProfileMaxErrors $ var "p"))
          (Validation.validationResult
            (Lists.concat2 (var "errs") (Lists.singleton $ var "payload"))
            (var "wrns"))
          (var "acc"))
        (Logic.ifElse (Sets.member (var "ruleName") (Validation.validationProfileWarningRules $ var "p"))
          (Logic.ifElse (Equality.lt (Lists.length $ var "wrns") (Validation.validationProfileMaxWarnings $ var "p"))
            (Validation.validationResult
              (var "errs")
              (Lists.concat2 (var "wrns") (Lists.singleton $ var "payload")))
            (var "acc"))
          (var "acc")))

-- ============================================================================
-- Profile-aware leaf validators
--
-- Each of these returns a fresh 'ValidationResult E' (no accumulator
-- parameter) — they are leaves, not orchestrators. The orchestrator
-- 'validateGraph' calls them, lifts their findings into
-- 'InvalidGraphError', and accumulates into its own result.
-- ============================================================================

-- | Validate a single vertex against its expected type. Each per-rule
-- check ('label', 'id', 'property') is gated by the profile.
validateVertex :: TTermDefinition (
     ValidationProfile
  -> (t -> v -> Maybe InvalidValueError)
  -> PG.VertexType t
  -> PG.Vertex v
  -> ValidationResult InvalidVertexError)
validateVertex = validationDefinition "validateVertex" $
  doc "Validate a vertex against its VertexType under the given ValidationProfile, returning a ValidationResult InvalidVertexError." $
  "p" ~> "checkValue" ~> "typ" ~> "el" ~>
  Lists.foldl
    ("acc" ~> "guarded" ~>
      Logic.ifElse
        (Equality.gte
          (Lists.length $ Validation.validationResultErrors $ var "acc")
          (Validation.validationProfileMaxErrors $ var "p"))
        (var "acc")
        (appendFindingVertex @@ var "p" @@ var "acc" @@ var "guarded"))
    emptyResult
    (list [
      -- label: vertex label must match its VertexType label
      guardedVertexRule (var "p") _InvalidVertexError _InvalidVertexError_label
        (lets [
          "expected">: project _VertexType _VertexType_label @@ var "typ",
          "actual">: project _Vertex _Vertex_label @@ var "el"]
          $ Logic.ifElse
              (Equality.equal
                (unwrap _VertexLabel @@ var "actual")
                (unwrap _VertexLabel @@ var "expected"))
              nothing
              (just $ inject _InvalidVertexError _InvalidVertexError_label $
                record _NoSuchVertexLabelError [
                  _NoSuchVertexLabelError_label>>: var "actual"])),
      -- id: caller's checkValue applied to the id; lift into InvalidVertexError.id
      guardedVertexRule (var "p") _InvalidVertexError _InvalidVertexError_id
        (Maybes.map
          ("err" ~> inject _InvalidVertexError _InvalidVertexError_id $ var "err")
          (var "checkValue"
            @@ (project _VertexType _VertexType_id @@ var "typ")
            @@ (project _Vertex _Vertex_id @@ var "el"))),
      -- property: delegates to validateProperties; lift first finding into InvalidVertexError.property.
      -- Note: validateProperties returns a ValidationResult; for the rule-tagged
      -- finding here we only need the *head* of its errors list, since the
      -- per-vertex-rule guard fires once per vertex. Multi-property accumulation
      -- across the whole graph happens at the orchestrator level via maxErrors.
      guardedVertexRule (var "p") _InvalidVertexError _InvalidVertexError_property
        (Maybes.map
          ("err" ~> inject _InvalidVertexError _InvalidVertexError_property $ var "err")
          (Lists.maybeHead $ Validation.validationResultErrors $ validateProperties
            @@ var "p"
            @@ var "checkValue"
            @@ (project _VertexType _VertexType_properties @@ var "typ")
            @@ (project _Vertex _Vertex_properties @@ var "el")))])

-- | Validate a single edge against its expected type. Each per-rule
-- check is gated by the profile.
validateEdge :: TTermDefinition (
     ValidationProfile
  -> (t -> v -> Maybe InvalidValueError)
  -> Y.Maybe (v -> Y.Maybe PG.VertexLabel)
  -> PG.EdgeType t
  -> PG.Edge v
  -> ValidationResult InvalidEdgeError)
validateEdge = validationDefinition "validateEdge" $
  doc "Validate an edge against its EdgeType under the given ValidationProfile, returning a ValidationResult InvalidEdgeError." $
  "p" ~> "checkValue" ~> "labelForVertexId" ~> "typ" ~> "el" ~>
  Lists.foldl
    ("acc" ~> "guarded" ~>
      Logic.ifElse
        (Equality.gte
          (Lists.length $ Validation.validationResultErrors $ var "acc")
          (Validation.validationProfileMaxErrors $ var "p"))
        (var "acc")
        (appendFindingEdge @@ var "p" @@ var "acc" @@ var "guarded"))
    emptyResult
    (list [
      -- label: edge label must match its EdgeType label
      guardedEdgeRule (var "p") _InvalidEdgeError _InvalidEdgeError_label
        (lets [
          "expected">: project _EdgeType _EdgeType_label @@ var "typ",
          "actual">: project _Edge _Edge_label @@ var "el"]
          $ Logic.ifElse
              (Equality.equal
                (unwrap _EdgeLabel @@ var "actual")
                (unwrap _EdgeLabel @@ var "expected"))
              nothing
              (just $ inject _InvalidEdgeError _InvalidEdgeError_label $
                record _NoSuchEdgeLabelError [
                  _NoSuchEdgeLabelError_label>>: var "actual"])),
      -- id: caller's checkValue applied to the edge id
      guardedEdgeRule (var "p") _InvalidEdgeError _InvalidEdgeError_id
        (Maybes.map
          ("err" ~> inject _InvalidEdgeError _InvalidEdgeError_id $ var "err")
          (var "checkValue"
            @@ (project _EdgeType _EdgeType_id @@ var "typ")
            @@ (project _Edge _Edge_id @@ var "el"))),
      -- property: delegates to validateProperties (head of errors list)
      guardedEdgeRule (var "p") _InvalidEdgeError _InvalidEdgeError_property
        (Maybes.map
          ("err" ~> inject _InvalidEdgeError _InvalidEdgeError_property $ var "err")
          (Lists.maybeHead $ Validation.validationResultErrors $ validateProperties
            @@ var "p"
            @@ var "checkValue"
            @@ (project _EdgeType _EdgeType_properties @@ var "typ")
            @@ (project _Edge _Edge_properties @@ var "el"))),
      -- outVertexNotFound: out-vertex id resolved to no label.
      -- Only fires when labelForVertexId is Just (i.e., the caller asked
      -- for vertex-existence checks). When labelForVertexId is Nothing,
      -- both outVertexNotFound and outVertexLabel are skipped.
      guardedEdgeRule (var "p") _InvalidEdgeError _InvalidEdgeError_outVertexNotFound
        (Maybes.maybe nothing
          ("f" ~> Maybes.maybe
            (just (inject _InvalidEdgeError _InvalidEdgeError_outVertexNotFound $ unit))
            ("_label" ~> nothing)
            (var "f" @@ (project _Edge _Edge_out @@ var "el")))
          (var "labelForVertexId")),
      -- outVertexLabel: out-vertex's label doesn't match the EdgeType's expected out label.
      guardedEdgeRule (var "p") _InvalidEdgeError _InvalidEdgeError_outVertexLabel
        (Maybes.maybe nothing
          ("f" ~> Maybes.maybe
            nothing
            ("label" ~> Logic.ifElse
              (Equality.equal
                (unwrap _VertexLabel @@ var "label")
                (unwrap _VertexLabel @@ (project _EdgeType _EdgeType_out @@ var "typ")))
              nothing
              (just $ inject _InvalidEdgeError _InvalidEdgeError_outVertexLabel $
                record _WrongVertexLabelError [
                  _WrongVertexLabelError_expected>>: project _EdgeType _EdgeType_out @@ var "typ",
                  _WrongVertexLabelError_actual>>: var "label"]))
            (var "f" @@ (project _Edge _Edge_out @@ var "el")))
          (var "labelForVertexId")),
      -- inVertexNotFound: same as outVertexNotFound but for the in-vertex.
      guardedEdgeRule (var "p") _InvalidEdgeError _InvalidEdgeError_inVertexNotFound
        (Maybes.maybe nothing
          ("f" ~> Maybes.maybe
            (just (inject _InvalidEdgeError _InvalidEdgeError_inVertexNotFound $ unit))
            ("_label" ~> nothing)
            (var "f" @@ (project _Edge _Edge_in @@ var "el")))
          (var "labelForVertexId")),
      -- inVertexLabel
      guardedEdgeRule (var "p") _InvalidEdgeError _InvalidEdgeError_inVertexLabel
        (Maybes.maybe nothing
          ("f" ~> Maybes.maybe
            nothing
            ("label" ~> Logic.ifElse
              (Equality.equal
                (unwrap _VertexLabel @@ var "label")
                (unwrap _VertexLabel @@ (project _EdgeType _EdgeType_in @@ var "typ")))
              nothing
              (just $ inject _InvalidEdgeError _InvalidEdgeError_inVertexLabel $
                record _WrongVertexLabelError [
                  _WrongVertexLabelError_expected>>: project _EdgeType _EdgeType_in @@ var "typ",
                  _WrongVertexLabelError_actual>>: var "label"]))
            (var "f" @@ (project _Edge _Edge_in @@ var "el")))
          (var "labelForVertexId"))])

-- | Validate a property map against its expected schema. Three rules:
-- 'missingRequired' (per required type), 'unexpectedKey' (per actual key
-- not in schema), 'invalidValue' (per actual key whose value fails the
-- caller's checkValue). Each finding is a per-property
-- 'InvalidElementPropertyError' (key + inner InvalidPropertyError).
validateProperties :: TTermDefinition (
     ValidationProfile
  -> (t -> v -> Maybe InvalidValueError)
  -> [PG.PropertyType t]
  -> M.Map PG.PropertyKey v
  -> ValidationResult InvalidElementPropertyError)
validateProperties = validationDefinition "validateProperties" $
  doc "Validate a map of properties against a list of PropertyType under the given ValidationProfile, returning a ValidationResult InvalidElementPropertyError." $
  "p" ~> "checkValue" ~> "types" ~> "props" ~>
  lets [
    -- m: property-key -> expected-type, for fast lookup during invalidValue/unexpectedKey checks
    "m">: Maps.fromList (Lists.map
      ("pt" ~> pair
        (project _PropertyType _PropertyType_key @@ var "pt")
        (project _PropertyType _PropertyType_value @@ var "pt"))
      (var "types")),
    -- For each schema entry, build a guarded missingRequired finding.
    "missingChecks">: Lists.map
      ("t" ~> guardedPropertyRule (var "p") _InvalidPropertyError _InvalidPropertyError_missingRequired
        (Logic.ifElse (project _PropertyType _PropertyType_required @@ var "t")
          (Maybes.maybe
            (just (record _InvalidElementPropertyError [
              _InvalidElementPropertyError_key>>: project _PropertyType _PropertyType_key @@ var "t",
              _InvalidElementPropertyError_error>>:
                inject _InvalidPropertyError _InvalidPropertyError_missingRequired $
                  project _PropertyType _PropertyType_key @@ var "t"]))
            (constant nothing)
            (Maps.lookup (project _PropertyType _PropertyType_key @@ var "t") $ var "props"))
          nothing))
      (var "types"),
    -- For each actual property, build two guarded checks: unexpectedKey
    -- (key not in schema) or invalidValue (value fails checkValue). At
    -- most one of the two fires per property: if the key isn't in the
    -- schema, only unexpectedKey fires (invalidValue produces nothing
    -- because there's no expected type). If the key is in the schema,
    -- only invalidValue can fire.
    "valueChecks">: Lists.bind (Maps.toList $ var "props")
      ("kv" ~> lets [
        "key">: Pairs.first $ var "kv",
        "val">: Pairs.second $ var "kv"]
        $ list [
          guardedPropertyRule (var "p") _InvalidPropertyError _InvalidPropertyError_unexpectedKey
            (Maybes.maybe
              (just (record _InvalidElementPropertyError [
                _InvalidElementPropertyError_key>>: var "key",
                _InvalidElementPropertyError_error>>:
                  inject _InvalidPropertyError _InvalidPropertyError_unexpectedKey $ var "key"]))
              (constant nothing)
              (Maps.lookup (var "key") (var "m"))),
          guardedPropertyRule (var "p") _InvalidPropertyError _InvalidPropertyError_invalidValue
            (Maybes.maybe
              nothing
              ("typ" ~> Maybes.map
                ("err" ~> record _InvalidElementPropertyError [
                  _InvalidElementPropertyError_key>>: var "key",
                  _InvalidElementPropertyError_error>>:
                    inject _InvalidPropertyError _InvalidPropertyError_invalidValue $ var "err"])
                (var "checkValue" @@ var "typ" @@ var "val"))
              (Maps.lookup (var "key") (var "m")))])]
    $ Lists.foldl
      ("acc" ~> "guarded" ~>
        Logic.ifElse
          (Equality.gte
            (Lists.length $ Validation.validationResultErrors $ var "acc")
            (Validation.validationProfileMaxErrors $ var "p"))
          (var "acc")
          (appendFindingProperty @@ var "p" @@ var "acc" @@ var "guarded"))
      emptyResult
      (Lists.concat2 (var "missingChecks") (var "valueChecks"))

-- ============================================================================
-- Profile-aware orchestrator
-- ============================================================================

-- | Validate a property graph against a schema under the given
-- ValidationProfile. Walks vertices first, then edges, lifting each
-- per-element finding into 'InvalidGraphError' via the
-- 'vertex' / 'edge' wrappers. The lift is unconditional (the wrappers
-- are not per-rule checks); the per-rule gating happens inside
-- 'validateVertex' and 'validateEdge'.
--
-- The accumulator is threaded across the two phases (vertices, then
-- edges) so 'maxErrors' is enforced over the entire graph, not per
-- phase.
validateGraph :: TTermDefinition (
     ValidationProfile
  -> ValidationResult (InvalidGraphError v)
  -> (t -> v -> Maybe InvalidValueError)
  -> PG.GraphSchema t
  -> PG.Graph v
  -> ValidationResult (InvalidGraphError v))
validateGraph = validationDefinition "validateGraph" $
  doc "Validate a property graph against a GraphSchema under the given ValidationProfile, threading a ValidationResult InvalidGraphError accumulator. Errors hard-stop traversal once maxErrors is reached." $
  "p" ~> "acc0" ~> "checkValue" ~> "schema" ~> "graph" ~>
  lets [
    -- labelForVertexId: caller-side function used by validateEdge for
    -- in/out vertex existence/label checks. Lifted into Just so the
    -- profile is the only thing controlling whether those rules fire.
    "labelForVertexId">: just $ "i" ~>
      Maybes.map (project _Vertex _Vertex_label) (Maps.lookup (var "i") (project _Graph _Graph_vertices @@ var "graph")),
    -- vertexFindings: for each vertex, run validateVertex and lift each
    -- per-vertex finding into InvalidGraphError via the wrapper. The
    -- lift is unconditional — it does not go through the per-rule
    -- guard machinery; the gating already happened inside
    -- validateVertex.
    "vertexFindings">: Lists.bind
      (Maps.elems $ project _Graph _Graph_vertices @@ var "graph")
      ("el" ~> lets [
        -- For each vertex, look up its expected type. If the vertex's
        -- label is not in the schema, synthesize a one-finding
        -- ValidationResult so the orchestrator can lift it as if it
        -- came from validateVertex.
        "tOpt">: Maps.lookup
          (project _Vertex _Vertex_label @@ var "el")
          (project _GraphSchema _GraphSchema_vertices @@ var "schema"),
        "perVertex">: Maybes.maybe
          (Validation.validationResult
            (list [
              inject _InvalidVertexError _InvalidVertexError_label $
                record _NoSuchVertexLabelError [
                  _NoSuchVertexLabelError_label>>:
                    project _Vertex _Vertex_label @@ var "el"]])
            (list ([] :: [TTerm InvalidVertexError])))
          ("t" ~> validateVertex @@ var "p" @@ var "checkValue" @@ var "t" @@ var "el")
          (var "tOpt")]
        $ Lists.map
            ("ve" ~> inject _InvalidGraphError _InvalidGraphError_vertex $
              record _InvalidGraphVertexError [
                _InvalidGraphVertexError_id>>: project _Vertex _Vertex_id @@ var "el",
                _InvalidGraphVertexError_error>>: var "ve"])
            (Validation.validationResultErrors $ var "perVertex")),
    -- edgeFindings: same shape for edges.
    "edgeFindings">: Lists.bind
      (Maps.elems $ project _Graph _Graph_edges @@ var "graph")
      ("el" ~> lets [
        "tOpt">: Maps.lookup
          (project _Edge _Edge_label @@ var "el")
          (project _GraphSchema _GraphSchema_edges @@ var "schema"),
        "perEdge">: Maybes.maybe
          (Validation.validationResult
            (list [
              inject _InvalidEdgeError _InvalidEdgeError_label $
                record _NoSuchEdgeLabelError [
                  _NoSuchEdgeLabelError_label>>:
                    project _Edge _Edge_label @@ var "el"]])
            (list ([] :: [TTerm InvalidEdgeError])))
          ("t" ~> validateEdge @@ var "p" @@ var "checkValue" @@ var "labelForVertexId" @@ var "t" @@ var "el")
          (var "tOpt")]
        $ Lists.map
            ("ee" ~> inject _InvalidGraphError _InvalidGraphError_edge $
              record _InvalidGraphEdgeError [
                _InvalidGraphEdgeError_id>>: project _Edge _Edge_id @@ var "el",
                _InvalidGraphEdgeError_error>>: var "ee"])
            (Validation.validationResultErrors $ var "perEdge"))]
    -- Concat vertex+edge findings into the accumulator's errors list,
    -- bounded by maxErrors via Lists.take. Same lift-then-take pattern
    -- as 'package' in Validate/Packaging.hs. Warnings are not produced
    -- at the graph level (per-rule warnings live in the leaf
    -- validators' ValidationResults; we only lift errors here).
    $ lets [
      "liftedErrs">: Lists.concat2 (var "vertexFindings") (var "edgeFindings"),
      "newErrs">: Lists.take
        (Validation.validationProfileMaxErrors $ var "p")
        (Lists.concat2 (Validation.validationResultErrors $ var "acc0") (var "liftedErrs"))]
      $ Validation.validationResult
          (var "newErrs")
          (Validation.validationResultWarnings $ var "acc0")

