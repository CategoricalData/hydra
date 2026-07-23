{-# LANGUAGE ScopedTypeVariables #-}
module Hydra.Sources.Validate.Neo4j where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (
  Node(..), _Node, Relationship(..), _Relationship, Path(..), _Path,
  Element(..), _Element)
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import qualified Hydra.Dsl.Lib.Strings                as Strings
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms                   as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core                       as Core
import qualified Hydra.Dsl.Lib.Equality               as Equality
import qualified Hydra.Dsl.Lib.Ordering as Ordering
import qualified Hydra.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Lib.Optionals                 as Optionals
import qualified Hydra.Dsl.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Validation                      as Validation
import           Prelude hiding ((++))
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import Hydra.Neo4j.Model as N4
import Hydra.Error.Neo4j as Err
import qualified Hydra.Sources.Neo4j.Model as Neo4jModel
import qualified Hydra.Sources.Error.Neo4j as ErrorNeo4j


module_ :: Module
module_ = Module {
            moduleName = (ModuleName "hydra.validate.neo4j"),
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [Neo4jModel.ns, ErrorNeo4j.ns, ModuleName "hydra.validation", ModuleName "hydra.core"],
            moduleMetadata = descriptionMetadata (Just "Validation functions for Neo4j property graphs")}
  where
   definitions = [
     toDefinition allNeo4jRuleNamesDef,
     toDefinition appendFindingNode,
     toDefinition appendFindingRelationship,
     toDefinition defaultNeo4jProfile,
     toDefinition enabledNeo4j,
     toDefinition labelsForId,
     toDefinition matchesValueType,
     toDefinition strictNeo4jProfile,
     toDefinition validateGraph,
     toDefinition validateNode,
     toDefinition validateRelationship]

-- ============================================================================
-- Rule names (host-side)
-- ============================================================================

-- | The full set of Neo4j validation rule names. Single source of truth for
-- the profiles and the guarded* helpers. Each name is
-- 'hydra.error.neo4j.<UnionType>.<variant>'. The 'noSuchLabel' / 'noSuchType'
-- rules implement closed-world validation (a node/relationship that matches no
-- element type is an error); they are excluded from the default (open-world)
-- profile. The uniqueness/key rules are produced only by graph-level
-- validation, not the element-level checks here.
allNeo4jRuleNames :: [Name]
allNeo4jRuleNames = L.concat
  [ fmap (qualifiedRule _InvalidNodeError)
      [ _InvalidNodeError_keyViolation
      , _InvalidNodeError_missingImpliedLabel
      , _InvalidNodeError_missingProperty
      , _InvalidNodeError_noSuchLabel
      , _InvalidNodeError_uniquenessViolation
      , _InvalidNodeError_wrongPropertyType]
  , fmap (qualifiedRule _InvalidRelationshipError)
      [ _InvalidRelationshipError_endNodeNotFound
      , _InvalidRelationshipError_keyViolation
      , _InvalidRelationshipError_missingProperty
      , _InvalidRelationshipError_noMatchingPattern
      , _InvalidRelationshipError_noSuchType
      , _InvalidRelationshipError_startNodeNotFound
      , _InvalidRelationshipError_uniquenessViolation
      , _InvalidRelationshipError_wrongPropertyType]]

-- | The closed-world rules: those that flag an element matching no element
-- type. Excluded from the open-world default profile.
closedWorldRuleNames :: [Name]
closedWorldRuleNames =
  [ qualifiedRule _InvalidNodeError _InvalidNodeError_noSuchLabel
  , qualifiedRule _InvalidRelationshipError _InvalidRelationshipError_noSuchType]

-- ============================================================================
-- Host-side helpers (not DSL definitions)
-- ============================================================================

-- | Compose a fully qualified rule identifier from a union-type qualified
-- name and a variant local name, joined with '.'.
qualifiedRule :: Name -> Name -> Name
qualifiedRule (Name u) (Name v) = Name (L.concat [u, ".", v])

-- | An empty ValidationResult parameterized by 'e'.
emptyResult :: TypedTerm (ValidationResult e)
emptyResult = Validation.validationResult
  (list ([] :: [TypedTerm e]))
  (list ([] :: [TypedTerm e]))

-- | Wrap a leaf-shaped 'Maybe InvalidNodeError' finding with the rule that
-- produced it, gated by the active profile.
guardedNodeRule
  :: TypedTerm ValidationProfile
  -> Name
  -> Name
  -> TypedTerm (Maybe InvalidNodeError)
  -> TypedTerm (Maybe (Name, InvalidNodeError))
guardedNodeRule profile unionName variantName findingExpr =
  Logic.ifElse (enabledNeo4j @@ profile @@ ruleNameTerm)
    (Optionals.map ("f" ~> pair ruleNameTerm (var "f")) findingExpr)
    nothing
  where
    ruleNameTerm = nameLift (qualifiedRule unionName variantName)

-- | Relationship counterpart of 'guardedNodeRule'.
guardedRelationshipRule
  :: TypedTerm ValidationProfile
  -> Name
  -> Name
  -> TypedTerm (Maybe InvalidRelationshipError)
  -> TypedTerm (Maybe (Name, InvalidRelationshipError))
guardedRelationshipRule profile unionName variantName findingExpr =
  Logic.ifElse (enabledNeo4j @@ profile @@ ruleNameTerm)
    (Optionals.map ("f" ~> pair ruleNameTerm (var "f")) findingExpr)
    nothing
  where
    ruleNameTerm = nameLift (qualifiedRule unionName variantName)

validationDefinition :: String -> TypedTerm a -> TypedTermDefinition a
validationDefinition = definitionInModule module_

-- ============================================================================
-- DSL definitions
-- ============================================================================

-- | A no-op DSL definition exposing the full rule-name set.
allNeo4jRuleNamesDef :: TypedTermDefinition [Name]
allNeo4jRuleNamesDef = validationDefinition "allNeo4jRuleNames" $
  doc "All Neo4j validation rule names, as fully qualified 'hydra.error.neo4j.<UnionType>.<variant>' strings." $
  list (nameLift <$> allNeo4jRuleNames)

-- | Classify a rule-tagged node finding against the profile and append.
appendFindingNode :: TypedTermDefinition (
  ValidationProfile
  -> ValidationResult InvalidNodeError
  -> Maybe (Name, InvalidNodeError)
  -> ValidationResult InvalidNodeError)
appendFindingNode = validationDefinition "appendFindingNode" $
  doc "Append a rule-tagged InvalidNodeError finding to a ValidationResult, classifying as error or warning per the profile and respecting maxErrors/maxWarnings bounds." $
  "p" ~> "acc" ~> "finding" ~>
  Optionals.cases (var "finding")
    (var "acc")
    ("rp" ~>
      "ruleName" <~ Pairs.first (var "rp") $
      "payload" <~ Pairs.second (var "rp") $
      "errs" <~ Validation.validationResultErrors (var "acc") $
      "wrns" <~ Validation.validationResultWarnings (var "acc") $
      Logic.ifElse (Sets.member (var "ruleName") (Validation.validationProfileErrorRules $ var "p"))
        (Logic.ifElse (Ordering.lt (Lists.length $ var "errs") (Validation.validationProfileMaxErrors $ var "p"))
          (Validation.validationResult
            (Lists.concat2 (var "errs") (Lists.singleton $ var "payload"))
            (var "wrns"))
          (var "acc"))
        (Logic.ifElse (Sets.member (var "ruleName") (Validation.validationProfileWarningRules $ var "p"))
          (Logic.ifElse (Ordering.lt (Lists.length $ var "wrns") (Validation.validationProfileMaxWarnings $ var "p"))
            (Validation.validationResult
              (var "errs")
              (Lists.concat2 (var "wrns") (Lists.singleton $ var "payload")))
            (var "acc"))
          (var "acc")))

-- | Relationship counterpart of 'appendFindingNode'.
appendFindingRelationship :: TypedTermDefinition (
  ValidationProfile
  -> ValidationResult InvalidRelationshipError
  -> Maybe (Name, InvalidRelationshipError)
  -> ValidationResult InvalidRelationshipError)
appendFindingRelationship = validationDefinition "appendFindingRelationship" $
  doc "Append a rule-tagged InvalidRelationshipError finding to a ValidationResult." $
  "p" ~> "acc" ~> "finding" ~>
  Optionals.cases (var "finding")
    (var "acc")
    ("rp" ~>
      "ruleName" <~ Pairs.first (var "rp") $
      "payload" <~ Pairs.second (var "rp") $
      "errs" <~ Validation.validationResultErrors (var "acc") $
      "wrns" <~ Validation.validationResultWarnings (var "acc") $
      Logic.ifElse (Sets.member (var "ruleName") (Validation.validationProfileErrorRules $ var "p"))
        (Logic.ifElse (Ordering.lt (Lists.length $ var "errs") (Validation.validationProfileMaxErrors $ var "p"))
          (Validation.validationResult
            (Lists.concat2 (var "errs") (Lists.singleton $ var "payload"))
            (var "wrns"))
          (var "acc"))
        (Logic.ifElse (Sets.member (var "ruleName") (Validation.validationProfileWarningRules $ var "p"))
          (Logic.ifElse (Ordering.lt (Lists.length $ var "wrns") (Validation.validationProfileMaxWarnings $ var "p"))
            (Validation.validationResult
              (var "errs")
              (Lists.concat2 (var "wrns") (Lists.singleton $ var "payload")))
            (var "acc"))
          (var "acc")))

-- | The default validation profile: open-world (no closed-world rules); every
-- other rule classified as an error; no warnings; maxErrors=1; maxWarnings=20.
defaultNeo4jProfile :: TypedTermDefinition ValidationProfile
defaultNeo4jProfile = validationDefinition "defaultNeo4jProfile" $
  doc "The default (open-world) validation profile for Neo4j validation. Every rule except the closed-world noSuchLabel/noSuchType rules is classified as an error; no warnings; maxErrors=1, maxWarnings=20." $
  Validation.validationProfile
    (Sets.fromList $ list $ nameLift <$> L.filter (\n -> not (n `L.elem` closedWorldRuleNames)) allNeo4jRuleNames)
    Sets.empty
    (int32 1)
    (int32 20)

-- | True iff the given rule name appears in the profile's errorRules or warningRules.
enabledNeo4j :: TypedTermDefinition (ValidationProfile -> Name -> Bool)
enabledNeo4j = validationDefinition "enabledNeo4j" $
  doc "True iff the given rule name appears in the profile's errorRules or warningRules." $
  "p" ~> "ruleName" ~>
  Logic.or
    (Sets.member (var "ruleName") (Validation.validationProfileErrorRules $ var "p"))
    (Sets.member (var "ruleName") (Validation.validationProfileWarningRules $ var "p"))

-- | Build an element-id -> labels resolver from a list of nodes, as a map
-- lookup. Used by 'validateGraph' to resolve relationship endpoints.
labelsForId :: TypedTermDefinition ([N4.Node] -> ElementId -> Maybe (S.Set NodeLabel))
labelsForId = validationDefinition "labelsForId" $
  doc "Given a list of nodes, return a function from element id to that node's labels (or nothing if no node has that id)." $
  "nodes" ~>
  lets [
    "m">: (Maps.fromList (Lists.map
      ("n" ~> pair
        (unwrap _ElementId @@ (project _Node _Node_id @@ var "n"))
        (project _Node _Node_labels @@ var "n"))
      (var "nodes")) :: TypedTerm (M.Map String (S.Set NodeLabel)))]
  $ "eid" ~> Maps.lookup (unwrap _ElementId @@ var "eid" :: TypedTerm String) (var "m")

-- | Validate a whole Neo4j graph (a list of nodes and a list of relationships)
-- against a graph type, returning a ValidationResult of InvalidGraphError.
-- Each node and relationship is validated element-wise (see 'validateNode' /
-- 'validateRelationship'); each finding is lifted into an InvalidGraphError
-- tagged with the offending element's id. Endpoint labels for relationship
-- checks are resolved from the node list. All findings are collected, bounded
-- by the profile's maxErrors.
--
-- This validator is intentionally **pure and client-side**: it checks only what
-- is determinable from the data it is handed (the elements themselves, plus the
-- in-memory node list for endpoint labels). It performs no effects and never
-- queries a database.
--
-- PLACEHOLDER (future work): the uniqueness and key-uniqueness constraints
-- (whose error variants 'InvalidNodeError.uniquenessViolation' /
-- 'InvalidRelationshipError.uniquenessViolation' / '...keyViolation' already
-- exist) are NOT checked here. They compare property values across elements, so
-- checking them needs either the whole graph in memory or — for incremental
-- validation against a live database — a query back to the server ("does any
-- other element already have this value?"). The latter requires effects, which
-- this pure validator deliberately does not have. An effectful, server-querying
-- variant should be added in the future to produce those findings; until then
-- the variants remain defined but unproduced.
validateGraph :: TypedTermDefinition (
     ValidationProfile
  -> GraphType
  -> [N4.Node]
  -> [N4.Relationship]
  -> ValidationResult InvalidGraphError)
validateGraph = validationDefinition "validateGraph" $
  doc "Validate a Neo4j graph (nodes and relationships) against a GraphType under the given ValidationProfile, returning a ValidationResult InvalidGraphError. Each element is validated element-wise and its findings lifted into graph-level findings tagged with the element id. Cross-element uniqueness/key checks are not yet performed." $
  "p" ~> "gt" ~> "nodes" ~> "rels" ~>
  lets [
    "resolver">: just (labelsForId @@ var "nodes"),
    -- Per-node findings lifted into InvalidGraphError.node.
    "nodeFindings">: Lists.bind (var "nodes")
      ("n" ~> Lists.map
        ("e" ~> inject _InvalidGraphError _InvalidGraphError_node $
          record _InvalidGraphNodeError [
            _InvalidGraphNodeError_id>>: project _Node _Node_id @@ var "n",
            _InvalidGraphNodeError_error>>: var "e"])
        (Validation.validationResultErrors $ validateNode @@ var "p" @@ var "gt" @@ var "n")),
    -- Per-relationship findings lifted into InvalidGraphError.relationship.
    "relFindings">: Lists.bind (var "rels")
      ("r" ~> Lists.map
        ("e" ~> inject _InvalidGraphError _InvalidGraphError_relationship $
          record _InvalidGraphRelationshipError [
            _InvalidGraphRelationshipError_id>>: project _Relationship _Relationship_id @@ var "r",
            _InvalidGraphRelationshipError_error>>: var "e"])
        (Validation.validationResultErrors $ validateRelationship @@ var "p" @@ var "resolver" @@ var "gt" @@ var "r"))]
  $ lets [
    "allErrs">: Lists.take
      (Validation.validationProfileMaxErrors $ var "p")
      (Lists.concat2 (var "nodeFindings") (var "relFindings"))]
    $ Validation.validationResult
        (var "allErrs")
        (list ([] :: [TypedTerm InvalidGraphError]))

-- | Whether a Value matches a ValueType. For atomic types this is a simple
-- kind match; LIST matches a list whose elements all match; VECTOR matches a
-- list; closed UNION matches if the value matches any member type.
matchesValueType :: TypedTermDefinition (ValueType -> Value -> Bool)
matchesValueType = validationDefinition "matchesValueType" $
  doc "True iff the given Value has the kind required by the given ValueType." $
  "vt" ~> "v" ~>
  match _ValueType Nothing [
    _ValueType_boolean      >>: constant (isValueCase _Value_boolean (var "v")),
    _ValueType_string       >>: constant (isValueCase _Value_string (var "v")),
    _ValueType_integer      >>: constant (isValueCase _Value_integer (var "v")),
    _ValueType_float        >>: constant (isValueCase _Value_float (var "v")),
    _ValueType_date         >>: constant (isValueCase _Value_date (var "v")),
    _ValueType_localTime    >>: constant (isValueCase _Value_localTime (var "v")),
    _ValueType_zonedTime    >>: constant (isValueCase _Value_time (var "v")),
    _ValueType_localDateTime>>: constant (isValueCase _Value_localDateTime (var "v")),
    _ValueType_zonedDateTime>>: constant (isValueCase _Value_dateTime (var "v")),
    _ValueType_duration     >>: constant (isValueCase _Value_duration (var "v")),
    _ValueType_point        >>: constant (isValueCase _Value_point (var "v")),
    _ValueType_list         >>: ("inner" ~> matchesListOf (var "inner") (var "v")),
    _ValueType_vector       >>: constant (isValueCase _Value_list (var "v")),
    _ValueType_union        >>: ("members" ~> Lists.foldl
        ("acc" ~> "m" ~> Logic.or (var "acc") (matchesValueType @@ var "m" @@ var "v"))
        false
        (var "members"))]
    @@ var "vt"

-- | The strict (closed-world) validation profile: like the default, but the
-- closed-world rules (noSuchLabel/noSuchType) are also errors, so a node or
-- relationship that matches no element type is invalid.
strictNeo4jProfile :: TypedTermDefinition ValidationProfile
strictNeo4jProfile = validationDefinition "strictNeo4jProfile" $
  doc "The strict (closed-world) validation profile for Neo4j validation. Like the default, but a node or relationship that matches no element type is also an error." $
  Validation.validationProfile
    (Sets.fromList $ list $ nameLift <$> allNeo4jRuleNames)
    Sets.empty
    (int32 1)
    (int32 20)

-- | Validate a single node against a graph type. A node matches every node
-- element type whose identifying label is among the node's labels (a node
-- carries a label set, so it may match several); the matched element types'
-- constraints are all enforced. A node matching none is valid by default
-- (open-world); under a strict profile, the noSuchLabel rule makes it an error.
validateNode :: TypedTermDefinition (
     ValidationProfile
  -> GraphType
  -> N4.Node
  -> ValidationResult InvalidNodeError)
validateNode = validationDefinition "validateNode" $
  doc "Validate a node against a GraphType under the given ValidationProfile, returning a ValidationResult InvalidNodeError. The node is checked against every node element type whose identifying label it carries; a node matching none is valid unless the closed-world noSuchLabel rule is enabled." $
  "p" ~> "gt" ~> "el" ~>
  lets [
    "nodeLabels">: (project _Node _Node_labels @@ var "el" :: TypedTerm (S.Set NodeLabel)),
    "props">: (project _Node _Node_properties @@ var "el" :: TypedTerm (M.Map Key Value)),
    -- The node element types whose identifying label the node carries.
    "matches">: Lists.filter
      ("net" ~> Sets.member
        (project _NodeElementType _NodeElementType_identifyingLabel @@ var "net" :: TypedTerm NodeLabel)
        (var "nodeLabels"))
      (project _GraphType _GraphType_nodes @@ var "gt"),
    -- noSuchLabel finding (closed-world; only fires if the rule is enabled and
    -- there are no matches).
    "noMatchCheck">: guardedNodeRule (var "p") _InvalidNodeError _InvalidNodeError_noSuchLabel
      (Logic.ifElse (Lists.null (var "matches"))
        (just $ inject _InvalidNodeError _InvalidNodeError_noSuchLabel $
          record _NoSuchLabelError [
            _NoSuchLabelError_labels>>: Sets.toList (var "nodeLabels" :: TypedTerm (S.Set NodeLabel))])
        nothing),
    -- For each matched element type, its implied-label and property findings.
    "matchChecks">: Lists.bind (var "matches")
      ("net" ~> Lists.concat2
        (nodeImpliedLabelChecks (var "p") (var "nodeLabels") (var "net"))
        (Lists.bind (project _NodeElementType _NodeElementType_constraints @@ var "net")
          ("cd" ~> nodePropertyChecks (var "p") (var "props")
            (project _ConstraintDefinition _ConstraintDefinition_body @@ var "cd"))))]
  $ Lists.foldl
    ("acc" ~> "guarded" ~>
      Logic.ifElse
        (Ordering.gte
          (Lists.length $ Validation.validationResultErrors $ var "acc")
          (Validation.validationProfileMaxErrors $ var "p"))
        (var "acc")
        (appendFindingNode @@ var "p" @@ var "acc" @@ var "guarded"))
    emptyResult
    (Lists.cons (var "noMatchCheck") (var "matchChecks"))

-- | Validate a single relationship against a graph type. Candidate element
-- types are those whose relationship type equals the relationship's type;
-- because relationship types may be overloaded, the candidate whose start/end
-- labels match the relationship's endpoint nodes is selected (endpoints select
-- the applicable element type, not merely constrain it). A relationship whose
-- type has candidates but whose endpoints match no candidate's pattern is a
-- noMatchingPattern error. A relationship whose type has no candidates is valid
-- by default (open-world); under a strict profile, noSuchType makes it an
-- error. Endpoint resolution uses the supplied id->labels resolver; when it is
-- Nothing, endpoint-dependent checks are skipped.
validateRelationship :: TypedTermDefinition (
     ValidationProfile
  -> Y.Maybe (ElementId -> Y.Maybe (S.Set NodeLabel))
  -> GraphType
  -> N4.Relationship
  -> ValidationResult InvalidRelationshipError)
validateRelationship = validationDefinition "validateRelationship" $
  doc "Validate a relationship against a GraphType under the given ValidationProfile, returning a ValidationResult InvalidRelationshipError. Endpoint labels select the applicable (possibly overloaded) relationship element type; property constraints of the matched type are enforced." $
  "p" ~> "labelsForId" ~> "gt" ~> "rel" ~>
  lets [
    "relType">: (project _Relationship _Relationship_type @@ var "rel" :: TypedTerm RelationshipType),
    "props">: (project _Relationship _Relationship_properties @@ var "rel" :: TypedTerm (M.Map Key Value)),
    "startLabels">: resolveLabels (var "labelsForId") (project _Relationship _Relationship_start @@ var "rel"),
    "endLabels">: resolveLabels (var "labelsForId") (project _Relationship _Relationship_end @@ var "rel"),
    -- Candidate element types: same relationship type.
    "candidates">: Lists.filter
      ("ret" ~> Equality.equal
        (unwrap _RelationshipType @@ (project _RelationshipElementType _RelationshipElementType_type @@ var "ret"))
        (unwrap _RelationshipType @@ var "relType"))
      (project _GraphType _GraphType_relationships @@ var "gt"),
    -- Among candidates, those whose endpoints match. If endpoint labels are
    -- unknown (resolver returned Nothing), treat all candidates as matching so
    -- property checks still run.
    "matched">: Lists.filter
      ("ret" ~> endpointsMatch (var "startLabels") (var "endLabels") (var "ret"))
      (var "candidates"),
    -- noSuchType (closed-world): the relationship's type has no element type.
    "noSuchTypeCheck">: guardedRelationshipRule (var "p") _InvalidRelationshipError _InvalidRelationshipError_noSuchType
      (Logic.ifElse (Lists.null (var "candidates"))
        (just $ inject _InvalidRelationshipError _InvalidRelationshipError_noSuchType $
          record _NoSuchRelationshipTypeError [
            _NoSuchRelationshipTypeError_type>>: var "relType"])
        nothing),
    -- noMatchingPattern: the type has candidates, endpoints are known, but no
    -- candidate's pattern matches.
    "noPatternCheck">: guardedRelationshipRule (var "p") _InvalidRelationshipError _InvalidRelationshipError_noMatchingPattern
      (Logic.ifElse
        (Logic.and (Logic.not (Lists.null (var "candidates")))
          (Logic.and (endpointsKnown (var "startLabels") (var "endLabels")) (Lists.null (var "matched"))))
        (just $ inject _InvalidRelationshipError _InvalidRelationshipError_noMatchingPattern $
          record _NoMatchingPatternError [
            _NoMatchingPatternError_allowedPatterns>>: Lists.map
              ("ret" ~> record _RelationshipPattern [
                _RelationshipPattern_startLabel>>: project _RelationshipElementType _RelationshipElementType_startLabel @@ var "ret",
                _RelationshipPattern_endLabel>>: project _RelationshipElementType _RelationshipElementType_endLabel @@ var "ret"])
              (var "candidates"),
            _NoMatchingPatternError_actualStartLabels>>: optionalLabelsToList (var "startLabels"),
            _NoMatchingPatternError_actualEndLabels>>: optionalLabelsToList (var "endLabels")])
        nothing),
    -- Property findings for each matched element type.
    "propChecks">: Lists.bind (var "matched")
      ("ret" ~> Lists.bind (project _RelationshipElementType _RelationshipElementType_constraints @@ var "ret")
        ("cd" ~> relationshipPropertyChecks (var "p") (var "props")
          (project _ConstraintDefinition _ConstraintDefinition_body @@ var "cd")))]
  $ Lists.foldl
    ("acc" ~> "guarded" ~>
      Logic.ifElse
        (Ordering.gte
          (Lists.length $ Validation.validationResultErrors $ var "acc")
          (Validation.validationProfileMaxErrors $ var "p"))
        (var "acc")
        (appendFindingRelationship @@ var "p" @@ var "acc" @@ var "guarded"))
    emptyResult
    (Lists.cons (var "noSuchTypeCheck") (Lists.cons (var "noPatternCheck") (var "propChecks")))

-- ============================================================================
-- Host-side term helpers (inlined expression builders, not DSL definitions)
-- ============================================================================

-- | True iff the given Value is of the given variant (by case).
isValueCase :: Name -> TypedTerm Value -> TypedTerm Bool
isValueCase variantName v =
  match _Value (Just false) [variantName >>: constant true] @@ v

-- | True iff the Value is a list whose elements all match the inner ValueType.
matchesListOf :: TypedTerm ValueType -> TypedTerm Value -> TypedTerm Bool
matchesListOf inner v =
  match _Value (Just false) [
    _Value_list >>: ("xs" ~> Lists.foldl
        ("acc" ~> "x" ~> Logic.and (var "acc") (matchesValueType @@ inner @@ var "x"))
        true
        (var "xs"))]
    @@ v

-- | Resolve an element id to an optional label set via the optional resolver.
resolveLabels
  :: TypedTerm (Maybe (ElementId -> Maybe (S.Set NodeLabel)))
  -> TypedTerm ElementId
  -> TypedTerm (Maybe (S.Set NodeLabel))
resolveLabels resolver eid =
  Optionals.cases resolver
    nothing
    ("f" ~> var "f" @@ eid)

-- | True iff both endpoint label sets are known (the resolver resolved them).
endpointsKnown
  :: TypedTerm (Maybe (S.Set NodeLabel))
  -> TypedTerm (Maybe (S.Set NodeLabel))
  -> TypedTerm Bool
endpointsKnown startLabels endLabels =
  Logic.and (Optionals.isGiven startLabels) (Optionals.isGiven endLabels)

-- | Render an optional label set as a list (empty when unknown).
optionalLabelsToList
  :: TypedTerm (Maybe (S.Set NodeLabel))
  -> TypedTerm [NodeLabel]
optionalLabelsToList labels =
  Optionals.cases labels
    (list ([] :: [TypedTerm NodeLabel]))
    ("ls" ~> Sets.toList (var "ls" :: TypedTerm (S.Set NodeLabel)))

-- | Whether a relationship element type's endpoint pattern matches the given
-- (optional) endpoint label sets. When an endpoint's labels are unknown, that
-- side is treated as matching (so property checks still run).
endpointsMatch
  :: TypedTerm (Maybe (S.Set NodeLabel))
  -> TypedTerm (Maybe (S.Set NodeLabel))
  -> TypedTerm RelationshipElementType
  -> TypedTerm Bool
endpointsMatch startLabels endLabels ret =
  Logic.and
    (sideMatches startLabels (project _RelationshipElementType _RelationshipElementType_startLabel @@ ret))
    (sideMatches endLabels (project _RelationshipElementType _RelationshipElementType_endLabel @@ ret))
  where
    sideMatches labels required =
      Optionals.cases labels
        true
        ("ls" ~> Sets.member required (var "ls" :: TypedTerm (S.Set NodeLabel)))

-- | One guarded missingImpliedLabel finding per implied label not present on
-- the node.
nodeImpliedLabelChecks
  :: TypedTerm ValidationProfile
  -> TypedTerm (S.Set NodeLabel)
  -> TypedTerm NodeElementType
  -> TypedTerm [Maybe (Name, InvalidNodeError)]
nodeImpliedLabelChecks profile nodeLabels net =
  Lists.map
    ("lab" ~> guardedNodeRule profile _InvalidNodeError _InvalidNodeError_missingImpliedLabel
      (Logic.ifElse (Sets.member (var "lab" :: TypedTerm NodeLabel) nodeLabels)
        nothing
        (just $ inject _InvalidNodeError _InvalidNodeError_missingImpliedLabel $
          record _MissingLabelError [_MissingLabelError_label>>: var "lab"])))
    (Sets.toList (project _NodeElementType _NodeElementType_impliedLabels @@ net :: TypedTerm (S.Set NodeLabel)))

-- | A property existence finding (the property must be present), as a node
-- error if absent.
nodeExistenceFinding
  :: TypedTerm ValidationProfile
  -> TypedTerm (M.Map Key Value)
  -> TypedTerm Key
  -> TypedTerm (Maybe (Name, InvalidNodeError))
nodeExistenceFinding profile props k =
  guardedNodeRule profile _InvalidNodeError _InvalidNodeError_missingProperty
    (Optionals.cases (Maps.lookup k (props :: TypedTerm (M.Map Key Value)))
      (just $ inject _InvalidNodeError _InvalidNodeError_missingProperty $
        record _PropertyExistenceError [_PropertyExistenceError_key>>: k])
      (constant nothing))

-- | A property type finding for a node: fires if the property is present and
-- its value does not match the type.
nodeTypeFinding
  :: TypedTerm ValidationProfile
  -> TypedTerm (M.Map Key Value)
  -> TypedTerm Key
  -> TypedTerm ValueType
  -> TypedTerm (Maybe (Name, InvalidNodeError))
nodeTypeFinding profile props k vt =
  guardedNodeRule profile _InvalidNodeError _InvalidNodeError_wrongPropertyType
    (Optionals.cases (Maps.lookup k (props :: TypedTerm (M.Map Key Value)))
      nothing
      ("val" ~> Logic.ifElse (matchesValueType @@ vt @@ var "val")
        nothing
        (just $ inject _InvalidNodeError _InvalidNodeError_wrongPropertyType $
          record _PropertyTypeError [
            _PropertyTypeError_key>>: k,
            _PropertyTypeError_expectedType>>: vt,
            _PropertyTypeError_value>>: var "val"])))

-- | The node property findings for one constraint.
nodePropertyChecks
  :: TypedTerm ValidationProfile
  -> TypedTerm (M.Map Key Value)
  -> TypedTerm Constraint
  -> TypedTerm [Maybe (Name, InvalidNodeError)]
nodePropertyChecks profile props c =
  match _Constraint Nothing [
    _Constraint_propertyExistence >>: ("ec" ~> list [
      nodeExistenceFinding profile props (project _PropertyExistenceConstraint _PropertyExistenceConstraint_property @@ var "ec")]),
    _Constraint_key >>: ("kc" ~> Lists.map
      ("k" ~> nodeExistenceFinding profile props (var "k"))
      (project _KeyConstraint _KeyConstraint_properties @@ var "kc")),
    _Constraint_propertyType >>: ("tc" ~> list [
      nodeTypeFinding profile props
        (project _PropertyTypeConstraint _PropertyTypeConstraint_property @@ var "tc")
        (project _PropertyTypeConstraint _PropertyTypeConstraint_type @@ var "tc")]),
    _Constraint_propertyUniqueness >>: constant (list ([] :: [TypedTerm (Maybe (Name, InvalidNodeError))]))]
    @@ c

-- | Relationship counterpart of 'nodeExistenceFinding'.
relationshipExistenceFinding
  :: TypedTerm ValidationProfile
  -> TypedTerm (M.Map Key Value)
  -> TypedTerm Key
  -> TypedTerm (Maybe (Name, InvalidRelationshipError))
relationshipExistenceFinding profile props k =
  guardedRelationshipRule profile _InvalidRelationshipError _InvalidRelationshipError_missingProperty
    (Optionals.cases (Maps.lookup k (props :: TypedTerm (M.Map Key Value)))
      (just $ inject _InvalidRelationshipError _InvalidRelationshipError_missingProperty $
        record _PropertyExistenceError [_PropertyExistenceError_key>>: k])
      (constant nothing))

-- | Relationship counterpart of 'nodeTypeFinding'.
relationshipTypeFinding
  :: TypedTerm ValidationProfile
  -> TypedTerm (M.Map Key Value)
  -> TypedTerm Key
  -> TypedTerm ValueType
  -> TypedTerm (Maybe (Name, InvalidRelationshipError))
relationshipTypeFinding profile props k vt =
  guardedRelationshipRule profile _InvalidRelationshipError _InvalidRelationshipError_wrongPropertyType
    (Optionals.cases (Maps.lookup k (props :: TypedTerm (M.Map Key Value)))
      nothing
      ("val" ~> Logic.ifElse (matchesValueType @@ vt @@ var "val")
        nothing
        (just $ inject _InvalidRelationshipError _InvalidRelationshipError_wrongPropertyType $
          record _PropertyTypeError [
            _PropertyTypeError_key>>: k,
            _PropertyTypeError_expectedType>>: vt,
            _PropertyTypeError_value>>: var "val"])))

-- | Relationship counterpart of 'nodePropertyChecks'.
relationshipPropertyChecks
  :: TypedTerm ValidationProfile
  -> TypedTerm (M.Map Key Value)
  -> TypedTerm Constraint
  -> TypedTerm [Maybe (Name, InvalidRelationshipError)]
relationshipPropertyChecks profile props c =
  match _Constraint Nothing [
    _Constraint_propertyExistence >>: ("ec" ~> list [
      relationshipExistenceFinding profile props (project _PropertyExistenceConstraint _PropertyExistenceConstraint_property @@ var "ec")]),
    _Constraint_key >>: ("kc" ~> Lists.map
      ("k" ~> relationshipExistenceFinding profile props (var "k"))
      (project _KeyConstraint _KeyConstraint_properties @@ var "kc")),
    _Constraint_propertyType >>: ("tc" ~> list [
      relationshipTypeFinding profile props
        (project _PropertyTypeConstraint _PropertyTypeConstraint_property @@ var "tc")
        (project _PropertyTypeConstraint _PropertyTypeConstraint_type @@ var "tc")]),
    _Constraint_propertyUniqueness >>: constant (list ([] :: [TypedTerm (Maybe (Name, InvalidRelationshipError))]))]
    @@ c
