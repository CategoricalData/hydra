-- Note: this is an automatically generated file. Do not edit.
-- | Validation functions for Neo4j property graphs

module Hydra.Validate.Neo4j where
import qualified Hydra.Core as Core
import qualified Hydra.Error.Neo4j as Neo4j
import qualified Hydra.Overlay.Haskell.Lib.Equality as Equality
import qualified Hydra.Overlay.Haskell.Lib.Lists as Lists
import qualified Hydra.Overlay.Haskell.Lib.Logic as Logic
import qualified Hydra.Overlay.Haskell.Lib.Maps as Maps
import qualified Hydra.Overlay.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Overlay.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Overlay.Haskell.Lib.Sets as Sets
import qualified Hydra.Neo4j.Model as Model
import qualified Hydra.Validation as Validation
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Set as S
-- | All Neo4j validation rule names, as fully qualified 'hydra.error.neo4j.<UnionType>.<variant>' strings.
allNeo4jRuleNames :: [Core.Name]
allNeo4jRuleNames =
    [
      Core.Name "hydra.error.neo4j.InvalidNodeError.keyViolation",
      (Core.Name "hydra.error.neo4j.InvalidNodeError.missingImpliedLabel"),
      (Core.Name "hydra.error.neo4j.InvalidNodeError.missingProperty"),
      (Core.Name "hydra.error.neo4j.InvalidNodeError.noSuchLabel"),
      (Core.Name "hydra.error.neo4j.InvalidNodeError.uniquenessViolation"),
      (Core.Name "hydra.error.neo4j.InvalidNodeError.wrongPropertyType"),
      (Core.Name "hydra.error.neo4j.InvalidRelationshipError.endNodeNotFound"),
      (Core.Name "hydra.error.neo4j.InvalidRelationshipError.keyViolation"),
      (Core.Name "hydra.error.neo4j.InvalidRelationshipError.missingProperty"),
      (Core.Name "hydra.error.neo4j.InvalidRelationshipError.noMatchingPattern"),
      (Core.Name "hydra.error.neo4j.InvalidRelationshipError.noSuchType"),
      (Core.Name "hydra.error.neo4j.InvalidRelationshipError.startNodeNotFound"),
      (Core.Name "hydra.error.neo4j.InvalidRelationshipError.uniquenessViolation"),
      (Core.Name "hydra.error.neo4j.InvalidRelationshipError.wrongPropertyType")]
-- | Append a rule-tagged InvalidNodeError finding to a ValidationResult, classifying as error or warning per the profile and respecting maxErrors/maxWarnings bounds.
appendFindingNode :: Validation.ValidationProfile -> Validation.ValidationResult t0 -> Maybe (Core.Name, t0) -> Validation.ValidationResult t0
appendFindingNode p acc finding =
    Optionals.cases finding acc (\rp ->
      let ruleName = Pairs.first rp
          payload = Pairs.second rp
          errs = Validation.validationResultErrors acc
          wrns = Validation.validationResultWarnings acc
      in (Logic.ifElse (Sets.member ruleName (Validation.validationProfileErrorRules p)) (Logic.ifElse (Equality.lt (Lists.length errs) (Validation.validationProfileMaxErrors p)) (Validation.ValidationResult {
        Validation.validationResultErrors = (Lists.concat2 errs (Lists.singleton payload)),
        Validation.validationResultWarnings = wrns}) acc) (Logic.ifElse (Sets.member ruleName (Validation.validationProfileWarningRules p)) (Logic.ifElse (Equality.lt (Lists.length wrns) (Validation.validationProfileMaxWarnings p)) (Validation.ValidationResult {
        Validation.validationResultErrors = errs,
        Validation.validationResultWarnings = (Lists.concat2 wrns (Lists.singleton payload))}) acc) acc)))
-- | Append a rule-tagged InvalidRelationshipError finding to a ValidationResult.
appendFindingRelationship :: Validation.ValidationProfile -> Validation.ValidationResult t0 -> Maybe (Core.Name, t0) -> Validation.ValidationResult t0
appendFindingRelationship p acc finding =
    Optionals.cases finding acc (\rp ->
      let ruleName = Pairs.first rp
          payload = Pairs.second rp
          errs = Validation.validationResultErrors acc
          wrns = Validation.validationResultWarnings acc
      in (Logic.ifElse (Sets.member ruleName (Validation.validationProfileErrorRules p)) (Logic.ifElse (Equality.lt (Lists.length errs) (Validation.validationProfileMaxErrors p)) (Validation.ValidationResult {
        Validation.validationResultErrors = (Lists.concat2 errs (Lists.singleton payload)),
        Validation.validationResultWarnings = wrns}) acc) (Logic.ifElse (Sets.member ruleName (Validation.validationProfileWarningRules p)) (Logic.ifElse (Equality.lt (Lists.length wrns) (Validation.validationProfileMaxWarnings p)) (Validation.ValidationResult {
        Validation.validationResultErrors = errs,
        Validation.validationResultWarnings = (Lists.concat2 wrns (Lists.singleton payload))}) acc) acc)))
-- | The default (open-world) validation profile for Neo4j validation. Every rule except the closed-world noSuchLabel/noSuchType rules is classified as an error; no warnings; maxErrors=1, maxWarnings=20.
defaultNeo4jProfile :: Validation.ValidationProfile
defaultNeo4jProfile =
    Validation.ValidationProfile {
      Validation.validationProfileErrorRules = (Sets.fromList [
        Core.Name "hydra.error.neo4j.InvalidNodeError.keyViolation",
        (Core.Name "hydra.error.neo4j.InvalidNodeError.missingImpliedLabel"),
        (Core.Name "hydra.error.neo4j.InvalidNodeError.missingProperty"),
        (Core.Name "hydra.error.neo4j.InvalidNodeError.uniquenessViolation"),
        (Core.Name "hydra.error.neo4j.InvalidNodeError.wrongPropertyType"),
        (Core.Name "hydra.error.neo4j.InvalidRelationshipError.endNodeNotFound"),
        (Core.Name "hydra.error.neo4j.InvalidRelationshipError.keyViolation"),
        (Core.Name "hydra.error.neo4j.InvalidRelationshipError.missingProperty"),
        (Core.Name "hydra.error.neo4j.InvalidRelationshipError.noMatchingPattern"),
        (Core.Name "hydra.error.neo4j.InvalidRelationshipError.startNodeNotFound"),
        (Core.Name "hydra.error.neo4j.InvalidRelationshipError.uniquenessViolation"),
        (Core.Name "hydra.error.neo4j.InvalidRelationshipError.wrongPropertyType")]),
      Validation.validationProfileWarningRules = Sets.empty,
      Validation.validationProfileMaxErrors = 1,
      Validation.validationProfileMaxWarnings = 20}
-- | True iff the given rule name appears in the profile's errorRules or warningRules.
enabledNeo4j :: Validation.ValidationProfile -> Core.Name -> Bool
enabledNeo4j p ruleName =
    Logic.or (Sets.member ruleName (Validation.validationProfileErrorRules p)) (Sets.member ruleName (Validation.validationProfileWarningRules p))
-- | Given a list of nodes, return a function from element id to that node's labels (or nothing if no node has that id).
labelsForId :: [Model.Node] -> Model.ElementId -> Maybe (S.Set Model.NodeLabel)
labelsForId nodes eid =

      let m = Maps.fromList (Lists.map (\n -> (Model.unElementId (Model.nodeId n), (Model.nodeLabels n))) nodes)
      in (Maps.lookup (Model.unElementId eid) m)
-- | True iff the given Value has the kind required by the given ValueType.
matchesValueType :: Model.ValueType -> Model.Value -> Bool
matchesValueType vt v =
    case vt of
      Model.ValueTypeBoolean -> case v of
        Model.ValueBoolean _ -> True
        _ -> False
      Model.ValueTypeString -> case v of
        Model.ValueString _ -> True
        _ -> False
      Model.ValueTypeInteger -> case v of
        Model.ValueInteger _ -> True
        _ -> False
      Model.ValueTypeFloat -> case v of
        Model.ValueFloat _ -> True
        _ -> False
      Model.ValueTypeDate -> case v of
        Model.ValueDate _ -> True
        _ -> False
      Model.ValueTypeLocalTime -> case v of
        Model.ValueLocalTime _ -> True
        _ -> False
      Model.ValueTypeZonedTime -> case v of
        Model.ValueTime _ -> True
        _ -> False
      Model.ValueTypeLocalDateTime -> case v of
        Model.ValueLocalDateTime _ -> True
        _ -> False
      Model.ValueTypeZonedDateTime -> case v of
        Model.ValueDateTime _ -> True
        _ -> False
      Model.ValueTypeDuration -> case v of
        Model.ValueDuration _ -> True
        _ -> False
      Model.ValueTypePoint -> case v of
        Model.ValuePoint _ -> True
        _ -> False
      Model.ValueTypeList v0 -> case v of
        Model.ValueList v1 -> Lists.foldl (\acc -> \x -> Logic.and acc (matchesValueType v0 x)) True v1
        _ -> False
      Model.ValueTypeVector _ -> case v of
        Model.ValueList _ -> True
        _ -> False
      Model.ValueTypeUnion v0 -> Lists.foldl (\acc -> \m -> Logic.or acc (matchesValueType m v)) False v0
-- | The strict (closed-world) validation profile for Neo4j validation. Like the default, but a node or relationship that matches no element type is also an error.
strictNeo4jProfile :: Validation.ValidationProfile
strictNeo4jProfile =
    Validation.ValidationProfile {
      Validation.validationProfileErrorRules = (Sets.fromList [
        Core.Name "hydra.error.neo4j.InvalidNodeError.keyViolation",
        (Core.Name "hydra.error.neo4j.InvalidNodeError.missingImpliedLabel"),
        (Core.Name "hydra.error.neo4j.InvalidNodeError.missingProperty"),
        (Core.Name "hydra.error.neo4j.InvalidNodeError.noSuchLabel"),
        (Core.Name "hydra.error.neo4j.InvalidNodeError.uniquenessViolation"),
        (Core.Name "hydra.error.neo4j.InvalidNodeError.wrongPropertyType"),
        (Core.Name "hydra.error.neo4j.InvalidRelationshipError.endNodeNotFound"),
        (Core.Name "hydra.error.neo4j.InvalidRelationshipError.keyViolation"),
        (Core.Name "hydra.error.neo4j.InvalidRelationshipError.missingProperty"),
        (Core.Name "hydra.error.neo4j.InvalidRelationshipError.noMatchingPattern"),
        (Core.Name "hydra.error.neo4j.InvalidRelationshipError.noSuchType"),
        (Core.Name "hydra.error.neo4j.InvalidRelationshipError.startNodeNotFound"),
        (Core.Name "hydra.error.neo4j.InvalidRelationshipError.uniquenessViolation"),
        (Core.Name "hydra.error.neo4j.InvalidRelationshipError.wrongPropertyType")]),
      Validation.validationProfileWarningRules = Sets.empty,
      Validation.validationProfileMaxErrors = 1,
      Validation.validationProfileMaxWarnings = 20}
-- | Validate a Neo4j graph (nodes and relationships) against a GraphType under the given ValidationProfile, returning a ValidationResult InvalidGraphError. Each element is validated element-wise and its findings lifted into graph-level findings tagged with the element id. Cross-element uniqueness/key checks are not yet performed.
validateGraph :: Validation.ValidationProfile -> Model.GraphType -> [Model.Node] -> [Model.Relationship] -> Validation.ValidationResult Neo4j.InvalidGraphError
validateGraph p gt nodes rels =

      let resolver = Just (labelsForId nodes)
          nodeFindings =
                  Lists.bind nodes (\n -> Lists.map (\e -> Neo4j.InvalidGraphErrorNode (Neo4j.InvalidGraphNodeError {
                    Neo4j.invalidGraphNodeErrorId = (Model.nodeId n),
                    Neo4j.invalidGraphNodeErrorError = e})) (Validation.validationResultErrors (validateNode p gt n)))
          relFindings =
                  Lists.bind rels (\r -> Lists.map (\e -> Neo4j.InvalidGraphErrorRelationship (Neo4j.InvalidGraphRelationshipError {
                    Neo4j.invalidGraphRelationshipErrorId = (Model.relationshipId r),
                    Neo4j.invalidGraphRelationshipErrorError = e})) (Validation.validationResultErrors (validateRelationship p resolver gt r)))
          allErrs = Lists.take (Validation.validationProfileMaxErrors p) (Lists.concat2 nodeFindings relFindings)
      in Validation.ValidationResult {
        Validation.validationResultErrors = allErrs,
        Validation.validationResultWarnings = []}
-- | Validate a node against a GraphType under the given ValidationProfile, returning a ValidationResult InvalidNodeError. The node is checked against every node element type whose identifying label it carries; a node matching none is valid unless the closed-world noSuchLabel rule is enabled.
validateNode :: Validation.ValidationProfile -> Model.GraphType -> Model.Node -> Validation.ValidationResult Neo4j.InvalidNodeError
validateNode p gt el =

      let nodeLabels = Model.nodeLabels el
          props = Model.nodeProperties el
          matches =
                  Lists.filter (\net -> Sets.member (Model.nodeElementTypeIdentifyingLabel net) nodeLabels) (Model.graphTypeNodes gt)
          noMatchCheck =
                  Logic.ifElse (enabledNeo4j p (Core.Name "hydra.error.neo4j.InvalidNodeError.noSuchLabel")) (Optionals.map (\f -> (Core.Name "hydra.error.neo4j.InvalidNodeError.noSuchLabel", f)) (Logic.ifElse (Lists.null matches) (Just (Neo4j.InvalidNodeErrorNoSuchLabel (Neo4j.NoSuchLabelError {
                    Neo4j.noSuchLabelErrorLabels = (Sets.toList nodeLabels)}))) Nothing)) Nothing
          matchChecks =
                  Lists.bind matches (\net -> Lists.concat2 (Lists.map (\lab -> Logic.ifElse (enabledNeo4j p (Core.Name "hydra.error.neo4j.InvalidNodeError.missingImpliedLabel")) (Optionals.map (\f -> (Core.Name "hydra.error.neo4j.InvalidNodeError.missingImpliedLabel", f)) (Logic.ifElse (Sets.member lab nodeLabels) Nothing (Just (Neo4j.InvalidNodeErrorMissingImpliedLabel (Neo4j.MissingLabelError {
                    Neo4j.missingLabelErrorLabel = lab}))))) Nothing) (Sets.toList (Model.nodeElementTypeImpliedLabels net))) (Lists.bind (Model.nodeElementTypeConstraints net) (\cd -> case (Model.constraintDefinitionBody cd) of
                    Model.ConstraintPropertyExistence v0 -> [
                      Logic.ifElse (enabledNeo4j p (Core.Name "hydra.error.neo4j.InvalidNodeError.missingProperty")) (Optionals.map (\f -> (Core.Name "hydra.error.neo4j.InvalidNodeError.missingProperty", f)) (Optionals.cases (Maps.lookup (Model.propertyExistenceConstraintProperty v0) props) (Just (Neo4j.InvalidNodeErrorMissingProperty (Neo4j.PropertyExistenceError {
                        Neo4j.propertyExistenceErrorKey = (Model.propertyExistenceConstraintProperty v0)}))) (\_ -> Nothing))) Nothing]
                    Model.ConstraintKey v0 -> Lists.map (\k -> Logic.ifElse (enabledNeo4j p (Core.Name "hydra.error.neo4j.InvalidNodeError.missingProperty")) (Optionals.map (\f -> (Core.Name "hydra.error.neo4j.InvalidNodeError.missingProperty", f)) (Optionals.cases (Maps.lookup k props) (Just (Neo4j.InvalidNodeErrorMissingProperty (Neo4j.PropertyExistenceError {
                      Neo4j.propertyExistenceErrorKey = k}))) (\_ -> Nothing))) Nothing) (Model.keyConstraintProperties v0)
                    Model.ConstraintPropertyType v0 -> [
                      Logic.ifElse (enabledNeo4j p (Core.Name "hydra.error.neo4j.InvalidNodeError.wrongPropertyType")) (Optionals.map (\f -> (Core.Name "hydra.error.neo4j.InvalidNodeError.wrongPropertyType", f)) (Optionals.cases (Maps.lookup (Model.propertyTypeConstraintProperty v0) props) Nothing (\val -> Logic.ifElse (matchesValueType (Model.propertyTypeConstraintType v0) val) Nothing (Just (Neo4j.InvalidNodeErrorWrongPropertyType (Neo4j.PropertyTypeError {
                        Neo4j.propertyTypeErrorKey = (Model.propertyTypeConstraintProperty v0),
                        Neo4j.propertyTypeErrorExpectedType = (Model.propertyTypeConstraintType v0),
                        Neo4j.propertyTypeErrorValue = val})))))) Nothing]
                    Model.ConstraintPropertyUniqueness _ -> [])))
      in (Lists.foldl (\acc -> \guarded -> Logic.ifElse (Equality.gte (Lists.length (Validation.validationResultErrors acc)) (Validation.validationProfileMaxErrors p)) acc (appendFindingNode p acc guarded)) (Validation.ValidationResult {
        Validation.validationResultErrors = [],
        Validation.validationResultWarnings = []}) (Lists.cons noMatchCheck matchChecks))
-- | Validate a relationship against a GraphType under the given ValidationProfile, returning a ValidationResult InvalidRelationshipError. Endpoint labels select the applicable (possibly overloaded) relationship element type; property constraints of the matched type are enforced.
validateRelationship :: Validation.ValidationProfile -> Maybe (Model.ElementId -> Maybe (S.Set Model.NodeLabel)) -> Model.GraphType -> Model.Relationship -> Validation.ValidationResult Neo4j.InvalidRelationshipError
validateRelationship p labelsForId gt rel =

      let relType = Model.relationshipType rel
          props = Model.relationshipProperties rel
          startLabels = Optionals.cases labelsForId Nothing (\f -> f (Model.relationshipStart rel))
          endLabels = Optionals.cases labelsForId Nothing (\f -> f (Model.relationshipEnd rel))
          candidates =
                  Lists.filter (\ret -> Equality.equal (Model.unRelationshipType (Model.relationshipElementTypeType ret)) (Model.unRelationshipType relType)) (Model.graphTypeRelationships gt)
          matched =
                  Lists.filter (\ret -> Logic.and (Optionals.cases startLabels True (\ls -> Sets.member (Model.relationshipElementTypeStartLabel ret) ls)) (Optionals.cases endLabels True (\ls -> Sets.member (Model.relationshipElementTypeEndLabel ret) ls))) candidates
          noSuchTypeCheck =
                  Logic.ifElse (enabledNeo4j p (Core.Name "hydra.error.neo4j.InvalidRelationshipError.noSuchType")) (Optionals.map (\f -> (Core.Name "hydra.error.neo4j.InvalidRelationshipError.noSuchType", f)) (Logic.ifElse (Lists.null candidates) (Just (Neo4j.InvalidRelationshipErrorNoSuchType (Neo4j.NoSuchRelationshipTypeError {
                    Neo4j.noSuchRelationshipTypeErrorType = relType}))) Nothing)) Nothing
          noPatternCheck =
                  Logic.ifElse (enabledNeo4j p (Core.Name "hydra.error.neo4j.InvalidRelationshipError.noMatchingPattern")) (Optionals.map (\f -> (Core.Name "hydra.error.neo4j.InvalidRelationshipError.noMatchingPattern", f)) (Logic.ifElse (Logic.and (Logic.not (Lists.null candidates)) (Logic.and (Logic.and (Optionals.isGiven startLabels) (Optionals.isGiven endLabels)) (Lists.null matched))) (Just (Neo4j.InvalidRelationshipErrorNoMatchingPattern (Neo4j.NoMatchingPatternError {
                    Neo4j.noMatchingPatternErrorAllowedPatterns = (Lists.map (\ret -> Neo4j.RelationshipPattern {
                      Neo4j.relationshipPatternStartLabel = (Model.relationshipElementTypeStartLabel ret),
                      Neo4j.relationshipPatternEndLabel = (Model.relationshipElementTypeEndLabel ret)}) candidates),
                    Neo4j.noMatchingPatternErrorActualStartLabels = (Optionals.cases startLabels [] (\ls -> Sets.toList ls)),
                    Neo4j.noMatchingPatternErrorActualEndLabels = (Optionals.cases endLabels [] (\ls -> Sets.toList ls))}))) Nothing)) Nothing
          propChecks =
                  Lists.bind matched (\ret -> Lists.bind (Model.relationshipElementTypeConstraints ret) (\cd -> case (Model.constraintDefinitionBody cd) of
                    Model.ConstraintPropertyExistence v0 -> [
                      Logic.ifElse (enabledNeo4j p (Core.Name "hydra.error.neo4j.InvalidRelationshipError.missingProperty")) (Optionals.map (\f -> (Core.Name "hydra.error.neo4j.InvalidRelationshipError.missingProperty", f)) (Optionals.cases (Maps.lookup (Model.propertyExistenceConstraintProperty v0) props) (Just (Neo4j.InvalidRelationshipErrorMissingProperty (Neo4j.PropertyExistenceError {
                        Neo4j.propertyExistenceErrorKey = (Model.propertyExistenceConstraintProperty v0)}))) (\_ -> Nothing))) Nothing]
                    Model.ConstraintKey v0 -> Lists.map (\k -> Logic.ifElse (enabledNeo4j p (Core.Name "hydra.error.neo4j.InvalidRelationshipError.missingProperty")) (Optionals.map (\f -> (Core.Name "hydra.error.neo4j.InvalidRelationshipError.missingProperty", f)) (Optionals.cases (Maps.lookup k props) (Just (Neo4j.InvalidRelationshipErrorMissingProperty (Neo4j.PropertyExistenceError {
                      Neo4j.propertyExistenceErrorKey = k}))) (\_ -> Nothing))) Nothing) (Model.keyConstraintProperties v0)
                    Model.ConstraintPropertyType v0 -> [
                      Logic.ifElse (enabledNeo4j p (Core.Name "hydra.error.neo4j.InvalidRelationshipError.wrongPropertyType")) (Optionals.map (\f -> (Core.Name "hydra.error.neo4j.InvalidRelationshipError.wrongPropertyType", f)) (Optionals.cases (Maps.lookup (Model.propertyTypeConstraintProperty v0) props) Nothing (\val -> Logic.ifElse (matchesValueType (Model.propertyTypeConstraintType v0) val) Nothing (Just (Neo4j.InvalidRelationshipErrorWrongPropertyType (Neo4j.PropertyTypeError {
                        Neo4j.propertyTypeErrorKey = (Model.propertyTypeConstraintProperty v0),
                        Neo4j.propertyTypeErrorExpectedType = (Model.propertyTypeConstraintType v0),
                        Neo4j.propertyTypeErrorValue = val})))))) Nothing]
                    Model.ConstraintPropertyUniqueness _ -> []))
      in (Lists.foldl (\acc -> \guarded -> Logic.ifElse (Equality.gte (Lists.length (Validation.validationResultErrors acc)) (Validation.validationProfileMaxErrors p)) acc (appendFindingRelationship p acc guarded)) (Validation.ValidationResult {
        Validation.validationResultErrors = [],
        Validation.validationResultWarnings = []}) (Lists.cons noSuchTypeCheck (Lists.cons noPatternCheck propChecks)))
