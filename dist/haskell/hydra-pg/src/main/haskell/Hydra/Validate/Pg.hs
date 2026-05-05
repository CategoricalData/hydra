-- Note: this is an automatically generated file. Do not edit.
-- | Validation functions for property graphs

module Hydra.Validate.Pg where
import qualified Hydra.Core as Core
import qualified Hydra.Error.Pg as Pg
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Pg.Model as Model
import qualified Hydra.Validation as Validation
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | Append a rule-tagged InvalidEdgeError finding to a ValidationResult.
appendFindingEdge :: Validation.ValidationProfile -> Validation.ValidationResult t0 -> Maybe (Core.Name, t0) -> Validation.ValidationResult t0
appendFindingEdge p acc finding =
    Maybes.cases finding acc (\rp ->
      let ruleName = Pairs.first rp
          payload = Pairs.second rp
          errs = Validation.validationResultErrors acc
          wrns = Validation.validationResultWarnings acc
      in (Logic.ifElse (Sets.member ruleName (Validation.validationProfileErrorRules p)) (Logic.ifElse (Equality.lt (Lists.length errs) (Validation.validationProfileMaxErrors p)) (Validation.ValidationResult {
        Validation.validationResultErrors = (Lists.concat2 errs (Lists.singleton payload)),
        Validation.validationResultWarnings = wrns}) acc) (Logic.ifElse (Sets.member ruleName (Validation.validationProfileWarningRules p)) (Logic.ifElse (Equality.lt (Lists.length wrns) (Validation.validationProfileMaxWarnings p)) (Validation.ValidationResult {
        Validation.validationResultErrors = errs,
        Validation.validationResultWarnings = (Lists.concat2 wrns (Lists.singleton payload))}) acc) acc)))
-- | Append a rule-tagged InvalidGraphError finding to a ValidationResult.
appendFindingGraph :: Validation.ValidationProfile -> Validation.ValidationResult t0 -> Maybe (Core.Name, t0) -> Validation.ValidationResult t0
appendFindingGraph p acc finding =
    Maybes.cases finding acc (\rp ->
      let ruleName = Pairs.first rp
          payload = Pairs.second rp
          errs = Validation.validationResultErrors acc
          wrns = Validation.validationResultWarnings acc
      in (Logic.ifElse (Sets.member ruleName (Validation.validationProfileErrorRules p)) (Logic.ifElse (Equality.lt (Lists.length errs) (Validation.validationProfileMaxErrors p)) (Validation.ValidationResult {
        Validation.validationResultErrors = (Lists.concat2 errs (Lists.singleton payload)),
        Validation.validationResultWarnings = wrns}) acc) (Logic.ifElse (Sets.member ruleName (Validation.validationProfileWarningRules p)) (Logic.ifElse (Equality.lt (Lists.length wrns) (Validation.validationProfileMaxWarnings p)) (Validation.ValidationResult {
        Validation.validationResultErrors = errs,
        Validation.validationResultWarnings = (Lists.concat2 wrns (Lists.singleton payload))}) acc) acc)))
-- | Append a rule-tagged InvalidElementPropertyError finding to a ValidationResult.
appendFindingProperty :: Validation.ValidationProfile -> Validation.ValidationResult t0 -> Maybe (Core.Name, t0) -> Validation.ValidationResult t0
appendFindingProperty p acc finding =
    Maybes.cases finding acc (\rp ->
      let ruleName = Pairs.first rp
          payload = Pairs.second rp
          errs = Validation.validationResultErrors acc
          wrns = Validation.validationResultWarnings acc
      in (Logic.ifElse (Sets.member ruleName (Validation.validationProfileErrorRules p)) (Logic.ifElse (Equality.lt (Lists.length errs) (Validation.validationProfileMaxErrors p)) (Validation.ValidationResult {
        Validation.validationResultErrors = (Lists.concat2 errs (Lists.singleton payload)),
        Validation.validationResultWarnings = wrns}) acc) (Logic.ifElse (Sets.member ruleName (Validation.validationProfileWarningRules p)) (Logic.ifElse (Equality.lt (Lists.length wrns) (Validation.validationProfileMaxWarnings p)) (Validation.ValidationResult {
        Validation.validationResultErrors = errs,
        Validation.validationResultWarnings = (Lists.concat2 wrns (Lists.singleton payload))}) acc) acc)))
-- | Append a rule-tagged InvalidVertexError finding to a ValidationResult, classifying as error or warning per the profile and respecting maxErrors/maxWarnings bounds.
appendFindingVertex :: Validation.ValidationProfile -> Validation.ValidationResult t0 -> Maybe (Core.Name, t0) -> Validation.ValidationResult t0
appendFindingVertex p acc finding =
    Maybes.cases finding acc (\rp ->
      let ruleName = Pairs.first rp
          payload = Pairs.second rp
          errs = Validation.validationResultErrors acc
          wrns = Validation.validationResultWarnings acc
      in (Logic.ifElse (Sets.member ruleName (Validation.validationProfileErrorRules p)) (Logic.ifElse (Equality.lt (Lists.length errs) (Validation.validationProfileMaxErrors p)) (Validation.ValidationResult {
        Validation.validationResultErrors = (Lists.concat2 errs (Lists.singleton payload)),
        Validation.validationResultWarnings = wrns}) acc) (Logic.ifElse (Sets.member ruleName (Validation.validationProfileWarningRules p)) (Logic.ifElse (Equality.lt (Lists.length wrns) (Validation.validationProfileMaxWarnings p)) (Validation.ValidationResult {
        Validation.validationResultErrors = errs,
        Validation.validationResultWarnings = (Lists.concat2 wrns (Lists.singleton payload))}) acc) acc)))
-- | The default validation profile for property-graph validation. Every PG rule classified as an error; no warnings; maxErrors=1, maxWarnings=20.
defaultPgProfile :: Validation.ValidationProfile
defaultPgProfile =
    Validation.ValidationProfile {
      Validation.validationProfileErrorRules = (Sets.fromList [
        Core.Name "hydra.error.pg.InvalidVertexError.id",
        (Core.Name "hydra.error.pg.InvalidVertexError.label"),
        (Core.Name "hydra.error.pg.InvalidVertexError.property"),
        (Core.Name "hydra.error.pg.InvalidEdgeError.id"),
        (Core.Name "hydra.error.pg.InvalidEdgeError.inVertexLabel"),
        (Core.Name "hydra.error.pg.InvalidEdgeError.inVertexNotFound"),
        (Core.Name "hydra.error.pg.InvalidEdgeError.label"),
        (Core.Name "hydra.error.pg.InvalidEdgeError.outVertexLabel"),
        (Core.Name "hydra.error.pg.InvalidEdgeError.outVertexNotFound"),
        (Core.Name "hydra.error.pg.InvalidEdgeError.property"),
        (Core.Name "hydra.error.pg.InvalidPropertyError.invalidValue"),
        (Core.Name "hydra.error.pg.InvalidPropertyError.missingRequired"),
        (Core.Name "hydra.error.pg.InvalidPropertyError.unexpectedKey")]),
      Validation.validationProfileWarningRules = Sets.empty,
      Validation.validationProfileMaxErrors = 1,
      Validation.validationProfileMaxWarnings = 20}
-- | True iff the given rule name appears in the profile's errorRules or warningRules.
enabledPg :: Validation.ValidationProfile -> Core.Name -> Bool
enabledPg p ruleName =
    Logic.or (Sets.member ruleName (Validation.validationProfileErrorRules p)) (Sets.member ruleName (Validation.validationProfileWarningRules p))
-- | Validate an edge against its EdgeType under the given ValidationProfile, returning a ValidationResult InvalidEdgeError.
validateEdge :: Validation.ValidationProfile -> (t0 -> t1 -> Maybe Pg.InvalidValueError) -> Maybe (t1 -> Maybe Model.VertexLabel) -> Model.EdgeType t0 -> Model.Edge t1 -> Validation.ValidationResult Pg.InvalidEdgeError
validateEdge p checkValue labelForVertexId typ el =
    Lists.foldl (\acc -> \guarded -> Logic.ifElse (Equality.gte (Lists.length (Validation.validationResultErrors acc)) (Validation.validationProfileMaxErrors p)) acc (appendFindingEdge p acc guarded)) (Validation.ValidationResult {
      Validation.validationResultErrors = [],
      Validation.validationResultWarnings = []}) [
      Logic.ifElse (enabledPg p (Core.Name "hydra.error.pg.InvalidEdgeError.label")) (Maybes.map (\f -> (Core.Name "hydra.error.pg.InvalidEdgeError.label", f)) (
        let expected = Model.edgeTypeLabel typ
            actual = Model.edgeLabel el
        in (Logic.ifElse (Equality.equal (Model.unEdgeLabel actual) (Model.unEdgeLabel expected)) Nothing (Just (Pg.InvalidEdgeErrorLabel (Pg.NoSuchEdgeLabelError {
          Pg.noSuchEdgeLabelErrorLabel = actual})))))) Nothing,
      (Logic.ifElse (enabledPg p (Core.Name "hydra.error.pg.InvalidEdgeError.id")) (Maybes.map (\f -> (Core.Name "hydra.error.pg.InvalidEdgeError.id", f)) (Maybes.map (\err -> Pg.InvalidEdgeErrorId err) (checkValue (Model.edgeTypeId typ) (Model.edgeId el)))) Nothing),
      (Logic.ifElse (enabledPg p (Core.Name "hydra.error.pg.InvalidEdgeError.property")) (Maybes.map (\f -> (Core.Name "hydra.error.pg.InvalidEdgeError.property", f)) (Maybes.map (\err -> Pg.InvalidEdgeErrorProperty err) (Lists.maybeHead (Validation.validationResultErrors (validateProperties p checkValue (Model.edgeTypeProperties typ) (Model.edgeProperties el)))))) Nothing),
      (Logic.ifElse (enabledPg p (Core.Name "hydra.error.pg.InvalidEdgeError.outVertexNotFound")) (Maybes.map (\f -> (Core.Name "hydra.error.pg.InvalidEdgeError.outVertexNotFound", f)) (Maybes.maybe Nothing (\f -> Maybes.maybe (Just Pg.InvalidEdgeErrorOutVertexNotFound) (\_label -> Nothing) (f (Model.edgeOut el))) labelForVertexId)) Nothing),
      (Logic.ifElse (enabledPg p (Core.Name "hydra.error.pg.InvalidEdgeError.outVertexLabel")) (Maybes.map (\f -> (Core.Name "hydra.error.pg.InvalidEdgeError.outVertexLabel", f)) (Maybes.maybe Nothing (\f -> Maybes.maybe Nothing (\label -> Logic.ifElse (Equality.equal (Model.unVertexLabel label) (Model.unVertexLabel (Model.edgeTypeOut typ))) Nothing (Just (Pg.InvalidEdgeErrorOutVertexLabel (Pg.WrongVertexLabelError {
        Pg.wrongVertexLabelErrorExpected = (Model.edgeTypeOut typ),
        Pg.wrongVertexLabelErrorActual = label})))) (f (Model.edgeOut el))) labelForVertexId)) Nothing),
      (Logic.ifElse (enabledPg p (Core.Name "hydra.error.pg.InvalidEdgeError.inVertexNotFound")) (Maybes.map (\f -> (Core.Name "hydra.error.pg.InvalidEdgeError.inVertexNotFound", f)) (Maybes.maybe Nothing (\f -> Maybes.maybe (Just Pg.InvalidEdgeErrorInVertexNotFound) (\_label -> Nothing) (f (Model.edgeIn el))) labelForVertexId)) Nothing),
      (Logic.ifElse (enabledPg p (Core.Name "hydra.error.pg.InvalidEdgeError.inVertexLabel")) (Maybes.map (\f -> (Core.Name "hydra.error.pg.InvalidEdgeError.inVertexLabel", f)) (Maybes.maybe Nothing (\f -> Maybes.maybe Nothing (\label -> Logic.ifElse (Equality.equal (Model.unVertexLabel label) (Model.unVertexLabel (Model.edgeTypeIn typ))) Nothing (Just (Pg.InvalidEdgeErrorInVertexLabel (Pg.WrongVertexLabelError {
        Pg.wrongVertexLabelErrorExpected = (Model.edgeTypeIn typ),
        Pg.wrongVertexLabelErrorActual = label})))) (f (Model.edgeIn el))) labelForVertexId)) Nothing)]
-- | Validate a property graph against a GraphSchema under the given ValidationProfile, threading a ValidationResult InvalidGraphError accumulator. Errors hard-stop traversal once maxErrors is reached.
validateGraph :: Ord t0 => (Validation.ValidationProfile -> Validation.ValidationResult (Pg.InvalidGraphError t0) -> (t1 -> t0 -> Maybe Pg.InvalidValueError) -> Model.GraphSchema t1 -> Model.Graph t0 -> Validation.ValidationResult (Pg.InvalidGraphError t0))
validateGraph p acc0 checkValue schema graph =

      let labelForVertexId = Just (\i -> Maybes.map Model.vertexLabel (Maps.lookup i (Model.graphVertices graph)))
          vertexFindings =
                  Lists.bind (Maps.elems (Model.graphVertices graph)) (\el ->
                    let tOpt = Maps.lookup (Model.vertexLabel el) (Model.graphSchemaVertices schema)
                        perVertex =
                                Maybes.maybe (Validation.ValidationResult {
                                  Validation.validationResultErrors = [
                                    Pg.InvalidVertexErrorLabel (Pg.NoSuchVertexLabelError {
                                      Pg.noSuchVertexLabelErrorLabel = (Model.vertexLabel el)})],
                                  Validation.validationResultWarnings = []}) (\t -> validateVertex p checkValue t el) tOpt
                    in (Lists.map (\ve -> Pg.InvalidGraphErrorVertex (Pg.InvalidGraphVertexError {
                      Pg.invalidGraphVertexErrorId = (Model.vertexId el),
                      Pg.invalidGraphVertexErrorError = ve})) (Validation.validationResultErrors perVertex)))
          edgeFindings =
                  Lists.bind (Maps.elems (Model.graphEdges graph)) (\el ->
                    let tOpt = Maps.lookup (Model.edgeLabel el) (Model.graphSchemaEdges schema)
                        perEdge =
                                Maybes.maybe (Validation.ValidationResult {
                                  Validation.validationResultErrors = [
                                    Pg.InvalidEdgeErrorLabel (Pg.NoSuchEdgeLabelError {
                                      Pg.noSuchEdgeLabelErrorLabel = (Model.edgeLabel el)})],
                                  Validation.validationResultWarnings = []}) (\t -> validateEdge p checkValue labelForVertexId t el) tOpt
                    in (Lists.map (\ee -> Pg.InvalidGraphErrorEdge (Pg.InvalidGraphEdgeError {
                      Pg.invalidGraphEdgeErrorId = (Model.edgeId el),
                      Pg.invalidGraphEdgeErrorError = ee})) (Validation.validationResultErrors perEdge)))
          liftedErrs = Lists.concat2 vertexFindings edgeFindings
          newErrs =
                  Lists.take (Validation.validationProfileMaxErrors p) (Lists.concat2 (Validation.validationResultErrors acc0) liftedErrs)
      in Validation.ValidationResult {
        Validation.validationResultErrors = newErrs,
        Validation.validationResultWarnings = (Validation.validationResultWarnings acc0)}
-- | Validate a map of properties against a list of PropertyType under the given ValidationProfile, returning a ValidationResult InvalidElementPropertyError.
validateProperties :: Validation.ValidationProfile -> (t0 -> t1 -> Maybe Pg.InvalidValueError) -> [Model.PropertyType t0] -> M.Map Model.PropertyKey t1 -> Validation.ValidationResult Pg.InvalidElementPropertyError
validateProperties p checkValue types props =

      let m = Maps.fromList (Lists.map (\pt -> (Model.propertyTypeKey pt, (Model.propertyTypeValue pt))) types)
          missingChecks =
                  Lists.map (\t -> Logic.ifElse (enabledPg p (Core.Name "hydra.error.pg.InvalidPropertyError.missingRequired")) (Maybes.map (\f -> (Core.Name "hydra.error.pg.InvalidPropertyError.missingRequired", f)) (Logic.ifElse (Model.propertyTypeRequired t) (Maybes.maybe (Just (Pg.InvalidElementPropertyError {
                    Pg.invalidElementPropertyErrorKey = (Model.propertyTypeKey t),
                    Pg.invalidElementPropertyErrorError = (Pg.InvalidPropertyErrorMissingRequired (Model.propertyTypeKey t))})) (\_ -> Nothing) (Maps.lookup (Model.propertyTypeKey t) props)) Nothing)) Nothing) types
          valueChecks =
                  Lists.bind (Maps.toList props) (\kv ->
                    let key = Pairs.first kv
                        val = Pairs.second kv
                    in [
                      Logic.ifElse (enabledPg p (Core.Name "hydra.error.pg.InvalidPropertyError.unexpectedKey")) (Maybes.map (\f -> (Core.Name "hydra.error.pg.InvalidPropertyError.unexpectedKey", f)) (Maybes.maybe (Just (Pg.InvalidElementPropertyError {
                        Pg.invalidElementPropertyErrorKey = key,
                        Pg.invalidElementPropertyErrorError = (Pg.InvalidPropertyErrorUnexpectedKey key)})) (\_ -> Nothing) (Maps.lookup key m))) Nothing,
                      (Logic.ifElse (enabledPg p (Core.Name "hydra.error.pg.InvalidPropertyError.invalidValue")) (Maybes.map (\f -> (Core.Name "hydra.error.pg.InvalidPropertyError.invalidValue", f)) (Maybes.maybe Nothing (\typ -> Maybes.map (\err -> Pg.InvalidElementPropertyError {
                        Pg.invalidElementPropertyErrorKey = key,
                        Pg.invalidElementPropertyErrorError = (Pg.InvalidPropertyErrorInvalidValue err)}) (checkValue typ val)) (Maps.lookup key m))) Nothing)])
      in (Lists.foldl (\acc -> \guarded -> Logic.ifElse (Equality.gte (Lists.length (Validation.validationResultErrors acc)) (Validation.validationProfileMaxErrors p)) acc (appendFindingProperty p acc guarded)) (Validation.ValidationResult {
        Validation.validationResultErrors = [],
        Validation.validationResultWarnings = []}) (Lists.concat2 missingChecks valueChecks))
-- | Validate a vertex against its VertexType under the given ValidationProfile, returning a ValidationResult InvalidVertexError.
validateVertex :: Validation.ValidationProfile -> (t0 -> t1 -> Maybe Pg.InvalidValueError) -> Model.VertexType t0 -> Model.Vertex t1 -> Validation.ValidationResult Pg.InvalidVertexError
validateVertex p checkValue typ el =
    Lists.foldl (\acc -> \guarded -> Logic.ifElse (Equality.gte (Lists.length (Validation.validationResultErrors acc)) (Validation.validationProfileMaxErrors p)) acc (appendFindingVertex p acc guarded)) (Validation.ValidationResult {
      Validation.validationResultErrors = [],
      Validation.validationResultWarnings = []}) [
      Logic.ifElse (enabledPg p (Core.Name "hydra.error.pg.InvalidVertexError.label")) (Maybes.map (\f -> (Core.Name "hydra.error.pg.InvalidVertexError.label", f)) (
        let expected = Model.vertexTypeLabel typ
            actual = Model.vertexLabel el
        in (Logic.ifElse (Equality.equal (Model.unVertexLabel actual) (Model.unVertexLabel expected)) Nothing (Just (Pg.InvalidVertexErrorLabel (Pg.NoSuchVertexLabelError {
          Pg.noSuchVertexLabelErrorLabel = actual})))))) Nothing,
      (Logic.ifElse (enabledPg p (Core.Name "hydra.error.pg.InvalidVertexError.id")) (Maybes.map (\f -> (Core.Name "hydra.error.pg.InvalidVertexError.id", f)) (Maybes.map (\err -> Pg.InvalidVertexErrorId err) (checkValue (Model.vertexTypeId typ) (Model.vertexId el)))) Nothing),
      (Logic.ifElse (enabledPg p (Core.Name "hydra.error.pg.InvalidVertexError.property")) (Maybes.map (\f -> (Core.Name "hydra.error.pg.InvalidVertexError.property", f)) (Maybes.map (\err -> Pg.InvalidVertexErrorProperty err) (Lists.maybeHead (Validation.validationResultErrors (validateProperties p checkValue (Model.vertexTypeProperties typ) (Model.vertexProperties el)))))) Nothing)]
