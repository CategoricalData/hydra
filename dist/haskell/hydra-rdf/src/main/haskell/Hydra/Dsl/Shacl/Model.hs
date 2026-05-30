-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.shacl.model

module Hydra.Dsl.Shacl.Model where
import qualified Hydra.Core as Core
import qualified Hydra.Typed as Phantoms
import qualified Hydra.Rdf.Syntax as Syntax
import qualified Hydra.Shacl.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Set as S
-- | DSL constructor for hydra.shacl.model.Closed
closed :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm (Maybe (S.Set Syntax.Property)) -> Phantoms.TypedTerm Model.Closed
closed isClosed ignoredProperties =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Closed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isClosed"),
          Core.fieldTerm = (Phantoms.unTypedTerm isClosed)},
        Core.Field {
          Core.fieldName = (Core.Name "ignoredProperties"),
          Core.fieldTerm = (Phantoms.unTypedTerm ignoredProperties)}]}))
-- | DSL accessor for the ignoredProperties field of hydra.shacl.model.Closed
closedIgnoredProperties :: Phantoms.TypedTerm Model.Closed -> Phantoms.TypedTerm (Maybe (S.Set Syntax.Property))
closedIgnoredProperties x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Closed"),
        Core.projectionFieldName = (Core.Name "ignoredProperties")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the isClosed field of hydra.shacl.model.Closed
closedIsClosed :: Phantoms.TypedTerm Model.Closed -> Phantoms.TypedTerm Bool
closedIsClosed x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Closed"),
        Core.projectionFieldName = (Core.Name "isClosed")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the ignoredProperties field of hydra.shacl.model.Closed
closedWithIgnoredProperties :: Phantoms.TypedTerm Model.Closed -> Phantoms.TypedTerm (Maybe (S.Set Syntax.Property)) -> Phantoms.TypedTerm Model.Closed
closedWithIgnoredProperties original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Closed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isClosed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.Closed"),
              Core.projectionFieldName = (Core.Name "isClosed")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignoredProperties"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the isClosed field of hydra.shacl.model.Closed
closedWithIsClosed :: Phantoms.TypedTerm Model.Closed -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Model.Closed
closedWithIsClosed original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Closed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isClosed"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ignoredProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.Closed"),
              Core.projectionFieldName = (Core.Name "ignoredProperties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL injection for the and variant of hydra.shacl.model.CommonConstraint
commonConstraintAnd :: Phantoms.TypedTerm (S.Set (Model.Reference Model.Shape)) -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintAnd x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the class variant of hydra.shacl.model.CommonConstraint
commonConstraintClass :: Phantoms.TypedTerm (S.Set Syntax.RdfsClass) -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintClass x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the closed variant of hydra.shacl.model.CommonConstraint
commonConstraintClosed :: Phantoms.TypedTerm Model.Closed -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintClosed x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "closed"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the datatype variant of hydra.shacl.model.CommonConstraint
commonConstraintDatatype :: Phantoms.TypedTerm Syntax.Iri -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintDatatype x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "datatype"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the disjoint variant of hydra.shacl.model.CommonConstraint
commonConstraintDisjoint :: Phantoms.TypedTerm (S.Set Syntax.Property) -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintDisjoint x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "disjoint"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the equals variant of hydra.shacl.model.CommonConstraint
commonConstraintEquals :: Phantoms.TypedTerm (S.Set Syntax.Property) -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintEquals x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equals"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the hasValue variant of hydra.shacl.model.CommonConstraint
commonConstraintHasValue :: Phantoms.TypedTerm (S.Set Syntax.Node) -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintHasValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hasValue"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the in variant of hydra.shacl.model.CommonConstraint
commonConstraintIn :: Phantoms.TypedTerm [Syntax.Node] -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintIn x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the languageIn variant of hydra.shacl.model.CommonConstraint
commonConstraintLanguageIn :: Phantoms.TypedTerm (S.Set Syntax.LanguageTag) -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintLanguageIn x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "languageIn"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the maxExclusive variant of hydra.shacl.model.CommonConstraint
commonConstraintMaxExclusive :: Phantoms.TypedTerm Syntax.Literal -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintMaxExclusive x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxExclusive"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the maxInclusive variant of hydra.shacl.model.CommonConstraint
commonConstraintMaxInclusive :: Phantoms.TypedTerm Syntax.Literal -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintMaxInclusive x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxInclusive"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the maxLength variant of hydra.shacl.model.CommonConstraint
commonConstraintMaxLength :: Phantoms.TypedTerm Integer -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintMaxLength x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxLength"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the minExclusive variant of hydra.shacl.model.CommonConstraint
commonConstraintMinExclusive :: Phantoms.TypedTerm Syntax.Literal -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintMinExclusive x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minExclusive"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the minInclusive variant of hydra.shacl.model.CommonConstraint
commonConstraintMinInclusive :: Phantoms.TypedTerm Syntax.Literal -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintMinInclusive x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minInclusive"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the minLength variant of hydra.shacl.model.CommonConstraint
commonConstraintMinLength :: Phantoms.TypedTerm Integer -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintMinLength x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minLength"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the node variant of hydra.shacl.model.CommonConstraint
commonConstraintNode :: Phantoms.TypedTerm (S.Set (Model.Reference Model.NodeShape)) -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintNode x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "node"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the nodeKind variant of hydra.shacl.model.CommonConstraint
commonConstraintNodeKind :: Phantoms.TypedTerm Model.NodeKind -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintNodeKind x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nodeKind"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the not variant of hydra.shacl.model.CommonConstraint
commonConstraintNot :: Phantoms.TypedTerm (S.Set (Model.Reference Model.Shape)) -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintNot x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the or variant of hydra.shacl.model.CommonConstraint
commonConstraintOr :: Phantoms.TypedTerm (S.Set (Model.Reference Model.Shape)) -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintOr x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the pattern variant of hydra.shacl.model.CommonConstraint
commonConstraintPattern :: Phantoms.TypedTerm Model.Pattern -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintPattern x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the property variant of hydra.shacl.model.CommonConstraint
commonConstraintProperty :: Phantoms.TypedTerm (S.Set (Model.Reference Model.PropertyShape)) -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintProperty x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the xone variant of hydra.shacl.model.CommonConstraint
commonConstraintXone :: Phantoms.TypedTerm (S.Set (Model.Reference Model.Shape)) -> Phantoms.TypedTerm Model.CommonConstraint
commonConstraintXone x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "xone"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for hydra.shacl.model.CommonProperties
commonProperties :: Phantoms.TypedTerm (S.Set Model.CommonConstraint) -> Phantoms.TypedTerm (Maybe Bool) -> Phantoms.TypedTerm Syntax.LangStrings -> Phantoms.TypedTerm Model.Severity -> Phantoms.TypedTerm (S.Set Syntax.RdfsClass) -> Phantoms.TypedTerm (S.Set Syntax.IriOrLiteral) -> Phantoms.TypedTerm (S.Set Syntax.Property) -> Phantoms.TypedTerm (S.Set Syntax.Property) -> Phantoms.TypedTerm Model.CommonProperties
commonProperties constraints deactivated message severity targetClass targetNode targetObjectsOf targetSubjectsOf =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTypedTerm constraints)},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Phantoms.unTypedTerm deactivated)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Phantoms.unTypedTerm message)},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Phantoms.unTypedTerm severity)},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Phantoms.unTypedTerm targetClass)},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Phantoms.unTypedTerm targetNode)},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Phantoms.unTypedTerm targetObjectsOf)},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Phantoms.unTypedTerm targetSubjectsOf)}]}))
-- | DSL accessor for the constraints field of hydra.shacl.model.CommonProperties
commonPropertiesConstraints :: Phantoms.TypedTerm Model.CommonProperties -> Phantoms.TypedTerm (S.Set Model.CommonConstraint)
commonPropertiesConstraints x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionFieldName = (Core.Name "constraints")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the deactivated field of hydra.shacl.model.CommonProperties
commonPropertiesDeactivated :: Phantoms.TypedTerm Model.CommonProperties -> Phantoms.TypedTerm (Maybe Bool)
commonPropertiesDeactivated x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionFieldName = (Core.Name "deactivated")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the message field of hydra.shacl.model.CommonProperties
commonPropertiesMessage :: Phantoms.TypedTerm Model.CommonProperties -> Phantoms.TypedTerm Syntax.LangStrings
commonPropertiesMessage x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionFieldName = (Core.Name "message")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the severity field of hydra.shacl.model.CommonProperties
commonPropertiesSeverity :: Phantoms.TypedTerm Model.CommonProperties -> Phantoms.TypedTerm Model.Severity
commonPropertiesSeverity x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionFieldName = (Core.Name "severity")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the targetClass field of hydra.shacl.model.CommonProperties
commonPropertiesTargetClass :: Phantoms.TypedTerm Model.CommonProperties -> Phantoms.TypedTerm (S.Set Syntax.RdfsClass)
commonPropertiesTargetClass x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionFieldName = (Core.Name "targetClass")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the targetNode field of hydra.shacl.model.CommonProperties
commonPropertiesTargetNode :: Phantoms.TypedTerm Model.CommonProperties -> Phantoms.TypedTerm (S.Set Syntax.IriOrLiteral)
commonPropertiesTargetNode x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionFieldName = (Core.Name "targetNode")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the targetObjectsOf field of hydra.shacl.model.CommonProperties
commonPropertiesTargetObjectsOf :: Phantoms.TypedTerm Model.CommonProperties -> Phantoms.TypedTerm (S.Set Syntax.Property)
commonPropertiesTargetObjectsOf x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionFieldName = (Core.Name "targetObjectsOf")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the targetSubjectsOf field of hydra.shacl.model.CommonProperties
commonPropertiesTargetSubjectsOf :: Phantoms.TypedTerm Model.CommonProperties -> Phantoms.TypedTerm (S.Set Syntax.Property)
commonPropertiesTargetSubjectsOf x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionFieldName = (Core.Name "targetSubjectsOf")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the constraints field of hydra.shacl.model.CommonProperties
commonPropertiesWithConstraints :: Phantoms.TypedTerm Model.CommonProperties -> Phantoms.TypedTerm (S.Set Model.CommonConstraint) -> Phantoms.TypedTerm Model.CommonProperties
commonPropertiesWithConstraints original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "deactivated")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetClass")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetNode")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the deactivated field of hydra.shacl.model.CommonProperties
commonPropertiesWithDeactivated :: Phantoms.TypedTerm Model.CommonProperties -> Phantoms.TypedTerm (Maybe Bool) -> Phantoms.TypedTerm Model.CommonProperties
commonPropertiesWithDeactivated original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetClass")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetNode")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the message field of hydra.shacl.model.CommonProperties
commonPropertiesWithMessage :: Phantoms.TypedTerm Model.CommonProperties -> Phantoms.TypedTerm Syntax.LangStrings -> Phantoms.TypedTerm Model.CommonProperties
commonPropertiesWithMessage original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "deactivated")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetClass")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetNode")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the severity field of hydra.shacl.model.CommonProperties
commonPropertiesWithSeverity :: Phantoms.TypedTerm Model.CommonProperties -> Phantoms.TypedTerm Model.Severity -> Phantoms.TypedTerm Model.CommonProperties
commonPropertiesWithSeverity original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "deactivated")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetClass")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetNode")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the targetClass field of hydra.shacl.model.CommonProperties
commonPropertiesWithTargetClass :: Phantoms.TypedTerm Model.CommonProperties -> Phantoms.TypedTerm (S.Set Syntax.RdfsClass) -> Phantoms.TypedTerm Model.CommonProperties
commonPropertiesWithTargetClass original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "deactivated")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetNode")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the targetNode field of hydra.shacl.model.CommonProperties
commonPropertiesWithTargetNode :: Phantoms.TypedTerm Model.CommonProperties -> Phantoms.TypedTerm (S.Set Syntax.IriOrLiteral) -> Phantoms.TypedTerm Model.CommonProperties
commonPropertiesWithTargetNode original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "deactivated")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetClass")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the targetObjectsOf field of hydra.shacl.model.CommonProperties
commonPropertiesWithTargetObjectsOf :: Phantoms.TypedTerm Model.CommonProperties -> Phantoms.TypedTerm (S.Set Syntax.Property) -> Phantoms.TypedTerm Model.CommonProperties
commonPropertiesWithTargetObjectsOf original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "deactivated")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetClass")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetNode")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the targetSubjectsOf field of hydra.shacl.model.CommonProperties
commonPropertiesWithTargetSubjectsOf :: Phantoms.TypedTerm Model.CommonProperties -> Phantoms.TypedTerm (S.Set Syntax.Property) -> Phantoms.TypedTerm Model.CommonProperties
commonPropertiesWithTargetSubjectsOf original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "deactivated")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetClass")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetNode")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.shacl.model.Definition
definition :: Phantoms.TypedTerm Syntax.Iri -> Phantoms.TypedTerm a -> Phantoms.TypedTerm (Model.Definition a)
definition iri target =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Phantoms.unTypedTerm iri)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTypedTerm target)}]}))
-- | DSL accessor for the iri field of hydra.shacl.model.Definition
definitionIri :: Phantoms.TypedTerm (Model.Definition a) -> Phantoms.TypedTerm Syntax.Iri
definitionIri x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Definition"),
        Core.projectionFieldName = (Core.Name "iri")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the target field of hydra.shacl.model.Definition
definitionTarget :: Phantoms.TypedTerm (Model.Definition a) -> Phantoms.TypedTerm a
definitionTarget x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Definition"),
        Core.projectionFieldName = (Core.Name "target")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the iri field of hydra.shacl.model.Definition
definitionWithIri :: Phantoms.TypedTerm (Model.Definition a) -> Phantoms.TypedTerm Syntax.Iri -> Phantoms.TypedTerm (Model.Definition a)
definitionWithIri original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.Definition"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the target field of hydra.shacl.model.Definition
definitionWithTarget :: Phantoms.TypedTerm (Model.Definition a) -> Phantoms.TypedTerm a -> Phantoms.TypedTerm (Model.Definition a)
definitionWithTarget original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.Definition"),
              Core.projectionFieldName = (Core.Name "iri")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL injection for the blankNode variant of hydra.shacl.model.NodeKind
nodeKindBlankNode :: Phantoms.TypedTerm Model.NodeKind
nodeKindBlankNode =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "blankNode"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the blankNodeOrIri variant of hydra.shacl.model.NodeKind
nodeKindBlankNodeOrIri :: Phantoms.TypedTerm Model.NodeKind
nodeKindBlankNodeOrIri =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "blankNodeOrIri"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the blankNodeOrLiteral variant of hydra.shacl.model.NodeKind
nodeKindBlankNodeOrLiteral :: Phantoms.TypedTerm Model.NodeKind
nodeKindBlankNodeOrLiteral =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "blankNodeOrLiteral"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the iri variant of hydra.shacl.model.NodeKind
nodeKindIri :: Phantoms.TypedTerm Model.NodeKind
nodeKindIri =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iri"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the iriOrLiteral variant of hydra.shacl.model.NodeKind
nodeKindIriOrLiteral :: Phantoms.TypedTerm Model.NodeKind
nodeKindIriOrLiteral =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iriOrLiteral"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the literal variant of hydra.shacl.model.NodeKind
nodeKindLiteral :: Phantoms.TypedTerm Model.NodeKind
nodeKindLiteral =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.shacl.model.NodeShape
nodeShape :: Phantoms.TypedTerm Model.CommonProperties -> Phantoms.TypedTerm Model.NodeShape
nodeShape common =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.NodeShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Phantoms.unTypedTerm common)}]}))
-- | DSL accessor for the common field of hydra.shacl.model.NodeShape
nodeShapeCommon :: Phantoms.TypedTerm Model.NodeShape -> Phantoms.TypedTerm Model.CommonProperties
nodeShapeCommon x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.NodeShape"),
        Core.projectionFieldName = (Core.Name "common")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the common field of hydra.shacl.model.NodeShape
nodeShapeWithCommon :: Phantoms.TypedTerm Model.NodeShape -> Phantoms.TypedTerm Model.CommonProperties -> Phantoms.TypedTerm Model.NodeShape
nodeShapeWithCommon original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.NodeShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.shacl.model.Pattern
pattern :: Phantoms.TypedTerm String -> Phantoms.TypedTerm (Maybe String) -> Phantoms.TypedTerm Model.Pattern
pattern regex flags =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Pattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "regex"),
          Core.fieldTerm = (Phantoms.unTypedTerm regex)},
        Core.Field {
          Core.fieldName = (Core.Name "flags"),
          Core.fieldTerm = (Phantoms.unTypedTerm flags)}]}))
-- | DSL accessor for the flags field of hydra.shacl.model.Pattern
patternFlags :: Phantoms.TypedTerm Model.Pattern -> Phantoms.TypedTerm (Maybe String)
patternFlags x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Pattern"),
        Core.projectionFieldName = (Core.Name "flags")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the regex field of hydra.shacl.model.Pattern
patternRegex :: Phantoms.TypedTerm Model.Pattern -> Phantoms.TypedTerm String
patternRegex x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Pattern"),
        Core.projectionFieldName = (Core.Name "regex")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the flags field of hydra.shacl.model.Pattern
patternWithFlags :: Phantoms.TypedTerm Model.Pattern -> Phantoms.TypedTerm (Maybe String) -> Phantoms.TypedTerm Model.Pattern
patternWithFlags original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Pattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "regex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.Pattern"),
              Core.projectionFieldName = (Core.Name "regex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "flags"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the regex field of hydra.shacl.model.Pattern
patternWithRegex :: Phantoms.TypedTerm Model.Pattern -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Model.Pattern
patternWithRegex original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Pattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "regex"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "flags"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.Pattern"),
              Core.projectionFieldName = (Core.Name "flags")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.shacl.model.PropertyShape
propertyShape :: Phantoms.TypedTerm Model.CommonProperties -> Phantoms.TypedTerm (S.Set Model.PropertyShapeConstraint) -> Phantoms.TypedTerm (Maybe Syntax.Node) -> Phantoms.TypedTerm Syntax.LangStrings -> Phantoms.TypedTerm Syntax.LangStrings -> Phantoms.TypedTerm (Maybe Integer) -> Phantoms.TypedTerm Syntax.Iri -> Phantoms.TypedTerm Model.PropertyShape
propertyShape common constraints defaultValue description name order path =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Phantoms.unTypedTerm common)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTypedTerm constraints)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Phantoms.unTypedTerm defaultValue)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTypedTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Phantoms.unTypedTerm order)},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTypedTerm path)}]}))
-- | DSL accessor for the common field of hydra.shacl.model.PropertyShape
propertyShapeCommon :: Phantoms.TypedTerm Model.PropertyShape -> Phantoms.TypedTerm Model.CommonProperties
propertyShapeCommon x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionFieldName = (Core.Name "common")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL injection for the lessThan variant of hydra.shacl.model.PropertyShapeConstraint
propertyShapeConstraintLessThan :: Phantoms.TypedTerm (S.Set Syntax.Property) -> Phantoms.TypedTerm Model.PropertyShapeConstraint
propertyShapeConstraintLessThan x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThan"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the lessThanOrEquals variant of hydra.shacl.model.PropertyShapeConstraint
propertyShapeConstraintLessThanOrEquals :: Phantoms.TypedTerm (S.Set Syntax.Property) -> Phantoms.TypedTerm Model.PropertyShapeConstraint
propertyShapeConstraintLessThanOrEquals x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThanOrEquals"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the maxCount variant of hydra.shacl.model.PropertyShapeConstraint
propertyShapeConstraintMaxCount :: Phantoms.TypedTerm Integer -> Phantoms.TypedTerm Model.PropertyShapeConstraint
propertyShapeConstraintMaxCount x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxCount"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the minCount variant of hydra.shacl.model.PropertyShapeConstraint
propertyShapeConstraintMinCount :: Phantoms.TypedTerm Integer -> Phantoms.TypedTerm Model.PropertyShapeConstraint
propertyShapeConstraintMinCount x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minCount"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the qualifiedValueShape variant of hydra.shacl.model.PropertyShapeConstraint
propertyShapeConstraintQualifiedValueShape :: Phantoms.TypedTerm Model.QualifiedValueShape -> Phantoms.TypedTerm Model.PropertyShapeConstraint
propertyShapeConstraintQualifiedValueShape x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualifiedValueShape"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the uniqueLang variant of hydra.shacl.model.PropertyShapeConstraint
propertyShapeConstraintUniqueLang :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Model.PropertyShapeConstraint
propertyShapeConstraintUniqueLang x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uniqueLang"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL accessor for the constraints field of hydra.shacl.model.PropertyShape
propertyShapeConstraints :: Phantoms.TypedTerm Model.PropertyShape -> Phantoms.TypedTerm (S.Set Model.PropertyShapeConstraint)
propertyShapeConstraints x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionFieldName = (Core.Name "constraints")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the defaultValue field of hydra.shacl.model.PropertyShape
propertyShapeDefaultValue :: Phantoms.TypedTerm Model.PropertyShape -> Phantoms.TypedTerm (Maybe Syntax.Node)
propertyShapeDefaultValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionFieldName = (Core.Name "defaultValue")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the description field of hydra.shacl.model.PropertyShape
propertyShapeDescription :: Phantoms.TypedTerm Model.PropertyShape -> Phantoms.TypedTerm Syntax.LangStrings
propertyShapeDescription x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionFieldName = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.shacl.model.PropertyShape
propertyShapeName :: Phantoms.TypedTerm Model.PropertyShape -> Phantoms.TypedTerm Syntax.LangStrings
propertyShapeName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the order field of hydra.shacl.model.PropertyShape
propertyShapeOrder :: Phantoms.TypedTerm Model.PropertyShape -> Phantoms.TypedTerm (Maybe Integer)
propertyShapeOrder x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionFieldName = (Core.Name "order")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the path field of hydra.shacl.model.PropertyShape
propertyShapePath :: Phantoms.TypedTerm Model.PropertyShape -> Phantoms.TypedTerm Syntax.Iri
propertyShapePath x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionFieldName = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the common field of hydra.shacl.model.PropertyShape
propertyShapeWithCommon :: Phantoms.TypedTerm Model.PropertyShape -> Phantoms.TypedTerm Model.CommonProperties -> Phantoms.TypedTerm Model.PropertyShape
propertyShapeWithCommon original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "defaultValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the constraints field of hydra.shacl.model.PropertyShape
propertyShapeWithConstraints :: Phantoms.TypedTerm Model.PropertyShape -> Phantoms.TypedTerm (S.Set Model.PropertyShapeConstraint) -> Phantoms.TypedTerm Model.PropertyShape
propertyShapeWithConstraints original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "common")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "defaultValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the defaultValue field of hydra.shacl.model.PropertyShape
propertyShapeWithDefaultValue :: Phantoms.TypedTerm Model.PropertyShape -> Phantoms.TypedTerm (Maybe Syntax.Node) -> Phantoms.TypedTerm Model.PropertyShape
propertyShapeWithDefaultValue original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "common")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the description field of hydra.shacl.model.PropertyShape
propertyShapeWithDescription :: Phantoms.TypedTerm Model.PropertyShape -> Phantoms.TypedTerm Syntax.LangStrings -> Phantoms.TypedTerm Model.PropertyShape
propertyShapeWithDescription original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "common")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "defaultValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.shacl.model.PropertyShape
propertyShapeWithName :: Phantoms.TypedTerm Model.PropertyShape -> Phantoms.TypedTerm Syntax.LangStrings -> Phantoms.TypedTerm Model.PropertyShape
propertyShapeWithName original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "common")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "defaultValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the order field of hydra.shacl.model.PropertyShape
propertyShapeWithOrder :: Phantoms.TypedTerm Model.PropertyShape -> Phantoms.TypedTerm (Maybe Integer) -> Phantoms.TypedTerm Model.PropertyShape
propertyShapeWithOrder original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "common")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "defaultValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the path field of hydra.shacl.model.PropertyShape
propertyShapeWithPath :: Phantoms.TypedTerm Model.PropertyShape -> Phantoms.TypedTerm Syntax.Iri -> Phantoms.TypedTerm Model.PropertyShape
propertyShapeWithPath original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "common")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "defaultValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.shacl.model.QualifiedValueShape
qualifiedValueShape :: Phantoms.TypedTerm (Model.Reference Model.Shape) -> Phantoms.TypedTerm Integer -> Phantoms.TypedTerm Integer -> Phantoms.TypedTerm (Maybe Bool) -> Phantoms.TypedTerm Model.QualifiedValueShape
qualifiedValueShape qualifiedValueShape qualifiedMaxCount qualifiedMinCount qualifiedValueShapesDisjoint =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShape"),
          Core.fieldTerm = (Phantoms.unTypedTerm qualifiedValueShape)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMaxCount"),
          Core.fieldTerm = (Phantoms.unTypedTerm qualifiedMaxCount)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMinCount"),
          Core.fieldTerm = (Phantoms.unTypedTerm qualifiedMinCount)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShapesDisjoint"),
          Core.fieldTerm = (Phantoms.unTypedTerm qualifiedValueShapesDisjoint)}]}))
-- | DSL accessor for the qualifiedMaxCount field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeQualifiedMaxCount :: Phantoms.TypedTerm Model.QualifiedValueShape -> Phantoms.TypedTerm Integer
qualifiedValueShapeQualifiedMaxCount x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
        Core.projectionFieldName = (Core.Name "qualifiedMaxCount")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the qualifiedMinCount field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeQualifiedMinCount :: Phantoms.TypedTerm Model.QualifiedValueShape -> Phantoms.TypedTerm Integer
qualifiedValueShapeQualifiedMinCount x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
        Core.projectionFieldName = (Core.Name "qualifiedMinCount")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the qualifiedValueShape field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeQualifiedValueShape :: Phantoms.TypedTerm Model.QualifiedValueShape -> Phantoms.TypedTerm (Model.Reference Model.Shape)
qualifiedValueShapeQualifiedValueShape x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
        Core.projectionFieldName = (Core.Name "qualifiedValueShape")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the qualifiedValueShapesDisjoint field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeQualifiedValueShapesDisjoint :: Phantoms.TypedTerm Model.QualifiedValueShape -> Phantoms.TypedTerm (Maybe Bool)
qualifiedValueShapeQualifiedValueShapesDisjoint x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
        Core.projectionFieldName = (Core.Name "qualifiedValueShapesDisjoint")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the qualifiedMaxCount field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeWithQualifiedMaxCount :: Phantoms.TypedTerm Model.QualifiedValueShape -> Phantoms.TypedTerm Integer -> Phantoms.TypedTerm Model.QualifiedValueShape
qualifiedValueShapeWithQualifiedMaxCount original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShape"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedValueShape")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMaxCount"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedMinCount")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShapesDisjoint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedValueShapesDisjoint")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the qualifiedMinCount field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeWithQualifiedMinCount :: Phantoms.TypedTerm Model.QualifiedValueShape -> Phantoms.TypedTerm Integer -> Phantoms.TypedTerm Model.QualifiedValueShape
qualifiedValueShapeWithQualifiedMinCount original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShape"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedValueShape")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedMaxCount")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMinCount"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShapesDisjoint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedValueShapesDisjoint")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the qualifiedValueShape field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeWithQualifiedValueShape :: Phantoms.TypedTerm Model.QualifiedValueShape -> Phantoms.TypedTerm (Model.Reference Model.Shape) -> Phantoms.TypedTerm Model.QualifiedValueShape
qualifiedValueShapeWithQualifiedValueShape original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShape"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedMaxCount")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedMinCount")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShapesDisjoint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedValueShapesDisjoint")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the qualifiedValueShapesDisjoint field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeWithQualifiedValueShapesDisjoint :: Phantoms.TypedTerm Model.QualifiedValueShape -> Phantoms.TypedTerm (Maybe Bool) -> Phantoms.TypedTerm Model.QualifiedValueShape
qualifiedValueShapeWithQualifiedValueShapesDisjoint original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShape"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedValueShape")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedMaxCount")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedMinCount")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShapesDisjoint"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL injection for the anonymous variant of hydra.shacl.model.Reference
referenceAnonymous :: Phantoms.TypedTerm a -> Phantoms.TypedTerm (Model.Reference a)
referenceAnonymous x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Reference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymous"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the definition variant of hydra.shacl.model.Reference
referenceDefinition :: Phantoms.TypedTerm (Model.Definition a) -> Phantoms.TypedTerm (Model.Reference a)
referenceDefinition x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Reference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "definition"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the named variant of hydra.shacl.model.Reference
referenceNamed :: Phantoms.TypedTerm Syntax.Iri -> Phantoms.TypedTerm (Model.Reference a)
referenceNamed x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Reference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the info variant of hydra.shacl.model.Severity
severityInfo :: Phantoms.TypedTerm Model.Severity
severityInfo =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Severity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "info"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the violation variant of hydra.shacl.model.Severity
severityViolation :: Phantoms.TypedTerm Model.Severity
severityViolation =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Severity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "violation"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the warning variant of hydra.shacl.model.Severity
severityWarning :: Phantoms.TypedTerm Model.Severity
severityWarning =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Severity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "warning"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the node variant of hydra.shacl.model.Shape
shapeNode :: Phantoms.TypedTerm Model.NodeShape -> Phantoms.TypedTerm Model.Shape
shapeNode x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Shape"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "node"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the property variant of hydra.shacl.model.Shape
shapeProperty :: Phantoms.TypedTerm Model.PropertyShape -> Phantoms.TypedTerm Model.Shape
shapeProperty x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Shape"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for the hydra.shacl.model.ShapesGraph wrapper
shapesGraph :: Phantoms.TypedTerm (S.Set (Model.Definition Model.Shape)) -> Phantoms.TypedTerm Model.ShapesGraph
shapesGraph x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shacl.model.ShapesGraph"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.shacl.model.ShapesGraph
unShapesGraph :: Phantoms.TypedTerm Model.ShapesGraph -> Phantoms.TypedTerm (S.Set (Model.Definition Model.Shape))
unShapesGraph x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shacl.model.ShapesGraph")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
