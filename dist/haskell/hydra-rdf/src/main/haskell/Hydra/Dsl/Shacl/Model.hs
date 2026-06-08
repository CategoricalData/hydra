-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.shacl.model

module Hydra.Dsl.Shacl.Model where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Rdf.Syntax as RdfSyntax
import qualified Hydra.Rdf.Syntax as Syntax
import qualified Hydra.Shacl.Model as Model
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Set as S
-- | DSL constructor for hydra.shacl.model.Closed
closed :: Typed.TypedTerm Bool -> Typed.TypedTerm (Maybe (S.Set Syntax.Property)) -> Typed.TypedTerm Model.Closed
closed isClosed ignoredProperties =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Closed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isClosed"),
          Core.fieldTerm = (Typed.unTypedTerm isClosed)},
        Core.Field {
          Core.fieldName = (Core.Name "ignoredProperties"),
          Core.fieldTerm = (Typed.unTypedTerm ignoredProperties)}]}))
-- | DSL accessor for the ignoredProperties field of hydra.shacl.model.Closed
closedIgnoredProperties :: Typed.TypedTerm Model.Closed -> Typed.TypedTerm (Maybe (S.Set Syntax.Property))
closedIgnoredProperties x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Closed"),
        Core.projectionFieldName = (Core.Name "ignoredProperties")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the isClosed field of hydra.shacl.model.Closed
closedIsClosed :: Typed.TypedTerm Model.Closed -> Typed.TypedTerm Bool
closedIsClosed x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Closed"),
        Core.projectionFieldName = (Core.Name "isClosed")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the ignoredProperties field of hydra.shacl.model.Closed
closedWithIgnoredProperties :: Typed.TypedTerm Model.Closed -> Typed.TypedTerm (Maybe (S.Set Syntax.Property)) -> Typed.TypedTerm Model.Closed
closedWithIgnoredProperties original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Closed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isClosed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.Closed"),
              Core.projectionFieldName = (Core.Name "isClosed")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignoredProperties"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the isClosed field of hydra.shacl.model.Closed
closedWithIsClosed :: Typed.TypedTerm Model.Closed -> Typed.TypedTerm Bool -> Typed.TypedTerm Model.Closed
closedWithIsClosed original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Closed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isClosed"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ignoredProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.Closed"),
              Core.projectionFieldName = (Core.Name "ignoredProperties")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the and variant of hydra.shacl.model.CommonConstraint
commonConstraintAnd :: Typed.TypedTerm (S.Set (Model.Reference Model.Shape)) -> Typed.TypedTerm Model.CommonConstraint
commonConstraintAnd x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the class variant of hydra.shacl.model.CommonConstraint
commonConstraintClass :: Typed.TypedTerm (S.Set Syntax.RdfsClass) -> Typed.TypedTerm Model.CommonConstraint
commonConstraintClass x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the closed variant of hydra.shacl.model.CommonConstraint
commonConstraintClosed :: Typed.TypedTerm Model.Closed -> Typed.TypedTerm Model.CommonConstraint
commonConstraintClosed x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "closed"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the datatype variant of hydra.shacl.model.CommonConstraint
commonConstraintDatatype :: Typed.TypedTerm Syntax.Iri -> Typed.TypedTerm Model.CommonConstraint
commonConstraintDatatype x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "datatype"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the disjoint variant of hydra.shacl.model.CommonConstraint
commonConstraintDisjoint :: Typed.TypedTerm (S.Set Syntax.Property) -> Typed.TypedTerm Model.CommonConstraint
commonConstraintDisjoint x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "disjoint"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the equals variant of hydra.shacl.model.CommonConstraint
commonConstraintEquals :: Typed.TypedTerm (S.Set Syntax.Property) -> Typed.TypedTerm Model.CommonConstraint
commonConstraintEquals x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equals"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the hasValue variant of hydra.shacl.model.CommonConstraint
commonConstraintHasValue :: Typed.TypedTerm (S.Set Syntax.Node) -> Typed.TypedTerm Model.CommonConstraint
commonConstraintHasValue x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hasValue"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the in variant of hydra.shacl.model.CommonConstraint
commonConstraintIn :: Typed.TypedTerm [Syntax.Node] -> Typed.TypedTerm Model.CommonConstraint
commonConstraintIn x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the languageIn variant of hydra.shacl.model.CommonConstraint
commonConstraintLanguageIn :: Typed.TypedTerm (S.Set Syntax.LanguageTag) -> Typed.TypedTerm Model.CommonConstraint
commonConstraintLanguageIn x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "languageIn"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the maxExclusive variant of hydra.shacl.model.CommonConstraint
commonConstraintMaxExclusive :: Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm Model.CommonConstraint
commonConstraintMaxExclusive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxExclusive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the maxInclusive variant of hydra.shacl.model.CommonConstraint
commonConstraintMaxInclusive :: Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm Model.CommonConstraint
commonConstraintMaxInclusive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxInclusive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the maxLength variant of hydra.shacl.model.CommonConstraint
commonConstraintMaxLength :: Typed.TypedTerm Integer -> Typed.TypedTerm Model.CommonConstraint
commonConstraintMaxLength x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxLength"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the minExclusive variant of hydra.shacl.model.CommonConstraint
commonConstraintMinExclusive :: Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm Model.CommonConstraint
commonConstraintMinExclusive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minExclusive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the minInclusive variant of hydra.shacl.model.CommonConstraint
commonConstraintMinInclusive :: Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm Model.CommonConstraint
commonConstraintMinInclusive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minInclusive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the minLength variant of hydra.shacl.model.CommonConstraint
commonConstraintMinLength :: Typed.TypedTerm Integer -> Typed.TypedTerm Model.CommonConstraint
commonConstraintMinLength x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minLength"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the node variant of hydra.shacl.model.CommonConstraint
commonConstraintNode :: Typed.TypedTerm (S.Set (Model.Reference Model.NodeShape)) -> Typed.TypedTerm Model.CommonConstraint
commonConstraintNode x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "node"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the nodeKind variant of hydra.shacl.model.CommonConstraint
commonConstraintNodeKind :: Typed.TypedTerm Model.NodeKind -> Typed.TypedTerm Model.CommonConstraint
commonConstraintNodeKind x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nodeKind"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the not variant of hydra.shacl.model.CommonConstraint
commonConstraintNot :: Typed.TypedTerm (S.Set (Model.Reference Model.Shape)) -> Typed.TypedTerm Model.CommonConstraint
commonConstraintNot x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the or variant of hydra.shacl.model.CommonConstraint
commonConstraintOr :: Typed.TypedTerm (S.Set (Model.Reference Model.Shape)) -> Typed.TypedTerm Model.CommonConstraint
commonConstraintOr x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the pattern variant of hydra.shacl.model.CommonConstraint
commonConstraintPattern :: Typed.TypedTerm Model.Pattern -> Typed.TypedTerm Model.CommonConstraint
commonConstraintPattern x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the property variant of hydra.shacl.model.CommonConstraint
commonConstraintProperty :: Typed.TypedTerm (S.Set (Model.Reference Model.PropertyShape)) -> Typed.TypedTerm Model.CommonConstraint
commonConstraintProperty x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the xone variant of hydra.shacl.model.CommonConstraint
commonConstraintXone :: Typed.TypedTerm (S.Set (Model.Reference Model.Shape)) -> Typed.TypedTerm Model.CommonConstraint
commonConstraintXone x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "xone"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.shacl.model.CommonProperties
commonProperties :: Typed.TypedTerm (S.Set Model.CommonConstraint) -> Typed.TypedTerm (Maybe Bool) -> Typed.TypedTerm Syntax.LangStrings -> Typed.TypedTerm Model.Severity -> Typed.TypedTerm (S.Set Syntax.RdfsClass) -> Typed.TypedTerm (S.Set Syntax.IriOrLiteral) -> Typed.TypedTerm (S.Set Syntax.Property) -> Typed.TypedTerm (S.Set Syntax.Property) -> Typed.TypedTerm Model.CommonProperties
commonProperties constraints deactivated message severity targetClass targetNode targetObjectsOf targetSubjectsOf =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Typed.unTypedTerm constraints)},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Typed.unTypedTerm deactivated)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Typed.unTypedTerm message)},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Typed.unTypedTerm severity)},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Typed.unTypedTerm targetClass)},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Typed.unTypedTerm targetNode)},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Typed.unTypedTerm targetObjectsOf)},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Typed.unTypedTerm targetSubjectsOf)}]}))
-- | DSL accessor for the constraints field of hydra.shacl.model.CommonProperties
commonPropertiesConstraints :: Typed.TypedTerm Model.CommonProperties -> Typed.TypedTerm (S.Set Model.CommonConstraint)
commonPropertiesConstraints x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionFieldName = (Core.Name "constraints")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the deactivated field of hydra.shacl.model.CommonProperties
commonPropertiesDeactivated :: Typed.TypedTerm Model.CommonProperties -> Typed.TypedTerm (Maybe Bool)
commonPropertiesDeactivated x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionFieldName = (Core.Name "deactivated")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the message field of hydra.shacl.model.CommonProperties
commonPropertiesMessage :: Typed.TypedTerm Model.CommonProperties -> Typed.TypedTerm Syntax.LangStrings
commonPropertiesMessage x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionFieldName = (Core.Name "message")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the severity field of hydra.shacl.model.CommonProperties
commonPropertiesSeverity :: Typed.TypedTerm Model.CommonProperties -> Typed.TypedTerm Model.Severity
commonPropertiesSeverity x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionFieldName = (Core.Name "severity")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the targetClass field of hydra.shacl.model.CommonProperties
commonPropertiesTargetClass :: Typed.TypedTerm Model.CommonProperties -> Typed.TypedTerm (S.Set Syntax.RdfsClass)
commonPropertiesTargetClass x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionFieldName = (Core.Name "targetClass")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the targetNode field of hydra.shacl.model.CommonProperties
commonPropertiesTargetNode :: Typed.TypedTerm Model.CommonProperties -> Typed.TypedTerm (S.Set Syntax.IriOrLiteral)
commonPropertiesTargetNode x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionFieldName = (Core.Name "targetNode")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the targetObjectsOf field of hydra.shacl.model.CommonProperties
commonPropertiesTargetObjectsOf :: Typed.TypedTerm Model.CommonProperties -> Typed.TypedTerm (S.Set Syntax.Property)
commonPropertiesTargetObjectsOf x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionFieldName = (Core.Name "targetObjectsOf")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the targetSubjectsOf field of hydra.shacl.model.CommonProperties
commonPropertiesTargetSubjectsOf :: Typed.TypedTerm Model.CommonProperties -> Typed.TypedTerm (S.Set Syntax.Property)
commonPropertiesTargetSubjectsOf x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionFieldName = (Core.Name "targetSubjectsOf")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the constraints field of hydra.shacl.model.CommonProperties
commonPropertiesWithConstraints :: Typed.TypedTerm Model.CommonProperties -> Typed.TypedTerm (S.Set Model.CommonConstraint) -> Typed.TypedTerm Model.CommonProperties
commonPropertiesWithConstraints original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "deactivated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "severity")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetClass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the deactivated field of hydra.shacl.model.CommonProperties
commonPropertiesWithDeactivated :: Typed.TypedTerm Model.CommonProperties -> Typed.TypedTerm (Maybe Bool) -> Typed.TypedTerm Model.CommonProperties
commonPropertiesWithDeactivated original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "severity")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetClass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the message field of hydra.shacl.model.CommonProperties
commonPropertiesWithMessage :: Typed.TypedTerm Model.CommonProperties -> Typed.TypedTerm Syntax.LangStrings -> Typed.TypedTerm Model.CommonProperties
commonPropertiesWithMessage original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "deactivated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "severity")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetClass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the severity field of hydra.shacl.model.CommonProperties
commonPropertiesWithSeverity :: Typed.TypedTerm Model.CommonProperties -> Typed.TypedTerm Model.Severity -> Typed.TypedTerm Model.CommonProperties
commonPropertiesWithSeverity original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "deactivated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetClass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the targetClass field of hydra.shacl.model.CommonProperties
commonPropertiesWithTargetClass :: Typed.TypedTerm Model.CommonProperties -> Typed.TypedTerm (S.Set Syntax.RdfsClass) -> Typed.TypedTerm Model.CommonProperties
commonPropertiesWithTargetClass original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "deactivated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "severity")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the targetNode field of hydra.shacl.model.CommonProperties
commonPropertiesWithTargetNode :: Typed.TypedTerm Model.CommonProperties -> Typed.TypedTerm (S.Set Syntax.IriOrLiteral) -> Typed.TypedTerm Model.CommonProperties
commonPropertiesWithTargetNode original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "deactivated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "severity")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetClass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the targetObjectsOf field of hydra.shacl.model.CommonProperties
commonPropertiesWithTargetObjectsOf :: Typed.TypedTerm Model.CommonProperties -> Typed.TypedTerm (S.Set Syntax.Property) -> Typed.TypedTerm Model.CommonProperties
commonPropertiesWithTargetObjectsOf original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "deactivated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "severity")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetClass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the targetSubjectsOf field of hydra.shacl.model.CommonProperties
commonPropertiesWithTargetSubjectsOf :: Typed.TypedTerm Model.CommonProperties -> Typed.TypedTerm (S.Set Syntax.Property) -> Typed.TypedTerm Model.CommonProperties
commonPropertiesWithTargetSubjectsOf original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "deactivated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "severity")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetClass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionFieldName = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.shacl.model.Definition
definition :: Typed.TypedTerm Syntax.Iri -> Typed.TypedTerm a -> Typed.TypedTerm (Model.Definition a)
definition iri target =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Typed.unTypedTerm iri)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Typed.unTypedTerm target)}]}))
-- | DSL accessor for the iri field of hydra.shacl.model.Definition
definitionIri :: Typed.TypedTerm (Model.Definition a) -> Typed.TypedTerm Syntax.Iri
definitionIri x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Definition"),
        Core.projectionFieldName = (Core.Name "iri")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the target field of hydra.shacl.model.Definition
definitionTarget :: Typed.TypedTerm (Model.Definition a) -> Typed.TypedTerm a
definitionTarget x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Definition"),
        Core.projectionFieldName = (Core.Name "target")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the iri field of hydra.shacl.model.Definition
definitionWithIri :: Typed.TypedTerm (Model.Definition a) -> Typed.TypedTerm Syntax.Iri -> Typed.TypedTerm (Model.Definition a)
definitionWithIri original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.Definition"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the target field of hydra.shacl.model.Definition
definitionWithTarget :: Typed.TypedTerm (Model.Definition a) -> Typed.TypedTerm a -> Typed.TypedTerm (Model.Definition a)
definitionWithTarget original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.Definition"),
              Core.projectionFieldName = (Core.Name "iri")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the blankNode variant of hydra.shacl.model.NodeKind
nodeKindBlankNode :: Typed.TypedTerm Model.NodeKind
nodeKindBlankNode =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "blankNode"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the blankNodeOrIri variant of hydra.shacl.model.NodeKind
nodeKindBlankNodeOrIri :: Typed.TypedTerm Model.NodeKind
nodeKindBlankNodeOrIri =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "blankNodeOrIri"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the blankNodeOrLiteral variant of hydra.shacl.model.NodeKind
nodeKindBlankNodeOrLiteral :: Typed.TypedTerm Model.NodeKind
nodeKindBlankNodeOrLiteral =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "blankNodeOrLiteral"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the iri variant of hydra.shacl.model.NodeKind
nodeKindIri :: Typed.TypedTerm Model.NodeKind
nodeKindIri =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iri"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the iriOrLiteral variant of hydra.shacl.model.NodeKind
nodeKindIriOrLiteral :: Typed.TypedTerm Model.NodeKind
nodeKindIriOrLiteral =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iriOrLiteral"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the literal variant of hydra.shacl.model.NodeKind
nodeKindLiteral :: Typed.TypedTerm Model.NodeKind
nodeKindLiteral =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.shacl.model.NodeShape
nodeShape :: Typed.TypedTerm Model.CommonProperties -> Typed.TypedTerm Model.NodeShape
nodeShape common =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.NodeShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Typed.unTypedTerm common)}]}))
-- | DSL accessor for the common field of hydra.shacl.model.NodeShape
nodeShapeCommon :: Typed.TypedTerm Model.NodeShape -> Typed.TypedTerm Model.CommonProperties
nodeShapeCommon x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.NodeShape"),
        Core.projectionFieldName = (Core.Name "common")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the common field of hydra.shacl.model.NodeShape
nodeShapeWithCommon :: Typed.TypedTerm Model.NodeShape -> Typed.TypedTerm Model.CommonProperties -> Typed.TypedTerm Model.NodeShape
nodeShapeWithCommon original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.NodeShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.shacl.model.Pattern
pattern :: Typed.TypedTerm String -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Model.Pattern
pattern regex flags =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Pattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "regex"),
          Core.fieldTerm = (Typed.unTypedTerm regex)},
        Core.Field {
          Core.fieldName = (Core.Name "flags"),
          Core.fieldTerm = (Typed.unTypedTerm flags)}]}))
-- | DSL accessor for the flags field of hydra.shacl.model.Pattern
patternFlags :: Typed.TypedTerm Model.Pattern -> Typed.TypedTerm (Maybe String)
patternFlags x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Pattern"),
        Core.projectionFieldName = (Core.Name "flags")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the regex field of hydra.shacl.model.Pattern
patternRegex :: Typed.TypedTerm Model.Pattern -> Typed.TypedTerm String
patternRegex x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Pattern"),
        Core.projectionFieldName = (Core.Name "regex")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the flags field of hydra.shacl.model.Pattern
patternWithFlags :: Typed.TypedTerm Model.Pattern -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Model.Pattern
patternWithFlags original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Pattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "regex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.Pattern"),
              Core.projectionFieldName = (Core.Name "regex")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "flags"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the regex field of hydra.shacl.model.Pattern
patternWithRegex :: Typed.TypedTerm Model.Pattern -> Typed.TypedTerm String -> Typed.TypedTerm Model.Pattern
patternWithRegex original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Pattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "regex"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "flags"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.Pattern"),
              Core.projectionFieldName = (Core.Name "flags")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.shacl.model.PropertyShape
propertyShape :: Typed.TypedTerm Model.CommonProperties -> Typed.TypedTerm (S.Set Model.PropertyShapeConstraint) -> Typed.TypedTerm (Maybe Syntax.Node) -> Typed.TypedTerm Syntax.LangStrings -> Typed.TypedTerm Syntax.LangStrings -> Typed.TypedTerm (Maybe Integer) -> Typed.TypedTerm Syntax.Iri -> Typed.TypedTerm Model.PropertyShape
propertyShape common constraints defaultValue description name order path =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Typed.unTypedTerm common)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Typed.unTypedTerm constraints)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Typed.unTypedTerm defaultValue)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Typed.unTypedTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Typed.unTypedTerm order)},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Typed.unTypedTerm path)}]}))
-- | DSL accessor for the common field of hydra.shacl.model.PropertyShape
propertyShapeCommon :: Typed.TypedTerm Model.PropertyShape -> Typed.TypedTerm Model.CommonProperties
propertyShapeCommon x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionFieldName = (Core.Name "common")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the lessThan variant of hydra.shacl.model.PropertyShapeConstraint
propertyShapeConstraintLessThan :: Typed.TypedTerm (S.Set Syntax.Property) -> Typed.TypedTerm Model.PropertyShapeConstraint
propertyShapeConstraintLessThan x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThan"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the lessThanOrEquals variant of hydra.shacl.model.PropertyShapeConstraint
propertyShapeConstraintLessThanOrEquals :: Typed.TypedTerm (S.Set Syntax.Property) -> Typed.TypedTerm Model.PropertyShapeConstraint
propertyShapeConstraintLessThanOrEquals x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThanOrEquals"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the maxCount variant of hydra.shacl.model.PropertyShapeConstraint
propertyShapeConstraintMaxCount :: Typed.TypedTerm Integer -> Typed.TypedTerm Model.PropertyShapeConstraint
propertyShapeConstraintMaxCount x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxCount"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the minCount variant of hydra.shacl.model.PropertyShapeConstraint
propertyShapeConstraintMinCount :: Typed.TypedTerm Integer -> Typed.TypedTerm Model.PropertyShapeConstraint
propertyShapeConstraintMinCount x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minCount"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the qualifiedValueShape variant of hydra.shacl.model.PropertyShapeConstraint
propertyShapeConstraintQualifiedValueShape :: Typed.TypedTerm Model.QualifiedValueShape -> Typed.TypedTerm Model.PropertyShapeConstraint
propertyShapeConstraintQualifiedValueShape x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualifiedValueShape"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the uniqueLang variant of hydra.shacl.model.PropertyShapeConstraint
propertyShapeConstraintUniqueLang :: Typed.TypedTerm Bool -> Typed.TypedTerm Model.PropertyShapeConstraint
propertyShapeConstraintUniqueLang x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uniqueLang"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL accessor for the constraints field of hydra.shacl.model.PropertyShape
propertyShapeConstraints :: Typed.TypedTerm Model.PropertyShape -> Typed.TypedTerm (S.Set Model.PropertyShapeConstraint)
propertyShapeConstraints x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionFieldName = (Core.Name "constraints")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the defaultValue field of hydra.shacl.model.PropertyShape
propertyShapeDefaultValue :: Typed.TypedTerm Model.PropertyShape -> Typed.TypedTerm (Maybe Syntax.Node)
propertyShapeDefaultValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionFieldName = (Core.Name "defaultValue")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the description field of hydra.shacl.model.PropertyShape
propertyShapeDescription :: Typed.TypedTerm Model.PropertyShape -> Typed.TypedTerm Syntax.LangStrings
propertyShapeDescription x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionFieldName = (Core.Name "description")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.shacl.model.PropertyShape
propertyShapeName :: Typed.TypedTerm Model.PropertyShape -> Typed.TypedTerm Syntax.LangStrings
propertyShapeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the order field of hydra.shacl.model.PropertyShape
propertyShapeOrder :: Typed.TypedTerm Model.PropertyShape -> Typed.TypedTerm (Maybe Integer)
propertyShapeOrder x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionFieldName = (Core.Name "order")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the path field of hydra.shacl.model.PropertyShape
propertyShapePath :: Typed.TypedTerm Model.PropertyShape -> Typed.TypedTerm Syntax.Iri
propertyShapePath x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionFieldName = (Core.Name "path")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the common field of hydra.shacl.model.PropertyShape
propertyShapeWithCommon :: Typed.TypedTerm Model.PropertyShape -> Typed.TypedTerm Model.CommonProperties -> Typed.TypedTerm Model.PropertyShape
propertyShapeWithCommon original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "defaultValue")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "order")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the constraints field of hydra.shacl.model.PropertyShape
propertyShapeWithConstraints :: Typed.TypedTerm Model.PropertyShape -> Typed.TypedTerm (S.Set Model.PropertyShapeConstraint) -> Typed.TypedTerm Model.PropertyShape
propertyShapeWithConstraints original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "common")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "defaultValue")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "order")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the defaultValue field of hydra.shacl.model.PropertyShape
propertyShapeWithDefaultValue :: Typed.TypedTerm Model.PropertyShape -> Typed.TypedTerm (Maybe Syntax.Node) -> Typed.TypedTerm Model.PropertyShape
propertyShapeWithDefaultValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "common")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "order")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the description field of hydra.shacl.model.PropertyShape
propertyShapeWithDescription :: Typed.TypedTerm Model.PropertyShape -> Typed.TypedTerm Syntax.LangStrings -> Typed.TypedTerm Model.PropertyShape
propertyShapeWithDescription original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "common")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "defaultValue")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "order")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.shacl.model.PropertyShape
propertyShapeWithName :: Typed.TypedTerm Model.PropertyShape -> Typed.TypedTerm Syntax.LangStrings -> Typed.TypedTerm Model.PropertyShape
propertyShapeWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "common")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "defaultValue")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "order")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the order field of hydra.shacl.model.PropertyShape
propertyShapeWithOrder :: Typed.TypedTerm Model.PropertyShape -> Typed.TypedTerm (Maybe Integer) -> Typed.TypedTerm Model.PropertyShape
propertyShapeWithOrder original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "common")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "defaultValue")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the path field of hydra.shacl.model.PropertyShape
propertyShapeWithPath :: Typed.TypedTerm Model.PropertyShape -> Typed.TypedTerm Syntax.Iri -> Typed.TypedTerm Model.PropertyShape
propertyShapeWithPath original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "common")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "defaultValue")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionFieldName = (Core.Name "order")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.shacl.model.QualifiedValueShape
qualifiedValueShape :: Typed.TypedTerm (Model.Reference Model.Shape) -> Typed.TypedTerm Integer -> Typed.TypedTerm Integer -> Typed.TypedTerm (Maybe Bool) -> Typed.TypedTerm Model.QualifiedValueShape
qualifiedValueShape qualifiedValueShape qualifiedMaxCount qualifiedMinCount qualifiedValueShapesDisjoint =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShape"),
          Core.fieldTerm = (Typed.unTypedTerm qualifiedValueShape)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMaxCount"),
          Core.fieldTerm = (Typed.unTypedTerm qualifiedMaxCount)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMinCount"),
          Core.fieldTerm = (Typed.unTypedTerm qualifiedMinCount)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShapesDisjoint"),
          Core.fieldTerm = (Typed.unTypedTerm qualifiedValueShapesDisjoint)}]}))
-- | DSL accessor for the qualifiedMaxCount field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeQualifiedMaxCount :: Typed.TypedTerm Model.QualifiedValueShape -> Typed.TypedTerm Integer
qualifiedValueShapeQualifiedMaxCount x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
        Core.projectionFieldName = (Core.Name "qualifiedMaxCount")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the qualifiedMinCount field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeQualifiedMinCount :: Typed.TypedTerm Model.QualifiedValueShape -> Typed.TypedTerm Integer
qualifiedValueShapeQualifiedMinCount x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
        Core.projectionFieldName = (Core.Name "qualifiedMinCount")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the qualifiedValueShape field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeQualifiedValueShape :: Typed.TypedTerm Model.QualifiedValueShape -> Typed.TypedTerm (Model.Reference Model.Shape)
qualifiedValueShapeQualifiedValueShape x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
        Core.projectionFieldName = (Core.Name "qualifiedValueShape")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the qualifiedValueShapesDisjoint field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeQualifiedValueShapesDisjoint :: Typed.TypedTerm Model.QualifiedValueShape -> Typed.TypedTerm (Maybe Bool)
qualifiedValueShapeQualifiedValueShapesDisjoint x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
        Core.projectionFieldName = (Core.Name "qualifiedValueShapesDisjoint")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the qualifiedMaxCount field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeWithQualifiedMaxCount :: Typed.TypedTerm Model.QualifiedValueShape -> Typed.TypedTerm Integer -> Typed.TypedTerm Model.QualifiedValueShape
qualifiedValueShapeWithQualifiedMaxCount original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShape"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedValueShape")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMaxCount"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedMinCount")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShapesDisjoint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedValueShapesDisjoint")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the qualifiedMinCount field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeWithQualifiedMinCount :: Typed.TypedTerm Model.QualifiedValueShape -> Typed.TypedTerm Integer -> Typed.TypedTerm Model.QualifiedValueShape
qualifiedValueShapeWithQualifiedMinCount original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShape"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedValueShape")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedMaxCount")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMinCount"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShapesDisjoint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedValueShapesDisjoint")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the qualifiedValueShape field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeWithQualifiedValueShape :: Typed.TypedTerm Model.QualifiedValueShape -> Typed.TypedTerm (Model.Reference Model.Shape) -> Typed.TypedTerm Model.QualifiedValueShape
qualifiedValueShapeWithQualifiedValueShape original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShape"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedMaxCount")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedMinCount")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShapesDisjoint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedValueShapesDisjoint")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the qualifiedValueShapesDisjoint field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeWithQualifiedValueShapesDisjoint :: Typed.TypedTerm Model.QualifiedValueShape -> Typed.TypedTerm (Maybe Bool) -> Typed.TypedTerm Model.QualifiedValueShape
qualifiedValueShapeWithQualifiedValueShapesDisjoint original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShape"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedValueShape")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedMaxCount")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionFieldName = (Core.Name "qualifiedMinCount")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShapesDisjoint"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the anonymous variant of hydra.shacl.model.Reference
referenceAnonymous :: Typed.TypedTerm a -> Typed.TypedTerm (Model.Reference a)
referenceAnonymous x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Reference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymous"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the definition variant of hydra.shacl.model.Reference
referenceDefinition :: Typed.TypedTerm (Model.Definition a) -> Typed.TypedTerm (Model.Reference a)
referenceDefinition x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Reference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "definition"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the named variant of hydra.shacl.model.Reference
referenceNamed :: Typed.TypedTerm Syntax.Iri -> Typed.TypedTerm (Model.Reference a)
referenceNamed x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Reference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the info variant of hydra.shacl.model.Severity
severityInfo :: Typed.TypedTerm Model.Severity
severityInfo =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Severity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "info"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the violation variant of hydra.shacl.model.Severity
severityViolation :: Typed.TypedTerm Model.Severity
severityViolation =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Severity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "violation"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the warning variant of hydra.shacl.model.Severity
severityWarning :: Typed.TypedTerm Model.Severity
severityWarning =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Severity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "warning"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the node variant of hydra.shacl.model.Shape
shapeNode :: Typed.TypedTerm Model.NodeShape -> Typed.TypedTerm Model.Shape
shapeNode x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Shape"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "node"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the property variant of hydra.shacl.model.Shape
shapeProperty :: Typed.TypedTerm Model.PropertyShape -> Typed.TypedTerm Model.Shape
shapeProperty x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Shape"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.shacl.model.ShapesGraph wrapper
shapesGraph :: Typed.TypedTerm (S.Set (Model.Definition Model.Shape)) -> Typed.TypedTerm Model.ShapesGraph
shapesGraph x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shacl.model.ShapesGraph"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.shacl.model.ShapesGraph
unShapesGraph :: Typed.TypedTerm Model.ShapesGraph -> Typed.TypedTerm (S.Set (Model.Definition Model.Shape))
unShapesGraph x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shacl.model.ShapesGraph")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
