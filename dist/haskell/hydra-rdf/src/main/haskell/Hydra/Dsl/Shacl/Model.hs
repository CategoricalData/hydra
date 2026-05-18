-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.shacl.model

module Hydra.Dsl.Shacl.Model where
import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Rdf.Syntax as Syntax
import qualified Hydra.Shacl.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Set as S
-- | DSL constructor for hydra.shacl.model.Closed
closed :: Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe (S.Set Syntax.Property)) -> Phantoms.TTerm Model.Closed
closed isClosed ignoredProperties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Closed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isClosed"),
          Core.fieldTerm = (Phantoms.unTTerm isClosed)},
        Core.Field {
          Core.fieldName = (Core.Name "ignoredProperties"),
          Core.fieldTerm = (Phantoms.unTTerm ignoredProperties)}]}))
-- | DSL accessor for the ignoredProperties field of hydra.shacl.model.Closed
closedIgnoredProperties :: Phantoms.TTerm Model.Closed -> Phantoms.TTerm (Maybe (S.Set Syntax.Property))
closedIgnoredProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Closed"),
        Core.projectionField = (Core.Name "ignoredProperties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the isClosed field of hydra.shacl.model.Closed
closedIsClosed :: Phantoms.TTerm Model.Closed -> Phantoms.TTerm Bool
closedIsClosed x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Closed"),
        Core.projectionField = (Core.Name "isClosed")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the ignoredProperties field of hydra.shacl.model.Closed
closedWithIgnoredProperties :: Phantoms.TTerm Model.Closed -> Phantoms.TTerm (Maybe (S.Set Syntax.Property)) -> Phantoms.TTerm Model.Closed
closedWithIgnoredProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Closed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isClosed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.Closed"),
              Core.projectionField = (Core.Name "isClosed")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignoredProperties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the isClosed field of hydra.shacl.model.Closed
closedWithIsClosed :: Phantoms.TTerm Model.Closed -> Phantoms.TTerm Bool -> Phantoms.TTerm Model.Closed
closedWithIsClosed original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Closed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isClosed"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ignoredProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.Closed"),
              Core.projectionField = (Core.Name "ignoredProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the and variant of hydra.shacl.model.CommonConstraint
commonConstraintAnd :: Phantoms.TTerm (S.Set (Model.Reference Model.Shape)) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintAnd x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the class variant of hydra.shacl.model.CommonConstraint
commonConstraintClass :: Phantoms.TTerm (S.Set Syntax.RdfsClass) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintClass x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the closed variant of hydra.shacl.model.CommonConstraint
commonConstraintClosed :: Phantoms.TTerm Model.Closed -> Phantoms.TTerm Model.CommonConstraint
commonConstraintClosed x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "closed"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the datatype variant of hydra.shacl.model.CommonConstraint
commonConstraintDatatype :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Model.CommonConstraint
commonConstraintDatatype x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "datatype"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the disjoint variant of hydra.shacl.model.CommonConstraint
commonConstraintDisjoint :: Phantoms.TTerm (S.Set Syntax.Property) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintDisjoint x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "disjoint"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the equals variant of hydra.shacl.model.CommonConstraint
commonConstraintEquals :: Phantoms.TTerm (S.Set Syntax.Property) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintEquals x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equals"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the hasValue variant of hydra.shacl.model.CommonConstraint
commonConstraintHasValue :: Phantoms.TTerm (S.Set Syntax.Node) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintHasValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hasValue"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the in variant of hydra.shacl.model.CommonConstraint
commonConstraintIn :: Phantoms.TTerm [Syntax.Node] -> Phantoms.TTerm Model.CommonConstraint
commonConstraintIn x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the languageIn variant of hydra.shacl.model.CommonConstraint
commonConstraintLanguageIn :: Phantoms.TTerm (S.Set Syntax.LanguageTag) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintLanguageIn x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "languageIn"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the maxExclusive variant of hydra.shacl.model.CommonConstraint
commonConstraintMaxExclusive :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Model.CommonConstraint
commonConstraintMaxExclusive x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxExclusive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the maxInclusive variant of hydra.shacl.model.CommonConstraint
commonConstraintMaxInclusive :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Model.CommonConstraint
commonConstraintMaxInclusive x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxInclusive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the maxLength variant of hydra.shacl.model.CommonConstraint
commonConstraintMaxLength :: Phantoms.TTerm Integer -> Phantoms.TTerm Model.CommonConstraint
commonConstraintMaxLength x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxLength"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the minExclusive variant of hydra.shacl.model.CommonConstraint
commonConstraintMinExclusive :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Model.CommonConstraint
commonConstraintMinExclusive x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minExclusive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the minInclusive variant of hydra.shacl.model.CommonConstraint
commonConstraintMinInclusive :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Model.CommonConstraint
commonConstraintMinInclusive x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minInclusive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the minLength variant of hydra.shacl.model.CommonConstraint
commonConstraintMinLength :: Phantoms.TTerm Integer -> Phantoms.TTerm Model.CommonConstraint
commonConstraintMinLength x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minLength"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the node variant of hydra.shacl.model.CommonConstraint
commonConstraintNode :: Phantoms.TTerm (S.Set (Model.Reference Model.NodeShape)) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintNode x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "node"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the nodeKind variant of hydra.shacl.model.CommonConstraint
commonConstraintNodeKind :: Phantoms.TTerm Model.NodeKind -> Phantoms.TTerm Model.CommonConstraint
commonConstraintNodeKind x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nodeKind"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the not variant of hydra.shacl.model.CommonConstraint
commonConstraintNot :: Phantoms.TTerm (S.Set (Model.Reference Model.Shape)) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintNot x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the or variant of hydra.shacl.model.CommonConstraint
commonConstraintOr :: Phantoms.TTerm (S.Set (Model.Reference Model.Shape)) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintOr x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the pattern variant of hydra.shacl.model.CommonConstraint
commonConstraintPattern :: Phantoms.TTerm Model.Pattern -> Phantoms.TTerm Model.CommonConstraint
commonConstraintPattern x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the property variant of hydra.shacl.model.CommonConstraint
commonConstraintProperty :: Phantoms.TTerm (S.Set (Model.Reference Model.PropertyShape)) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintProperty x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the xone variant of hydra.shacl.model.CommonConstraint
commonConstraintXone :: Phantoms.TTerm (S.Set (Model.Reference Model.Shape)) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintXone x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "xone"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.shacl.model.CommonProperties
commonProperties :: Phantoms.TTerm (S.Set Model.CommonConstraint) -> Phantoms.TTerm (Maybe Bool) -> Phantoms.TTerm Syntax.LangStrings -> Phantoms.TTerm Model.Severity -> Phantoms.TTerm (S.Set Syntax.RdfsClass) -> Phantoms.TTerm (S.Set Syntax.IriOrLiteral) -> Phantoms.TTerm (S.Set Syntax.Property) -> Phantoms.TTerm (S.Set Syntax.Property) -> Phantoms.TTerm Model.CommonProperties
commonProperties constraints deactivated message severity targetClass targetNode targetObjectsOf targetSubjectsOf =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTTerm constraints)},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Phantoms.unTTerm deactivated)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Phantoms.unTTerm message)},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Phantoms.unTTerm severity)},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Phantoms.unTTerm targetClass)},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Phantoms.unTTerm targetNode)},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Phantoms.unTTerm targetObjectsOf)},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Phantoms.unTTerm targetSubjectsOf)}]}))
-- | DSL accessor for the constraints field of hydra.shacl.model.CommonProperties
commonPropertiesConstraints :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm (S.Set Model.CommonConstraint)
commonPropertiesConstraints x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionField = (Core.Name "constraints")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the deactivated field of hydra.shacl.model.CommonProperties
commonPropertiesDeactivated :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm (Maybe Bool)
commonPropertiesDeactivated x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionField = (Core.Name "deactivated")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the message field of hydra.shacl.model.CommonProperties
commonPropertiesMessage :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm Syntax.LangStrings
commonPropertiesMessage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionField = (Core.Name "message")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the severity field of hydra.shacl.model.CommonProperties
commonPropertiesSeverity :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm Model.Severity
commonPropertiesSeverity x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionField = (Core.Name "severity")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the targetClass field of hydra.shacl.model.CommonProperties
commonPropertiesTargetClass :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm (S.Set Syntax.RdfsClass)
commonPropertiesTargetClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionField = (Core.Name "targetClass")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the targetNode field of hydra.shacl.model.CommonProperties
commonPropertiesTargetNode :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm (S.Set Syntax.IriOrLiteral)
commonPropertiesTargetNode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionField = (Core.Name "targetNode")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the targetObjectsOf field of hydra.shacl.model.CommonProperties
commonPropertiesTargetObjectsOf :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm (S.Set Syntax.Property)
commonPropertiesTargetObjectsOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionField = (Core.Name "targetObjectsOf")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the targetSubjectsOf field of hydra.shacl.model.CommonProperties
commonPropertiesTargetSubjectsOf :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm (S.Set Syntax.Property)
commonPropertiesTargetSubjectsOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionField = (Core.Name "targetSubjectsOf")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the constraints field of hydra.shacl.model.CommonProperties
commonPropertiesWithConstraints :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm (S.Set Model.CommonConstraint) -> Phantoms.TTerm Model.CommonProperties
commonPropertiesWithConstraints original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "deactivated")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetClass")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetNode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the deactivated field of hydra.shacl.model.CommonProperties
commonPropertiesWithDeactivated :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm (Maybe Bool) -> Phantoms.TTerm Model.CommonProperties
commonPropertiesWithDeactivated original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetClass")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetNode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the message field of hydra.shacl.model.CommonProperties
commonPropertiesWithMessage :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm Syntax.LangStrings -> Phantoms.TTerm Model.CommonProperties
commonPropertiesWithMessage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "deactivated")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetClass")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetNode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the severity field of hydra.shacl.model.CommonProperties
commonPropertiesWithSeverity :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm Model.Severity -> Phantoms.TTerm Model.CommonProperties
commonPropertiesWithSeverity original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "deactivated")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetClass")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetNode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the targetClass field of hydra.shacl.model.CommonProperties
commonPropertiesWithTargetClass :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm (S.Set Syntax.RdfsClass) -> Phantoms.TTerm Model.CommonProperties
commonPropertiesWithTargetClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "deactivated")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetNode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the targetNode field of hydra.shacl.model.CommonProperties
commonPropertiesWithTargetNode :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm (S.Set Syntax.IriOrLiteral) -> Phantoms.TTerm Model.CommonProperties
commonPropertiesWithTargetNode original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "deactivated")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetClass")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the targetObjectsOf field of hydra.shacl.model.CommonProperties
commonPropertiesWithTargetObjectsOf :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm (S.Set Syntax.Property) -> Phantoms.TTerm Model.CommonProperties
commonPropertiesWithTargetObjectsOf original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "deactivated")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetClass")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetNode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetSubjectsOf")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the targetSubjectsOf field of hydra.shacl.model.CommonProperties
commonPropertiesWithTargetSubjectsOf :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm (S.Set Syntax.Property) -> Phantoms.TTerm Model.CommonProperties
commonPropertiesWithTargetSubjectsOf original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deactivated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "deactivated")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "severity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "severity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetClass")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetNode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetObjectsOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
              Core.projectionField = (Core.Name "targetObjectsOf")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targetSubjectsOf"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.shacl.model.Definition
definition :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm a -> Phantoms.TTerm (Model.Definition a)
definition iri target =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Phantoms.unTTerm iri)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm target)}]}))
-- | DSL accessor for the iri field of hydra.shacl.model.Definition
definitionIri :: Phantoms.TTerm (Model.Definition a) -> Phantoms.TTerm Syntax.Iri
definitionIri x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Definition"),
        Core.projectionField = (Core.Name "iri")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the target field of hydra.shacl.model.Definition
definitionTarget :: Phantoms.TTerm (Model.Definition a) -> Phantoms.TTerm a
definitionTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Definition"),
        Core.projectionField = (Core.Name "target")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the iri field of hydra.shacl.model.Definition
definitionWithIri :: Phantoms.TTerm (Model.Definition a) -> Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm (Model.Definition a)
definitionWithIri original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.Definition"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the target field of hydra.shacl.model.Definition
definitionWithTarget :: Phantoms.TTerm (Model.Definition a) -> Phantoms.TTerm a -> Phantoms.TTerm (Model.Definition a)
definitionWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.Definition"),
              Core.projectionField = (Core.Name "iri")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the blankNode variant of hydra.shacl.model.NodeKind
nodeKindBlankNode :: Phantoms.TTerm Model.NodeKind
nodeKindBlankNode =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "blankNode"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the blankNodeOrIri variant of hydra.shacl.model.NodeKind
nodeKindBlankNodeOrIri :: Phantoms.TTerm Model.NodeKind
nodeKindBlankNodeOrIri =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "blankNodeOrIri"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the blankNodeOrLiteral variant of hydra.shacl.model.NodeKind
nodeKindBlankNodeOrLiteral :: Phantoms.TTerm Model.NodeKind
nodeKindBlankNodeOrLiteral =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "blankNodeOrLiteral"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the iri variant of hydra.shacl.model.NodeKind
nodeKindIri :: Phantoms.TTerm Model.NodeKind
nodeKindIri =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iri"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the iriOrLiteral variant of hydra.shacl.model.NodeKind
nodeKindIriOrLiteral :: Phantoms.TTerm Model.NodeKind
nodeKindIriOrLiteral =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iriOrLiteral"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the literal variant of hydra.shacl.model.NodeKind
nodeKindLiteral :: Phantoms.TTerm Model.NodeKind
nodeKindLiteral =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.shacl.model.NodeShape
nodeShape :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm Model.NodeShape
nodeShape common =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.NodeShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Phantoms.unTTerm common)}]}))
-- | DSL accessor for the common field of hydra.shacl.model.NodeShape
nodeShapeCommon :: Phantoms.TTerm Model.NodeShape -> Phantoms.TTerm Model.CommonProperties
nodeShapeCommon x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.NodeShape"),
        Core.projectionField = (Core.Name "common")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the common field of hydra.shacl.model.NodeShape
nodeShapeWithCommon :: Phantoms.TTerm Model.NodeShape -> Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm Model.NodeShape
nodeShapeWithCommon original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.NodeShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.shacl.model.Pattern
pattern :: Phantoms.TTerm String -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Model.Pattern
pattern regex flags =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Pattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "regex"),
          Core.fieldTerm = (Phantoms.unTTerm regex)},
        Core.Field {
          Core.fieldName = (Core.Name "flags"),
          Core.fieldTerm = (Phantoms.unTTerm flags)}]}))
-- | DSL accessor for the flags field of hydra.shacl.model.Pattern
patternFlags :: Phantoms.TTerm Model.Pattern -> Phantoms.TTerm (Maybe String)
patternFlags x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Pattern"),
        Core.projectionField = (Core.Name "flags")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the regex field of hydra.shacl.model.Pattern
patternRegex :: Phantoms.TTerm Model.Pattern -> Phantoms.TTerm String
patternRegex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Pattern"),
        Core.projectionField = (Core.Name "regex")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the flags field of hydra.shacl.model.Pattern
patternWithFlags :: Phantoms.TTerm Model.Pattern -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Model.Pattern
patternWithFlags original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Pattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "regex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.Pattern"),
              Core.projectionField = (Core.Name "regex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "flags"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the regex field of hydra.shacl.model.Pattern
patternWithRegex :: Phantoms.TTerm Model.Pattern -> Phantoms.TTerm String -> Phantoms.TTerm Model.Pattern
patternWithRegex original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.Pattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "regex"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "flags"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.Pattern"),
              Core.projectionField = (Core.Name "flags")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.shacl.model.PropertyShape
propertyShape :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm (S.Set Model.PropertyShapeConstraint) -> Phantoms.TTerm (Maybe Syntax.Node) -> Phantoms.TTerm Syntax.LangStrings -> Phantoms.TTerm Syntax.LangStrings -> Phantoms.TTerm (Maybe Integer) -> Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Model.PropertyShape
propertyShape common constraints defaultValue description name order path =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Phantoms.unTTerm common)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTTerm constraints)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Phantoms.unTTerm defaultValue)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Phantoms.unTTerm order)},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm path)}]}))
-- | DSL accessor for the common field of hydra.shacl.model.PropertyShape
propertyShapeCommon :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm Model.CommonProperties
propertyShapeCommon x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionField = (Core.Name "common")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL injection for the lessThan variant of hydra.shacl.model.PropertyShapeConstraint
propertyShapeConstraintLessThan :: Phantoms.TTerm (S.Set Syntax.Property) -> Phantoms.TTerm Model.PropertyShapeConstraint
propertyShapeConstraintLessThan x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThan"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the lessThanOrEquals variant of hydra.shacl.model.PropertyShapeConstraint
propertyShapeConstraintLessThanOrEquals :: Phantoms.TTerm (S.Set Syntax.Property) -> Phantoms.TTerm Model.PropertyShapeConstraint
propertyShapeConstraintLessThanOrEquals x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThanOrEquals"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the maxCount variant of hydra.shacl.model.PropertyShapeConstraint
propertyShapeConstraintMaxCount :: Phantoms.TTerm Integer -> Phantoms.TTerm Model.PropertyShapeConstraint
propertyShapeConstraintMaxCount x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxCount"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the minCount variant of hydra.shacl.model.PropertyShapeConstraint
propertyShapeConstraintMinCount :: Phantoms.TTerm Integer -> Phantoms.TTerm Model.PropertyShapeConstraint
propertyShapeConstraintMinCount x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minCount"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the qualifiedValueShape variant of hydra.shacl.model.PropertyShapeConstraint
propertyShapeConstraintQualifiedValueShape :: Phantoms.TTerm Model.QualifiedValueShape -> Phantoms.TTerm Model.PropertyShapeConstraint
propertyShapeConstraintQualifiedValueShape x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualifiedValueShape"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the uniqueLang variant of hydra.shacl.model.PropertyShapeConstraint
propertyShapeConstraintUniqueLang :: Phantoms.TTerm Bool -> Phantoms.TTerm Model.PropertyShapeConstraint
propertyShapeConstraintUniqueLang x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uniqueLang"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL accessor for the constraints field of hydra.shacl.model.PropertyShape
propertyShapeConstraints :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm (S.Set Model.PropertyShapeConstraint)
propertyShapeConstraints x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionField = (Core.Name "constraints")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the defaultValue field of hydra.shacl.model.PropertyShape
propertyShapeDefaultValue :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm (Maybe Syntax.Node)
propertyShapeDefaultValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionField = (Core.Name "defaultValue")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the description field of hydra.shacl.model.PropertyShape
propertyShapeDescription :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm Syntax.LangStrings
propertyShapeDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionField = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.shacl.model.PropertyShape
propertyShapeName :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm Syntax.LangStrings
propertyShapeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the order field of hydra.shacl.model.PropertyShape
propertyShapeOrder :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm (Maybe Integer)
propertyShapeOrder x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionField = (Core.Name "order")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the path field of hydra.shacl.model.PropertyShape
propertyShapePath :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm Syntax.Iri
propertyShapePath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionField = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the common field of hydra.shacl.model.PropertyShape
propertyShapeWithCommon :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm Model.PropertyShape
propertyShapeWithCommon original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "defaultValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the constraints field of hydra.shacl.model.PropertyShape
propertyShapeWithConstraints :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm (S.Set Model.PropertyShapeConstraint) -> Phantoms.TTerm Model.PropertyShape
propertyShapeWithConstraints original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "common")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "defaultValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the defaultValue field of hydra.shacl.model.PropertyShape
propertyShapeWithDefaultValue :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm (Maybe Syntax.Node) -> Phantoms.TTerm Model.PropertyShape
propertyShapeWithDefaultValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "common")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the description field of hydra.shacl.model.PropertyShape
propertyShapeWithDescription :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm Syntax.LangStrings -> Phantoms.TTerm Model.PropertyShape
propertyShapeWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "common")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "defaultValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.shacl.model.PropertyShape
propertyShapeWithName :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm Syntax.LangStrings -> Phantoms.TTerm Model.PropertyShape
propertyShapeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "common")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "defaultValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the order field of hydra.shacl.model.PropertyShape
propertyShapeWithOrder :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm (Maybe Integer) -> Phantoms.TTerm Model.PropertyShape
propertyShapeWithOrder original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "common")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "defaultValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the path field of hydra.shacl.model.PropertyShape
propertyShapeWithPath :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Model.PropertyShape
propertyShapeWithPath original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "common")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "defaultValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
              Core.projectionField = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.shacl.model.QualifiedValueShape
qualifiedValueShape :: Phantoms.TTerm (Model.Reference Model.Shape) -> Phantoms.TTerm Integer -> Phantoms.TTerm Integer -> Phantoms.TTerm (Maybe Bool) -> Phantoms.TTerm Model.QualifiedValueShape
qualifiedValueShape qualifiedValueShape qualifiedMaxCount qualifiedMinCount qualifiedValueShapesDisjoint =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShape"),
          Core.fieldTerm = (Phantoms.unTTerm qualifiedValueShape)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMaxCount"),
          Core.fieldTerm = (Phantoms.unTTerm qualifiedMaxCount)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMinCount"),
          Core.fieldTerm = (Phantoms.unTTerm qualifiedMinCount)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShapesDisjoint"),
          Core.fieldTerm = (Phantoms.unTTerm qualifiedValueShapesDisjoint)}]}))
-- | DSL accessor for the qualifiedMaxCount field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeQualifiedMaxCount :: Phantoms.TTerm Model.QualifiedValueShape -> Phantoms.TTerm Integer
qualifiedValueShapeQualifiedMaxCount x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
        Core.projectionField = (Core.Name "qualifiedMaxCount")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the qualifiedMinCount field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeQualifiedMinCount :: Phantoms.TTerm Model.QualifiedValueShape -> Phantoms.TTerm Integer
qualifiedValueShapeQualifiedMinCount x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
        Core.projectionField = (Core.Name "qualifiedMinCount")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the qualifiedValueShape field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeQualifiedValueShape :: Phantoms.TTerm Model.QualifiedValueShape -> Phantoms.TTerm (Model.Reference Model.Shape)
qualifiedValueShapeQualifiedValueShape x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
        Core.projectionField = (Core.Name "qualifiedValueShape")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the qualifiedValueShapesDisjoint field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeQualifiedValueShapesDisjoint :: Phantoms.TTerm Model.QualifiedValueShape -> Phantoms.TTerm (Maybe Bool)
qualifiedValueShapeQualifiedValueShapesDisjoint x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
        Core.projectionField = (Core.Name "qualifiedValueShapesDisjoint")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the qualifiedMaxCount field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeWithQualifiedMaxCount :: Phantoms.TTerm Model.QualifiedValueShape -> Phantoms.TTerm Integer -> Phantoms.TTerm Model.QualifiedValueShape
qualifiedValueShapeWithQualifiedMaxCount original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShape"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionField = (Core.Name "qualifiedValueShape")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMaxCount"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionField = (Core.Name "qualifiedMinCount")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShapesDisjoint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionField = (Core.Name "qualifiedValueShapesDisjoint")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the qualifiedMinCount field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeWithQualifiedMinCount :: Phantoms.TTerm Model.QualifiedValueShape -> Phantoms.TTerm Integer -> Phantoms.TTerm Model.QualifiedValueShape
qualifiedValueShapeWithQualifiedMinCount original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShape"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionField = (Core.Name "qualifiedValueShape")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionField = (Core.Name "qualifiedMaxCount")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMinCount"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShapesDisjoint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionField = (Core.Name "qualifiedValueShapesDisjoint")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the qualifiedValueShape field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeWithQualifiedValueShape :: Phantoms.TTerm Model.QualifiedValueShape -> Phantoms.TTerm (Model.Reference Model.Shape) -> Phantoms.TTerm Model.QualifiedValueShape
qualifiedValueShapeWithQualifiedValueShape original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShape"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionField = (Core.Name "qualifiedMaxCount")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionField = (Core.Name "qualifiedMinCount")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShapesDisjoint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionField = (Core.Name "qualifiedValueShapesDisjoint")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the qualifiedValueShapesDisjoint field of hydra.shacl.model.QualifiedValueShape
qualifiedValueShapeWithQualifiedValueShapesDisjoint :: Phantoms.TTerm Model.QualifiedValueShape -> Phantoms.TTerm (Maybe Bool) -> Phantoms.TTerm Model.QualifiedValueShape
qualifiedValueShapeWithQualifiedValueShapesDisjoint original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShape"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionField = (Core.Name "qualifiedValueShape")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMaxCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionField = (Core.Name "qualifiedMaxCount")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedMinCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
              Core.projectionField = (Core.Name "qualifiedMinCount")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualifiedValueShapesDisjoint"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the anonymous variant of hydra.shacl.model.Reference
referenceAnonymous :: Phantoms.TTerm a -> Phantoms.TTerm (Model.Reference a)
referenceAnonymous x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Reference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymous"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the definition variant of hydra.shacl.model.Reference
referenceDefinition :: Phantoms.TTerm (Model.Definition a) -> Phantoms.TTerm (Model.Reference a)
referenceDefinition x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Reference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "definition"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the named variant of hydra.shacl.model.Reference
referenceNamed :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm (Model.Reference a)
referenceNamed x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Reference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the info variant of hydra.shacl.model.Severity
severityInfo :: Phantoms.TTerm Model.Severity
severityInfo =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Severity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "info"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the violation variant of hydra.shacl.model.Severity
severityViolation :: Phantoms.TTerm Model.Severity
severityViolation =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Severity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "violation"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the warning variant of hydra.shacl.model.Severity
severityWarning :: Phantoms.TTerm Model.Severity
severityWarning =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Severity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "warning"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the node variant of hydra.shacl.model.Shape
shapeNode :: Phantoms.TTerm Model.NodeShape -> Phantoms.TTerm Model.Shape
shapeNode x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Shape"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "node"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the property variant of hydra.shacl.model.Shape
shapeProperty :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm Model.Shape
shapeProperty x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Shape"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.shacl.model.ShapesGraph wrapper
shapesGraph :: Phantoms.TTerm (S.Set (Model.Definition Model.Shape)) -> Phantoms.TTerm Model.ShapesGraph
shapesGraph x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shacl.model.ShapesGraph"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.shacl.model.ShapesGraph
unShapesGraph :: Phantoms.TTerm Model.ShapesGraph -> Phantoms.TTerm (S.Set (Model.Definition Model.Shape))
unShapesGraph x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shacl.model.ShapesGraph")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
