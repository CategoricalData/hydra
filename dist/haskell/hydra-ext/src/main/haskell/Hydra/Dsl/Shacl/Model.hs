-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.shacl.model

module Hydra.Dsl.Shacl.Model where

import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Rdf.Syntax as Syntax
import qualified Hydra.Shacl.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Set as S

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

closedIgnoredProperties :: Phantoms.TTerm Model.Closed -> Phantoms.TTerm (Maybe (S.Set Syntax.Property))
closedIgnoredProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Closed"),
        Core.projectionField = (Core.Name "ignoredProperties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

closedIsClosed :: Phantoms.TTerm Model.Closed -> Phantoms.TTerm Bool
closedIsClosed x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Closed"),
        Core.projectionField = (Core.Name "isClosed")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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

commonConstraintAnd :: Phantoms.TTerm (S.Set (Model.Reference Model.Shape)) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintAnd x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintClass :: Phantoms.TTerm (S.Set Syntax.RdfsClass) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintClass x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintClosed :: Phantoms.TTerm Model.Closed -> Phantoms.TTerm Model.CommonConstraint
commonConstraintClosed x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "closed"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintDatatype :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Model.CommonConstraint
commonConstraintDatatype x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "datatype"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintDisjoint :: Phantoms.TTerm (S.Set Syntax.Property) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintDisjoint x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "disjoint"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintEquals :: Phantoms.TTerm (S.Set Syntax.Property) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintEquals x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equals"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintHasValue :: Phantoms.TTerm (S.Set Syntax.Node) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintHasValue x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hasValue"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintIn :: Phantoms.TTerm [Syntax.Node] -> Phantoms.TTerm Model.CommonConstraint
commonConstraintIn x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintLanguageIn :: Phantoms.TTerm (S.Set Syntax.LanguageTag) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintLanguageIn x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "languageIn"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintMaxExclusive :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Model.CommonConstraint
commonConstraintMaxExclusive x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxExclusive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintMaxInclusive :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Model.CommonConstraint
commonConstraintMaxInclusive x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxInclusive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintMaxLength :: Phantoms.TTerm Integer -> Phantoms.TTerm Model.CommonConstraint
commonConstraintMaxLength x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxLength"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintMinExclusive :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Model.CommonConstraint
commonConstraintMinExclusive x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minExclusive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintMinInclusive :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Model.CommonConstraint
commonConstraintMinInclusive x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minInclusive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintMinLength :: Phantoms.TTerm Integer -> Phantoms.TTerm Model.CommonConstraint
commonConstraintMinLength x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minLength"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintNode :: Phantoms.TTerm (S.Set (Model.Reference Model.NodeShape)) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintNode x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "node"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintNodeKind :: Phantoms.TTerm Model.NodeKind -> Phantoms.TTerm Model.CommonConstraint
commonConstraintNodeKind x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nodeKind"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintNot :: Phantoms.TTerm (S.Set (Model.Reference Model.Shape)) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintNot x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintOr :: Phantoms.TTerm (S.Set (Model.Reference Model.Shape)) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintOr x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintPattern :: Phantoms.TTerm Model.Pattern -> Phantoms.TTerm Model.CommonConstraint
commonConstraintPattern x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintProperty :: Phantoms.TTerm (S.Set (Model.Reference Model.PropertyShape)) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonConstraintXone :: Phantoms.TTerm (S.Set (Model.Reference Model.Shape)) -> Phantoms.TTerm Model.CommonConstraint
commonConstraintXone x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.CommonConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "xone"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

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

commonPropertiesConstraints :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm (S.Set Model.CommonConstraint)
commonPropertiesConstraints x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionField = (Core.Name "constraints")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

commonPropertiesDeactivated :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm (Maybe Bool)
commonPropertiesDeactivated x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionField = (Core.Name "deactivated")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

commonPropertiesMessage :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm Syntax.LangStrings
commonPropertiesMessage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionField = (Core.Name "message")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

commonPropertiesSeverity :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm Model.Severity
commonPropertiesSeverity x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionField = (Core.Name "severity")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

commonPropertiesTargetClass :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm (S.Set Syntax.RdfsClass)
commonPropertiesTargetClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionField = (Core.Name "targetClass")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

commonPropertiesTargetNode :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm (S.Set Syntax.IriOrLiteral)
commonPropertiesTargetNode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionField = (Core.Name "targetNode")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

commonPropertiesTargetObjectsOf :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm (S.Set Syntax.Property)
commonPropertiesTargetObjectsOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionField = (Core.Name "targetObjectsOf")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

commonPropertiesTargetSubjectsOf :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm (S.Set Syntax.Property)
commonPropertiesTargetSubjectsOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.CommonProperties"),
        Core.projectionField = (Core.Name "targetSubjectsOf")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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

definitionIri :: Phantoms.TTerm (Model.Definition a) -> Phantoms.TTerm Syntax.Iri
definitionIri x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Definition"),
        Core.projectionField = (Core.Name "iri")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

definitionTarget :: Phantoms.TTerm (Model.Definition a) -> Phantoms.TTerm a
definitionTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Definition"),
        Core.projectionField = (Core.Name "target")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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

nodeKindBlankNode :: Phantoms.TTerm Model.NodeKind
nodeKindBlankNode =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "blankNode"),
        Core.fieldTerm = Core.TermUnit}}))

nodeKindBlankNodeOrIri :: Phantoms.TTerm Model.NodeKind
nodeKindBlankNodeOrIri =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "blankNodeOrIri"),
        Core.fieldTerm = Core.TermUnit}}))

nodeKindBlankNodeOrLiteral :: Phantoms.TTerm Model.NodeKind
nodeKindBlankNodeOrLiteral =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "blankNodeOrLiteral"),
        Core.fieldTerm = Core.TermUnit}}))

nodeKindIri :: Phantoms.TTerm Model.NodeKind
nodeKindIri =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iri"),
        Core.fieldTerm = Core.TermUnit}}))

nodeKindIriOrLiteral :: Phantoms.TTerm Model.NodeKind
nodeKindIriOrLiteral =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iriOrLiteral"),
        Core.fieldTerm = Core.TermUnit}}))

nodeKindLiteral :: Phantoms.TTerm Model.NodeKind
nodeKindLiteral =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.NodeKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = Core.TermUnit}}))

nodeShape :: Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm Model.NodeShape
nodeShape common =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.NodeShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Phantoms.unTTerm common)}]}))

nodeShapeCommon :: Phantoms.TTerm Model.NodeShape -> Phantoms.TTerm Model.CommonProperties
nodeShapeCommon x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.NodeShape"),
        Core.projectionField = (Core.Name "common")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nodeShapeWithCommon :: Phantoms.TTerm Model.NodeShape -> Phantoms.TTerm Model.CommonProperties -> Phantoms.TTerm Model.NodeShape
nodeShapeWithCommon original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shacl.model.NodeShape"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "common"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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

patternFlags :: Phantoms.TTerm Model.Pattern -> Phantoms.TTerm (Maybe String)
patternFlags x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Pattern"),
        Core.projectionField = (Core.Name "flags")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

patternRegex :: Phantoms.TTerm Model.Pattern -> Phantoms.TTerm String
patternRegex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.Pattern"),
        Core.projectionField = (Core.Name "regex")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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

propertyShapeCommon :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm Model.CommonProperties
propertyShapeCommon x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionField = (Core.Name "common")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyShapeConstraintLessThan :: Phantoms.TTerm (S.Set Syntax.Property) -> Phantoms.TTerm Model.PropertyShapeConstraint
propertyShapeConstraintLessThan x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThan"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

propertyShapeConstraintLessThanOrEquals :: Phantoms.TTerm (S.Set Syntax.Property) -> Phantoms.TTerm Model.PropertyShapeConstraint
propertyShapeConstraintLessThanOrEquals x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThanOrEquals"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

propertyShapeConstraintMaxCount :: Phantoms.TTerm Integer -> Phantoms.TTerm Model.PropertyShapeConstraint
propertyShapeConstraintMaxCount x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxCount"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

propertyShapeConstraintMinCount :: Phantoms.TTerm Integer -> Phantoms.TTerm Model.PropertyShapeConstraint
propertyShapeConstraintMinCount x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minCount"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

propertyShapeConstraintQualifiedValueShape :: Phantoms.TTerm Model.QualifiedValueShape -> Phantoms.TTerm Model.PropertyShapeConstraint
propertyShapeConstraintQualifiedValueShape x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualifiedValueShape"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

propertyShapeConstraintUniqueLang :: Phantoms.TTerm Bool -> Phantoms.TTerm Model.PropertyShapeConstraint
propertyShapeConstraintUniqueLang x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.PropertyShapeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uniqueLang"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

propertyShapeConstraints :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm (S.Set Model.PropertyShapeConstraint)
propertyShapeConstraints x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionField = (Core.Name "constraints")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyShapeDefaultValue :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm (Maybe Syntax.Node)
propertyShapeDefaultValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionField = (Core.Name "defaultValue")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyShapeDescription :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm Syntax.LangStrings
propertyShapeDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionField = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyShapeName :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm Syntax.LangStrings
propertyShapeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyShapeOrder :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm (Maybe Integer)
propertyShapeOrder x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionField = (Core.Name "order")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyShapePath :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm Syntax.Iri
propertyShapePath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.PropertyShape"),
        Core.projectionField = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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

qualifiedValueShapeQualifiedMaxCount :: Phantoms.TTerm Model.QualifiedValueShape -> Phantoms.TTerm Integer
qualifiedValueShapeQualifiedMaxCount x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
        Core.projectionField = (Core.Name "qualifiedMaxCount")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualifiedValueShapeQualifiedMinCount :: Phantoms.TTerm Model.QualifiedValueShape -> Phantoms.TTerm Integer
qualifiedValueShapeQualifiedMinCount x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
        Core.projectionField = (Core.Name "qualifiedMinCount")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualifiedValueShapeQualifiedValueShape :: Phantoms.TTerm Model.QualifiedValueShape -> Phantoms.TTerm (Model.Reference Model.Shape)
qualifiedValueShapeQualifiedValueShape x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
        Core.projectionField = (Core.Name "qualifiedValueShape")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualifiedValueShapeQualifiedValueShapesDisjoint :: Phantoms.TTerm Model.QualifiedValueShape -> Phantoms.TTerm (Maybe Bool)
qualifiedValueShapeQualifiedValueShapesDisjoint x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shacl.model.QualifiedValueShape"),
        Core.projectionField = (Core.Name "qualifiedValueShapesDisjoint")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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

referenceAnonymous :: Phantoms.TTerm a -> Phantoms.TTerm (Model.Reference a)
referenceAnonymous x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Reference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymous"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

referenceDefinition :: Phantoms.TTerm (Model.Definition a) -> Phantoms.TTerm (Model.Reference a)
referenceDefinition x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Reference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "definition"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

referenceNamed :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm (Model.Reference a)
referenceNamed x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Reference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

severityInfo :: Phantoms.TTerm Model.Severity
severityInfo =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Severity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "info"),
        Core.fieldTerm = Core.TermUnit}}))

severityViolation :: Phantoms.TTerm Model.Severity
severityViolation =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Severity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "violation"),
        Core.fieldTerm = Core.TermUnit}}))

severityWarning :: Phantoms.TTerm Model.Severity
severityWarning =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Severity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "warning"),
        Core.fieldTerm = Core.TermUnit}}))

shapeNode :: Phantoms.TTerm Model.NodeShape -> Phantoms.TTerm Model.Shape
shapeNode x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Shape"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "node"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

shapeProperty :: Phantoms.TTerm Model.PropertyShape -> Phantoms.TTerm Model.Shape
shapeProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shacl.model.Shape"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

shapesGraph :: Phantoms.TTerm (S.Set (Model.Definition Model.Shape)) -> Phantoms.TTerm Model.ShapesGraph
shapesGraph x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shacl.model.ShapesGraph"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unShapesGraph :: Phantoms.TTerm Model.ShapesGraph -> Phantoms.TTerm (S.Set (Model.Definition Model.Shape))
unShapesGraph x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shacl.model.ShapesGraph")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
