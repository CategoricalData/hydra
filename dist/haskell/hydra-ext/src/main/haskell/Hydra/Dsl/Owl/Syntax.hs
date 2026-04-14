-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.owl.syntax

module Hydra.Dsl.Owl.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Owl.Syntax as OwlSyntax
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Rdf.Syntax as RdfSyntax
import qualified Hydra.Xml.Schema as Schema
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

annotation :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.AnnotationProperty -> Phantoms.TTerm OwlSyntax.AnnotationValue -> Phantoms.TTerm OwlSyntax.Annotation
annotation annotations property value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.Annotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

annotationAnnotations :: Phantoms.TTerm OwlSyntax.Annotation -> Phantoms.TTerm [OwlSyntax.Annotation]
annotationAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Annotation"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationAssertion :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.AnnotationProperty -> Phantoms.TTerm OwlSyntax.AnnotationSubject -> Phantoms.TTerm OwlSyntax.AnnotationValue -> Phantoms.TTerm OwlSyntax.AnnotationAssertion
annotationAssertion annotations property subject value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTTerm subject)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

annotationAssertionAnnotations :: Phantoms.TTerm OwlSyntax.AnnotationAssertion -> Phantoms.TTerm [OwlSyntax.Annotation]
annotationAssertionAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationAssertionProperty :: Phantoms.TTerm OwlSyntax.AnnotationAssertion -> Phantoms.TTerm OwlSyntax.AnnotationProperty
annotationAssertionProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationAssertionSubject :: Phantoms.TTerm OwlSyntax.AnnotationAssertion -> Phantoms.TTerm OwlSyntax.AnnotationSubject
annotationAssertionSubject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
        Core.projectionField = (Core.Name "subject")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationAssertionValue :: Phantoms.TTerm OwlSyntax.AnnotationAssertion -> Phantoms.TTerm OwlSyntax.AnnotationValue
annotationAssertionValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationAssertionWithAnnotations :: Phantoms.TTerm OwlSyntax.AnnotationAssertion -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.AnnotationAssertion
annotationAssertionWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "subject")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationAssertionWithProperty :: Phantoms.TTerm OwlSyntax.AnnotationAssertion -> Phantoms.TTerm OwlSyntax.AnnotationProperty -> Phantoms.TTerm OwlSyntax.AnnotationAssertion
annotationAssertionWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "subject")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationAssertionWithSubject :: Phantoms.TTerm OwlSyntax.AnnotationAssertion -> Phantoms.TTerm OwlSyntax.AnnotationSubject -> Phantoms.TTerm OwlSyntax.AnnotationAssertion
annotationAssertionWithSubject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationAssertionWithValue :: Phantoms.TTerm OwlSyntax.AnnotationAssertion -> Phantoms.TTerm OwlSyntax.AnnotationValue -> Phantoms.TTerm OwlSyntax.AnnotationAssertion
annotationAssertionWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "subject")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

annotationAxiomAnnotationAssertion :: Phantoms.TTerm OwlSyntax.AnnotationAssertion -> Phantoms.TTerm OwlSyntax.AnnotationAxiom
annotationAxiomAnnotationAssertion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotationAssertion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationAxiomAnnotationPropertyDomain :: Phantoms.TTerm OwlSyntax.AnnotationPropertyDomain -> Phantoms.TTerm OwlSyntax.AnnotationAxiom
annotationAxiomAnnotationPropertyDomain x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotationPropertyDomain"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationAxiomAnnotationPropertyRange :: Phantoms.TTerm OwlSyntax.AnnotationPropertyRange -> Phantoms.TTerm OwlSyntax.AnnotationAxiom
annotationAxiomAnnotationPropertyRange x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotationPropertyRange"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationAxiomSubAnnotationPropertyOf :: Phantoms.TTerm OwlSyntax.SubAnnotationPropertyOf -> Phantoms.TTerm OwlSyntax.AnnotationAxiom
annotationAxiomSubAnnotationPropertyOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subAnnotationPropertyOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationProperty :: Phantoms.TTerm OwlSyntax.Annotation -> Phantoms.TTerm OwlSyntax.AnnotationProperty
annotationProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Annotation"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationPropertyDomain :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.AnnotationProperty -> Phantoms.TTerm RdfSyntax.Iri -> Phantoms.TTerm OwlSyntax.AnnotationPropertyDomain
annotationPropertyDomain annotations property iri =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Phantoms.unTTerm iri)}]}))

annotationPropertyDomainAnnotations :: Phantoms.TTerm OwlSyntax.AnnotationPropertyDomain -> Phantoms.TTerm [OwlSyntax.Annotation]
annotationPropertyDomainAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyDomain"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationPropertyDomainIri :: Phantoms.TTerm OwlSyntax.AnnotationPropertyDomain -> Phantoms.TTerm RdfSyntax.Iri
annotationPropertyDomainIri x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyDomain"),
        Core.projectionField = (Core.Name "iri")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationPropertyDomainProperty :: Phantoms.TTerm OwlSyntax.AnnotationPropertyDomain -> Phantoms.TTerm OwlSyntax.AnnotationProperty
annotationPropertyDomainProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyDomain"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationPropertyDomainWithAnnotations :: Phantoms.TTerm OwlSyntax.AnnotationPropertyDomain -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.AnnotationPropertyDomain
annotationPropertyDomainWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyDomain"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyDomain"),
              Core.projectionField = (Core.Name "iri")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationPropertyDomainWithIri :: Phantoms.TTerm OwlSyntax.AnnotationPropertyDomain -> Phantoms.TTerm RdfSyntax.Iri -> Phantoms.TTerm OwlSyntax.AnnotationPropertyDomain
annotationPropertyDomainWithIri original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyDomain"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyDomain"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

annotationPropertyDomainWithProperty :: Phantoms.TTerm OwlSyntax.AnnotationPropertyDomain -> Phantoms.TTerm OwlSyntax.AnnotationProperty -> Phantoms.TTerm OwlSyntax.AnnotationPropertyDomain
annotationPropertyDomainWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyDomain"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyDomain"),
              Core.projectionField = (Core.Name "iri")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationPropertyRange :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.AnnotationProperty -> Phantoms.TTerm RdfSyntax.Iri -> Phantoms.TTerm OwlSyntax.AnnotationPropertyRange
annotationPropertyRange annotations property iri =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Phantoms.unTTerm iri)}]}))

annotationPropertyRangeAnnotations :: Phantoms.TTerm OwlSyntax.AnnotationPropertyRange -> Phantoms.TTerm [OwlSyntax.Annotation]
annotationPropertyRangeAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyRange"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationPropertyRangeIri :: Phantoms.TTerm OwlSyntax.AnnotationPropertyRange -> Phantoms.TTerm RdfSyntax.Iri
annotationPropertyRangeIri x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyRange"),
        Core.projectionField = (Core.Name "iri")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationPropertyRangeProperty :: Phantoms.TTerm OwlSyntax.AnnotationPropertyRange -> Phantoms.TTerm OwlSyntax.AnnotationProperty
annotationPropertyRangeProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyRange"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationPropertyRangeWithAnnotations :: Phantoms.TTerm OwlSyntax.AnnotationPropertyRange -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.AnnotationPropertyRange
annotationPropertyRangeWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyRange"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyRange"),
              Core.projectionField = (Core.Name "iri")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationPropertyRangeWithIri :: Phantoms.TTerm OwlSyntax.AnnotationPropertyRange -> Phantoms.TTerm RdfSyntax.Iri -> Phantoms.TTerm OwlSyntax.AnnotationPropertyRange
annotationPropertyRangeWithIri original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyRange"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyRange"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

annotationPropertyRangeWithProperty :: Phantoms.TTerm OwlSyntax.AnnotationPropertyRange -> Phantoms.TTerm OwlSyntax.AnnotationProperty -> Phantoms.TTerm OwlSyntax.AnnotationPropertyRange
annotationPropertyRangeWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyRange"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationPropertyRange"),
              Core.projectionField = (Core.Name "iri")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationProperty_ :: Phantoms.TTerm () -> Phantoms.TTerm OwlSyntax.AnnotationProperty
annotationProperty_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.owl.syntax.AnnotationProperty"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

annotationSubjectAnonymousIndividual :: Phantoms.TTerm OwlSyntax.AnonymousIndividual -> Phantoms.TTerm OwlSyntax.AnnotationSubject
annotationSubjectAnonymousIndividual x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationSubject"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymousIndividual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationSubjectIri :: Phantoms.TTerm RdfSyntax.Iri -> Phantoms.TTerm OwlSyntax.AnnotationSubject
annotationSubjectIri x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationSubject"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iri"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationValue :: Phantoms.TTerm OwlSyntax.Annotation -> Phantoms.TTerm OwlSyntax.AnnotationValue
annotationValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Annotation"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationValueAnonymousIndividual :: Phantoms.TTerm OwlSyntax.AnonymousIndividual -> Phantoms.TTerm OwlSyntax.AnnotationValue
annotationValueAnonymousIndividual x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymousIndividual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationValueIri :: Phantoms.TTerm RdfSyntax.Iri -> Phantoms.TTerm OwlSyntax.AnnotationValue
annotationValueIri x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iri"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationValueLiteral :: Phantoms.TTerm RdfSyntax.Literal -> Phantoms.TTerm OwlSyntax.AnnotationValue
annotationValueLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.AnnotationValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationWithAnnotations :: Phantoms.TTerm OwlSyntax.Annotation -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.Annotation
annotationWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.Annotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Annotation"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Annotation"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationWithProperty :: Phantoms.TTerm OwlSyntax.Annotation -> Phantoms.TTerm OwlSyntax.AnnotationProperty -> Phantoms.TTerm OwlSyntax.Annotation
annotationWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.Annotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Annotation"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Annotation"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationWithValue :: Phantoms.TTerm OwlSyntax.Annotation -> Phantoms.TTerm OwlSyntax.AnnotationValue -> Phantoms.TTerm OwlSyntax.Annotation
annotationWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.Annotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Annotation"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Annotation"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

anonymousIndividual :: Phantoms.TTerm () -> Phantoms.TTerm OwlSyntax.AnonymousIndividual
anonymousIndividual x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.owl.syntax.AnonymousIndividual"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

assertionClassAssertion :: Phantoms.TTerm OwlSyntax.ClassAssertion -> Phantoms.TTerm OwlSyntax.Assertion
assertionClassAssertion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Assertion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classAssertion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assertionDataPropertyAssertion :: Phantoms.TTerm OwlSyntax.DataPropertyAssertion -> Phantoms.TTerm OwlSyntax.Assertion
assertionDataPropertyAssertion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Assertion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataPropertyAssertion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assertionDifferentIndividuals :: Phantoms.TTerm OwlSyntax.DifferentIndividuals -> Phantoms.TTerm OwlSyntax.Assertion
assertionDifferentIndividuals x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Assertion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "differentIndividuals"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assertionNegativeDataPropertyAssertion :: Phantoms.TTerm OwlSyntax.NegativeDataPropertyAssertion -> Phantoms.TTerm OwlSyntax.Assertion
assertionNegativeDataPropertyAssertion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Assertion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negativeDataPropertyAssertion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assertionNegativeObjectPropertyAssertion :: Phantoms.TTerm OwlSyntax.NegativeObjectPropertyAssertion -> Phantoms.TTerm OwlSyntax.Assertion
assertionNegativeObjectPropertyAssertion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Assertion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negativeObjectPropertyAssertion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assertionObjectPropertyAssertion :: Phantoms.TTerm OwlSyntax.ObjectPropertyAssertion -> Phantoms.TTerm OwlSyntax.Assertion
assertionObjectPropertyAssertion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Assertion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectPropertyAssertion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assertionSameIndividual :: Phantoms.TTerm OwlSyntax.SameIndividual -> Phantoms.TTerm OwlSyntax.Assertion
assertionSameIndividual x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Assertion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sameIndividual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

asymmetricObjectProperty :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.AsymmetricObjectProperty
asymmetricObjectProperty annotations property =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.AsymmetricObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)}]}))

asymmetricObjectPropertyAnnotations :: Phantoms.TTerm OwlSyntax.AsymmetricObjectProperty -> Phantoms.TTerm [OwlSyntax.Annotation]
asymmetricObjectPropertyAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AsymmetricObjectProperty"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

asymmetricObjectPropertyProperty :: Phantoms.TTerm OwlSyntax.AsymmetricObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
asymmetricObjectPropertyProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AsymmetricObjectProperty"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

asymmetricObjectPropertyWithAnnotations :: Phantoms.TTerm OwlSyntax.AsymmetricObjectProperty -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.AsymmetricObjectProperty
asymmetricObjectPropertyWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.AsymmetricObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AsymmetricObjectProperty"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

asymmetricObjectPropertyWithProperty :: Phantoms.TTerm OwlSyntax.AsymmetricObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.AsymmetricObjectProperty
asymmetricObjectPropertyWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.AsymmetricObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.AsymmetricObjectProperty"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

axiomAnnotationAxiom :: Phantoms.TTerm OwlSyntax.AnnotationAxiom -> Phantoms.TTerm OwlSyntax.Axiom
axiomAnnotationAxiom x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Axiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotationAxiom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

axiomAssertion :: Phantoms.TTerm OwlSyntax.Assertion -> Phantoms.TTerm OwlSyntax.Axiom
axiomAssertion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Axiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assertion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

axiomClassAxiom :: Phantoms.TTerm OwlSyntax.ClassAxiom -> Phantoms.TTerm OwlSyntax.Axiom
axiomClassAxiom x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Axiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classAxiom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

axiomDataPropertyAxiom :: Phantoms.TTerm OwlSyntax.DataPropertyAxiom -> Phantoms.TTerm OwlSyntax.Axiom
axiomDataPropertyAxiom x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Axiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataPropertyAxiom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

axiomDatatypeDefinition :: Phantoms.TTerm OwlSyntax.DatatypeDefinition -> Phantoms.TTerm OwlSyntax.Axiom
axiomDatatypeDefinition x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Axiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "datatypeDefinition"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

axiomDeclaration :: Phantoms.TTerm OwlSyntax.Declaration -> Phantoms.TTerm OwlSyntax.Axiom
axiomDeclaration x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Axiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "declaration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

axiomHasKey :: Phantoms.TTerm OwlSyntax.HasKey -> Phantoms.TTerm OwlSyntax.Axiom
axiomHasKey x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Axiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hasKey"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

axiomObjectPropertyAxiom :: Phantoms.TTerm OwlSyntax.ObjectPropertyAxiom -> Phantoms.TTerm OwlSyntax.Axiom
axiomObjectPropertyAxiom x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Axiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectPropertyAxiom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

class_ :: Phantoms.TTerm () -> Phantoms.TTerm OwlSyntax.Class
class_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.owl.syntax.Class"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

classAssertion :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm OwlSyntax.Individual -> Phantoms.TTerm OwlSyntax.ClassAssertion
classAssertion annotations class_ individual =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ClassAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm class_)},
        Core.Field {
          Core.fieldName = (Core.Name "individual"),
          Core.fieldTerm = (Phantoms.unTTerm individual)}]}))

classAssertionAnnotations :: Phantoms.TTerm OwlSyntax.ClassAssertion -> Phantoms.TTerm [OwlSyntax.Annotation]
classAssertionAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ClassAssertion"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classAssertionClass :: Phantoms.TTerm OwlSyntax.ClassAssertion -> Phantoms.TTerm OwlSyntax.ClassExpression
classAssertionClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ClassAssertion"),
        Core.projectionField = (Core.Name "class")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classAssertionIndividual :: Phantoms.TTerm OwlSyntax.ClassAssertion -> Phantoms.TTerm OwlSyntax.Individual
classAssertionIndividual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ClassAssertion"),
        Core.projectionField = (Core.Name "individual")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classAssertionWithAnnotations :: Phantoms.TTerm OwlSyntax.ClassAssertion -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.ClassAssertion
classAssertionWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ClassAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ClassAssertion"),
              Core.projectionField = (Core.Name "class")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "individual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ClassAssertion"),
              Core.projectionField = (Core.Name "individual")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

classAssertionWithClass :: Phantoms.TTerm OwlSyntax.ClassAssertion -> Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm OwlSyntax.ClassAssertion
classAssertionWithClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ClassAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ClassAssertion"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "individual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ClassAssertion"),
              Core.projectionField = (Core.Name "individual")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

classAssertionWithIndividual :: Phantoms.TTerm OwlSyntax.ClassAssertion -> Phantoms.TTerm OwlSyntax.Individual -> Phantoms.TTerm OwlSyntax.ClassAssertion
classAssertionWithIndividual original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ClassAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ClassAssertion"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ClassAssertion"),
              Core.projectionField = (Core.Name "class")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "individual"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

classAxiomDisjointClasses :: Phantoms.TTerm OwlSyntax.DisjointClasses -> Phantoms.TTerm OwlSyntax.ClassAxiom
classAxiomDisjointClasses x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "disjointClasses"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classAxiomDisjointUnion :: Phantoms.TTerm OwlSyntax.DisjointUnion -> Phantoms.TTerm OwlSyntax.ClassAxiom
classAxiomDisjointUnion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "disjointUnion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classAxiomEquivalentClasses :: Phantoms.TTerm OwlSyntax.EquivalentClasses -> Phantoms.TTerm OwlSyntax.ClassAxiom
classAxiomEquivalentClasses x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equivalentClasses"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classAxiomSubClassOf :: Phantoms.TTerm OwlSyntax.SubClassOf -> Phantoms.TTerm OwlSyntax.ClassAxiom
classAxiomSubClassOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subClassOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionClass :: Phantoms.TTerm OwlSyntax.Class -> Phantoms.TTerm OwlSyntax.ClassExpression
classExpressionClass x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionDataAllValuesFrom :: Phantoms.TTerm OwlSyntax.DataAllValuesFrom -> Phantoms.TTerm OwlSyntax.ClassExpression
classExpressionDataAllValuesFrom x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataAllValuesFrom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionDataExactCardinality :: Phantoms.TTerm OwlSyntax.DataExactCardinality -> Phantoms.TTerm OwlSyntax.ClassExpression
classExpressionDataExactCardinality x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataExactCardinality"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionDataHasValue :: Phantoms.TTerm OwlSyntax.DataHasValue -> Phantoms.TTerm OwlSyntax.ClassExpression
classExpressionDataHasValue x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataHasValue"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionDataMaxCardinality :: Phantoms.TTerm OwlSyntax.DataMaxCardinality -> Phantoms.TTerm OwlSyntax.ClassExpression
classExpressionDataMaxCardinality x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataMaxCardinality"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionDataMinCardinality :: Phantoms.TTerm OwlSyntax.DataMinCardinality -> Phantoms.TTerm OwlSyntax.ClassExpression
classExpressionDataMinCardinality x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataMinCardinality"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionDataSomeValuesFrom :: Phantoms.TTerm OwlSyntax.DataSomeValuesFrom -> Phantoms.TTerm OwlSyntax.ClassExpression
classExpressionDataSomeValuesFrom x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataSomeValuesFrom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionObjectAllValuesFrom :: Phantoms.TTerm OwlSyntax.ObjectAllValuesFrom -> Phantoms.TTerm OwlSyntax.ClassExpression
classExpressionObjectAllValuesFrom x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectAllValuesFrom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionObjectExactCardinality :: Phantoms.TTerm OwlSyntax.ObjectExactCardinality -> Phantoms.TTerm OwlSyntax.ClassExpression
classExpressionObjectExactCardinality x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectExactCardinality"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionObjectHasSelf :: Phantoms.TTerm OwlSyntax.ObjectHasSelf -> Phantoms.TTerm OwlSyntax.ClassExpression
classExpressionObjectHasSelf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectHasSelf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionObjectHasValue :: Phantoms.TTerm OwlSyntax.ObjectHasValue -> Phantoms.TTerm OwlSyntax.ClassExpression
classExpressionObjectHasValue x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectHasValue"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionObjectIntersectionOf :: Phantoms.TTerm OwlSyntax.ObjectIntersectionOf -> Phantoms.TTerm OwlSyntax.ClassExpression
classExpressionObjectIntersectionOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectIntersectionOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionObjectMaxCardinality :: Phantoms.TTerm OwlSyntax.ObjectMaxCardinality -> Phantoms.TTerm OwlSyntax.ClassExpression
classExpressionObjectMaxCardinality x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectMaxCardinality"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionObjectMinCardinality :: Phantoms.TTerm OwlSyntax.ObjectMinCardinality -> Phantoms.TTerm OwlSyntax.ClassExpression
classExpressionObjectMinCardinality x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectMinCardinality"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionObjectOneOf :: Phantoms.TTerm OwlSyntax.ObjectOneOf -> Phantoms.TTerm OwlSyntax.ClassExpression
classExpressionObjectOneOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectOneOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionObjectSomeValuesFrom :: Phantoms.TTerm OwlSyntax.ObjectSomeValuesFrom -> Phantoms.TTerm OwlSyntax.ClassExpression
classExpressionObjectSomeValuesFrom x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectSomeValuesFrom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionObjectUnionOf :: Phantoms.TTerm OwlSyntax.ObjectUnionOf -> Phantoms.TTerm OwlSyntax.ClassExpression
classExpressionObjectUnionOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectUnionOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataAllValuesFrom :: Phantoms.TTerm [OwlSyntax.DataPropertyExpression] -> Phantoms.TTerm OwlSyntax.DataRange -> Phantoms.TTerm OwlSyntax.DataAllValuesFrom
dataAllValuesFrom property range =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataAllValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm range)}]}))

dataAllValuesFromProperty :: Phantoms.TTerm OwlSyntax.DataAllValuesFrom -> Phantoms.TTerm [OwlSyntax.DataPropertyExpression]
dataAllValuesFromProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataAllValuesFrom"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataAllValuesFromRange :: Phantoms.TTerm OwlSyntax.DataAllValuesFrom -> Phantoms.TTerm OwlSyntax.DataRange
dataAllValuesFromRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataAllValuesFrom"),
        Core.projectionField = (Core.Name "range")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataAllValuesFromWithProperty :: Phantoms.TTerm OwlSyntax.DataAllValuesFrom -> Phantoms.TTerm [OwlSyntax.DataPropertyExpression] -> Phantoms.TTerm OwlSyntax.DataAllValuesFrom
dataAllValuesFromWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataAllValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataAllValuesFrom"),
              Core.projectionField = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataAllValuesFromWithRange :: Phantoms.TTerm OwlSyntax.DataAllValuesFrom -> Phantoms.TTerm OwlSyntax.DataRange -> Phantoms.TTerm OwlSyntax.DataAllValuesFrom
dataAllValuesFromWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataAllValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataAllValuesFrom"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataComplementOf :: Phantoms.TTerm OwlSyntax.DataRange -> Phantoms.TTerm OwlSyntax.DataComplementOf
dataComplementOf x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.owl.syntax.DataComplementOf"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

dataExactCardinality :: Phantoms.TTerm Integer -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm [OwlSyntax.DataRange] -> Phantoms.TTerm OwlSyntax.DataExactCardinality
dataExactCardinality bound property range =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataExactCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm bound)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm range)}]}))

dataExactCardinalityBound :: Phantoms.TTerm OwlSyntax.DataExactCardinality -> Phantoms.TTerm Integer
dataExactCardinalityBound x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataExactCardinality"),
        Core.projectionField = (Core.Name "bound")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataExactCardinalityProperty :: Phantoms.TTerm OwlSyntax.DataExactCardinality -> Phantoms.TTerm OwlSyntax.DataPropertyExpression
dataExactCardinalityProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataExactCardinality"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataExactCardinalityRange :: Phantoms.TTerm OwlSyntax.DataExactCardinality -> Phantoms.TTerm [OwlSyntax.DataRange]
dataExactCardinalityRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataExactCardinality"),
        Core.projectionField = (Core.Name "range")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataExactCardinalityWithBound :: Phantoms.TTerm OwlSyntax.DataExactCardinality -> Phantoms.TTerm Integer -> Phantoms.TTerm OwlSyntax.DataExactCardinality
dataExactCardinalityWithBound original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataExactCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataExactCardinality"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataExactCardinality"),
              Core.projectionField = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataExactCardinalityWithProperty :: Phantoms.TTerm OwlSyntax.DataExactCardinality -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm OwlSyntax.DataExactCardinality
dataExactCardinalityWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataExactCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataExactCardinality"),
              Core.projectionField = (Core.Name "bound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataExactCardinality"),
              Core.projectionField = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataExactCardinalityWithRange :: Phantoms.TTerm OwlSyntax.DataExactCardinality -> Phantoms.TTerm [OwlSyntax.DataRange] -> Phantoms.TTerm OwlSyntax.DataExactCardinality
dataExactCardinalityWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataExactCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataExactCardinality"),
              Core.projectionField = (Core.Name "bound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataExactCardinality"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataHasValue :: Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm RdfSyntax.Literal -> Phantoms.TTerm OwlSyntax.DataHasValue
dataHasValue property value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataHasValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

dataHasValueProperty :: Phantoms.TTerm OwlSyntax.DataHasValue -> Phantoms.TTerm OwlSyntax.DataPropertyExpression
dataHasValueProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataHasValue"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataHasValueValue :: Phantoms.TTerm OwlSyntax.DataHasValue -> Phantoms.TTerm RdfSyntax.Literal
dataHasValueValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataHasValue"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataHasValueWithProperty :: Phantoms.TTerm OwlSyntax.DataHasValue -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm OwlSyntax.DataHasValue
dataHasValueWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataHasValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataHasValue"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataHasValueWithValue :: Phantoms.TTerm OwlSyntax.DataHasValue -> Phantoms.TTerm RdfSyntax.Literal -> Phantoms.TTerm OwlSyntax.DataHasValue
dataHasValueWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataHasValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataHasValue"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataIntersectionOf :: Phantoms.TTerm [OwlSyntax.DataRange] -> Phantoms.TTerm OwlSyntax.DataIntersectionOf
dataIntersectionOf x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.owl.syntax.DataIntersectionOf"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

dataMaxCardinality :: Phantoms.TTerm Integer -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm [OwlSyntax.DataRange] -> Phantoms.TTerm OwlSyntax.DataMaxCardinality
dataMaxCardinality bound property range =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataMaxCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm bound)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm range)}]}))

dataMaxCardinalityBound :: Phantoms.TTerm OwlSyntax.DataMaxCardinality -> Phantoms.TTerm Integer
dataMaxCardinalityBound x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataMaxCardinality"),
        Core.projectionField = (Core.Name "bound")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataMaxCardinalityProperty :: Phantoms.TTerm OwlSyntax.DataMaxCardinality -> Phantoms.TTerm OwlSyntax.DataPropertyExpression
dataMaxCardinalityProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataMaxCardinality"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataMaxCardinalityRange :: Phantoms.TTerm OwlSyntax.DataMaxCardinality -> Phantoms.TTerm [OwlSyntax.DataRange]
dataMaxCardinalityRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataMaxCardinality"),
        Core.projectionField = (Core.Name "range")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataMaxCardinalityWithBound :: Phantoms.TTerm OwlSyntax.DataMaxCardinality -> Phantoms.TTerm Integer -> Phantoms.TTerm OwlSyntax.DataMaxCardinality
dataMaxCardinalityWithBound original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataMaxCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataMaxCardinality"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataMaxCardinality"),
              Core.projectionField = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataMaxCardinalityWithProperty :: Phantoms.TTerm OwlSyntax.DataMaxCardinality -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm OwlSyntax.DataMaxCardinality
dataMaxCardinalityWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataMaxCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataMaxCardinality"),
              Core.projectionField = (Core.Name "bound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataMaxCardinality"),
              Core.projectionField = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataMaxCardinalityWithRange :: Phantoms.TTerm OwlSyntax.DataMaxCardinality -> Phantoms.TTerm [OwlSyntax.DataRange] -> Phantoms.TTerm OwlSyntax.DataMaxCardinality
dataMaxCardinalityWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataMaxCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataMaxCardinality"),
              Core.projectionField = (Core.Name "bound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataMaxCardinality"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataMinCardinality :: Phantoms.TTerm Integer -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm [OwlSyntax.DataRange] -> Phantoms.TTerm OwlSyntax.DataMinCardinality
dataMinCardinality bound property range =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataMinCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm bound)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm range)}]}))

dataMinCardinalityBound :: Phantoms.TTerm OwlSyntax.DataMinCardinality -> Phantoms.TTerm Integer
dataMinCardinalityBound x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataMinCardinality"),
        Core.projectionField = (Core.Name "bound")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataMinCardinalityProperty :: Phantoms.TTerm OwlSyntax.DataMinCardinality -> Phantoms.TTerm OwlSyntax.DataPropertyExpression
dataMinCardinalityProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataMinCardinality"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataMinCardinalityRange :: Phantoms.TTerm OwlSyntax.DataMinCardinality -> Phantoms.TTerm [OwlSyntax.DataRange]
dataMinCardinalityRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataMinCardinality"),
        Core.projectionField = (Core.Name "range")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataMinCardinalityWithBound :: Phantoms.TTerm OwlSyntax.DataMinCardinality -> Phantoms.TTerm Integer -> Phantoms.TTerm OwlSyntax.DataMinCardinality
dataMinCardinalityWithBound original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataMinCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataMinCardinality"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataMinCardinality"),
              Core.projectionField = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataMinCardinalityWithProperty :: Phantoms.TTerm OwlSyntax.DataMinCardinality -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm OwlSyntax.DataMinCardinality
dataMinCardinalityWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataMinCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataMinCardinality"),
              Core.projectionField = (Core.Name "bound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataMinCardinality"),
              Core.projectionField = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataMinCardinalityWithRange :: Phantoms.TTerm OwlSyntax.DataMinCardinality -> Phantoms.TTerm [OwlSyntax.DataRange] -> Phantoms.TTerm OwlSyntax.DataMinCardinality
dataMinCardinalityWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataMinCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataMinCardinality"),
              Core.projectionField = (Core.Name "bound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataMinCardinality"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataOneOf :: Phantoms.TTerm [RdfSyntax.Literal] -> Phantoms.TTerm OwlSyntax.DataOneOf
dataOneOf x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.owl.syntax.DataOneOf"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

dataProperty :: Phantoms.TTerm () -> Phantoms.TTerm OwlSyntax.DataProperty
dataProperty x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.owl.syntax.DataProperty"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

dataPropertyAssertion :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm OwlSyntax.Individual -> Phantoms.TTerm OwlSyntax.Individual -> Phantoms.TTerm OwlSyntax.DataPropertyAssertion
dataPropertyAssertion annotations property source target =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm source)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm target)}]}))

dataPropertyAssertionAnnotations :: Phantoms.TTerm OwlSyntax.DataPropertyAssertion -> Phantoms.TTerm [OwlSyntax.Annotation]
dataPropertyAssertionAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPropertyAssertionProperty :: Phantoms.TTerm OwlSyntax.DataPropertyAssertion -> Phantoms.TTerm OwlSyntax.DataPropertyExpression
dataPropertyAssertionProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPropertyAssertionSource :: Phantoms.TTerm OwlSyntax.DataPropertyAssertion -> Phantoms.TTerm OwlSyntax.Individual
dataPropertyAssertionSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
        Core.projectionField = (Core.Name "source")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPropertyAssertionTarget :: Phantoms.TTerm OwlSyntax.DataPropertyAssertion -> Phantoms.TTerm OwlSyntax.Individual
dataPropertyAssertionTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
        Core.projectionField = (Core.Name "target")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPropertyAssertionWithAnnotations :: Phantoms.TTerm OwlSyntax.DataPropertyAssertion -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.DataPropertyAssertion
dataPropertyAssertionWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPropertyAssertionWithProperty :: Phantoms.TTerm OwlSyntax.DataPropertyAssertion -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm OwlSyntax.DataPropertyAssertion
dataPropertyAssertionWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPropertyAssertionWithSource :: Phantoms.TTerm OwlSyntax.DataPropertyAssertion -> Phantoms.TTerm OwlSyntax.Individual -> Phantoms.TTerm OwlSyntax.DataPropertyAssertion
dataPropertyAssertionWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPropertyAssertionWithTarget :: Phantoms.TTerm OwlSyntax.DataPropertyAssertion -> Phantoms.TTerm OwlSyntax.Individual -> Phantoms.TTerm OwlSyntax.DataPropertyAssertion
dataPropertyAssertionWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataPropertyAxiomDataPropertyAxiom :: Phantoms.TTerm OwlSyntax.DataPropertyAxiom -> Phantoms.TTerm OwlSyntax.DataPropertyAxiom
dataPropertyAxiomDataPropertyAxiom x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataPropertyAxiom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataPropertyAxiomDataPropertyRange :: Phantoms.TTerm OwlSyntax.DataPropertyRange -> Phantoms.TTerm OwlSyntax.DataPropertyAxiom
dataPropertyAxiomDataPropertyRange x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataPropertyRange"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataPropertyAxiomDisjointDataProperties :: Phantoms.TTerm OwlSyntax.DisjointDataProperties -> Phantoms.TTerm OwlSyntax.DataPropertyAxiom
dataPropertyAxiomDisjointDataProperties x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "disjointDataProperties"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataPropertyAxiomEquivalentDataProperties :: Phantoms.TTerm OwlSyntax.EquivalentDataProperties -> Phantoms.TTerm OwlSyntax.DataPropertyAxiom
dataPropertyAxiomEquivalentDataProperties x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equivalentDataProperties"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataPropertyAxiomFunctionalDataProperty :: Phantoms.TTerm OwlSyntax.FunctionalDataProperty -> Phantoms.TTerm OwlSyntax.DataPropertyAxiom
dataPropertyAxiomFunctionalDataProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "functionalDataProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataPropertyAxiomSubDataPropertyOf :: Phantoms.TTerm OwlSyntax.SubDataPropertyOf -> Phantoms.TTerm OwlSyntax.DataPropertyAxiom
dataPropertyAxiomSubDataPropertyOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subDataPropertyOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataPropertyDomain :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm OwlSyntax.DataPropertyDomain
dataPropertyDomain annotations property domain =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm domain)}]}))

dataPropertyDomainAnnotations :: Phantoms.TTerm OwlSyntax.DataPropertyDomain -> Phantoms.TTerm [OwlSyntax.Annotation]
dataPropertyDomainAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyDomain"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPropertyDomainDomain :: Phantoms.TTerm OwlSyntax.DataPropertyDomain -> Phantoms.TTerm OwlSyntax.ClassExpression
dataPropertyDomainDomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyDomain"),
        Core.projectionField = (Core.Name "domain")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPropertyDomainProperty :: Phantoms.TTerm OwlSyntax.DataPropertyDomain -> Phantoms.TTerm OwlSyntax.DataPropertyExpression
dataPropertyDomainProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyDomain"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPropertyDomainWithAnnotations :: Phantoms.TTerm OwlSyntax.DataPropertyDomain -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.DataPropertyDomain
dataPropertyDomainWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyDomain"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyDomain"),
              Core.projectionField = (Core.Name "domain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPropertyDomainWithDomain :: Phantoms.TTerm OwlSyntax.DataPropertyDomain -> Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm OwlSyntax.DataPropertyDomain
dataPropertyDomainWithDomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyDomain"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyDomain"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataPropertyDomainWithProperty :: Phantoms.TTerm OwlSyntax.DataPropertyDomain -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm OwlSyntax.DataPropertyDomain
dataPropertyDomainWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyDomain"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyDomain"),
              Core.projectionField = (Core.Name "domain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPropertyExpression :: Phantoms.TTerm OwlSyntax.DataProperty -> Phantoms.TTerm OwlSyntax.DataPropertyExpression
dataPropertyExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.owl.syntax.DataPropertyExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

dataPropertyRange :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm OwlSyntax.DataPropertyRange
dataPropertyRange annotations property range =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm range)}]}))

dataPropertyRangeAnnotations :: Phantoms.TTerm OwlSyntax.DataPropertyRange -> Phantoms.TTerm [OwlSyntax.Annotation]
dataPropertyRangeAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyRange"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPropertyRangeProperty :: Phantoms.TTerm OwlSyntax.DataPropertyRange -> Phantoms.TTerm OwlSyntax.DataPropertyExpression
dataPropertyRangeProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyRange"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPropertyRangeRange :: Phantoms.TTerm OwlSyntax.DataPropertyRange -> Phantoms.TTerm OwlSyntax.ClassExpression
dataPropertyRangeRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyRange"),
        Core.projectionField = (Core.Name "range")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPropertyRangeWithAnnotations :: Phantoms.TTerm OwlSyntax.DataPropertyRange -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.DataPropertyRange
dataPropertyRangeWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyRange"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyRange"),
              Core.projectionField = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPropertyRangeWithProperty :: Phantoms.TTerm OwlSyntax.DataPropertyRange -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm OwlSyntax.DataPropertyRange
dataPropertyRangeWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyRange"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyRange"),
              Core.projectionField = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPropertyRangeWithRange :: Phantoms.TTerm OwlSyntax.DataPropertyRange -> Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm OwlSyntax.DataPropertyRange
dataPropertyRangeWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyRange"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataPropertyRange"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataRangeDataComplementOf :: Phantoms.TTerm OwlSyntax.DataComplementOf -> Phantoms.TTerm OwlSyntax.DataRange
dataRangeDataComplementOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.DataRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataComplementOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataRangeDataIntersectionOf :: Phantoms.TTerm OwlSyntax.DataIntersectionOf -> Phantoms.TTerm OwlSyntax.DataRange
dataRangeDataIntersectionOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.DataRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataIntersectionOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataRangeDataOneOf :: Phantoms.TTerm OwlSyntax.DataOneOf -> Phantoms.TTerm OwlSyntax.DataRange
dataRangeDataOneOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.DataRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataOneOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataRangeDataUnionOf :: Phantoms.TTerm OwlSyntax.DataUnionOf -> Phantoms.TTerm OwlSyntax.DataRange
dataRangeDataUnionOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.DataRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataUnionOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataRangeDatatype :: Phantoms.TTerm OwlSyntax.Datatype -> Phantoms.TTerm OwlSyntax.DataRange
dataRangeDatatype x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.DataRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "datatype"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataRangeDatatypeRestriction :: Phantoms.TTerm OwlSyntax.DatatypeRestriction -> Phantoms.TTerm OwlSyntax.DataRange
dataRangeDatatypeRestriction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.DataRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "datatypeRestriction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataSomeValuesFrom :: Phantoms.TTerm [OwlSyntax.DataPropertyExpression] -> Phantoms.TTerm OwlSyntax.DataRange -> Phantoms.TTerm OwlSyntax.DataSomeValuesFrom
dataSomeValuesFrom property range =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataSomeValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm range)}]}))

dataSomeValuesFromProperty :: Phantoms.TTerm OwlSyntax.DataSomeValuesFrom -> Phantoms.TTerm [OwlSyntax.DataPropertyExpression]
dataSomeValuesFromProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataSomeValuesFrom"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataSomeValuesFromRange :: Phantoms.TTerm OwlSyntax.DataSomeValuesFrom -> Phantoms.TTerm OwlSyntax.DataRange
dataSomeValuesFromRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataSomeValuesFrom"),
        Core.projectionField = (Core.Name "range")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataSomeValuesFromWithProperty :: Phantoms.TTerm OwlSyntax.DataSomeValuesFrom -> Phantoms.TTerm [OwlSyntax.DataPropertyExpression] -> Phantoms.TTerm OwlSyntax.DataSomeValuesFrom
dataSomeValuesFromWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataSomeValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataSomeValuesFrom"),
              Core.projectionField = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataSomeValuesFromWithRange :: Phantoms.TTerm OwlSyntax.DataSomeValuesFrom -> Phantoms.TTerm OwlSyntax.DataRange -> Phantoms.TTerm OwlSyntax.DataSomeValuesFrom
dataSomeValuesFromWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DataSomeValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DataSomeValuesFrom"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataUnionOf :: Phantoms.TTerm [OwlSyntax.DataRange] -> Phantoms.TTerm OwlSyntax.DataUnionOf
dataUnionOf x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.owl.syntax.DataUnionOf"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

datatypeDefinition :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.Datatype -> Phantoms.TTerm OwlSyntax.DataRange -> Phantoms.TTerm OwlSyntax.DatatypeDefinition
datatypeDefinition annotations datatype range =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DatatypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "datatype"),
          Core.fieldTerm = (Phantoms.unTTerm datatype)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm range)}]}))

datatypeDefinitionAnnotations :: Phantoms.TTerm OwlSyntax.DatatypeDefinition -> Phantoms.TTerm [OwlSyntax.Annotation]
datatypeDefinitionAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DatatypeDefinition"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

datatypeDefinitionDatatype :: Phantoms.TTerm OwlSyntax.DatatypeDefinition -> Phantoms.TTerm OwlSyntax.Datatype
datatypeDefinitionDatatype x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DatatypeDefinition"),
        Core.projectionField = (Core.Name "datatype")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

datatypeDefinitionRange :: Phantoms.TTerm OwlSyntax.DatatypeDefinition -> Phantoms.TTerm OwlSyntax.DataRange
datatypeDefinitionRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DatatypeDefinition"),
        Core.projectionField = (Core.Name "range")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

datatypeDefinitionWithAnnotations :: Phantoms.TTerm OwlSyntax.DatatypeDefinition -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.DatatypeDefinition
datatypeDefinitionWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DatatypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "datatype"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DatatypeDefinition"),
              Core.projectionField = (Core.Name "datatype")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DatatypeDefinition"),
              Core.projectionField = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

datatypeDefinitionWithDatatype :: Phantoms.TTerm OwlSyntax.DatatypeDefinition -> Phantoms.TTerm OwlSyntax.Datatype -> Phantoms.TTerm OwlSyntax.DatatypeDefinition
datatypeDefinitionWithDatatype original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DatatypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DatatypeDefinition"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "datatype"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DatatypeDefinition"),
              Core.projectionField = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

datatypeDefinitionWithRange :: Phantoms.TTerm OwlSyntax.DatatypeDefinition -> Phantoms.TTerm OwlSyntax.DataRange -> Phantoms.TTerm OwlSyntax.DatatypeDefinition
datatypeDefinitionWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DatatypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DatatypeDefinition"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "datatype"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DatatypeDefinition"),
              Core.projectionField = (Core.Name "datatype")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

datatypeOther :: Phantoms.TTerm RdfSyntax.Iri -> Phantoms.TTerm OwlSyntax.Datatype
datatypeOther x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

datatypeRestriction :: Phantoms.TTerm OwlSyntax.Datatype -> Phantoms.TTerm [OwlSyntax.DatatypeRestriction_Constraint] -> Phantoms.TTerm OwlSyntax.DatatypeRestriction
datatypeRestriction datatype constraints =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DatatypeRestriction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "datatype"),
          Core.fieldTerm = (Phantoms.unTTerm datatype)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTTerm constraints)}]}))

datatypeRestrictionConstraints :: Phantoms.TTerm OwlSyntax.DatatypeRestriction -> Phantoms.TTerm [OwlSyntax.DatatypeRestriction_Constraint]
datatypeRestrictionConstraints x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DatatypeRestriction"),
        Core.projectionField = (Core.Name "constraints")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

datatypeRestrictionDatatype :: Phantoms.TTerm OwlSyntax.DatatypeRestriction -> Phantoms.TTerm OwlSyntax.Datatype
datatypeRestrictionDatatype x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DatatypeRestriction"),
        Core.projectionField = (Core.Name "datatype")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

datatypeRestrictionWithConstraints :: Phantoms.TTerm OwlSyntax.DatatypeRestriction -> Phantoms.TTerm [OwlSyntax.DatatypeRestriction_Constraint] -> Phantoms.TTerm OwlSyntax.DatatypeRestriction
datatypeRestrictionWithConstraints original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DatatypeRestriction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "datatype"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DatatypeRestriction"),
              Core.projectionField = (Core.Name "datatype")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

datatypeRestrictionWithDatatype :: Phantoms.TTerm OwlSyntax.DatatypeRestriction -> Phantoms.TTerm OwlSyntax.Datatype -> Phantoms.TTerm OwlSyntax.DatatypeRestriction
datatypeRestrictionWithDatatype original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DatatypeRestriction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "datatype"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DatatypeRestriction"),
              Core.projectionField = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

datatypeRestriction_ConstrainingFacetOther :: Phantoms.TTerm RdfSyntax.Iri -> Phantoms.TTerm OwlSyntax.DatatypeRestriction_ConstrainingFacet
datatypeRestriction_ConstrainingFacetOther x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.DatatypeRestriction_ConstrainingFacet"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

datatypeRestriction_ConstrainingFacetXmlSchema :: Phantoms.TTerm Schema.ConstrainingFacet -> Phantoms.TTerm OwlSyntax.DatatypeRestriction_ConstrainingFacet
datatypeRestriction_ConstrainingFacetXmlSchema x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.DatatypeRestriction_ConstrainingFacet"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "xmlSchema"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

datatypeRestriction_Constraint :: Phantoms.TTerm OwlSyntax.DatatypeRestriction_ConstrainingFacet -> Phantoms.TTerm RdfSyntax.Literal -> Phantoms.TTerm OwlSyntax.DatatypeRestriction_Constraint
datatypeRestriction_Constraint constrainingFacet restrictionValue =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DatatypeRestriction_Constraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constrainingFacet"),
          Core.fieldTerm = (Phantoms.unTTerm constrainingFacet)},
        Core.Field {
          Core.fieldName = (Core.Name "restrictionValue"),
          Core.fieldTerm = (Phantoms.unTTerm restrictionValue)}]}))

datatypeRestriction_ConstraintConstrainingFacet :: Phantoms.TTerm OwlSyntax.DatatypeRestriction_Constraint -> Phantoms.TTerm OwlSyntax.DatatypeRestriction_ConstrainingFacet
datatypeRestriction_ConstraintConstrainingFacet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DatatypeRestriction_Constraint"),
        Core.projectionField = (Core.Name "constrainingFacet")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

datatypeRestriction_ConstraintRestrictionValue :: Phantoms.TTerm OwlSyntax.DatatypeRestriction_Constraint -> Phantoms.TTerm RdfSyntax.Literal
datatypeRestriction_ConstraintRestrictionValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DatatypeRestriction_Constraint"),
        Core.projectionField = (Core.Name "restrictionValue")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

datatypeRestriction_ConstraintWithConstrainingFacet :: Phantoms.TTerm OwlSyntax.DatatypeRestriction_Constraint -> Phantoms.TTerm OwlSyntax.DatatypeRestriction_ConstrainingFacet -> Phantoms.TTerm OwlSyntax.DatatypeRestriction_Constraint
datatypeRestriction_ConstraintWithConstrainingFacet original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DatatypeRestriction_Constraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constrainingFacet"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "restrictionValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DatatypeRestriction_Constraint"),
              Core.projectionField = (Core.Name "restrictionValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

datatypeRestriction_ConstraintWithRestrictionValue :: Phantoms.TTerm OwlSyntax.DatatypeRestriction_Constraint -> Phantoms.TTerm RdfSyntax.Literal -> Phantoms.TTerm OwlSyntax.DatatypeRestriction_Constraint
datatypeRestriction_ConstraintWithRestrictionValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DatatypeRestriction_Constraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constrainingFacet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DatatypeRestriction_Constraint"),
              Core.projectionField = (Core.Name "constrainingFacet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restrictionValue"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

datatypeXmlSchema :: Phantoms.TTerm Schema.Datatype -> Phantoms.TTerm OwlSyntax.Datatype
datatypeXmlSchema x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "xmlSchema"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declaration :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.Entity -> Phantoms.TTerm OwlSyntax.Declaration
declaration annotations entity =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.Declaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "entity"),
          Core.fieldTerm = (Phantoms.unTTerm entity)}]}))

declarationAnnotations :: Phantoms.TTerm OwlSyntax.Declaration -> Phantoms.TTerm [OwlSyntax.Annotation]
declarationAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Declaration"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

declarationEntity :: Phantoms.TTerm OwlSyntax.Declaration -> Phantoms.TTerm OwlSyntax.Entity
declarationEntity x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Declaration"),
        Core.projectionField = (Core.Name "entity")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

declarationWithAnnotations :: Phantoms.TTerm OwlSyntax.Declaration -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.Declaration
declarationWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.Declaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "entity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Declaration"),
              Core.projectionField = (Core.Name "entity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

declarationWithEntity :: Phantoms.TTerm OwlSyntax.Declaration -> Phantoms.TTerm OwlSyntax.Entity -> Phantoms.TTerm OwlSyntax.Declaration
declarationWithEntity original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.Declaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Declaration"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "entity"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

differentIndividuals :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm [OwlSyntax.Individual] -> Phantoms.TTerm OwlSyntax.DifferentIndividuals
differentIndividuals annotations individuals =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DifferentIndividuals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "individuals"),
          Core.fieldTerm = (Phantoms.unTTerm individuals)}]}))

differentIndividualsAnnotations :: Phantoms.TTerm OwlSyntax.DifferentIndividuals -> Phantoms.TTerm [OwlSyntax.Annotation]
differentIndividualsAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DifferentIndividuals"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

differentIndividualsIndividuals :: Phantoms.TTerm OwlSyntax.DifferentIndividuals -> Phantoms.TTerm [OwlSyntax.Individual]
differentIndividualsIndividuals x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DifferentIndividuals"),
        Core.projectionField = (Core.Name "individuals")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

differentIndividualsWithAnnotations :: Phantoms.TTerm OwlSyntax.DifferentIndividuals -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.DifferentIndividuals
differentIndividualsWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DifferentIndividuals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "individuals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DifferentIndividuals"),
              Core.projectionField = (Core.Name "individuals")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

differentIndividualsWithIndividuals :: Phantoms.TTerm OwlSyntax.DifferentIndividuals -> Phantoms.TTerm [OwlSyntax.Individual] -> Phantoms.TTerm OwlSyntax.DifferentIndividuals
differentIndividualsWithIndividuals original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DifferentIndividuals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DifferentIndividuals"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "individuals"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

disjointClasses :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm [OwlSyntax.ClassExpression] -> Phantoms.TTerm OwlSyntax.DisjointClasses
disjointClasses annotations classes =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DisjointClasses"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Phantoms.unTTerm classes)}]}))

disjointClassesAnnotations :: Phantoms.TTerm OwlSyntax.DisjointClasses -> Phantoms.TTerm [OwlSyntax.Annotation]
disjointClassesAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointClasses"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

disjointClassesClasses :: Phantoms.TTerm OwlSyntax.DisjointClasses -> Phantoms.TTerm [OwlSyntax.ClassExpression]
disjointClassesClasses x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointClasses"),
        Core.projectionField = (Core.Name "classes")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

disjointClassesWithAnnotations :: Phantoms.TTerm OwlSyntax.DisjointClasses -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.DisjointClasses
disjointClassesWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DisjointClasses"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointClasses"),
              Core.projectionField = (Core.Name "classes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

disjointClassesWithClasses :: Phantoms.TTerm OwlSyntax.DisjointClasses -> Phantoms.TTerm [OwlSyntax.ClassExpression] -> Phantoms.TTerm OwlSyntax.DisjointClasses
disjointClassesWithClasses original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DisjointClasses"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointClasses"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

disjointDataProperties :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm [OwlSyntax.DataPropertyExpression] -> Phantoms.TTerm OwlSyntax.DisjointDataProperties
disjointDataProperties annotations properties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DisjointDataProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)}]}))

disjointDataPropertiesAnnotations :: Phantoms.TTerm OwlSyntax.DisjointDataProperties -> Phantoms.TTerm [OwlSyntax.Annotation]
disjointDataPropertiesAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointDataProperties"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

disjointDataPropertiesProperties :: Phantoms.TTerm OwlSyntax.DisjointDataProperties -> Phantoms.TTerm [OwlSyntax.DataPropertyExpression]
disjointDataPropertiesProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointDataProperties"),
        Core.projectionField = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

disjointDataPropertiesWithAnnotations :: Phantoms.TTerm OwlSyntax.DisjointDataProperties -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.DisjointDataProperties
disjointDataPropertiesWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DisjointDataProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointDataProperties"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

disjointDataPropertiesWithProperties :: Phantoms.TTerm OwlSyntax.DisjointDataProperties -> Phantoms.TTerm [OwlSyntax.DataPropertyExpression] -> Phantoms.TTerm OwlSyntax.DisjointDataProperties
disjointDataPropertiesWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DisjointDataProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointDataProperties"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

disjointObjectProperties :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm [OwlSyntax.ObjectPropertyExpression] -> Phantoms.TTerm OwlSyntax.DisjointObjectProperties
disjointObjectProperties annotations properties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DisjointObjectProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)}]}))

disjointObjectPropertiesAnnotations :: Phantoms.TTerm OwlSyntax.DisjointObjectProperties -> Phantoms.TTerm [OwlSyntax.Annotation]
disjointObjectPropertiesAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointObjectProperties"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

disjointObjectPropertiesProperties :: Phantoms.TTerm OwlSyntax.DisjointObjectProperties -> Phantoms.TTerm [OwlSyntax.ObjectPropertyExpression]
disjointObjectPropertiesProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointObjectProperties"),
        Core.projectionField = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

disjointObjectPropertiesWithAnnotations :: Phantoms.TTerm OwlSyntax.DisjointObjectProperties -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.DisjointObjectProperties
disjointObjectPropertiesWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DisjointObjectProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointObjectProperties"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

disjointObjectPropertiesWithProperties :: Phantoms.TTerm OwlSyntax.DisjointObjectProperties -> Phantoms.TTerm [OwlSyntax.ObjectPropertyExpression] -> Phantoms.TTerm OwlSyntax.DisjointObjectProperties
disjointObjectPropertiesWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DisjointObjectProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointObjectProperties"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

disjointUnion :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.Class -> Phantoms.TTerm [OwlSyntax.ClassExpression] -> Phantoms.TTerm OwlSyntax.DisjointUnion
disjointUnion annotations class_ classes =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DisjointUnion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm class_)},
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Phantoms.unTTerm classes)}]}))

disjointUnionAnnotations :: Phantoms.TTerm OwlSyntax.DisjointUnion -> Phantoms.TTerm [OwlSyntax.Annotation]
disjointUnionAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointUnion"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

disjointUnionClass :: Phantoms.TTerm OwlSyntax.DisjointUnion -> Phantoms.TTerm OwlSyntax.Class
disjointUnionClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointUnion"),
        Core.projectionField = (Core.Name "class")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

disjointUnionClasses :: Phantoms.TTerm OwlSyntax.DisjointUnion -> Phantoms.TTerm [OwlSyntax.ClassExpression]
disjointUnionClasses x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointUnion"),
        Core.projectionField = (Core.Name "classes")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

disjointUnionWithAnnotations :: Phantoms.TTerm OwlSyntax.DisjointUnion -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.DisjointUnion
disjointUnionWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DisjointUnion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointUnion"),
              Core.projectionField = (Core.Name "class")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointUnion"),
              Core.projectionField = (Core.Name "classes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

disjointUnionWithClass :: Phantoms.TTerm OwlSyntax.DisjointUnion -> Phantoms.TTerm OwlSyntax.Class -> Phantoms.TTerm OwlSyntax.DisjointUnion
disjointUnionWithClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DisjointUnion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointUnion"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointUnion"),
              Core.projectionField = (Core.Name "classes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

disjointUnionWithClasses :: Phantoms.TTerm OwlSyntax.DisjointUnion -> Phantoms.TTerm [OwlSyntax.ClassExpression] -> Phantoms.TTerm OwlSyntax.DisjointUnion
disjointUnionWithClasses original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.DisjointUnion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointUnion"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.DisjointUnion"),
              Core.projectionField = (Core.Name "class")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

entityAnnotationProperty :: Phantoms.TTerm OwlSyntax.AnnotationProperty -> Phantoms.TTerm OwlSyntax.Entity
entityAnnotationProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Entity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotationProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

entityClass :: Phantoms.TTerm OwlSyntax.Class -> Phantoms.TTerm OwlSyntax.Entity
entityClass x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Entity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

entityDataProperty :: Phantoms.TTerm OwlSyntax.DataProperty -> Phantoms.TTerm OwlSyntax.Entity
entityDataProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Entity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

entityDatatype :: Phantoms.TTerm OwlSyntax.Datatype -> Phantoms.TTerm OwlSyntax.Entity
entityDatatype x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Entity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "datatype"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

entityNamedIndividual :: Phantoms.TTerm OwlSyntax.NamedIndividual -> Phantoms.TTerm OwlSyntax.Entity
entityNamedIndividual x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Entity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "namedIndividual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

entityObjectProperty :: Phantoms.TTerm OwlSyntax.ObjectProperty -> Phantoms.TTerm OwlSyntax.Entity
entityObjectProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Entity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

equivalentClasses :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm [OwlSyntax.ClassExpression] -> Phantoms.TTerm OwlSyntax.EquivalentClasses
equivalentClasses annotations classes =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.EquivalentClasses"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Phantoms.unTTerm classes)}]}))

equivalentClassesAnnotations :: Phantoms.TTerm OwlSyntax.EquivalentClasses -> Phantoms.TTerm [OwlSyntax.Annotation]
equivalentClassesAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.EquivalentClasses"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equivalentClassesClasses :: Phantoms.TTerm OwlSyntax.EquivalentClasses -> Phantoms.TTerm [OwlSyntax.ClassExpression]
equivalentClassesClasses x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.EquivalentClasses"),
        Core.projectionField = (Core.Name "classes")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equivalentClassesWithAnnotations :: Phantoms.TTerm OwlSyntax.EquivalentClasses -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.EquivalentClasses
equivalentClassesWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.EquivalentClasses"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.EquivalentClasses"),
              Core.projectionField = (Core.Name "classes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

equivalentClassesWithClasses :: Phantoms.TTerm OwlSyntax.EquivalentClasses -> Phantoms.TTerm [OwlSyntax.ClassExpression] -> Phantoms.TTerm OwlSyntax.EquivalentClasses
equivalentClassesWithClasses original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.EquivalentClasses"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.EquivalentClasses"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

equivalentDataProperties :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm [OwlSyntax.DataPropertyExpression] -> Phantoms.TTerm OwlSyntax.EquivalentDataProperties
equivalentDataProperties annotations properties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.EquivalentDataProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)}]}))

equivalentDataPropertiesAnnotations :: Phantoms.TTerm OwlSyntax.EquivalentDataProperties -> Phantoms.TTerm [OwlSyntax.Annotation]
equivalentDataPropertiesAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.EquivalentDataProperties"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equivalentDataPropertiesProperties :: Phantoms.TTerm OwlSyntax.EquivalentDataProperties -> Phantoms.TTerm [OwlSyntax.DataPropertyExpression]
equivalentDataPropertiesProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.EquivalentDataProperties"),
        Core.projectionField = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equivalentDataPropertiesWithAnnotations :: Phantoms.TTerm OwlSyntax.EquivalentDataProperties -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.EquivalentDataProperties
equivalentDataPropertiesWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.EquivalentDataProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.EquivalentDataProperties"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

equivalentDataPropertiesWithProperties :: Phantoms.TTerm OwlSyntax.EquivalentDataProperties -> Phantoms.TTerm [OwlSyntax.DataPropertyExpression] -> Phantoms.TTerm OwlSyntax.EquivalentDataProperties
equivalentDataPropertiesWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.EquivalentDataProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.EquivalentDataProperties"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

equivalentObjectProperties :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm [OwlSyntax.ObjectPropertyExpression] -> Phantoms.TTerm OwlSyntax.EquivalentObjectProperties
equivalentObjectProperties annotations properties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.EquivalentObjectProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)}]}))

equivalentObjectPropertiesAnnotations :: Phantoms.TTerm OwlSyntax.EquivalentObjectProperties -> Phantoms.TTerm [OwlSyntax.Annotation]
equivalentObjectPropertiesAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.EquivalentObjectProperties"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equivalentObjectPropertiesProperties :: Phantoms.TTerm OwlSyntax.EquivalentObjectProperties -> Phantoms.TTerm [OwlSyntax.ObjectPropertyExpression]
equivalentObjectPropertiesProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.EquivalentObjectProperties"),
        Core.projectionField = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equivalentObjectPropertiesWithAnnotations :: Phantoms.TTerm OwlSyntax.EquivalentObjectProperties -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.EquivalentObjectProperties
equivalentObjectPropertiesWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.EquivalentObjectProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.EquivalentObjectProperties"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

equivalentObjectPropertiesWithProperties :: Phantoms.TTerm OwlSyntax.EquivalentObjectProperties -> Phantoms.TTerm [OwlSyntax.ObjectPropertyExpression] -> Phantoms.TTerm OwlSyntax.EquivalentObjectProperties
equivalentObjectPropertiesWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.EquivalentObjectProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.EquivalentObjectProperties"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionalDataProperty :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm OwlSyntax.FunctionalDataProperty
functionalDataProperty annotations property =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.FunctionalDataProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)}]}))

functionalDataPropertyAnnotations :: Phantoms.TTerm OwlSyntax.FunctionalDataProperty -> Phantoms.TTerm [OwlSyntax.Annotation]
functionalDataPropertyAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.FunctionalDataProperty"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionalDataPropertyProperty :: Phantoms.TTerm OwlSyntax.FunctionalDataProperty -> Phantoms.TTerm OwlSyntax.DataPropertyExpression
functionalDataPropertyProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.FunctionalDataProperty"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionalDataPropertyWithAnnotations :: Phantoms.TTerm OwlSyntax.FunctionalDataProperty -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.FunctionalDataProperty
functionalDataPropertyWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.FunctionalDataProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.FunctionalDataProperty"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionalDataPropertyWithProperty :: Phantoms.TTerm OwlSyntax.FunctionalDataProperty -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm OwlSyntax.FunctionalDataProperty
functionalDataPropertyWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.FunctionalDataProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.FunctionalDataProperty"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionalObjectProperty :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.FunctionalObjectProperty
functionalObjectProperty annotations property =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.FunctionalObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)}]}))

functionalObjectPropertyAnnotations :: Phantoms.TTerm OwlSyntax.FunctionalObjectProperty -> Phantoms.TTerm [OwlSyntax.Annotation]
functionalObjectPropertyAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.FunctionalObjectProperty"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionalObjectPropertyProperty :: Phantoms.TTerm OwlSyntax.FunctionalObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
functionalObjectPropertyProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.FunctionalObjectProperty"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionalObjectPropertyWithAnnotations :: Phantoms.TTerm OwlSyntax.FunctionalObjectProperty -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.FunctionalObjectProperty
functionalObjectPropertyWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.FunctionalObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.FunctionalObjectProperty"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionalObjectPropertyWithProperty :: Phantoms.TTerm OwlSyntax.FunctionalObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.FunctionalObjectProperty
functionalObjectPropertyWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.FunctionalObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.FunctionalObjectProperty"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

hasKey :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm [OwlSyntax.ObjectPropertyExpression] -> Phantoms.TTerm [OwlSyntax.DataPropertyExpression] -> Phantoms.TTerm OwlSyntax.HasKey
hasKey annotations class_ objectProperties dataProperties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm class_)},
        Core.Field {
          Core.fieldName = (Core.Name "objectProperties"),
          Core.fieldTerm = (Phantoms.unTTerm objectProperties)},
        Core.Field {
          Core.fieldName = (Core.Name "dataProperties"),
          Core.fieldTerm = (Phantoms.unTTerm dataProperties)}]}))

hasKeyAnnotations :: Phantoms.TTerm OwlSyntax.HasKey -> Phantoms.TTerm [OwlSyntax.Annotation]
hasKeyAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hasKeyClass :: Phantoms.TTerm OwlSyntax.HasKey -> Phantoms.TTerm OwlSyntax.ClassExpression
hasKeyClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
        Core.projectionField = (Core.Name "class")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hasKeyDataProperties :: Phantoms.TTerm OwlSyntax.HasKey -> Phantoms.TTerm [OwlSyntax.DataPropertyExpression]
hasKeyDataProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
        Core.projectionField = (Core.Name "dataProperties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hasKeyObjectProperties :: Phantoms.TTerm OwlSyntax.HasKey -> Phantoms.TTerm [OwlSyntax.ObjectPropertyExpression]
hasKeyObjectProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
        Core.projectionField = (Core.Name "objectProperties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hasKeyWithAnnotations :: Phantoms.TTerm OwlSyntax.HasKey -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.HasKey
hasKeyWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "class")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "objectProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "objectProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "dataProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

hasKeyWithClass :: Phantoms.TTerm OwlSyntax.HasKey -> Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm OwlSyntax.HasKey
hasKeyWithClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "objectProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "objectProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "dataProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

hasKeyWithDataProperties :: Phantoms.TTerm OwlSyntax.HasKey -> Phantoms.TTerm [OwlSyntax.DataPropertyExpression] -> Phantoms.TTerm OwlSyntax.HasKey
hasKeyWithDataProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "class")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "objectProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "objectProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataProperties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

hasKeyWithObjectProperties :: Phantoms.TTerm OwlSyntax.HasKey -> Phantoms.TTerm [OwlSyntax.ObjectPropertyExpression] -> Phantoms.TTerm OwlSyntax.HasKey
hasKeyWithObjectProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "class")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "objectProperties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dataProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "dataProperties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

individualAnonymous :: Phantoms.TTerm OwlSyntax.AnonymousIndividual -> Phantoms.TTerm OwlSyntax.Individual
individualAnonymous x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Individual"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymous"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

individualNamed :: Phantoms.TTerm OwlSyntax.NamedIndividual -> Phantoms.TTerm OwlSyntax.Individual
individualNamed x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.Individual"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

inverseFunctionalObjectProperty :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.InverseFunctionalObjectProperty
inverseFunctionalObjectProperty annotations property =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.InverseFunctionalObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)}]}))

inverseFunctionalObjectPropertyAnnotations :: Phantoms.TTerm OwlSyntax.InverseFunctionalObjectProperty -> Phantoms.TTerm [OwlSyntax.Annotation]
inverseFunctionalObjectPropertyAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.InverseFunctionalObjectProperty"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inverseFunctionalObjectPropertyProperty :: Phantoms.TTerm OwlSyntax.InverseFunctionalObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
inverseFunctionalObjectPropertyProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.InverseFunctionalObjectProperty"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inverseFunctionalObjectPropertyWithAnnotations :: Phantoms.TTerm OwlSyntax.InverseFunctionalObjectProperty -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.InverseFunctionalObjectProperty
inverseFunctionalObjectPropertyWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.InverseFunctionalObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.InverseFunctionalObjectProperty"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inverseFunctionalObjectPropertyWithProperty :: Phantoms.TTerm OwlSyntax.InverseFunctionalObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.InverseFunctionalObjectProperty
inverseFunctionalObjectPropertyWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.InverseFunctionalObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.InverseFunctionalObjectProperty"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inverseObjectProperties :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.InverseObjectProperties
inverseObjectProperties annotations property1 property2 =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.InverseObjectProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property1"),
          Core.fieldTerm = (Phantoms.unTTerm property1)},
        Core.Field {
          Core.fieldName = (Core.Name "property2"),
          Core.fieldTerm = (Phantoms.unTTerm property2)}]}))

inverseObjectPropertiesAnnotations :: Phantoms.TTerm OwlSyntax.InverseObjectProperties -> Phantoms.TTerm [OwlSyntax.Annotation]
inverseObjectPropertiesAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.InverseObjectProperties"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inverseObjectPropertiesProperty1 :: Phantoms.TTerm OwlSyntax.InverseObjectProperties -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
inverseObjectPropertiesProperty1 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.InverseObjectProperties"),
        Core.projectionField = (Core.Name "property1")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inverseObjectPropertiesProperty2 :: Phantoms.TTerm OwlSyntax.InverseObjectProperties -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
inverseObjectPropertiesProperty2 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.InverseObjectProperties"),
        Core.projectionField = (Core.Name "property2")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inverseObjectPropertiesWithAnnotations :: Phantoms.TTerm OwlSyntax.InverseObjectProperties -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.InverseObjectProperties
inverseObjectPropertiesWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.InverseObjectProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.InverseObjectProperties"),
              Core.projectionField = (Core.Name "property1")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.InverseObjectProperties"),
              Core.projectionField = (Core.Name "property2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inverseObjectPropertiesWithProperty1 :: Phantoms.TTerm OwlSyntax.InverseObjectProperties -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.InverseObjectProperties
inverseObjectPropertiesWithProperty1 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.InverseObjectProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.InverseObjectProperties"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property1"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.InverseObjectProperties"),
              Core.projectionField = (Core.Name "property2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inverseObjectPropertiesWithProperty2 :: Phantoms.TTerm OwlSyntax.InverseObjectProperties -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.InverseObjectProperties
inverseObjectPropertiesWithProperty2 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.InverseObjectProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.InverseObjectProperties"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.InverseObjectProperties"),
              Core.projectionField = (Core.Name "property1")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property2"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inverseObjectProperty :: Phantoms.TTerm OwlSyntax.ObjectProperty -> Phantoms.TTerm OwlSyntax.InverseObjectProperty
inverseObjectProperty x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.owl.syntax.InverseObjectProperty"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

irreflexiveObjectProperty :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.IrreflexiveObjectProperty
irreflexiveObjectProperty annotations property =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.IrreflexiveObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)}]}))

irreflexiveObjectPropertyAnnotations :: Phantoms.TTerm OwlSyntax.IrreflexiveObjectProperty -> Phantoms.TTerm [OwlSyntax.Annotation]
irreflexiveObjectPropertyAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.IrreflexiveObjectProperty"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

irreflexiveObjectPropertyProperty :: Phantoms.TTerm OwlSyntax.IrreflexiveObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
irreflexiveObjectPropertyProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.IrreflexiveObjectProperty"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

irreflexiveObjectPropertyWithAnnotations :: Phantoms.TTerm OwlSyntax.IrreflexiveObjectProperty -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.IrreflexiveObjectProperty
irreflexiveObjectPropertyWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.IrreflexiveObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.IrreflexiveObjectProperty"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

irreflexiveObjectPropertyWithProperty :: Phantoms.TTerm OwlSyntax.IrreflexiveObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.IrreflexiveObjectProperty
irreflexiveObjectPropertyWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.IrreflexiveObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.IrreflexiveObjectProperty"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

namedIndividual :: Phantoms.TTerm () -> Phantoms.TTerm OwlSyntax.NamedIndividual
namedIndividual x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.owl.syntax.NamedIndividual"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

negativeDataPropertyAssertion :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm OwlSyntax.Individual -> Phantoms.TTerm OwlSyntax.Individual -> Phantoms.TTerm OwlSyntax.NegativeDataPropertyAssertion
negativeDataPropertyAssertion annotations property source target =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm source)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm target)}]}))

negativeDataPropertyAssertionAnnotations :: Phantoms.TTerm OwlSyntax.NegativeDataPropertyAssertion -> Phantoms.TTerm [OwlSyntax.Annotation]
negativeDataPropertyAssertionAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

negativeDataPropertyAssertionProperty :: Phantoms.TTerm OwlSyntax.NegativeDataPropertyAssertion -> Phantoms.TTerm OwlSyntax.DataPropertyExpression
negativeDataPropertyAssertionProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

negativeDataPropertyAssertionSource :: Phantoms.TTerm OwlSyntax.NegativeDataPropertyAssertion -> Phantoms.TTerm OwlSyntax.Individual
negativeDataPropertyAssertionSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
        Core.projectionField = (Core.Name "source")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

negativeDataPropertyAssertionTarget :: Phantoms.TTerm OwlSyntax.NegativeDataPropertyAssertion -> Phantoms.TTerm OwlSyntax.Individual
negativeDataPropertyAssertionTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
        Core.projectionField = (Core.Name "target")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

negativeDataPropertyAssertionWithAnnotations :: Phantoms.TTerm OwlSyntax.NegativeDataPropertyAssertion -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.NegativeDataPropertyAssertion
negativeDataPropertyAssertionWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

negativeDataPropertyAssertionWithProperty :: Phantoms.TTerm OwlSyntax.NegativeDataPropertyAssertion -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm OwlSyntax.NegativeDataPropertyAssertion
negativeDataPropertyAssertionWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

negativeDataPropertyAssertionWithSource :: Phantoms.TTerm OwlSyntax.NegativeDataPropertyAssertion -> Phantoms.TTerm OwlSyntax.Individual -> Phantoms.TTerm OwlSyntax.NegativeDataPropertyAssertion
negativeDataPropertyAssertionWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

negativeDataPropertyAssertionWithTarget :: Phantoms.TTerm OwlSyntax.NegativeDataPropertyAssertion -> Phantoms.TTerm OwlSyntax.Individual -> Phantoms.TTerm OwlSyntax.NegativeDataPropertyAssertion
negativeDataPropertyAssertionWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

negativeObjectPropertyAssertion :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.Individual -> Phantoms.TTerm OwlSyntax.Individual -> Phantoms.TTerm OwlSyntax.NegativeObjectPropertyAssertion
negativeObjectPropertyAssertion annotations property source target =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm source)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm target)}]}))

negativeObjectPropertyAssertionAnnotations :: Phantoms.TTerm OwlSyntax.NegativeObjectPropertyAssertion -> Phantoms.TTerm [OwlSyntax.Annotation]
negativeObjectPropertyAssertionAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

negativeObjectPropertyAssertionProperty :: Phantoms.TTerm OwlSyntax.NegativeObjectPropertyAssertion -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
negativeObjectPropertyAssertionProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

negativeObjectPropertyAssertionSource :: Phantoms.TTerm OwlSyntax.NegativeObjectPropertyAssertion -> Phantoms.TTerm OwlSyntax.Individual
negativeObjectPropertyAssertionSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
        Core.projectionField = (Core.Name "source")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

negativeObjectPropertyAssertionTarget :: Phantoms.TTerm OwlSyntax.NegativeObjectPropertyAssertion -> Phantoms.TTerm OwlSyntax.Individual
negativeObjectPropertyAssertionTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
        Core.projectionField = (Core.Name "target")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

negativeObjectPropertyAssertionWithAnnotations :: Phantoms.TTerm OwlSyntax.NegativeObjectPropertyAssertion -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.NegativeObjectPropertyAssertion
negativeObjectPropertyAssertionWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

negativeObjectPropertyAssertionWithProperty :: Phantoms.TTerm OwlSyntax.NegativeObjectPropertyAssertion -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.NegativeObjectPropertyAssertion
negativeObjectPropertyAssertionWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

negativeObjectPropertyAssertionWithSource :: Phantoms.TTerm OwlSyntax.NegativeObjectPropertyAssertion -> Phantoms.TTerm OwlSyntax.Individual -> Phantoms.TTerm OwlSyntax.NegativeObjectPropertyAssertion
negativeObjectPropertyAssertionWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

negativeObjectPropertyAssertionWithTarget :: Phantoms.TTerm OwlSyntax.NegativeObjectPropertyAssertion -> Phantoms.TTerm OwlSyntax.Individual -> Phantoms.TTerm OwlSyntax.NegativeObjectPropertyAssertion
negativeObjectPropertyAssertionWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectAllValuesFrom :: Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm OwlSyntax.ObjectAllValuesFrom
objectAllValuesFrom property class_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectAllValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm class_)}]}))

objectAllValuesFromClass :: Phantoms.TTerm OwlSyntax.ObjectAllValuesFrom -> Phantoms.TTerm OwlSyntax.ClassExpression
objectAllValuesFromClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectAllValuesFrom"),
        Core.projectionField = (Core.Name "class")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectAllValuesFromProperty :: Phantoms.TTerm OwlSyntax.ObjectAllValuesFrom -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
objectAllValuesFromProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectAllValuesFrom"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectAllValuesFromWithClass :: Phantoms.TTerm OwlSyntax.ObjectAllValuesFrom -> Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm OwlSyntax.ObjectAllValuesFrom
objectAllValuesFromWithClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectAllValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectAllValuesFrom"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectAllValuesFromWithProperty :: Phantoms.TTerm OwlSyntax.ObjectAllValuesFrom -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.ObjectAllValuesFrom
objectAllValuesFromWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectAllValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectAllValuesFrom"),
              Core.projectionField = (Core.Name "class")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectComplementOf :: Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm OwlSyntax.ObjectComplementOf
objectComplementOf x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.owl.syntax.ObjectComplementOf"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

objectExactCardinality :: Phantoms.TTerm Integer -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm [OwlSyntax.ClassExpression] -> Phantoms.TTerm OwlSyntax.ObjectExactCardinality
objectExactCardinality bound property class_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectExactCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm bound)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm class_)}]}))

objectExactCardinalityBound :: Phantoms.TTerm OwlSyntax.ObjectExactCardinality -> Phantoms.TTerm Integer
objectExactCardinalityBound x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectExactCardinality"),
        Core.projectionField = (Core.Name "bound")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectExactCardinalityClass :: Phantoms.TTerm OwlSyntax.ObjectExactCardinality -> Phantoms.TTerm [OwlSyntax.ClassExpression]
objectExactCardinalityClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectExactCardinality"),
        Core.projectionField = (Core.Name "class")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectExactCardinalityProperty :: Phantoms.TTerm OwlSyntax.ObjectExactCardinality -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
objectExactCardinalityProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectExactCardinality"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectExactCardinalityWithBound :: Phantoms.TTerm OwlSyntax.ObjectExactCardinality -> Phantoms.TTerm Integer -> Phantoms.TTerm OwlSyntax.ObjectExactCardinality
objectExactCardinalityWithBound original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectExactCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectExactCardinality"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectExactCardinality"),
              Core.projectionField = (Core.Name "class")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectExactCardinalityWithClass :: Phantoms.TTerm OwlSyntax.ObjectExactCardinality -> Phantoms.TTerm [OwlSyntax.ClassExpression] -> Phantoms.TTerm OwlSyntax.ObjectExactCardinality
objectExactCardinalityWithClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectExactCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectExactCardinality"),
              Core.projectionField = (Core.Name "bound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectExactCardinality"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectExactCardinalityWithProperty :: Phantoms.TTerm OwlSyntax.ObjectExactCardinality -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.ObjectExactCardinality
objectExactCardinalityWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectExactCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectExactCardinality"),
              Core.projectionField = (Core.Name "bound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectExactCardinality"),
              Core.projectionField = (Core.Name "class")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectHasSelf :: Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.ObjectHasSelf
objectHasSelf x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.owl.syntax.ObjectHasSelf"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

objectHasValue :: Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.Individual -> Phantoms.TTerm OwlSyntax.ObjectHasValue
objectHasValue property individual =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectHasValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "individual"),
          Core.fieldTerm = (Phantoms.unTTerm individual)}]}))

objectHasValueIndividual :: Phantoms.TTerm OwlSyntax.ObjectHasValue -> Phantoms.TTerm OwlSyntax.Individual
objectHasValueIndividual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectHasValue"),
        Core.projectionField = (Core.Name "individual")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectHasValueProperty :: Phantoms.TTerm OwlSyntax.ObjectHasValue -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
objectHasValueProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectHasValue"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectHasValueWithIndividual :: Phantoms.TTerm OwlSyntax.ObjectHasValue -> Phantoms.TTerm OwlSyntax.Individual -> Phantoms.TTerm OwlSyntax.ObjectHasValue
objectHasValueWithIndividual original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectHasValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectHasValue"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "individual"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectHasValueWithProperty :: Phantoms.TTerm OwlSyntax.ObjectHasValue -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.ObjectHasValue
objectHasValueWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectHasValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "individual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectHasValue"),
              Core.projectionField = (Core.Name "individual")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectIntersectionOf :: Phantoms.TTerm [OwlSyntax.ClassExpression] -> Phantoms.TTerm OwlSyntax.ObjectIntersectionOf
objectIntersectionOf x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.owl.syntax.ObjectIntersectionOf"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

objectMaxCardinality :: Phantoms.TTerm Integer -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm [OwlSyntax.ClassExpression] -> Phantoms.TTerm OwlSyntax.ObjectMaxCardinality
objectMaxCardinality bound property class_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectMaxCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm bound)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm class_)}]}))

objectMaxCardinalityBound :: Phantoms.TTerm OwlSyntax.ObjectMaxCardinality -> Phantoms.TTerm Integer
objectMaxCardinalityBound x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectMaxCardinality"),
        Core.projectionField = (Core.Name "bound")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectMaxCardinalityClass :: Phantoms.TTerm OwlSyntax.ObjectMaxCardinality -> Phantoms.TTerm [OwlSyntax.ClassExpression]
objectMaxCardinalityClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectMaxCardinality"),
        Core.projectionField = (Core.Name "class")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectMaxCardinalityProperty :: Phantoms.TTerm OwlSyntax.ObjectMaxCardinality -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
objectMaxCardinalityProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectMaxCardinality"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectMaxCardinalityWithBound :: Phantoms.TTerm OwlSyntax.ObjectMaxCardinality -> Phantoms.TTerm Integer -> Phantoms.TTerm OwlSyntax.ObjectMaxCardinality
objectMaxCardinalityWithBound original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectMaxCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectMaxCardinality"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectMaxCardinality"),
              Core.projectionField = (Core.Name "class")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectMaxCardinalityWithClass :: Phantoms.TTerm OwlSyntax.ObjectMaxCardinality -> Phantoms.TTerm [OwlSyntax.ClassExpression] -> Phantoms.TTerm OwlSyntax.ObjectMaxCardinality
objectMaxCardinalityWithClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectMaxCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectMaxCardinality"),
              Core.projectionField = (Core.Name "bound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectMaxCardinality"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectMaxCardinalityWithProperty :: Phantoms.TTerm OwlSyntax.ObjectMaxCardinality -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.ObjectMaxCardinality
objectMaxCardinalityWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectMaxCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectMaxCardinality"),
              Core.projectionField = (Core.Name "bound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectMaxCardinality"),
              Core.projectionField = (Core.Name "class")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectMinCardinality :: Phantoms.TTerm Integer -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm [OwlSyntax.ClassExpression] -> Phantoms.TTerm OwlSyntax.ObjectMinCardinality
objectMinCardinality bound property class_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectMinCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm bound)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm class_)}]}))

objectMinCardinalityBound :: Phantoms.TTerm OwlSyntax.ObjectMinCardinality -> Phantoms.TTerm Integer
objectMinCardinalityBound x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectMinCardinality"),
        Core.projectionField = (Core.Name "bound")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectMinCardinalityClass :: Phantoms.TTerm OwlSyntax.ObjectMinCardinality -> Phantoms.TTerm [OwlSyntax.ClassExpression]
objectMinCardinalityClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectMinCardinality"),
        Core.projectionField = (Core.Name "class")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectMinCardinalityProperty :: Phantoms.TTerm OwlSyntax.ObjectMinCardinality -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
objectMinCardinalityProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectMinCardinality"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectMinCardinalityWithBound :: Phantoms.TTerm OwlSyntax.ObjectMinCardinality -> Phantoms.TTerm Integer -> Phantoms.TTerm OwlSyntax.ObjectMinCardinality
objectMinCardinalityWithBound original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectMinCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectMinCardinality"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectMinCardinality"),
              Core.projectionField = (Core.Name "class")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectMinCardinalityWithClass :: Phantoms.TTerm OwlSyntax.ObjectMinCardinality -> Phantoms.TTerm [OwlSyntax.ClassExpression] -> Phantoms.TTerm OwlSyntax.ObjectMinCardinality
objectMinCardinalityWithClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectMinCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectMinCardinality"),
              Core.projectionField = (Core.Name "bound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectMinCardinality"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectMinCardinalityWithProperty :: Phantoms.TTerm OwlSyntax.ObjectMinCardinality -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.ObjectMinCardinality
objectMinCardinalityWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectMinCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectMinCardinality"),
              Core.projectionField = (Core.Name "bound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectMinCardinality"),
              Core.projectionField = (Core.Name "class")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectOneOf :: Phantoms.TTerm [OwlSyntax.Individual] -> Phantoms.TTerm OwlSyntax.ObjectOneOf
objectOneOf x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.owl.syntax.ObjectOneOf"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

objectProperty :: Phantoms.TTerm () -> Phantoms.TTerm OwlSyntax.ObjectProperty
objectProperty x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.owl.syntax.ObjectProperty"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

objectPropertyAssertion :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.Individual -> Phantoms.TTerm OwlSyntax.Individual -> Phantoms.TTerm OwlSyntax.ObjectPropertyAssertion
objectPropertyAssertion annotations property source target =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm source)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm target)}]}))

objectPropertyAssertionAnnotations :: Phantoms.TTerm OwlSyntax.ObjectPropertyAssertion -> Phantoms.TTerm [OwlSyntax.Annotation]
objectPropertyAssertionAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPropertyAssertionProperty :: Phantoms.TTerm OwlSyntax.ObjectPropertyAssertion -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
objectPropertyAssertionProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPropertyAssertionSource :: Phantoms.TTerm OwlSyntax.ObjectPropertyAssertion -> Phantoms.TTerm OwlSyntax.Individual
objectPropertyAssertionSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
        Core.projectionField = (Core.Name "source")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPropertyAssertionTarget :: Phantoms.TTerm OwlSyntax.ObjectPropertyAssertion -> Phantoms.TTerm OwlSyntax.Individual
objectPropertyAssertionTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
        Core.projectionField = (Core.Name "target")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPropertyAssertionWithAnnotations :: Phantoms.TTerm OwlSyntax.ObjectPropertyAssertion -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.ObjectPropertyAssertion
objectPropertyAssertionWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectPropertyAssertionWithProperty :: Phantoms.TTerm OwlSyntax.ObjectPropertyAssertion -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.ObjectPropertyAssertion
objectPropertyAssertionWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectPropertyAssertionWithSource :: Phantoms.TTerm OwlSyntax.ObjectPropertyAssertion -> Phantoms.TTerm OwlSyntax.Individual -> Phantoms.TTerm OwlSyntax.ObjectPropertyAssertion
objectPropertyAssertionWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectPropertyAssertionWithTarget :: Phantoms.TTerm OwlSyntax.ObjectPropertyAssertion -> Phantoms.TTerm OwlSyntax.Individual -> Phantoms.TTerm OwlSyntax.ObjectPropertyAssertion
objectPropertyAssertionWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectPropertyAxiomAsymmetricObjectProperty :: Phantoms.TTerm OwlSyntax.AsymmetricObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyAxiom
objectPropertyAxiomAsymmetricObjectProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "asymmetricObjectProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomDisjointObjectProperties :: Phantoms.TTerm OwlSyntax.DisjointObjectProperties -> Phantoms.TTerm OwlSyntax.ObjectPropertyAxiom
objectPropertyAxiomDisjointObjectProperties x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "disjointObjectProperties"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomEquivalentObjectProperties :: Phantoms.TTerm OwlSyntax.EquivalentObjectProperties -> Phantoms.TTerm OwlSyntax.ObjectPropertyAxiom
objectPropertyAxiomEquivalentObjectProperties x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equivalentObjectProperties"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomFunctionalObjectProperty :: Phantoms.TTerm OwlSyntax.FunctionalObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyAxiom
objectPropertyAxiomFunctionalObjectProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "functionalObjectProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomInverseFunctionalObjectProperty :: Phantoms.TTerm OwlSyntax.InverseFunctionalObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyAxiom
objectPropertyAxiomInverseFunctionalObjectProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inverseFunctionalObjectProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomInverseObjectProperties :: Phantoms.TTerm OwlSyntax.InverseObjectProperties -> Phantoms.TTerm OwlSyntax.ObjectPropertyAxiom
objectPropertyAxiomInverseObjectProperties x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inverseObjectProperties"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomIrreflexiveObjectProperty :: Phantoms.TTerm OwlSyntax.IrreflexiveObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyAxiom
objectPropertyAxiomIrreflexiveObjectProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "irreflexiveObjectProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomObjectPropertyDomain :: Phantoms.TTerm OwlSyntax.ObjectPropertyDomain -> Phantoms.TTerm OwlSyntax.ObjectPropertyAxiom
objectPropertyAxiomObjectPropertyDomain x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectPropertyDomain"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomObjectPropertyRange :: Phantoms.TTerm OwlSyntax.ObjectPropertyRange -> Phantoms.TTerm OwlSyntax.ObjectPropertyAxiom
objectPropertyAxiomObjectPropertyRange x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectPropertyRange"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomReflexiveObjectProperty :: Phantoms.TTerm OwlSyntax.ReflexiveObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyAxiom
objectPropertyAxiomReflexiveObjectProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reflexiveObjectProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomSubObjectPropertyOf :: Phantoms.TTerm OwlSyntax.SubObjectPropertyOf -> Phantoms.TTerm OwlSyntax.ObjectPropertyAxiom
objectPropertyAxiomSubObjectPropertyOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subObjectPropertyOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomSymmetricObjectProperty :: Phantoms.TTerm OwlSyntax.SymmetricObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyAxiom
objectPropertyAxiomSymmetricObjectProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "symmetricObjectProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomTransitiveObjectProperty :: Phantoms.TTerm OwlSyntax.TransitiveObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyAxiom
objectPropertyAxiomTransitiveObjectProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "transitiveObjectProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyDomain :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm OwlSyntax.ObjectPropertyDomain
objectPropertyDomain annotations property domain =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm domain)}]}))

objectPropertyDomainAnnotations :: Phantoms.TTerm OwlSyntax.ObjectPropertyDomain -> Phantoms.TTerm [OwlSyntax.Annotation]
objectPropertyDomainAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyDomain"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPropertyDomainDomain :: Phantoms.TTerm OwlSyntax.ObjectPropertyDomain -> Phantoms.TTerm OwlSyntax.ClassExpression
objectPropertyDomainDomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyDomain"),
        Core.projectionField = (Core.Name "domain")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPropertyDomainProperty :: Phantoms.TTerm OwlSyntax.ObjectPropertyDomain -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
objectPropertyDomainProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyDomain"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPropertyDomainWithAnnotations :: Phantoms.TTerm OwlSyntax.ObjectPropertyDomain -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.ObjectPropertyDomain
objectPropertyDomainWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyDomain"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyDomain"),
              Core.projectionField = (Core.Name "domain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectPropertyDomainWithDomain :: Phantoms.TTerm OwlSyntax.ObjectPropertyDomain -> Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm OwlSyntax.ObjectPropertyDomain
objectPropertyDomainWithDomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyDomain"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyDomain"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectPropertyDomainWithProperty :: Phantoms.TTerm OwlSyntax.ObjectPropertyDomain -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.ObjectPropertyDomain
objectPropertyDomainWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyDomain"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyDomain"),
              Core.projectionField = (Core.Name "domain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectPropertyExpressionInverseObject :: Phantoms.TTerm OwlSyntax.InverseObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
objectPropertyExpressionInverseObject x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inverseObject"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyExpressionObject :: Phantoms.TTerm OwlSyntax.ObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
objectPropertyExpressionObject x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyRange :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm OwlSyntax.ObjectPropertyRange
objectPropertyRange annotations property range =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm range)}]}))

objectPropertyRangeAnnotations :: Phantoms.TTerm OwlSyntax.ObjectPropertyRange -> Phantoms.TTerm [OwlSyntax.Annotation]
objectPropertyRangeAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyRange"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPropertyRangeProperty :: Phantoms.TTerm OwlSyntax.ObjectPropertyRange -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
objectPropertyRangeProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyRange"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPropertyRangeRange :: Phantoms.TTerm OwlSyntax.ObjectPropertyRange -> Phantoms.TTerm OwlSyntax.ClassExpression
objectPropertyRangeRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyRange"),
        Core.projectionField = (Core.Name "range")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPropertyRangeWithAnnotations :: Phantoms.TTerm OwlSyntax.ObjectPropertyRange -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.ObjectPropertyRange
objectPropertyRangeWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyRange"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyRange"),
              Core.projectionField = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectPropertyRangeWithProperty :: Phantoms.TTerm OwlSyntax.ObjectPropertyRange -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.ObjectPropertyRange
objectPropertyRangeWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyRange"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyRange"),
              Core.projectionField = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectPropertyRangeWithRange :: Phantoms.TTerm OwlSyntax.ObjectPropertyRange -> Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm OwlSyntax.ObjectPropertyRange
objectPropertyRangeWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyRange"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectPropertyRange"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectSomeValuesFrom :: Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm OwlSyntax.ObjectSomeValuesFrom
objectSomeValuesFrom property class_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectSomeValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm class_)}]}))

objectSomeValuesFromClass :: Phantoms.TTerm OwlSyntax.ObjectSomeValuesFrom -> Phantoms.TTerm OwlSyntax.ClassExpression
objectSomeValuesFromClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectSomeValuesFrom"),
        Core.projectionField = (Core.Name "class")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectSomeValuesFromProperty :: Phantoms.TTerm OwlSyntax.ObjectSomeValuesFrom -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
objectSomeValuesFromProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectSomeValuesFrom"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectSomeValuesFromWithClass :: Phantoms.TTerm OwlSyntax.ObjectSomeValuesFrom -> Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm OwlSyntax.ObjectSomeValuesFrom
objectSomeValuesFromWithClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectSomeValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectSomeValuesFrom"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectSomeValuesFromWithProperty :: Phantoms.TTerm OwlSyntax.ObjectSomeValuesFrom -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.ObjectSomeValuesFrom
objectSomeValuesFromWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ObjectSomeValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ObjectSomeValuesFrom"),
              Core.projectionField = (Core.Name "class")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectUnionOf :: Phantoms.TTerm [OwlSyntax.ClassExpression] -> Phantoms.TTerm OwlSyntax.ObjectUnionOf
objectUnionOf x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.owl.syntax.ObjectUnionOf"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

ontology :: Phantoms.TTerm [OwlSyntax.Ontology] -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm [OwlSyntax.Axiom] -> Phantoms.TTerm OwlSyntax.Ontology
ontology directImports annotations axioms =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.Ontology"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "directImports"),
          Core.fieldTerm = (Phantoms.unTTerm directImports)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "axioms"),
          Core.fieldTerm = (Phantoms.unTTerm axioms)}]}))

ontologyAnnotations :: Phantoms.TTerm OwlSyntax.Ontology -> Phantoms.TTerm [OwlSyntax.Annotation]
ontologyAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Ontology"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ontologyAxioms :: Phantoms.TTerm OwlSyntax.Ontology -> Phantoms.TTerm [OwlSyntax.Axiom]
ontologyAxioms x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Ontology"),
        Core.projectionField = (Core.Name "axioms")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ontologyDirectImports :: Phantoms.TTerm OwlSyntax.Ontology -> Phantoms.TTerm [OwlSyntax.Ontology]
ontologyDirectImports x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Ontology"),
        Core.projectionField = (Core.Name "directImports")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ontologyWithAnnotations :: Phantoms.TTerm OwlSyntax.Ontology -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.Ontology
ontologyWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.Ontology"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "directImports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Ontology"),
              Core.projectionField = (Core.Name "directImports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "axioms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Ontology"),
              Core.projectionField = (Core.Name "axioms")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ontologyWithAxioms :: Phantoms.TTerm OwlSyntax.Ontology -> Phantoms.TTerm [OwlSyntax.Axiom] -> Phantoms.TTerm OwlSyntax.Ontology
ontologyWithAxioms original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.Ontology"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "directImports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Ontology"),
              Core.projectionField = (Core.Name "directImports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Ontology"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "axioms"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

ontologyWithDirectImports :: Phantoms.TTerm OwlSyntax.Ontology -> Phantoms.TTerm [OwlSyntax.Ontology] -> Phantoms.TTerm OwlSyntax.Ontology
ontologyWithDirectImports original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.Ontology"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "directImports"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Ontology"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "axioms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.Ontology"),
              Core.projectionField = (Core.Name "axioms")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

reflexiveObjectProperty :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.ReflexiveObjectProperty
reflexiveObjectProperty annotations property =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ReflexiveObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)}]}))

reflexiveObjectPropertyAnnotations :: Phantoms.TTerm OwlSyntax.ReflexiveObjectProperty -> Phantoms.TTerm [OwlSyntax.Annotation]
reflexiveObjectPropertyAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ReflexiveObjectProperty"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

reflexiveObjectPropertyProperty :: Phantoms.TTerm OwlSyntax.ReflexiveObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
reflexiveObjectPropertyProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ReflexiveObjectProperty"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

reflexiveObjectPropertyWithAnnotations :: Phantoms.TTerm OwlSyntax.ReflexiveObjectProperty -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.ReflexiveObjectProperty
reflexiveObjectPropertyWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ReflexiveObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ReflexiveObjectProperty"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

reflexiveObjectPropertyWithProperty :: Phantoms.TTerm OwlSyntax.ReflexiveObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.ReflexiveObjectProperty
reflexiveObjectPropertyWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.ReflexiveObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.ReflexiveObjectProperty"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

sameIndividual :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm [OwlSyntax.Individual] -> Phantoms.TTerm OwlSyntax.SameIndividual
sameIndividual annotations individuals =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SameIndividual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "individuals"),
          Core.fieldTerm = (Phantoms.unTTerm individuals)}]}))

sameIndividualAnnotations :: Phantoms.TTerm OwlSyntax.SameIndividual -> Phantoms.TTerm [OwlSyntax.Annotation]
sameIndividualAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SameIndividual"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sameIndividualIndividuals :: Phantoms.TTerm OwlSyntax.SameIndividual -> Phantoms.TTerm [OwlSyntax.Individual]
sameIndividualIndividuals x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SameIndividual"),
        Core.projectionField = (Core.Name "individuals")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sameIndividualWithAnnotations :: Phantoms.TTerm OwlSyntax.SameIndividual -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.SameIndividual
sameIndividualWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SameIndividual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "individuals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SameIndividual"),
              Core.projectionField = (Core.Name "individuals")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

sameIndividualWithIndividuals :: Phantoms.TTerm OwlSyntax.SameIndividual -> Phantoms.TTerm [OwlSyntax.Individual] -> Phantoms.TTerm OwlSyntax.SameIndividual
sameIndividualWithIndividuals original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SameIndividual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SameIndividual"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "individuals"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

subAnnotationPropertyOf :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.AnnotationProperty -> Phantoms.TTerm OwlSyntax.AnnotationProperty -> Phantoms.TTerm OwlSyntax.SubAnnotationPropertyOf
subAnnotationPropertyOf annotations subProperty superProperty =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SubAnnotationPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Phantoms.unTTerm subProperty)},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Phantoms.unTTerm superProperty)}]}))

subAnnotationPropertyOfAnnotations :: Phantoms.TTerm OwlSyntax.SubAnnotationPropertyOf -> Phantoms.TTerm [OwlSyntax.Annotation]
subAnnotationPropertyOfAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubAnnotationPropertyOf"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subAnnotationPropertyOfSubProperty :: Phantoms.TTerm OwlSyntax.SubAnnotationPropertyOf -> Phantoms.TTerm OwlSyntax.AnnotationProperty
subAnnotationPropertyOfSubProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubAnnotationPropertyOf"),
        Core.projectionField = (Core.Name "subProperty")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subAnnotationPropertyOfSuperProperty :: Phantoms.TTerm OwlSyntax.SubAnnotationPropertyOf -> Phantoms.TTerm OwlSyntax.AnnotationProperty
subAnnotationPropertyOfSuperProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubAnnotationPropertyOf"),
        Core.projectionField = (Core.Name "superProperty")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subAnnotationPropertyOfWithAnnotations :: Phantoms.TTerm OwlSyntax.SubAnnotationPropertyOf -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.SubAnnotationPropertyOf
subAnnotationPropertyOfWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SubAnnotationPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubAnnotationPropertyOf"),
              Core.projectionField = (Core.Name "subProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubAnnotationPropertyOf"),
              Core.projectionField = (Core.Name "superProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subAnnotationPropertyOfWithSubProperty :: Phantoms.TTerm OwlSyntax.SubAnnotationPropertyOf -> Phantoms.TTerm OwlSyntax.AnnotationProperty -> Phantoms.TTerm OwlSyntax.SubAnnotationPropertyOf
subAnnotationPropertyOfWithSubProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SubAnnotationPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubAnnotationPropertyOf"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubAnnotationPropertyOf"),
              Core.projectionField = (Core.Name "superProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subAnnotationPropertyOfWithSuperProperty :: Phantoms.TTerm OwlSyntax.SubAnnotationPropertyOf -> Phantoms.TTerm OwlSyntax.AnnotationProperty -> Phantoms.TTerm OwlSyntax.SubAnnotationPropertyOf
subAnnotationPropertyOfWithSuperProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SubAnnotationPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubAnnotationPropertyOf"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubAnnotationPropertyOf"),
              Core.projectionField = (Core.Name "subProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

subClassOf :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm OwlSyntax.SubClassOf
subClassOf annotations subClass superClass =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SubClassOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "subClass"),
          Core.fieldTerm = (Phantoms.unTTerm subClass)},
        Core.Field {
          Core.fieldName = (Core.Name "superClass"),
          Core.fieldTerm = (Phantoms.unTTerm superClass)}]}))

subClassOfAnnotations :: Phantoms.TTerm OwlSyntax.SubClassOf -> Phantoms.TTerm [OwlSyntax.Annotation]
subClassOfAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubClassOf"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subClassOfSubClass :: Phantoms.TTerm OwlSyntax.SubClassOf -> Phantoms.TTerm OwlSyntax.ClassExpression
subClassOfSubClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubClassOf"),
        Core.projectionField = (Core.Name "subClass")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subClassOfSuperClass :: Phantoms.TTerm OwlSyntax.SubClassOf -> Phantoms.TTerm OwlSyntax.ClassExpression
subClassOfSuperClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubClassOf"),
        Core.projectionField = (Core.Name "superClass")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subClassOfWithAnnotations :: Phantoms.TTerm OwlSyntax.SubClassOf -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.SubClassOf
subClassOfWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SubClassOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubClassOf"),
              Core.projectionField = (Core.Name "subClass")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubClassOf"),
              Core.projectionField = (Core.Name "superClass")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subClassOfWithSubClass :: Phantoms.TTerm OwlSyntax.SubClassOf -> Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm OwlSyntax.SubClassOf
subClassOfWithSubClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SubClassOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubClassOf"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subClass"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "superClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubClassOf"),
              Core.projectionField = (Core.Name "superClass")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subClassOfWithSuperClass :: Phantoms.TTerm OwlSyntax.SubClassOf -> Phantoms.TTerm OwlSyntax.ClassExpression -> Phantoms.TTerm OwlSyntax.SubClassOf
subClassOfWithSuperClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SubClassOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubClassOf"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubClassOf"),
              Core.projectionField = (Core.Name "subClass")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superClass"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

subDataPropertyOf :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm OwlSyntax.SubDataPropertyOf
subDataPropertyOf annotations subProperty superProperty =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SubDataPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Phantoms.unTTerm subProperty)},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Phantoms.unTTerm superProperty)}]}))

subDataPropertyOfAnnotations :: Phantoms.TTerm OwlSyntax.SubDataPropertyOf -> Phantoms.TTerm [OwlSyntax.Annotation]
subDataPropertyOfAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubDataPropertyOf"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subDataPropertyOfSubProperty :: Phantoms.TTerm OwlSyntax.SubDataPropertyOf -> Phantoms.TTerm OwlSyntax.DataPropertyExpression
subDataPropertyOfSubProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubDataPropertyOf"),
        Core.projectionField = (Core.Name "subProperty")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subDataPropertyOfSuperProperty :: Phantoms.TTerm OwlSyntax.SubDataPropertyOf -> Phantoms.TTerm OwlSyntax.DataPropertyExpression
subDataPropertyOfSuperProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubDataPropertyOf"),
        Core.projectionField = (Core.Name "superProperty")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subDataPropertyOfWithAnnotations :: Phantoms.TTerm OwlSyntax.SubDataPropertyOf -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.SubDataPropertyOf
subDataPropertyOfWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SubDataPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubDataPropertyOf"),
              Core.projectionField = (Core.Name "subProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubDataPropertyOf"),
              Core.projectionField = (Core.Name "superProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subDataPropertyOfWithSubProperty :: Phantoms.TTerm OwlSyntax.SubDataPropertyOf -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm OwlSyntax.SubDataPropertyOf
subDataPropertyOfWithSubProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SubDataPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubDataPropertyOf"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubDataPropertyOf"),
              Core.projectionField = (Core.Name "superProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subDataPropertyOfWithSuperProperty :: Phantoms.TTerm OwlSyntax.SubDataPropertyOf -> Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm OwlSyntax.SubDataPropertyOf
subDataPropertyOfWithSuperProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SubDataPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubDataPropertyOf"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubDataPropertyOf"),
              Core.projectionField = (Core.Name "subProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

subObjectPropertyOf :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm [OwlSyntax.ObjectPropertyExpression] -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.SubObjectPropertyOf
subObjectPropertyOf annotations subProperty superProperty =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SubObjectPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Phantoms.unTTerm subProperty)},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Phantoms.unTTerm superProperty)}]}))

subObjectPropertyOfAnnotations :: Phantoms.TTerm OwlSyntax.SubObjectPropertyOf -> Phantoms.TTerm [OwlSyntax.Annotation]
subObjectPropertyOfAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubObjectPropertyOf"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subObjectPropertyOfSubProperty :: Phantoms.TTerm OwlSyntax.SubObjectPropertyOf -> Phantoms.TTerm [OwlSyntax.ObjectPropertyExpression]
subObjectPropertyOfSubProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubObjectPropertyOf"),
        Core.projectionField = (Core.Name "subProperty")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subObjectPropertyOfSuperProperty :: Phantoms.TTerm OwlSyntax.SubObjectPropertyOf -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
subObjectPropertyOfSuperProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubObjectPropertyOf"),
        Core.projectionField = (Core.Name "superProperty")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subObjectPropertyOfWithAnnotations :: Phantoms.TTerm OwlSyntax.SubObjectPropertyOf -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.SubObjectPropertyOf
subObjectPropertyOfWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SubObjectPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubObjectPropertyOf"),
              Core.projectionField = (Core.Name "subProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubObjectPropertyOf"),
              Core.projectionField = (Core.Name "superProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subObjectPropertyOfWithSubProperty :: Phantoms.TTerm OwlSyntax.SubObjectPropertyOf -> Phantoms.TTerm [OwlSyntax.ObjectPropertyExpression] -> Phantoms.TTerm OwlSyntax.SubObjectPropertyOf
subObjectPropertyOfWithSubProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SubObjectPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubObjectPropertyOf"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubObjectPropertyOf"),
              Core.projectionField = (Core.Name "superProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subObjectPropertyOfWithSuperProperty :: Phantoms.TTerm OwlSyntax.SubObjectPropertyOf -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.SubObjectPropertyOf
subObjectPropertyOfWithSuperProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SubObjectPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubObjectPropertyOf"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SubObjectPropertyOf"),
              Core.projectionField = (Core.Name "subProperty")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

symmetricObjectProperty :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.SymmetricObjectProperty
symmetricObjectProperty annotations property =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SymmetricObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)}]}))

symmetricObjectPropertyAnnotations :: Phantoms.TTerm OwlSyntax.SymmetricObjectProperty -> Phantoms.TTerm [OwlSyntax.Annotation]
symmetricObjectPropertyAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SymmetricObjectProperty"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

symmetricObjectPropertyProperty :: Phantoms.TTerm OwlSyntax.SymmetricObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
symmetricObjectPropertyProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SymmetricObjectProperty"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

symmetricObjectPropertyWithAnnotations :: Phantoms.TTerm OwlSyntax.SymmetricObjectProperty -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.SymmetricObjectProperty
symmetricObjectPropertyWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SymmetricObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SymmetricObjectProperty"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

symmetricObjectPropertyWithProperty :: Phantoms.TTerm OwlSyntax.SymmetricObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.SymmetricObjectProperty
symmetricObjectPropertyWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.SymmetricObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.SymmetricObjectProperty"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

transitiveObjectProperty :: Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.TransitiveObjectProperty
transitiveObjectProperty annotations property =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.TransitiveObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)}]}))

transitiveObjectPropertyAnnotations :: Phantoms.TTerm OwlSyntax.TransitiveObjectProperty -> Phantoms.TTerm [OwlSyntax.Annotation]
transitiveObjectPropertyAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.TransitiveObjectProperty"),
        Core.projectionField = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

transitiveObjectPropertyProperty :: Phantoms.TTerm OwlSyntax.TransitiveObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
transitiveObjectPropertyProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.owl.syntax.TransitiveObjectProperty"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

transitiveObjectPropertyWithAnnotations :: Phantoms.TTerm OwlSyntax.TransitiveObjectProperty -> Phantoms.TTerm [OwlSyntax.Annotation] -> Phantoms.TTerm OwlSyntax.TransitiveObjectProperty
transitiveObjectPropertyWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.TransitiveObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.TransitiveObjectProperty"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

transitiveObjectPropertyWithProperty :: Phantoms.TTerm OwlSyntax.TransitiveObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression -> Phantoms.TTerm OwlSyntax.TransitiveObjectProperty
transitiveObjectPropertyWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.owl.syntax.TransitiveObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.owl.syntax.TransitiveObjectProperty"),
              Core.projectionField = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unAnnotationProperty :: Phantoms.TTerm OwlSyntax.AnnotationProperty -> Phantoms.TTerm ()
unAnnotationProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.owl.syntax.AnnotationProperty")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unAnonymousIndividual :: Phantoms.TTerm OwlSyntax.AnonymousIndividual -> Phantoms.TTerm ()
unAnonymousIndividual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.owl.syntax.AnonymousIndividual")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unClass :: Phantoms.TTerm OwlSyntax.Class -> Phantoms.TTerm ()
unClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.owl.syntax.Class")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDataComplementOf :: Phantoms.TTerm OwlSyntax.DataComplementOf -> Phantoms.TTerm OwlSyntax.DataRange
unDataComplementOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.owl.syntax.DataComplementOf")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDataIntersectionOf :: Phantoms.TTerm OwlSyntax.DataIntersectionOf -> Phantoms.TTerm [OwlSyntax.DataRange]
unDataIntersectionOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.owl.syntax.DataIntersectionOf")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDataOneOf :: Phantoms.TTerm OwlSyntax.DataOneOf -> Phantoms.TTerm [RdfSyntax.Literal]
unDataOneOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.owl.syntax.DataOneOf")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDataProperty :: Phantoms.TTerm OwlSyntax.DataProperty -> Phantoms.TTerm ()
unDataProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.owl.syntax.DataProperty")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDataPropertyExpression :: Phantoms.TTerm OwlSyntax.DataPropertyExpression -> Phantoms.TTerm OwlSyntax.DataProperty
unDataPropertyExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.owl.syntax.DataPropertyExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDataUnionOf :: Phantoms.TTerm OwlSyntax.DataUnionOf -> Phantoms.TTerm [OwlSyntax.DataRange]
unDataUnionOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.owl.syntax.DataUnionOf")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unInverseObjectProperty :: Phantoms.TTerm OwlSyntax.InverseObjectProperty -> Phantoms.TTerm OwlSyntax.ObjectProperty
unInverseObjectProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.owl.syntax.InverseObjectProperty")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unNamedIndividual :: Phantoms.TTerm OwlSyntax.NamedIndividual -> Phantoms.TTerm ()
unNamedIndividual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.owl.syntax.NamedIndividual")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unObjectComplementOf :: Phantoms.TTerm OwlSyntax.ObjectComplementOf -> Phantoms.TTerm OwlSyntax.ClassExpression
unObjectComplementOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.owl.syntax.ObjectComplementOf")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unObjectHasSelf :: Phantoms.TTerm OwlSyntax.ObjectHasSelf -> Phantoms.TTerm OwlSyntax.ObjectPropertyExpression
unObjectHasSelf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.owl.syntax.ObjectHasSelf")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unObjectIntersectionOf :: Phantoms.TTerm OwlSyntax.ObjectIntersectionOf -> Phantoms.TTerm [OwlSyntax.ClassExpression]
unObjectIntersectionOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.owl.syntax.ObjectIntersectionOf")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unObjectOneOf :: Phantoms.TTerm OwlSyntax.ObjectOneOf -> Phantoms.TTerm [OwlSyntax.Individual]
unObjectOneOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.owl.syntax.ObjectOneOf")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unObjectProperty :: Phantoms.TTerm OwlSyntax.ObjectProperty -> Phantoms.TTerm ()
unObjectProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.owl.syntax.ObjectProperty")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unObjectUnionOf :: Phantoms.TTerm OwlSyntax.ObjectUnionOf -> Phantoms.TTerm [OwlSyntax.ClassExpression]
unObjectUnionOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.owl.syntax.ObjectUnionOf")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
