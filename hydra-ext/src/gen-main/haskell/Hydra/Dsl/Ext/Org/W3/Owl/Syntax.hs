-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.org.w3.owl.syntax

module Hydra.Dsl.Ext.Org.W3.Owl.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Org.W3.Owl.Syntax as Syntax
import qualified Hydra.Ext.Org.W3.Rdf.Syntax as Syntax_
import qualified Hydra.Ext.Org.W3.Xml.Schema as Schema
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

annotation :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.AnnotationProperty -> Phantoms.TTerm Syntax.AnnotationValue -> Phantoms.TTerm Syntax.Annotation
annotation annotations property value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Annotation"),
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

annotationAnnotations :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm [Syntax.Annotation]
annotationAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Annotation"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationAssertion :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.AnnotationProperty -> Phantoms.TTerm Syntax.AnnotationSubject -> Phantoms.TTerm Syntax.AnnotationValue -> Phantoms.TTerm Syntax.AnnotationAssertion
annotationAssertion annotations property subject value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
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

annotationAssertionAnnotations :: Phantoms.TTerm Syntax.AnnotationAssertion -> Phantoms.TTerm [Syntax.Annotation]
annotationAssertionAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationAssertionProperty :: Phantoms.TTerm Syntax.AnnotationAssertion -> Phantoms.TTerm Syntax.AnnotationProperty
annotationAssertionProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationAssertionSubject :: Phantoms.TTerm Syntax.AnnotationAssertion -> Phantoms.TTerm Syntax.AnnotationSubject
annotationAssertionSubject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
        Core.projectionField = (Core.Name "subject")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationAssertionValue :: Phantoms.TTerm Syntax.AnnotationAssertion -> Phantoms.TTerm Syntax.AnnotationValue
annotationAssertionValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationAssertionWithAnnotations :: Phantoms.TTerm Syntax.AnnotationAssertion -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.AnnotationAssertion
annotationAssertionWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "subject")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationAssertionWithProperty :: Phantoms.TTerm Syntax.AnnotationAssertion -> Phantoms.TTerm Syntax.AnnotationProperty -> Phantoms.TTerm Syntax.AnnotationAssertion
annotationAssertionWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "subject")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationAssertionWithSubject :: Phantoms.TTerm Syntax.AnnotationAssertion -> Phantoms.TTerm Syntax.AnnotationSubject -> Phantoms.TTerm Syntax.AnnotationAssertion
annotationAssertionWithSubject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationAssertionWithValue :: Phantoms.TTerm Syntax.AnnotationAssertion -> Phantoms.TTerm Syntax.AnnotationValue -> Phantoms.TTerm Syntax.AnnotationAssertion
annotationAssertionWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAssertion"),
              Core.projectionField = (Core.Name "subject")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

annotationAxiomAnnotationAssertion :: Phantoms.TTerm Syntax.AnnotationAssertion -> Phantoms.TTerm Syntax.AnnotationAxiom
annotationAxiomAnnotationAssertion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotationAssertion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationAxiomAnnotationPropertyDomain :: Phantoms.TTerm Syntax.AnnotationPropertyDomain -> Phantoms.TTerm Syntax.AnnotationAxiom
annotationAxiomAnnotationPropertyDomain x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotationPropertyDomain"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationAxiomAnnotationPropertyRange :: Phantoms.TTerm Syntax.AnnotationPropertyRange -> Phantoms.TTerm Syntax.AnnotationAxiom
annotationAxiomAnnotationPropertyRange x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotationPropertyRange"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationAxiomSubAnnotationPropertyOf :: Phantoms.TTerm Syntax.SubAnnotationPropertyOf -> Phantoms.TTerm Syntax.AnnotationAxiom
annotationAxiomSubAnnotationPropertyOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subAnnotationPropertyOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationProperty :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.AnnotationProperty
annotationProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Annotation"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationPropertyDomain :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.AnnotationProperty -> Phantoms.TTerm Syntax_.Iri -> Phantoms.TTerm Syntax.AnnotationPropertyDomain
annotationPropertyDomain annotations property iri =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyDomain"),
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

annotationPropertyDomainAnnotations :: Phantoms.TTerm Syntax.AnnotationPropertyDomain -> Phantoms.TTerm [Syntax.Annotation]
annotationPropertyDomainAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyDomain"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationPropertyDomainIri :: Phantoms.TTerm Syntax.AnnotationPropertyDomain -> Phantoms.TTerm Syntax_.Iri
annotationPropertyDomainIri x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyDomain"),
        Core.projectionField = (Core.Name "iri")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationPropertyDomainProperty :: Phantoms.TTerm Syntax.AnnotationPropertyDomain -> Phantoms.TTerm Syntax.AnnotationProperty
annotationPropertyDomainProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyDomain"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationPropertyDomainWithAnnotations :: Phantoms.TTerm Syntax.AnnotationPropertyDomain -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.AnnotationPropertyDomain
annotationPropertyDomainWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyDomain"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyDomain"),
              Core.projectionField = (Core.Name "iri")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationPropertyDomainWithIri :: Phantoms.TTerm Syntax.AnnotationPropertyDomain -> Phantoms.TTerm Syntax_.Iri -> Phantoms.TTerm Syntax.AnnotationPropertyDomain
annotationPropertyDomainWithIri original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyDomain"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyDomain"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

annotationPropertyDomainWithProperty :: Phantoms.TTerm Syntax.AnnotationPropertyDomain -> Phantoms.TTerm Syntax.AnnotationProperty -> Phantoms.TTerm Syntax.AnnotationPropertyDomain
annotationPropertyDomainWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyDomain"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyDomain"),
              Core.projectionField = (Core.Name "iri")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationPropertyRange :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.AnnotationProperty -> Phantoms.TTerm Syntax_.Iri -> Phantoms.TTerm Syntax.AnnotationPropertyRange
annotationPropertyRange annotations property iri =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyRange"),
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

annotationPropertyRangeAnnotations :: Phantoms.TTerm Syntax.AnnotationPropertyRange -> Phantoms.TTerm [Syntax.Annotation]
annotationPropertyRangeAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyRange"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationPropertyRangeIri :: Phantoms.TTerm Syntax.AnnotationPropertyRange -> Phantoms.TTerm Syntax_.Iri
annotationPropertyRangeIri x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyRange"),
        Core.projectionField = (Core.Name "iri")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationPropertyRangeProperty :: Phantoms.TTerm Syntax.AnnotationPropertyRange -> Phantoms.TTerm Syntax.AnnotationProperty
annotationPropertyRangeProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyRange"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationPropertyRangeWithAnnotations :: Phantoms.TTerm Syntax.AnnotationPropertyRange -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.AnnotationPropertyRange
annotationPropertyRangeWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyRange"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyRange"),
              Core.projectionField = (Core.Name "iri")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationPropertyRangeWithIri :: Phantoms.TTerm Syntax.AnnotationPropertyRange -> Phantoms.TTerm Syntax_.Iri -> Phantoms.TTerm Syntax.AnnotationPropertyRange
annotationPropertyRangeWithIri original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyRange"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyRange"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

annotationPropertyRangeWithProperty :: Phantoms.TTerm Syntax.AnnotationPropertyRange -> Phantoms.TTerm Syntax.AnnotationProperty -> Phantoms.TTerm Syntax.AnnotationPropertyRange
annotationPropertyRangeWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyRange"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "iri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationPropertyRange"),
              Core.projectionField = (Core.Name "iri")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationProperty_ :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.AnnotationProperty
annotationProperty_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationProperty"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

annotationSubjectAnonymousIndividual :: Phantoms.TTerm Syntax.AnonymousIndividual -> Phantoms.TTerm Syntax.AnnotationSubject
annotationSubjectAnonymousIndividual x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationSubject"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymousIndividual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationSubjectIri :: Phantoms.TTerm Syntax_.Iri -> Phantoms.TTerm Syntax.AnnotationSubject
annotationSubjectIri x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationSubject"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iri"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationValue :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.AnnotationValue
annotationValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Annotation"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationValueAnonymousIndividual :: Phantoms.TTerm Syntax.AnonymousIndividual -> Phantoms.TTerm Syntax.AnnotationValue
annotationValueAnonymousIndividual x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymousIndividual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationValueIri :: Phantoms.TTerm Syntax_.Iri -> Phantoms.TTerm Syntax.AnnotationValue
annotationValueIri x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iri"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationValueLiteral :: Phantoms.TTerm Syntax_.Literal -> Phantoms.TTerm Syntax.AnnotationValue
annotationValueLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotationWithAnnotations :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.Annotation
annotationWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Annotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Annotation"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Annotation"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationWithProperty :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.AnnotationProperty -> Phantoms.TTerm Syntax.Annotation
annotationWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Annotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Annotation"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Annotation"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationWithValue :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.AnnotationValue -> Phantoms.TTerm Syntax.Annotation
annotationWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Annotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Annotation"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Annotation"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

anonymousIndividual :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.AnonymousIndividual
anonymousIndividual x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AnonymousIndividual"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

assertionClassAssertion :: Phantoms.TTerm Syntax.ClassAssertion -> Phantoms.TTerm Syntax.Assertion
assertionClassAssertion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Assertion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classAssertion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assertionDataPropertyAssertion :: Phantoms.TTerm Syntax.DataPropertyAssertion -> Phantoms.TTerm Syntax.Assertion
assertionDataPropertyAssertion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Assertion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataPropertyAssertion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assertionDifferentIndividuals :: Phantoms.TTerm Syntax.DifferentIndividuals -> Phantoms.TTerm Syntax.Assertion
assertionDifferentIndividuals x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Assertion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "differentIndividuals"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assertionNegativeDataPropertyAssertion :: Phantoms.TTerm Syntax.NegativeDataPropertyAssertion -> Phantoms.TTerm Syntax.Assertion
assertionNegativeDataPropertyAssertion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Assertion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negativeDataPropertyAssertion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assertionNegativeObjectPropertyAssertion :: Phantoms.TTerm Syntax.NegativeObjectPropertyAssertion -> Phantoms.TTerm Syntax.Assertion
assertionNegativeObjectPropertyAssertion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Assertion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negativeObjectPropertyAssertion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assertionObjectPropertyAssertion :: Phantoms.TTerm Syntax.ObjectPropertyAssertion -> Phantoms.TTerm Syntax.Assertion
assertionObjectPropertyAssertion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Assertion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectPropertyAssertion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assertionSameIndividual :: Phantoms.TTerm Syntax.SameIndividual -> Phantoms.TTerm Syntax.Assertion
assertionSameIndividual x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Assertion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sameIndividual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

asymmetricObjectProperty :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.AsymmetricObjectProperty
asymmetricObjectProperty annotations property =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AsymmetricObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)}]}))

asymmetricObjectPropertyAnnotations :: Phantoms.TTerm Syntax.AsymmetricObjectProperty -> Phantoms.TTerm [Syntax.Annotation]
asymmetricObjectPropertyAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AsymmetricObjectProperty"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

asymmetricObjectPropertyProperty :: Phantoms.TTerm Syntax.AsymmetricObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyExpression
asymmetricObjectPropertyProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AsymmetricObjectProperty"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

asymmetricObjectPropertyWithAnnotations :: Phantoms.TTerm Syntax.AsymmetricObjectProperty -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.AsymmetricObjectProperty
asymmetricObjectPropertyWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AsymmetricObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AsymmetricObjectProperty"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

asymmetricObjectPropertyWithProperty :: Phantoms.TTerm Syntax.AsymmetricObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.AsymmetricObjectProperty
asymmetricObjectPropertyWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AsymmetricObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.AsymmetricObjectProperty"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

axiomAnnotationAxiom :: Phantoms.TTerm Syntax.AnnotationAxiom -> Phantoms.TTerm Syntax.Axiom
axiomAnnotationAxiom x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Axiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotationAxiom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

axiomAssertion :: Phantoms.TTerm Syntax.Assertion -> Phantoms.TTerm Syntax.Axiom
axiomAssertion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Axiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assertion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

axiomClassAxiom :: Phantoms.TTerm Syntax.ClassAxiom -> Phantoms.TTerm Syntax.Axiom
axiomClassAxiom x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Axiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classAxiom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

axiomDataPropertyAxiom :: Phantoms.TTerm Syntax.DataPropertyAxiom -> Phantoms.TTerm Syntax.Axiom
axiomDataPropertyAxiom x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Axiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataPropertyAxiom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

axiomDatatypeDefinition :: Phantoms.TTerm Syntax.DatatypeDefinition -> Phantoms.TTerm Syntax.Axiom
axiomDatatypeDefinition x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Axiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "datatypeDefinition"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

axiomDeclaration :: Phantoms.TTerm Syntax.Declaration -> Phantoms.TTerm Syntax.Axiom
axiomDeclaration x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Axiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "declaration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

axiomHasKey :: Phantoms.TTerm Syntax.HasKey -> Phantoms.TTerm Syntax.Axiom
axiomHasKey x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Axiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hasKey"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

axiomObjectPropertyAxiom :: Phantoms.TTerm Syntax.ObjectPropertyAxiom -> Phantoms.TTerm Syntax.Axiom
axiomObjectPropertyAxiom x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Axiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectPropertyAxiom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

class_ :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.Class
class_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Class"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

classAssertion :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm Syntax.Individual -> Phantoms.TTerm Syntax.ClassAssertion
classAssertion annotations class_ individual =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassAssertion"),
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

classAssertionAnnotations :: Phantoms.TTerm Syntax.ClassAssertion -> Phantoms.TTerm [Syntax.Annotation]
classAssertionAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassAssertion"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classAssertionClass :: Phantoms.TTerm Syntax.ClassAssertion -> Phantoms.TTerm Syntax.ClassExpression
classAssertionClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassAssertion"),
        Core.projectionField = (Core.Name "class")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classAssertionIndividual :: Phantoms.TTerm Syntax.ClassAssertion -> Phantoms.TTerm Syntax.Individual
classAssertionIndividual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassAssertion"),
        Core.projectionField = (Core.Name "individual")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classAssertionWithAnnotations :: Phantoms.TTerm Syntax.ClassAssertion -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ClassAssertion
classAssertionWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassAssertion"),
              Core.projectionField = (Core.Name "class")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "individual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassAssertion"),
              Core.projectionField = (Core.Name "individual")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

classAssertionWithClass :: Phantoms.TTerm Syntax.ClassAssertion -> Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm Syntax.ClassAssertion
classAssertionWithClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassAssertion"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "individual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassAssertion"),
              Core.projectionField = (Core.Name "individual")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

classAssertionWithIndividual :: Phantoms.TTerm Syntax.ClassAssertion -> Phantoms.TTerm Syntax.Individual -> Phantoms.TTerm Syntax.ClassAssertion
classAssertionWithIndividual original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassAssertion"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassAssertion"),
              Core.projectionField = (Core.Name "class")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "individual"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

classAxiomDisjointClasses :: Phantoms.TTerm Syntax.DisjointClasses -> Phantoms.TTerm Syntax.ClassAxiom
classAxiomDisjointClasses x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "disjointClasses"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classAxiomDisjointUnion :: Phantoms.TTerm Syntax.DisjointUnion -> Phantoms.TTerm Syntax.ClassAxiom
classAxiomDisjointUnion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "disjointUnion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classAxiomEquivalentClasses :: Phantoms.TTerm Syntax.EquivalentClasses -> Phantoms.TTerm Syntax.ClassAxiom
classAxiomEquivalentClasses x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equivalentClasses"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classAxiomSubClassOf :: Phantoms.TTerm Syntax.SubClassOf -> Phantoms.TTerm Syntax.ClassAxiom
classAxiomSubClassOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subClassOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionClass :: Phantoms.TTerm Syntax.Class -> Phantoms.TTerm Syntax.ClassExpression
classExpressionClass x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionDataAllValuesFrom :: Phantoms.TTerm Syntax.DataAllValuesFrom -> Phantoms.TTerm Syntax.ClassExpression
classExpressionDataAllValuesFrom x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataAllValuesFrom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionDataExactCardinality :: Phantoms.TTerm Syntax.DataExactCardinality -> Phantoms.TTerm Syntax.ClassExpression
classExpressionDataExactCardinality x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataExactCardinality"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionDataHasValue :: Phantoms.TTerm Syntax.DataHasValue -> Phantoms.TTerm Syntax.ClassExpression
classExpressionDataHasValue x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataHasValue"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionDataMaxCardinality :: Phantoms.TTerm Syntax.DataMaxCardinality -> Phantoms.TTerm Syntax.ClassExpression
classExpressionDataMaxCardinality x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataMaxCardinality"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionDataMinCardinality :: Phantoms.TTerm Syntax.DataMinCardinality -> Phantoms.TTerm Syntax.ClassExpression
classExpressionDataMinCardinality x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataMinCardinality"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionDataSomeValuesFrom :: Phantoms.TTerm Syntax.DataSomeValuesFrom -> Phantoms.TTerm Syntax.ClassExpression
classExpressionDataSomeValuesFrom x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataSomeValuesFrom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionObjectAllValuesFrom :: Phantoms.TTerm Syntax.ObjectAllValuesFrom -> Phantoms.TTerm Syntax.ClassExpression
classExpressionObjectAllValuesFrom x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectAllValuesFrom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionObjectExactCardinality :: Phantoms.TTerm Syntax.ObjectExactCardinality -> Phantoms.TTerm Syntax.ClassExpression
classExpressionObjectExactCardinality x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectExactCardinality"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionObjectHasSelf :: Phantoms.TTerm Syntax.ObjectHasSelf -> Phantoms.TTerm Syntax.ClassExpression
classExpressionObjectHasSelf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectHasSelf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionObjectHasValue :: Phantoms.TTerm Syntax.ObjectHasValue -> Phantoms.TTerm Syntax.ClassExpression
classExpressionObjectHasValue x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectHasValue"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionObjectIntersectionOf :: Phantoms.TTerm Syntax.ObjectIntersectionOf -> Phantoms.TTerm Syntax.ClassExpression
classExpressionObjectIntersectionOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectIntersectionOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionObjectMaxCardinality :: Phantoms.TTerm Syntax.ObjectMaxCardinality -> Phantoms.TTerm Syntax.ClassExpression
classExpressionObjectMaxCardinality x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectMaxCardinality"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionObjectMinCardinality :: Phantoms.TTerm Syntax.ObjectMinCardinality -> Phantoms.TTerm Syntax.ClassExpression
classExpressionObjectMinCardinality x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectMinCardinality"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionObjectOneOf :: Phantoms.TTerm Syntax.ObjectOneOf -> Phantoms.TTerm Syntax.ClassExpression
classExpressionObjectOneOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectOneOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionObjectSomeValuesFrom :: Phantoms.TTerm Syntax.ObjectSomeValuesFrom -> Phantoms.TTerm Syntax.ClassExpression
classExpressionObjectSomeValuesFrom x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectSomeValuesFrom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

classExpressionObjectUnionOf :: Phantoms.TTerm Syntax.ObjectUnionOf -> Phantoms.TTerm Syntax.ClassExpression
classExpressionObjectUnionOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ClassExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectUnionOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataAllValuesFrom :: Phantoms.TTerm [Syntax.DataPropertyExpression] -> Phantoms.TTerm Syntax.DataRange -> Phantoms.TTerm Syntax.DataAllValuesFrom
dataAllValuesFrom property range =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataAllValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm range)}]}))

dataAllValuesFromProperty :: Phantoms.TTerm Syntax.DataAllValuesFrom -> Phantoms.TTerm [Syntax.DataPropertyExpression]
dataAllValuesFromProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataAllValuesFrom"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataAllValuesFromRange :: Phantoms.TTerm Syntax.DataAllValuesFrom -> Phantoms.TTerm Syntax.DataRange
dataAllValuesFromRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataAllValuesFrom"),
        Core.projectionField = (Core.Name "range")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataAllValuesFromWithProperty :: Phantoms.TTerm Syntax.DataAllValuesFrom -> Phantoms.TTerm [Syntax.DataPropertyExpression] -> Phantoms.TTerm Syntax.DataAllValuesFrom
dataAllValuesFromWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataAllValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataAllValuesFrom"),
              Core.projectionField = (Core.Name "range")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataAllValuesFromWithRange :: Phantoms.TTerm Syntax.DataAllValuesFrom -> Phantoms.TTerm Syntax.DataRange -> Phantoms.TTerm Syntax.DataAllValuesFrom
dataAllValuesFromWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataAllValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataAllValuesFrom"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataComplementOf :: Phantoms.TTerm Syntax.DataRange -> Phantoms.TTerm Syntax.DataComplementOf
dataComplementOf x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataComplementOf"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

dataExactCardinality :: Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm [Syntax.DataRange] -> Phantoms.TTerm Syntax.DataExactCardinality
dataExactCardinality bound property range =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataExactCardinality"),
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

dataExactCardinalityBound :: Phantoms.TTerm Syntax.DataExactCardinality -> Phantoms.TTerm Integer
dataExactCardinalityBound x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataExactCardinality"),
        Core.projectionField = (Core.Name "bound")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataExactCardinalityProperty :: Phantoms.TTerm Syntax.DataExactCardinality -> Phantoms.TTerm Syntax.DataPropertyExpression
dataExactCardinalityProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataExactCardinality"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataExactCardinalityRange :: Phantoms.TTerm Syntax.DataExactCardinality -> Phantoms.TTerm [Syntax.DataRange]
dataExactCardinalityRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataExactCardinality"),
        Core.projectionField = (Core.Name "range")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataExactCardinalityWithBound :: Phantoms.TTerm Syntax.DataExactCardinality -> Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.DataExactCardinality
dataExactCardinalityWithBound original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataExactCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataExactCardinality"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataExactCardinality"),
              Core.projectionField = (Core.Name "range")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataExactCardinalityWithProperty :: Phantoms.TTerm Syntax.DataExactCardinality -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm Syntax.DataExactCardinality
dataExactCardinalityWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataExactCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataExactCardinality"),
              Core.projectionField = (Core.Name "bound")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataExactCardinality"),
              Core.projectionField = (Core.Name "range")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataExactCardinalityWithRange :: Phantoms.TTerm Syntax.DataExactCardinality -> Phantoms.TTerm [Syntax.DataRange] -> Phantoms.TTerm Syntax.DataExactCardinality
dataExactCardinalityWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataExactCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataExactCardinality"),
              Core.projectionField = (Core.Name "bound")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataExactCardinality"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataHasValue :: Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm Syntax_.Literal -> Phantoms.TTerm Syntax.DataHasValue
dataHasValue property value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataHasValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

dataHasValueProperty :: Phantoms.TTerm Syntax.DataHasValue -> Phantoms.TTerm Syntax.DataPropertyExpression
dataHasValueProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataHasValue"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataHasValueValue :: Phantoms.TTerm Syntax.DataHasValue -> Phantoms.TTerm Syntax_.Literal
dataHasValueValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataHasValue"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataHasValueWithProperty :: Phantoms.TTerm Syntax.DataHasValue -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm Syntax.DataHasValue
dataHasValueWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataHasValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataHasValue"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataHasValueWithValue :: Phantoms.TTerm Syntax.DataHasValue -> Phantoms.TTerm Syntax_.Literal -> Phantoms.TTerm Syntax.DataHasValue
dataHasValueWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataHasValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataHasValue"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataIntersectionOf :: Phantoms.TTerm [Syntax.DataRange] -> Phantoms.TTerm Syntax.DataIntersectionOf
dataIntersectionOf x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataIntersectionOf"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

dataMaxCardinality :: Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm [Syntax.DataRange] -> Phantoms.TTerm Syntax.DataMaxCardinality
dataMaxCardinality bound property range =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMaxCardinality"),
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

dataMaxCardinalityBound :: Phantoms.TTerm Syntax.DataMaxCardinality -> Phantoms.TTerm Integer
dataMaxCardinalityBound x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMaxCardinality"),
        Core.projectionField = (Core.Name "bound")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataMaxCardinalityProperty :: Phantoms.TTerm Syntax.DataMaxCardinality -> Phantoms.TTerm Syntax.DataPropertyExpression
dataMaxCardinalityProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMaxCardinality"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataMaxCardinalityRange :: Phantoms.TTerm Syntax.DataMaxCardinality -> Phantoms.TTerm [Syntax.DataRange]
dataMaxCardinalityRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMaxCardinality"),
        Core.projectionField = (Core.Name "range")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataMaxCardinalityWithBound :: Phantoms.TTerm Syntax.DataMaxCardinality -> Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.DataMaxCardinality
dataMaxCardinalityWithBound original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMaxCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMaxCardinality"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMaxCardinality"),
              Core.projectionField = (Core.Name "range")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataMaxCardinalityWithProperty :: Phantoms.TTerm Syntax.DataMaxCardinality -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm Syntax.DataMaxCardinality
dataMaxCardinalityWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMaxCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMaxCardinality"),
              Core.projectionField = (Core.Name "bound")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMaxCardinality"),
              Core.projectionField = (Core.Name "range")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataMaxCardinalityWithRange :: Phantoms.TTerm Syntax.DataMaxCardinality -> Phantoms.TTerm [Syntax.DataRange] -> Phantoms.TTerm Syntax.DataMaxCardinality
dataMaxCardinalityWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMaxCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMaxCardinality"),
              Core.projectionField = (Core.Name "bound")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMaxCardinality"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataMinCardinality :: Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm [Syntax.DataRange] -> Phantoms.TTerm Syntax.DataMinCardinality
dataMinCardinality bound property range =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMinCardinality"),
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

dataMinCardinalityBound :: Phantoms.TTerm Syntax.DataMinCardinality -> Phantoms.TTerm Integer
dataMinCardinalityBound x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMinCardinality"),
        Core.projectionField = (Core.Name "bound")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataMinCardinalityProperty :: Phantoms.TTerm Syntax.DataMinCardinality -> Phantoms.TTerm Syntax.DataPropertyExpression
dataMinCardinalityProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMinCardinality"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataMinCardinalityRange :: Phantoms.TTerm Syntax.DataMinCardinality -> Phantoms.TTerm [Syntax.DataRange]
dataMinCardinalityRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMinCardinality"),
        Core.projectionField = (Core.Name "range")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataMinCardinalityWithBound :: Phantoms.TTerm Syntax.DataMinCardinality -> Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.DataMinCardinality
dataMinCardinalityWithBound original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMinCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMinCardinality"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMinCardinality"),
              Core.projectionField = (Core.Name "range")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataMinCardinalityWithProperty :: Phantoms.TTerm Syntax.DataMinCardinality -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm Syntax.DataMinCardinality
dataMinCardinalityWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMinCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMinCardinality"),
              Core.projectionField = (Core.Name "bound")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMinCardinality"),
              Core.projectionField = (Core.Name "range")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataMinCardinalityWithRange :: Phantoms.TTerm Syntax.DataMinCardinality -> Phantoms.TTerm [Syntax.DataRange] -> Phantoms.TTerm Syntax.DataMinCardinality
dataMinCardinalityWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMinCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMinCardinality"),
              Core.projectionField = (Core.Name "bound")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataMinCardinality"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataOneOf :: Phantoms.TTerm [Syntax_.Literal] -> Phantoms.TTerm Syntax.DataOneOf
dataOneOf x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataOneOf"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

dataProperty :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.DataProperty
dataProperty x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataProperty"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

dataPropertyAssertion :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm Syntax.Individual -> Phantoms.TTerm Syntax.Individual -> Phantoms.TTerm Syntax.DataPropertyAssertion
dataPropertyAssertion annotations property source target =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
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

dataPropertyAssertionAnnotations :: Phantoms.TTerm Syntax.DataPropertyAssertion -> Phantoms.TTerm [Syntax.Annotation]
dataPropertyAssertionAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPropertyAssertionProperty :: Phantoms.TTerm Syntax.DataPropertyAssertion -> Phantoms.TTerm Syntax.DataPropertyExpression
dataPropertyAssertionProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPropertyAssertionSource :: Phantoms.TTerm Syntax.DataPropertyAssertion -> Phantoms.TTerm Syntax.Individual
dataPropertyAssertionSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
        Core.projectionField = (Core.Name "source")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPropertyAssertionTarget :: Phantoms.TTerm Syntax.DataPropertyAssertion -> Phantoms.TTerm Syntax.Individual
dataPropertyAssertionTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
        Core.projectionField = (Core.Name "target")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPropertyAssertionWithAnnotations :: Phantoms.TTerm Syntax.DataPropertyAssertion -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.DataPropertyAssertion
dataPropertyAssertionWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPropertyAssertionWithProperty :: Phantoms.TTerm Syntax.DataPropertyAssertion -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm Syntax.DataPropertyAssertion
dataPropertyAssertionWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPropertyAssertionWithSource :: Phantoms.TTerm Syntax.DataPropertyAssertion -> Phantoms.TTerm Syntax.Individual -> Phantoms.TTerm Syntax.DataPropertyAssertion
dataPropertyAssertionWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPropertyAssertionWithTarget :: Phantoms.TTerm Syntax.DataPropertyAssertion -> Phantoms.TTerm Syntax.Individual -> Phantoms.TTerm Syntax.DataPropertyAssertion
dataPropertyAssertionWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataPropertyAxiomDataPropertyAxiom :: Phantoms.TTerm Syntax.DataPropertyAxiom -> Phantoms.TTerm Syntax.DataPropertyAxiom
dataPropertyAxiomDataPropertyAxiom x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataPropertyAxiom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataPropertyAxiomDataPropertyRange :: Phantoms.TTerm Syntax.DataPropertyRange -> Phantoms.TTerm Syntax.DataPropertyAxiom
dataPropertyAxiomDataPropertyRange x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataPropertyRange"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataPropertyAxiomDisjointDataProperties :: Phantoms.TTerm Syntax.DisjointDataProperties -> Phantoms.TTerm Syntax.DataPropertyAxiom
dataPropertyAxiomDisjointDataProperties x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "disjointDataProperties"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataPropertyAxiomEquivalentDataProperties :: Phantoms.TTerm Syntax.EquivalentDataProperties -> Phantoms.TTerm Syntax.DataPropertyAxiom
dataPropertyAxiomEquivalentDataProperties x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equivalentDataProperties"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataPropertyAxiomFunctionalDataProperty :: Phantoms.TTerm Syntax.FunctionalDataProperty -> Phantoms.TTerm Syntax.DataPropertyAxiom
dataPropertyAxiomFunctionalDataProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "functionalDataProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataPropertyAxiomSubDataPropertyOf :: Phantoms.TTerm Syntax.SubDataPropertyOf -> Phantoms.TTerm Syntax.DataPropertyAxiom
dataPropertyAxiomSubDataPropertyOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subDataPropertyOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataPropertyDomain :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm Syntax.DataPropertyDomain
dataPropertyDomain annotations property domain =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyDomain"),
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

dataPropertyDomainAnnotations :: Phantoms.TTerm Syntax.DataPropertyDomain -> Phantoms.TTerm [Syntax.Annotation]
dataPropertyDomainAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyDomain"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPropertyDomainDomain :: Phantoms.TTerm Syntax.DataPropertyDomain -> Phantoms.TTerm Syntax.ClassExpression
dataPropertyDomainDomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyDomain"),
        Core.projectionField = (Core.Name "domain")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPropertyDomainProperty :: Phantoms.TTerm Syntax.DataPropertyDomain -> Phantoms.TTerm Syntax.DataPropertyExpression
dataPropertyDomainProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyDomain"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPropertyDomainWithAnnotations :: Phantoms.TTerm Syntax.DataPropertyDomain -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.DataPropertyDomain
dataPropertyDomainWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyDomain"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyDomain"),
              Core.projectionField = (Core.Name "domain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPropertyDomainWithDomain :: Phantoms.TTerm Syntax.DataPropertyDomain -> Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm Syntax.DataPropertyDomain
dataPropertyDomainWithDomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyDomain"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyDomain"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataPropertyDomainWithProperty :: Phantoms.TTerm Syntax.DataPropertyDomain -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm Syntax.DataPropertyDomain
dataPropertyDomainWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyDomain"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyDomain"),
              Core.projectionField = (Core.Name "domain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPropertyExpression :: Phantoms.TTerm Syntax.DataProperty -> Phantoms.TTerm Syntax.DataPropertyExpression
dataPropertyExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

dataPropertyRange :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm Syntax.DataPropertyRange
dataPropertyRange annotations property range =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyRange"),
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

dataPropertyRangeAnnotations :: Phantoms.TTerm Syntax.DataPropertyRange -> Phantoms.TTerm [Syntax.Annotation]
dataPropertyRangeAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyRange"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPropertyRangeProperty :: Phantoms.TTerm Syntax.DataPropertyRange -> Phantoms.TTerm Syntax.DataPropertyExpression
dataPropertyRangeProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyRange"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPropertyRangeRange :: Phantoms.TTerm Syntax.DataPropertyRange -> Phantoms.TTerm Syntax.ClassExpression
dataPropertyRangeRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyRange"),
        Core.projectionField = (Core.Name "range")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPropertyRangeWithAnnotations :: Phantoms.TTerm Syntax.DataPropertyRange -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.DataPropertyRange
dataPropertyRangeWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyRange"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyRange"),
              Core.projectionField = (Core.Name "range")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPropertyRangeWithProperty :: Phantoms.TTerm Syntax.DataPropertyRange -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm Syntax.DataPropertyRange
dataPropertyRangeWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyRange"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyRange"),
              Core.projectionField = (Core.Name "range")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPropertyRangeWithRange :: Phantoms.TTerm Syntax.DataPropertyRange -> Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm Syntax.DataPropertyRange
dataPropertyRangeWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyRange"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyRange"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataRangeDataComplementOf :: Phantoms.TTerm Syntax.DataComplementOf -> Phantoms.TTerm Syntax.DataRange
dataRangeDataComplementOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataComplementOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataRangeDataIntersectionOf :: Phantoms.TTerm Syntax.DataIntersectionOf -> Phantoms.TTerm Syntax.DataRange
dataRangeDataIntersectionOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataIntersectionOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataRangeDataOneOf :: Phantoms.TTerm Syntax.DataOneOf -> Phantoms.TTerm Syntax.DataRange
dataRangeDataOneOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataOneOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataRangeDataUnionOf :: Phantoms.TTerm Syntax.DataUnionOf -> Phantoms.TTerm Syntax.DataRange
dataRangeDataUnionOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataUnionOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataRangeDatatype :: Phantoms.TTerm Syntax.Datatype -> Phantoms.TTerm Syntax.DataRange
dataRangeDatatype x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "datatype"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataRangeDatatypeRestriction :: Phantoms.TTerm Syntax.DatatypeRestriction -> Phantoms.TTerm Syntax.DataRange
dataRangeDatatypeRestriction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "datatypeRestriction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataSomeValuesFrom :: Phantoms.TTerm [Syntax.DataPropertyExpression] -> Phantoms.TTerm Syntax.DataRange -> Phantoms.TTerm Syntax.DataSomeValuesFrom
dataSomeValuesFrom property range =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataSomeValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm range)}]}))

dataSomeValuesFromProperty :: Phantoms.TTerm Syntax.DataSomeValuesFrom -> Phantoms.TTerm [Syntax.DataPropertyExpression]
dataSomeValuesFromProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataSomeValuesFrom"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataSomeValuesFromRange :: Phantoms.TTerm Syntax.DataSomeValuesFrom -> Phantoms.TTerm Syntax.DataRange
dataSomeValuesFromRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataSomeValuesFrom"),
        Core.projectionField = (Core.Name "range")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataSomeValuesFromWithProperty :: Phantoms.TTerm Syntax.DataSomeValuesFrom -> Phantoms.TTerm [Syntax.DataPropertyExpression] -> Phantoms.TTerm Syntax.DataSomeValuesFrom
dataSomeValuesFromWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataSomeValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataSomeValuesFrom"),
              Core.projectionField = (Core.Name "range")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataSomeValuesFromWithRange :: Phantoms.TTerm Syntax.DataSomeValuesFrom -> Phantoms.TTerm Syntax.DataRange -> Phantoms.TTerm Syntax.DataSomeValuesFrom
dataSomeValuesFromWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataSomeValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataSomeValuesFrom"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataUnionOf :: Phantoms.TTerm [Syntax.DataRange] -> Phantoms.TTerm Syntax.DataUnionOf
dataUnionOf x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DataUnionOf"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

datatypeDefinition :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.Datatype -> Phantoms.TTerm Syntax.DataRange -> Phantoms.TTerm Syntax.DatatypeDefinition
datatypeDefinition annotations datatype range =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeDefinition"),
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

datatypeDefinitionAnnotations :: Phantoms.TTerm Syntax.DatatypeDefinition -> Phantoms.TTerm [Syntax.Annotation]
datatypeDefinitionAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeDefinition"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

datatypeDefinitionDatatype :: Phantoms.TTerm Syntax.DatatypeDefinition -> Phantoms.TTerm Syntax.Datatype
datatypeDefinitionDatatype x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeDefinition"),
        Core.projectionField = (Core.Name "datatype")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

datatypeDefinitionRange :: Phantoms.TTerm Syntax.DatatypeDefinition -> Phantoms.TTerm Syntax.DataRange
datatypeDefinitionRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeDefinition"),
        Core.projectionField = (Core.Name "range")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

datatypeDefinitionWithAnnotations :: Phantoms.TTerm Syntax.DatatypeDefinition -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.DatatypeDefinition
datatypeDefinitionWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "datatype"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeDefinition"),
              Core.projectionField = (Core.Name "datatype")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeDefinition"),
              Core.projectionField = (Core.Name "range")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

datatypeDefinitionWithDatatype :: Phantoms.TTerm Syntax.DatatypeDefinition -> Phantoms.TTerm Syntax.Datatype -> Phantoms.TTerm Syntax.DatatypeDefinition
datatypeDefinitionWithDatatype original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeDefinition"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "datatype"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeDefinition"),
              Core.projectionField = (Core.Name "range")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

datatypeDefinitionWithRange :: Phantoms.TTerm Syntax.DatatypeDefinition -> Phantoms.TTerm Syntax.DataRange -> Phantoms.TTerm Syntax.DatatypeDefinition
datatypeDefinitionWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeDefinition"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "datatype"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeDefinition"),
              Core.projectionField = (Core.Name "datatype")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

datatypeOther :: Phantoms.TTerm Syntax_.Iri -> Phantoms.TTerm Syntax.Datatype
datatypeOther x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

datatypeRestriction :: Phantoms.TTerm Syntax.Datatype -> Phantoms.TTerm [Syntax.DatatypeRestriction_Constraint] -> Phantoms.TTerm Syntax.DatatypeRestriction
datatypeRestriction datatype constraints =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeRestriction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "datatype"),
          Core.fieldTerm = (Phantoms.unTTerm datatype)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTTerm constraints)}]}))

datatypeRestrictionConstraints :: Phantoms.TTerm Syntax.DatatypeRestriction -> Phantoms.TTerm [Syntax.DatatypeRestriction_Constraint]
datatypeRestrictionConstraints x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeRestriction"),
        Core.projectionField = (Core.Name "constraints")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

datatypeRestrictionDatatype :: Phantoms.TTerm Syntax.DatatypeRestriction -> Phantoms.TTerm Syntax.Datatype
datatypeRestrictionDatatype x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeRestriction"),
        Core.projectionField = (Core.Name "datatype")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

datatypeRestrictionWithConstraints :: Phantoms.TTerm Syntax.DatatypeRestriction -> Phantoms.TTerm [Syntax.DatatypeRestriction_Constraint] -> Phantoms.TTerm Syntax.DatatypeRestriction
datatypeRestrictionWithConstraints original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeRestriction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "datatype"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeRestriction"),
              Core.projectionField = (Core.Name "datatype")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

datatypeRestrictionWithDatatype :: Phantoms.TTerm Syntax.DatatypeRestriction -> Phantoms.TTerm Syntax.Datatype -> Phantoms.TTerm Syntax.DatatypeRestriction
datatypeRestrictionWithDatatype original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeRestriction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "datatype"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeRestriction"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

datatypeRestriction_ConstrainingFacetOther :: Phantoms.TTerm Syntax_.Iri -> Phantoms.TTerm Syntax.DatatypeRestriction_ConstrainingFacet
datatypeRestriction_ConstrainingFacetOther x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeRestriction_ConstrainingFacet"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

datatypeRestriction_ConstrainingFacetXmlSchema :: Phantoms.TTerm Schema.ConstrainingFacet -> Phantoms.TTerm Syntax.DatatypeRestriction_ConstrainingFacet
datatypeRestriction_ConstrainingFacetXmlSchema x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeRestriction_ConstrainingFacet"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "xmlSchema"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

datatypeRestriction_Constraint :: Phantoms.TTerm Syntax.DatatypeRestriction_ConstrainingFacet -> Phantoms.TTerm Syntax_.Literal -> Phantoms.TTerm Syntax.DatatypeRestriction_Constraint
datatypeRestriction_Constraint constrainingFacet restrictionValue =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeRestriction_Constraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constrainingFacet"),
          Core.fieldTerm = (Phantoms.unTTerm constrainingFacet)},
        Core.Field {
          Core.fieldName = (Core.Name "restrictionValue"),
          Core.fieldTerm = (Phantoms.unTTerm restrictionValue)}]}))

datatypeRestriction_ConstraintConstrainingFacet :: Phantoms.TTerm Syntax.DatatypeRestriction_Constraint -> Phantoms.TTerm Syntax.DatatypeRestriction_ConstrainingFacet
datatypeRestriction_ConstraintConstrainingFacet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeRestriction_Constraint"),
        Core.projectionField = (Core.Name "constrainingFacet")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

datatypeRestriction_ConstraintRestrictionValue :: Phantoms.TTerm Syntax.DatatypeRestriction_Constraint -> Phantoms.TTerm Syntax_.Literal
datatypeRestriction_ConstraintRestrictionValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeRestriction_Constraint"),
        Core.projectionField = (Core.Name "restrictionValue")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

datatypeRestriction_ConstraintWithConstrainingFacet :: Phantoms.TTerm Syntax.DatatypeRestriction_Constraint -> Phantoms.TTerm Syntax.DatatypeRestriction_ConstrainingFacet -> Phantoms.TTerm Syntax.DatatypeRestriction_Constraint
datatypeRestriction_ConstraintWithConstrainingFacet original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeRestriction_Constraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constrainingFacet"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "restrictionValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeRestriction_Constraint"),
              Core.projectionField = (Core.Name "restrictionValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

datatypeRestriction_ConstraintWithRestrictionValue :: Phantoms.TTerm Syntax.DatatypeRestriction_Constraint -> Phantoms.TTerm Syntax_.Literal -> Phantoms.TTerm Syntax.DatatypeRestriction_Constraint
datatypeRestriction_ConstraintWithRestrictionValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeRestriction_Constraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constrainingFacet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DatatypeRestriction_Constraint"),
              Core.projectionField = (Core.Name "constrainingFacet")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restrictionValue"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

datatypeXmlSchema :: Phantoms.TTerm Schema.Datatype -> Phantoms.TTerm Syntax.Datatype
datatypeXmlSchema x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "xmlSchema"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declaration :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.Entity -> Phantoms.TTerm Syntax.Declaration
declaration annotations entity =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Declaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "entity"),
          Core.fieldTerm = (Phantoms.unTTerm entity)}]}))

declarationAnnotations :: Phantoms.TTerm Syntax.Declaration -> Phantoms.TTerm [Syntax.Annotation]
declarationAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Declaration"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

declarationEntity :: Phantoms.TTerm Syntax.Declaration -> Phantoms.TTerm Syntax.Entity
declarationEntity x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Declaration"),
        Core.projectionField = (Core.Name "entity")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

declarationWithAnnotations :: Phantoms.TTerm Syntax.Declaration -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.Declaration
declarationWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Declaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "entity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Declaration"),
              Core.projectionField = (Core.Name "entity")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

declarationWithEntity :: Phantoms.TTerm Syntax.Declaration -> Phantoms.TTerm Syntax.Entity -> Phantoms.TTerm Syntax.Declaration
declarationWithEntity original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Declaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Declaration"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "entity"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

differentIndividuals :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm [Syntax.Individual] -> Phantoms.TTerm Syntax.DifferentIndividuals
differentIndividuals annotations individuals =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DifferentIndividuals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "individuals"),
          Core.fieldTerm = (Phantoms.unTTerm individuals)}]}))

differentIndividualsAnnotations :: Phantoms.TTerm Syntax.DifferentIndividuals -> Phantoms.TTerm [Syntax.Annotation]
differentIndividualsAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DifferentIndividuals"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

differentIndividualsIndividuals :: Phantoms.TTerm Syntax.DifferentIndividuals -> Phantoms.TTerm [Syntax.Individual]
differentIndividualsIndividuals x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DifferentIndividuals"),
        Core.projectionField = (Core.Name "individuals")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

differentIndividualsWithAnnotations :: Phantoms.TTerm Syntax.DifferentIndividuals -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.DifferentIndividuals
differentIndividualsWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DifferentIndividuals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "individuals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DifferentIndividuals"),
              Core.projectionField = (Core.Name "individuals")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

differentIndividualsWithIndividuals :: Phantoms.TTerm Syntax.DifferentIndividuals -> Phantoms.TTerm [Syntax.Individual] -> Phantoms.TTerm Syntax.DifferentIndividuals
differentIndividualsWithIndividuals original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DifferentIndividuals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DifferentIndividuals"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "individuals"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

disjointClasses :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm [Syntax.ClassExpression] -> Phantoms.TTerm Syntax.DisjointClasses
disjointClasses annotations classes =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointClasses"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Phantoms.unTTerm classes)}]}))

disjointClassesAnnotations :: Phantoms.TTerm Syntax.DisjointClasses -> Phantoms.TTerm [Syntax.Annotation]
disjointClassesAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointClasses"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

disjointClassesClasses :: Phantoms.TTerm Syntax.DisjointClasses -> Phantoms.TTerm [Syntax.ClassExpression]
disjointClassesClasses x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointClasses"),
        Core.projectionField = (Core.Name "classes")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

disjointClassesWithAnnotations :: Phantoms.TTerm Syntax.DisjointClasses -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.DisjointClasses
disjointClassesWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointClasses"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointClasses"),
              Core.projectionField = (Core.Name "classes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

disjointClassesWithClasses :: Phantoms.TTerm Syntax.DisjointClasses -> Phantoms.TTerm [Syntax.ClassExpression] -> Phantoms.TTerm Syntax.DisjointClasses
disjointClassesWithClasses original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointClasses"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointClasses"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

disjointDataProperties :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm [Syntax.DataPropertyExpression] -> Phantoms.TTerm Syntax.DisjointDataProperties
disjointDataProperties annotations properties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointDataProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)}]}))

disjointDataPropertiesAnnotations :: Phantoms.TTerm Syntax.DisjointDataProperties -> Phantoms.TTerm [Syntax.Annotation]
disjointDataPropertiesAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointDataProperties"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

disjointDataPropertiesProperties :: Phantoms.TTerm Syntax.DisjointDataProperties -> Phantoms.TTerm [Syntax.DataPropertyExpression]
disjointDataPropertiesProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointDataProperties"),
        Core.projectionField = (Core.Name "properties")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

disjointDataPropertiesWithAnnotations :: Phantoms.TTerm Syntax.DisjointDataProperties -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.DisjointDataProperties
disjointDataPropertiesWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointDataProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointDataProperties"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

disjointDataPropertiesWithProperties :: Phantoms.TTerm Syntax.DisjointDataProperties -> Phantoms.TTerm [Syntax.DataPropertyExpression] -> Phantoms.TTerm Syntax.DisjointDataProperties
disjointDataPropertiesWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointDataProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointDataProperties"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

disjointObjectProperties :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm [Syntax.ObjectPropertyExpression] -> Phantoms.TTerm Syntax.DisjointObjectProperties
disjointObjectProperties annotations properties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointObjectProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)}]}))

disjointObjectPropertiesAnnotations :: Phantoms.TTerm Syntax.DisjointObjectProperties -> Phantoms.TTerm [Syntax.Annotation]
disjointObjectPropertiesAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointObjectProperties"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

disjointObjectPropertiesProperties :: Phantoms.TTerm Syntax.DisjointObjectProperties -> Phantoms.TTerm [Syntax.ObjectPropertyExpression]
disjointObjectPropertiesProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointObjectProperties"),
        Core.projectionField = (Core.Name "properties")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

disjointObjectPropertiesWithAnnotations :: Phantoms.TTerm Syntax.DisjointObjectProperties -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.DisjointObjectProperties
disjointObjectPropertiesWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointObjectProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointObjectProperties"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

disjointObjectPropertiesWithProperties :: Phantoms.TTerm Syntax.DisjointObjectProperties -> Phantoms.TTerm [Syntax.ObjectPropertyExpression] -> Phantoms.TTerm Syntax.DisjointObjectProperties
disjointObjectPropertiesWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointObjectProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointObjectProperties"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

disjointUnion :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.Class -> Phantoms.TTerm [Syntax.ClassExpression] -> Phantoms.TTerm Syntax.DisjointUnion
disjointUnion annotations class_ classes =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointUnion"),
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

disjointUnionAnnotations :: Phantoms.TTerm Syntax.DisjointUnion -> Phantoms.TTerm [Syntax.Annotation]
disjointUnionAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointUnion"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

disjointUnionClass :: Phantoms.TTerm Syntax.DisjointUnion -> Phantoms.TTerm Syntax.Class
disjointUnionClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointUnion"),
        Core.projectionField = (Core.Name "class")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

disjointUnionClasses :: Phantoms.TTerm Syntax.DisjointUnion -> Phantoms.TTerm [Syntax.ClassExpression]
disjointUnionClasses x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointUnion"),
        Core.projectionField = (Core.Name "classes")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

disjointUnionWithAnnotations :: Phantoms.TTerm Syntax.DisjointUnion -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.DisjointUnion
disjointUnionWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointUnion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointUnion"),
              Core.projectionField = (Core.Name "class")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointUnion"),
              Core.projectionField = (Core.Name "classes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

disjointUnionWithClass :: Phantoms.TTerm Syntax.DisjointUnion -> Phantoms.TTerm Syntax.Class -> Phantoms.TTerm Syntax.DisjointUnion
disjointUnionWithClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointUnion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointUnion"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointUnion"),
              Core.projectionField = (Core.Name "classes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

disjointUnionWithClasses :: Phantoms.TTerm Syntax.DisjointUnion -> Phantoms.TTerm [Syntax.ClassExpression] -> Phantoms.TTerm Syntax.DisjointUnion
disjointUnionWithClasses original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointUnion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointUnion"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.DisjointUnion"),
              Core.projectionField = (Core.Name "class")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

entityAnnotationProperty :: Phantoms.TTerm Syntax.AnnotationProperty -> Phantoms.TTerm Syntax.Entity
entityAnnotationProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Entity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotationProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

entityClass :: Phantoms.TTerm Syntax.Class -> Phantoms.TTerm Syntax.Entity
entityClass x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Entity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

entityDataProperty :: Phantoms.TTerm Syntax.DataProperty -> Phantoms.TTerm Syntax.Entity
entityDataProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Entity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

entityDatatype :: Phantoms.TTerm Syntax.Datatype -> Phantoms.TTerm Syntax.Entity
entityDatatype x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Entity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "datatype"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

entityNamedIndividual :: Phantoms.TTerm Syntax.NamedIndividual -> Phantoms.TTerm Syntax.Entity
entityNamedIndividual x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Entity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "namedIndividual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

entityObjectProperty :: Phantoms.TTerm Syntax.ObjectProperty -> Phantoms.TTerm Syntax.Entity
entityObjectProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Entity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

equivalentClasses :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm [Syntax.ClassExpression] -> Phantoms.TTerm Syntax.EquivalentClasses
equivalentClasses annotations classes =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentClasses"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Phantoms.unTTerm classes)}]}))

equivalentClassesAnnotations :: Phantoms.TTerm Syntax.EquivalentClasses -> Phantoms.TTerm [Syntax.Annotation]
equivalentClassesAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentClasses"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equivalentClassesClasses :: Phantoms.TTerm Syntax.EquivalentClasses -> Phantoms.TTerm [Syntax.ClassExpression]
equivalentClassesClasses x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentClasses"),
        Core.projectionField = (Core.Name "classes")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equivalentClassesWithAnnotations :: Phantoms.TTerm Syntax.EquivalentClasses -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.EquivalentClasses
equivalentClassesWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentClasses"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentClasses"),
              Core.projectionField = (Core.Name "classes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

equivalentClassesWithClasses :: Phantoms.TTerm Syntax.EquivalentClasses -> Phantoms.TTerm [Syntax.ClassExpression] -> Phantoms.TTerm Syntax.EquivalentClasses
equivalentClassesWithClasses original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentClasses"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentClasses"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

equivalentDataProperties :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm [Syntax.DataPropertyExpression] -> Phantoms.TTerm Syntax.EquivalentDataProperties
equivalentDataProperties annotations properties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentDataProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)}]}))

equivalentDataPropertiesAnnotations :: Phantoms.TTerm Syntax.EquivalentDataProperties -> Phantoms.TTerm [Syntax.Annotation]
equivalentDataPropertiesAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentDataProperties"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equivalentDataPropertiesProperties :: Phantoms.TTerm Syntax.EquivalentDataProperties -> Phantoms.TTerm [Syntax.DataPropertyExpression]
equivalentDataPropertiesProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentDataProperties"),
        Core.projectionField = (Core.Name "properties")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equivalentDataPropertiesWithAnnotations :: Phantoms.TTerm Syntax.EquivalentDataProperties -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.EquivalentDataProperties
equivalentDataPropertiesWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentDataProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentDataProperties"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

equivalentDataPropertiesWithProperties :: Phantoms.TTerm Syntax.EquivalentDataProperties -> Phantoms.TTerm [Syntax.DataPropertyExpression] -> Phantoms.TTerm Syntax.EquivalentDataProperties
equivalentDataPropertiesWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentDataProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentDataProperties"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

equivalentObjectProperties :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm [Syntax.ObjectPropertyExpression] -> Phantoms.TTerm Syntax.EquivalentObjectProperties
equivalentObjectProperties annotations properties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentObjectProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)}]}))

equivalentObjectPropertiesAnnotations :: Phantoms.TTerm Syntax.EquivalentObjectProperties -> Phantoms.TTerm [Syntax.Annotation]
equivalentObjectPropertiesAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentObjectProperties"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equivalentObjectPropertiesProperties :: Phantoms.TTerm Syntax.EquivalentObjectProperties -> Phantoms.TTerm [Syntax.ObjectPropertyExpression]
equivalentObjectPropertiesProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentObjectProperties"),
        Core.projectionField = (Core.Name "properties")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equivalentObjectPropertiesWithAnnotations :: Phantoms.TTerm Syntax.EquivalentObjectProperties -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.EquivalentObjectProperties
equivalentObjectPropertiesWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentObjectProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentObjectProperties"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

equivalentObjectPropertiesWithProperties :: Phantoms.TTerm Syntax.EquivalentObjectProperties -> Phantoms.TTerm [Syntax.ObjectPropertyExpression] -> Phantoms.TTerm Syntax.EquivalentObjectProperties
equivalentObjectPropertiesWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentObjectProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.EquivalentObjectProperties"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionalDataProperty :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm Syntax.FunctionalDataProperty
functionalDataProperty annotations property =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.FunctionalDataProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)}]}))

functionalDataPropertyAnnotations :: Phantoms.TTerm Syntax.FunctionalDataProperty -> Phantoms.TTerm [Syntax.Annotation]
functionalDataPropertyAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.FunctionalDataProperty"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionalDataPropertyProperty :: Phantoms.TTerm Syntax.FunctionalDataProperty -> Phantoms.TTerm Syntax.DataPropertyExpression
functionalDataPropertyProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.FunctionalDataProperty"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionalDataPropertyWithAnnotations :: Phantoms.TTerm Syntax.FunctionalDataProperty -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.FunctionalDataProperty
functionalDataPropertyWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.FunctionalDataProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.FunctionalDataProperty"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionalDataPropertyWithProperty :: Phantoms.TTerm Syntax.FunctionalDataProperty -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm Syntax.FunctionalDataProperty
functionalDataPropertyWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.FunctionalDataProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.FunctionalDataProperty"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionalObjectProperty :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.FunctionalObjectProperty
functionalObjectProperty annotations property =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.FunctionalObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)}]}))

functionalObjectPropertyAnnotations :: Phantoms.TTerm Syntax.FunctionalObjectProperty -> Phantoms.TTerm [Syntax.Annotation]
functionalObjectPropertyAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.FunctionalObjectProperty"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionalObjectPropertyProperty :: Phantoms.TTerm Syntax.FunctionalObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyExpression
functionalObjectPropertyProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.FunctionalObjectProperty"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionalObjectPropertyWithAnnotations :: Phantoms.TTerm Syntax.FunctionalObjectProperty -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.FunctionalObjectProperty
functionalObjectPropertyWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.FunctionalObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.FunctionalObjectProperty"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionalObjectPropertyWithProperty :: Phantoms.TTerm Syntax.FunctionalObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.FunctionalObjectProperty
functionalObjectPropertyWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.FunctionalObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.FunctionalObjectProperty"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

hasKey :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm [Syntax.ObjectPropertyExpression] -> Phantoms.TTerm [Syntax.DataPropertyExpression] -> Phantoms.TTerm Syntax.HasKey
hasKey annotations class_ objectProperties dataProperties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
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

hasKeyAnnotations :: Phantoms.TTerm Syntax.HasKey -> Phantoms.TTerm [Syntax.Annotation]
hasKeyAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hasKeyClass :: Phantoms.TTerm Syntax.HasKey -> Phantoms.TTerm Syntax.ClassExpression
hasKeyClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
        Core.projectionField = (Core.Name "class")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hasKeyDataProperties :: Phantoms.TTerm Syntax.HasKey -> Phantoms.TTerm [Syntax.DataPropertyExpression]
hasKeyDataProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
        Core.projectionField = (Core.Name "dataProperties")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hasKeyObjectProperties :: Phantoms.TTerm Syntax.HasKey -> Phantoms.TTerm [Syntax.ObjectPropertyExpression]
hasKeyObjectProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
        Core.projectionField = (Core.Name "objectProperties")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hasKeyWithAnnotations :: Phantoms.TTerm Syntax.HasKey -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.HasKey
hasKeyWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "class")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "objectProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "objectProperties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "dataProperties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

hasKeyWithClass :: Phantoms.TTerm Syntax.HasKey -> Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm Syntax.HasKey
hasKeyWithClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "objectProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "objectProperties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "dataProperties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

hasKeyWithDataProperties :: Phantoms.TTerm Syntax.HasKey -> Phantoms.TTerm [Syntax.DataPropertyExpression] -> Phantoms.TTerm Syntax.HasKey
hasKeyWithDataProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "class")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "objectProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "objectProperties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataProperties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

hasKeyWithObjectProperties :: Phantoms.TTerm Syntax.HasKey -> Phantoms.TTerm [Syntax.ObjectPropertyExpression] -> Phantoms.TTerm Syntax.HasKey
hasKeyWithObjectProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "class")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "objectProperties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dataProperties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.HasKey"),
              Core.projectionField = (Core.Name "dataProperties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

individualAnonymous :: Phantoms.TTerm Syntax.AnonymousIndividual -> Phantoms.TTerm Syntax.Individual
individualAnonymous x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Individual"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anonymous"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

individualNamed :: Phantoms.TTerm Syntax.NamedIndividual -> Phantoms.TTerm Syntax.Individual
individualNamed x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Individual"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

inverseFunctionalObjectProperty :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.InverseFunctionalObjectProperty
inverseFunctionalObjectProperty annotations property =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseFunctionalObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)}]}))

inverseFunctionalObjectPropertyAnnotations :: Phantoms.TTerm Syntax.InverseFunctionalObjectProperty -> Phantoms.TTerm [Syntax.Annotation]
inverseFunctionalObjectPropertyAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseFunctionalObjectProperty"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inverseFunctionalObjectPropertyProperty :: Phantoms.TTerm Syntax.InverseFunctionalObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyExpression
inverseFunctionalObjectPropertyProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseFunctionalObjectProperty"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inverseFunctionalObjectPropertyWithAnnotations :: Phantoms.TTerm Syntax.InverseFunctionalObjectProperty -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.InverseFunctionalObjectProperty
inverseFunctionalObjectPropertyWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseFunctionalObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseFunctionalObjectProperty"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inverseFunctionalObjectPropertyWithProperty :: Phantoms.TTerm Syntax.InverseFunctionalObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.InverseFunctionalObjectProperty
inverseFunctionalObjectPropertyWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseFunctionalObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseFunctionalObjectProperty"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inverseObjectProperties :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.InverseObjectProperties
inverseObjectProperties annotations property1 property2 =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseObjectProperties"),
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

inverseObjectPropertiesAnnotations :: Phantoms.TTerm Syntax.InverseObjectProperties -> Phantoms.TTerm [Syntax.Annotation]
inverseObjectPropertiesAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseObjectProperties"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inverseObjectPropertiesProperty1 :: Phantoms.TTerm Syntax.InverseObjectProperties -> Phantoms.TTerm Syntax.ObjectPropertyExpression
inverseObjectPropertiesProperty1 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseObjectProperties"),
        Core.projectionField = (Core.Name "property1")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inverseObjectPropertiesProperty2 :: Phantoms.TTerm Syntax.InverseObjectProperties -> Phantoms.TTerm Syntax.ObjectPropertyExpression
inverseObjectPropertiesProperty2 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseObjectProperties"),
        Core.projectionField = (Core.Name "property2")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inverseObjectPropertiesWithAnnotations :: Phantoms.TTerm Syntax.InverseObjectProperties -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.InverseObjectProperties
inverseObjectPropertiesWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseObjectProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseObjectProperties"),
              Core.projectionField = (Core.Name "property1")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseObjectProperties"),
              Core.projectionField = (Core.Name "property2")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inverseObjectPropertiesWithProperty1 :: Phantoms.TTerm Syntax.InverseObjectProperties -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.InverseObjectProperties
inverseObjectPropertiesWithProperty1 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseObjectProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseObjectProperties"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property1"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseObjectProperties"),
              Core.projectionField = (Core.Name "property2")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inverseObjectPropertiesWithProperty2 :: Phantoms.TTerm Syntax.InverseObjectProperties -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.InverseObjectProperties
inverseObjectPropertiesWithProperty2 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseObjectProperties"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseObjectProperties"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseObjectProperties"),
              Core.projectionField = (Core.Name "property1")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property2"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inverseObjectProperty :: Phantoms.TTerm Syntax.ObjectProperty -> Phantoms.TTerm Syntax.InverseObjectProperty
inverseObjectProperty x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.InverseObjectProperty"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

irreflexiveObjectProperty :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.IrreflexiveObjectProperty
irreflexiveObjectProperty annotations property =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.IrreflexiveObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)}]}))

irreflexiveObjectPropertyAnnotations :: Phantoms.TTerm Syntax.IrreflexiveObjectProperty -> Phantoms.TTerm [Syntax.Annotation]
irreflexiveObjectPropertyAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.IrreflexiveObjectProperty"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

irreflexiveObjectPropertyProperty :: Phantoms.TTerm Syntax.IrreflexiveObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyExpression
irreflexiveObjectPropertyProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.IrreflexiveObjectProperty"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

irreflexiveObjectPropertyWithAnnotations :: Phantoms.TTerm Syntax.IrreflexiveObjectProperty -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.IrreflexiveObjectProperty
irreflexiveObjectPropertyWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.IrreflexiveObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.IrreflexiveObjectProperty"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

irreflexiveObjectPropertyWithProperty :: Phantoms.TTerm Syntax.IrreflexiveObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.IrreflexiveObjectProperty
irreflexiveObjectPropertyWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.IrreflexiveObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.IrreflexiveObjectProperty"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

namedIndividual :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.NamedIndividual
namedIndividual x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NamedIndividual"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

negativeDataPropertyAssertion :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm Syntax.Individual -> Phantoms.TTerm Syntax.Individual -> Phantoms.TTerm Syntax.NegativeDataPropertyAssertion
negativeDataPropertyAssertion annotations property source target =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
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

negativeDataPropertyAssertionAnnotations :: Phantoms.TTerm Syntax.NegativeDataPropertyAssertion -> Phantoms.TTerm [Syntax.Annotation]
negativeDataPropertyAssertionAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

negativeDataPropertyAssertionProperty :: Phantoms.TTerm Syntax.NegativeDataPropertyAssertion -> Phantoms.TTerm Syntax.DataPropertyExpression
negativeDataPropertyAssertionProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

negativeDataPropertyAssertionSource :: Phantoms.TTerm Syntax.NegativeDataPropertyAssertion -> Phantoms.TTerm Syntax.Individual
negativeDataPropertyAssertionSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
        Core.projectionField = (Core.Name "source")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

negativeDataPropertyAssertionTarget :: Phantoms.TTerm Syntax.NegativeDataPropertyAssertion -> Phantoms.TTerm Syntax.Individual
negativeDataPropertyAssertionTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
        Core.projectionField = (Core.Name "target")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

negativeDataPropertyAssertionWithAnnotations :: Phantoms.TTerm Syntax.NegativeDataPropertyAssertion -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.NegativeDataPropertyAssertion
negativeDataPropertyAssertionWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

negativeDataPropertyAssertionWithProperty :: Phantoms.TTerm Syntax.NegativeDataPropertyAssertion -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm Syntax.NegativeDataPropertyAssertion
negativeDataPropertyAssertionWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

negativeDataPropertyAssertionWithSource :: Phantoms.TTerm Syntax.NegativeDataPropertyAssertion -> Phantoms.TTerm Syntax.Individual -> Phantoms.TTerm Syntax.NegativeDataPropertyAssertion
negativeDataPropertyAssertionWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

negativeDataPropertyAssertionWithTarget :: Phantoms.TTerm Syntax.NegativeDataPropertyAssertion -> Phantoms.TTerm Syntax.Individual -> Phantoms.TTerm Syntax.NegativeDataPropertyAssertion
negativeDataPropertyAssertionWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

negativeObjectPropertyAssertion :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.Individual -> Phantoms.TTerm Syntax.Individual -> Phantoms.TTerm Syntax.NegativeObjectPropertyAssertion
negativeObjectPropertyAssertion annotations property source target =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
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

negativeObjectPropertyAssertionAnnotations :: Phantoms.TTerm Syntax.NegativeObjectPropertyAssertion -> Phantoms.TTerm [Syntax.Annotation]
negativeObjectPropertyAssertionAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

negativeObjectPropertyAssertionProperty :: Phantoms.TTerm Syntax.NegativeObjectPropertyAssertion -> Phantoms.TTerm Syntax.ObjectPropertyExpression
negativeObjectPropertyAssertionProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

negativeObjectPropertyAssertionSource :: Phantoms.TTerm Syntax.NegativeObjectPropertyAssertion -> Phantoms.TTerm Syntax.Individual
negativeObjectPropertyAssertionSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
        Core.projectionField = (Core.Name "source")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

negativeObjectPropertyAssertionTarget :: Phantoms.TTerm Syntax.NegativeObjectPropertyAssertion -> Phantoms.TTerm Syntax.Individual
negativeObjectPropertyAssertionTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
        Core.projectionField = (Core.Name "target")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

negativeObjectPropertyAssertionWithAnnotations :: Phantoms.TTerm Syntax.NegativeObjectPropertyAssertion -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.NegativeObjectPropertyAssertion
negativeObjectPropertyAssertionWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

negativeObjectPropertyAssertionWithProperty :: Phantoms.TTerm Syntax.NegativeObjectPropertyAssertion -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.NegativeObjectPropertyAssertion
negativeObjectPropertyAssertionWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

negativeObjectPropertyAssertionWithSource :: Phantoms.TTerm Syntax.NegativeObjectPropertyAssertion -> Phantoms.TTerm Syntax.Individual -> Phantoms.TTerm Syntax.NegativeObjectPropertyAssertion
negativeObjectPropertyAssertionWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

negativeObjectPropertyAssertionWithTarget :: Phantoms.TTerm Syntax.NegativeObjectPropertyAssertion -> Phantoms.TTerm Syntax.Individual -> Phantoms.TTerm Syntax.NegativeObjectPropertyAssertion
negativeObjectPropertyAssertionWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectAllValuesFrom :: Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm Syntax.ObjectAllValuesFrom
objectAllValuesFrom property class_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectAllValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm class_)}]}))

objectAllValuesFromClass :: Phantoms.TTerm Syntax.ObjectAllValuesFrom -> Phantoms.TTerm Syntax.ClassExpression
objectAllValuesFromClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectAllValuesFrom"),
        Core.projectionField = (Core.Name "class")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectAllValuesFromProperty :: Phantoms.TTerm Syntax.ObjectAllValuesFrom -> Phantoms.TTerm Syntax.ObjectPropertyExpression
objectAllValuesFromProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectAllValuesFrom"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectAllValuesFromWithClass :: Phantoms.TTerm Syntax.ObjectAllValuesFrom -> Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm Syntax.ObjectAllValuesFrom
objectAllValuesFromWithClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectAllValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectAllValuesFrom"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectAllValuesFromWithProperty :: Phantoms.TTerm Syntax.ObjectAllValuesFrom -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.ObjectAllValuesFrom
objectAllValuesFromWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectAllValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectAllValuesFrom"),
              Core.projectionField = (Core.Name "class")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectComplementOf :: Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm Syntax.ObjectComplementOf
objectComplementOf x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectComplementOf"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

objectExactCardinality :: Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm [Syntax.ClassExpression] -> Phantoms.TTerm Syntax.ObjectExactCardinality
objectExactCardinality bound property class_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectExactCardinality"),
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

objectExactCardinalityBound :: Phantoms.TTerm Syntax.ObjectExactCardinality -> Phantoms.TTerm Integer
objectExactCardinalityBound x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectExactCardinality"),
        Core.projectionField = (Core.Name "bound")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectExactCardinalityClass :: Phantoms.TTerm Syntax.ObjectExactCardinality -> Phantoms.TTerm [Syntax.ClassExpression]
objectExactCardinalityClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectExactCardinality"),
        Core.projectionField = (Core.Name "class")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectExactCardinalityProperty :: Phantoms.TTerm Syntax.ObjectExactCardinality -> Phantoms.TTerm Syntax.ObjectPropertyExpression
objectExactCardinalityProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectExactCardinality"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectExactCardinalityWithBound :: Phantoms.TTerm Syntax.ObjectExactCardinality -> Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.ObjectExactCardinality
objectExactCardinalityWithBound original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectExactCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectExactCardinality"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectExactCardinality"),
              Core.projectionField = (Core.Name "class")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectExactCardinalityWithClass :: Phantoms.TTerm Syntax.ObjectExactCardinality -> Phantoms.TTerm [Syntax.ClassExpression] -> Phantoms.TTerm Syntax.ObjectExactCardinality
objectExactCardinalityWithClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectExactCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectExactCardinality"),
              Core.projectionField = (Core.Name "bound")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectExactCardinality"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectExactCardinalityWithProperty :: Phantoms.TTerm Syntax.ObjectExactCardinality -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.ObjectExactCardinality
objectExactCardinalityWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectExactCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectExactCardinality"),
              Core.projectionField = (Core.Name "bound")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectExactCardinality"),
              Core.projectionField = (Core.Name "class")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectHasSelf :: Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.ObjectHasSelf
objectHasSelf x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectHasSelf"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

objectHasValue :: Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.Individual -> Phantoms.TTerm Syntax.ObjectHasValue
objectHasValue property individual =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectHasValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "individual"),
          Core.fieldTerm = (Phantoms.unTTerm individual)}]}))

objectHasValueIndividual :: Phantoms.TTerm Syntax.ObjectHasValue -> Phantoms.TTerm Syntax.Individual
objectHasValueIndividual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectHasValue"),
        Core.projectionField = (Core.Name "individual")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectHasValueProperty :: Phantoms.TTerm Syntax.ObjectHasValue -> Phantoms.TTerm Syntax.ObjectPropertyExpression
objectHasValueProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectHasValue"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectHasValueWithIndividual :: Phantoms.TTerm Syntax.ObjectHasValue -> Phantoms.TTerm Syntax.Individual -> Phantoms.TTerm Syntax.ObjectHasValue
objectHasValueWithIndividual original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectHasValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectHasValue"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "individual"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectHasValueWithProperty :: Phantoms.TTerm Syntax.ObjectHasValue -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.ObjectHasValue
objectHasValueWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectHasValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "individual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectHasValue"),
              Core.projectionField = (Core.Name "individual")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectIntersectionOf :: Phantoms.TTerm [Syntax.ClassExpression] -> Phantoms.TTerm Syntax.ObjectIntersectionOf
objectIntersectionOf x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectIntersectionOf"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

objectMaxCardinality :: Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm [Syntax.ClassExpression] -> Phantoms.TTerm Syntax.ObjectMaxCardinality
objectMaxCardinality bound property class_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMaxCardinality"),
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

objectMaxCardinalityBound :: Phantoms.TTerm Syntax.ObjectMaxCardinality -> Phantoms.TTerm Integer
objectMaxCardinalityBound x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMaxCardinality"),
        Core.projectionField = (Core.Name "bound")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectMaxCardinalityClass :: Phantoms.TTerm Syntax.ObjectMaxCardinality -> Phantoms.TTerm [Syntax.ClassExpression]
objectMaxCardinalityClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMaxCardinality"),
        Core.projectionField = (Core.Name "class")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectMaxCardinalityProperty :: Phantoms.TTerm Syntax.ObjectMaxCardinality -> Phantoms.TTerm Syntax.ObjectPropertyExpression
objectMaxCardinalityProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMaxCardinality"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectMaxCardinalityWithBound :: Phantoms.TTerm Syntax.ObjectMaxCardinality -> Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.ObjectMaxCardinality
objectMaxCardinalityWithBound original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMaxCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMaxCardinality"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMaxCardinality"),
              Core.projectionField = (Core.Name "class")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectMaxCardinalityWithClass :: Phantoms.TTerm Syntax.ObjectMaxCardinality -> Phantoms.TTerm [Syntax.ClassExpression] -> Phantoms.TTerm Syntax.ObjectMaxCardinality
objectMaxCardinalityWithClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMaxCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMaxCardinality"),
              Core.projectionField = (Core.Name "bound")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMaxCardinality"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectMaxCardinalityWithProperty :: Phantoms.TTerm Syntax.ObjectMaxCardinality -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.ObjectMaxCardinality
objectMaxCardinalityWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMaxCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMaxCardinality"),
              Core.projectionField = (Core.Name "bound")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMaxCardinality"),
              Core.projectionField = (Core.Name "class")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectMinCardinality :: Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm [Syntax.ClassExpression] -> Phantoms.TTerm Syntax.ObjectMinCardinality
objectMinCardinality bound property class_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMinCardinality"),
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

objectMinCardinalityBound :: Phantoms.TTerm Syntax.ObjectMinCardinality -> Phantoms.TTerm Integer
objectMinCardinalityBound x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMinCardinality"),
        Core.projectionField = (Core.Name "bound")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectMinCardinalityClass :: Phantoms.TTerm Syntax.ObjectMinCardinality -> Phantoms.TTerm [Syntax.ClassExpression]
objectMinCardinalityClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMinCardinality"),
        Core.projectionField = (Core.Name "class")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectMinCardinalityProperty :: Phantoms.TTerm Syntax.ObjectMinCardinality -> Phantoms.TTerm Syntax.ObjectPropertyExpression
objectMinCardinalityProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMinCardinality"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectMinCardinalityWithBound :: Phantoms.TTerm Syntax.ObjectMinCardinality -> Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.ObjectMinCardinality
objectMinCardinalityWithBound original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMinCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMinCardinality"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMinCardinality"),
              Core.projectionField = (Core.Name "class")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectMinCardinalityWithClass :: Phantoms.TTerm Syntax.ObjectMinCardinality -> Phantoms.TTerm [Syntax.ClassExpression] -> Phantoms.TTerm Syntax.ObjectMinCardinality
objectMinCardinalityWithClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMinCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMinCardinality"),
              Core.projectionField = (Core.Name "bound")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMinCardinality"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectMinCardinalityWithProperty :: Phantoms.TTerm Syntax.ObjectMinCardinality -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.ObjectMinCardinality
objectMinCardinalityWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMinCardinality"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMinCardinality"),
              Core.projectionField = (Core.Name "bound")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectMinCardinality"),
              Core.projectionField = (Core.Name "class")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectOneOf :: Phantoms.TTerm [Syntax.Individual] -> Phantoms.TTerm Syntax.ObjectOneOf
objectOneOf x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectOneOf"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

objectProperty :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.ObjectProperty
objectProperty x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectProperty"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

objectPropertyAssertion :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.Individual -> Phantoms.TTerm Syntax.Individual -> Phantoms.TTerm Syntax.ObjectPropertyAssertion
objectPropertyAssertion annotations property source target =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
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

objectPropertyAssertionAnnotations :: Phantoms.TTerm Syntax.ObjectPropertyAssertion -> Phantoms.TTerm [Syntax.Annotation]
objectPropertyAssertionAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPropertyAssertionProperty :: Phantoms.TTerm Syntax.ObjectPropertyAssertion -> Phantoms.TTerm Syntax.ObjectPropertyExpression
objectPropertyAssertionProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPropertyAssertionSource :: Phantoms.TTerm Syntax.ObjectPropertyAssertion -> Phantoms.TTerm Syntax.Individual
objectPropertyAssertionSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
        Core.projectionField = (Core.Name "source")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPropertyAssertionTarget :: Phantoms.TTerm Syntax.ObjectPropertyAssertion -> Phantoms.TTerm Syntax.Individual
objectPropertyAssertionTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
        Core.projectionField = (Core.Name "target")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPropertyAssertionWithAnnotations :: Phantoms.TTerm Syntax.ObjectPropertyAssertion -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ObjectPropertyAssertion
objectPropertyAssertionWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectPropertyAssertionWithProperty :: Phantoms.TTerm Syntax.ObjectPropertyAssertion -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.ObjectPropertyAssertion
objectPropertyAssertionWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectPropertyAssertionWithSource :: Phantoms.TTerm Syntax.ObjectPropertyAssertion -> Phantoms.TTerm Syntax.Individual -> Phantoms.TTerm Syntax.ObjectPropertyAssertion
objectPropertyAssertionWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectPropertyAssertionWithTarget :: Phantoms.TTerm Syntax.ObjectPropertyAssertion -> Phantoms.TTerm Syntax.Individual -> Phantoms.TTerm Syntax.ObjectPropertyAssertion
objectPropertyAssertionWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectPropertyAxiomAsymmetricObjectProperty :: Phantoms.TTerm Syntax.AsymmetricObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyAxiom
objectPropertyAxiomAsymmetricObjectProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "asymmetricObjectProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomDisjointObjectProperties :: Phantoms.TTerm Syntax.DisjointObjectProperties -> Phantoms.TTerm Syntax.ObjectPropertyAxiom
objectPropertyAxiomDisjointObjectProperties x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "disjointObjectProperties"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomEquivalentObjectProperties :: Phantoms.TTerm Syntax.EquivalentObjectProperties -> Phantoms.TTerm Syntax.ObjectPropertyAxiom
objectPropertyAxiomEquivalentObjectProperties x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equivalentObjectProperties"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomFunctionalObjectProperty :: Phantoms.TTerm Syntax.FunctionalObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyAxiom
objectPropertyAxiomFunctionalObjectProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "functionalObjectProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomInverseFunctionalObjectProperty :: Phantoms.TTerm Syntax.InverseFunctionalObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyAxiom
objectPropertyAxiomInverseFunctionalObjectProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inverseFunctionalObjectProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomInverseObjectProperties :: Phantoms.TTerm Syntax.InverseObjectProperties -> Phantoms.TTerm Syntax.ObjectPropertyAxiom
objectPropertyAxiomInverseObjectProperties x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inverseObjectProperties"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomIrreflexiveObjectProperty :: Phantoms.TTerm Syntax.IrreflexiveObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyAxiom
objectPropertyAxiomIrreflexiveObjectProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "irreflexiveObjectProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomObjectPropertyDomain :: Phantoms.TTerm Syntax.ObjectPropertyDomain -> Phantoms.TTerm Syntax.ObjectPropertyAxiom
objectPropertyAxiomObjectPropertyDomain x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectPropertyDomain"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomObjectPropertyRange :: Phantoms.TTerm Syntax.ObjectPropertyRange -> Phantoms.TTerm Syntax.ObjectPropertyAxiom
objectPropertyAxiomObjectPropertyRange x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "objectPropertyRange"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomReflexiveObjectProperty :: Phantoms.TTerm Syntax.ReflexiveObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyAxiom
objectPropertyAxiomReflexiveObjectProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reflexiveObjectProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomSubObjectPropertyOf :: Phantoms.TTerm Syntax.SubObjectPropertyOf -> Phantoms.TTerm Syntax.ObjectPropertyAxiom
objectPropertyAxiomSubObjectPropertyOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subObjectPropertyOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomSymmetricObjectProperty :: Phantoms.TTerm Syntax.SymmetricObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyAxiom
objectPropertyAxiomSymmetricObjectProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "symmetricObjectProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyAxiomTransitiveObjectProperty :: Phantoms.TTerm Syntax.TransitiveObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyAxiom
objectPropertyAxiomTransitiveObjectProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyAxiom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "transitiveObjectProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyDomain :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm Syntax.ObjectPropertyDomain
objectPropertyDomain annotations property domain =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyDomain"),
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

objectPropertyDomainAnnotations :: Phantoms.TTerm Syntax.ObjectPropertyDomain -> Phantoms.TTerm [Syntax.Annotation]
objectPropertyDomainAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyDomain"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPropertyDomainDomain :: Phantoms.TTerm Syntax.ObjectPropertyDomain -> Phantoms.TTerm Syntax.ClassExpression
objectPropertyDomainDomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyDomain"),
        Core.projectionField = (Core.Name "domain")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPropertyDomainProperty :: Phantoms.TTerm Syntax.ObjectPropertyDomain -> Phantoms.TTerm Syntax.ObjectPropertyExpression
objectPropertyDomainProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyDomain"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPropertyDomainWithAnnotations :: Phantoms.TTerm Syntax.ObjectPropertyDomain -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ObjectPropertyDomain
objectPropertyDomainWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyDomain"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyDomain"),
              Core.projectionField = (Core.Name "domain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectPropertyDomainWithDomain :: Phantoms.TTerm Syntax.ObjectPropertyDomain -> Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm Syntax.ObjectPropertyDomain
objectPropertyDomainWithDomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyDomain"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyDomain"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectPropertyDomainWithProperty :: Phantoms.TTerm Syntax.ObjectPropertyDomain -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.ObjectPropertyDomain
objectPropertyDomainWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyDomain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyDomain"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyDomain"),
              Core.projectionField = (Core.Name "domain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectPropertyExpressionInverseObject :: Phantoms.TTerm Syntax.InverseObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyExpression
objectPropertyExpressionInverseObject x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inverseObject"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyExpressionObject :: Phantoms.TTerm Syntax.ObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyExpression
objectPropertyExpressionObject x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPropertyRange :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm Syntax.ObjectPropertyRange
objectPropertyRange annotations property range =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyRange"),
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

objectPropertyRangeAnnotations :: Phantoms.TTerm Syntax.ObjectPropertyRange -> Phantoms.TTerm [Syntax.Annotation]
objectPropertyRangeAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyRange"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPropertyRangeProperty :: Phantoms.TTerm Syntax.ObjectPropertyRange -> Phantoms.TTerm Syntax.ObjectPropertyExpression
objectPropertyRangeProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyRange"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPropertyRangeRange :: Phantoms.TTerm Syntax.ObjectPropertyRange -> Phantoms.TTerm Syntax.ClassExpression
objectPropertyRangeRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyRange"),
        Core.projectionField = (Core.Name "range")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPropertyRangeWithAnnotations :: Phantoms.TTerm Syntax.ObjectPropertyRange -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ObjectPropertyRange
objectPropertyRangeWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyRange"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyRange"),
              Core.projectionField = (Core.Name "range")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectPropertyRangeWithProperty :: Phantoms.TTerm Syntax.ObjectPropertyRange -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.ObjectPropertyRange
objectPropertyRangeWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyRange"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyRange"),
              Core.projectionField = (Core.Name "range")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectPropertyRangeWithRange :: Phantoms.TTerm Syntax.ObjectPropertyRange -> Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm Syntax.ObjectPropertyRange
objectPropertyRangeWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyRange"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectPropertyRange"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectSomeValuesFrom :: Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm Syntax.ObjectSomeValuesFrom
objectSomeValuesFrom property class_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectSomeValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm class_)}]}))

objectSomeValuesFromClass :: Phantoms.TTerm Syntax.ObjectSomeValuesFrom -> Phantoms.TTerm Syntax.ClassExpression
objectSomeValuesFromClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectSomeValuesFrom"),
        Core.projectionField = (Core.Name "class")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectSomeValuesFromProperty :: Phantoms.TTerm Syntax.ObjectSomeValuesFrom -> Phantoms.TTerm Syntax.ObjectPropertyExpression
objectSomeValuesFromProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectSomeValuesFrom"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectSomeValuesFromWithClass :: Phantoms.TTerm Syntax.ObjectSomeValuesFrom -> Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm Syntax.ObjectSomeValuesFrom
objectSomeValuesFromWithClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectSomeValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectSomeValuesFrom"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

objectSomeValuesFromWithProperty :: Phantoms.TTerm Syntax.ObjectSomeValuesFrom -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.ObjectSomeValuesFrom
objectSomeValuesFromWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectSomeValuesFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectSomeValuesFrom"),
              Core.projectionField = (Core.Name "class")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectUnionOf :: Phantoms.TTerm [Syntax.ClassExpression] -> Phantoms.TTerm Syntax.ObjectUnionOf
objectUnionOf x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectUnionOf"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

ontology :: Phantoms.TTerm [Syntax.Ontology] -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm [Syntax.Axiom] -> Phantoms.TTerm Syntax.Ontology
ontology directImports annotations axioms =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Ontology"),
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

ontologyAnnotations :: Phantoms.TTerm Syntax.Ontology -> Phantoms.TTerm [Syntax.Annotation]
ontologyAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Ontology"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ontologyAxioms :: Phantoms.TTerm Syntax.Ontology -> Phantoms.TTerm [Syntax.Axiom]
ontologyAxioms x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Ontology"),
        Core.projectionField = (Core.Name "axioms")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ontologyDirectImports :: Phantoms.TTerm Syntax.Ontology -> Phantoms.TTerm [Syntax.Ontology]
ontologyDirectImports x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Ontology"),
        Core.projectionField = (Core.Name "directImports")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ontologyWithAnnotations :: Phantoms.TTerm Syntax.Ontology -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.Ontology
ontologyWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Ontology"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "directImports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Ontology"),
              Core.projectionField = (Core.Name "directImports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "axioms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Ontology"),
              Core.projectionField = (Core.Name "axioms")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ontologyWithAxioms :: Phantoms.TTerm Syntax.Ontology -> Phantoms.TTerm [Syntax.Axiom] -> Phantoms.TTerm Syntax.Ontology
ontologyWithAxioms original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Ontology"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "directImports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Ontology"),
              Core.projectionField = (Core.Name "directImports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Ontology"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "axioms"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

ontologyWithDirectImports :: Phantoms.TTerm Syntax.Ontology -> Phantoms.TTerm [Syntax.Ontology] -> Phantoms.TTerm Syntax.Ontology
ontologyWithDirectImports original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Ontology"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "directImports"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Ontology"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "axioms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.Ontology"),
              Core.projectionField = (Core.Name "axioms")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

reflexiveObjectProperty :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.ReflexiveObjectProperty
reflexiveObjectProperty annotations property =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ReflexiveObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)}]}))

reflexiveObjectPropertyAnnotations :: Phantoms.TTerm Syntax.ReflexiveObjectProperty -> Phantoms.TTerm [Syntax.Annotation]
reflexiveObjectPropertyAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ReflexiveObjectProperty"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

reflexiveObjectPropertyProperty :: Phantoms.TTerm Syntax.ReflexiveObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyExpression
reflexiveObjectPropertyProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ReflexiveObjectProperty"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

reflexiveObjectPropertyWithAnnotations :: Phantoms.TTerm Syntax.ReflexiveObjectProperty -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ReflexiveObjectProperty
reflexiveObjectPropertyWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ReflexiveObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ReflexiveObjectProperty"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

reflexiveObjectPropertyWithProperty :: Phantoms.TTerm Syntax.ReflexiveObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.ReflexiveObjectProperty
reflexiveObjectPropertyWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ReflexiveObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.ReflexiveObjectProperty"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

sameIndividual :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm [Syntax.Individual] -> Phantoms.TTerm Syntax.SameIndividual
sameIndividual annotations individuals =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SameIndividual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "individuals"),
          Core.fieldTerm = (Phantoms.unTTerm individuals)}]}))

sameIndividualAnnotations :: Phantoms.TTerm Syntax.SameIndividual -> Phantoms.TTerm [Syntax.Annotation]
sameIndividualAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SameIndividual"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sameIndividualIndividuals :: Phantoms.TTerm Syntax.SameIndividual -> Phantoms.TTerm [Syntax.Individual]
sameIndividualIndividuals x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SameIndividual"),
        Core.projectionField = (Core.Name "individuals")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sameIndividualWithAnnotations :: Phantoms.TTerm Syntax.SameIndividual -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.SameIndividual
sameIndividualWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SameIndividual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "individuals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SameIndividual"),
              Core.projectionField = (Core.Name "individuals")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

sameIndividualWithIndividuals :: Phantoms.TTerm Syntax.SameIndividual -> Phantoms.TTerm [Syntax.Individual] -> Phantoms.TTerm Syntax.SameIndividual
sameIndividualWithIndividuals original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SameIndividual"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SameIndividual"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "individuals"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

subAnnotationPropertyOf :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.AnnotationProperty -> Phantoms.TTerm Syntax.AnnotationProperty -> Phantoms.TTerm Syntax.SubAnnotationPropertyOf
subAnnotationPropertyOf annotations subProperty superProperty =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubAnnotationPropertyOf"),
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

subAnnotationPropertyOfAnnotations :: Phantoms.TTerm Syntax.SubAnnotationPropertyOf -> Phantoms.TTerm [Syntax.Annotation]
subAnnotationPropertyOfAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubAnnotationPropertyOf"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subAnnotationPropertyOfSubProperty :: Phantoms.TTerm Syntax.SubAnnotationPropertyOf -> Phantoms.TTerm Syntax.AnnotationProperty
subAnnotationPropertyOfSubProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubAnnotationPropertyOf"),
        Core.projectionField = (Core.Name "subProperty")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subAnnotationPropertyOfSuperProperty :: Phantoms.TTerm Syntax.SubAnnotationPropertyOf -> Phantoms.TTerm Syntax.AnnotationProperty
subAnnotationPropertyOfSuperProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubAnnotationPropertyOf"),
        Core.projectionField = (Core.Name "superProperty")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subAnnotationPropertyOfWithAnnotations :: Phantoms.TTerm Syntax.SubAnnotationPropertyOf -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.SubAnnotationPropertyOf
subAnnotationPropertyOfWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubAnnotationPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubAnnotationPropertyOf"),
              Core.projectionField = (Core.Name "subProperty")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubAnnotationPropertyOf"),
              Core.projectionField = (Core.Name "superProperty")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subAnnotationPropertyOfWithSubProperty :: Phantoms.TTerm Syntax.SubAnnotationPropertyOf -> Phantoms.TTerm Syntax.AnnotationProperty -> Phantoms.TTerm Syntax.SubAnnotationPropertyOf
subAnnotationPropertyOfWithSubProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubAnnotationPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubAnnotationPropertyOf"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubAnnotationPropertyOf"),
              Core.projectionField = (Core.Name "superProperty")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subAnnotationPropertyOfWithSuperProperty :: Phantoms.TTerm Syntax.SubAnnotationPropertyOf -> Phantoms.TTerm Syntax.AnnotationProperty -> Phantoms.TTerm Syntax.SubAnnotationPropertyOf
subAnnotationPropertyOfWithSuperProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubAnnotationPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubAnnotationPropertyOf"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubAnnotationPropertyOf"),
              Core.projectionField = (Core.Name "subProperty")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

subClassOf :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm Syntax.SubClassOf
subClassOf annotations subClass superClass =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubClassOf"),
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

subClassOfAnnotations :: Phantoms.TTerm Syntax.SubClassOf -> Phantoms.TTerm [Syntax.Annotation]
subClassOfAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubClassOf"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subClassOfSubClass :: Phantoms.TTerm Syntax.SubClassOf -> Phantoms.TTerm Syntax.ClassExpression
subClassOfSubClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubClassOf"),
        Core.projectionField = (Core.Name "subClass")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subClassOfSuperClass :: Phantoms.TTerm Syntax.SubClassOf -> Phantoms.TTerm Syntax.ClassExpression
subClassOfSuperClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubClassOf"),
        Core.projectionField = (Core.Name "superClass")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subClassOfWithAnnotations :: Phantoms.TTerm Syntax.SubClassOf -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.SubClassOf
subClassOfWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubClassOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubClassOf"),
              Core.projectionField = (Core.Name "subClass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubClassOf"),
              Core.projectionField = (Core.Name "superClass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subClassOfWithSubClass :: Phantoms.TTerm Syntax.SubClassOf -> Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm Syntax.SubClassOf
subClassOfWithSubClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubClassOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubClassOf"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subClass"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "superClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubClassOf"),
              Core.projectionField = (Core.Name "superClass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subClassOfWithSuperClass :: Phantoms.TTerm Syntax.SubClassOf -> Phantoms.TTerm Syntax.ClassExpression -> Phantoms.TTerm Syntax.SubClassOf
subClassOfWithSuperClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubClassOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubClassOf"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubClassOf"),
              Core.projectionField = (Core.Name "subClass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superClass"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

subDataPropertyOf :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm Syntax.SubDataPropertyOf
subDataPropertyOf annotations subProperty superProperty =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubDataPropertyOf"),
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

subDataPropertyOfAnnotations :: Phantoms.TTerm Syntax.SubDataPropertyOf -> Phantoms.TTerm [Syntax.Annotation]
subDataPropertyOfAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubDataPropertyOf"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subDataPropertyOfSubProperty :: Phantoms.TTerm Syntax.SubDataPropertyOf -> Phantoms.TTerm Syntax.DataPropertyExpression
subDataPropertyOfSubProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubDataPropertyOf"),
        Core.projectionField = (Core.Name "subProperty")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subDataPropertyOfSuperProperty :: Phantoms.TTerm Syntax.SubDataPropertyOf -> Phantoms.TTerm Syntax.DataPropertyExpression
subDataPropertyOfSuperProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubDataPropertyOf"),
        Core.projectionField = (Core.Name "superProperty")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subDataPropertyOfWithAnnotations :: Phantoms.TTerm Syntax.SubDataPropertyOf -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.SubDataPropertyOf
subDataPropertyOfWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubDataPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubDataPropertyOf"),
              Core.projectionField = (Core.Name "subProperty")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubDataPropertyOf"),
              Core.projectionField = (Core.Name "superProperty")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subDataPropertyOfWithSubProperty :: Phantoms.TTerm Syntax.SubDataPropertyOf -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm Syntax.SubDataPropertyOf
subDataPropertyOfWithSubProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubDataPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubDataPropertyOf"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubDataPropertyOf"),
              Core.projectionField = (Core.Name "superProperty")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subDataPropertyOfWithSuperProperty :: Phantoms.TTerm Syntax.SubDataPropertyOf -> Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm Syntax.SubDataPropertyOf
subDataPropertyOfWithSuperProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubDataPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubDataPropertyOf"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubDataPropertyOf"),
              Core.projectionField = (Core.Name "subProperty")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

subObjectPropertyOf :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm [Syntax.ObjectPropertyExpression] -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.SubObjectPropertyOf
subObjectPropertyOf annotations subProperty superProperty =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubObjectPropertyOf"),
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

subObjectPropertyOfAnnotations :: Phantoms.TTerm Syntax.SubObjectPropertyOf -> Phantoms.TTerm [Syntax.Annotation]
subObjectPropertyOfAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubObjectPropertyOf"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subObjectPropertyOfSubProperty :: Phantoms.TTerm Syntax.SubObjectPropertyOf -> Phantoms.TTerm [Syntax.ObjectPropertyExpression]
subObjectPropertyOfSubProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubObjectPropertyOf"),
        Core.projectionField = (Core.Name "subProperty")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subObjectPropertyOfSuperProperty :: Phantoms.TTerm Syntax.SubObjectPropertyOf -> Phantoms.TTerm Syntax.ObjectPropertyExpression
subObjectPropertyOfSuperProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubObjectPropertyOf"),
        Core.projectionField = (Core.Name "superProperty")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subObjectPropertyOfWithAnnotations :: Phantoms.TTerm Syntax.SubObjectPropertyOf -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.SubObjectPropertyOf
subObjectPropertyOfWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubObjectPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubObjectPropertyOf"),
              Core.projectionField = (Core.Name "subProperty")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubObjectPropertyOf"),
              Core.projectionField = (Core.Name "superProperty")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subObjectPropertyOfWithSubProperty :: Phantoms.TTerm Syntax.SubObjectPropertyOf -> Phantoms.TTerm [Syntax.ObjectPropertyExpression] -> Phantoms.TTerm Syntax.SubObjectPropertyOf
subObjectPropertyOfWithSubProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubObjectPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubObjectPropertyOf"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubObjectPropertyOf"),
              Core.projectionField = (Core.Name "superProperty")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

subObjectPropertyOfWithSuperProperty :: Phantoms.TTerm Syntax.SubObjectPropertyOf -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.SubObjectPropertyOf
subObjectPropertyOfWithSuperProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubObjectPropertyOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubObjectPropertyOf"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subProperty"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SubObjectPropertyOf"),
              Core.projectionField = (Core.Name "subProperty")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superProperty"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

symmetricObjectProperty :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.SymmetricObjectProperty
symmetricObjectProperty annotations property =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SymmetricObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)}]}))

symmetricObjectPropertyAnnotations :: Phantoms.TTerm Syntax.SymmetricObjectProperty -> Phantoms.TTerm [Syntax.Annotation]
symmetricObjectPropertyAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SymmetricObjectProperty"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

symmetricObjectPropertyProperty :: Phantoms.TTerm Syntax.SymmetricObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyExpression
symmetricObjectPropertyProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SymmetricObjectProperty"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

symmetricObjectPropertyWithAnnotations :: Phantoms.TTerm Syntax.SymmetricObjectProperty -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.SymmetricObjectProperty
symmetricObjectPropertyWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SymmetricObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SymmetricObjectProperty"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

symmetricObjectPropertyWithProperty :: Phantoms.TTerm Syntax.SymmetricObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.SymmetricObjectProperty
symmetricObjectPropertyWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SymmetricObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.SymmetricObjectProperty"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

transitiveObjectProperty :: Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.TransitiveObjectProperty
transitiveObjectProperty annotations property =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.TransitiveObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)}]}))

transitiveObjectPropertyAnnotations :: Phantoms.TTerm Syntax.TransitiveObjectProperty -> Phantoms.TTerm [Syntax.Annotation]
transitiveObjectPropertyAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.TransitiveObjectProperty"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

transitiveObjectPropertyProperty :: Phantoms.TTerm Syntax.TransitiveObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyExpression
transitiveObjectPropertyProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.TransitiveObjectProperty"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

transitiveObjectPropertyWithAnnotations :: Phantoms.TTerm Syntax.TransitiveObjectProperty -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.TransitiveObjectProperty
transitiveObjectPropertyWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.TransitiveObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.TransitiveObjectProperty"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

transitiveObjectPropertyWithProperty :: Phantoms.TTerm Syntax.TransitiveObjectProperty -> Phantoms.TTerm Syntax.ObjectPropertyExpression -> Phantoms.TTerm Syntax.TransitiveObjectProperty
transitiveObjectPropertyWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.TransitiveObjectProperty"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.owl.syntax.TransitiveObjectProperty"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unAnnotationProperty :: Phantoms.TTerm Syntax.AnnotationProperty -> Phantoms.TTerm ()
unAnnotationProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.owl.syntax.AnnotationProperty")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unAnonymousIndividual :: Phantoms.TTerm Syntax.AnonymousIndividual -> Phantoms.TTerm ()
unAnonymousIndividual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.owl.syntax.AnonymousIndividual")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unClass :: Phantoms.TTerm Syntax.Class -> Phantoms.TTerm ()
unClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.owl.syntax.Class")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDataComplementOf :: Phantoms.TTerm Syntax.DataComplementOf -> Phantoms.TTerm Syntax.DataRange
unDataComplementOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.owl.syntax.DataComplementOf")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDataIntersectionOf :: Phantoms.TTerm Syntax.DataIntersectionOf -> Phantoms.TTerm [Syntax.DataRange]
unDataIntersectionOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.owl.syntax.DataIntersectionOf")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDataOneOf :: Phantoms.TTerm Syntax.DataOneOf -> Phantoms.TTerm [Syntax_.Literal]
unDataOneOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.owl.syntax.DataOneOf")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDataProperty :: Phantoms.TTerm Syntax.DataProperty -> Phantoms.TTerm ()
unDataProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.owl.syntax.DataProperty")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDataPropertyExpression :: Phantoms.TTerm Syntax.DataPropertyExpression -> Phantoms.TTerm Syntax.DataProperty
unDataPropertyExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.owl.syntax.DataPropertyExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDataUnionOf :: Phantoms.TTerm Syntax.DataUnionOf -> Phantoms.TTerm [Syntax.DataRange]
unDataUnionOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.owl.syntax.DataUnionOf")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unInverseObjectProperty :: Phantoms.TTerm Syntax.InverseObjectProperty -> Phantoms.TTerm Syntax.ObjectProperty
unInverseObjectProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.owl.syntax.InverseObjectProperty")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unNamedIndividual :: Phantoms.TTerm Syntax.NamedIndividual -> Phantoms.TTerm ()
unNamedIndividual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.owl.syntax.NamedIndividual")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unObjectComplementOf :: Phantoms.TTerm Syntax.ObjectComplementOf -> Phantoms.TTerm Syntax.ClassExpression
unObjectComplementOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectComplementOf")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unObjectHasSelf :: Phantoms.TTerm Syntax.ObjectHasSelf -> Phantoms.TTerm Syntax.ObjectPropertyExpression
unObjectHasSelf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectHasSelf")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unObjectIntersectionOf :: Phantoms.TTerm Syntax.ObjectIntersectionOf -> Phantoms.TTerm [Syntax.ClassExpression]
unObjectIntersectionOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectIntersectionOf")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unObjectOneOf :: Phantoms.TTerm Syntax.ObjectOneOf -> Phantoms.TTerm [Syntax.Individual]
unObjectOneOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectOneOf")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unObjectProperty :: Phantoms.TTerm Syntax.ObjectProperty -> Phantoms.TTerm ()
unObjectProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectProperty")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unObjectUnionOf :: Phantoms.TTerm Syntax.ObjectUnionOf -> Phantoms.TTerm [Syntax.ClassExpression]
unObjectUnionOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.owl.syntax.ObjectUnionOf")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
