-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.rdf.syntax

module Hydra.Dsl.Rdf.Syntax where
import qualified Hydra.Core as Core
import qualified Hydra.Typed as Phantoms
import qualified Hydra.Rdf.Syntax as Syntax
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | DSL constructor for the hydra.rdf.syntax.BlankNode wrapper
blankNode :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Syntax.BlankNode
blankNode x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.BlankNode"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for the hydra.rdf.syntax.Dataset wrapper
dataset :: Phantoms.TypedTerm (S.Set Syntax.Quad) -> Phantoms.TypedTerm Syntax.Dataset
dataset x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.Dataset"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for hydra.rdf.syntax.Description
description :: Phantoms.TypedTerm Syntax.Node -> Phantoms.TypedTerm Syntax.Graph -> Phantoms.TypedTerm Syntax.Description
description subject graph =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Description"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTypedTerm subject)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTypedTerm graph)}]}))
-- | DSL accessor for the graph field of hydra.rdf.syntax.Description
descriptionGraph :: Phantoms.TypedTerm Syntax.Description -> Phantoms.TypedTerm Syntax.Graph
descriptionGraph x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Description"),
        Core.projectionFieldName = (Core.Name "graph")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the subject field of hydra.rdf.syntax.Description
descriptionSubject :: Phantoms.TypedTerm Syntax.Description -> Phantoms.TypedTerm Syntax.Node
descriptionSubject x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Description"),
        Core.projectionFieldName = (Core.Name "subject")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the graph field of hydra.rdf.syntax.Description
descriptionWithGraph :: Phantoms.TypedTerm Syntax.Description -> Phantoms.TypedTerm Syntax.Graph -> Phantoms.TypedTerm Syntax.Description
descriptionWithGraph original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Description"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Description"),
              Core.projectionFieldName = (Core.Name "subject")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the subject field of hydra.rdf.syntax.Description
descriptionWithSubject :: Phantoms.TypedTerm Syntax.Description -> Phantoms.TypedTerm Syntax.Node -> Phantoms.TypedTerm Syntax.Description
descriptionWithSubject original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Description"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Description"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.rdf.syntax.Graph wrapper
graph :: Phantoms.TypedTerm (S.Set Syntax.Triple) -> Phantoms.TypedTerm Syntax.Graph
graph x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.Graph"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for the hydra.rdf.syntax.Iri wrapper
iri :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Syntax.Iri
iri x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.Iri"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL injection for the iri variant of hydra.rdf.syntax.IriOrLiteral
iriOrLiteralIri :: Phantoms.TypedTerm Syntax.Iri -> Phantoms.TypedTerm Syntax.IriOrLiteral
iriOrLiteralIri x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.IriOrLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iri"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the literal variant of hydra.rdf.syntax.IriOrLiteral
iriOrLiteralLiteral :: Phantoms.TypedTerm Syntax.Literal -> Phantoms.TypedTerm Syntax.IriOrLiteral
iriOrLiteralLiteral x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.IriOrLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for the hydra.rdf.syntax.LangStrings wrapper
langStrings :: Phantoms.TypedTerm (M.Map (Maybe Syntax.LanguageTag) String) -> Phantoms.TypedTerm Syntax.LangStrings
langStrings x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.LangStrings"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for the hydra.rdf.syntax.LanguageTag wrapper
languageTag :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Syntax.LanguageTag
languageTag x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.LanguageTag"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for hydra.rdf.syntax.Literal
literal :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Syntax.Iri -> Phantoms.TypedTerm (Maybe Syntax.LanguageTag) -> Phantoms.TypedTerm Syntax.Literal
literal lexicalForm datatypeIri languageTag =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lexicalForm"),
          Core.fieldTerm = (Phantoms.unTypedTerm lexicalForm)},
        Core.Field {
          Core.fieldName = (Core.Name "datatypeIri"),
          Core.fieldTerm = (Phantoms.unTypedTerm datatypeIri)},
        Core.Field {
          Core.fieldName = (Core.Name "languageTag"),
          Core.fieldTerm = (Phantoms.unTypedTerm languageTag)}]}))
-- | DSL accessor for the datatypeIri field of hydra.rdf.syntax.Literal
literalDatatypeIri :: Phantoms.TypedTerm Syntax.Literal -> Phantoms.TypedTerm Syntax.Iri
literalDatatypeIri x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
        Core.projectionFieldName = (Core.Name "datatypeIri")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the languageTag field of hydra.rdf.syntax.Literal
literalLanguageTag :: Phantoms.TypedTerm Syntax.Literal -> Phantoms.TypedTerm (Maybe Syntax.LanguageTag)
literalLanguageTag x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
        Core.projectionFieldName = (Core.Name "languageTag")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the lexicalForm field of hydra.rdf.syntax.Literal
literalLexicalForm :: Phantoms.TypedTerm Syntax.Literal -> Phantoms.TypedTerm String
literalLexicalForm x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
        Core.projectionFieldName = (Core.Name "lexicalForm")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the datatypeIri field of hydra.rdf.syntax.Literal
literalWithDatatypeIri :: Phantoms.TypedTerm Syntax.Literal -> Phantoms.TypedTerm Syntax.Iri -> Phantoms.TypedTerm Syntax.Literal
literalWithDatatypeIri original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lexicalForm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
              Core.projectionFieldName = (Core.Name "lexicalForm")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "datatypeIri"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "languageTag"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
              Core.projectionFieldName = (Core.Name "languageTag")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the languageTag field of hydra.rdf.syntax.Literal
literalWithLanguageTag :: Phantoms.TypedTerm Syntax.Literal -> Phantoms.TypedTerm (Maybe Syntax.LanguageTag) -> Phantoms.TypedTerm Syntax.Literal
literalWithLanguageTag original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lexicalForm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
              Core.projectionFieldName = (Core.Name "lexicalForm")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "datatypeIri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
              Core.projectionFieldName = (Core.Name "datatypeIri")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "languageTag"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the lexicalForm field of hydra.rdf.syntax.Literal
literalWithLexicalForm :: Phantoms.TypedTerm Syntax.Literal -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Syntax.Literal
literalWithLexicalForm original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lexicalForm"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "datatypeIri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
              Core.projectionFieldName = (Core.Name "datatypeIri")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "languageTag"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
              Core.projectionFieldName = (Core.Name "languageTag")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL injection for the bnode variant of hydra.rdf.syntax.Node
nodeBnode :: Phantoms.TypedTerm Syntax.BlankNode -> Phantoms.TypedTerm Syntax.Node
nodeBnode x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bnode"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the iri variant of hydra.rdf.syntax.Node
nodeIri :: Phantoms.TypedTerm Syntax.Iri -> Phantoms.TypedTerm Syntax.Node
nodeIri x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iri"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the literal variant of hydra.rdf.syntax.Node
nodeLiteral :: Phantoms.TypedTerm Syntax.Literal -> Phantoms.TypedTerm Syntax.Node
nodeLiteral x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for hydra.rdf.syntax.Property
property :: Phantoms.TypedTerm (S.Set Syntax.RdfsClass) -> Phantoms.TypedTerm (S.Set Syntax.RdfsClass) -> Phantoms.TypedTerm (S.Set Syntax.Property) -> Phantoms.TypedTerm Syntax.Property
property domain range subPropertyOf =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTypedTerm domain)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTypedTerm range)},
        Core.Field {
          Core.fieldName = (Core.Name "subPropertyOf"),
          Core.fieldTerm = (Phantoms.unTypedTerm subPropertyOf)}]}))
-- | DSL accessor for the domain field of hydra.rdf.syntax.Property
propertyDomain :: Phantoms.TypedTerm Syntax.Property -> Phantoms.TypedTerm (S.Set Syntax.RdfsClass)
propertyDomain x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
        Core.projectionFieldName = (Core.Name "domain")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the range field of hydra.rdf.syntax.Property
propertyRange :: Phantoms.TypedTerm Syntax.Property -> Phantoms.TypedTerm (S.Set Syntax.RdfsClass)
propertyRange x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
        Core.projectionFieldName = (Core.Name "range")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the subPropertyOf field of hydra.rdf.syntax.Property
propertySubPropertyOf :: Phantoms.TypedTerm Syntax.Property -> Phantoms.TypedTerm (S.Set Syntax.Property)
propertySubPropertyOf x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
        Core.projectionFieldName = (Core.Name "subPropertyOf")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the domain field of hydra.rdf.syntax.Property
propertyWithDomain :: Phantoms.TypedTerm Syntax.Property -> Phantoms.TypedTerm (S.Set Syntax.RdfsClass) -> Phantoms.TypedTerm Syntax.Property
propertyWithDomain original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
              Core.projectionFieldName = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subPropertyOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
              Core.projectionFieldName = (Core.Name "subPropertyOf")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the range field of hydra.rdf.syntax.Property
propertyWithRange :: Phantoms.TypedTerm Syntax.Property -> Phantoms.TypedTerm (S.Set Syntax.RdfsClass) -> Phantoms.TypedTerm Syntax.Property
propertyWithRange original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
              Core.projectionFieldName = (Core.Name "domain")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subPropertyOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
              Core.projectionFieldName = (Core.Name "subPropertyOf")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the subPropertyOf field of hydra.rdf.syntax.Property
propertyWithSubPropertyOf :: Phantoms.TypedTerm Syntax.Property -> Phantoms.TypedTerm (S.Set Syntax.Property) -> Phantoms.TypedTerm Syntax.Property
propertyWithSubPropertyOf original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
              Core.projectionFieldName = (Core.Name "domain")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
              Core.projectionFieldName = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subPropertyOf"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.rdf.syntax.Quad
quad :: Phantoms.TypedTerm Syntax.Resource -> Phantoms.TypedTerm Syntax.Iri -> Phantoms.TypedTerm Syntax.Node -> Phantoms.TypedTerm (Maybe Syntax.Iri) -> Phantoms.TypedTerm Syntax.Quad
quad subject predicate object graph =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTypedTerm subject)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTypedTerm predicate)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTypedTerm object)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTypedTerm graph)}]}))
-- | DSL accessor for the graph field of hydra.rdf.syntax.Quad
quadGraph :: Phantoms.TypedTerm Syntax.Quad -> Phantoms.TypedTerm (Maybe Syntax.Iri)
quadGraph x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
        Core.projectionFieldName = (Core.Name "graph")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the object field of hydra.rdf.syntax.Quad
quadObject :: Phantoms.TypedTerm Syntax.Quad -> Phantoms.TypedTerm Syntax.Node
quadObject x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
        Core.projectionFieldName = (Core.Name "object")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the predicate field of hydra.rdf.syntax.Quad
quadPredicate :: Phantoms.TypedTerm Syntax.Quad -> Phantoms.TypedTerm Syntax.Iri
quadPredicate x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
        Core.projectionFieldName = (Core.Name "predicate")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the subject field of hydra.rdf.syntax.Quad
quadSubject :: Phantoms.TypedTerm Syntax.Quad -> Phantoms.TypedTerm Syntax.Resource
quadSubject x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
        Core.projectionFieldName = (Core.Name "subject")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the graph field of hydra.rdf.syntax.Quad
quadWithGraph :: Phantoms.TypedTerm Syntax.Quad -> Phantoms.TypedTerm (Maybe Syntax.Iri) -> Phantoms.TypedTerm Syntax.Quad
quadWithGraph original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "subject")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "object")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the object field of hydra.rdf.syntax.Quad
quadWithObject :: Phantoms.TypedTerm Syntax.Quad -> Phantoms.TypedTerm Syntax.Node -> Phantoms.TypedTerm Syntax.Quad
quadWithObject original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "subject")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the predicate field of hydra.rdf.syntax.Quad
quadWithPredicate :: Phantoms.TypedTerm Syntax.Quad -> Phantoms.TypedTerm Syntax.Iri -> Phantoms.TypedTerm Syntax.Quad
quadWithPredicate original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "subject")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "object")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the subject field of hydra.rdf.syntax.Quad
quadWithSubject :: Phantoms.TypedTerm Syntax.Quad -> Phantoms.TypedTerm Syntax.Resource -> Phantoms.TypedTerm Syntax.Quad
quadWithSubject original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "object")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.rdf.syntax.RdfsClass wrapper
rdfsClass :: Phantoms.TypedTerm () -> Phantoms.TypedTerm Syntax.RdfsClass
rdfsClass x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.RdfsClass"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL injection for the bnode variant of hydra.rdf.syntax.Resource
resourceBnode :: Phantoms.TypedTerm Syntax.BlankNode -> Phantoms.TypedTerm Syntax.Resource
resourceBnode x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.Resource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bnode"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the iri variant of hydra.rdf.syntax.Resource
resourceIri :: Phantoms.TypedTerm Syntax.Iri -> Phantoms.TypedTerm Syntax.Resource
resourceIri x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.Resource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iri"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for hydra.rdf.syntax.Triple
triple :: Phantoms.TypedTerm Syntax.Resource -> Phantoms.TypedTerm Syntax.Iri -> Phantoms.TypedTerm Syntax.Node -> Phantoms.TypedTerm Syntax.Triple
triple subject predicate object =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTypedTerm subject)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTypedTerm predicate)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTypedTerm object)}]}))
-- | DSL accessor for the object field of hydra.rdf.syntax.Triple
tripleObject :: Phantoms.TypedTerm Syntax.Triple -> Phantoms.TypedTerm Syntax.Node
tripleObject x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
        Core.projectionFieldName = (Core.Name "object")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the predicate field of hydra.rdf.syntax.Triple
triplePredicate :: Phantoms.TypedTerm Syntax.Triple -> Phantoms.TypedTerm Syntax.Iri
triplePredicate x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
        Core.projectionFieldName = (Core.Name "predicate")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the subject field of hydra.rdf.syntax.Triple
tripleSubject :: Phantoms.TypedTerm Syntax.Triple -> Phantoms.TypedTerm Syntax.Resource
tripleSubject x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
        Core.projectionFieldName = (Core.Name "subject")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the object field of hydra.rdf.syntax.Triple
tripleWithObject :: Phantoms.TypedTerm Syntax.Triple -> Phantoms.TypedTerm Syntax.Node -> Phantoms.TypedTerm Syntax.Triple
tripleWithObject original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
              Core.projectionFieldName = (Core.Name "subject")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
              Core.projectionFieldName = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the predicate field of hydra.rdf.syntax.Triple
tripleWithPredicate :: Phantoms.TypedTerm Syntax.Triple -> Phantoms.TypedTerm Syntax.Iri -> Phantoms.TypedTerm Syntax.Triple
tripleWithPredicate original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
              Core.projectionFieldName = (Core.Name "subject")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
              Core.projectionFieldName = (Core.Name "object")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the subject field of hydra.rdf.syntax.Triple
tripleWithSubject :: Phantoms.TypedTerm Syntax.Triple -> Phantoms.TypedTerm Syntax.Resource -> Phantoms.TypedTerm Syntax.Triple
tripleWithSubject original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
              Core.projectionFieldName = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
              Core.projectionFieldName = (Core.Name "object")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL accessor for the body of hydra.rdf.syntax.BlankNode
unBlankNode :: Phantoms.TypedTerm Syntax.BlankNode -> Phantoms.TypedTerm String
unBlankNode x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.BlankNode")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.rdf.syntax.Dataset
unDataset :: Phantoms.TypedTerm Syntax.Dataset -> Phantoms.TypedTerm (S.Set Syntax.Quad)
unDataset x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.Dataset")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.rdf.syntax.Graph
unGraph :: Phantoms.TypedTerm Syntax.Graph -> Phantoms.TypedTerm (S.Set Syntax.Triple)
unGraph x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.Graph")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.rdf.syntax.Iri
unIri :: Phantoms.TypedTerm Syntax.Iri -> Phantoms.TypedTerm String
unIri x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.Iri")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.rdf.syntax.LangStrings
unLangStrings :: Phantoms.TypedTerm Syntax.LangStrings -> Phantoms.TypedTerm (M.Map (Maybe Syntax.LanguageTag) String)
unLangStrings x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.LangStrings")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.rdf.syntax.LanguageTag
unLanguageTag :: Phantoms.TypedTerm Syntax.LanguageTag -> Phantoms.TypedTerm String
unLanguageTag x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.LanguageTag")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.rdf.syntax.RdfsClass
unRdfsClass :: Phantoms.TypedTerm Syntax.RdfsClass -> Phantoms.TypedTerm ()
unRdfsClass x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.RdfsClass")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
