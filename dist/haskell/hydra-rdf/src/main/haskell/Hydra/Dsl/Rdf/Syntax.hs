-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.rdf.syntax

module Hydra.Dsl.Rdf.Syntax where
import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Rdf.Syntax as Syntax
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | DSL constructor for the hydra.rdf.syntax.BlankNode wrapper
blankNode :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.BlankNode
blankNode x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.BlankNode"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.rdf.syntax.Dataset wrapper
dataset :: Phantoms.TTerm (S.Set Syntax.Quad) -> Phantoms.TTerm Syntax.Dataset
dataset x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.Dataset"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.rdf.syntax.Description
description :: Phantoms.TTerm Syntax.Node -> Phantoms.TTerm Syntax.Graph -> Phantoms.TTerm Syntax.Description
description subject graph =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Description"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTTerm subject)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm graph)}]}))
-- | DSL accessor for the graph field of hydra.rdf.syntax.Description
descriptionGraph :: Phantoms.TTerm Syntax.Description -> Phantoms.TTerm Syntax.Graph
descriptionGraph x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Description"),
        Core.projectionField = (Core.Name "graph")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the subject field of hydra.rdf.syntax.Description
descriptionSubject :: Phantoms.TTerm Syntax.Description -> Phantoms.TTerm Syntax.Node
descriptionSubject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Description"),
        Core.projectionField = (Core.Name "subject")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the graph field of hydra.rdf.syntax.Description
descriptionWithGraph :: Phantoms.TTerm Syntax.Description -> Phantoms.TTerm Syntax.Graph -> Phantoms.TTerm Syntax.Description
descriptionWithGraph original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Description"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Description"),
              Core.projectionField = (Core.Name "subject")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the subject field of hydra.rdf.syntax.Description
descriptionWithSubject :: Phantoms.TTerm Syntax.Description -> Phantoms.TTerm Syntax.Node -> Phantoms.TTerm Syntax.Description
descriptionWithSubject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Description"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Description"),
              Core.projectionField = (Core.Name "graph")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.rdf.syntax.Graph wrapper
graph :: Phantoms.TTerm (S.Set Syntax.Triple) -> Phantoms.TTerm Syntax.Graph
graph x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.Graph"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.rdf.syntax.Iri wrapper
iri :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Iri
iri x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.Iri"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the iri variant of hydra.rdf.syntax.IriOrLiteral
iriOrLiteralIri :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.IriOrLiteral
iriOrLiteralIri x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.IriOrLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iri"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the literal variant of hydra.rdf.syntax.IriOrLiteral
iriOrLiteralLiteral :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.IriOrLiteral
iriOrLiteralLiteral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.IriOrLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.rdf.syntax.LangStrings wrapper
langStrings :: Phantoms.TTerm (M.Map (Maybe Syntax.LanguageTag) String) -> Phantoms.TTerm Syntax.LangStrings
langStrings x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.LangStrings"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.rdf.syntax.LanguageTag wrapper
languageTag :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.LanguageTag
languageTag x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.LanguageTag"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.rdf.syntax.Literal
literal :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm (Maybe Syntax.LanguageTag) -> Phantoms.TTerm Syntax.Literal
literal lexicalForm datatypeIri languageTag =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lexicalForm"),
          Core.fieldTerm = (Phantoms.unTTerm lexicalForm)},
        Core.Field {
          Core.fieldName = (Core.Name "datatypeIri"),
          Core.fieldTerm = (Phantoms.unTTerm datatypeIri)},
        Core.Field {
          Core.fieldName = (Core.Name "languageTag"),
          Core.fieldTerm = (Phantoms.unTTerm languageTag)}]}))
-- | DSL accessor for the datatypeIri field of hydra.rdf.syntax.Literal
literalDatatypeIri :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.Iri
literalDatatypeIri x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
        Core.projectionField = (Core.Name "datatypeIri")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the languageTag field of hydra.rdf.syntax.Literal
literalLanguageTag :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm (Maybe Syntax.LanguageTag)
literalLanguageTag x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
        Core.projectionField = (Core.Name "languageTag")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the lexicalForm field of hydra.rdf.syntax.Literal
literalLexicalForm :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm String
literalLexicalForm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
        Core.projectionField = (Core.Name "lexicalForm")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the datatypeIri field of hydra.rdf.syntax.Literal
literalWithDatatypeIri :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.Literal
literalWithDatatypeIri original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lexicalForm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
              Core.projectionField = (Core.Name "lexicalForm")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "datatypeIri"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "languageTag"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
              Core.projectionField = (Core.Name "languageTag")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the languageTag field of hydra.rdf.syntax.Literal
literalWithLanguageTag :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm (Maybe Syntax.LanguageTag) -> Phantoms.TTerm Syntax.Literal
literalWithLanguageTag original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lexicalForm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
              Core.projectionField = (Core.Name "lexicalForm")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "datatypeIri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
              Core.projectionField = (Core.Name "datatypeIri")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "languageTag"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the lexicalForm field of hydra.rdf.syntax.Literal
literalWithLexicalForm :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.Literal
literalWithLexicalForm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lexicalForm"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "datatypeIri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
              Core.projectionField = (Core.Name "datatypeIri")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "languageTag"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
              Core.projectionField = (Core.Name "languageTag")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the bnode variant of hydra.rdf.syntax.Node
nodeBnode :: Phantoms.TTerm Syntax.BlankNode -> Phantoms.TTerm Syntax.Node
nodeBnode x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bnode"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the iri variant of hydra.rdf.syntax.Node
nodeIri :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.Node
nodeIri x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iri"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the literal variant of hydra.rdf.syntax.Node
nodeLiteral :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.Node
nodeLiteral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.rdf.syntax.Property
property :: Phantoms.TTerm (S.Set Syntax.RdfsClass) -> Phantoms.TTerm (S.Set Syntax.RdfsClass) -> Phantoms.TTerm (S.Set Syntax.Property) -> Phantoms.TTerm Syntax.Property
property domain range subPropertyOf =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm domain)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm range)},
        Core.Field {
          Core.fieldName = (Core.Name "subPropertyOf"),
          Core.fieldTerm = (Phantoms.unTTerm subPropertyOf)}]}))
-- | DSL accessor for the domain field of hydra.rdf.syntax.Property
propertyDomain :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm (S.Set Syntax.RdfsClass)
propertyDomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
        Core.projectionField = (Core.Name "domain")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the range field of hydra.rdf.syntax.Property
propertyRange :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm (S.Set Syntax.RdfsClass)
propertyRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
        Core.projectionField = (Core.Name "range")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the subPropertyOf field of hydra.rdf.syntax.Property
propertySubPropertyOf :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm (S.Set Syntax.Property)
propertySubPropertyOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
        Core.projectionField = (Core.Name "subPropertyOf")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the domain field of hydra.rdf.syntax.Property
propertyWithDomain :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm (S.Set Syntax.RdfsClass) -> Phantoms.TTerm Syntax.Property
propertyWithDomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
              Core.projectionField = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subPropertyOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
              Core.projectionField = (Core.Name "subPropertyOf")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the range field of hydra.rdf.syntax.Property
propertyWithRange :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm (S.Set Syntax.RdfsClass) -> Phantoms.TTerm Syntax.Property
propertyWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
              Core.projectionField = (Core.Name "domain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subPropertyOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
              Core.projectionField = (Core.Name "subPropertyOf")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the subPropertyOf field of hydra.rdf.syntax.Property
propertyWithSubPropertyOf :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm (S.Set Syntax.Property) -> Phantoms.TTerm Syntax.Property
propertyWithSubPropertyOf original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
              Core.projectionField = (Core.Name "domain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
              Core.projectionField = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subPropertyOf"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.rdf.syntax.Quad
quad :: Phantoms.TTerm Syntax.Resource -> Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.Node -> Phantoms.TTerm (Maybe Syntax.Iri) -> Phantoms.TTerm Syntax.Quad
quad subject predicate object graph =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTTerm subject)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTTerm predicate)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm object)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm graph)}]}))
-- | DSL accessor for the graph field of hydra.rdf.syntax.Quad
quadGraph :: Phantoms.TTerm Syntax.Quad -> Phantoms.TTerm (Maybe Syntax.Iri)
quadGraph x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
        Core.projectionField = (Core.Name "graph")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the object field of hydra.rdf.syntax.Quad
quadObject :: Phantoms.TTerm Syntax.Quad -> Phantoms.TTerm Syntax.Node
quadObject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
        Core.projectionField = (Core.Name "object")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the predicate field of hydra.rdf.syntax.Quad
quadPredicate :: Phantoms.TTerm Syntax.Quad -> Phantoms.TTerm Syntax.Iri
quadPredicate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
        Core.projectionField = (Core.Name "predicate")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the subject field of hydra.rdf.syntax.Quad
quadSubject :: Phantoms.TTerm Syntax.Quad -> Phantoms.TTerm Syntax.Resource
quadSubject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
        Core.projectionField = (Core.Name "subject")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the graph field of hydra.rdf.syntax.Quad
quadWithGraph :: Phantoms.TTerm Syntax.Quad -> Phantoms.TTerm (Maybe Syntax.Iri) -> Phantoms.TTerm Syntax.Quad
quadWithGraph original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "subject")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "object")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the object field of hydra.rdf.syntax.Quad
quadWithObject :: Phantoms.TTerm Syntax.Quad -> Phantoms.TTerm Syntax.Node -> Phantoms.TTerm Syntax.Quad
quadWithObject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "subject")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "graph")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the predicate field of hydra.rdf.syntax.Quad
quadWithPredicate :: Phantoms.TTerm Syntax.Quad -> Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.Quad
quadWithPredicate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "subject")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "object")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "graph")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the subject field of hydra.rdf.syntax.Quad
quadWithSubject :: Phantoms.TTerm Syntax.Quad -> Phantoms.TTerm Syntax.Resource -> Phantoms.TTerm Syntax.Quad
quadWithSubject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "object")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "graph")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.rdf.syntax.RdfsClass wrapper
rdfsClass :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.RdfsClass
rdfsClass x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.RdfsClass"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the bnode variant of hydra.rdf.syntax.Resource
resourceBnode :: Phantoms.TTerm Syntax.BlankNode -> Phantoms.TTerm Syntax.Resource
resourceBnode x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.Resource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bnode"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the iri variant of hydra.rdf.syntax.Resource
resourceIri :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.Resource
resourceIri x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.Resource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iri"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.rdf.syntax.Triple
triple :: Phantoms.TTerm Syntax.Resource -> Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.Node -> Phantoms.TTerm Syntax.Triple
triple subject predicate object =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTTerm subject)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTTerm predicate)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm object)}]}))
-- | DSL accessor for the object field of hydra.rdf.syntax.Triple
tripleObject :: Phantoms.TTerm Syntax.Triple -> Phantoms.TTerm Syntax.Node
tripleObject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
        Core.projectionField = (Core.Name "object")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the predicate field of hydra.rdf.syntax.Triple
triplePredicate :: Phantoms.TTerm Syntax.Triple -> Phantoms.TTerm Syntax.Iri
triplePredicate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
        Core.projectionField = (Core.Name "predicate")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the subject field of hydra.rdf.syntax.Triple
tripleSubject :: Phantoms.TTerm Syntax.Triple -> Phantoms.TTerm Syntax.Resource
tripleSubject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
        Core.projectionField = (Core.Name "subject")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the object field of hydra.rdf.syntax.Triple
tripleWithObject :: Phantoms.TTerm Syntax.Triple -> Phantoms.TTerm Syntax.Node -> Phantoms.TTerm Syntax.Triple
tripleWithObject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
              Core.projectionField = (Core.Name "subject")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
              Core.projectionField = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the predicate field of hydra.rdf.syntax.Triple
tripleWithPredicate :: Phantoms.TTerm Syntax.Triple -> Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.Triple
tripleWithPredicate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
              Core.projectionField = (Core.Name "subject")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
              Core.projectionField = (Core.Name "object")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the subject field of hydra.rdf.syntax.Triple
tripleWithSubject :: Phantoms.TTerm Syntax.Triple -> Phantoms.TTerm Syntax.Resource -> Phantoms.TTerm Syntax.Triple
tripleWithSubject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
              Core.projectionField = (Core.Name "predicate")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
              Core.projectionField = (Core.Name "object")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL accessor for the body of hydra.rdf.syntax.BlankNode
unBlankNode :: Phantoms.TTerm Syntax.BlankNode -> Phantoms.TTerm String
unBlankNode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.BlankNode")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.rdf.syntax.Dataset
unDataset :: Phantoms.TTerm Syntax.Dataset -> Phantoms.TTerm (S.Set Syntax.Quad)
unDataset x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.Dataset")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.rdf.syntax.Graph
unGraph :: Phantoms.TTerm Syntax.Graph -> Phantoms.TTerm (S.Set Syntax.Triple)
unGraph x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.Graph")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.rdf.syntax.Iri
unIri :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm String
unIri x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.Iri")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.rdf.syntax.LangStrings
unLangStrings :: Phantoms.TTerm Syntax.LangStrings -> Phantoms.TTerm (M.Map (Maybe Syntax.LanguageTag) String)
unLangStrings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.LangStrings")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.rdf.syntax.LanguageTag
unLanguageTag :: Phantoms.TTerm Syntax.LanguageTag -> Phantoms.TTerm String
unLanguageTag x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.LanguageTag")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.rdf.syntax.RdfsClass
unRdfsClass :: Phantoms.TTerm Syntax.RdfsClass -> Phantoms.TTerm ()
unRdfsClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.RdfsClass")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
