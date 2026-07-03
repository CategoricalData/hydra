-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.rdf.syntax

module Hydra.Dsl.Rdf.Syntax where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Rdf.Syntax as Syntax
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | DSL constructor for the hydra.rdf.syntax.BlankNode wrapper
blankNode :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.BlankNode
blankNode x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.BlankNode"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.rdf.syntax.Dataset wrapper
dataset :: Typed.TypedTerm (S.Set Syntax.Quad) -> Typed.TypedTerm Syntax.Dataset
dataset x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.Dataset"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.rdf.syntax.Description
description :: Typed.TypedTerm Syntax.Node -> Typed.TypedTerm Syntax.Graph -> Typed.TypedTerm Syntax.Description
description subject graph =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Description"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Typed.unTypedTerm subject)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Typed.unTypedTerm graph)}]}))
-- | DSL accessor for the graph field of hydra.rdf.syntax.Description
descriptionGraph :: Typed.TypedTerm Syntax.Description -> Typed.TypedTerm Syntax.Graph
descriptionGraph x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Description"),
        Core.projectionFieldName = (Core.Name "graph")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the subject field of hydra.rdf.syntax.Description
descriptionSubject :: Typed.TypedTerm Syntax.Description -> Typed.TypedTerm Syntax.Node
descriptionSubject x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Description"),
        Core.projectionFieldName = (Core.Name "subject")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the graph field of hydra.rdf.syntax.Description
descriptionWithGraph :: Typed.TypedTerm Syntax.Description -> Typed.TypedTerm Syntax.Graph -> Typed.TypedTerm Syntax.Description
descriptionWithGraph original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Description"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Description"),
              Core.projectionFieldName = (Core.Name "subject")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the subject field of hydra.rdf.syntax.Description
descriptionWithSubject :: Typed.TypedTerm Syntax.Description -> Typed.TypedTerm Syntax.Node -> Typed.TypedTerm Syntax.Description
descriptionWithSubject original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Description"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Description"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.rdf.syntax.Graph wrapper
graph :: Typed.TypedTerm (S.Set Syntax.Triple) -> Typed.TypedTerm Syntax.Graph
graph x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.Graph"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.rdf.syntax.Iri wrapper
iri :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Iri
iri x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.Iri"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the iri variant of hydra.rdf.syntax.IriOrLiteral
iriOrLiteralIri :: Typed.TypedTerm Syntax.Iri -> Typed.TypedTerm Syntax.IriOrLiteral
iriOrLiteralIri x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.IriOrLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iri"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the literal variant of hydra.rdf.syntax.IriOrLiteral
iriOrLiteralLiteral :: Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm Syntax.IriOrLiteral
iriOrLiteralLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.IriOrLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.rdf.syntax.LangStrings wrapper
langStrings :: Typed.TypedTerm (M.Map (Maybe Syntax.LanguageTag) String) -> Typed.TypedTerm Syntax.LangStrings
langStrings x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.LangStrings"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.rdf.syntax.LanguageTag wrapper
languageTag :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.LanguageTag
languageTag x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.LanguageTag"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.rdf.syntax.Literal
literal :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Iri -> Typed.TypedTerm (Maybe Syntax.LanguageTag) -> Typed.TypedTerm Syntax.Literal
literal lexicalForm datatypeIri languageTag =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lexicalForm"),
          Core.fieldTerm = (Typed.unTypedTerm lexicalForm)},
        Core.Field {
          Core.fieldName = (Core.Name "datatypeIri"),
          Core.fieldTerm = (Typed.unTypedTerm datatypeIri)},
        Core.Field {
          Core.fieldName = (Core.Name "languageTag"),
          Core.fieldTerm = (Typed.unTypedTerm languageTag)}]}))
-- | DSL accessor for the datatypeIri field of hydra.rdf.syntax.Literal
literalDatatypeIri :: Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm Syntax.Iri
literalDatatypeIri x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
        Core.projectionFieldName = (Core.Name "datatypeIri")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the languageTag field of hydra.rdf.syntax.Literal
literalLanguageTag :: Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm (Maybe Syntax.LanguageTag)
literalLanguageTag x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
        Core.projectionFieldName = (Core.Name "languageTag")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the lexicalForm field of hydra.rdf.syntax.Literal
literalLexicalForm :: Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm String
literalLexicalForm x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
        Core.projectionFieldName = (Core.Name "lexicalForm")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the datatypeIri field of hydra.rdf.syntax.Literal
literalWithDatatypeIri :: Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm Syntax.Iri -> Typed.TypedTerm Syntax.Literal
literalWithDatatypeIri original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lexicalForm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
              Core.projectionFieldName = (Core.Name "lexicalForm")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "datatypeIri"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "languageTag"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
              Core.projectionFieldName = (Core.Name "languageTag")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the languageTag field of hydra.rdf.syntax.Literal
literalWithLanguageTag :: Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm (Maybe Syntax.LanguageTag) -> Typed.TypedTerm Syntax.Literal
literalWithLanguageTag original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lexicalForm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
              Core.projectionFieldName = (Core.Name "lexicalForm")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "datatypeIri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
              Core.projectionFieldName = (Core.Name "datatypeIri")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "languageTag"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the lexicalForm field of hydra.rdf.syntax.Literal
literalWithLexicalForm :: Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm String -> Typed.TypedTerm Syntax.Literal
literalWithLexicalForm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lexicalForm"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "datatypeIri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
              Core.projectionFieldName = (Core.Name "datatypeIri")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "languageTag"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Literal"),
              Core.projectionFieldName = (Core.Name "languageTag")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the bnode variant of hydra.rdf.syntax.Node
nodeBnode :: Typed.TypedTerm Syntax.BlankNode -> Typed.TypedTerm Syntax.Node
nodeBnode x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bnode"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the iri variant of hydra.rdf.syntax.Node
nodeIri :: Typed.TypedTerm Syntax.Iri -> Typed.TypedTerm Syntax.Node
nodeIri x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iri"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the literal variant of hydra.rdf.syntax.Node
nodeLiteral :: Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm Syntax.Node
nodeLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.rdf.syntax.Property
property :: Typed.TypedTerm (S.Set Syntax.RdfsClass) -> Typed.TypedTerm (S.Set Syntax.RdfsClass) -> Typed.TypedTerm (S.Set Syntax.Property) -> Typed.TypedTerm Syntax.Property
property domain range subPropertyOf =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Typed.unTypedTerm domain)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Typed.unTypedTerm range)},
        Core.Field {
          Core.fieldName = (Core.Name "subPropertyOf"),
          Core.fieldTerm = (Typed.unTypedTerm subPropertyOf)}]}))
-- | DSL accessor for the domain field of hydra.rdf.syntax.Property
propertyDomain :: Typed.TypedTerm Syntax.Property -> Typed.TypedTerm (S.Set Syntax.RdfsClass)
propertyDomain x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
        Core.projectionFieldName = (Core.Name "domain")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the range field of hydra.rdf.syntax.Property
propertyRange :: Typed.TypedTerm Syntax.Property -> Typed.TypedTerm (S.Set Syntax.RdfsClass)
propertyRange x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
        Core.projectionFieldName = (Core.Name "range")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the subPropertyOf field of hydra.rdf.syntax.Property
propertySubPropertyOf :: Typed.TypedTerm Syntax.Property -> Typed.TypedTerm (S.Set Syntax.Property)
propertySubPropertyOf x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
        Core.projectionFieldName = (Core.Name "subPropertyOf")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the domain field of hydra.rdf.syntax.Property
propertyWithDomain :: Typed.TypedTerm Syntax.Property -> Typed.TypedTerm (S.Set Syntax.RdfsClass) -> Typed.TypedTerm Syntax.Property
propertyWithDomain original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
              Core.projectionFieldName = (Core.Name "range")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subPropertyOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
              Core.projectionFieldName = (Core.Name "subPropertyOf")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the range field of hydra.rdf.syntax.Property
propertyWithRange :: Typed.TypedTerm Syntax.Property -> Typed.TypedTerm (S.Set Syntax.RdfsClass) -> Typed.TypedTerm Syntax.Property
propertyWithRange original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
              Core.projectionFieldName = (Core.Name "domain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subPropertyOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
              Core.projectionFieldName = (Core.Name "subPropertyOf")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the subPropertyOf field of hydra.rdf.syntax.Property
propertyWithSubPropertyOf :: Typed.TypedTerm Syntax.Property -> Typed.TypedTerm (S.Set Syntax.Property) -> Typed.TypedTerm Syntax.Property
propertyWithSubPropertyOf original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
              Core.projectionFieldName = (Core.Name "domain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Property"),
              Core.projectionFieldName = (Core.Name "range")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subPropertyOf"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.rdf.syntax.Quad
quad :: Typed.TypedTerm Syntax.Resource -> Typed.TypedTerm Syntax.Iri -> Typed.TypedTerm Syntax.Node -> Typed.TypedTerm (Maybe Syntax.Iri) -> Typed.TypedTerm Syntax.Quad
quad subject predicate object graph =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Typed.unTypedTerm subject)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Typed.unTypedTerm predicate)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Typed.unTypedTerm object)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Typed.unTypedTerm graph)}]}))
-- | DSL accessor for the graph field of hydra.rdf.syntax.Quad
quadGraph :: Typed.TypedTerm Syntax.Quad -> Typed.TypedTerm (Maybe Syntax.Iri)
quadGraph x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
        Core.projectionFieldName = (Core.Name "graph")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the object field of hydra.rdf.syntax.Quad
quadObject :: Typed.TypedTerm Syntax.Quad -> Typed.TypedTerm Syntax.Node
quadObject x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
        Core.projectionFieldName = (Core.Name "object")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the predicate field of hydra.rdf.syntax.Quad
quadPredicate :: Typed.TypedTerm Syntax.Quad -> Typed.TypedTerm Syntax.Iri
quadPredicate x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
        Core.projectionFieldName = (Core.Name "predicate")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the subject field of hydra.rdf.syntax.Quad
quadSubject :: Typed.TypedTerm Syntax.Quad -> Typed.TypedTerm Syntax.Resource
quadSubject x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
        Core.projectionFieldName = (Core.Name "subject")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the graph field of hydra.rdf.syntax.Quad
quadWithGraph :: Typed.TypedTerm Syntax.Quad -> Typed.TypedTerm (Maybe Syntax.Iri) -> Typed.TypedTerm Syntax.Quad
quadWithGraph original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "subject")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "predicate")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "object")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the object field of hydra.rdf.syntax.Quad
quadWithObject :: Typed.TypedTerm Syntax.Quad -> Typed.TypedTerm Syntax.Node -> Typed.TypedTerm Syntax.Quad
quadWithObject original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "subject")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "predicate")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the predicate field of hydra.rdf.syntax.Quad
quadWithPredicate :: Typed.TypedTerm Syntax.Quad -> Typed.TypedTerm Syntax.Iri -> Typed.TypedTerm Syntax.Quad
quadWithPredicate original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "subject")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "object")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the subject field of hydra.rdf.syntax.Quad
quadWithSubject :: Typed.TypedTerm Syntax.Quad -> Typed.TypedTerm Syntax.Resource -> Typed.TypedTerm Syntax.Quad
quadWithSubject original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "predicate")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "object")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Quad"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.rdf.syntax.RdfsClass wrapper
rdfsClass :: Typed.TypedTerm () -> Typed.TypedTerm Syntax.RdfsClass
rdfsClass x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.rdf.syntax.RdfsClass"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the bnode variant of hydra.rdf.syntax.Resource
resourceBnode :: Typed.TypedTerm Syntax.BlankNode -> Typed.TypedTerm Syntax.Resource
resourceBnode x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.Resource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bnode"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the iri variant of hydra.rdf.syntax.Resource
resourceIri :: Typed.TypedTerm Syntax.Iri -> Typed.TypedTerm Syntax.Resource
resourceIri x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.rdf.syntax.Resource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iri"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.rdf.syntax.Triple
triple :: Typed.TypedTerm Syntax.Resource -> Typed.TypedTerm Syntax.Iri -> Typed.TypedTerm Syntax.Node -> Typed.TypedTerm Syntax.Triple
triple subject predicate object =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Typed.unTypedTerm subject)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Typed.unTypedTerm predicate)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Typed.unTypedTerm object)}]}))
-- | DSL accessor for the object field of hydra.rdf.syntax.Triple
tripleObject :: Typed.TypedTerm Syntax.Triple -> Typed.TypedTerm Syntax.Node
tripleObject x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
        Core.projectionFieldName = (Core.Name "object")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the predicate field of hydra.rdf.syntax.Triple
triplePredicate :: Typed.TypedTerm Syntax.Triple -> Typed.TypedTerm Syntax.Iri
triplePredicate x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
        Core.projectionFieldName = (Core.Name "predicate")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the subject field of hydra.rdf.syntax.Triple
tripleSubject :: Typed.TypedTerm Syntax.Triple -> Typed.TypedTerm Syntax.Resource
tripleSubject x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
        Core.projectionFieldName = (Core.Name "subject")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the object field of hydra.rdf.syntax.Triple
tripleWithObject :: Typed.TypedTerm Syntax.Triple -> Typed.TypedTerm Syntax.Node -> Typed.TypedTerm Syntax.Triple
tripleWithObject original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
              Core.projectionFieldName = (Core.Name "subject")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
              Core.projectionFieldName = (Core.Name "predicate")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the predicate field of hydra.rdf.syntax.Triple
tripleWithPredicate :: Typed.TypedTerm Syntax.Triple -> Typed.TypedTerm Syntax.Iri -> Typed.TypedTerm Syntax.Triple
tripleWithPredicate original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
              Core.projectionFieldName = (Core.Name "subject")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
              Core.projectionFieldName = (Core.Name "object")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the subject field of hydra.rdf.syntax.Triple
tripleWithSubject :: Typed.TypedTerm Syntax.Triple -> Typed.TypedTerm Syntax.Resource -> Typed.TypedTerm Syntax.Triple
tripleWithSubject original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
              Core.projectionFieldName = (Core.Name "predicate")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.rdf.syntax.Triple"),
              Core.projectionFieldName = (Core.Name "object")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL accessor for the body of hydra.rdf.syntax.BlankNode
unBlankNode :: Typed.TypedTerm Syntax.BlankNode -> Typed.TypedTerm String
unBlankNode x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.BlankNode")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.rdf.syntax.Dataset
unDataset :: Typed.TypedTerm Syntax.Dataset -> Typed.TypedTerm (S.Set Syntax.Quad)
unDataset x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.Dataset")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.rdf.syntax.Graph
unGraph :: Typed.TypedTerm Syntax.Graph -> Typed.TypedTerm (S.Set Syntax.Triple)
unGraph x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.Graph")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.rdf.syntax.Iri
unIri :: Typed.TypedTerm Syntax.Iri -> Typed.TypedTerm String
unIri x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.Iri")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.rdf.syntax.LangStrings
unLangStrings :: Typed.TypedTerm Syntax.LangStrings -> Typed.TypedTerm (M.Map (Maybe Syntax.LanguageTag) String)
unLangStrings x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.LangStrings")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.rdf.syntax.LanguageTag
unLanguageTag :: Typed.TypedTerm Syntax.LanguageTag -> Typed.TypedTerm String
unLanguageTag x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.LanguageTag")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.rdf.syntax.RdfsClass
unRdfsClass :: Typed.TypedTerm Syntax.RdfsClass -> Typed.TypedTerm ()
unRdfsClass x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.rdf.syntax.RdfsClass")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
