-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.org.w3.rdf.syntax

module Hydra.Dsl.Ext.Org.W3.Rdf.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Org.W3.Rdf.Syntax as Syntax
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

blankNode :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.BlankNode
blankNode x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.BlankNode"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unBlankNode :: Phantoms.TTerm Syntax.BlankNode -> Phantoms.TTerm String
unBlankNode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.rdf.syntax.BlankNode")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rdfsClass :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.RdfsClass
rdfsClass x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.RdfsClass"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unRdfsClass :: Phantoms.TTerm Syntax.RdfsClass -> Phantoms.TTerm ()
unRdfsClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.rdf.syntax.RdfsClass")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataset :: Phantoms.TTerm (S.Set Syntax.Quad) -> Phantoms.TTerm Syntax.Dataset
dataset x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Dataset"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unDataset :: Phantoms.TTerm Syntax.Dataset -> Phantoms.TTerm (S.Set Syntax.Quad)
unDataset x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.rdf.syntax.Dataset")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

description :: Phantoms.TTerm Syntax.Node -> Phantoms.TTerm Syntax.Graph -> Phantoms.TTerm Syntax.Description
description subject graph =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Description"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTTerm subject)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm graph)}]}))

descriptionSubject :: Phantoms.TTerm Syntax.Description -> Phantoms.TTerm Syntax.Node
descriptionSubject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Description"),
        Core.projectionField = (Core.Name "subject")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

descriptionGraph :: Phantoms.TTerm Syntax.Description -> Phantoms.TTerm Syntax.Graph
descriptionGraph x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Description"),
        Core.projectionField = (Core.Name "graph")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

descriptionWithSubject :: Phantoms.TTerm Syntax.Description -> Phantoms.TTerm Syntax.Node -> Phantoms.TTerm Syntax.Description
descriptionWithSubject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Description"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Description"),
              Core.projectionField = (Core.Name "graph")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

descriptionWithGraph :: Phantoms.TTerm Syntax.Description -> Phantoms.TTerm Syntax.Graph -> Phantoms.TTerm Syntax.Description
descriptionWithGraph original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Description"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Description"),
              Core.projectionField = (Core.Name "subject")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

graph :: Phantoms.TTerm (S.Set Syntax.Triple) -> Phantoms.TTerm Syntax.Graph
graph x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Graph"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unGraph :: Phantoms.TTerm Syntax.Graph -> Phantoms.TTerm (S.Set Syntax.Triple)
unGraph x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.rdf.syntax.Graph")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

iri :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Iri
iri x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Iri"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unIri :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm String
unIri x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.rdf.syntax.Iri")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

iriOrLiteralIri :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.IriOrLiteral
iriOrLiteralIri x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.IriOrLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iri"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

iriOrLiteralLiteral :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.IriOrLiteral
iriOrLiteralLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.IriOrLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

langStrings :: Phantoms.TTerm (M.Map (Maybe Syntax.LanguageTag) String) -> Phantoms.TTerm Syntax.LangStrings
langStrings x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.LangStrings"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unLangStrings :: Phantoms.TTerm Syntax.LangStrings -> Phantoms.TTerm (M.Map (Maybe Syntax.LanguageTag) String)
unLangStrings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.rdf.syntax.LangStrings")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

languageTag :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.LanguageTag
languageTag x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.LanguageTag"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unLanguageTag :: Phantoms.TTerm Syntax.LanguageTag -> Phantoms.TTerm String
unLanguageTag x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.w3.rdf.syntax.LanguageTag")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

literal :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm (Maybe Syntax.LanguageTag) -> Phantoms.TTerm Syntax.Literal
literal lexicalForm datatypeIri languageTag =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Literal"),
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

literalLexicalForm :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm String
literalLexicalForm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Literal"),
        Core.projectionField = (Core.Name "lexicalForm")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

literalDatatypeIri :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.Iri
literalDatatypeIri x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Literal"),
        Core.projectionField = (Core.Name "datatypeIri")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

literalLanguageTag :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm (Maybe Syntax.LanguageTag)
literalLanguageTag x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Literal"),
        Core.projectionField = (Core.Name "languageTag")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

literalWithLexicalForm :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.Literal
literalWithLexicalForm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Literal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lexicalForm"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "datatypeIri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Literal"),
              Core.projectionField = (Core.Name "datatypeIri")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "languageTag"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Literal"),
              Core.projectionField = (Core.Name "languageTag")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

literalWithDatatypeIri :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.Literal
literalWithDatatypeIri original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Literal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lexicalForm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Literal"),
              Core.projectionField = (Core.Name "lexicalForm")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "datatypeIri"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "languageTag"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Literal"),
              Core.projectionField = (Core.Name "languageTag")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

literalWithLanguageTag :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm (Maybe Syntax.LanguageTag) -> Phantoms.TTerm Syntax.Literal
literalWithLanguageTag original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Literal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lexicalForm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Literal"),
              Core.projectionField = (Core.Name "lexicalForm")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "datatypeIri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Literal"),
              Core.projectionField = (Core.Name "datatypeIri")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "languageTag"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

nodeIri :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.Node
nodeIri x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iri"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nodeBnode :: Phantoms.TTerm Syntax.BlankNode -> Phantoms.TTerm Syntax.Node
nodeBnode x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bnode"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nodeLiteral :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.Node
nodeLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Node"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

property :: Phantoms.TTerm (S.Set Syntax.RdfsClass) -> Phantoms.TTerm (S.Set Syntax.RdfsClass) -> Phantoms.TTerm (S.Set Syntax.Property) -> Phantoms.TTerm Syntax.Property
property domain range subPropertyOf =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Property"),
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

propertyDomain :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm (S.Set Syntax.RdfsClass)
propertyDomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Property"),
        Core.projectionField = (Core.Name "domain")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyRange :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm (S.Set Syntax.RdfsClass)
propertyRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Property"),
        Core.projectionField = (Core.Name "range")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertySubPropertyOf :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm (S.Set Syntax.Property)
propertySubPropertyOf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Property"),
        Core.projectionField = (Core.Name "subPropertyOf")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyWithDomain :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm (S.Set Syntax.RdfsClass) -> Phantoms.TTerm Syntax.Property
propertyWithDomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Property"),
              Core.projectionField = (Core.Name "range")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subPropertyOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Property"),
              Core.projectionField = (Core.Name "subPropertyOf")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyWithRange :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm (S.Set Syntax.RdfsClass) -> Phantoms.TTerm Syntax.Property
propertyWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Property"),
              Core.projectionField = (Core.Name "domain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subPropertyOf"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Property"),
              Core.projectionField = (Core.Name "subPropertyOf")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyWithSubPropertyOf :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm (S.Set Syntax.Property) -> Phantoms.TTerm Syntax.Property
propertyWithSubPropertyOf original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Property"),
              Core.projectionField = (Core.Name "domain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Property"),
              Core.projectionField = (Core.Name "range")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subPropertyOf"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

quad :: Phantoms.TTerm Syntax.Resource -> Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.Node -> Phantoms.TTerm (Maybe Syntax.Iri) -> Phantoms.TTerm Syntax.Quad
quad subject predicate object graph =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
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

quadSubject :: Phantoms.TTerm Syntax.Quad -> Phantoms.TTerm Syntax.Resource
quadSubject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
        Core.projectionField = (Core.Name "subject")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

quadPredicate :: Phantoms.TTerm Syntax.Quad -> Phantoms.TTerm Syntax.Iri
quadPredicate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
        Core.projectionField = (Core.Name "predicate")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

quadObject :: Phantoms.TTerm Syntax.Quad -> Phantoms.TTerm Syntax.Node
quadObject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
        Core.projectionField = (Core.Name "object")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

quadGraph :: Phantoms.TTerm Syntax.Quad -> Phantoms.TTerm (Maybe Syntax.Iri)
quadGraph x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
        Core.projectionField = (Core.Name "graph")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

quadWithSubject :: Phantoms.TTerm Syntax.Quad -> Phantoms.TTerm Syntax.Resource -> Phantoms.TTerm Syntax.Quad
quadWithSubject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "predicate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "object")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "graph")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

quadWithPredicate :: Phantoms.TTerm Syntax.Quad -> Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.Quad
quadWithPredicate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "subject")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "object")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "graph")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

quadWithObject :: Phantoms.TTerm Syntax.Quad -> Phantoms.TTerm Syntax.Node -> Phantoms.TTerm Syntax.Quad
quadWithObject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "subject")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "predicate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "graph")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

quadWithGraph :: Phantoms.TTerm Syntax.Quad -> Phantoms.TTerm (Maybe Syntax.Iri) -> Phantoms.TTerm Syntax.Quad
quadWithGraph original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "subject")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "predicate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad"),
              Core.projectionField = (Core.Name "object")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

resourceIri :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.Resource
resourceIri x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Resource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iri"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

resourceBnode :: Phantoms.TTerm Syntax.BlankNode -> Phantoms.TTerm Syntax.Resource
resourceBnode x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Resource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bnode"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

triple :: Phantoms.TTerm Syntax.Resource -> Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.Node -> Phantoms.TTerm Syntax.Triple
triple subject predicate object =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Triple"),
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

tripleSubject :: Phantoms.TTerm Syntax.Triple -> Phantoms.TTerm Syntax.Resource
tripleSubject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Triple"),
        Core.projectionField = (Core.Name "subject")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

triplePredicate :: Phantoms.TTerm Syntax.Triple -> Phantoms.TTerm Syntax.Iri
triplePredicate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Triple"),
        Core.projectionField = (Core.Name "predicate")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tripleObject :: Phantoms.TTerm Syntax.Triple -> Phantoms.TTerm Syntax.Node
tripleObject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Triple"),
        Core.projectionField = (Core.Name "object")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tripleWithSubject :: Phantoms.TTerm Syntax.Triple -> Phantoms.TTerm Syntax.Resource -> Phantoms.TTerm Syntax.Triple
tripleWithSubject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Triple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Triple"),
              Core.projectionField = (Core.Name "predicate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Triple"),
              Core.projectionField = (Core.Name "object")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tripleWithPredicate :: Phantoms.TTerm Syntax.Triple -> Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.Triple
tripleWithPredicate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Triple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Triple"),
              Core.projectionField = (Core.Name "subject")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Triple"),
              Core.projectionField = (Core.Name "object")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tripleWithObject :: Phantoms.TTerm Syntax.Triple -> Phantoms.TTerm Syntax.Node -> Phantoms.TTerm Syntax.Triple
tripleWithObject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Triple"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Triple"),
              Core.projectionField = (Core.Name "subject")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.w3.rdf.syntax.Triple"),
              Core.projectionField = (Core.Name "predicate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
