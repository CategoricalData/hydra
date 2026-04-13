-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.shex.syntax

module Hydra.Dsl.Shex.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Shex.Syntax as Syntax
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

annotation :: Phantoms.TTerm Syntax.Predicate -> Phantoms.TTerm Syntax.Annotation_Alts -> Phantoms.TTerm Syntax.Annotation
annotation predicate alts =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Annotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Predicate"),
          Core.fieldTerm = (Phantoms.unTTerm predicate)},
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Phantoms.unTTerm alts)}]}))

annotationAlts :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.Annotation_Alts
annotationAlts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Annotation"),
        Core.projectionField = (Core.Name "alts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationPredicate :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.Predicate
annotationPredicate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Annotation"),
        Core.projectionField = (Core.Name "Predicate")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationWithAlts :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.Annotation_Alts -> Phantoms.TTerm Syntax.Annotation
annotationWithAlts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Annotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Annotation"),
              Core.projectionField = (Core.Name "Predicate")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

annotationWithPredicate :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.Predicate -> Phantoms.TTerm Syntax.Annotation
annotationWithPredicate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Annotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Predicate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Annotation"),
              Core.projectionField = (Core.Name "alts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotation_AltsIri :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.Annotation_Alts
annotation_AltsIri x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Annotation_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Iri"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

annotation_AltsLiteral :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.Annotation_Alts
annotation_AltsLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Annotation_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

atpNameLn :: Phantoms.TTerm Syntax.PnameNs -> Phantoms.TTerm Syntax.PnLocal -> Phantoms.TTerm Syntax.AtpNameLn
atpNameLn pnameNs pnLocal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.AtpNameLn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "PnameNs"),
          Core.fieldTerm = (Phantoms.unTTerm pnameNs)},
        Core.Field {
          Core.fieldName = (Core.Name "PnLocal"),
          Core.fieldTerm = (Phantoms.unTTerm pnLocal)}]}))

atpNameLnPnLocal :: Phantoms.TTerm Syntax.AtpNameLn -> Phantoms.TTerm Syntax.PnLocal
atpNameLnPnLocal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.AtpNameLn"),
        Core.projectionField = (Core.Name "PnLocal")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atpNameLnPnameNs :: Phantoms.TTerm Syntax.AtpNameLn -> Phantoms.TTerm Syntax.PnameNs
atpNameLnPnameNs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.AtpNameLn"),
        Core.projectionField = (Core.Name "PnameNs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

atpNameLnWithPnLocal :: Phantoms.TTerm Syntax.AtpNameLn -> Phantoms.TTerm Syntax.PnLocal -> Phantoms.TTerm Syntax.AtpNameLn
atpNameLnWithPnLocal original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.AtpNameLn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "PnameNs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.AtpNameLn"),
              Core.projectionField = (Core.Name "PnameNs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "PnLocal"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

atpNameLnWithPnameNs :: Phantoms.TTerm Syntax.AtpNameLn -> Phantoms.TTerm Syntax.PnameNs -> Phantoms.TTerm Syntax.AtpNameLn
atpNameLnWithPnameNs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.AtpNameLn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "PnameNs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "PnLocal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.AtpNameLn"),
              Core.projectionField = (Core.Name "PnLocal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

atpNameNs :: Phantoms.TTerm (Maybe Syntax.PnPrefix) -> Phantoms.TTerm Syntax.AtpNameNs
atpNameNs x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.AtpNameNs"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

baseDecl :: Phantoms.TTerm Syntax.IriRef -> Phantoms.TTerm Syntax.BaseDecl
baseDecl x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.BaseDecl"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

blankNode :: Phantoms.TTerm Syntax.BlankNodeLabel -> Phantoms.TTerm Syntax.BlankNode
blankNode x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.BlankNode"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

blankNodeLabel :: Phantoms.TTerm Syntax.BlankNodeLabel_Alts -> Phantoms.TTerm (Maybe [Syntax.BlankNodeLabel_ListOfAlts_Option_Elmt]) -> Phantoms.TTerm Syntax.PnChars -> Phantoms.TTerm Syntax.BlankNodeLabel
blankNodeLabel alts listOfAlts pnChars =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.BlankNodeLabel"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Phantoms.unTTerm alts)},
        Core.Field {
          Core.fieldName = (Core.Name "ListOfAlts"),
          Core.fieldTerm = (Phantoms.unTTerm listOfAlts)},
        Core.Field {
          Core.fieldName = (Core.Name "PnChars"),
          Core.fieldTerm = (Phantoms.unTTerm pnChars)}]}))

blankNodeLabelAlts :: Phantoms.TTerm Syntax.BlankNodeLabel -> Phantoms.TTerm Syntax.BlankNodeLabel_Alts
blankNodeLabelAlts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BlankNodeLabel"),
        Core.projectionField = (Core.Name "alts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

blankNodeLabelListOfAlts :: Phantoms.TTerm Syntax.BlankNodeLabel -> Phantoms.TTerm (Maybe [Syntax.BlankNodeLabel_ListOfAlts_Option_Elmt])
blankNodeLabelListOfAlts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BlankNodeLabel"),
        Core.projectionField = (Core.Name "ListOfAlts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

blankNodeLabelPnChars :: Phantoms.TTerm Syntax.BlankNodeLabel -> Phantoms.TTerm Syntax.PnChars
blankNodeLabelPnChars x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BlankNodeLabel"),
        Core.projectionField = (Core.Name "PnChars")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

blankNodeLabelWithAlts :: Phantoms.TTerm Syntax.BlankNodeLabel -> Phantoms.TTerm Syntax.BlankNodeLabel_Alts -> Phantoms.TTerm Syntax.BlankNodeLabel
blankNodeLabelWithAlts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.BlankNodeLabel"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ListOfAlts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BlankNodeLabel"),
              Core.projectionField = (Core.Name "ListOfAlts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "PnChars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BlankNodeLabel"),
              Core.projectionField = (Core.Name "PnChars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

blankNodeLabelWithListOfAlts :: Phantoms.TTerm Syntax.BlankNodeLabel -> Phantoms.TTerm (Maybe [Syntax.BlankNodeLabel_ListOfAlts_Option_Elmt]) -> Phantoms.TTerm Syntax.BlankNodeLabel
blankNodeLabelWithListOfAlts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.BlankNodeLabel"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BlankNodeLabel"),
              Core.projectionField = (Core.Name "alts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ListOfAlts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "PnChars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BlankNodeLabel"),
              Core.projectionField = (Core.Name "PnChars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

blankNodeLabelWithPnChars :: Phantoms.TTerm Syntax.BlankNodeLabel -> Phantoms.TTerm Syntax.PnChars -> Phantoms.TTerm Syntax.BlankNodeLabel
blankNodeLabelWithPnChars original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.BlankNodeLabel"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BlankNodeLabel"),
              Core.projectionField = (Core.Name "alts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ListOfAlts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BlankNodeLabel"),
              Core.projectionField = (Core.Name "ListOfAlts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "PnChars"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

blankNodeLabel_AltsPnCharsU :: Phantoms.TTerm Syntax.PnCharsU -> Phantoms.TTerm Syntax.BlankNodeLabel_Alts
blankNodeLabel_AltsPnCharsU x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.BlankNodeLabel_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "PnCharsU"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

blankNodeLabel_AltsRegex :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.BlankNodeLabel_Alts
blankNodeLabel_AltsRegex x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.BlankNodeLabel_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

blankNodeLabel_ListOfAlts_Option_ElmtPeriod :: Phantoms.TTerm Syntax.BlankNodeLabel_ListOfAlts_Option_Elmt
blankNodeLabel_ListOfAlts_Option_ElmtPeriod =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.BlankNodeLabel_ListOfAlts_Option_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Period"),
        Core.fieldTerm = Core.TermUnit}}))

blankNodeLabel_ListOfAlts_Option_ElmtPnChars :: Phantoms.TTerm Syntax.PnChars -> Phantoms.TTerm Syntax.BlankNodeLabel_ListOfAlts_Option_Elmt
blankNodeLabel_ListOfAlts_Option_ElmtPnChars x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.BlankNodeLabel_ListOfAlts_Option_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "PnChars"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

booleanLiteralFalse :: Phantoms.TTerm Syntax.BooleanLiteral
booleanLiteralFalse =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.BooleanLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "False"),
        Core.fieldTerm = Core.TermUnit}}))

booleanLiteralTrue :: Phantoms.TTerm Syntax.BooleanLiteral
booleanLiteralTrue =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.BooleanLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "True"),
        Core.fieldTerm = Core.TermUnit}}))

bracketedTripleExpr :: Phantoms.TTerm Syntax.InnerTripleExpr -> Phantoms.TTerm (Maybe Syntax.Cardinality) -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.SemanticActions -> Phantoms.TTerm Syntax.BracketedTripleExpr
bracketedTripleExpr innerTripleExpr cardinality listOfAnnotation semanticActions =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "InnerTripleExpr"),
          Core.fieldTerm = (Phantoms.unTTerm innerTripleExpr)},
        Core.Field {
          Core.fieldName = (Core.Name "Cardinality"),
          Core.fieldTerm = (Phantoms.unTTerm cardinality)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfAnnotation"),
          Core.fieldTerm = (Phantoms.unTTerm listOfAnnotation)},
        Core.Field {
          Core.fieldName = (Core.Name "SemanticActions"),
          Core.fieldTerm = (Phantoms.unTTerm semanticActions)}]}))

bracketedTripleExprCardinality :: Phantoms.TTerm Syntax.BracketedTripleExpr -> Phantoms.TTerm (Maybe Syntax.Cardinality)
bracketedTripleExprCardinality x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
        Core.projectionField = (Core.Name "Cardinality")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bracketedTripleExprInnerTripleExpr :: Phantoms.TTerm Syntax.BracketedTripleExpr -> Phantoms.TTerm Syntax.InnerTripleExpr
bracketedTripleExprInnerTripleExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
        Core.projectionField = (Core.Name "InnerTripleExpr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bracketedTripleExprListOfAnnotation :: Phantoms.TTerm Syntax.BracketedTripleExpr -> Phantoms.TTerm [Syntax.Annotation]
bracketedTripleExprListOfAnnotation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
        Core.projectionField = (Core.Name "listOfAnnotation")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bracketedTripleExprSemanticActions :: Phantoms.TTerm Syntax.BracketedTripleExpr -> Phantoms.TTerm Syntax.SemanticActions
bracketedTripleExprSemanticActions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
        Core.projectionField = (Core.Name "SemanticActions")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bracketedTripleExprWithCardinality :: Phantoms.TTerm Syntax.BracketedTripleExpr -> Phantoms.TTerm (Maybe Syntax.Cardinality) -> Phantoms.TTerm Syntax.BracketedTripleExpr
bracketedTripleExprWithCardinality original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "InnerTripleExpr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
              Core.projectionField = (Core.Name "InnerTripleExpr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Cardinality"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfAnnotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
              Core.projectionField = (Core.Name "listOfAnnotation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SemanticActions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
              Core.projectionField = (Core.Name "SemanticActions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

bracketedTripleExprWithInnerTripleExpr :: Phantoms.TTerm Syntax.BracketedTripleExpr -> Phantoms.TTerm Syntax.InnerTripleExpr -> Phantoms.TTerm Syntax.BracketedTripleExpr
bracketedTripleExprWithInnerTripleExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "InnerTripleExpr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
              Core.projectionField = (Core.Name "Cardinality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfAnnotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
              Core.projectionField = (Core.Name "listOfAnnotation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SemanticActions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
              Core.projectionField = (Core.Name "SemanticActions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

bracketedTripleExprWithListOfAnnotation :: Phantoms.TTerm Syntax.BracketedTripleExpr -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.BracketedTripleExpr
bracketedTripleExprWithListOfAnnotation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "InnerTripleExpr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
              Core.projectionField = (Core.Name "InnerTripleExpr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
              Core.projectionField = (Core.Name "Cardinality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfAnnotation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "SemanticActions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
              Core.projectionField = (Core.Name "SemanticActions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

bracketedTripleExprWithSemanticActions :: Phantoms.TTerm Syntax.BracketedTripleExpr -> Phantoms.TTerm Syntax.SemanticActions -> Phantoms.TTerm Syntax.BracketedTripleExpr
bracketedTripleExprWithSemanticActions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "InnerTripleExpr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
              Core.projectionField = (Core.Name "InnerTripleExpr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
              Core.projectionField = (Core.Name "Cardinality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfAnnotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.BracketedTripleExpr"),
              Core.projectionField = (Core.Name "listOfAnnotation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SemanticActions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

cardinalityAst :: Phantoms.TTerm Syntax.Cardinality
cardinalityAst =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Cardinality"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Ast"),
        Core.fieldTerm = Core.TermUnit}}))

cardinalityPlus :: Phantoms.TTerm Syntax.Cardinality
cardinalityPlus =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Cardinality"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Plus"),
        Core.fieldTerm = Core.TermUnit}}))

cardinalityQuest :: Phantoms.TTerm Syntax.Cardinality
cardinalityQuest =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Cardinality"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Quest"),
        Core.fieldTerm = Core.TermUnit}}))

cardinalityRepeatRange :: Phantoms.TTerm Syntax.RepeatRange -> Phantoms.TTerm Syntax.Cardinality
cardinalityRepeatRange x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Cardinality"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "RepeatRange"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

code :: Phantoms.TTerm [Syntax.Code_Elmt] -> Phantoms.TTerm Syntax.Code
code x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.Code"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

codeDecl :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.CodeDecl_Alts -> Phantoms.TTerm Syntax.CodeDecl
codeDecl iri alts =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.CodeDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Iri"),
          Core.fieldTerm = (Phantoms.unTTerm iri)},
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Phantoms.unTTerm alts)}]}))

codeDeclAlts :: Phantoms.TTerm Syntax.CodeDecl -> Phantoms.TTerm Syntax.CodeDecl_Alts
codeDeclAlts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.CodeDecl"),
        Core.projectionField = (Core.Name "alts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

codeDeclIri :: Phantoms.TTerm Syntax.CodeDecl -> Phantoms.TTerm Syntax.Iri
codeDeclIri x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.CodeDecl"),
        Core.projectionField = (Core.Name "Iri")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

codeDeclWithAlts :: Phantoms.TTerm Syntax.CodeDecl -> Phantoms.TTerm Syntax.CodeDecl_Alts -> Phantoms.TTerm Syntax.CodeDecl
codeDeclWithAlts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.CodeDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Iri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.CodeDecl"),
              Core.projectionField = (Core.Name "Iri")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

codeDeclWithIri :: Phantoms.TTerm Syntax.CodeDecl -> Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.CodeDecl
codeDeclWithIri original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.CodeDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Iri"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.CodeDecl"),
              Core.projectionField = (Core.Name "alts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

codeDecl_AltsCode :: Phantoms.TTerm Syntax.Code -> Phantoms.TTerm Syntax.CodeDecl_Alts
codeDecl_AltsCode x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.CodeDecl_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Code"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

codeDecl_AltsPercnt :: Phantoms.TTerm Syntax.CodeDecl_Alts
codeDecl_AltsPercnt =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.CodeDecl_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Percnt"),
        Core.fieldTerm = Core.TermUnit}}))

code_ElmtRegex :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Code_Elmt
code_ElmtRegex x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Code_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

code_ElmtSequence :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Code_Elmt
code_ElmtSequence x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Code_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

code_ElmtUchar :: Phantoms.TTerm Syntax.Uchar -> Phantoms.TTerm Syntax.Code_Elmt
code_ElmtUchar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Code_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Uchar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

datatype :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.Datatype
datatype x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.Datatype"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

decimal :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Decimal
decimal x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.Decimal"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

directiveBaseDecl :: Phantoms.TTerm Syntax.BaseDecl -> Phantoms.TTerm Syntax.Directive
directiveBaseDecl x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Directive"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "BaseDecl"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

directivePrefixDecl :: Phantoms.TTerm Syntax.PrefixDecl -> Phantoms.TTerm Syntax.Directive
directivePrefixDecl x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Directive"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "PrefixDecl"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

double :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Double_
double x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.Double"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

echar :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Echar
echar x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.Echar"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

exclusion :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.Exclusion
exclusion x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.Exclusion"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

extraPropertySet :: Phantoms.TTerm [Syntax.Predicate] -> Phantoms.TTerm Syntax.ExtraPropertySet
extraPropertySet x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.ExtraPropertySet"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

groupTripleExprMultiElementGroup :: Phantoms.TTerm Syntax.MultiElementGroup -> Phantoms.TTerm Syntax.GroupTripleExpr
groupTripleExprMultiElementGroup x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.GroupTripleExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "MultiElementGroup"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

groupTripleExprSingleElementGroup :: Phantoms.TTerm Syntax.SingleElementGroup -> Phantoms.TTerm Syntax.GroupTripleExpr
groupTripleExprSingleElementGroup x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.GroupTripleExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "SingleElementGroup"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

hex :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Hex
hex x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.Hex"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

include :: Phantoms.TTerm Syntax.TripleExprLabel -> Phantoms.TTerm Syntax.Include
include x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.Include"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

includeSet :: Phantoms.TTerm [Syntax.ShapeExprLabel] -> Phantoms.TTerm Syntax.IncludeSet
includeSet x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.IncludeSet"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

inlineShapeAnd :: Phantoms.TTerm Syntax.InlineShapeNot -> Phantoms.TTerm [Syntax.InlineShapeNot] -> Phantoms.TTerm Syntax.InlineShapeAnd
inlineShapeAnd inlineShapeNot listOfSequence =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAnd"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "InlineShapeNot"),
          Core.fieldTerm = (Phantoms.unTTerm inlineShapeNot)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfSequence"),
          Core.fieldTerm = (Phantoms.unTTerm listOfSequence)}]}))

inlineShapeAndInlineShapeNot :: Phantoms.TTerm Syntax.InlineShapeAnd -> Phantoms.TTerm Syntax.InlineShapeNot
inlineShapeAndInlineShapeNot x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAnd"),
        Core.projectionField = (Core.Name "InlineShapeNot")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inlineShapeAndListOfSequence :: Phantoms.TTerm Syntax.InlineShapeAnd -> Phantoms.TTerm [Syntax.InlineShapeNot]
inlineShapeAndListOfSequence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAnd"),
        Core.projectionField = (Core.Name "listOfSequence")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inlineShapeAndWithInlineShapeNot :: Phantoms.TTerm Syntax.InlineShapeAnd -> Phantoms.TTerm Syntax.InlineShapeNot -> Phantoms.TTerm Syntax.InlineShapeAnd
inlineShapeAndWithInlineShapeNot original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAnd"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "InlineShapeNot"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfSequence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAnd"),
              Core.projectionField = (Core.Name "listOfSequence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inlineShapeAndWithListOfSequence :: Phantoms.TTerm Syntax.InlineShapeAnd -> Phantoms.TTerm [Syntax.InlineShapeNot] -> Phantoms.TTerm Syntax.InlineShapeAnd
inlineShapeAndWithListOfSequence original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAnd"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "InlineShapeNot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAnd"),
              Core.projectionField = (Core.Name "InlineShapeNot")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfSequence"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inlineShapeAtomPeriod :: Phantoms.TTerm Syntax.InlineShapeAtom
inlineShapeAtomPeriod =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Period"),
        Core.fieldTerm = Core.TermUnit}}))

inlineShapeAtomSequence :: Phantoms.TTerm Syntax.InlineShapeAtom_Sequence -> Phantoms.TTerm Syntax.InlineShapeAtom
inlineShapeAtomSequence x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

inlineShapeAtomSequence2 :: Phantoms.TTerm Syntax.InlineShapeAtom_Sequence2 -> Phantoms.TTerm Syntax.InlineShapeAtom
inlineShapeAtomSequence2 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

inlineShapeAtomSequence3 :: Phantoms.TTerm Syntax.ShapeExpression -> Phantoms.TTerm Syntax.InlineShapeAtom
inlineShapeAtomSequence3 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence3"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

inlineShapeAtom_Sequence :: Phantoms.TTerm Syntax.NodeConstraint -> Phantoms.TTerm (Maybe Syntax.InlineShapeOrRef) -> Phantoms.TTerm Syntax.InlineShapeAtom_Sequence
inlineShapeAtom_Sequence nodeConstraint inlineShapeOrRef =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAtom_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NodeConstraint"),
          Core.fieldTerm = (Phantoms.unTTerm nodeConstraint)},
        Core.Field {
          Core.fieldName = (Core.Name "InlineShapeOrRef"),
          Core.fieldTerm = (Phantoms.unTTerm inlineShapeOrRef)}]}))

inlineShapeAtom_Sequence2 :: Phantoms.TTerm Syntax.InlineShapeOrRef -> Phantoms.TTerm (Maybe Syntax.NodeConstraint) -> Phantoms.TTerm Syntax.InlineShapeAtom_Sequence2
inlineShapeAtom_Sequence2 inlineShapeOrRef nodeConstraint =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAtom_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "InlineShapeOrRef"),
          Core.fieldTerm = (Phantoms.unTTerm inlineShapeOrRef)},
        Core.Field {
          Core.fieldName = (Core.Name "NodeConstraint"),
          Core.fieldTerm = (Phantoms.unTTerm nodeConstraint)}]}))

inlineShapeAtom_Sequence2InlineShapeOrRef :: Phantoms.TTerm Syntax.InlineShapeAtom_Sequence2 -> Phantoms.TTerm Syntax.InlineShapeOrRef
inlineShapeAtom_Sequence2InlineShapeOrRef x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAtom_Sequence2"),
        Core.projectionField = (Core.Name "InlineShapeOrRef")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inlineShapeAtom_Sequence2NodeConstraint :: Phantoms.TTerm Syntax.InlineShapeAtom_Sequence2 -> Phantoms.TTerm (Maybe Syntax.NodeConstraint)
inlineShapeAtom_Sequence2NodeConstraint x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAtom_Sequence2"),
        Core.projectionField = (Core.Name "NodeConstraint")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inlineShapeAtom_Sequence2WithInlineShapeOrRef :: Phantoms.TTerm Syntax.InlineShapeAtom_Sequence2 -> Phantoms.TTerm Syntax.InlineShapeOrRef -> Phantoms.TTerm Syntax.InlineShapeAtom_Sequence2
inlineShapeAtom_Sequence2WithInlineShapeOrRef original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAtom_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "InlineShapeOrRef"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "NodeConstraint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAtom_Sequence2"),
              Core.projectionField = (Core.Name "NodeConstraint")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inlineShapeAtom_Sequence2WithNodeConstraint :: Phantoms.TTerm Syntax.InlineShapeAtom_Sequence2 -> Phantoms.TTerm (Maybe Syntax.NodeConstraint) -> Phantoms.TTerm Syntax.InlineShapeAtom_Sequence2
inlineShapeAtom_Sequence2WithNodeConstraint original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAtom_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "InlineShapeOrRef"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAtom_Sequence2"),
              Core.projectionField = (Core.Name "InlineShapeOrRef")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "NodeConstraint"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inlineShapeAtom_SequenceInlineShapeOrRef :: Phantoms.TTerm Syntax.InlineShapeAtom_Sequence -> Phantoms.TTerm (Maybe Syntax.InlineShapeOrRef)
inlineShapeAtom_SequenceInlineShapeOrRef x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAtom_Sequence"),
        Core.projectionField = (Core.Name "InlineShapeOrRef")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inlineShapeAtom_SequenceNodeConstraint :: Phantoms.TTerm Syntax.InlineShapeAtom_Sequence -> Phantoms.TTerm Syntax.NodeConstraint
inlineShapeAtom_SequenceNodeConstraint x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAtom_Sequence"),
        Core.projectionField = (Core.Name "NodeConstraint")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inlineShapeAtom_SequenceWithInlineShapeOrRef :: Phantoms.TTerm Syntax.InlineShapeAtom_Sequence -> Phantoms.TTerm (Maybe Syntax.InlineShapeOrRef) -> Phantoms.TTerm Syntax.InlineShapeAtom_Sequence
inlineShapeAtom_SequenceWithInlineShapeOrRef original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAtom_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NodeConstraint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAtom_Sequence"),
              Core.projectionField = (Core.Name "NodeConstraint")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "InlineShapeOrRef"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inlineShapeAtom_SequenceWithNodeConstraint :: Phantoms.TTerm Syntax.InlineShapeAtom_Sequence -> Phantoms.TTerm Syntax.NodeConstraint -> Phantoms.TTerm Syntax.InlineShapeAtom_Sequence
inlineShapeAtom_SequenceWithNodeConstraint original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAtom_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NodeConstraint"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "InlineShapeOrRef"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeAtom_Sequence"),
              Core.projectionField = (Core.Name "InlineShapeOrRef")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inlineShapeDefinition :: Phantoms.TTerm [Syntax.InlineShapeDefinition_ListOfAlts_Elmt] -> Phantoms.TTerm (Maybe Syntax.TripleExpression) -> Phantoms.TTerm Syntax.InlineShapeDefinition
inlineShapeDefinition listOfAlts tripleExpression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.InlineShapeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "listOfAlts"),
          Core.fieldTerm = (Phantoms.unTTerm listOfAlts)},
        Core.Field {
          Core.fieldName = (Core.Name "TripleExpression"),
          Core.fieldTerm = (Phantoms.unTTerm tripleExpression)}]}))

inlineShapeDefinitionListOfAlts :: Phantoms.TTerm Syntax.InlineShapeDefinition -> Phantoms.TTerm [Syntax.InlineShapeDefinition_ListOfAlts_Elmt]
inlineShapeDefinitionListOfAlts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeDefinition"),
        Core.projectionField = (Core.Name "listOfAlts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inlineShapeDefinitionTripleExpression :: Phantoms.TTerm Syntax.InlineShapeDefinition -> Phantoms.TTerm (Maybe Syntax.TripleExpression)
inlineShapeDefinitionTripleExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeDefinition"),
        Core.projectionField = (Core.Name "TripleExpression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inlineShapeDefinitionWithListOfAlts :: Phantoms.TTerm Syntax.InlineShapeDefinition -> Phantoms.TTerm [Syntax.InlineShapeDefinition_ListOfAlts_Elmt] -> Phantoms.TTerm Syntax.InlineShapeDefinition
inlineShapeDefinitionWithListOfAlts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.InlineShapeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "listOfAlts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "TripleExpression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeDefinition"),
              Core.projectionField = (Core.Name "TripleExpression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inlineShapeDefinitionWithTripleExpression :: Phantoms.TTerm Syntax.InlineShapeDefinition -> Phantoms.TTerm (Maybe Syntax.TripleExpression) -> Phantoms.TTerm Syntax.InlineShapeDefinition
inlineShapeDefinitionWithTripleExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.InlineShapeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "listOfAlts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeDefinition"),
              Core.projectionField = (Core.Name "listOfAlts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "TripleExpression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inlineShapeDefinition_ListOfAlts_ElmtCLOSED :: Phantoms.TTerm Syntax.InlineShapeDefinition_ListOfAlts_Elmt
inlineShapeDefinition_ListOfAlts_ElmtCLOSED =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeDefinition_ListOfAlts_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "CLOSED"),
        Core.fieldTerm = Core.TermUnit}}))

inlineShapeDefinition_ListOfAlts_ElmtExtraPropertySet :: Phantoms.TTerm Syntax.ExtraPropertySet -> Phantoms.TTerm Syntax.InlineShapeDefinition_ListOfAlts_Elmt
inlineShapeDefinition_ListOfAlts_ElmtExtraPropertySet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeDefinition_ListOfAlts_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ExtraPropertySet"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

inlineShapeDefinition_ListOfAlts_ElmtIncludeSet :: Phantoms.TTerm Syntax.IncludeSet -> Phantoms.TTerm Syntax.InlineShapeDefinition_ListOfAlts_Elmt
inlineShapeDefinition_ListOfAlts_ElmtIncludeSet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeDefinition_ListOfAlts_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "IncludeSet"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

inlineShapeExpression :: Phantoms.TTerm Syntax.InlineShapeOr -> Phantoms.TTerm Syntax.InlineShapeExpression
inlineShapeExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.InlineShapeExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

inlineShapeNot :: Phantoms.TTerm (Maybe ()) -> Phantoms.TTerm Syntax.InlineShapeAtom -> Phantoms.TTerm Syntax.InlineShapeNot
inlineShapeNot nOT inlineShapeAtom =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.InlineShapeNot"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NOT"),
          Core.fieldTerm = (Phantoms.unTTerm nOT)},
        Core.Field {
          Core.fieldName = (Core.Name "InlineShapeAtom"),
          Core.fieldTerm = (Phantoms.unTTerm inlineShapeAtom)}]}))

inlineShapeNotInlineShapeAtom :: Phantoms.TTerm Syntax.InlineShapeNot -> Phantoms.TTerm Syntax.InlineShapeAtom
inlineShapeNotInlineShapeAtom x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeNot"),
        Core.projectionField = (Core.Name "InlineShapeAtom")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inlineShapeNotNOT :: Phantoms.TTerm Syntax.InlineShapeNot -> Phantoms.TTerm (Maybe ())
inlineShapeNotNOT x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeNot"),
        Core.projectionField = (Core.Name "NOT")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inlineShapeNotWithInlineShapeAtom :: Phantoms.TTerm Syntax.InlineShapeNot -> Phantoms.TTerm Syntax.InlineShapeAtom -> Phantoms.TTerm Syntax.InlineShapeNot
inlineShapeNotWithInlineShapeAtom original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.InlineShapeNot"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NOT"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeNot"),
              Core.projectionField = (Core.Name "NOT")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "InlineShapeAtom"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inlineShapeNotWithNOT :: Phantoms.TTerm Syntax.InlineShapeNot -> Phantoms.TTerm (Maybe ()) -> Phantoms.TTerm Syntax.InlineShapeNot
inlineShapeNotWithNOT original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.InlineShapeNot"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NOT"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "InlineShapeAtom"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeNot"),
              Core.projectionField = (Core.Name "InlineShapeAtom")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inlineShapeOr :: Phantoms.TTerm Syntax.ShapeAnd -> Phantoms.TTerm [Syntax.InlineShapeAnd] -> Phantoms.TTerm Syntax.InlineShapeOr
inlineShapeOr shapeAnd listOfSequence =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.InlineShapeOr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ShapeAnd"),
          Core.fieldTerm = (Phantoms.unTTerm shapeAnd)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfSequence"),
          Core.fieldTerm = (Phantoms.unTTerm listOfSequence)}]}))

inlineShapeOrListOfSequence :: Phantoms.TTerm Syntax.InlineShapeOr -> Phantoms.TTerm [Syntax.InlineShapeAnd]
inlineShapeOrListOfSequence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeOr"),
        Core.projectionField = (Core.Name "listOfSequence")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inlineShapeOrRefAtpNameLn :: Phantoms.TTerm Syntax.AtpNameLn -> Phantoms.TTerm Syntax.InlineShapeOrRef
inlineShapeOrRefAtpNameLn x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeOrRef"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "AtpNameLn"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

inlineShapeOrRefAtpNameNs :: Phantoms.TTerm Syntax.AtpNameNs -> Phantoms.TTerm Syntax.InlineShapeOrRef
inlineShapeOrRefAtpNameNs x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeOrRef"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "AtpNameNs"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

inlineShapeOrRefInlineShapeDefinition :: Phantoms.TTerm Syntax.InlineShapeDefinition -> Phantoms.TTerm Syntax.InlineShapeOrRef
inlineShapeOrRefInlineShapeDefinition x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeOrRef"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "InlineShapeDefinition"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

inlineShapeOrRefSequence :: Phantoms.TTerm Syntax.ShapeExprLabel -> Phantoms.TTerm Syntax.InlineShapeOrRef
inlineShapeOrRefSequence x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeOrRef"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

inlineShapeOrShapeAnd :: Phantoms.TTerm Syntax.InlineShapeOr -> Phantoms.TTerm Syntax.ShapeAnd
inlineShapeOrShapeAnd x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeOr"),
        Core.projectionField = (Core.Name "ShapeAnd")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inlineShapeOrWithListOfSequence :: Phantoms.TTerm Syntax.InlineShapeOr -> Phantoms.TTerm [Syntax.InlineShapeAnd] -> Phantoms.TTerm Syntax.InlineShapeOr
inlineShapeOrWithListOfSequence original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.InlineShapeOr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ShapeAnd"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeOr"),
              Core.projectionField = (Core.Name "ShapeAnd")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfSequence"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inlineShapeOrWithShapeAnd :: Phantoms.TTerm Syntax.InlineShapeOr -> Phantoms.TTerm Syntax.ShapeAnd -> Phantoms.TTerm Syntax.InlineShapeOr
inlineShapeOrWithShapeAnd original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.InlineShapeOr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ShapeAnd"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfSequence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.InlineShapeOr"),
              Core.projectionField = (Core.Name "listOfSequence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

innerTripleExprMultiElementGroup :: Phantoms.TTerm Syntax.MultiElementGroup -> Phantoms.TTerm Syntax.InnerTripleExpr
innerTripleExprMultiElementGroup x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.InnerTripleExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "MultiElementGroup"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

innerTripleExprMultiElementOneOf :: Phantoms.TTerm Syntax.MultiElementOneOf -> Phantoms.TTerm Syntax.InnerTripleExpr
innerTripleExprMultiElementOneOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.InnerTripleExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "MultiElementOneOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

integer :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Integer_
integer x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.Integer"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

iriIriRef :: Phantoms.TTerm Syntax.IriRef -> Phantoms.TTerm Syntax.Iri
iriIriRef x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Iri"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "IriRef"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

iriPrefixedName :: Phantoms.TTerm Syntax.PrefixedName -> Phantoms.TTerm Syntax.Iri
iriPrefixedName x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Iri"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "PrefixedName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

iriRangeSequence :: Phantoms.TTerm Syntax.IriRange_Sequence -> Phantoms.TTerm Syntax.IriRange
iriRangeSequence x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.IriRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

iriRangeSequence2 :: Phantoms.TTerm [Syntax.Exclusion] -> Phantoms.TTerm Syntax.IriRange
iriRangeSequence2 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.IriRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

iriRange_Sequence :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm (Maybe [Syntax.Exclusion]) -> Phantoms.TTerm Syntax.IriRange_Sequence
iriRange_Sequence iri sequence =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.IriRange_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Iri"),
          Core.fieldTerm = (Phantoms.unTTerm iri)},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Phantoms.unTTerm sequence)}]}))

iriRange_SequenceIri :: Phantoms.TTerm Syntax.IriRange_Sequence -> Phantoms.TTerm Syntax.Iri
iriRange_SequenceIri x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.IriRange_Sequence"),
        Core.projectionField = (Core.Name "Iri")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

iriRange_SequenceSequence :: Phantoms.TTerm Syntax.IriRange_Sequence -> Phantoms.TTerm (Maybe [Syntax.Exclusion])
iriRange_SequenceSequence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.IriRange_Sequence"),
        Core.projectionField = (Core.Name "Sequence")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

iriRange_SequenceWithIri :: Phantoms.TTerm Syntax.IriRange_Sequence -> Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.IriRange_Sequence
iriRange_SequenceWithIri original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.IriRange_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Iri"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.IriRange_Sequence"),
              Core.projectionField = (Core.Name "Sequence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

iriRange_SequenceWithSequence :: Phantoms.TTerm Syntax.IriRange_Sequence -> Phantoms.TTerm (Maybe [Syntax.Exclusion]) -> Phantoms.TTerm Syntax.IriRange_Sequence
iriRange_SequenceWithSequence original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.IriRange_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Iri"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.IriRange_Sequence"),
              Core.projectionField = (Core.Name "Iri")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

iriRef :: Phantoms.TTerm [Syntax.IriRef_Elmt] -> Phantoms.TTerm Syntax.IriRef
iriRef x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.IriRef"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

iriRef_ElmtRegex :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.IriRef_Elmt
iriRef_ElmtRegex x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.IriRef_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

iriRef_ElmtUchar :: Phantoms.TTerm Syntax.Uchar -> Phantoms.TTerm Syntax.IriRef_Elmt
iriRef_ElmtUchar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.IriRef_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Uchar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

langTag :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.LangTag
langTag x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.LangTag"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

literalBooleanLiteral :: Phantoms.TTerm Syntax.BooleanLiteral -> Phantoms.TTerm Syntax.Literal
literalBooleanLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "BooleanLiteral"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalNumericLiteral :: Phantoms.TTerm Syntax.NumericLiteral -> Phantoms.TTerm Syntax.Literal
literalNumericLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "NumericLiteral"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalRdfLiteral :: Phantoms.TTerm Syntax.RdfLiteral -> Phantoms.TTerm Syntax.Literal
literalRdfLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "RdfLiteral"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

multiElementGroup :: Phantoms.TTerm Syntax.UnaryTripleExpr -> Phantoms.TTerm [Syntax.UnaryTripleExpr] -> Phantoms.TTerm (Maybe ()) -> Phantoms.TTerm Syntax.MultiElementGroup
multiElementGroup unaryTripleExpr listOfSequence semi =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.MultiElementGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "UnaryTripleExpr"),
          Core.fieldTerm = (Phantoms.unTTerm unaryTripleExpr)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfSequence"),
          Core.fieldTerm = (Phantoms.unTTerm listOfSequence)},
        Core.Field {
          Core.fieldName = (Core.Name "Semi"),
          Core.fieldTerm = (Phantoms.unTTerm semi)}]}))

multiElementGroupListOfSequence :: Phantoms.TTerm Syntax.MultiElementGroup -> Phantoms.TTerm [Syntax.UnaryTripleExpr]
multiElementGroupListOfSequence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.MultiElementGroup"),
        Core.projectionField = (Core.Name "listOfSequence")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiElementGroupSemi :: Phantoms.TTerm Syntax.MultiElementGroup -> Phantoms.TTerm (Maybe ())
multiElementGroupSemi x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.MultiElementGroup"),
        Core.projectionField = (Core.Name "Semi")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiElementGroupUnaryTripleExpr :: Phantoms.TTerm Syntax.MultiElementGroup -> Phantoms.TTerm Syntax.UnaryTripleExpr
multiElementGroupUnaryTripleExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.MultiElementGroup"),
        Core.projectionField = (Core.Name "UnaryTripleExpr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiElementGroupWithListOfSequence :: Phantoms.TTerm Syntax.MultiElementGroup -> Phantoms.TTerm [Syntax.UnaryTripleExpr] -> Phantoms.TTerm Syntax.MultiElementGroup
multiElementGroupWithListOfSequence original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.MultiElementGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "UnaryTripleExpr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.MultiElementGroup"),
              Core.projectionField = (Core.Name "UnaryTripleExpr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfSequence"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Semi"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.MultiElementGroup"),
              Core.projectionField = (Core.Name "Semi")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

multiElementGroupWithSemi :: Phantoms.TTerm Syntax.MultiElementGroup -> Phantoms.TTerm (Maybe ()) -> Phantoms.TTerm Syntax.MultiElementGroup
multiElementGroupWithSemi original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.MultiElementGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "UnaryTripleExpr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.MultiElementGroup"),
              Core.projectionField = (Core.Name "UnaryTripleExpr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfSequence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.MultiElementGroup"),
              Core.projectionField = (Core.Name "listOfSequence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Semi"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

multiElementGroupWithUnaryTripleExpr :: Phantoms.TTerm Syntax.MultiElementGroup -> Phantoms.TTerm Syntax.UnaryTripleExpr -> Phantoms.TTerm Syntax.MultiElementGroup
multiElementGroupWithUnaryTripleExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.MultiElementGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "UnaryTripleExpr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfSequence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.MultiElementGroup"),
              Core.projectionField = (Core.Name "listOfSequence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Semi"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.MultiElementGroup"),
              Core.projectionField = (Core.Name "Semi")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

multiElementOneOf :: Phantoms.TTerm Syntax.GroupTripleExpr -> Phantoms.TTerm [Syntax.GroupTripleExpr] -> Phantoms.TTerm Syntax.MultiElementOneOf
multiElementOneOf groupTripleExpr listOfSequence =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.MultiElementOneOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "GroupTripleExpr"),
          Core.fieldTerm = (Phantoms.unTTerm groupTripleExpr)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfSequence"),
          Core.fieldTerm = (Phantoms.unTTerm listOfSequence)}]}))

multiElementOneOfGroupTripleExpr :: Phantoms.TTerm Syntax.MultiElementOneOf -> Phantoms.TTerm Syntax.GroupTripleExpr
multiElementOneOfGroupTripleExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.MultiElementOneOf"),
        Core.projectionField = (Core.Name "GroupTripleExpr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiElementOneOfListOfSequence :: Phantoms.TTerm Syntax.MultiElementOneOf -> Phantoms.TTerm [Syntax.GroupTripleExpr]
multiElementOneOfListOfSequence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.MultiElementOneOf"),
        Core.projectionField = (Core.Name "listOfSequence")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiElementOneOfWithGroupTripleExpr :: Phantoms.TTerm Syntax.MultiElementOneOf -> Phantoms.TTerm Syntax.GroupTripleExpr -> Phantoms.TTerm Syntax.MultiElementOneOf
multiElementOneOfWithGroupTripleExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.MultiElementOneOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "GroupTripleExpr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfSequence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.MultiElementOneOf"),
              Core.projectionField = (Core.Name "listOfSequence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

multiElementOneOfWithListOfSequence :: Phantoms.TTerm Syntax.MultiElementOneOf -> Phantoms.TTerm [Syntax.GroupTripleExpr] -> Phantoms.TTerm Syntax.MultiElementOneOf
multiElementOneOfWithListOfSequence original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.MultiElementOneOf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "GroupTripleExpr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.MultiElementOneOf"),
              Core.projectionField = (Core.Name "GroupTripleExpr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfSequence"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

nodeConstraintListOfXsFacet :: Phantoms.TTerm [Syntax.XsFacet] -> Phantoms.TTerm Syntax.NodeConstraint
nodeConstraintListOfXsFacet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "listOfXsFacet"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nodeConstraintSequence :: Phantoms.TTerm [Syntax.XsFacet] -> Phantoms.TTerm Syntax.NodeConstraint
nodeConstraintSequence x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nodeConstraintSequence2 :: Phantoms.TTerm Syntax.NodeConstraint_Sequence2 -> Phantoms.TTerm Syntax.NodeConstraint
nodeConstraintSequence2 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nodeConstraintSequence3 :: Phantoms.TTerm Syntax.NodeConstraint_Sequence3 -> Phantoms.TTerm Syntax.NodeConstraint
nodeConstraintSequence3 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence3"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nodeConstraintSequence4 :: Phantoms.TTerm Syntax.NodeConstraint_Sequence4 -> Phantoms.TTerm Syntax.NodeConstraint
nodeConstraintSequence4 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence4"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nodeConstraintSequence5 :: Phantoms.TTerm Syntax.NodeConstraint_Sequence5 -> Phantoms.TTerm Syntax.NodeConstraint
nodeConstraintSequence5 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence5"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nodeConstraint_Sequence2 :: Phantoms.TTerm Syntax.NonLiteralKind -> Phantoms.TTerm [Syntax.StringFacet] -> Phantoms.TTerm Syntax.NodeConstraint_Sequence2
nodeConstraint_Sequence2 nonLiteralKind listOfStringFacet =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NonLiteralKind"),
          Core.fieldTerm = (Phantoms.unTTerm nonLiteralKind)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfStringFacet"),
          Core.fieldTerm = (Phantoms.unTTerm listOfStringFacet)}]}))

nodeConstraint_Sequence2ListOfStringFacet :: Phantoms.TTerm Syntax.NodeConstraint_Sequence2 -> Phantoms.TTerm [Syntax.StringFacet]
nodeConstraint_Sequence2ListOfStringFacet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence2"),
        Core.projectionField = (Core.Name "listOfStringFacet")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nodeConstraint_Sequence2NonLiteralKind :: Phantoms.TTerm Syntax.NodeConstraint_Sequence2 -> Phantoms.TTerm Syntax.NonLiteralKind
nodeConstraint_Sequence2NonLiteralKind x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence2"),
        Core.projectionField = (Core.Name "NonLiteralKind")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nodeConstraint_Sequence2WithListOfStringFacet :: Phantoms.TTerm Syntax.NodeConstraint_Sequence2 -> Phantoms.TTerm [Syntax.StringFacet] -> Phantoms.TTerm Syntax.NodeConstraint_Sequence2
nodeConstraint_Sequence2WithListOfStringFacet original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NonLiteralKind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence2"),
              Core.projectionField = (Core.Name "NonLiteralKind")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfStringFacet"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

nodeConstraint_Sequence2WithNonLiteralKind :: Phantoms.TTerm Syntax.NodeConstraint_Sequence2 -> Phantoms.TTerm Syntax.NonLiteralKind -> Phantoms.TTerm Syntax.NodeConstraint_Sequence2
nodeConstraint_Sequence2WithNonLiteralKind original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NonLiteralKind"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfStringFacet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence2"),
              Core.projectionField = (Core.Name "listOfStringFacet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

nodeConstraint_Sequence3 :: Phantoms.TTerm Syntax.Datatype -> Phantoms.TTerm [Syntax.XsFacet] -> Phantoms.TTerm Syntax.NodeConstraint_Sequence3
nodeConstraint_Sequence3 datatype listOfXsFacet =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Datatype"),
          Core.fieldTerm = (Phantoms.unTTerm datatype)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfXsFacet"),
          Core.fieldTerm = (Phantoms.unTTerm listOfXsFacet)}]}))

nodeConstraint_Sequence3Datatype :: Phantoms.TTerm Syntax.NodeConstraint_Sequence3 -> Phantoms.TTerm Syntax.Datatype
nodeConstraint_Sequence3Datatype x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence3"),
        Core.projectionField = (Core.Name "Datatype")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nodeConstraint_Sequence3ListOfXsFacet :: Phantoms.TTerm Syntax.NodeConstraint_Sequence3 -> Phantoms.TTerm [Syntax.XsFacet]
nodeConstraint_Sequence3ListOfXsFacet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence3"),
        Core.projectionField = (Core.Name "listOfXsFacet")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nodeConstraint_Sequence3WithDatatype :: Phantoms.TTerm Syntax.NodeConstraint_Sequence3 -> Phantoms.TTerm Syntax.Datatype -> Phantoms.TTerm Syntax.NodeConstraint_Sequence3
nodeConstraint_Sequence3WithDatatype original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Datatype"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfXsFacet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence3"),
              Core.projectionField = (Core.Name "listOfXsFacet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

nodeConstraint_Sequence3WithListOfXsFacet :: Phantoms.TTerm Syntax.NodeConstraint_Sequence3 -> Phantoms.TTerm [Syntax.XsFacet] -> Phantoms.TTerm Syntax.NodeConstraint_Sequence3
nodeConstraint_Sequence3WithListOfXsFacet original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Datatype"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence3"),
              Core.projectionField = (Core.Name "Datatype")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfXsFacet"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

nodeConstraint_Sequence4 :: Phantoms.TTerm Syntax.ValueSet -> Phantoms.TTerm [Syntax.XsFacet] -> Phantoms.TTerm Syntax.NodeConstraint_Sequence4
nodeConstraint_Sequence4 valueSet listOfXsFacet =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence4"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ValueSet"),
          Core.fieldTerm = (Phantoms.unTTerm valueSet)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfXsFacet"),
          Core.fieldTerm = (Phantoms.unTTerm listOfXsFacet)}]}))

nodeConstraint_Sequence4ListOfXsFacet :: Phantoms.TTerm Syntax.NodeConstraint_Sequence4 -> Phantoms.TTerm [Syntax.XsFacet]
nodeConstraint_Sequence4ListOfXsFacet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence4"),
        Core.projectionField = (Core.Name "listOfXsFacet")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nodeConstraint_Sequence4ValueSet :: Phantoms.TTerm Syntax.NodeConstraint_Sequence4 -> Phantoms.TTerm Syntax.ValueSet
nodeConstraint_Sequence4ValueSet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence4"),
        Core.projectionField = (Core.Name "ValueSet")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nodeConstraint_Sequence4WithListOfXsFacet :: Phantoms.TTerm Syntax.NodeConstraint_Sequence4 -> Phantoms.TTerm [Syntax.XsFacet] -> Phantoms.TTerm Syntax.NodeConstraint_Sequence4
nodeConstraint_Sequence4WithListOfXsFacet original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence4"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ValueSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence4"),
              Core.projectionField = (Core.Name "ValueSet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfXsFacet"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

nodeConstraint_Sequence4WithValueSet :: Phantoms.TTerm Syntax.NodeConstraint_Sequence4 -> Phantoms.TTerm Syntax.ValueSet -> Phantoms.TTerm Syntax.NodeConstraint_Sequence4
nodeConstraint_Sequence4WithValueSet original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence4"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ValueSet"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfXsFacet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence4"),
              Core.projectionField = (Core.Name "listOfXsFacet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

nodeConstraint_Sequence5 :: Phantoms.TTerm Syntax.ValueSet -> Phantoms.TTerm [Syntax.XsFacet] -> Phantoms.TTerm Syntax.NodeConstraint_Sequence5
nodeConstraint_Sequence5 valueSet listOfXsFacet =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence5"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ValueSet"),
          Core.fieldTerm = (Phantoms.unTTerm valueSet)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfXsFacet"),
          Core.fieldTerm = (Phantoms.unTTerm listOfXsFacet)}]}))

nodeConstraint_Sequence5ListOfXsFacet :: Phantoms.TTerm Syntax.NodeConstraint_Sequence5 -> Phantoms.TTerm [Syntax.XsFacet]
nodeConstraint_Sequence5ListOfXsFacet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence5"),
        Core.projectionField = (Core.Name "listOfXsFacet")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nodeConstraint_Sequence5ValueSet :: Phantoms.TTerm Syntax.NodeConstraint_Sequence5 -> Phantoms.TTerm Syntax.ValueSet
nodeConstraint_Sequence5ValueSet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence5"),
        Core.projectionField = (Core.Name "ValueSet")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nodeConstraint_Sequence5WithListOfXsFacet :: Phantoms.TTerm Syntax.NodeConstraint_Sequence5 -> Phantoms.TTerm [Syntax.XsFacet] -> Phantoms.TTerm Syntax.NodeConstraint_Sequence5
nodeConstraint_Sequence5WithListOfXsFacet original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence5"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ValueSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence5"),
              Core.projectionField = (Core.Name "ValueSet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfXsFacet"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

nodeConstraint_Sequence5WithValueSet :: Phantoms.TTerm Syntax.NodeConstraint_Sequence5 -> Phantoms.TTerm Syntax.ValueSet -> Phantoms.TTerm Syntax.NodeConstraint_Sequence5
nodeConstraint_Sequence5WithValueSet original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence5"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ValueSet"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfXsFacet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NodeConstraint_Sequence5"),
              Core.projectionField = (Core.Name "listOfXsFacet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

nonLiteralKindBNODE :: Phantoms.TTerm Syntax.NonLiteralKind
nonLiteralKindBNODE =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NonLiteralKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "BNODE"),
        Core.fieldTerm = Core.TermUnit}}))

nonLiteralKindIRI :: Phantoms.TTerm Syntax.NonLiteralKind
nonLiteralKindIRI =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NonLiteralKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "IRI"),
        Core.fieldTerm = Core.TermUnit}}))

nonLiteralKindNONLITERAL :: Phantoms.TTerm Syntax.NonLiteralKind
nonLiteralKindNONLITERAL =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NonLiteralKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "NONLITERAL"),
        Core.fieldTerm = Core.TermUnit}}))

notStartActionShapeExprDecl :: Phantoms.TTerm Syntax.NotStartAction_ShapeExprDecl -> Phantoms.TTerm Syntax.NotStartAction
notStartActionShapeExprDecl x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NotStartAction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shapeExprDecl"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

notStartActionStart :: Phantoms.TTerm Syntax.ShapeExpression -> Phantoms.TTerm Syntax.NotStartAction
notStartActionStart x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NotStartAction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "start"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

notStartAction_ShapeExprDecl :: Phantoms.TTerm Syntax.ShapeExprLabel -> Phantoms.TTerm Syntax.NotStartAction_ShapeExprDecl_Alts -> Phantoms.TTerm Syntax.NotStartAction_ShapeExprDecl
notStartAction_ShapeExprDecl shapeExprLabel alts =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NotStartAction_ShapeExprDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ShapeExprLabel"),
          Core.fieldTerm = (Phantoms.unTTerm shapeExprLabel)},
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Phantoms.unTTerm alts)}]}))

notStartAction_ShapeExprDeclAlts :: Phantoms.TTerm Syntax.NotStartAction_ShapeExprDecl -> Phantoms.TTerm Syntax.NotStartAction_ShapeExprDecl_Alts
notStartAction_ShapeExprDeclAlts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NotStartAction_ShapeExprDecl"),
        Core.projectionField = (Core.Name "alts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

notStartAction_ShapeExprDeclShapeExprLabel :: Phantoms.TTerm Syntax.NotStartAction_ShapeExprDecl -> Phantoms.TTerm Syntax.ShapeExprLabel
notStartAction_ShapeExprDeclShapeExprLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NotStartAction_ShapeExprDecl"),
        Core.projectionField = (Core.Name "ShapeExprLabel")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

notStartAction_ShapeExprDeclWithAlts :: Phantoms.TTerm Syntax.NotStartAction_ShapeExprDecl -> Phantoms.TTerm Syntax.NotStartAction_ShapeExprDecl_Alts -> Phantoms.TTerm Syntax.NotStartAction_ShapeExprDecl
notStartAction_ShapeExprDeclWithAlts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NotStartAction_ShapeExprDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ShapeExprLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NotStartAction_ShapeExprDecl"),
              Core.projectionField = (Core.Name "ShapeExprLabel")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

notStartAction_ShapeExprDeclWithShapeExprLabel :: Phantoms.TTerm Syntax.NotStartAction_ShapeExprDecl -> Phantoms.TTerm Syntax.ShapeExprLabel -> Phantoms.TTerm Syntax.NotStartAction_ShapeExprDecl
notStartAction_ShapeExprDeclWithShapeExprLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NotStartAction_ShapeExprDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ShapeExprLabel"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NotStartAction_ShapeExprDecl"),
              Core.projectionField = (Core.Name "alts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

notStartAction_ShapeExprDecl_AltsEXTERNAL :: Phantoms.TTerm Syntax.NotStartAction_ShapeExprDecl_Alts
notStartAction_ShapeExprDecl_AltsEXTERNAL =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NotStartAction_ShapeExprDecl_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "EXTERNAL"),
        Core.fieldTerm = Core.TermUnit}}))

notStartAction_ShapeExprDecl_AltsShapeExpression :: Phantoms.TTerm Syntax.ShapeExpression -> Phantoms.TTerm Syntax.NotStartAction_ShapeExprDecl_Alts
notStartAction_ShapeExprDecl_AltsShapeExpression x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NotStartAction_ShapeExprDecl_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ShapeExpression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

numericFacetSequence :: Phantoms.TTerm Syntax.NumericFacet_Sequence -> Phantoms.TTerm Syntax.NumericFacet
numericFacetSequence x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NumericFacet"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

numericFacetSequence2 :: Phantoms.TTerm Syntax.NumericFacet_Sequence2 -> Phantoms.TTerm Syntax.NumericFacet
numericFacetSequence2 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NumericFacet"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

numericFacet_Sequence :: Phantoms.TTerm Syntax.NumericRange -> Phantoms.TTerm Syntax.NumericLiteral -> Phantoms.TTerm Syntax.NumericFacet_Sequence
numericFacet_Sequence numericRange numericLiteral =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NumericFacet_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NumericRange"),
          Core.fieldTerm = (Phantoms.unTTerm numericRange)},
        Core.Field {
          Core.fieldName = (Core.Name "NumericLiteral"),
          Core.fieldTerm = (Phantoms.unTTerm numericLiteral)}]}))

numericFacet_Sequence2 :: Phantoms.TTerm Syntax.NumericLength -> Phantoms.TTerm Syntax.Integer_ -> Phantoms.TTerm Syntax.NumericFacet_Sequence2
numericFacet_Sequence2 numericLength integer =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NumericFacet_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NumericLength"),
          Core.fieldTerm = (Phantoms.unTTerm numericLength)},
        Core.Field {
          Core.fieldName = (Core.Name "Integer"),
          Core.fieldTerm = (Phantoms.unTTerm integer)}]}))

numericFacet_Sequence2Integer :: Phantoms.TTerm Syntax.NumericFacet_Sequence2 -> Phantoms.TTerm Syntax.Integer_
numericFacet_Sequence2Integer x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NumericFacet_Sequence2"),
        Core.projectionField = (Core.Name "Integer")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

numericFacet_Sequence2NumericLength :: Phantoms.TTerm Syntax.NumericFacet_Sequence2 -> Phantoms.TTerm Syntax.NumericLength
numericFacet_Sequence2NumericLength x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NumericFacet_Sequence2"),
        Core.projectionField = (Core.Name "NumericLength")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

numericFacet_Sequence2WithInteger :: Phantoms.TTerm Syntax.NumericFacet_Sequence2 -> Phantoms.TTerm Syntax.Integer_ -> Phantoms.TTerm Syntax.NumericFacet_Sequence2
numericFacet_Sequence2WithInteger original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NumericFacet_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NumericLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NumericFacet_Sequence2"),
              Core.projectionField = (Core.Name "NumericLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Integer"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

numericFacet_Sequence2WithNumericLength :: Phantoms.TTerm Syntax.NumericFacet_Sequence2 -> Phantoms.TTerm Syntax.NumericLength -> Phantoms.TTerm Syntax.NumericFacet_Sequence2
numericFacet_Sequence2WithNumericLength original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NumericFacet_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NumericLength"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Integer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NumericFacet_Sequence2"),
              Core.projectionField = (Core.Name "Integer")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

numericFacet_SequenceNumericLiteral :: Phantoms.TTerm Syntax.NumericFacet_Sequence -> Phantoms.TTerm Syntax.NumericLiteral
numericFacet_SequenceNumericLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NumericFacet_Sequence"),
        Core.projectionField = (Core.Name "NumericLiteral")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

numericFacet_SequenceNumericRange :: Phantoms.TTerm Syntax.NumericFacet_Sequence -> Phantoms.TTerm Syntax.NumericRange
numericFacet_SequenceNumericRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NumericFacet_Sequence"),
        Core.projectionField = (Core.Name "NumericRange")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

numericFacet_SequenceWithNumericLiteral :: Phantoms.TTerm Syntax.NumericFacet_Sequence -> Phantoms.TTerm Syntax.NumericLiteral -> Phantoms.TTerm Syntax.NumericFacet_Sequence
numericFacet_SequenceWithNumericLiteral original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NumericFacet_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NumericRange"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NumericFacet_Sequence"),
              Core.projectionField = (Core.Name "NumericRange")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "NumericLiteral"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

numericFacet_SequenceWithNumericRange :: Phantoms.TTerm Syntax.NumericFacet_Sequence -> Phantoms.TTerm Syntax.NumericRange -> Phantoms.TTerm Syntax.NumericFacet_Sequence
numericFacet_SequenceWithNumericRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.NumericFacet_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NumericRange"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "NumericLiteral"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.NumericFacet_Sequence"),
              Core.projectionField = (Core.Name "NumericLiteral")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

numericLengthFRACTIONDIGITS :: Phantoms.TTerm Syntax.NumericLength
numericLengthFRACTIONDIGITS =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NumericLength"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "FRACTIONDIGITS"),
        Core.fieldTerm = Core.TermUnit}}))

numericLengthTOTALDIGITS :: Phantoms.TTerm Syntax.NumericLength
numericLengthTOTALDIGITS =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NumericLength"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "TOTALDIGITS"),
        Core.fieldTerm = Core.TermUnit}}))

numericLiteralDecimal :: Phantoms.TTerm Syntax.Decimal -> Phantoms.TTerm Syntax.NumericLiteral
numericLiteralDecimal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NumericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Decimal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

numericLiteralDouble :: Phantoms.TTerm Syntax.Double_ -> Phantoms.TTerm Syntax.NumericLiteral
numericLiteralDouble x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NumericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Double"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

numericLiteralInteger :: Phantoms.TTerm Syntax.Integer_ -> Phantoms.TTerm Syntax.NumericLiteral
numericLiteralInteger x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NumericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

numericRangeMAXEXCLUSIVE :: Phantoms.TTerm Syntax.NumericRange
numericRangeMAXEXCLUSIVE =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NumericRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "MAXEXCLUSIVE"),
        Core.fieldTerm = Core.TermUnit}}))

numericRangeMAXINCLUSIVE :: Phantoms.TTerm Syntax.NumericRange
numericRangeMAXINCLUSIVE =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NumericRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "MAXINCLUSIVE"),
        Core.fieldTerm = Core.TermUnit}}))

numericRangeMINEXCLUSIVE :: Phantoms.TTerm Syntax.NumericRange
numericRangeMINEXCLUSIVE =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NumericRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "MINEXCLUSIVE"),
        Core.fieldTerm = Core.TermUnit}}))

numericRangeMININCLUSIVE :: Phantoms.TTerm Syntax.NumericRange
numericRangeMININCLUSIVE =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.NumericRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "MININCLUSIVE"),
        Core.fieldTerm = Core.TermUnit}}))

oneOfTripleExprGroupTripleExpr :: Phantoms.TTerm Syntax.GroupTripleExpr -> Phantoms.TTerm Syntax.OneOfTripleExpr
oneOfTripleExprGroupTripleExpr x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.OneOfTripleExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "GroupTripleExpr"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

oneOfTripleExprMultiElementOneOf :: Phantoms.TTerm Syntax.MultiElementOneOf -> Phantoms.TTerm Syntax.OneOfTripleExpr
oneOfTripleExprMultiElementOneOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.OneOfTripleExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "MultiElementOneOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

percent :: Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Percent
percent hex hex2 =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Percent"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Hex"),
          Core.fieldTerm = (Phantoms.unTTerm hex)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex2"),
          Core.fieldTerm = (Phantoms.unTTerm hex2)}]}))

percentHex :: Phantoms.TTerm Syntax.Percent -> Phantoms.TTerm Syntax.Hex
percentHex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Percent"),
        Core.projectionField = (Core.Name "Hex")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

percentHex2 :: Phantoms.TTerm Syntax.Percent -> Phantoms.TTerm Syntax.Hex
percentHex2 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Percent"),
        Core.projectionField = (Core.Name "Hex2")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

percentWithHex :: Phantoms.TTerm Syntax.Percent -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Percent
percentWithHex original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Percent"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Hex"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Percent"),
              Core.projectionField = (Core.Name "Hex2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

percentWithHex2 :: Phantoms.TTerm Syntax.Percent -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Percent
percentWithHex2 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Percent"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Hex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Percent"),
              Core.projectionField = (Core.Name "Hex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex2"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

plxPercent :: Phantoms.TTerm Syntax.Percent -> Phantoms.TTerm Syntax.Plx
plxPercent x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Plx"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Percent"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

plxPnLocalEsc :: Phantoms.TTerm Syntax.PnLocalEsc -> Phantoms.TTerm Syntax.Plx
plxPnLocalEsc x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Plx"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "PnLocalEsc"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pnCharsBaseRegex :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.PnCharsBase
pnCharsBaseRegex x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PnCharsBase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pnCharsBaseRegex2 :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.PnCharsBase
pnCharsBaseRegex2 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PnCharsBase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regex2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pnCharsMinus :: Phantoms.TTerm Syntax.PnChars
pnCharsMinus =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PnChars"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Minus"),
        Core.fieldTerm = Core.TermUnit}}))

pnCharsPnCharsU :: Phantoms.TTerm Syntax.PnCharsU -> Phantoms.TTerm Syntax.PnChars
pnCharsPnCharsU x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PnChars"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "PnCharsU"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pnCharsRegex :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.PnChars
pnCharsRegex x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PnChars"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pnCharsULowbar :: Phantoms.TTerm Syntax.PnCharsU
pnCharsULowbar =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PnCharsU"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Lowbar"),
        Core.fieldTerm = Core.TermUnit}}))

pnCharsUPnCharsBase :: Phantoms.TTerm Syntax.PnCharsBase -> Phantoms.TTerm Syntax.PnCharsU
pnCharsUPnCharsBase x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PnCharsU"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "PnCharsBase"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pnLocal :: Phantoms.TTerm Syntax.PnLocal_Alts -> Phantoms.TTerm (Maybe Syntax.PnLocal_Sequence_Option) -> Phantoms.TTerm Syntax.PnLocal
pnLocal alts sequence =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.PnLocal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Phantoms.unTTerm alts)},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Phantoms.unTTerm sequence)}]}))

pnLocalAlts :: Phantoms.TTerm Syntax.PnLocal -> Phantoms.TTerm Syntax.PnLocal_Alts
pnLocalAlts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PnLocal"),
        Core.projectionField = (Core.Name "alts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pnLocalEsc :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.PnLocalEsc
pnLocalEsc x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.PnLocalEsc"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

pnLocalSequence :: Phantoms.TTerm Syntax.PnLocal -> Phantoms.TTerm (Maybe Syntax.PnLocal_Sequence_Option)
pnLocalSequence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PnLocal"),
        Core.projectionField = (Core.Name "Sequence")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pnLocalWithAlts :: Phantoms.TTerm Syntax.PnLocal -> Phantoms.TTerm Syntax.PnLocal_Alts -> Phantoms.TTerm Syntax.PnLocal
pnLocalWithAlts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.PnLocal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PnLocal"),
              Core.projectionField = (Core.Name "Sequence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pnLocalWithSequence :: Phantoms.TTerm Syntax.PnLocal -> Phantoms.TTerm (Maybe Syntax.PnLocal_Sequence_Option) -> Phantoms.TTerm Syntax.PnLocal
pnLocalWithSequence original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.PnLocal"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PnLocal"),
              Core.projectionField = (Core.Name "alts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pnLocal_AltsColon :: Phantoms.TTerm Syntax.PnLocal_Alts
pnLocal_AltsColon =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PnLocal_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Colon"),
        Core.fieldTerm = Core.TermUnit}}))

pnLocal_AltsPlx :: Phantoms.TTerm Syntax.Plx -> Phantoms.TTerm Syntax.PnLocal_Alts
pnLocal_AltsPlx x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PnLocal_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Plx"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pnLocal_AltsPnCharsU :: Phantoms.TTerm Syntax.PnCharsU -> Phantoms.TTerm Syntax.PnLocal_Alts
pnLocal_AltsPnCharsU x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PnLocal_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "PnCharsU"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pnLocal_AltsRegex :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.PnLocal_Alts
pnLocal_AltsRegex x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PnLocal_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pnLocal_Sequence_Option :: Phantoms.TTerm [Syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt] -> Phantoms.TTerm Syntax.PnLocal_Sequence_Option_Alts -> Phantoms.TTerm Syntax.PnLocal_Sequence_Option
pnLocal_Sequence_Option listOfAlts alts =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.PnLocal_Sequence_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "listOfAlts"),
          Core.fieldTerm = (Phantoms.unTTerm listOfAlts)},
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Phantoms.unTTerm alts)}]}))

pnLocal_Sequence_OptionAlts :: Phantoms.TTerm Syntax.PnLocal_Sequence_Option -> Phantoms.TTerm Syntax.PnLocal_Sequence_Option_Alts
pnLocal_Sequence_OptionAlts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PnLocal_Sequence_Option"),
        Core.projectionField = (Core.Name "alts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pnLocal_Sequence_OptionListOfAlts :: Phantoms.TTerm Syntax.PnLocal_Sequence_Option -> Phantoms.TTerm [Syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt]
pnLocal_Sequence_OptionListOfAlts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PnLocal_Sequence_Option"),
        Core.projectionField = (Core.Name "listOfAlts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pnLocal_Sequence_OptionWithAlts :: Phantoms.TTerm Syntax.PnLocal_Sequence_Option -> Phantoms.TTerm Syntax.PnLocal_Sequence_Option_Alts -> Phantoms.TTerm Syntax.PnLocal_Sequence_Option
pnLocal_Sequence_OptionWithAlts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.PnLocal_Sequence_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "listOfAlts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PnLocal_Sequence_Option"),
              Core.projectionField = (Core.Name "listOfAlts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pnLocal_Sequence_OptionWithListOfAlts :: Phantoms.TTerm Syntax.PnLocal_Sequence_Option -> Phantoms.TTerm [Syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt] -> Phantoms.TTerm Syntax.PnLocal_Sequence_Option
pnLocal_Sequence_OptionWithListOfAlts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.PnLocal_Sequence_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "listOfAlts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PnLocal_Sequence_Option"),
              Core.projectionField = (Core.Name "alts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pnLocal_Sequence_Option_AltsColon :: Phantoms.TTerm Syntax.PnLocal_Sequence_Option_Alts
pnLocal_Sequence_Option_AltsColon =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PnLocal_Sequence_Option_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Colon"),
        Core.fieldTerm = Core.TermUnit}}))

pnLocal_Sequence_Option_AltsPlx :: Phantoms.TTerm Syntax.Plx -> Phantoms.TTerm Syntax.PnLocal_Sequence_Option_Alts
pnLocal_Sequence_Option_AltsPlx x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PnLocal_Sequence_Option_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Plx"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pnLocal_Sequence_Option_AltsPnChars :: Phantoms.TTerm Syntax.PnChars -> Phantoms.TTerm Syntax.PnLocal_Sequence_Option_Alts
pnLocal_Sequence_Option_AltsPnChars x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PnLocal_Sequence_Option_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "PnChars"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pnLocal_Sequence_Option_ListOfAlts_ElmtColon :: Phantoms.TTerm Syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt
pnLocal_Sequence_Option_ListOfAlts_ElmtColon =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Colon"),
        Core.fieldTerm = Core.TermUnit}}))

pnLocal_Sequence_Option_ListOfAlts_ElmtPeriod :: Phantoms.TTerm Syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt
pnLocal_Sequence_Option_ListOfAlts_ElmtPeriod =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Period"),
        Core.fieldTerm = Core.TermUnit}}))

pnLocal_Sequence_Option_ListOfAlts_ElmtPlx :: Phantoms.TTerm Syntax.Plx -> Phantoms.TTerm Syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt
pnLocal_Sequence_Option_ListOfAlts_ElmtPlx x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Plx"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pnLocal_Sequence_Option_ListOfAlts_ElmtPnChars :: Phantoms.TTerm Syntax.PnChars -> Phantoms.TTerm Syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt
pnLocal_Sequence_Option_ListOfAlts_ElmtPnChars x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "PnChars"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pnPrefix :: Phantoms.TTerm Syntax.PnCharsBase -> Phantoms.TTerm (Maybe Syntax.PnPrefix_Sequence_Option) -> Phantoms.TTerm Syntax.PnPrefix
pnPrefix pnCharsBase sequence =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.PnPrefix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "PnCharsBase"),
          Core.fieldTerm = (Phantoms.unTTerm pnCharsBase)},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Phantoms.unTTerm sequence)}]}))

pnPrefixPnCharsBase :: Phantoms.TTerm Syntax.PnPrefix -> Phantoms.TTerm Syntax.PnCharsBase
pnPrefixPnCharsBase x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PnPrefix"),
        Core.projectionField = (Core.Name "PnCharsBase")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pnPrefixSequence :: Phantoms.TTerm Syntax.PnPrefix -> Phantoms.TTerm (Maybe Syntax.PnPrefix_Sequence_Option)
pnPrefixSequence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PnPrefix"),
        Core.projectionField = (Core.Name "Sequence")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pnPrefixWithPnCharsBase :: Phantoms.TTerm Syntax.PnPrefix -> Phantoms.TTerm Syntax.PnCharsBase -> Phantoms.TTerm Syntax.PnPrefix
pnPrefixWithPnCharsBase original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.PnPrefix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "PnCharsBase"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PnPrefix"),
              Core.projectionField = (Core.Name "Sequence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pnPrefixWithSequence :: Phantoms.TTerm Syntax.PnPrefix -> Phantoms.TTerm (Maybe Syntax.PnPrefix_Sequence_Option) -> Phantoms.TTerm Syntax.PnPrefix
pnPrefixWithSequence original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.PnPrefix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "PnCharsBase"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PnPrefix"),
              Core.projectionField = (Core.Name "PnCharsBase")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pnPrefix_Sequence_Option :: Phantoms.TTerm Syntax.PnPrefix_Sequence_Option_Alts -> Phantoms.TTerm Syntax.PnChars -> Phantoms.TTerm Syntax.PnPrefix_Sequence_Option
pnPrefix_Sequence_Option alts pnChars =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.PnPrefix_Sequence_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Phantoms.unTTerm alts)},
        Core.Field {
          Core.fieldName = (Core.Name "PnChars"),
          Core.fieldTerm = (Phantoms.unTTerm pnChars)}]}))

pnPrefix_Sequence_OptionAlts :: Phantoms.TTerm Syntax.PnPrefix_Sequence_Option -> Phantoms.TTerm Syntax.PnPrefix_Sequence_Option_Alts
pnPrefix_Sequence_OptionAlts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PnPrefix_Sequence_Option"),
        Core.projectionField = (Core.Name "alts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pnPrefix_Sequence_OptionPnChars :: Phantoms.TTerm Syntax.PnPrefix_Sequence_Option -> Phantoms.TTerm Syntax.PnChars
pnPrefix_Sequence_OptionPnChars x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PnPrefix_Sequence_Option"),
        Core.projectionField = (Core.Name "PnChars")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pnPrefix_Sequence_OptionWithAlts :: Phantoms.TTerm Syntax.PnPrefix_Sequence_Option -> Phantoms.TTerm Syntax.PnPrefix_Sequence_Option_Alts -> Phantoms.TTerm Syntax.PnPrefix_Sequence_Option
pnPrefix_Sequence_OptionWithAlts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.PnPrefix_Sequence_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "PnChars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PnPrefix_Sequence_Option"),
              Core.projectionField = (Core.Name "PnChars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pnPrefix_Sequence_OptionWithPnChars :: Phantoms.TTerm Syntax.PnPrefix_Sequence_Option -> Phantoms.TTerm Syntax.PnChars -> Phantoms.TTerm Syntax.PnPrefix_Sequence_Option
pnPrefix_Sequence_OptionWithPnChars original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.PnPrefix_Sequence_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PnPrefix_Sequence_Option"),
              Core.projectionField = (Core.Name "alts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "PnChars"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pnPrefix_Sequence_Option_AltsPeriod :: Phantoms.TTerm Syntax.PnPrefix_Sequence_Option_Alts
pnPrefix_Sequence_Option_AltsPeriod =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PnPrefix_Sequence_Option_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Period"),
        Core.fieldTerm = Core.TermUnit}}))

pnPrefix_Sequence_Option_AltsPnChars :: Phantoms.TTerm Syntax.PnChars -> Phantoms.TTerm Syntax.PnPrefix_Sequence_Option_Alts
pnPrefix_Sequence_Option_AltsPnChars x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PnPrefix_Sequence_Option_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "PnChars"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pnameLn :: Phantoms.TTerm Syntax.PnameNs -> Phantoms.TTerm Syntax.PnLocal -> Phantoms.TTerm Syntax.PnameLn
pnameLn pnameNs pnLocal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.PnameLn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "PnameNs"),
          Core.fieldTerm = (Phantoms.unTTerm pnameNs)},
        Core.Field {
          Core.fieldName = (Core.Name "PnLocal"),
          Core.fieldTerm = (Phantoms.unTTerm pnLocal)}]}))

pnameLnPnLocal :: Phantoms.TTerm Syntax.PnameLn -> Phantoms.TTerm Syntax.PnLocal
pnameLnPnLocal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PnameLn"),
        Core.projectionField = (Core.Name "PnLocal")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pnameLnPnameNs :: Phantoms.TTerm Syntax.PnameLn -> Phantoms.TTerm Syntax.PnameNs
pnameLnPnameNs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PnameLn"),
        Core.projectionField = (Core.Name "PnameNs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pnameLnWithPnLocal :: Phantoms.TTerm Syntax.PnameLn -> Phantoms.TTerm Syntax.PnLocal -> Phantoms.TTerm Syntax.PnameLn
pnameLnWithPnLocal original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.PnameLn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "PnameNs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PnameLn"),
              Core.projectionField = (Core.Name "PnameNs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "PnLocal"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pnameLnWithPnameNs :: Phantoms.TTerm Syntax.PnameLn -> Phantoms.TTerm Syntax.PnameNs -> Phantoms.TTerm Syntax.PnameLn
pnameLnWithPnameNs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.PnameLn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "PnameNs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "PnLocal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PnameLn"),
              Core.projectionField = (Core.Name "PnLocal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pnameNs :: Phantoms.TTerm (Maybe Syntax.PnPrefix) -> Phantoms.TTerm Syntax.PnameNs
pnameNs x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.PnameNs"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

predicateIri :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.Predicate
predicateIri x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Predicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Iri"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

predicateRdfType :: Phantoms.TTerm Syntax.RdfType -> Phantoms.TTerm Syntax.Predicate
predicateRdfType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Predicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "RdfType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

prefixDecl :: Phantoms.TTerm Syntax.PnameNs -> Phantoms.TTerm Syntax.IriRef -> Phantoms.TTerm Syntax.PrefixDecl
prefixDecl pnameNs iriRef =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.PrefixDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "PnameNs"),
          Core.fieldTerm = (Phantoms.unTTerm pnameNs)},
        Core.Field {
          Core.fieldName = (Core.Name "IriRef"),
          Core.fieldTerm = (Phantoms.unTTerm iriRef)}]}))

prefixDeclIriRef :: Phantoms.TTerm Syntax.PrefixDecl -> Phantoms.TTerm Syntax.IriRef
prefixDeclIriRef x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PrefixDecl"),
        Core.projectionField = (Core.Name "IriRef")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

prefixDeclPnameNs :: Phantoms.TTerm Syntax.PrefixDecl -> Phantoms.TTerm Syntax.PnameNs
prefixDeclPnameNs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PrefixDecl"),
        Core.projectionField = (Core.Name "PnameNs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

prefixDeclWithIriRef :: Phantoms.TTerm Syntax.PrefixDecl -> Phantoms.TTerm Syntax.IriRef -> Phantoms.TTerm Syntax.PrefixDecl
prefixDeclWithIriRef original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.PrefixDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "PnameNs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PrefixDecl"),
              Core.projectionField = (Core.Name "PnameNs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "IriRef"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

prefixDeclWithPnameNs :: Phantoms.TTerm Syntax.PrefixDecl -> Phantoms.TTerm Syntax.PnameNs -> Phantoms.TTerm Syntax.PrefixDecl
prefixDeclWithPnameNs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.PrefixDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "PnameNs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "IriRef"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.PrefixDecl"),
              Core.projectionField = (Core.Name "IriRef")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

prefixedNamePnameLn :: Phantoms.TTerm Syntax.PnameLn -> Phantoms.TTerm Syntax.PrefixedName
prefixedNamePnameLn x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PrefixedName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "PnameLn"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

prefixedNamePnameNs :: Phantoms.TTerm Syntax.PnameNs -> Phantoms.TTerm Syntax.PrefixedName
prefixedNamePnameNs x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.PrefixedName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "PnameNs"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

rdfLiteral :: Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm (Maybe Syntax.RdfLiteral_Alts_Option) -> Phantoms.TTerm Syntax.RdfLiteral
rdfLiteral string alts =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.RdfLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "String"),
          Core.fieldTerm = (Phantoms.unTTerm string)},
        Core.Field {
          Core.fieldName = (Core.Name "Alts"),
          Core.fieldTerm = (Phantoms.unTTerm alts)}]}))

rdfLiteralAlts :: Phantoms.TTerm Syntax.RdfLiteral -> Phantoms.TTerm (Maybe Syntax.RdfLiteral_Alts_Option)
rdfLiteralAlts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.RdfLiteral"),
        Core.projectionField = (Core.Name "Alts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rdfLiteralString :: Phantoms.TTerm Syntax.RdfLiteral -> Phantoms.TTerm Syntax.String_
rdfLiteralString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.RdfLiteral"),
        Core.projectionField = (Core.Name "String")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rdfLiteralWithAlts :: Phantoms.TTerm Syntax.RdfLiteral -> Phantoms.TTerm (Maybe Syntax.RdfLiteral_Alts_Option) -> Phantoms.TTerm Syntax.RdfLiteral
rdfLiteralWithAlts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.RdfLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "String"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.RdfLiteral"),
              Core.projectionField = (Core.Name "String")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Alts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

rdfLiteralWithString :: Phantoms.TTerm Syntax.RdfLiteral -> Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm Syntax.RdfLiteral
rdfLiteralWithString original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.RdfLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "String"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Alts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.RdfLiteral"),
              Core.projectionField = (Core.Name "Alts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rdfLiteral_Alts_OptionLangTag :: Phantoms.TTerm Syntax.LangTag -> Phantoms.TTerm Syntax.RdfLiteral_Alts_Option
rdfLiteral_Alts_OptionLangTag x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.RdfLiteral_Alts_Option"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "LangTag"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

rdfLiteral_Alts_OptionSequence :: Phantoms.TTerm Syntax.Datatype -> Phantoms.TTerm Syntax.RdfLiteral_Alts_Option
rdfLiteral_Alts_OptionSequence x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.RdfLiteral_Alts_Option"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

rdfType :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.RdfType
rdfType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.RdfType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

regexp :: Phantoms.TTerm [Syntax.Regexp_ListOfAlts_Elmt] -> Phantoms.TTerm [String] -> Phantoms.TTerm Syntax.Regexp
regexp listOfAlts listOfRegex =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Regexp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "listOfAlts"),
          Core.fieldTerm = (Phantoms.unTTerm listOfAlts)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfRegex"),
          Core.fieldTerm = (Phantoms.unTTerm listOfRegex)}]}))

regexpListOfAlts :: Phantoms.TTerm Syntax.Regexp -> Phantoms.TTerm [Syntax.Regexp_ListOfAlts_Elmt]
regexpListOfAlts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Regexp"),
        Core.projectionField = (Core.Name "listOfAlts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

regexpListOfRegex :: Phantoms.TTerm Syntax.Regexp -> Phantoms.TTerm [String]
regexpListOfRegex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Regexp"),
        Core.projectionField = (Core.Name "listOfRegex")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

regexpWithListOfAlts :: Phantoms.TTerm Syntax.Regexp -> Phantoms.TTerm [Syntax.Regexp_ListOfAlts_Elmt] -> Phantoms.TTerm Syntax.Regexp
regexpWithListOfAlts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Regexp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "listOfAlts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfRegex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Regexp"),
              Core.projectionField = (Core.Name "listOfRegex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

regexpWithListOfRegex :: Phantoms.TTerm Syntax.Regexp -> Phantoms.TTerm [String] -> Phantoms.TTerm Syntax.Regexp
regexpWithListOfRegex original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Regexp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "listOfAlts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Regexp"),
              Core.projectionField = (Core.Name "listOfAlts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfRegex"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

regexp_ListOfAlts_ElmtRegex :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Regexp_ListOfAlts_Elmt
regexp_ListOfAlts_ElmtRegex x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Regexp_ListOfAlts_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

regexp_ListOfAlts_ElmtSequence :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Regexp_ListOfAlts_Elmt
regexp_ListOfAlts_ElmtSequence x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Regexp_ListOfAlts_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

regexp_ListOfAlts_ElmtUchar :: Phantoms.TTerm Syntax.Uchar -> Phantoms.TTerm Syntax.Regexp_ListOfAlts_Elmt
regexp_ListOfAlts_ElmtUchar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Regexp_ListOfAlts_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Uchar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

repeatRange :: Phantoms.TTerm Syntax.Integer_ -> Phantoms.TTerm (Maybe (Maybe (Maybe Syntax.RepeatRange_Sequence_Option_Option_Option))) -> Phantoms.TTerm Syntax.RepeatRange
repeatRange integer sequence =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.RepeatRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Integer"),
          Core.fieldTerm = (Phantoms.unTTerm integer)},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Phantoms.unTTerm sequence)}]}))

repeatRangeInteger :: Phantoms.TTerm Syntax.RepeatRange -> Phantoms.TTerm Syntax.Integer_
repeatRangeInteger x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.RepeatRange"),
        Core.projectionField = (Core.Name "Integer")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

repeatRangeSequence :: Phantoms.TTerm Syntax.RepeatRange -> Phantoms.TTerm (Maybe (Maybe (Maybe Syntax.RepeatRange_Sequence_Option_Option_Option)))
repeatRangeSequence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.RepeatRange"),
        Core.projectionField = (Core.Name "Sequence")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

repeatRangeWithInteger :: Phantoms.TTerm Syntax.RepeatRange -> Phantoms.TTerm Syntax.Integer_ -> Phantoms.TTerm Syntax.RepeatRange
repeatRangeWithInteger original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.RepeatRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Integer"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.RepeatRange"),
              Core.projectionField = (Core.Name "Sequence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

repeatRangeWithSequence :: Phantoms.TTerm Syntax.RepeatRange -> Phantoms.TTerm (Maybe (Maybe (Maybe Syntax.RepeatRange_Sequence_Option_Option_Option))) -> Phantoms.TTerm Syntax.RepeatRange
repeatRangeWithSequence original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.RepeatRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Integer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.RepeatRange"),
              Core.projectionField = (Core.Name "Integer")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

repeatRange_Sequence_Option_Option_OptionAst :: Phantoms.TTerm Syntax.RepeatRange_Sequence_Option_Option_Option
repeatRange_Sequence_Option_Option_OptionAst =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.RepeatRange_Sequence_Option_Option_Option"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Ast"),
        Core.fieldTerm = Core.TermUnit}}))

repeatRange_Sequence_Option_Option_OptionInteger :: Phantoms.TTerm Syntax.Integer_ -> Phantoms.TTerm Syntax.RepeatRange_Sequence_Option_Option_Option
repeatRange_Sequence_Option_Option_OptionInteger x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.RepeatRange_Sequence_Option_Option_Option"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

semanticActions :: Phantoms.TTerm [Syntax.CodeDecl] -> Phantoms.TTerm Syntax.SemanticActions
semanticActions x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.SemanticActions"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

senseFlags :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.SenseFlags
senseFlags x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.SenseFlags"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

shapeAnd :: Phantoms.TTerm Syntax.ShapeNot -> Phantoms.TTerm [Syntax.ShapeNot] -> Phantoms.TTerm Syntax.ShapeAnd
shapeAnd shapeNot listOfSequence =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShapeAnd"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ShapeNot"),
          Core.fieldTerm = (Phantoms.unTTerm shapeNot)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfSequence"),
          Core.fieldTerm = (Phantoms.unTTerm listOfSequence)}]}))

shapeAndListOfSequence :: Phantoms.TTerm Syntax.ShapeAnd -> Phantoms.TTerm [Syntax.ShapeNot]
shapeAndListOfSequence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeAnd"),
        Core.projectionField = (Core.Name "listOfSequence")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

shapeAndShapeNot :: Phantoms.TTerm Syntax.ShapeAnd -> Phantoms.TTerm Syntax.ShapeNot
shapeAndShapeNot x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeAnd"),
        Core.projectionField = (Core.Name "ShapeNot")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

shapeAndWithListOfSequence :: Phantoms.TTerm Syntax.ShapeAnd -> Phantoms.TTerm [Syntax.ShapeNot] -> Phantoms.TTerm Syntax.ShapeAnd
shapeAndWithListOfSequence original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShapeAnd"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ShapeNot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeAnd"),
              Core.projectionField = (Core.Name "ShapeNot")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfSequence"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

shapeAndWithShapeNot :: Phantoms.TTerm Syntax.ShapeAnd -> Phantoms.TTerm Syntax.ShapeNot -> Phantoms.TTerm Syntax.ShapeAnd
shapeAndWithShapeNot original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShapeAnd"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ShapeNot"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfSequence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeAnd"),
              Core.projectionField = (Core.Name "listOfSequence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

shapeAtomPeriod :: Phantoms.TTerm Syntax.ShapeAtom
shapeAtomPeriod =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.ShapeAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Period"),
        Core.fieldTerm = Core.TermUnit}}))

shapeAtomSequence :: Phantoms.TTerm Syntax.ShapeAtom_Sequence -> Phantoms.TTerm Syntax.ShapeAtom
shapeAtomSequence x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.ShapeAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

shapeAtomSequence2 :: Phantoms.TTerm Syntax.ShapeExpression -> Phantoms.TTerm Syntax.ShapeAtom
shapeAtomSequence2 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.ShapeAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

shapeAtomShapeOrRef :: Phantoms.TTerm Syntax.ShapeOrRef -> Phantoms.TTerm Syntax.ShapeAtom
shapeAtomShapeOrRef x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.ShapeAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ShapeOrRef"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

shapeAtom_Sequence :: Phantoms.TTerm Syntax.NodeConstraint -> Phantoms.TTerm (Maybe Syntax.ShapeOrRef) -> Phantoms.TTerm Syntax.ShapeAtom_Sequence
shapeAtom_Sequence nodeConstraint shapeOrRef =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShapeAtom_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NodeConstraint"),
          Core.fieldTerm = (Phantoms.unTTerm nodeConstraint)},
        Core.Field {
          Core.fieldName = (Core.Name "ShapeOrRef"),
          Core.fieldTerm = (Phantoms.unTTerm shapeOrRef)}]}))

shapeAtom_SequenceNodeConstraint :: Phantoms.TTerm Syntax.ShapeAtom_Sequence -> Phantoms.TTerm Syntax.NodeConstraint
shapeAtom_SequenceNodeConstraint x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeAtom_Sequence"),
        Core.projectionField = (Core.Name "NodeConstraint")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

shapeAtom_SequenceShapeOrRef :: Phantoms.TTerm Syntax.ShapeAtom_Sequence -> Phantoms.TTerm (Maybe Syntax.ShapeOrRef)
shapeAtom_SequenceShapeOrRef x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeAtom_Sequence"),
        Core.projectionField = (Core.Name "ShapeOrRef")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

shapeAtom_SequenceWithNodeConstraint :: Phantoms.TTerm Syntax.ShapeAtom_Sequence -> Phantoms.TTerm Syntax.NodeConstraint -> Phantoms.TTerm Syntax.ShapeAtom_Sequence
shapeAtom_SequenceWithNodeConstraint original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShapeAtom_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NodeConstraint"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ShapeOrRef"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeAtom_Sequence"),
              Core.projectionField = (Core.Name "ShapeOrRef")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

shapeAtom_SequenceWithShapeOrRef :: Phantoms.TTerm Syntax.ShapeAtom_Sequence -> Phantoms.TTerm (Maybe Syntax.ShapeOrRef) -> Phantoms.TTerm Syntax.ShapeAtom_Sequence
shapeAtom_SequenceWithShapeOrRef original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShapeAtom_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NodeConstraint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeAtom_Sequence"),
              Core.projectionField = (Core.Name "NodeConstraint")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ShapeOrRef"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

shapeDefinition :: Phantoms.TTerm [Syntax.ShapeDefinition_ListOfAlts_Elmt] -> Phantoms.TTerm (Maybe Syntax.TripleExpression) -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.SemanticActions -> Phantoms.TTerm Syntax.ShapeDefinition
shapeDefinition listOfAlts tripleExpression listOfAnnotation semanticActions =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "listOfAlts"),
          Core.fieldTerm = (Phantoms.unTTerm listOfAlts)},
        Core.Field {
          Core.fieldName = (Core.Name "TripleExpression"),
          Core.fieldTerm = (Phantoms.unTTerm tripleExpression)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfAnnotation"),
          Core.fieldTerm = (Phantoms.unTTerm listOfAnnotation)},
        Core.Field {
          Core.fieldName = (Core.Name "SemanticActions"),
          Core.fieldTerm = (Phantoms.unTTerm semanticActions)}]}))

shapeDefinitionListOfAlts :: Phantoms.TTerm Syntax.ShapeDefinition -> Phantoms.TTerm [Syntax.ShapeDefinition_ListOfAlts_Elmt]
shapeDefinitionListOfAlts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
        Core.projectionField = (Core.Name "listOfAlts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

shapeDefinitionListOfAnnotation :: Phantoms.TTerm Syntax.ShapeDefinition -> Phantoms.TTerm [Syntax.Annotation]
shapeDefinitionListOfAnnotation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
        Core.projectionField = (Core.Name "listOfAnnotation")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

shapeDefinitionSemanticActions :: Phantoms.TTerm Syntax.ShapeDefinition -> Phantoms.TTerm Syntax.SemanticActions
shapeDefinitionSemanticActions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
        Core.projectionField = (Core.Name "SemanticActions")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

shapeDefinitionTripleExpression :: Phantoms.TTerm Syntax.ShapeDefinition -> Phantoms.TTerm (Maybe Syntax.TripleExpression)
shapeDefinitionTripleExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
        Core.projectionField = (Core.Name "TripleExpression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

shapeDefinitionWithListOfAlts :: Phantoms.TTerm Syntax.ShapeDefinition -> Phantoms.TTerm [Syntax.ShapeDefinition_ListOfAlts_Elmt] -> Phantoms.TTerm Syntax.ShapeDefinition
shapeDefinitionWithListOfAlts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "listOfAlts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "TripleExpression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
              Core.projectionField = (Core.Name "TripleExpression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfAnnotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
              Core.projectionField = (Core.Name "listOfAnnotation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SemanticActions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
              Core.projectionField = (Core.Name "SemanticActions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

shapeDefinitionWithListOfAnnotation :: Phantoms.TTerm Syntax.ShapeDefinition -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.ShapeDefinition
shapeDefinitionWithListOfAnnotation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "listOfAlts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
              Core.projectionField = (Core.Name "listOfAlts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "TripleExpression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
              Core.projectionField = (Core.Name "TripleExpression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfAnnotation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "SemanticActions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
              Core.projectionField = (Core.Name "SemanticActions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

shapeDefinitionWithSemanticActions :: Phantoms.TTerm Syntax.ShapeDefinition -> Phantoms.TTerm Syntax.SemanticActions -> Phantoms.TTerm Syntax.ShapeDefinition
shapeDefinitionWithSemanticActions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "listOfAlts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
              Core.projectionField = (Core.Name "listOfAlts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "TripleExpression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
              Core.projectionField = (Core.Name "TripleExpression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfAnnotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
              Core.projectionField = (Core.Name "listOfAnnotation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SemanticActions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

shapeDefinitionWithTripleExpression :: Phantoms.TTerm Syntax.ShapeDefinition -> Phantoms.TTerm (Maybe Syntax.TripleExpression) -> Phantoms.TTerm Syntax.ShapeDefinition
shapeDefinitionWithTripleExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "listOfAlts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
              Core.projectionField = (Core.Name "listOfAlts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "TripleExpression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfAnnotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
              Core.projectionField = (Core.Name "listOfAnnotation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SemanticActions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition"),
              Core.projectionField = (Core.Name "SemanticActions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

shapeDefinition_ListOfAlts_ElmtCLOSED :: Phantoms.TTerm Syntax.ShapeDefinition_ListOfAlts_Elmt
shapeDefinition_ListOfAlts_ElmtCLOSED =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition_ListOfAlts_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "CLOSED"),
        Core.fieldTerm = Core.TermUnit}}))

shapeDefinition_ListOfAlts_ElmtExtraPropertySet :: Phantoms.TTerm Syntax.ExtraPropertySet -> Phantoms.TTerm Syntax.ShapeDefinition_ListOfAlts_Elmt
shapeDefinition_ListOfAlts_ElmtExtraPropertySet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition_ListOfAlts_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ExtraPropertySet"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

shapeDefinition_ListOfAlts_ElmtIncludeSet :: Phantoms.TTerm Syntax.IncludeSet -> Phantoms.TTerm Syntax.ShapeDefinition_ListOfAlts_Elmt
shapeDefinition_ListOfAlts_ElmtIncludeSet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.ShapeDefinition_ListOfAlts_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "IncludeSet"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

shapeExprLabelBlankNode :: Phantoms.TTerm Syntax.BlankNode -> Phantoms.TTerm Syntax.ShapeExprLabel
shapeExprLabelBlankNode x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.ShapeExprLabel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "BlankNode"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

shapeExprLabelIri :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.ShapeExprLabel
shapeExprLabelIri x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.ShapeExprLabel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Iri"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

shapeExpression :: Phantoms.TTerm Syntax.ShapeOr -> Phantoms.TTerm Syntax.ShapeExpression
shapeExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.ShapeExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

shapeNot :: Phantoms.TTerm (Maybe ()) -> Phantoms.TTerm Syntax.ShapeAtom -> Phantoms.TTerm Syntax.ShapeNot
shapeNot nOT shapeAtom =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShapeNot"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NOT"),
          Core.fieldTerm = (Phantoms.unTTerm nOT)},
        Core.Field {
          Core.fieldName = (Core.Name "ShapeAtom"),
          Core.fieldTerm = (Phantoms.unTTerm shapeAtom)}]}))

shapeNotNOT :: Phantoms.TTerm Syntax.ShapeNot -> Phantoms.TTerm (Maybe ())
shapeNotNOT x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeNot"),
        Core.projectionField = (Core.Name "NOT")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

shapeNotShapeAtom :: Phantoms.TTerm Syntax.ShapeNot -> Phantoms.TTerm Syntax.ShapeAtom
shapeNotShapeAtom x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeNot"),
        Core.projectionField = (Core.Name "ShapeAtom")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

shapeNotWithNOT :: Phantoms.TTerm Syntax.ShapeNot -> Phantoms.TTerm (Maybe ()) -> Phantoms.TTerm Syntax.ShapeNot
shapeNotWithNOT original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShapeNot"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NOT"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ShapeAtom"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeNot"),
              Core.projectionField = (Core.Name "ShapeAtom")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

shapeNotWithShapeAtom :: Phantoms.TTerm Syntax.ShapeNot -> Phantoms.TTerm Syntax.ShapeAtom -> Phantoms.TTerm Syntax.ShapeNot
shapeNotWithShapeAtom original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShapeNot"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NOT"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeNot"),
              Core.projectionField = (Core.Name "NOT")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ShapeAtom"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

shapeOr :: Phantoms.TTerm Syntax.ShapeAnd -> Phantoms.TTerm [Syntax.ShapeAnd] -> Phantoms.TTerm Syntax.ShapeOr
shapeOr shapeAnd listOfSequence =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShapeOr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ShapeAnd"),
          Core.fieldTerm = (Phantoms.unTTerm shapeAnd)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfSequence"),
          Core.fieldTerm = (Phantoms.unTTerm listOfSequence)}]}))

shapeOrListOfSequence :: Phantoms.TTerm Syntax.ShapeOr -> Phantoms.TTerm [Syntax.ShapeAnd]
shapeOrListOfSequence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeOr"),
        Core.projectionField = (Core.Name "listOfSequence")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

shapeOrRefAtpNameLn :: Phantoms.TTerm Syntax.AtpNameLn -> Phantoms.TTerm Syntax.ShapeOrRef
shapeOrRefAtpNameLn x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.ShapeOrRef"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "AtpNameLn"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

shapeOrRefAtpNameNs :: Phantoms.TTerm Syntax.AtpNameNs -> Phantoms.TTerm Syntax.ShapeOrRef
shapeOrRefAtpNameNs x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.ShapeOrRef"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "AtpNameNs"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

shapeOrRefSequence :: Phantoms.TTerm Syntax.ShapeExprLabel -> Phantoms.TTerm Syntax.ShapeOrRef
shapeOrRefSequence x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.ShapeOrRef"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

shapeOrRefShapeDefinition :: Phantoms.TTerm Syntax.ShapeDefinition -> Phantoms.TTerm Syntax.ShapeOrRef
shapeOrRefShapeDefinition x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.ShapeOrRef"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ShapeDefinition"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

shapeOrShapeAnd :: Phantoms.TTerm Syntax.ShapeOr -> Phantoms.TTerm Syntax.ShapeAnd
shapeOrShapeAnd x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeOr"),
        Core.projectionField = (Core.Name "ShapeAnd")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

shapeOrWithListOfSequence :: Phantoms.TTerm Syntax.ShapeOr -> Phantoms.TTerm [Syntax.ShapeAnd] -> Phantoms.TTerm Syntax.ShapeOr
shapeOrWithListOfSequence original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShapeOr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ShapeAnd"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeOr"),
              Core.projectionField = (Core.Name "ShapeAnd")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfSequence"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

shapeOrWithShapeAnd :: Phantoms.TTerm Syntax.ShapeOr -> Phantoms.TTerm Syntax.ShapeAnd -> Phantoms.TTerm Syntax.ShapeOr
shapeOrWithShapeAnd original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShapeOr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ShapeAnd"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfSequence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShapeOr"),
              Core.projectionField = (Core.Name "listOfSequence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

shexDoc :: Phantoms.TTerm [Syntax.Directive] -> Phantoms.TTerm (Maybe Syntax.ShexDoc_Sequence_Option) -> Phantoms.TTerm Syntax.PrefixDecl -> Phantoms.TTerm Syntax.ShexDoc
shexDoc listOfDirective sequence prefixDecl =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShexDoc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "listOfDirective"),
          Core.fieldTerm = (Phantoms.unTTerm listOfDirective)},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Phantoms.unTTerm sequence)},
        Core.Field {
          Core.fieldName = (Core.Name "PrefixDecl"),
          Core.fieldTerm = (Phantoms.unTTerm prefixDecl)}]}))

shexDocListOfDirective :: Phantoms.TTerm Syntax.ShexDoc -> Phantoms.TTerm [Syntax.Directive]
shexDocListOfDirective x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShexDoc"),
        Core.projectionField = (Core.Name "listOfDirective")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

shexDocPrefixDecl :: Phantoms.TTerm Syntax.ShexDoc -> Phantoms.TTerm Syntax.PrefixDecl
shexDocPrefixDecl x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShexDoc"),
        Core.projectionField = (Core.Name "PrefixDecl")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

shexDocSequence :: Phantoms.TTerm Syntax.ShexDoc -> Phantoms.TTerm (Maybe Syntax.ShexDoc_Sequence_Option)
shexDocSequence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShexDoc"),
        Core.projectionField = (Core.Name "Sequence")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

shexDocWithListOfDirective :: Phantoms.TTerm Syntax.ShexDoc -> Phantoms.TTerm [Syntax.Directive] -> Phantoms.TTerm Syntax.ShexDoc
shexDocWithListOfDirective original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShexDoc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "listOfDirective"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShexDoc"),
              Core.projectionField = (Core.Name "Sequence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "PrefixDecl"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShexDoc"),
              Core.projectionField = (Core.Name "PrefixDecl")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

shexDocWithPrefixDecl :: Phantoms.TTerm Syntax.ShexDoc -> Phantoms.TTerm Syntax.PrefixDecl -> Phantoms.TTerm Syntax.ShexDoc
shexDocWithPrefixDecl original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShexDoc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "listOfDirective"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShexDoc"),
              Core.projectionField = (Core.Name "listOfDirective")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShexDoc"),
              Core.projectionField = (Core.Name "Sequence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "PrefixDecl"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

shexDocWithSequence :: Phantoms.TTerm Syntax.ShexDoc -> Phantoms.TTerm (Maybe Syntax.ShexDoc_Sequence_Option) -> Phantoms.TTerm Syntax.ShexDoc
shexDocWithSequence original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShexDoc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "listOfDirective"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShexDoc"),
              Core.projectionField = (Core.Name "listOfDirective")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "PrefixDecl"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShexDoc"),
              Core.projectionField = (Core.Name "PrefixDecl")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

shexDoc_Sequence_Option :: Phantoms.TTerm Syntax.ShexDoc_Sequence_Option_Alts -> Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.ShexDoc_Sequence_Option
shexDoc_Sequence_Option alts listOfStatement =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShexDoc_Sequence_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Phantoms.unTTerm alts)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfStatement"),
          Core.fieldTerm = (Phantoms.unTTerm listOfStatement)}]}))

shexDoc_Sequence_OptionAlts :: Phantoms.TTerm Syntax.ShexDoc_Sequence_Option -> Phantoms.TTerm Syntax.ShexDoc_Sequence_Option_Alts
shexDoc_Sequence_OptionAlts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShexDoc_Sequence_Option"),
        Core.projectionField = (Core.Name "alts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

shexDoc_Sequence_OptionListOfStatement :: Phantoms.TTerm Syntax.ShexDoc_Sequence_Option -> Phantoms.TTerm [Syntax.Statement]
shexDoc_Sequence_OptionListOfStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShexDoc_Sequence_Option"),
        Core.projectionField = (Core.Name "listOfStatement")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

shexDoc_Sequence_OptionWithAlts :: Phantoms.TTerm Syntax.ShexDoc_Sequence_Option -> Phantoms.TTerm Syntax.ShexDoc_Sequence_Option_Alts -> Phantoms.TTerm Syntax.ShexDoc_Sequence_Option
shexDoc_Sequence_OptionWithAlts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShexDoc_Sequence_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfStatement"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShexDoc_Sequence_Option"),
              Core.projectionField = (Core.Name "listOfStatement")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

shexDoc_Sequence_OptionWithListOfStatement :: Phantoms.TTerm Syntax.ShexDoc_Sequence_Option -> Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.ShexDoc_Sequence_Option
shexDoc_Sequence_OptionWithListOfStatement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.ShexDoc_Sequence_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.ShexDoc_Sequence_Option"),
              Core.projectionField = (Core.Name "alts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfStatement"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

shexDoc_Sequence_Option_AltsNotStartAction :: Phantoms.TTerm Syntax.NotStartAction -> Phantoms.TTerm Syntax.ShexDoc_Sequence_Option_Alts
shexDoc_Sequence_Option_AltsNotStartAction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.ShexDoc_Sequence_Option_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "NotStartAction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

shexDoc_Sequence_Option_AltsStartActions :: Phantoms.TTerm Syntax.StartActions -> Phantoms.TTerm Syntax.ShexDoc_Sequence_Option_Alts
shexDoc_Sequence_Option_AltsStartActions x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.ShexDoc_Sequence_Option_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "StartActions"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

singleElementGroup :: Phantoms.TTerm Syntax.UnaryTripleExpr -> Phantoms.TTerm (Maybe ()) -> Phantoms.TTerm Syntax.SingleElementGroup
singleElementGroup unaryTripleExpr semi =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.SingleElementGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "UnaryTripleExpr"),
          Core.fieldTerm = (Phantoms.unTTerm unaryTripleExpr)},
        Core.Field {
          Core.fieldName = (Core.Name "Semi"),
          Core.fieldTerm = (Phantoms.unTTerm semi)}]}))

singleElementGroupSemi :: Phantoms.TTerm Syntax.SingleElementGroup -> Phantoms.TTerm (Maybe ())
singleElementGroupSemi x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.SingleElementGroup"),
        Core.projectionField = (Core.Name "Semi")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

singleElementGroupUnaryTripleExpr :: Phantoms.TTerm Syntax.SingleElementGroup -> Phantoms.TTerm Syntax.UnaryTripleExpr
singleElementGroupUnaryTripleExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.SingleElementGroup"),
        Core.projectionField = (Core.Name "UnaryTripleExpr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

singleElementGroupWithSemi :: Phantoms.TTerm Syntax.SingleElementGroup -> Phantoms.TTerm (Maybe ()) -> Phantoms.TTerm Syntax.SingleElementGroup
singleElementGroupWithSemi original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.SingleElementGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "UnaryTripleExpr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.SingleElementGroup"),
              Core.projectionField = (Core.Name "UnaryTripleExpr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Semi"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

singleElementGroupWithUnaryTripleExpr :: Phantoms.TTerm Syntax.SingleElementGroup -> Phantoms.TTerm Syntax.UnaryTripleExpr -> Phantoms.TTerm Syntax.SingleElementGroup
singleElementGroupWithUnaryTripleExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.SingleElementGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "UnaryTripleExpr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Semi"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.SingleElementGroup"),
              Core.projectionField = (Core.Name "Semi")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

startActions :: Phantoms.TTerm [Syntax.CodeDecl] -> Phantoms.TTerm Syntax.StartActions
startActions x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.StartActions"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

statementDirective :: Phantoms.TTerm Syntax.Directive -> Phantoms.TTerm Syntax.Statement
statementDirective x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Directive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementNotStartAction :: Phantoms.TTerm Syntax.NotStartAction -> Phantoms.TTerm Syntax.Statement
statementNotStartAction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "NotStartAction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringFacetRegexp :: Phantoms.TTerm Syntax.Regexp -> Phantoms.TTerm Syntax.StringFacet
stringFacetRegexp x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringFacet"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Regexp"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringFacetSequence :: Phantoms.TTerm Syntax.StringFacet_Sequence -> Phantoms.TTerm Syntax.StringFacet
stringFacetSequence x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringFacet"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringFacet_Sequence :: Phantoms.TTerm Syntax.StringLength -> Phantoms.TTerm Syntax.Integer_ -> Phantoms.TTerm Syntax.StringFacet_Sequence
stringFacet_Sequence stringLength integer =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.StringFacet_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "StringLength"),
          Core.fieldTerm = (Phantoms.unTTerm stringLength)},
        Core.Field {
          Core.fieldName = (Core.Name "Integer"),
          Core.fieldTerm = (Phantoms.unTTerm integer)}]}))

stringFacet_SequenceInteger :: Phantoms.TTerm Syntax.StringFacet_Sequence -> Phantoms.TTerm Syntax.Integer_
stringFacet_SequenceInteger x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.StringFacet_Sequence"),
        Core.projectionField = (Core.Name "Integer")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringFacet_SequenceStringLength :: Phantoms.TTerm Syntax.StringFacet_Sequence -> Phantoms.TTerm Syntax.StringLength
stringFacet_SequenceStringLength x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.StringFacet_Sequence"),
        Core.projectionField = (Core.Name "StringLength")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringFacet_SequenceWithInteger :: Phantoms.TTerm Syntax.StringFacet_Sequence -> Phantoms.TTerm Syntax.Integer_ -> Phantoms.TTerm Syntax.StringFacet_Sequence
stringFacet_SequenceWithInteger original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.StringFacet_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "StringLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.StringFacet_Sequence"),
              Core.projectionField = (Core.Name "StringLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Integer"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

stringFacet_SequenceWithStringLength :: Phantoms.TTerm Syntax.StringFacet_Sequence -> Phantoms.TTerm Syntax.StringLength -> Phantoms.TTerm Syntax.StringFacet_Sequence
stringFacet_SequenceWithStringLength original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.StringFacet_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "StringLength"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Integer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.StringFacet_Sequence"),
              Core.projectionField = (Core.Name "Integer")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

stringLengthLENGTH :: Phantoms.TTerm Syntax.StringLength
stringLengthLENGTH =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringLength"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "LENGTH"),
        Core.fieldTerm = Core.TermUnit}}))

stringLengthMAXLENGTH :: Phantoms.TTerm Syntax.StringLength
stringLengthMAXLENGTH =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringLength"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "MAXLENGTH"),
        Core.fieldTerm = Core.TermUnit}}))

stringLengthMINLENGTH :: Phantoms.TTerm Syntax.StringLength
stringLengthMINLENGTH =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringLength"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "MINLENGTH"),
        Core.fieldTerm = Core.TermUnit}}))

stringLiteral1 :: Phantoms.TTerm [Syntax.StringLiteral1_Elmt] -> Phantoms.TTerm Syntax.StringLiteral1
stringLiteral1 x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.StringLiteral1"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

stringLiteral1_ElmtEchar :: Phantoms.TTerm Syntax.Echar -> Phantoms.TTerm Syntax.StringLiteral1_Elmt
stringLiteral1_ElmtEchar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteral1_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Echar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringLiteral1_ElmtRegex :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.StringLiteral1_Elmt
stringLiteral1_ElmtRegex x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteral1_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringLiteral1_ElmtUchar :: Phantoms.TTerm Syntax.Uchar -> Phantoms.TTerm Syntax.StringLiteral1_Elmt
stringLiteral1_ElmtUchar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteral1_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Uchar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringLiteral2 :: Phantoms.TTerm [Syntax.StringLiteral2_Elmt] -> Phantoms.TTerm Syntax.StringLiteral2
stringLiteral2 x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.StringLiteral2"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

stringLiteral2_ElmtEchar :: Phantoms.TTerm Syntax.Echar -> Phantoms.TTerm Syntax.StringLiteral2_Elmt
stringLiteral2_ElmtEchar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteral2_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Echar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringLiteral2_ElmtRegex :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.StringLiteral2_Elmt
stringLiteral2_ElmtRegex x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteral2_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringLiteral2_ElmtUchar :: Phantoms.TTerm Syntax.Uchar -> Phantoms.TTerm Syntax.StringLiteral2_Elmt
stringLiteral2_ElmtUchar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteral2_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Uchar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringLiteralLong1 :: Phantoms.TTerm [Syntax.StringLiteralLong1_Elmt] -> Phantoms.TTerm Syntax.StringLiteralLong1
stringLiteralLong1 x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong1"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

stringLiteralLong1_ElmtEchar :: Phantoms.TTerm Syntax.Echar -> Phantoms.TTerm Syntax.StringLiteralLong1_Elmt
stringLiteralLong1_ElmtEchar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong1_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Echar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringLiteralLong1_ElmtSequence :: Phantoms.TTerm Syntax.StringLiteralLong1_Elmt_Sequence -> Phantoms.TTerm Syntax.StringLiteralLong1_Elmt
stringLiteralLong1_ElmtSequence x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong1_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringLiteralLong1_ElmtUchar :: Phantoms.TTerm Syntax.Uchar -> Phantoms.TTerm Syntax.StringLiteralLong1_Elmt
stringLiteralLong1_ElmtUchar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong1_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Uchar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringLiteralLong1_Elmt_Sequence :: Phantoms.TTerm (Maybe Syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option) -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.StringLiteralLong1_Elmt_Sequence
stringLiteralLong1_Elmt_Sequence alts regex =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong1_Elmt_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Alts"),
          Core.fieldTerm = (Phantoms.unTTerm alts)},
        Core.Field {
          Core.fieldName = (Core.Name "regex"),
          Core.fieldTerm = (Phantoms.unTTerm regex)}]}))

stringLiteralLong1_Elmt_SequenceAlts :: Phantoms.TTerm Syntax.StringLiteralLong1_Elmt_Sequence -> Phantoms.TTerm (Maybe Syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option)
stringLiteralLong1_Elmt_SequenceAlts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong1_Elmt_Sequence"),
        Core.projectionField = (Core.Name "Alts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringLiteralLong1_Elmt_SequenceRegex :: Phantoms.TTerm Syntax.StringLiteralLong1_Elmt_Sequence -> Phantoms.TTerm String
stringLiteralLong1_Elmt_SequenceRegex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong1_Elmt_Sequence"),
        Core.projectionField = (Core.Name "regex")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringLiteralLong1_Elmt_SequenceWithAlts :: Phantoms.TTerm Syntax.StringLiteralLong1_Elmt_Sequence -> Phantoms.TTerm (Maybe Syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option) -> Phantoms.TTerm Syntax.StringLiteralLong1_Elmt_Sequence
stringLiteralLong1_Elmt_SequenceWithAlts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong1_Elmt_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Alts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "regex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong1_Elmt_Sequence"),
              Core.projectionField = (Core.Name "regex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

stringLiteralLong1_Elmt_SequenceWithRegex :: Phantoms.TTerm Syntax.StringLiteralLong1_Elmt_Sequence -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.StringLiteralLong1_Elmt_Sequence
stringLiteralLong1_Elmt_SequenceWithRegex original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong1_Elmt_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Alts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong1_Elmt_Sequence"),
              Core.projectionField = (Core.Name "Alts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "regex"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

stringLiteralLong1_Elmt_Sequence_Alts_OptionApos :: Phantoms.TTerm Syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option
stringLiteralLong1_Elmt_Sequence_Alts_OptionApos =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Apos"),
        Core.fieldTerm = Core.TermUnit}}))

stringLiteralLong1_Elmt_Sequence_Alts_OptionSequence :: Phantoms.TTerm Syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence -> Phantoms.TTerm Syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option
stringLiteralLong1_Elmt_Sequence_Alts_OptionSequence x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence :: Phantoms.TTerm Syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence
stringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence"),
      Core.recordFields = []}))

stringLiteralLong2 :: Phantoms.TTerm [Syntax.StringLiteralLong2_Elmt] -> Phantoms.TTerm Syntax.StringLiteralLong2
stringLiteralLong2 x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong2"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

stringLiteralLong2_ElmtEchar :: Phantoms.TTerm Syntax.Echar -> Phantoms.TTerm Syntax.StringLiteralLong2_Elmt
stringLiteralLong2_ElmtEchar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong2_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Echar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringLiteralLong2_ElmtSequence :: Phantoms.TTerm Syntax.StringLiteralLong2_Elmt_Sequence -> Phantoms.TTerm Syntax.StringLiteralLong2_Elmt
stringLiteralLong2_ElmtSequence x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong2_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringLiteralLong2_ElmtUchar :: Phantoms.TTerm Syntax.Uchar -> Phantoms.TTerm Syntax.StringLiteralLong2_Elmt
stringLiteralLong2_ElmtUchar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong2_Elmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Uchar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringLiteralLong2_Elmt_Sequence :: Phantoms.TTerm (Maybe Syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option) -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.StringLiteralLong2_Elmt_Sequence
stringLiteralLong2_Elmt_Sequence alts regex =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong2_Elmt_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Alts"),
          Core.fieldTerm = (Phantoms.unTTerm alts)},
        Core.Field {
          Core.fieldName = (Core.Name "regex"),
          Core.fieldTerm = (Phantoms.unTTerm regex)}]}))

stringLiteralLong2_Elmt_SequenceAlts :: Phantoms.TTerm Syntax.StringLiteralLong2_Elmt_Sequence -> Phantoms.TTerm (Maybe Syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option)
stringLiteralLong2_Elmt_SequenceAlts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong2_Elmt_Sequence"),
        Core.projectionField = (Core.Name "Alts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringLiteralLong2_Elmt_SequenceRegex :: Phantoms.TTerm Syntax.StringLiteralLong2_Elmt_Sequence -> Phantoms.TTerm String
stringLiteralLong2_Elmt_SequenceRegex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong2_Elmt_Sequence"),
        Core.projectionField = (Core.Name "regex")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringLiteralLong2_Elmt_SequenceWithAlts :: Phantoms.TTerm Syntax.StringLiteralLong2_Elmt_Sequence -> Phantoms.TTerm (Maybe Syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option) -> Phantoms.TTerm Syntax.StringLiteralLong2_Elmt_Sequence
stringLiteralLong2_Elmt_SequenceWithAlts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong2_Elmt_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Alts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "regex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong2_Elmt_Sequence"),
              Core.projectionField = (Core.Name "regex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

stringLiteralLong2_Elmt_SequenceWithRegex :: Phantoms.TTerm Syntax.StringLiteralLong2_Elmt_Sequence -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.StringLiteralLong2_Elmt_Sequence
stringLiteralLong2_Elmt_SequenceWithRegex original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong2_Elmt_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Alts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong2_Elmt_Sequence"),
              Core.projectionField = (Core.Name "Alts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "regex"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

stringLiteralLong2_Elmt_Sequence_Alts_OptionQuot :: Phantoms.TTerm Syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option
stringLiteralLong2_Elmt_Sequence_Alts_OptionQuot =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Quot"),
        Core.fieldTerm = Core.TermUnit}}))

stringLiteralLong2_Elmt_Sequence_Alts_OptionSequence :: Phantoms.TTerm Syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence -> Phantoms.TTerm Syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option
stringLiteralLong2_Elmt_Sequence_Alts_OptionSequence x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence :: Phantoms.TTerm Syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence
stringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence"),
      Core.recordFields = []}))

stringStringLiteral1 :: Phantoms.TTerm Syntax.StringLiteral1 -> Phantoms.TTerm Syntax.String_
stringStringLiteral1 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.String"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "StringLiteral1"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringStringLiteral2 :: Phantoms.TTerm Syntax.StringLiteral2 -> Phantoms.TTerm Syntax.String_
stringStringLiteral2 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.String"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "StringLiteral2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringStringLiteralLong1 :: Phantoms.TTerm Syntax.StringLiteralLong1 -> Phantoms.TTerm Syntax.String_
stringStringLiteralLong1 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.String"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "StringLiteralLong1"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringStringLiteralLong2 :: Phantoms.TTerm Syntax.StringLiteralLong2 -> Phantoms.TTerm Syntax.String_
stringStringLiteralLong2 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.String"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "StringLiteralLong2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

tripleConstraint :: Phantoms.TTerm (Maybe Syntax.SenseFlags) -> Phantoms.TTerm Syntax.Predicate -> Phantoms.TTerm Syntax.InlineShapeExpression -> Phantoms.TTerm (Maybe Syntax.Cardinality) -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.SemanticActions -> Phantoms.TTerm Syntax.TripleConstraint
tripleConstraint senseFlags predicate inlineShapeExpression cardinality listOfAnnotation semanticActions =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "SenseFlags"),
          Core.fieldTerm = (Phantoms.unTTerm senseFlags)},
        Core.Field {
          Core.fieldName = (Core.Name "Predicate"),
          Core.fieldTerm = (Phantoms.unTTerm predicate)},
        Core.Field {
          Core.fieldName = (Core.Name "InlineShapeExpression"),
          Core.fieldTerm = (Phantoms.unTTerm inlineShapeExpression)},
        Core.Field {
          Core.fieldName = (Core.Name "Cardinality"),
          Core.fieldTerm = (Phantoms.unTTerm cardinality)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfAnnotation"),
          Core.fieldTerm = (Phantoms.unTTerm listOfAnnotation)},
        Core.Field {
          Core.fieldName = (Core.Name "SemanticActions"),
          Core.fieldTerm = (Phantoms.unTTerm semanticActions)}]}))

tripleConstraintCardinality :: Phantoms.TTerm Syntax.TripleConstraint -> Phantoms.TTerm (Maybe Syntax.Cardinality)
tripleConstraintCardinality x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
        Core.projectionField = (Core.Name "Cardinality")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tripleConstraintInlineShapeExpression :: Phantoms.TTerm Syntax.TripleConstraint -> Phantoms.TTerm Syntax.InlineShapeExpression
tripleConstraintInlineShapeExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
        Core.projectionField = (Core.Name "InlineShapeExpression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tripleConstraintListOfAnnotation :: Phantoms.TTerm Syntax.TripleConstraint -> Phantoms.TTerm [Syntax.Annotation]
tripleConstraintListOfAnnotation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
        Core.projectionField = (Core.Name "listOfAnnotation")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tripleConstraintPredicate :: Phantoms.TTerm Syntax.TripleConstraint -> Phantoms.TTerm Syntax.Predicate
tripleConstraintPredicate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
        Core.projectionField = (Core.Name "Predicate")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tripleConstraintSemanticActions :: Phantoms.TTerm Syntax.TripleConstraint -> Phantoms.TTerm Syntax.SemanticActions
tripleConstraintSemanticActions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
        Core.projectionField = (Core.Name "SemanticActions")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tripleConstraintSenseFlags :: Phantoms.TTerm Syntax.TripleConstraint -> Phantoms.TTerm (Maybe Syntax.SenseFlags)
tripleConstraintSenseFlags x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
        Core.projectionField = (Core.Name "SenseFlags")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tripleConstraintWithCardinality :: Phantoms.TTerm Syntax.TripleConstraint -> Phantoms.TTerm (Maybe Syntax.Cardinality) -> Phantoms.TTerm Syntax.TripleConstraint
tripleConstraintWithCardinality original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "SenseFlags"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "SenseFlags")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "Predicate")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "InlineShapeExpression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "InlineShapeExpression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Cardinality"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "listOfAnnotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "listOfAnnotation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SemanticActions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "SemanticActions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tripleConstraintWithInlineShapeExpression :: Phantoms.TTerm Syntax.TripleConstraint -> Phantoms.TTerm Syntax.InlineShapeExpression -> Phantoms.TTerm Syntax.TripleConstraint
tripleConstraintWithInlineShapeExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "SenseFlags"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "SenseFlags")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "Predicate")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "InlineShapeExpression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "Cardinality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfAnnotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "listOfAnnotation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SemanticActions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "SemanticActions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tripleConstraintWithListOfAnnotation :: Phantoms.TTerm Syntax.TripleConstraint -> Phantoms.TTerm [Syntax.Annotation] -> Phantoms.TTerm Syntax.TripleConstraint
tripleConstraintWithListOfAnnotation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "SenseFlags"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "SenseFlags")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "Predicate")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "InlineShapeExpression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "InlineShapeExpression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "Cardinality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfAnnotation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "SemanticActions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "SemanticActions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tripleConstraintWithPredicate :: Phantoms.TTerm Syntax.TripleConstraint -> Phantoms.TTerm Syntax.Predicate -> Phantoms.TTerm Syntax.TripleConstraint
tripleConstraintWithPredicate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "SenseFlags"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "SenseFlags")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Predicate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "InlineShapeExpression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "InlineShapeExpression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "Cardinality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfAnnotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "listOfAnnotation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SemanticActions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "SemanticActions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tripleConstraintWithSemanticActions :: Phantoms.TTerm Syntax.TripleConstraint -> Phantoms.TTerm Syntax.SemanticActions -> Phantoms.TTerm Syntax.TripleConstraint
tripleConstraintWithSemanticActions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "SenseFlags"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "SenseFlags")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "Predicate")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "InlineShapeExpression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "InlineShapeExpression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "Cardinality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfAnnotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "listOfAnnotation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SemanticActions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tripleConstraintWithSenseFlags :: Phantoms.TTerm Syntax.TripleConstraint -> Phantoms.TTerm (Maybe Syntax.SenseFlags) -> Phantoms.TTerm Syntax.TripleConstraint
tripleConstraintWithSenseFlags original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "SenseFlags"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "Predicate")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "InlineShapeExpression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "InlineShapeExpression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Cardinality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "Cardinality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listOfAnnotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "listOfAnnotation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "SemanticActions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.TripleConstraint"),
              Core.projectionField = (Core.Name "SemanticActions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tripleExprLabelBlankNode :: Phantoms.TTerm Syntax.BlankNode -> Phantoms.TTerm Syntax.TripleExprLabel
tripleExprLabelBlankNode x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.TripleExprLabel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "BlankNode"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

tripleExprLabelIri :: Phantoms.TTerm Syntax.Iri -> Phantoms.TTerm Syntax.TripleExprLabel
tripleExprLabelIri x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.TripleExprLabel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Iri"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

tripleExpression :: Phantoms.TTerm Syntax.OneOfTripleExpr -> Phantoms.TTerm Syntax.TripleExpression
tripleExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.TripleExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

ucharSequence :: Phantoms.TTerm Syntax.Uchar_Sequence -> Phantoms.TTerm Syntax.Uchar
ucharSequence x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Uchar"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

ucharSequence2 :: Phantoms.TTerm Syntax.Uchar_Sequence2 -> Phantoms.TTerm Syntax.Uchar
ucharSequence2 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.Uchar"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

uchar_Sequence :: Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Uchar_Sequence
uchar_Sequence hex hex2 hex3 hex4 =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Hex"),
          Core.fieldTerm = (Phantoms.unTTerm hex)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex2"),
          Core.fieldTerm = (Phantoms.unTTerm hex2)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex3"),
          Core.fieldTerm = (Phantoms.unTTerm hex3)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex4"),
          Core.fieldTerm = (Phantoms.unTTerm hex4)}]}))

uchar_Sequence2 :: Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Uchar_Sequence2
uchar_Sequence2 hex hex2 hex3 hex4 hex5 hex6 hex7 hex8 =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Hex"),
          Core.fieldTerm = (Phantoms.unTTerm hex)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex2"),
          Core.fieldTerm = (Phantoms.unTTerm hex2)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex3"),
          Core.fieldTerm = (Phantoms.unTTerm hex3)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex4"),
          Core.fieldTerm = (Phantoms.unTTerm hex4)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex5"),
          Core.fieldTerm = (Phantoms.unTTerm hex5)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex6"),
          Core.fieldTerm = (Phantoms.unTTerm hex6)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex7"),
          Core.fieldTerm = (Phantoms.unTTerm hex7)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex8"),
          Core.fieldTerm = (Phantoms.unTTerm hex8)}]}))

uchar_Sequence2Hex :: Phantoms.TTerm Syntax.Uchar_Sequence2 -> Phantoms.TTerm Syntax.Hex
uchar_Sequence2Hex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
        Core.projectionField = (Core.Name "Hex")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

uchar_Sequence2Hex2 :: Phantoms.TTerm Syntax.Uchar_Sequence2 -> Phantoms.TTerm Syntax.Hex
uchar_Sequence2Hex2 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
        Core.projectionField = (Core.Name "Hex2")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

uchar_Sequence2Hex3 :: Phantoms.TTerm Syntax.Uchar_Sequence2 -> Phantoms.TTerm Syntax.Hex
uchar_Sequence2Hex3 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
        Core.projectionField = (Core.Name "Hex3")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

uchar_Sequence2Hex4 :: Phantoms.TTerm Syntax.Uchar_Sequence2 -> Phantoms.TTerm Syntax.Hex
uchar_Sequence2Hex4 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
        Core.projectionField = (Core.Name "Hex4")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

uchar_Sequence2Hex5 :: Phantoms.TTerm Syntax.Uchar_Sequence2 -> Phantoms.TTerm Syntax.Hex
uchar_Sequence2Hex5 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
        Core.projectionField = (Core.Name "Hex5")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

uchar_Sequence2Hex6 :: Phantoms.TTerm Syntax.Uchar_Sequence2 -> Phantoms.TTerm Syntax.Hex
uchar_Sequence2Hex6 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
        Core.projectionField = (Core.Name "Hex6")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

uchar_Sequence2Hex7 :: Phantoms.TTerm Syntax.Uchar_Sequence2 -> Phantoms.TTerm Syntax.Hex
uchar_Sequence2Hex7 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
        Core.projectionField = (Core.Name "Hex7")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

uchar_Sequence2Hex8 :: Phantoms.TTerm Syntax.Uchar_Sequence2 -> Phantoms.TTerm Syntax.Hex
uchar_Sequence2Hex8 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
        Core.projectionField = (Core.Name "Hex8")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

uchar_Sequence2WithHex :: Phantoms.TTerm Syntax.Uchar_Sequence2 -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Uchar_Sequence2
uchar_Sequence2WithHex original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Hex"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex3"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex3")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex4"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex4")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex5"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex5")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex6"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex6")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex7"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex7")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex8"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex8")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

uchar_Sequence2WithHex2 :: Phantoms.TTerm Syntax.Uchar_Sequence2 -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Uchar_Sequence2
uchar_Sequence2WithHex2 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Hex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex2"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex3"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex3")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex4"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex4")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex5"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex5")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex6"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex6")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex7"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex7")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex8"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex8")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

uchar_Sequence2WithHex3 :: Phantoms.TTerm Syntax.Uchar_Sequence2 -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Uchar_Sequence2
uchar_Sequence2WithHex3 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Hex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex3"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex4"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex4")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex5"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex5")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex6"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex6")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex7"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex7")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex8"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex8")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

uchar_Sequence2WithHex4 :: Phantoms.TTerm Syntax.Uchar_Sequence2 -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Uchar_Sequence2
uchar_Sequence2WithHex4 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Hex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex3"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex3")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex4"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex5"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex5")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex6"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex6")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex7"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex7")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex8"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex8")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

uchar_Sequence2WithHex5 :: Phantoms.TTerm Syntax.Uchar_Sequence2 -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Uchar_Sequence2
uchar_Sequence2WithHex5 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Hex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex3"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex3")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex4"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex4")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex5"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex6"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex6")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex7"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex7")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex8"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex8")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

uchar_Sequence2WithHex6 :: Phantoms.TTerm Syntax.Uchar_Sequence2 -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Uchar_Sequence2
uchar_Sequence2WithHex6 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Hex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex3"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex3")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex4"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex4")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex5"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex5")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex6"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex7"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex7")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex8"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex8")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

uchar_Sequence2WithHex7 :: Phantoms.TTerm Syntax.Uchar_Sequence2 -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Uchar_Sequence2
uchar_Sequence2WithHex7 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Hex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex3"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex3")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex4"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex4")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex5"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex5")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex6"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex6")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex7"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex8"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex8")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

uchar_Sequence2WithHex8 :: Phantoms.TTerm Syntax.Uchar_Sequence2 -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Uchar_Sequence2
uchar_Sequence2WithHex8 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Hex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex3"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex3")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex4"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex4")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex5"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex5")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex6"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex6")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex7"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence2"),
              Core.projectionField = (Core.Name "Hex7")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex8"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

uchar_SequenceHex :: Phantoms.TTerm Syntax.Uchar_Sequence -> Phantoms.TTerm Syntax.Hex
uchar_SequenceHex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
        Core.projectionField = (Core.Name "Hex")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

uchar_SequenceHex2 :: Phantoms.TTerm Syntax.Uchar_Sequence -> Phantoms.TTerm Syntax.Hex
uchar_SequenceHex2 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
        Core.projectionField = (Core.Name "Hex2")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

uchar_SequenceHex3 :: Phantoms.TTerm Syntax.Uchar_Sequence -> Phantoms.TTerm Syntax.Hex
uchar_SequenceHex3 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
        Core.projectionField = (Core.Name "Hex3")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

uchar_SequenceHex4 :: Phantoms.TTerm Syntax.Uchar_Sequence -> Phantoms.TTerm Syntax.Hex
uchar_SequenceHex4 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
        Core.projectionField = (Core.Name "Hex4")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

uchar_SequenceWithHex :: Phantoms.TTerm Syntax.Uchar_Sequence -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Uchar_Sequence
uchar_SequenceWithHex original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Hex"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
              Core.projectionField = (Core.Name "Hex2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex3"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
              Core.projectionField = (Core.Name "Hex3")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex4"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
              Core.projectionField = (Core.Name "Hex4")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

uchar_SequenceWithHex2 :: Phantoms.TTerm Syntax.Uchar_Sequence -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Uchar_Sequence
uchar_SequenceWithHex2 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Hex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
              Core.projectionField = (Core.Name "Hex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex2"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex3"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
              Core.projectionField = (Core.Name "Hex3")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex4"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
              Core.projectionField = (Core.Name "Hex4")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

uchar_SequenceWithHex3 :: Phantoms.TTerm Syntax.Uchar_Sequence -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Uchar_Sequence
uchar_SequenceWithHex3 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Hex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
              Core.projectionField = (Core.Name "Hex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
              Core.projectionField = (Core.Name "Hex2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex3"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Hex4"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
              Core.projectionField = (Core.Name "Hex4")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

uchar_SequenceWithHex4 :: Phantoms.TTerm Syntax.Uchar_Sequence -> Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm Syntax.Uchar_Sequence
uchar_SequenceWithHex4 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Hex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
              Core.projectionField = (Core.Name "Hex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
              Core.projectionField = (Core.Name "Hex2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex3"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.Uchar_Sequence"),
              Core.projectionField = (Core.Name "Hex3")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Hex4"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unAtpNameNs :: Phantoms.TTerm Syntax.AtpNameNs -> Phantoms.TTerm (Maybe Syntax.PnPrefix)
unAtpNameNs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.AtpNameNs")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unBaseDecl :: Phantoms.TTerm Syntax.BaseDecl -> Phantoms.TTerm Syntax.IriRef
unBaseDecl x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.BaseDecl")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unBlankNode :: Phantoms.TTerm Syntax.BlankNode -> Phantoms.TTerm Syntax.BlankNodeLabel
unBlankNode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.BlankNode")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unCode :: Phantoms.TTerm Syntax.Code -> Phantoms.TTerm [Syntax.Code_Elmt]
unCode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.Code")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDatatype :: Phantoms.TTerm Syntax.Datatype -> Phantoms.TTerm Syntax.Iri
unDatatype x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.Datatype")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDecimal :: Phantoms.TTerm Syntax.Decimal -> Phantoms.TTerm String
unDecimal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.Decimal")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDouble :: Phantoms.TTerm Syntax.Double_ -> Phantoms.TTerm String
unDouble x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.Double")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unEchar :: Phantoms.TTerm Syntax.Echar -> Phantoms.TTerm String
unEchar x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.Echar")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unExclusion :: Phantoms.TTerm Syntax.Exclusion -> Phantoms.TTerm Syntax.Iri
unExclusion x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.Exclusion")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unExtraPropertySet :: Phantoms.TTerm Syntax.ExtraPropertySet -> Phantoms.TTerm [Syntax.Predicate]
unExtraPropertySet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.ExtraPropertySet")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unHex :: Phantoms.TTerm Syntax.Hex -> Phantoms.TTerm String
unHex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.Hex")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unInclude :: Phantoms.TTerm Syntax.Include -> Phantoms.TTerm Syntax.TripleExprLabel
unInclude x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.Include")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unIncludeSet :: Phantoms.TTerm Syntax.IncludeSet -> Phantoms.TTerm [Syntax.ShapeExprLabel]
unIncludeSet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.IncludeSet")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unInlineShapeExpression :: Phantoms.TTerm Syntax.InlineShapeExpression -> Phantoms.TTerm Syntax.InlineShapeOr
unInlineShapeExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.InlineShapeExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unInteger :: Phantoms.TTerm Syntax.Integer_ -> Phantoms.TTerm String
unInteger x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.Integer")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unIriRef :: Phantoms.TTerm Syntax.IriRef -> Phantoms.TTerm [Syntax.IriRef_Elmt]
unIriRef x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.IriRef")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unLangTag :: Phantoms.TTerm Syntax.LangTag -> Phantoms.TTerm String
unLangTag x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.LangTag")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unPnLocalEsc :: Phantoms.TTerm Syntax.PnLocalEsc -> Phantoms.TTerm String
unPnLocalEsc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.PnLocalEsc")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unPnameNs :: Phantoms.TTerm Syntax.PnameNs -> Phantoms.TTerm (Maybe Syntax.PnPrefix)
unPnameNs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.PnameNs")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unRdfType :: Phantoms.TTerm Syntax.RdfType -> Phantoms.TTerm ()
unRdfType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.RdfType")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unSemanticActions :: Phantoms.TTerm Syntax.SemanticActions -> Phantoms.TTerm [Syntax.CodeDecl]
unSemanticActions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.SemanticActions")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unSenseFlags :: Phantoms.TTerm Syntax.SenseFlags -> Phantoms.TTerm ()
unSenseFlags x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.SenseFlags")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unShapeExpression :: Phantoms.TTerm Syntax.ShapeExpression -> Phantoms.TTerm Syntax.ShapeOr
unShapeExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.ShapeExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unStartActions :: Phantoms.TTerm Syntax.StartActions -> Phantoms.TTerm [Syntax.CodeDecl]
unStartActions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.StartActions")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unStringLiteral1 :: Phantoms.TTerm Syntax.StringLiteral1 -> Phantoms.TTerm [Syntax.StringLiteral1_Elmt]
unStringLiteral1 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.StringLiteral1")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unStringLiteral2 :: Phantoms.TTerm Syntax.StringLiteral2 -> Phantoms.TTerm [Syntax.StringLiteral2_Elmt]
unStringLiteral2 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.StringLiteral2")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unStringLiteralLong1 :: Phantoms.TTerm Syntax.StringLiteralLong1 -> Phantoms.TTerm [Syntax.StringLiteralLong1_Elmt]
unStringLiteralLong1 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.StringLiteralLong1")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unStringLiteralLong2 :: Phantoms.TTerm Syntax.StringLiteralLong2 -> Phantoms.TTerm [Syntax.StringLiteralLong2_Elmt]
unStringLiteralLong2 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.StringLiteralLong2")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unTripleExpression :: Phantoms.TTerm Syntax.TripleExpression -> Phantoms.TTerm Syntax.OneOfTripleExpr
unTripleExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.TripleExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unValueSet :: Phantoms.TTerm Syntax.ValueSet -> Phantoms.TTerm [Syntax.ValueSetValue]
unValueSet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.shex.syntax.ValueSet")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryTripleExprInclude :: Phantoms.TTerm Syntax.Include -> Phantoms.TTerm Syntax.UnaryTripleExpr
unaryTripleExprInclude x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.UnaryTripleExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Include"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unaryTripleExprSequence :: Phantoms.TTerm Syntax.UnaryTripleExpr_Sequence -> Phantoms.TTerm Syntax.UnaryTripleExpr
unaryTripleExprSequence x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.UnaryTripleExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unaryTripleExpr_Sequence :: Phantoms.TTerm (Maybe Syntax.TripleExprLabel) -> Phantoms.TTerm Syntax.UnaryTripleExpr_Sequence_Alts -> Phantoms.TTerm Syntax.UnaryTripleExpr_Sequence
unaryTripleExpr_Sequence sequence alts =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.UnaryTripleExpr_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Phantoms.unTTerm sequence)},
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Phantoms.unTTerm alts)}]}))

unaryTripleExpr_SequenceAlts :: Phantoms.TTerm Syntax.UnaryTripleExpr_Sequence -> Phantoms.TTerm Syntax.UnaryTripleExpr_Sequence_Alts
unaryTripleExpr_SequenceAlts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.UnaryTripleExpr_Sequence"),
        Core.projectionField = (Core.Name "alts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryTripleExpr_SequenceSequence :: Phantoms.TTerm Syntax.UnaryTripleExpr_Sequence -> Phantoms.TTerm (Maybe Syntax.TripleExprLabel)
unaryTripleExpr_SequenceSequence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.shex.syntax.UnaryTripleExpr_Sequence"),
        Core.projectionField = (Core.Name "Sequence")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryTripleExpr_SequenceWithAlts :: Phantoms.TTerm Syntax.UnaryTripleExpr_Sequence -> Phantoms.TTerm Syntax.UnaryTripleExpr_Sequence_Alts -> Phantoms.TTerm Syntax.UnaryTripleExpr_Sequence
unaryTripleExpr_SequenceWithAlts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.UnaryTripleExpr_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.UnaryTripleExpr_Sequence"),
              Core.projectionField = (Core.Name "Sequence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unaryTripleExpr_SequenceWithSequence :: Phantoms.TTerm Syntax.UnaryTripleExpr_Sequence -> Phantoms.TTerm (Maybe Syntax.TripleExprLabel) -> Phantoms.TTerm Syntax.UnaryTripleExpr_Sequence
unaryTripleExpr_SequenceWithSequence original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.shex.syntax.UnaryTripleExpr_Sequence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "alts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.shex.syntax.UnaryTripleExpr_Sequence"),
              Core.projectionField = (Core.Name "alts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unaryTripleExpr_Sequence_AltsBracketedTripleExpr :: Phantoms.TTerm Syntax.BracketedTripleExpr -> Phantoms.TTerm Syntax.UnaryTripleExpr_Sequence_Alts
unaryTripleExpr_Sequence_AltsBracketedTripleExpr x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.UnaryTripleExpr_Sequence_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "BracketedTripleExpr"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unaryTripleExpr_Sequence_AltsTripleConstraint :: Phantoms.TTerm Syntax.TripleConstraint -> Phantoms.TTerm Syntax.UnaryTripleExpr_Sequence_Alts
unaryTripleExpr_Sequence_AltsTripleConstraint x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.UnaryTripleExpr_Sequence_Alts"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "TripleConstraint"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueSet :: Phantoms.TTerm [Syntax.ValueSetValue] -> Phantoms.TTerm Syntax.ValueSet
valueSet x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.shex.syntax.ValueSet"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

valueSetValueIriRange :: Phantoms.TTerm Syntax.IriRange -> Phantoms.TTerm Syntax.ValueSetValue
valueSetValueIriRange x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.ValueSetValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "IriRange"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueSetValueLiteral :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.ValueSetValue
valueSetValueLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.ValueSetValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "Literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

xsFacetNumericFacet :: Phantoms.TTerm Syntax.NumericFacet -> Phantoms.TTerm Syntax.XsFacet
xsFacetNumericFacet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.XsFacet"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "NumericFacet"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

xsFacetStringFacet :: Phantoms.TTerm Syntax.StringFacet -> Phantoms.TTerm Syntax.XsFacet
xsFacetStringFacet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.shex.syntax.XsFacet"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "StringFacet"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
