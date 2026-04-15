-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.coq.syntax

module Hydra.Dsl.Coq.Syntax where

import qualified Hydra.Coq.Syntax as Syntax
import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

annotatedApplication :: Phantoms.TTerm Syntax.QualidAnnotated -> Phantoms.TTerm [Syntax.Term1] -> Phantoms.TTerm Syntax.AnnotatedApplication
annotatedApplication annot terms =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Phantoms.unTTerm annot)},
        Core.Field {
          Core.fieldName = (Core.Name "terms"),
          Core.fieldTerm = (Phantoms.unTTerm terms)}]}))

annotatedApplicationAnnot :: Phantoms.TTerm Syntax.AnnotatedApplication -> Phantoms.TTerm Syntax.QualidAnnotated
annotatedApplicationAnnot x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
        Core.projectionField = (Core.Name "annot")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotatedApplicationTerms :: Phantoms.TTerm Syntax.AnnotatedApplication -> Phantoms.TTerm [Syntax.Term1]
annotatedApplicationTerms x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
        Core.projectionField = (Core.Name "terms")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotatedApplicationWithAnnot :: Phantoms.TTerm Syntax.AnnotatedApplication -> Phantoms.TTerm Syntax.QualidAnnotated -> Phantoms.TTerm Syntax.AnnotatedApplication
annotatedApplicationWithAnnot original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "terms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
              Core.projectionField = (Core.Name "terms")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotatedApplicationWithTerms :: Phantoms.TTerm Syntax.AnnotatedApplication -> Phantoms.TTerm [Syntax.Term1] -> Phantoms.TTerm Syntax.AnnotatedApplication
annotatedApplicationWithTerms original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
              Core.projectionField = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "terms"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

applicationAnnotated :: Phantoms.TTerm Syntax.AnnotatedApplication -> Phantoms.TTerm Syntax.Application
applicationAnnotated x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Application"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotated"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

applicationNormal :: Phantoms.TTerm Syntax.NormalApplication -> Phantoms.TTerm Syntax.Application
applicationNormal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Application"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

argIdent :: Phantoms.TTerm Syntax.IdentArg -> Phantoms.TTerm Syntax.Arg
argIdent x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Arg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ident"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

argNatural :: Phantoms.TTerm Syntax.NaturalArg -> Phantoms.TTerm Syntax.Arg
argNatural x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Arg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "natural"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

argTerm :: Phantoms.TTerm Syntax.Term1 -> Phantoms.TTerm Syntax.Arg
argTerm x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Arg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

axiomDeclaration :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.AxiomDeclaration
axiomDeclaration name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

axiomDeclarationName :: Phantoms.TTerm Syntax.AxiomDeclaration -> Phantoms.TTerm Syntax.Ident
axiomDeclarationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

axiomDeclarationType :: Phantoms.TTerm Syntax.AxiomDeclaration -> Phantoms.TTerm Syntax.Type
axiomDeclarationType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

axiomDeclarationWithName :: Phantoms.TTerm Syntax.AxiomDeclaration -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.AxiomDeclaration
axiomDeclarationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

axiomDeclarationWithType :: Phantoms.TTerm Syntax.AxiomDeclaration -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.AxiomDeclaration
axiomDeclarationWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

binderGeneralizing :: Phantoms.TTerm Syntax.GeneralizingBinder -> Phantoms.TTerm Syntax.Binder
binderGeneralizing x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "generalizing"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

binderImplicit :: Phantoms.TTerm Syntax.ImplicitBinders -> Phantoms.TTerm Syntax.Binder
binderImplicit x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicit"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

binderName :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Binder
binderName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

binderPattern :: Phantoms.TTerm Syntax.Pattern0 -> Phantoms.TTerm Syntax.Binder
binderPattern x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

binderTerm :: Phantoms.TTerm Syntax.LetBinder -> Phantoms.TTerm Syntax.Binder
binderTerm x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

binderType :: Phantoms.TTerm Syntax.TypeBinders -> Phantoms.TTerm Syntax.Binder
binderType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

caseItem :: Phantoms.TTerm Syntax.Term100 -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm (Maybe Syntax.Pattern) -> Phantoms.TTerm Syntax.CaseItem
caseItem term as in_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm as)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTTerm in_)}]}))

caseItemAs :: Phantoms.TTerm Syntax.CaseItem -> Phantoms.TTerm (Maybe Syntax.Name)
caseItemAs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
        Core.projectionField = (Core.Name "as")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseItemIn :: Phantoms.TTerm Syntax.CaseItem -> Phantoms.TTerm (Maybe Syntax.Pattern)
caseItemIn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
        Core.projectionField = (Core.Name "in")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseItemTerm :: Phantoms.TTerm Syntax.CaseItem -> Phantoms.TTerm Syntax.Term100
caseItemTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
        Core.projectionField = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseItemWithAs :: Phantoms.TTerm Syntax.CaseItem -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.CaseItem
caseItemWithAs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
              Core.projectionField = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

caseItemWithIn :: Phantoms.TTerm Syntax.CaseItem -> Phantoms.TTerm (Maybe Syntax.Pattern) -> Phantoms.TTerm Syntax.CaseItem
caseItemWithIn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
              Core.projectionField = (Core.Name "as")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

caseItemWithTerm :: Phantoms.TTerm Syntax.CaseItem -> Phantoms.TTerm Syntax.Term100 -> Phantoms.TTerm Syntax.CaseItem
caseItemWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
              Core.projectionField = (Core.Name "as")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
              Core.projectionField = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

cofix :: Phantoms.TTerm Syntax.CofixBody -> Phantoms.TTerm (Maybe Syntax.CofixQual) -> Phantoms.TTerm Syntax.Cofix
cofix body qual =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Phantoms.unTTerm qual)}]}))

cofixBody :: Phantoms.TTerm Syntax.Cofix -> Phantoms.TTerm Syntax.CofixBody
cofixBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

cofixBodyBinders :: Phantoms.TTerm Syntax.CofixBody -> Phantoms.TTerm [Syntax.Binder]
cofixBodyBinders x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
        Core.projectionField = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

cofixBodyIdent :: Phantoms.TTerm Syntax.CofixBody -> Phantoms.TTerm Syntax.Ident
cofixBodyIdent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
        Core.projectionField = (Core.Name "ident")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

cofixBodyTerm :: Phantoms.TTerm Syntax.CofixBody -> Phantoms.TTerm Syntax.Term
cofixBodyTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
        Core.projectionField = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

cofixBodyType :: Phantoms.TTerm Syntax.CofixBody -> Phantoms.TTerm (Maybe Syntax.Type)
cofixBodyType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

cofixBodyWithBinders :: Phantoms.TTerm Syntax.CofixBody -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm Syntax.CofixBody
cofixBodyWithBinders original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

cofixBodyWithIdent :: Phantoms.TTerm Syntax.CofixBody -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.CofixBody
cofixBodyWithIdent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

cofixBodyWithTerm :: Phantoms.TTerm Syntax.CofixBody -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.CofixBody
cofixBodyWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

cofixBodyWithType :: Phantoms.TTerm Syntax.CofixBody -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.CofixBody
cofixBodyWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

cofixBody_ :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.CofixBody
cofixBody_ ident binders type_ term =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm ident)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)}]}))

cofixQual :: Phantoms.TTerm Syntax.Cofix -> Phantoms.TTerm (Maybe Syntax.CofixQual)
cofixQual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
        Core.projectionField = (Core.Name "qual")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

cofixQualIn :: Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.CofixQual
cofixQualIn x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.CofixQual"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

cofixQualWith :: Phantoms.TTerm Syntax.CofixWith -> Phantoms.TTerm Syntax.CofixQual
cofixQualWith x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.CofixQual"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "with"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

cofixWith :: Phantoms.TTerm [Syntax.CofixBody] -> Phantoms.TTerm (Maybe Syntax.Ident) -> Phantoms.TTerm Syntax.CofixWith
cofixWith with for =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTTerm with)},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Phantoms.unTTerm for)}]}))

cofixWithBody :: Phantoms.TTerm Syntax.Cofix -> Phantoms.TTerm Syntax.CofixBody -> Phantoms.TTerm Syntax.Cofix
cofixWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
              Core.projectionField = (Core.Name "qual")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

cofixWithFor :: Phantoms.TTerm Syntax.CofixWith -> Phantoms.TTerm (Maybe Syntax.Ident)
cofixWithFor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
        Core.projectionField = (Core.Name "for")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

cofixWithQual :: Phantoms.TTerm Syntax.Cofix -> Phantoms.TTerm (Maybe Syntax.CofixQual) -> Phantoms.TTerm Syntax.Cofix
cofixWithQual original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

cofixWithWith :: Phantoms.TTerm Syntax.CofixWith -> Phantoms.TTerm [Syntax.CofixBody]
cofixWithWith x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
        Core.projectionField = (Core.Name "with")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

cofixWithWithFor :: Phantoms.TTerm Syntax.CofixWith -> Phantoms.TTerm (Maybe Syntax.Ident) -> Phantoms.TTerm Syntax.CofixWith
cofixWithWithFor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
              Core.projectionField = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

cofixWithWithWith :: Phantoms.TTerm Syntax.CofixWith -> Phantoms.TTerm [Syntax.CofixBody] -> Phantoms.TTerm Syntax.CofixWith
cofixWithWithWith original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
              Core.projectionField = (Core.Name "for")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

comment :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Comment
comment x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.Comment"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

constructor :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Constructor
constructor name binders type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

constructorBinders :: Phantoms.TTerm Syntax.Constructor -> Phantoms.TTerm [Syntax.Binder]
constructorBinders x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
        Core.projectionField = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorName :: Phantoms.TTerm Syntax.Constructor -> Phantoms.TTerm Syntax.Ident
constructorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorType :: Phantoms.TTerm Syntax.Constructor -> Phantoms.TTerm (Maybe Syntax.Type)
constructorType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorWithBinders :: Phantoms.TTerm Syntax.Constructor -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm Syntax.Constructor
constructorWithBinders original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constructorWithName :: Phantoms.TTerm Syntax.Constructor -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.Constructor
constructorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constructorWithType :: Phantoms.TTerm Syntax.Constructor -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Constructor
constructorWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

definition :: Phantoms.TTerm (Maybe Syntax.Locality) -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Definition
definition locality name binders type_ body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Phantoms.unTTerm locality)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

definitionBinders :: Phantoms.TTerm Syntax.Definition -> Phantoms.TTerm [Syntax.Binder]
definitionBinders x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
        Core.projectionField = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

definitionBody :: Phantoms.TTerm Syntax.Definition -> Phantoms.TTerm Syntax.Term
definitionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

definitionLocality :: Phantoms.TTerm Syntax.Definition -> Phantoms.TTerm (Maybe Syntax.Locality)
definitionLocality x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
        Core.projectionField = (Core.Name "locality")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

definitionName :: Phantoms.TTerm Syntax.Definition -> Phantoms.TTerm Syntax.Ident
definitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

definitionType :: Phantoms.TTerm Syntax.Definition -> Phantoms.TTerm (Maybe Syntax.Type)
definitionType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

definitionWithBinders :: Phantoms.TTerm Syntax.Definition -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm Syntax.Definition
definitionWithBinders original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionField = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

definitionWithBody :: Phantoms.TTerm Syntax.Definition -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Definition
definitionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionField = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

definitionWithLocality :: Phantoms.TTerm Syntax.Definition -> Phantoms.TTerm (Maybe Syntax.Locality) -> Phantoms.TTerm Syntax.Definition
definitionWithLocality original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

definitionWithName :: Phantoms.TTerm Syntax.Definition -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.Definition
definitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionField = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

definitionWithType :: Phantoms.TTerm Syntax.Definition -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Definition
definitionWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionField = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

document :: Phantoms.TTerm [Syntax.Sentence] -> Phantoms.TTerm Syntax.Document
document sentences =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Document"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Phantoms.unTTerm sentences)}]}))

documentSentences :: Phantoms.TTerm Syntax.Document -> Phantoms.TTerm [Syntax.Sentence]
documentSentences x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Document"),
        Core.projectionField = (Core.Name "sentences")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

documentWithSentences :: Phantoms.TTerm Syntax.Document -> Phantoms.TTerm [Syntax.Sentence] -> Phantoms.TTerm Syntax.Document
documentWithSentences original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Document"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

equation :: Phantoms.TTerm [[Syntax.Pattern]] -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Equation
equation pattern term =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Equation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)}]}))

equationPattern :: Phantoms.TTerm Syntax.Equation -> Phantoms.TTerm [[Syntax.Pattern]]
equationPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Equation"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equationTerm :: Phantoms.TTerm Syntax.Equation -> Phantoms.TTerm Syntax.Term
equationTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Equation"),
        Core.projectionField = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

equationWithPattern :: Phantoms.TTerm Syntax.Equation -> Phantoms.TTerm [[Syntax.Pattern]] -> Phantoms.TTerm Syntax.Equation
equationWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Equation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Equation"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

equationWithTerm :: Phantoms.TTerm Syntax.Equation -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Equation
equationWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Equation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Equation"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

existentialVariable :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.ExistentialVariableVariant -> Phantoms.TTerm Syntax.ExistentialVariable
existentialVariable ident variant =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm ident)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTTerm variant)}]}))

existentialVariableIdent :: Phantoms.TTerm Syntax.ExistentialVariable -> Phantoms.TTerm Syntax.Ident
existentialVariableIdent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
        Core.projectionField = (Core.Name "ident")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

existentialVariableVariant :: Phantoms.TTerm Syntax.ExistentialVariable -> Phantoms.TTerm Syntax.ExistentialVariableVariant
existentialVariableVariant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
        Core.projectionField = (Core.Name "variant")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

existentialVariableVariantInside1 :: Phantoms.TTerm Syntax.ExistentialVariableVariant
existentialVariableVariantInside1 =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariableVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inside1"),
        Core.fieldTerm = Core.TermUnit}}))

existentialVariableVariantInside2 :: Phantoms.TTerm Syntax.ExistentialVariableVariant
existentialVariableVariantInside2 =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariableVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inside2"),
        Core.fieldTerm = Core.TermUnit}}))

existentialVariableVariantOutside :: Phantoms.TTerm (Maybe Syntax.IdentArg) -> Phantoms.TTerm Syntax.ExistentialVariableVariant
existentialVariableVariantOutside x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariableVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "outside"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

existentialVariableVariantPlaceholder :: Phantoms.TTerm Syntax.ExistentialVariableVariant
existentialVariableVariantPlaceholder =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariableVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "placeholder"),
        Core.fieldTerm = Core.TermUnit}}))

existentialVariableWithIdent :: Phantoms.TTerm Syntax.ExistentialVariable -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.ExistentialVariable
existentialVariableWithIdent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
              Core.projectionField = (Core.Name "variant")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

existentialVariableWithVariant :: Phantoms.TTerm Syntax.ExistentialVariable -> Phantoms.TTerm Syntax.ExistentialVariableVariant -> Phantoms.TTerm Syntax.ExistentialVariable
existentialVariableWithVariant original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
              Core.projectionField = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fieldIdent :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.FieldIdent
fieldIdent x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.FieldIdent"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

fixAnnotMeasure :: Phantoms.TTerm Syntax.FixAnnot_Measure -> Phantoms.TTerm Syntax.FixAnnot
fixAnnotMeasure x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "measure"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fixAnnotStruct :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.FixAnnot
fixAnnotStruct x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "struct"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fixAnnotWf :: Phantoms.TTerm Syntax.FixAnnot_Wf -> Phantoms.TTerm Syntax.FixAnnot
fixAnnotWf x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fixAnnot_Measure :: Phantoms.TTerm Syntax.OneTerm -> Phantoms.TTerm (Maybe Syntax.Ident) -> Phantoms.TTerm (Maybe Syntax.OneTerm) -> Phantoms.TTerm Syntax.FixAnnot_Measure
fixAnnot_Measure term ident term2 =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm ident)},
        Core.Field {
          Core.fieldName = (Core.Name "term2"),
          Core.fieldTerm = (Phantoms.unTTerm term2)}]}))

fixAnnot_MeasureIdent :: Phantoms.TTerm Syntax.FixAnnot_Measure -> Phantoms.TTerm (Maybe Syntax.Ident)
fixAnnot_MeasureIdent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
        Core.projectionField = (Core.Name "ident")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixAnnot_MeasureTerm :: Phantoms.TTerm Syntax.FixAnnot_Measure -> Phantoms.TTerm Syntax.OneTerm
fixAnnot_MeasureTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
        Core.projectionField = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixAnnot_MeasureTerm2 :: Phantoms.TTerm Syntax.FixAnnot_Measure -> Phantoms.TTerm (Maybe Syntax.OneTerm)
fixAnnot_MeasureTerm2 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
        Core.projectionField = (Core.Name "term2")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixAnnot_MeasureWithIdent :: Phantoms.TTerm Syntax.FixAnnot_Measure -> Phantoms.TTerm (Maybe Syntax.Ident) -> Phantoms.TTerm Syntax.FixAnnot_Measure
fixAnnot_MeasureWithIdent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
              Core.projectionField = (Core.Name "term2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fixAnnot_MeasureWithTerm :: Phantoms.TTerm Syntax.FixAnnot_Measure -> Phantoms.TTerm Syntax.OneTerm -> Phantoms.TTerm Syntax.FixAnnot_Measure
fixAnnot_MeasureWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
              Core.projectionField = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
              Core.projectionField = (Core.Name "term2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fixAnnot_MeasureWithTerm2 :: Phantoms.TTerm Syntax.FixAnnot_Measure -> Phantoms.TTerm (Maybe Syntax.OneTerm) -> Phantoms.TTerm Syntax.FixAnnot_Measure
fixAnnot_MeasureWithTerm2 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
              Core.projectionField = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term2"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fixAnnot_Wf :: Phantoms.TTerm Syntax.OneTerm -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.FixAnnot_Wf
fixAnnot_Wf term ident =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm ident)}]}))

fixAnnot_WfIdent :: Phantoms.TTerm Syntax.FixAnnot_Wf -> Phantoms.TTerm Syntax.Ident
fixAnnot_WfIdent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
        Core.projectionField = (Core.Name "ident")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixAnnot_WfTerm :: Phantoms.TTerm Syntax.FixAnnot_Wf -> Phantoms.TTerm Syntax.OneTerm
fixAnnot_WfTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
        Core.projectionField = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixAnnot_WfWithIdent :: Phantoms.TTerm Syntax.FixAnnot_Wf -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.FixAnnot_Wf
fixAnnot_WfWithIdent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fixAnnot_WfWithTerm :: Phantoms.TTerm Syntax.FixAnnot_Wf -> Phantoms.TTerm Syntax.OneTerm -> Phantoms.TTerm Syntax.FixAnnot_Wf
fixAnnot_WfWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
              Core.projectionField = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fixDecl :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm Syntax.Fix
fixDecl x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Fix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decl"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fixQual :: Phantoms.TTerm (Maybe Syntax.Fix_Qual) -> Phantoms.TTerm Syntax.Fix
fixQual x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Fix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fixWith :: Phantoms.TTerm [Syntax.Fix_Decl] -> Phantoms.TTerm (Maybe Syntax.Ident) -> Phantoms.TTerm Syntax.FixWith
fixWith decls for =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decls"),
          Core.fieldTerm = (Phantoms.unTTerm decls)},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Phantoms.unTTerm for)}]}))

fixWithDecls :: Phantoms.TTerm Syntax.FixWith -> Phantoms.TTerm [Syntax.Fix_Decl]
fixWithDecls x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
        Core.projectionField = (Core.Name "decls")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixWithFor :: Phantoms.TTerm Syntax.FixWith -> Phantoms.TTerm (Maybe Syntax.Ident)
fixWithFor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
        Core.projectionField = (Core.Name "for")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixWithWithDecls :: Phantoms.TTerm Syntax.FixWith -> Phantoms.TTerm [Syntax.Fix_Decl] -> Phantoms.TTerm Syntax.FixWith
fixWithWithDecls original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decls"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
              Core.projectionField = (Core.Name "for")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fixWithWithFor :: Phantoms.TTerm Syntax.FixWith -> Phantoms.TTerm (Maybe Syntax.Ident) -> Phantoms.TTerm Syntax.FixWith
fixWithWithFor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decls"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
              Core.projectionField = (Core.Name "decls")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fix_Decl :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm (Maybe Syntax.FixAnnot) -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Fix_Decl
fix_Decl ident binders annot type_ term =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm ident)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Phantoms.unTTerm annot)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)}]}))

fix_DeclAnnot :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm (Maybe Syntax.FixAnnot)
fix_DeclAnnot x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
        Core.projectionField = (Core.Name "annot")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fix_DeclBinders :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm [Syntax.Binder]
fix_DeclBinders x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
        Core.projectionField = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fix_DeclIdent :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm Syntax.Ident
fix_DeclIdent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
        Core.projectionField = (Core.Name "ident")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fix_DeclTerm :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm Syntax.Term
fix_DeclTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
        Core.projectionField = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fix_DeclType :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm (Maybe Syntax.Type)
fix_DeclType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fix_DeclWithAnnot :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm (Maybe Syntax.FixAnnot) -> Phantoms.TTerm Syntax.Fix_Decl
fix_DeclWithAnnot original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fix_DeclWithBinders :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm Syntax.Fix_Decl
fix_DeclWithBinders original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fix_DeclWithIdent :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.Fix_Decl
fix_DeclWithIdent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fix_DeclWithTerm :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Fix_Decl
fix_DeclWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fix_DeclWithType :: Phantoms.TTerm Syntax.Fix_Decl -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Fix_Decl
fix_DeclWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fix_QualIn :: Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Fix_Qual
fix_QualIn x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Qual"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fix_QualWith :: Phantoms.TTerm Syntax.FixWith -> Phantoms.TTerm Syntax.Fix_Qual
fix_QualWith x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Qual"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "with"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fixpointDefinition :: Phantoms.TTerm (Maybe Syntax.Locality) -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm (Maybe Syntax.FixAnnot) -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm [Syntax.Fix_Decl] -> Phantoms.TTerm Syntax.FixpointDefinition
fixpointDefinition locality name binders annot type_ body with =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Phantoms.unTTerm locality)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Phantoms.unTTerm annot)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTTerm with)}]}))

fixpointDefinitionAnnot :: Phantoms.TTerm Syntax.FixpointDefinition -> Phantoms.TTerm (Maybe Syntax.FixAnnot)
fixpointDefinitionAnnot x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionField = (Core.Name "annot")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixpointDefinitionBinders :: Phantoms.TTerm Syntax.FixpointDefinition -> Phantoms.TTerm [Syntax.Binder]
fixpointDefinitionBinders x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionField = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixpointDefinitionBody :: Phantoms.TTerm Syntax.FixpointDefinition -> Phantoms.TTerm Syntax.Term
fixpointDefinitionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixpointDefinitionLocality :: Phantoms.TTerm Syntax.FixpointDefinition -> Phantoms.TTerm (Maybe Syntax.Locality)
fixpointDefinitionLocality x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionField = (Core.Name "locality")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixpointDefinitionName :: Phantoms.TTerm Syntax.FixpointDefinition -> Phantoms.TTerm Syntax.Ident
fixpointDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixpointDefinitionType :: Phantoms.TTerm Syntax.FixpointDefinition -> Phantoms.TTerm (Maybe Syntax.Type)
fixpointDefinitionType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixpointDefinitionWith :: Phantoms.TTerm Syntax.FixpointDefinition -> Phantoms.TTerm [Syntax.Fix_Decl]
fixpointDefinitionWith x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionField = (Core.Name "with")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fixpointDefinitionWithAnnot :: Phantoms.TTerm Syntax.FixpointDefinition -> Phantoms.TTerm (Maybe Syntax.FixAnnot) -> Phantoms.TTerm Syntax.FixpointDefinition
fixpointDefinitionWithAnnot original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fixpointDefinitionWithBinders :: Phantoms.TTerm Syntax.FixpointDefinition -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm Syntax.FixpointDefinition
fixpointDefinitionWithBinders original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fixpointDefinitionWithBody :: Phantoms.TTerm Syntax.FixpointDefinition -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.FixpointDefinition
fixpointDefinitionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fixpointDefinitionWithLocality :: Phantoms.TTerm Syntax.FixpointDefinition -> Phantoms.TTerm (Maybe Syntax.Locality) -> Phantoms.TTerm Syntax.FixpointDefinition
fixpointDefinitionWithLocality original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fixpointDefinitionWithName :: Phantoms.TTerm Syntax.FixpointDefinition -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.FixpointDefinition
fixpointDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fixpointDefinitionWithType :: Phantoms.TTerm Syntax.FixpointDefinition -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.FixpointDefinition
fixpointDefinitionWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fixpointDefinitionWithWith :: Phantoms.TTerm Syntax.FixpointDefinition -> Phantoms.TTerm [Syntax.Fix_Decl] -> Phantoms.TTerm Syntax.FixpointDefinition
fixpointDefinitionWithWith original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

forall_ :: Phantoms.TTerm Syntax.OpenBinders -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Forall
forall_ binders type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Forall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

forallBinders :: Phantoms.TTerm Syntax.Forall -> Phantoms.TTerm Syntax.OpenBinders
forallBinders x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Forall"),
        Core.projectionField = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forallOrFunForall :: Phantoms.TTerm Syntax.Forall -> Phantoms.TTerm Syntax.ForallOrFun
forallOrFunForall x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ForallOrFun"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forall"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

forallOrFunFun :: Phantoms.TTerm Syntax.Fun -> Phantoms.TTerm Syntax.ForallOrFun
forallOrFunFun x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ForallOrFun"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fun"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

forallType :: Phantoms.TTerm Syntax.Forall -> Phantoms.TTerm Syntax.Type
forallType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Forall"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forallWithBinders :: Phantoms.TTerm Syntax.Forall -> Phantoms.TTerm Syntax.OpenBinders -> Phantoms.TTerm Syntax.Forall
forallWithBinders original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Forall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Forall"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

forallWithType :: Phantoms.TTerm Syntax.Forall -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Forall
forallWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Forall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Forall"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fun :: Phantoms.TTerm Syntax.OpenBinders -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Fun
fun binders body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fun"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

funBinders :: Phantoms.TTerm Syntax.Fun -> Phantoms.TTerm Syntax.OpenBinders
funBinders x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fun"),
        Core.projectionField = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

funBody :: Phantoms.TTerm Syntax.Fun -> Phantoms.TTerm Syntax.Term
funBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fun"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

funWithBinders :: Phantoms.TTerm Syntax.Fun -> Phantoms.TTerm Syntax.OpenBinders -> Phantoms.TTerm Syntax.Fun
funWithBinders original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fun"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fun"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

funWithBody :: Phantoms.TTerm Syntax.Fun -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Fun
funWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fun"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fun"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

generalizingBinderExplicit :: Phantoms.TTerm Syntax.TypeclassConstraint -> Phantoms.TTerm Syntax.GeneralizingBinder
generalizingBinderExplicit x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.GeneralizingBinder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "explicit"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

generalizingBinderImplicitMaximallyInserted :: Phantoms.TTerm Syntax.TypeclassConstraint -> Phantoms.TTerm Syntax.GeneralizingBinder
generalizingBinderImplicitMaximallyInserted x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.GeneralizingBinder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicitMaximallyInserted"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

generalizingBinderImplicitNonMaximallyInserted :: Phantoms.TTerm Syntax.TypeclassConstraint -> Phantoms.TTerm Syntax.GeneralizingBinder
generalizingBinderImplicitNonMaximallyInserted x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.GeneralizingBinder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicitNonMaximallyInserted"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

ident :: Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm Syntax.Ident
ident x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.Ident"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

identArg :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.IdentArg
identArg ident term =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm ident)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)}]}))

identArgIdent :: Phantoms.TTerm Syntax.IdentArg -> Phantoms.TTerm Syntax.Ident
identArgIdent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
        Core.projectionField = (Core.Name "ident")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

identArgTerm :: Phantoms.TTerm Syntax.IdentArg -> Phantoms.TTerm Syntax.Term
identArgTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
        Core.projectionField = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

identArgWithIdent :: Phantoms.TTerm Syntax.IdentArg -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.IdentArg
identArgWithIdent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

identArgWithTerm :: Phantoms.TTerm Syntax.IdentArg -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.IdentArg
identArgWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
              Core.projectionField = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

if_ :: Phantoms.TTerm Syntax.Term -> Phantoms.TTerm (Maybe Syntax.ReturnAs) -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.If
if_ condition returnAs then_ else_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Phantoms.unTTerm returnAs)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm then_)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm else_)}]}))

ifCondition :: Phantoms.TTerm Syntax.If -> Phantoms.TTerm Syntax.Term
ifCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
        Core.projectionField = (Core.Name "condition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifElse :: Phantoms.TTerm Syntax.If -> Phantoms.TTerm Syntax.Term
ifElse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
        Core.projectionField = (Core.Name "else")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifReturnAs :: Phantoms.TTerm Syntax.If -> Phantoms.TTerm (Maybe Syntax.ReturnAs)
ifReturnAs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
        Core.projectionField = (Core.Name "returnAs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifThen :: Phantoms.TTerm Syntax.If -> Phantoms.TTerm Syntax.Term
ifThen x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
        Core.projectionField = (Core.Name "then")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifWithCondition :: Phantoms.TTerm Syntax.If -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.If
ifWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionField = (Core.Name "returnAs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionField = (Core.Name "then")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifWithElse :: Phantoms.TTerm Syntax.If -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.If
ifWithElse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionField = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionField = (Core.Name "returnAs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionField = (Core.Name "then")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

ifWithReturnAs :: Phantoms.TTerm Syntax.If -> Phantoms.TTerm (Maybe Syntax.ReturnAs) -> Phantoms.TTerm Syntax.If
ifWithReturnAs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionField = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionField = (Core.Name "then")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifWithThen :: Phantoms.TTerm Syntax.If -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.If
ifWithThen original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionField = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionField = (Core.Name "returnAs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

implicitBindersMaximallyInserted :: Phantoms.TTerm Syntax.TypeBinders -> Phantoms.TTerm Syntax.ImplicitBinders
implicitBindersMaximallyInserted x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ImplicitBinders"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maximallyInserted"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

implicitBindersNonMaximallyInserted :: Phantoms.TTerm Syntax.TypeBinders -> Phantoms.TTerm Syntax.ImplicitBinders
implicitBindersNonMaximallyInserted x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ImplicitBinders"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonMaximallyInserted"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importQualificationExport :: Phantoms.TTerm Syntax.ImportQualification
importQualificationExport =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ImportQualification"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "export"),
        Core.fieldTerm = Core.TermUnit}}))

importQualificationImport :: Phantoms.TTerm Syntax.ImportQualification
importQualificationImport =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ImportQualification"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "import"),
        Core.fieldTerm = Core.TermUnit}}))

inductiveBody :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm [Syntax.Constructor] -> Phantoms.TTerm Syntax.InductiveBody
inductiveBody name binders type_ constructors =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Phantoms.unTTerm constructors)}]}))

inductiveBodyBinders :: Phantoms.TTerm Syntax.InductiveBody -> Phantoms.TTerm [Syntax.Binder]
inductiveBodyBinders x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
        Core.projectionField = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inductiveBodyConstructors :: Phantoms.TTerm Syntax.InductiveBody -> Phantoms.TTerm [Syntax.Constructor]
inductiveBodyConstructors x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
        Core.projectionField = (Core.Name "constructors")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inductiveBodyName :: Phantoms.TTerm Syntax.InductiveBody -> Phantoms.TTerm Syntax.Ident
inductiveBodyName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inductiveBodyType :: Phantoms.TTerm Syntax.InductiveBody -> Phantoms.TTerm (Maybe Syntax.Type)
inductiveBodyType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inductiveBodyWithBinders :: Phantoms.TTerm Syntax.InductiveBody -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm Syntax.InductiveBody
inductiveBodyWithBinders original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionField = (Core.Name "constructors")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inductiveBodyWithConstructors :: Phantoms.TTerm Syntax.InductiveBody -> Phantoms.TTerm [Syntax.Constructor] -> Phantoms.TTerm Syntax.InductiveBody
inductiveBodyWithConstructors original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inductiveBodyWithName :: Phantoms.TTerm Syntax.InductiveBody -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.InductiveBody
inductiveBodyWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionField = (Core.Name "constructors")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inductiveBodyWithType :: Phantoms.TTerm Syntax.InductiveBody -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.InductiveBody
inductiveBodyWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionField = (Core.Name "constructors")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inductiveDefinition :: Phantoms.TTerm (Maybe Syntax.Locality) -> Phantoms.TTerm Bool -> Phantoms.TTerm [Syntax.InductiveBody] -> Phantoms.TTerm Syntax.InductiveDefinition
inductiveDefinition locality coinductive bodies =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Phantoms.unTTerm locality)},
        Core.Field {
          Core.fieldName = (Core.Name "coinductive"),
          Core.fieldTerm = (Phantoms.unTTerm coinductive)},
        Core.Field {
          Core.fieldName = (Core.Name "bodies"),
          Core.fieldTerm = (Phantoms.unTTerm bodies)}]}))

inductiveDefinitionBodies :: Phantoms.TTerm Syntax.InductiveDefinition -> Phantoms.TTerm [Syntax.InductiveBody]
inductiveDefinitionBodies x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
        Core.projectionField = (Core.Name "bodies")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inductiveDefinitionCoinductive :: Phantoms.TTerm Syntax.InductiveDefinition -> Phantoms.TTerm Bool
inductiveDefinitionCoinductive x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
        Core.projectionField = (Core.Name "coinductive")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inductiveDefinitionLocality :: Phantoms.TTerm Syntax.InductiveDefinition -> Phantoms.TTerm (Maybe Syntax.Locality)
inductiveDefinitionLocality x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
        Core.projectionField = (Core.Name "locality")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inductiveDefinitionWithBodies :: Phantoms.TTerm Syntax.InductiveDefinition -> Phantoms.TTerm [Syntax.InductiveBody] -> Phantoms.TTerm Syntax.InductiveDefinition
inductiveDefinitionWithBodies original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
              Core.projectionField = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "coinductive"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
              Core.projectionField = (Core.Name "coinductive")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bodies"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inductiveDefinitionWithCoinductive :: Phantoms.TTerm Syntax.InductiveDefinition -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.InductiveDefinition
inductiveDefinitionWithCoinductive original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
              Core.projectionField = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "coinductive"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bodies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
              Core.projectionField = (Core.Name "bodies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inductiveDefinitionWithLocality :: Phantoms.TTerm Syntax.InductiveDefinition -> Phantoms.TTerm (Maybe Syntax.Locality) -> Phantoms.TTerm Syntax.InductiveDefinition
inductiveDefinitionWithLocality original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "coinductive"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
              Core.projectionField = (Core.Name "coinductive")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bodies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
              Core.projectionField = (Core.Name "bodies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

let_ :: Phantoms.TTerm Syntax.LetBindings -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Let
let_ bindings in_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Let"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTTerm in_)}]}))

letBinder :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.LetBinder
letBinder name type_ term =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)}]}))

letBinderName :: Phantoms.TTerm Syntax.LetBinder -> Phantoms.TTerm Syntax.Name
letBinderName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letBinderTerm :: Phantoms.TTerm Syntax.LetBinder -> Phantoms.TTerm Syntax.Term
letBinderTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
        Core.projectionField = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letBinderType :: Phantoms.TTerm Syntax.LetBinder -> Phantoms.TTerm (Maybe Syntax.Type)
letBinderType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letBinderWithName :: Phantoms.TTerm Syntax.LetBinder -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.LetBinder
letBinderWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letBinderWithTerm :: Phantoms.TTerm Syntax.LetBinder -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.LetBinder
letBinderWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

letBinderWithType :: Phantoms.TTerm Syntax.LetBinder -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.LetBinder
letBinderWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letBindings :: Phantoms.TTerm Syntax.Let -> Phantoms.TTerm Syntax.LetBindings
letBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Let"),
        Core.projectionField = (Core.Name "bindings")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letBindingsDestructuring :: Phantoms.TTerm Syntax.LetDestructuring -> Phantoms.TTerm Syntax.LetBindings
letBindingsDestructuring x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.LetBindings"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "destructuring"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

letBindingsNamed :: Phantoms.TTerm Syntax.LetNamed -> Phantoms.TTerm Syntax.LetBindings
letBindingsNamed x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.LetBindings"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

letDestructuringVariant1 :: Phantoms.TTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TTerm Syntax.LetDestructuring
letDestructuringVariant1 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variant1"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

letDestructuringVariant2 :: Phantoms.TTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TTerm Syntax.LetDestructuring
letDestructuringVariant2 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variant2"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

letDestructuringVariant3 :: Phantoms.TTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TTerm Syntax.LetDestructuring
letDestructuringVariant3 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variant3"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

letDestructuring_Variant1 :: Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm (Maybe Syntax.ReturnAs) -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.LetDestructuring_Variant1
letDestructuring_Variant1 names returnAs term =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTTerm names)},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Phantoms.unTTerm returnAs)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)}]}))

letDestructuring_Variant1Names :: Phantoms.TTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TTerm [Syntax.Name]
letDestructuring_Variant1Names x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
        Core.projectionField = (Core.Name "names")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letDestructuring_Variant1ReturnAs :: Phantoms.TTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TTerm (Maybe Syntax.ReturnAs)
letDestructuring_Variant1ReturnAs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
        Core.projectionField = (Core.Name "returnAs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letDestructuring_Variant1Term :: Phantoms.TTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TTerm Syntax.Term
letDestructuring_Variant1Term x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
        Core.projectionField = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letDestructuring_Variant1WithNames :: Phantoms.TTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm Syntax.LetDestructuring_Variant1
letDestructuring_Variant1WithNames original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionField = (Core.Name "returnAs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letDestructuring_Variant1WithReturnAs :: Phantoms.TTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TTerm (Maybe Syntax.ReturnAs) -> Phantoms.TTerm Syntax.LetDestructuring_Variant1
letDestructuring_Variant1WithReturnAs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionField = (Core.Name "names")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letDestructuring_Variant1WithTerm :: Phantoms.TTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.LetDestructuring_Variant1
letDestructuring_Variant1WithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionField = (Core.Name "names")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionField = (Core.Name "returnAs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

letDestructuring_Variant2 :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm (Maybe Syntax.Term100) -> Phantoms.TTerm Syntax.LetDestructuring_Variant2
letDestructuring_Variant2 pattern term return =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTTerm return)}]}))

letDestructuring_Variant2Pattern :: Phantoms.TTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TTerm Syntax.Pattern
letDestructuring_Variant2Pattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letDestructuring_Variant2Return :: Phantoms.TTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TTerm (Maybe Syntax.Term100)
letDestructuring_Variant2Return x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
        Core.projectionField = (Core.Name "return")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letDestructuring_Variant2Term :: Phantoms.TTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TTerm Syntax.Term
letDestructuring_Variant2Term x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
        Core.projectionField = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letDestructuring_Variant2WithPattern :: Phantoms.TTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.LetDestructuring_Variant2
letDestructuring_Variant2WithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionField = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letDestructuring_Variant2WithReturn :: Phantoms.TTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TTerm (Maybe Syntax.Term100) -> Phantoms.TTerm Syntax.LetDestructuring_Variant2
letDestructuring_Variant2WithReturn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

letDestructuring_Variant2WithTerm :: Phantoms.TTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.LetDestructuring_Variant2
letDestructuring_Variant2WithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionField = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letDestructuring_Variant3 :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Term100 -> Phantoms.TTerm Syntax.LetDestructuring_Variant3
letDestructuring_Variant3 pattern1 pattern2 term return =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern1"),
          Core.fieldTerm = (Phantoms.unTTerm pattern1)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern2"),
          Core.fieldTerm = (Phantoms.unTTerm pattern2)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTTerm return)}]}))

letDestructuring_Variant3Pattern1 :: Phantoms.TTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TTerm Syntax.Pattern
letDestructuring_Variant3Pattern1 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
        Core.projectionField = (Core.Name "pattern1")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letDestructuring_Variant3Pattern2 :: Phantoms.TTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TTerm Syntax.Pattern
letDestructuring_Variant3Pattern2 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
        Core.projectionField = (Core.Name "pattern2")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letDestructuring_Variant3Return :: Phantoms.TTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TTerm Syntax.Term100
letDestructuring_Variant3Return x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
        Core.projectionField = (Core.Name "return")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letDestructuring_Variant3Term :: Phantoms.TTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TTerm Syntax.Term
letDestructuring_Variant3Term x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
        Core.projectionField = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letDestructuring_Variant3WithPattern1 :: Phantoms.TTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithPattern1 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern1"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "pattern2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letDestructuring_Variant3WithPattern2 :: Phantoms.TTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithPattern2 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "pattern1")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern2"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letDestructuring_Variant3WithReturn :: Phantoms.TTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TTerm Syntax.Term100 -> Phantoms.TTerm Syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithReturn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "pattern1")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "pattern2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

letDestructuring_Variant3WithTerm :: Phantoms.TTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "pattern1")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "pattern2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionField = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letIn :: Phantoms.TTerm Syntax.Let -> Phantoms.TTerm Syntax.Term
letIn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Let"),
        Core.projectionField = (Core.Name "in")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letNamed :: Phantoms.TTerm Syntax.LetBinder -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm Syntax.LetNamed
letNamed binder binders =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binder"),
          Core.fieldTerm = (Phantoms.unTTerm binder)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm binders)}]}))

letNamedBinder :: Phantoms.TTerm Syntax.LetNamed -> Phantoms.TTerm Syntax.LetBinder
letNamedBinder x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
        Core.projectionField = (Core.Name "binder")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letNamedBinders :: Phantoms.TTerm Syntax.LetNamed -> Phantoms.TTerm [Syntax.Binder]
letNamedBinders x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
        Core.projectionField = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letNamedWithBinder :: Phantoms.TTerm Syntax.LetNamed -> Phantoms.TTerm Syntax.LetBinder -> Phantoms.TTerm Syntax.LetNamed
letNamedWithBinder original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binder"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letNamedWithBinders :: Phantoms.TTerm Syntax.LetNamed -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm Syntax.LetNamed
letNamedWithBinders original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
              Core.projectionField = (Core.Name "binder")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

letWithBindings :: Phantoms.TTerm Syntax.Let -> Phantoms.TTerm Syntax.LetBindings -> Phantoms.TTerm Syntax.Let
letWithBindings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Let"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Let"),
              Core.projectionField = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letWithIn :: Phantoms.TTerm Syntax.Let -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Let
letWithIn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Let"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Let"),
              Core.projectionField = (Core.Name "bindings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

localityGlobal :: Phantoms.TTerm Syntax.Locality
localityGlobal =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Locality"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "global"),
        Core.fieldTerm = Core.TermUnit}}))

localityLocal :: Phantoms.TTerm Syntax.Locality
localityLocal =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Locality"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "local"),
        Core.fieldTerm = Core.TermUnit}}))

match :: Phantoms.TTerm [Syntax.CaseItem] -> Phantoms.TTerm (Maybe Syntax.Term100) -> Phantoms.TTerm Bool -> Phantoms.TTerm [Syntax.Equation] -> Phantoms.TTerm Syntax.Match
match caseItems return pipe equations =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "caseItems"),
          Core.fieldTerm = (Phantoms.unTTerm caseItems)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTTerm return)},
        Core.Field {
          Core.fieldName = (Core.Name "pipe"),
          Core.fieldTerm = (Phantoms.unTTerm pipe)},
        Core.Field {
          Core.fieldName = (Core.Name "equations"),
          Core.fieldTerm = (Phantoms.unTTerm equations)}]}))

matchCaseItems :: Phantoms.TTerm Syntax.Match -> Phantoms.TTerm [Syntax.CaseItem]
matchCaseItems x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
        Core.projectionField = (Core.Name "caseItems")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchEquations :: Phantoms.TTerm Syntax.Match -> Phantoms.TTerm [Syntax.Equation]
matchEquations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
        Core.projectionField = (Core.Name "equations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchPipe :: Phantoms.TTerm Syntax.Match -> Phantoms.TTerm Bool
matchPipe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
        Core.projectionField = (Core.Name "pipe")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchReturn :: Phantoms.TTerm Syntax.Match -> Phantoms.TTerm (Maybe Syntax.Term100)
matchReturn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
        Core.projectionField = (Core.Name "return")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchWithCaseItems :: Phantoms.TTerm Syntax.Match -> Phantoms.TTerm [Syntax.CaseItem] -> Phantoms.TTerm Syntax.Match
matchWithCaseItems original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "caseItems"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionField = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pipe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionField = (Core.Name "pipe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "equations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionField = (Core.Name "equations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

matchWithEquations :: Phantoms.TTerm Syntax.Match -> Phantoms.TTerm [Syntax.Equation] -> Phantoms.TTerm Syntax.Match
matchWithEquations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "caseItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionField = (Core.Name "caseItems")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionField = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pipe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionField = (Core.Name "pipe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "equations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

matchWithPipe :: Phantoms.TTerm Syntax.Match -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Match
matchWithPipe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "caseItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionField = (Core.Name "caseItems")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionField = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pipe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "equations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionField = (Core.Name "equations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

matchWithReturn :: Phantoms.TTerm Syntax.Match -> Phantoms.TTerm (Maybe Syntax.Term100) -> Phantoms.TTerm Syntax.Match
matchWithReturn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "caseItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionField = (Core.Name "caseItems")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pipe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionField = (Core.Name "pipe")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "equations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionField = (Core.Name "equations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleDefinition :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm [Syntax.Sentence] -> Phantoms.TTerm Syntax.ModuleDefinition
moduleDefinition name sentences =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Phantoms.unTTerm sentences)}]}))

moduleDefinitionName :: Phantoms.TTerm Syntax.ModuleDefinition -> Phantoms.TTerm Syntax.Ident
moduleDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleDefinitionSentences :: Phantoms.TTerm Syntax.ModuleDefinition -> Phantoms.TTerm [Syntax.Sentence]
moduleDefinitionSentences x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
        Core.projectionField = (Core.Name "sentences")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleDefinitionWithName :: Phantoms.TTerm Syntax.ModuleDefinition -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.ModuleDefinition
moduleDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
              Core.projectionField = (Core.Name "sentences")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleDefinitionWithSentences :: Phantoms.TTerm Syntax.ModuleDefinition -> Phantoms.TTerm [Syntax.Sentence] -> Phantoms.TTerm Syntax.ModuleDefinition
moduleDefinitionWithSentences original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

name :: Phantoms.TTerm (Maybe Syntax.Ident) -> Phantoms.TTerm Syntax.Name
name x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.Name"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

natural :: Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.Natural
natural x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.Natural"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

naturalArg :: Phantoms.TTerm Syntax.Natural -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.NaturalArg
naturalArg natural term =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "natural"),
          Core.fieldTerm = (Phantoms.unTTerm natural)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)}]}))

naturalArgNatural :: Phantoms.TTerm Syntax.NaturalArg -> Phantoms.TTerm Syntax.Natural
naturalArgNatural x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
        Core.projectionField = (Core.Name "natural")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

naturalArgTerm :: Phantoms.TTerm Syntax.NaturalArg -> Phantoms.TTerm Syntax.Term
naturalArgTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
        Core.projectionField = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

naturalArgWithNatural :: Phantoms.TTerm Syntax.NaturalArg -> Phantoms.TTerm Syntax.Natural -> Phantoms.TTerm Syntax.NaturalArg
naturalArgWithNatural original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "natural"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

naturalArgWithTerm :: Phantoms.TTerm Syntax.NaturalArg -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.NaturalArg
naturalArgWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "natural"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
              Core.projectionField = (Core.Name "natural")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

normalApplication :: Phantoms.TTerm Syntax.Term1 -> Phantoms.TTerm [Syntax.Arg] -> Phantoms.TTerm Syntax.NormalApplication
normalApplication lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

normalApplicationLhs :: Phantoms.TTerm Syntax.NormalApplication -> Phantoms.TTerm Syntax.Term1
normalApplicationLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

normalApplicationRhs :: Phantoms.TTerm Syntax.NormalApplication -> Phantoms.TTerm [Syntax.Arg]
normalApplicationRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

normalApplicationWithLhs :: Phantoms.TTerm Syntax.NormalApplication -> Phantoms.TTerm Syntax.Term1 -> Phantoms.TTerm Syntax.NormalApplication
normalApplicationWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

normalApplicationWithRhs :: Phantoms.TTerm Syntax.NormalApplication -> Phantoms.TTerm [Syntax.Arg] -> Phantoms.TTerm Syntax.NormalApplication
normalApplicationWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

notationDeclaration :: Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm (Maybe Syntax.Natural) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.NotationDeclaration
notationDeclaration notation definition level associativity =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "notation"),
          Core.fieldTerm = (Phantoms.unTTerm notation)},
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Phantoms.unTTerm definition)},
        Core.Field {
          Core.fieldName = (Core.Name "level"),
          Core.fieldTerm = (Phantoms.unTTerm level)},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Phantoms.unTTerm associativity)}]}))

notationDeclarationAssociativity :: Phantoms.TTerm Syntax.NotationDeclaration -> Phantoms.TTerm (Maybe String)
notationDeclarationAssociativity x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
        Core.projectionField = (Core.Name "associativity")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

notationDeclarationDefinition :: Phantoms.TTerm Syntax.NotationDeclaration -> Phantoms.TTerm Syntax.Term
notationDeclarationDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
        Core.projectionField = (Core.Name "definition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

notationDeclarationLevel :: Phantoms.TTerm Syntax.NotationDeclaration -> Phantoms.TTerm (Maybe Syntax.Natural)
notationDeclarationLevel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
        Core.projectionField = (Core.Name "level")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

notationDeclarationNotation :: Phantoms.TTerm Syntax.NotationDeclaration -> Phantoms.TTerm Syntax.String_
notationDeclarationNotation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
        Core.projectionField = (Core.Name "notation")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

notationDeclarationWithAssociativity :: Phantoms.TTerm Syntax.NotationDeclaration -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.NotationDeclaration
notationDeclarationWithAssociativity original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "notation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionField = (Core.Name "notation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionField = (Core.Name "definition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "level"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionField = (Core.Name "level")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

notationDeclarationWithDefinition :: Phantoms.TTerm Syntax.NotationDeclaration -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.NotationDeclaration
notationDeclarationWithDefinition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "notation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionField = (Core.Name "notation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "level"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionField = (Core.Name "level")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionField = (Core.Name "associativity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

notationDeclarationWithLevel :: Phantoms.TTerm Syntax.NotationDeclaration -> Phantoms.TTerm (Maybe Syntax.Natural) -> Phantoms.TTerm Syntax.NotationDeclaration
notationDeclarationWithLevel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "notation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionField = (Core.Name "notation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionField = (Core.Name "definition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "level"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionField = (Core.Name "associativity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

notationDeclarationWithNotation :: Phantoms.TTerm Syntax.NotationDeclaration -> Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm Syntax.NotationDeclaration
notationDeclarationWithNotation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "notation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionField = (Core.Name "definition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "level"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionField = (Core.Name "level")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionField = (Core.Name "associativity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

number :: Phantoms.TTerm Double -> Phantoms.TTerm Syntax.Number
number x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.Number"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

oneTermExplicit :: Phantoms.TTerm Syntax.QualidAnnotated -> Phantoms.TTerm Syntax.OneTerm
oneTermExplicit x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.OneTerm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "explicit"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

oneTermTerm1 :: Phantoms.TTerm Syntax.Term1 -> Phantoms.TTerm Syntax.OneTerm
oneTermTerm1 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.OneTerm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term1"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

openBindersBinders :: Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm Syntax.OpenBinders
openBindersBinders x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.OpenBinders"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binders"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

openBindersType :: Phantoms.TTerm Syntax.TypeBinders -> Phantoms.TTerm Syntax.OpenBinders
openBindersType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.OpenBinders"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pattern0Number :: Phantoms.TTerm Syntax.Number -> Phantoms.TTerm Syntax.Pattern0
pattern0Number x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pattern0Parens :: Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.Pattern0
pattern0Parens x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pattern0Placeholder :: Phantoms.TTerm Syntax.Pattern0
pattern0Placeholder =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "placeholder"),
        Core.fieldTerm = Core.TermUnit}}))

pattern0QualIdAndPattern :: Phantoms.TTerm Syntax.QualidAndPattern -> Phantoms.TTerm Syntax.Pattern0
pattern0QualIdAndPattern x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualIdAndPattern"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pattern0Qualid :: Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm Syntax.Pattern0
pattern0Qualid x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualid"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pattern0String :: Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm Syntax.Pattern0
pattern0String x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pattern1 :: Phantoms.TTerm Syntax.Pattern0 -> Phantoms.TTerm (Maybe Syntax.ScopeKey) -> Phantoms.TTerm Syntax.Pattern1
pattern1 pattern scope =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm scope)}]}))

pattern10As :: Phantoms.TTerm Syntax.Pattern10_As -> Phantoms.TTerm Syntax.Pattern10
pattern10As x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "as"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pattern10Patterns :: Phantoms.TTerm Syntax.Pattern10_Patterns -> Phantoms.TTerm Syntax.Pattern10
pattern10Patterns x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "patterns"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pattern10Qualiid :: Phantoms.TTerm Syntax.Pattern10_Qualid -> Phantoms.TTerm Syntax.Pattern10
pattern10Qualiid x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualiid"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pattern10_As :: Phantoms.TTerm Syntax.Pattern1 -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Pattern10_As
pattern10_As pattern as =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm as)}]}))

pattern10_AsAs :: Phantoms.TTerm Syntax.Pattern10_As -> Phantoms.TTerm Syntax.Name
pattern10_AsAs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
        Core.projectionField = (Core.Name "as")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pattern10_AsPattern :: Phantoms.TTerm Syntax.Pattern10_As -> Phantoms.TTerm Syntax.Pattern1
pattern10_AsPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pattern10_AsWithAs :: Phantoms.TTerm Syntax.Pattern10_As -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Pattern10_As
pattern10_AsWithAs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pattern10_AsWithPattern :: Phantoms.TTerm Syntax.Pattern10_As -> Phantoms.TTerm Syntax.Pattern1 -> Phantoms.TTerm Syntax.Pattern10_As
pattern10_AsWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
              Core.projectionField = (Core.Name "as")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pattern10_Patterns :: Phantoms.TTerm Syntax.Pattern1 -> Phantoms.TTerm [Syntax.Pattern1] -> Phantoms.TTerm Syntax.Pattern10_Patterns
pattern10_Patterns pattern patterns =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Phantoms.unTTerm patterns)}]}))

pattern10_PatternsPattern :: Phantoms.TTerm Syntax.Pattern10_Patterns -> Phantoms.TTerm Syntax.Pattern1
pattern10_PatternsPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pattern10_PatternsPatterns :: Phantoms.TTerm Syntax.Pattern10_Patterns -> Phantoms.TTerm [Syntax.Pattern1]
pattern10_PatternsPatterns x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
        Core.projectionField = (Core.Name "patterns")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pattern10_PatternsWithPattern :: Phantoms.TTerm Syntax.Pattern10_Patterns -> Phantoms.TTerm Syntax.Pattern1 -> Phantoms.TTerm Syntax.Pattern10_Patterns
pattern10_PatternsWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
              Core.projectionField = (Core.Name "patterns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pattern10_PatternsWithPatterns :: Phantoms.TTerm Syntax.Pattern10_Patterns -> Phantoms.TTerm [Syntax.Pattern1] -> Phantoms.TTerm Syntax.Pattern10_Patterns
pattern10_PatternsWithPatterns original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pattern10_Qualid :: Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm [Syntax.Pattern1] -> Phantoms.TTerm Syntax.Pattern10_Qualid
pattern10_Qualid qualid patterns =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Phantoms.unTTerm qualid)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Phantoms.unTTerm patterns)}]}))

pattern10_QualidPatterns :: Phantoms.TTerm Syntax.Pattern10_Qualid -> Phantoms.TTerm [Syntax.Pattern1]
pattern10_QualidPatterns x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
        Core.projectionField = (Core.Name "patterns")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pattern10_QualidQualid :: Phantoms.TTerm Syntax.Pattern10_Qualid -> Phantoms.TTerm Syntax.Qualid
pattern10_QualidQualid x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
        Core.projectionField = (Core.Name "qualid")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pattern10_QualidWithPatterns :: Phantoms.TTerm Syntax.Pattern10_Qualid -> Phantoms.TTerm [Syntax.Pattern1] -> Phantoms.TTerm Syntax.Pattern10_Qualid
pattern10_QualidWithPatterns original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
              Core.projectionField = (Core.Name "qualid")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pattern10_QualidWithQualid :: Phantoms.TTerm Syntax.Pattern10_Qualid -> Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm Syntax.Pattern10_Qualid
pattern10_QualidWithQualid original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
              Core.projectionField = (Core.Name "patterns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pattern1Pattern :: Phantoms.TTerm Syntax.Pattern1 -> Phantoms.TTerm Syntax.Pattern0
pattern1Pattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pattern1Scope :: Phantoms.TTerm Syntax.Pattern1 -> Phantoms.TTerm (Maybe Syntax.ScopeKey)
pattern1Scope x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
        Core.projectionField = (Core.Name "scope")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pattern1WithPattern :: Phantoms.TTerm Syntax.Pattern1 -> Phantoms.TTerm Syntax.Pattern0 -> Phantoms.TTerm Syntax.Pattern1
pattern1WithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
              Core.projectionField = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pattern1WithScope :: Phantoms.TTerm Syntax.Pattern1 -> Phantoms.TTerm (Maybe Syntax.ScopeKey) -> Phantoms.TTerm Syntax.Pattern1
pattern1WithScope original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

patternPattern :: Phantoms.TTerm Syntax.Pattern10 -> Phantoms.TTerm Syntax.Pattern
patternPattern x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternTerm :: Phantoms.TTerm (Maybe Syntax.Term) -> Phantoms.TTerm Syntax.Pattern
patternTerm x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primitiveNotationsNumber :: Phantoms.TTerm Syntax.Number -> Phantoms.TTerm Syntax.PrimitiveNotations
primitiveNotationsNumber x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.PrimitiveNotations"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

primitiveNotationsString :: Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm Syntax.PrimitiveNotations
primitiveNotationsString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.PrimitiveNotations"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

qualid :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm [Syntax.FieldIdent] -> Phantoms.TTerm Syntax.Qualid
qualid id fieldIds =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "fieldIds"),
          Core.fieldTerm = (Phantoms.unTTerm fieldIds)}]}))

qualidAndPattern :: Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.QualidAndPattern
qualidAndPattern qualid pattern =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Phantoms.unTTerm qualid)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)}]}))

qualidAndPatternPattern :: Phantoms.TTerm Syntax.QualidAndPattern -> Phantoms.TTerm Syntax.Pattern
qualidAndPatternPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualidAndPatternQualid :: Phantoms.TTerm Syntax.QualidAndPattern -> Phantoms.TTerm Syntax.Qualid
qualidAndPatternQualid x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
        Core.projectionField = (Core.Name "qualid")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualidAndPatternWithPattern :: Phantoms.TTerm Syntax.QualidAndPattern -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.QualidAndPattern
qualidAndPatternWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
              Core.projectionField = (Core.Name "qualid")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

qualidAndPatternWithQualid :: Phantoms.TTerm Syntax.QualidAndPattern -> Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm Syntax.QualidAndPattern
qualidAndPatternWithQualid original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

qualidAnnotated :: Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm (Maybe Syntax.UnivAnnot) -> Phantoms.TTerm Syntax.QualidAnnotated
qualidAnnotated qualid univAnnot =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Phantoms.unTTerm qualid)},
        Core.Field {
          Core.fieldName = (Core.Name "univAnnot"),
          Core.fieldTerm = (Phantoms.unTTerm univAnnot)}]}))

qualidAnnotatedQualid :: Phantoms.TTerm Syntax.QualidAnnotated -> Phantoms.TTerm Syntax.Qualid
qualidAnnotatedQualid x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
        Core.projectionField = (Core.Name "qualid")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualidAnnotatedUnivAnnot :: Phantoms.TTerm Syntax.QualidAnnotated -> Phantoms.TTerm (Maybe Syntax.UnivAnnot)
qualidAnnotatedUnivAnnot x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
        Core.projectionField = (Core.Name "univAnnot")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualidAnnotatedWithQualid :: Phantoms.TTerm Syntax.QualidAnnotated -> Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm Syntax.QualidAnnotated
qualidAnnotatedWithQualid original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "univAnnot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
              Core.projectionField = (Core.Name "univAnnot")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

qualidAnnotatedWithUnivAnnot :: Phantoms.TTerm Syntax.QualidAnnotated -> Phantoms.TTerm (Maybe Syntax.UnivAnnot) -> Phantoms.TTerm Syntax.QualidAnnotated
qualidAnnotatedWithUnivAnnot original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
              Core.projectionField = (Core.Name "qualid")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "univAnnot"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

qualidFieldIds :: Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm [Syntax.FieldIdent]
qualidFieldIds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
        Core.projectionField = (Core.Name "fieldIds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualidId :: Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm Syntax.Ident
qualidId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
        Core.projectionField = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualidWithFieldIds :: Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm [Syntax.FieldIdent] -> Phantoms.TTerm Syntax.Qualid
qualidWithFieldIds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fieldIds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

qualidWithId :: Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.Qualid
qualidWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fieldIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
              Core.projectionField = (Core.Name "fieldIds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

recordBody :: Phantoms.TTerm (Maybe Syntax.Ident) -> Phantoms.TTerm [Syntax.RecordField] -> Phantoms.TTerm Syntax.RecordBody
recordBody constructor fields =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Phantoms.unTTerm constructor)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)}]}))

recordBodyConstructor :: Phantoms.TTerm Syntax.RecordBody -> Phantoms.TTerm (Maybe Syntax.Ident)
recordBodyConstructor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
        Core.projectionField = (Core.Name "constructor")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordBodyFields :: Phantoms.TTerm Syntax.RecordBody -> Phantoms.TTerm [Syntax.RecordField]
recordBodyFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
        Core.projectionField = (Core.Name "fields")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordBodyWithConstructor :: Phantoms.TTerm Syntax.RecordBody -> Phantoms.TTerm (Maybe Syntax.Ident) -> Phantoms.TTerm Syntax.RecordBody
recordBodyWithConstructor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
              Core.projectionField = (Core.Name "fields")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

recordBodyWithFields :: Phantoms.TTerm Syntax.RecordBody -> Phantoms.TTerm [Syntax.RecordField] -> Phantoms.TTerm Syntax.RecordBody
recordBodyWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
              Core.projectionField = (Core.Name "constructor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

recordDefinition :: Phantoms.TTerm (Maybe Syntax.Locality) -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm (Maybe Syntax.Sort) -> Phantoms.TTerm Syntax.RecordBody -> Phantoms.TTerm Syntax.RecordDefinition
recordDefinition locality name binders sort body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Phantoms.unTTerm locality)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Phantoms.unTTerm sort)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

recordDefinitionBinders :: Phantoms.TTerm Syntax.RecordDefinition -> Phantoms.TTerm [Syntax.Binder]
recordDefinitionBinders x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
        Core.projectionField = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordDefinitionBody :: Phantoms.TTerm Syntax.RecordDefinition -> Phantoms.TTerm Syntax.RecordBody
recordDefinitionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordDefinitionLocality :: Phantoms.TTerm Syntax.RecordDefinition -> Phantoms.TTerm (Maybe Syntax.Locality)
recordDefinitionLocality x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
        Core.projectionField = (Core.Name "locality")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordDefinitionName :: Phantoms.TTerm Syntax.RecordDefinition -> Phantoms.TTerm Syntax.Ident
recordDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordDefinitionSort :: Phantoms.TTerm Syntax.RecordDefinition -> Phantoms.TTerm (Maybe Syntax.Sort)
recordDefinitionSort x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
        Core.projectionField = (Core.Name "sort")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordDefinitionWithBinders :: Phantoms.TTerm Syntax.RecordDefinition -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm Syntax.RecordDefinition
recordDefinitionWithBinders original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionField = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionField = (Core.Name "sort")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

recordDefinitionWithBody :: Phantoms.TTerm Syntax.RecordDefinition -> Phantoms.TTerm Syntax.RecordBody -> Phantoms.TTerm Syntax.RecordDefinition
recordDefinitionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionField = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionField = (Core.Name "sort")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

recordDefinitionWithLocality :: Phantoms.TTerm Syntax.RecordDefinition -> Phantoms.TTerm (Maybe Syntax.Locality) -> Phantoms.TTerm Syntax.RecordDefinition
recordDefinitionWithLocality original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionField = (Core.Name "sort")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

recordDefinitionWithName :: Phantoms.TTerm Syntax.RecordDefinition -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.RecordDefinition
recordDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionField = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionField = (Core.Name "sort")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

recordDefinitionWithSort :: Phantoms.TTerm Syntax.RecordDefinition -> Phantoms.TTerm (Maybe Syntax.Sort) -> Phantoms.TTerm Syntax.RecordDefinition
recordDefinitionWithSort original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionField = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

recordField :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.RecordField
recordField name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

recordFieldName :: Phantoms.TTerm Syntax.RecordField -> Phantoms.TTerm Syntax.Ident
recordFieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordFieldType :: Phantoms.TTerm Syntax.RecordField -> Phantoms.TTerm Syntax.Type
recordFieldType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordFieldWithName :: Phantoms.TTerm Syntax.RecordField -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.RecordField
recordFieldWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

recordFieldWithType :: Phantoms.TTerm Syntax.RecordField -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.RecordField
recordFieldWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

requireImport :: Phantoms.TTerm (Maybe Syntax.Qualid) -> Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe Syntax.ImportQualification) -> Phantoms.TTerm [Syntax.Qualid] -> Phantoms.TTerm Syntax.RequireImport
requireImport from require qualification modules =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Phantoms.unTTerm from)},
        Core.Field {
          Core.fieldName = (Core.Name "require"),
          Core.fieldTerm = (Phantoms.unTTerm require)},
        Core.Field {
          Core.fieldName = (Core.Name "qualification"),
          Core.fieldTerm = (Phantoms.unTTerm qualification)},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Phantoms.unTTerm modules)}]}))

requireImportFrom :: Phantoms.TTerm Syntax.RequireImport -> Phantoms.TTerm (Maybe Syntax.Qualid)
requireImportFrom x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
        Core.projectionField = (Core.Name "from")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

requireImportModules :: Phantoms.TTerm Syntax.RequireImport -> Phantoms.TTerm [Syntax.Qualid]
requireImportModules x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
        Core.projectionField = (Core.Name "modules")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

requireImportQualification :: Phantoms.TTerm Syntax.RequireImport -> Phantoms.TTerm (Maybe Syntax.ImportQualification)
requireImportQualification x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
        Core.projectionField = (Core.Name "qualification")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

requireImportRequire :: Phantoms.TTerm Syntax.RequireImport -> Phantoms.TTerm Bool
requireImportRequire x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
        Core.projectionField = (Core.Name "require")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

requireImportWithFrom :: Phantoms.TTerm Syntax.RequireImport -> Phantoms.TTerm (Maybe Syntax.Qualid) -> Phantoms.TTerm Syntax.RequireImport
requireImportWithFrom original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "require"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionField = (Core.Name "require")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionField = (Core.Name "qualification")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionField = (Core.Name "modules")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

requireImportWithModules :: Phantoms.TTerm Syntax.RequireImport -> Phantoms.TTerm [Syntax.Qualid] -> Phantoms.TTerm Syntax.RequireImport
requireImportWithModules original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionField = (Core.Name "from")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "require"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionField = (Core.Name "require")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionField = (Core.Name "qualification")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

requireImportWithQualification :: Phantoms.TTerm Syntax.RequireImport -> Phantoms.TTerm (Maybe Syntax.ImportQualification) -> Phantoms.TTerm Syntax.RequireImport
requireImportWithQualification original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionField = (Core.Name "from")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "require"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionField = (Core.Name "require")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualification"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionField = (Core.Name "modules")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

requireImportWithRequire :: Phantoms.TTerm Syntax.RequireImport -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.RequireImport
requireImportWithRequire original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionField = (Core.Name "from")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "require"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qualification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionField = (Core.Name "qualification")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionField = (Core.Name "modules")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

returnAs :: Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.Term100 -> Phantoms.TTerm Syntax.ReturnAs
returnAs as return =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm as)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTTerm return)}]}))

returnAsAs :: Phantoms.TTerm Syntax.ReturnAs -> Phantoms.TTerm (Maybe Syntax.Name)
returnAsAs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
        Core.projectionField = (Core.Name "as")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

returnAsReturn :: Phantoms.TTerm Syntax.ReturnAs -> Phantoms.TTerm Syntax.Term100
returnAsReturn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
        Core.projectionField = (Core.Name "return")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

returnAsWithAs :: Phantoms.TTerm Syntax.ReturnAs -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.ReturnAs
returnAsWithAs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
              Core.projectionField = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

returnAsWithReturn :: Phantoms.TTerm Syntax.ReturnAs -> Phantoms.TTerm Syntax.Term100 -> Phantoms.TTerm Syntax.ReturnAs
returnAsWithReturn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
              Core.projectionField = (Core.Name "as")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

scopeKey :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.ScopeKey
scopeKey x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.ScopeKey"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

sectionDefinition :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm [Syntax.Sentence] -> Phantoms.TTerm Syntax.SectionDefinition
sectionDefinition name sentences =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Phantoms.unTTerm sentences)}]}))

sectionDefinitionName :: Phantoms.TTerm Syntax.SectionDefinition -> Phantoms.TTerm Syntax.Ident
sectionDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sectionDefinitionSentences :: Phantoms.TTerm Syntax.SectionDefinition -> Phantoms.TTerm [Syntax.Sentence]
sectionDefinitionSentences x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
        Core.projectionField = (Core.Name "sentences")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sectionDefinitionWithName :: Phantoms.TTerm Syntax.SectionDefinition -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.SectionDefinition
sectionDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
              Core.projectionField = (Core.Name "sentences")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

sectionDefinitionWithSentences :: Phantoms.TTerm Syntax.SectionDefinition -> Phantoms.TTerm [Syntax.Sentence] -> Phantoms.TTerm Syntax.SectionDefinition
sectionDefinitionWithSentences original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

sentence :: Phantoms.TTerm (Maybe Syntax.Comment) -> Phantoms.TTerm Syntax.SentenceContent -> Phantoms.TTerm Syntax.Sentence
sentence comment content =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Phantoms.unTTerm comment)},
        Core.Field {
          Core.fieldName = (Core.Name "content"),
          Core.fieldTerm = (Phantoms.unTTerm content)}]}))

sentenceComment :: Phantoms.TTerm Syntax.Sentence -> Phantoms.TTerm (Maybe Syntax.Comment)
sentenceComment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
        Core.projectionField = (Core.Name "comment")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sentenceContent :: Phantoms.TTerm Syntax.Sentence -> Phantoms.TTerm Syntax.SentenceContent
sentenceContent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
        Core.projectionField = (Core.Name "content")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sentenceContentAxiom :: Phantoms.TTerm Syntax.AxiomDeclaration -> Phantoms.TTerm Syntax.SentenceContent
sentenceContentAxiom x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "axiom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

sentenceContentDefinition :: Phantoms.TTerm Syntax.Definition -> Phantoms.TTerm Syntax.SentenceContent
sentenceContentDefinition x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "definition"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

sentenceContentFixpoint :: Phantoms.TTerm Syntax.FixpointDefinition -> Phantoms.TTerm Syntax.SentenceContent
sentenceContentFixpoint x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fixpoint"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

sentenceContentInductive :: Phantoms.TTerm Syntax.InductiveDefinition -> Phantoms.TTerm Syntax.SentenceContent
sentenceContentInductive x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inductive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

sentenceContentModule :: Phantoms.TTerm Syntax.ModuleDefinition -> Phantoms.TTerm Syntax.SentenceContent
sentenceContentModule x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "module"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

sentenceContentNotation :: Phantoms.TTerm Syntax.NotationDeclaration -> Phantoms.TTerm Syntax.SentenceContent
sentenceContentNotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

sentenceContentRecord :: Phantoms.TTerm Syntax.RecordDefinition -> Phantoms.TTerm Syntax.SentenceContent
sentenceContentRecord x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

sentenceContentRequireImport :: Phantoms.TTerm Syntax.RequireImport -> Phantoms.TTerm Syntax.SentenceContent
sentenceContentRequireImport x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "requireImport"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

sentenceContentSection :: Phantoms.TTerm Syntax.SectionDefinition -> Phantoms.TTerm Syntax.SentenceContent
sentenceContentSection x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "section"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

sentenceContentTheorem :: Phantoms.TTerm Syntax.TheoremBody -> Phantoms.TTerm Syntax.SentenceContent
sentenceContentTheorem x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "theorem"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

sentenceWithComment :: Phantoms.TTerm Syntax.Sentence -> Phantoms.TTerm (Maybe Syntax.Comment) -> Phantoms.TTerm Syntax.Sentence
sentenceWithComment original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "content"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
              Core.projectionField = (Core.Name "content")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

sentenceWithContent :: Phantoms.TTerm Syntax.Sentence -> Phantoms.TTerm Syntax.SentenceContent -> Phantoms.TTerm Syntax.Sentence
sentenceWithContent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
              Core.projectionField = (Core.Name "comment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "content"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

sortProp :: Phantoms.TTerm Syntax.Sort
sortProp =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "prop"),
        Core.fieldTerm = Core.TermUnit}}))

sortSProp :: Phantoms.TTerm Syntax.Sort
sortSProp =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sProp"),
        Core.fieldTerm = Core.TermUnit}}))

sortSet :: Phantoms.TTerm Syntax.Sort
sortSet =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))

sortType :: Phantoms.TTerm Syntax.Sort
sortType =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = Core.TermUnit}}))

sortTypeWithAnyUniverse :: Phantoms.TTerm Syntax.Sort
sortTypeWithAnyUniverse =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeWithAnyUniverse"),
        Core.fieldTerm = Core.TermUnit}}))

sortTypeWithUniverse :: Phantoms.TTerm Syntax.Universe -> Phantoms.TTerm Syntax.Sort
sortTypeWithUniverse x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeWithUniverse"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

string :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.String_
string x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.String"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

term0Evar :: Phantoms.TTerm Syntax.ExistentialVariable -> Phantoms.TTerm Syntax.Term0
term0Evar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "evar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term0Generalizing :: Phantoms.TTerm Syntax.Term0
term0Generalizing =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "generalizing"),
        Core.fieldTerm = Core.TermUnit}}))

term0Ltac :: Phantoms.TTerm Syntax.Term0
term0Ltac =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ltac"),
        Core.fieldTerm = Core.TermUnit}}))

term0Match :: Phantoms.TTerm Syntax.Match -> Phantoms.TTerm Syntax.Term0
term0Match x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term0Parens :: Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Term0
term0Parens x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term0PrimitiveNotations :: Phantoms.TTerm Syntax.PrimitiveNotations -> Phantoms.TTerm Syntax.Term0
term0PrimitiveNotations x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitiveNotations"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term0QualidAnnotated :: Phantoms.TTerm Syntax.QualidAnnotated -> Phantoms.TTerm Syntax.Term0
term0QualidAnnotated x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualidAnnotated"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term0Record :: Phantoms.TTerm Syntax.Term0
term0Record =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = Core.TermUnit}}))

term0Sort :: Phantoms.TTerm Syntax.Sort -> Phantoms.TTerm Syntax.Term0
term0Sort x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sort"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term100Cast :: Phantoms.TTerm Syntax.TypeCast -> Phantoms.TTerm Syntax.Term100
term100Cast x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term100"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cast"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term100Term10 :: Phantoms.TTerm Syntax.Term10 -> Phantoms.TTerm Syntax.Term100
term100Term10 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term100"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term10"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term10Application :: Phantoms.TTerm Syntax.Application -> Phantoms.TTerm Syntax.Term10
term10Application x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term10"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term10OneTerm :: Phantoms.TTerm Syntax.OneTerm -> Phantoms.TTerm Syntax.Term10
term10OneTerm x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term10"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "oneTerm"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

term1Projection :: Phantoms.TTerm Syntax.Term1
term1Projection =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term1"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "projection"),
        Core.fieldTerm = Core.TermUnit}}))

term1Scope :: Phantoms.TTerm Syntax.Term1
term1Scope =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term1"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "scope"),
        Core.fieldTerm = Core.TermUnit}}))

term1Term0 :: Phantoms.TTerm Syntax.Term0 -> Phantoms.TTerm Syntax.Term1
term1Term0 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term1"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term0"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termCofix :: Phantoms.TTerm Syntax.Cofix -> Phantoms.TTerm Syntax.Term
termCofix x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cofix"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termFix :: Phantoms.TTerm Syntax.Fix -> Phantoms.TTerm Syntax.Term
termFix x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fix"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termForallOrFun :: Phantoms.TTerm Syntax.ForallOrFun -> Phantoms.TTerm Syntax.Term
termForallOrFun x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forallOrFun"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termIf :: Phantoms.TTerm Syntax.If -> Phantoms.TTerm Syntax.Term
termIf x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termLet :: Phantoms.TTerm Syntax.Let -> Phantoms.TTerm Syntax.Term
termLet x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termTerm100 :: Phantoms.TTerm Syntax.Term100 -> Phantoms.TTerm Syntax.Term
termTerm100 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term100"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

theoremBody :: Phantoms.TTerm Syntax.TheoremKind -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.TheoremBody
theoremBody kind name binders type_ proof =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Phantoms.unTTerm kind)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "proof"),
          Core.fieldTerm = (Phantoms.unTTerm proof)}]}))

theoremBodyBinders :: Phantoms.TTerm Syntax.TheoremBody -> Phantoms.TTerm [Syntax.Binder]
theoremBodyBinders x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
        Core.projectionField = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

theoremBodyKind :: Phantoms.TTerm Syntax.TheoremBody -> Phantoms.TTerm Syntax.TheoremKind
theoremBodyKind x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
        Core.projectionField = (Core.Name "kind")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

theoremBodyName :: Phantoms.TTerm Syntax.TheoremBody -> Phantoms.TTerm Syntax.Ident
theoremBodyName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

theoremBodyProof :: Phantoms.TTerm Syntax.TheoremBody -> Phantoms.TTerm Syntax.Term
theoremBodyProof x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
        Core.projectionField = (Core.Name "proof")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

theoremBodyType :: Phantoms.TTerm Syntax.TheoremBody -> Phantoms.TTerm Syntax.Type
theoremBodyType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

theoremBodyWithBinders :: Phantoms.TTerm Syntax.TheoremBody -> Phantoms.TTerm [Syntax.Binder] -> Phantoms.TTerm Syntax.TheoremBody
theoremBodyWithBinders original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionField = (Core.Name "kind")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "proof"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionField = (Core.Name "proof")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

theoremBodyWithKind :: Phantoms.TTerm Syntax.TheoremBody -> Phantoms.TTerm Syntax.TheoremKind -> Phantoms.TTerm Syntax.TheoremBody
theoremBodyWithKind original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "proof"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionField = (Core.Name "proof")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

theoremBodyWithName :: Phantoms.TTerm Syntax.TheoremBody -> Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.TheoremBody
theoremBodyWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionField = (Core.Name "kind")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "proof"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionField = (Core.Name "proof")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

theoremBodyWithProof :: Phantoms.TTerm Syntax.TheoremBody -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.TheoremBody
theoremBodyWithProof original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionField = (Core.Name "kind")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "proof"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

theoremBodyWithType :: Phantoms.TTerm Syntax.TheoremBody -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TheoremBody
theoremBodyWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionField = (Core.Name "kind")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionField = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "proof"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionField = (Core.Name "proof")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

theoremKindCorollary :: Phantoms.TTerm Syntax.TheoremKind
theoremKindCorollary =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TheoremKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "corollary"),
        Core.fieldTerm = Core.TermUnit}}))

theoremKindExample :: Phantoms.TTerm Syntax.TheoremKind
theoremKindExample =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TheoremKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "example"),
        Core.fieldTerm = Core.TermUnit}}))

theoremKindLemma :: Phantoms.TTerm Syntax.TheoremKind
theoremKindLemma =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TheoremKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lemma"),
        Core.fieldTerm = Core.TermUnit}}))

theoremKindProposition :: Phantoms.TTerm Syntax.TheoremKind
theoremKindProposition =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TheoremKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "proposition"),
        Core.fieldTerm = Core.TermUnit}}))

theoremKindTheorem :: Phantoms.TTerm Syntax.TheoremKind
theoremKindTheorem =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TheoremKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "theorem"),
        Core.fieldTerm = Core.TermUnit}}))

type_ :: Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Type
type_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.Type"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

typeBinders :: Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeBinders
typeBinders names type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTTerm names)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

typeBindersNames :: Phantoms.TTerm Syntax.TypeBinders -> Phantoms.TTerm [Syntax.Name]
typeBindersNames x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
        Core.projectionField = (Core.Name "names")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeBindersType :: Phantoms.TTerm Syntax.TypeBinders -> Phantoms.TTerm Syntax.Type
typeBindersType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeBindersWithNames :: Phantoms.TTerm Syntax.TypeBinders -> Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm Syntax.TypeBinders
typeBindersWithNames original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeBindersWithType :: Phantoms.TTerm Syntax.TypeBinders -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeBinders
typeBindersWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
              Core.projectionField = (Core.Name "names")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeCast :: Phantoms.TTerm Syntax.Term10 -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeCastOperator -> Phantoms.TTerm Syntax.TypeCast
typeCast term type_ operator =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)}]}))

typeCastOperator :: Phantoms.TTerm Syntax.TypeCast -> Phantoms.TTerm Syntax.TypeCastOperator
typeCastOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
        Core.projectionField = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeCastOperatorNativeCompute :: Phantoms.TTerm Syntax.TypeCastOperator
typeCastOperatorNativeCompute =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TypeCastOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nativeCompute"),
        Core.fieldTerm = Core.TermUnit}}))

typeCastOperatorNormal :: Phantoms.TTerm Syntax.TypeCastOperator
typeCastOperatorNormal =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TypeCastOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = Core.TermUnit}}))

typeCastOperatorVmCompute :: Phantoms.TTerm Syntax.TypeCastOperator
typeCastOperatorVmCompute =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TypeCastOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vmCompute"),
        Core.fieldTerm = Core.TermUnit}}))

typeCastTerm :: Phantoms.TTerm Syntax.TypeCast -> Phantoms.TTerm Syntax.Term10
typeCastTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
        Core.projectionField = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeCastType :: Phantoms.TTerm Syntax.TypeCast -> Phantoms.TTerm Syntax.Type
typeCastType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeCastWithOperator :: Phantoms.TTerm Syntax.TypeCast -> Phantoms.TTerm Syntax.TypeCastOperator -> Phantoms.TTerm Syntax.TypeCast
typeCastWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeCastWithTerm :: Phantoms.TTerm Syntax.TypeCast -> Phantoms.TTerm Syntax.Term10 -> Phantoms.TTerm Syntax.TypeCast
typeCastWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
              Core.projectionField = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeCastWithType :: Phantoms.TTerm Syntax.TypeCast -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeCast
typeCastWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
              Core.projectionField = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeclassConstraint :: Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.TypeclassConstraint
typeclassConstraint name generalizing term =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "generalizing"),
          Core.fieldTerm = (Phantoms.unTTerm generalizing)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)}]}))

typeclassConstraintGeneralizing :: Phantoms.TTerm Syntax.TypeclassConstraint -> Phantoms.TTerm Bool
typeclassConstraintGeneralizing x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
        Core.projectionField = (Core.Name "generalizing")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeclassConstraintName :: Phantoms.TTerm Syntax.TypeclassConstraint -> Phantoms.TTerm (Maybe Syntax.Name)
typeclassConstraintName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeclassConstraintTerm :: Phantoms.TTerm Syntax.TypeclassConstraint -> Phantoms.TTerm Syntax.Term
typeclassConstraintTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
        Core.projectionField = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeclassConstraintWithGeneralizing :: Phantoms.TTerm Syntax.TypeclassConstraint -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.TypeclassConstraint
typeclassConstraintWithGeneralizing original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generalizing"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeclassConstraintWithName :: Phantoms.TTerm Syntax.TypeclassConstraint -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.TypeclassConstraint
typeclassConstraintWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "generalizing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
              Core.projectionField = (Core.Name "generalizing")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeclassConstraintWithTerm :: Phantoms.TTerm Syntax.TypeclassConstraint -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.TypeclassConstraint
typeclassConstraintWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generalizing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
              Core.projectionField = (Core.Name "generalizing")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unComment :: Phantoms.TTerm Syntax.Comment -> Phantoms.TTerm String
unComment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.Comment")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unFieldIdent :: Phantoms.TTerm Syntax.FieldIdent -> Phantoms.TTerm Syntax.Ident
unFieldIdent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.FieldIdent")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unIdent :: Phantoms.TTerm Syntax.Ident -> Phantoms.TTerm Syntax.String_
unIdent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.Ident")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unName :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Ident)
unName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.Name")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unNatural :: Phantoms.TTerm Syntax.Natural -> Phantoms.TTerm Integer
unNatural x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.Natural")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unNumber :: Phantoms.TTerm Syntax.Number -> Phantoms.TTerm Double
unNumber x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.Number")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unScopeKey :: Phantoms.TTerm Syntax.ScopeKey -> Phantoms.TTerm Syntax.Ident
unScopeKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.ScopeKey")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unString :: Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm String
unString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.String")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Term
unType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.Type")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unUnivAnnot :: Phantoms.TTerm Syntax.UnivAnnot -> Phantoms.TTerm [Syntax.UniverseLevel]
unUnivAnnot x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.UnivAnnot")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

univAnnot :: Phantoms.TTerm [Syntax.UniverseLevel] -> Phantoms.TTerm Syntax.UnivAnnot
univAnnot x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.UnivAnnot"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

universeExpr :: Phantoms.TTerm Syntax.Universe_Expr -> Phantoms.TTerm Syntax.Universe
universeExpr x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Universe"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expr"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

universeLevelIgnored :: Phantoms.TTerm Syntax.UniverseLevel
universeLevelIgnored =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseLevel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ignored"),
        Core.fieldTerm = Core.TermUnit}}))

universeLevelProp :: Phantoms.TTerm Syntax.UniverseLevel
universeLevelProp =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseLevel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "prop"),
        Core.fieldTerm = Core.TermUnit}}))

universeLevelQualid :: Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm Syntax.UniverseLevel
universeLevelQualid x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseLevel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualid"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

universeLevelSet :: Phantoms.TTerm Syntax.UniverseLevel
universeLevelSet =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseLevel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))

universeLevelType :: Phantoms.TTerm Syntax.UniverseLevel
universeLevelType =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseLevel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = Core.TermUnit}}))

universeMax :: Phantoms.TTerm [Syntax.Universe_Expr] -> Phantoms.TTerm Syntax.Universe
universeMax x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Universe"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "max"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

universeNameProp :: Phantoms.TTerm Syntax.UniverseName
universeNameProp =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "prop"),
        Core.fieldTerm = Core.TermUnit}}))

universeNameQualid :: Phantoms.TTerm Syntax.Qualid -> Phantoms.TTerm Syntax.UniverseName
universeNameQualid x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualid"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

universeNameSet :: Phantoms.TTerm Syntax.UniverseName
universeNameSet =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))

universe_Expr :: Phantoms.TTerm Syntax.UniverseName -> Phantoms.TTerm (Maybe Syntax.Natural) -> Phantoms.TTerm Syntax.Universe_Expr
universe_Expr name number =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Phantoms.unTTerm number)}]}))

universe_ExprName :: Phantoms.TTerm Syntax.Universe_Expr -> Phantoms.TTerm Syntax.UniverseName
universe_ExprName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

universe_ExprNumber :: Phantoms.TTerm Syntax.Universe_Expr -> Phantoms.TTerm (Maybe Syntax.Natural)
universe_ExprNumber x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
        Core.projectionField = (Core.Name "number")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

universe_ExprWithName :: Phantoms.TTerm Syntax.Universe_Expr -> Phantoms.TTerm Syntax.UniverseName -> Phantoms.TTerm Syntax.Universe_Expr
universe_ExprWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
              Core.projectionField = (Core.Name "number")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

universe_ExprWithNumber :: Phantoms.TTerm Syntax.Universe_Expr -> Phantoms.TTerm (Maybe Syntax.Natural) -> Phantoms.TTerm Syntax.Universe_Expr
universe_ExprWithNumber original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
