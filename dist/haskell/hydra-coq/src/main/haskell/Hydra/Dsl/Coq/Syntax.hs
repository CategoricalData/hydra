-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.coq.syntax

module Hydra.Dsl.Coq.Syntax where
import qualified Hydra.Coq.Syntax as Syntax
import qualified Hydra.Core as Core
import qualified Hydra.Typed as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL constructor for hydra.coq.syntax.AnnotatedApplication
annotatedApplication :: Phantoms.TypedTerm Syntax.QualidAnnotated -> Phantoms.TypedTerm [Syntax.Term1] -> Phantoms.TypedTerm Syntax.AnnotatedApplication
annotatedApplication annot terms =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Phantoms.unTypedTerm annot)},
        Core.Field {
          Core.fieldName = (Core.Name "terms"),
          Core.fieldTerm = (Phantoms.unTypedTerm terms)}]}))
-- | DSL accessor for the annot field of hydra.coq.syntax.AnnotatedApplication
annotatedApplicationAnnot :: Phantoms.TypedTerm Syntax.AnnotatedApplication -> Phantoms.TypedTerm Syntax.QualidAnnotated
annotatedApplicationAnnot x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
        Core.projectionFieldName = (Core.Name "annot")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the terms field of hydra.coq.syntax.AnnotatedApplication
annotatedApplicationTerms :: Phantoms.TypedTerm Syntax.AnnotatedApplication -> Phantoms.TypedTerm [Syntax.Term1]
annotatedApplicationTerms x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
        Core.projectionFieldName = (Core.Name "terms")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the annot field of hydra.coq.syntax.AnnotatedApplication
annotatedApplicationWithAnnot :: Phantoms.TypedTerm Syntax.AnnotatedApplication -> Phantoms.TypedTerm Syntax.QualidAnnotated -> Phantoms.TypedTerm Syntax.AnnotatedApplication
annotatedApplicationWithAnnot original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "terms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
              Core.projectionFieldName = (Core.Name "terms")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the terms field of hydra.coq.syntax.AnnotatedApplication
annotatedApplicationWithTerms :: Phantoms.TypedTerm Syntax.AnnotatedApplication -> Phantoms.TypedTerm [Syntax.Term1] -> Phantoms.TypedTerm Syntax.AnnotatedApplication
annotatedApplicationWithTerms original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "terms"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL injection for the annotated variant of hydra.coq.syntax.Application
applicationAnnotated :: Phantoms.TypedTerm Syntax.AnnotatedApplication -> Phantoms.TypedTerm Syntax.Application
applicationAnnotated x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Application"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotated"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the normal variant of hydra.coq.syntax.Application
applicationNormal :: Phantoms.TypedTerm Syntax.NormalApplication -> Phantoms.TypedTerm Syntax.Application
applicationNormal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Application"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the ident variant of hydra.coq.syntax.Arg
argIdent :: Phantoms.TypedTerm Syntax.IdentArg -> Phantoms.TypedTerm Syntax.Arg
argIdent x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Arg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ident"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the natural variant of hydra.coq.syntax.Arg
argNatural :: Phantoms.TypedTerm Syntax.NaturalArg -> Phantoms.TypedTerm Syntax.Arg
argNatural x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Arg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "natural"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the term variant of hydra.coq.syntax.Arg
argTerm :: Phantoms.TypedTerm Syntax.Term1 -> Phantoms.TypedTerm Syntax.Arg
argTerm x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Arg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.AxiomDeclaration
axiomDeclaration :: Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.Type -> Phantoms.TypedTerm Syntax.AxiomDeclaration
axiomDeclaration name type_ =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm type_)}]}))
-- | DSL accessor for the name field of hydra.coq.syntax.AxiomDeclaration
axiomDeclarationName :: Phantoms.TypedTerm Syntax.AxiomDeclaration -> Phantoms.TypedTerm Syntax.Ident
axiomDeclarationName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.AxiomDeclaration
axiomDeclarationType :: Phantoms.TypedTerm Syntax.AxiomDeclaration -> Phantoms.TypedTerm Syntax.Type
axiomDeclarationType x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.coq.syntax.AxiomDeclaration
axiomDeclarationWithName :: Phantoms.TypedTerm Syntax.AxiomDeclaration -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.AxiomDeclaration
axiomDeclarationWithName original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.coq.syntax.AxiomDeclaration
axiomDeclarationWithType :: Phantoms.TypedTerm Syntax.AxiomDeclaration -> Phantoms.TypedTerm Syntax.Type -> Phantoms.TypedTerm Syntax.AxiomDeclaration
axiomDeclarationWithType original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL injection for the generalizing variant of hydra.coq.syntax.Binder
binderGeneralizing :: Phantoms.TypedTerm Syntax.GeneralizingBinder -> Phantoms.TypedTerm Syntax.Binder
binderGeneralizing x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "generalizing"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the implicit variant of hydra.coq.syntax.Binder
binderImplicit :: Phantoms.TypedTerm Syntax.ImplicitBinders -> Phantoms.TypedTerm Syntax.Binder
binderImplicit x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicit"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the name variant of hydra.coq.syntax.Binder
binderName :: Phantoms.TypedTerm Syntax.Name -> Phantoms.TypedTerm Syntax.Binder
binderName x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the pattern variant of hydra.coq.syntax.Binder
binderPattern :: Phantoms.TypedTerm Syntax.Pattern0 -> Phantoms.TypedTerm Syntax.Binder
binderPattern x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the term variant of hydra.coq.syntax.Binder
binderTerm :: Phantoms.TypedTerm Syntax.LetBinder -> Phantoms.TypedTerm Syntax.Binder
binderTerm x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.coq.syntax.Binder
binderType :: Phantoms.TypedTerm Syntax.TypeBinders -> Phantoms.TypedTerm Syntax.Binder
binderType x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.CaseItem
caseItem :: Phantoms.TypedTerm Syntax.Term100 -> Phantoms.TypedTerm (Maybe Syntax.Name) -> Phantoms.TypedTerm (Maybe Syntax.Pattern) -> Phantoms.TypedTerm Syntax.CaseItem
caseItem term as in_ =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTypedTerm as)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTypedTerm in_)}]}))
-- | DSL accessor for the as field of hydra.coq.syntax.CaseItem
caseItemAs :: Phantoms.TypedTerm Syntax.CaseItem -> Phantoms.TypedTerm (Maybe Syntax.Name)
caseItemAs x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the in field of hydra.coq.syntax.CaseItem
caseItemIn :: Phantoms.TypedTerm Syntax.CaseItem -> Phantoms.TypedTerm (Maybe Syntax.Pattern)
caseItemIn x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
        Core.projectionFieldName = (Core.Name "in")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.CaseItem
caseItemTerm :: Phantoms.TypedTerm Syntax.CaseItem -> Phantoms.TypedTerm Syntax.Term100
caseItemTerm x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the as field of hydra.coq.syntax.CaseItem
caseItemWithAs :: Phantoms.TypedTerm Syntax.CaseItem -> Phantoms.TypedTerm (Maybe Syntax.Name) -> Phantoms.TypedTerm Syntax.CaseItem
caseItemWithAs original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the in field of hydra.coq.syntax.CaseItem
caseItemWithIn :: Phantoms.TypedTerm Syntax.CaseItem -> Phantoms.TypedTerm (Maybe Syntax.Pattern) -> Phantoms.TypedTerm Syntax.CaseItem
caseItemWithIn original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the term field of hydra.coq.syntax.CaseItem
caseItemWithTerm :: Phantoms.TypedTerm Syntax.CaseItem -> Phantoms.TypedTerm Syntax.Term100 -> Phantoms.TypedTerm Syntax.CaseItem
caseItemWithTerm original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.Cofix
cofix :: Phantoms.TypedTerm Syntax.CofixBody -> Phantoms.TypedTerm (Maybe Syntax.CofixQual) -> Phantoms.TypedTerm Syntax.Cofix
cofix body qual =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Phantoms.unTypedTerm qual)}]}))
-- | DSL accessor for the body field of hydra.coq.syntax.Cofix
cofixBody :: Phantoms.TypedTerm Syntax.Cofix -> Phantoms.TypedTerm Syntax.CofixBody
cofixBody x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for hydra.coq.syntax.CofixBody
cofixBody2 :: Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm [Syntax.Binder] -> Phantoms.TypedTerm (Maybe Syntax.Type) -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.CofixBody
cofixBody2 ident binders type_ term =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTypedTerm ident)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm term)}]}))
-- | DSL accessor for the binders field of hydra.coq.syntax.CofixBody
cofixBodyBinders :: Phantoms.TypedTerm Syntax.CofixBody -> Phantoms.TypedTerm [Syntax.Binder]
cofixBodyBinders x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the ident field of hydra.coq.syntax.CofixBody
cofixBodyIdent :: Phantoms.TypedTerm Syntax.CofixBody -> Phantoms.TypedTerm Syntax.Ident
cofixBodyIdent x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
        Core.projectionFieldName = (Core.Name "ident")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.CofixBody
cofixBodyTerm :: Phantoms.TypedTerm Syntax.CofixBody -> Phantoms.TypedTerm Syntax.Term
cofixBodyTerm x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.CofixBody
cofixBodyType :: Phantoms.TypedTerm Syntax.CofixBody -> Phantoms.TypedTerm (Maybe Syntax.Type)
cofixBodyType x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the binders field of hydra.coq.syntax.CofixBody
cofixBodyWithBinders :: Phantoms.TypedTerm Syntax.CofixBody -> Phantoms.TypedTerm [Syntax.Binder] -> Phantoms.TypedTerm Syntax.CofixBody
cofixBodyWithBinders original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the ident field of hydra.coq.syntax.CofixBody
cofixBodyWithIdent :: Phantoms.TypedTerm Syntax.CofixBody -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.CofixBody
cofixBodyWithIdent original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.coq.syntax.CofixBody
cofixBodyWithTerm :: Phantoms.TypedTerm Syntax.CofixBody -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.CofixBody
cofixBodyWithTerm original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the type field of hydra.coq.syntax.CofixBody
cofixBodyWithType :: Phantoms.TypedTerm Syntax.CofixBody -> Phantoms.TypedTerm (Maybe Syntax.Type) -> Phantoms.TypedTerm Syntax.CofixBody
cofixBodyWithType original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL accessor for the qual field of hydra.coq.syntax.Cofix
cofixQual :: Phantoms.TypedTerm Syntax.Cofix -> Phantoms.TypedTerm (Maybe Syntax.CofixQual)
cofixQual x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
        Core.projectionFieldName = (Core.Name "qual")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL injection for the in variant of hydra.coq.syntax.CofixQual
cofixQualIn :: Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.CofixQual
cofixQualIn x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.CofixQual"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the with variant of hydra.coq.syntax.CofixQual
cofixQualWith :: Phantoms.TypedTerm Syntax.CofixWith -> Phantoms.TypedTerm Syntax.CofixQual
cofixQualWith x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.CofixQual"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "with"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.CofixWith
cofixWith :: Phantoms.TypedTerm [Syntax.CofixBody] -> Phantoms.TypedTerm (Maybe Syntax.Ident) -> Phantoms.TypedTerm Syntax.CofixWith
cofixWith with for =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTypedTerm with)},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Phantoms.unTypedTerm for)}]}))
-- | DSL updater for the body field of hydra.coq.syntax.Cofix
cofixWithBody :: Phantoms.TypedTerm Syntax.Cofix -> Phantoms.TypedTerm Syntax.CofixBody -> Phantoms.TypedTerm Syntax.Cofix
cofixWithBody original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
              Core.projectionFieldName = (Core.Name "qual")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL accessor for the for field of hydra.coq.syntax.CofixWith
cofixWithFor :: Phantoms.TypedTerm Syntax.CofixWith -> Phantoms.TypedTerm (Maybe Syntax.Ident)
cofixWithFor x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
        Core.projectionFieldName = (Core.Name "for")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the qual field of hydra.coq.syntax.Cofix
cofixWithQual :: Phantoms.TypedTerm Syntax.Cofix -> Phantoms.TypedTerm (Maybe Syntax.CofixQual) -> Phantoms.TypedTerm Syntax.Cofix
cofixWithQual original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL accessor for the with field of hydra.coq.syntax.CofixWith
cofixWithWith :: Phantoms.TypedTerm Syntax.CofixWith -> Phantoms.TypedTerm [Syntax.CofixBody]
cofixWithWith x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
        Core.projectionFieldName = (Core.Name "with")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the for field of hydra.coq.syntax.CofixWith
cofixWithWithFor :: Phantoms.TypedTerm Syntax.CofixWith -> Phantoms.TypedTerm (Maybe Syntax.Ident) -> Phantoms.TypedTerm Syntax.CofixWith
cofixWithWithFor original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
              Core.projectionFieldName = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the with field of hydra.coq.syntax.CofixWith
cofixWithWithWith :: Phantoms.TypedTerm Syntax.CofixWith -> Phantoms.TypedTerm [Syntax.CofixBody] -> Phantoms.TypedTerm Syntax.CofixWith
cofixWithWithWith original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
              Core.projectionFieldName = (Core.Name "for")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.coq.syntax.Comment wrapper
comment :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Syntax.Comment
comment x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.Comment"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for hydra.coq.syntax.Constructor
constructor :: Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm [Syntax.Binder] -> Phantoms.TypedTerm (Maybe Syntax.Type) -> Phantoms.TypedTerm Syntax.Constructor
constructor name binders type_ =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm type_)}]}))
-- | DSL accessor for the binders field of hydra.coq.syntax.Constructor
constructorBinders :: Phantoms.TypedTerm Syntax.Constructor -> Phantoms.TypedTerm [Syntax.Binder]
constructorBinders x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.coq.syntax.Constructor
constructorName :: Phantoms.TypedTerm Syntax.Constructor -> Phantoms.TypedTerm Syntax.Ident
constructorName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.Constructor
constructorType :: Phantoms.TypedTerm Syntax.Constructor -> Phantoms.TypedTerm (Maybe Syntax.Type)
constructorType x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the binders field of hydra.coq.syntax.Constructor
constructorWithBinders :: Phantoms.TypedTerm Syntax.Constructor -> Phantoms.TypedTerm [Syntax.Binder] -> Phantoms.TypedTerm Syntax.Constructor
constructorWithBinders original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.coq.syntax.Constructor
constructorWithName :: Phantoms.TypedTerm Syntax.Constructor -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.Constructor
constructorWithName original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.coq.syntax.Constructor
constructorWithType :: Phantoms.TypedTerm Syntax.Constructor -> Phantoms.TypedTerm (Maybe Syntax.Type) -> Phantoms.TypedTerm Syntax.Constructor
constructorWithType original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.Definition
definition :: Phantoms.TypedTerm (Maybe Syntax.Locality) -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm [Syntax.Binder] -> Phantoms.TypedTerm (Maybe Syntax.Type) -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.Definition
definition locality name binders type_ body =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Phantoms.unTypedTerm locality)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTypedTerm body)}]}))
-- | DSL accessor for the binders field of hydra.coq.syntax.Definition
definitionBinders :: Phantoms.TypedTerm Syntax.Definition -> Phantoms.TypedTerm [Syntax.Binder]
definitionBinders x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.coq.syntax.Definition
definitionBody :: Phantoms.TypedTerm Syntax.Definition -> Phantoms.TypedTerm Syntax.Term
definitionBody x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the locality field of hydra.coq.syntax.Definition
definitionLocality :: Phantoms.TypedTerm Syntax.Definition -> Phantoms.TypedTerm (Maybe Syntax.Locality)
definitionLocality x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
        Core.projectionFieldName = (Core.Name "locality")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.coq.syntax.Definition
definitionName :: Phantoms.TypedTerm Syntax.Definition -> Phantoms.TypedTerm Syntax.Ident
definitionName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.Definition
definitionType :: Phantoms.TypedTerm Syntax.Definition -> Phantoms.TypedTerm (Maybe Syntax.Type)
definitionType x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the binders field of hydra.coq.syntax.Definition
definitionWithBinders :: Phantoms.TypedTerm Syntax.Definition -> Phantoms.TypedTerm [Syntax.Binder] -> Phantoms.TypedTerm Syntax.Definition
definitionWithBinders original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.coq.syntax.Definition
definitionWithBody :: Phantoms.TypedTerm Syntax.Definition -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.Definition
definitionWithBody original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the locality field of hydra.coq.syntax.Definition
definitionWithLocality :: Phantoms.TypedTerm Syntax.Definition -> Phantoms.TypedTerm (Maybe Syntax.Locality) -> Phantoms.TypedTerm Syntax.Definition
definitionWithLocality original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.coq.syntax.Definition
definitionWithName :: Phantoms.TypedTerm Syntax.Definition -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.Definition
definitionWithName original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.coq.syntax.Definition
definitionWithType :: Phantoms.TypedTerm Syntax.Definition -> Phantoms.TypedTerm (Maybe Syntax.Type) -> Phantoms.TypedTerm Syntax.Definition
definitionWithType original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.Document
document :: Phantoms.TypedTerm [Syntax.Sentence] -> Phantoms.TypedTerm Syntax.Document
document sentences =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Document"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Phantoms.unTypedTerm sentences)}]}))
-- | DSL accessor for the sentences field of hydra.coq.syntax.Document
documentSentences :: Phantoms.TypedTerm Syntax.Document -> Phantoms.TypedTerm [Syntax.Sentence]
documentSentences x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Document"),
        Core.projectionFieldName = (Core.Name "sentences")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the sentences field of hydra.coq.syntax.Document
documentWithSentences :: Phantoms.TypedTerm Syntax.Document -> Phantoms.TypedTerm [Syntax.Sentence] -> Phantoms.TypedTerm Syntax.Document
documentWithSentences original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Document"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.Equation
equation :: Phantoms.TypedTerm [[Syntax.Pattern]] -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.Equation
equation pattern term =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Equation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm term)}]}))
-- | DSL accessor for the pattern field of hydra.coq.syntax.Equation
equationPattern :: Phantoms.TypedTerm Syntax.Equation -> Phantoms.TypedTerm [[Syntax.Pattern]]
equationPattern x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Equation"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.Equation
equationTerm :: Phantoms.TypedTerm Syntax.Equation -> Phantoms.TypedTerm Syntax.Term
equationTerm x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Equation"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the pattern field of hydra.coq.syntax.Equation
equationWithPattern :: Phantoms.TypedTerm Syntax.Equation -> Phantoms.TypedTerm [[Syntax.Pattern]] -> Phantoms.TypedTerm Syntax.Equation
equationWithPattern original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Equation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Equation"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.coq.syntax.Equation
equationWithTerm :: Phantoms.TypedTerm Syntax.Equation -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.Equation
equationWithTerm original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Equation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Equation"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.ExistentialVariable
existentialVariable :: Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.ExistentialVariableVariant -> Phantoms.TypedTerm Syntax.ExistentialVariable
existentialVariable ident variant =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTypedTerm ident)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTypedTerm variant)}]}))
-- | DSL accessor for the ident field of hydra.coq.syntax.ExistentialVariable
existentialVariableIdent :: Phantoms.TypedTerm Syntax.ExistentialVariable -> Phantoms.TypedTerm Syntax.Ident
existentialVariableIdent x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
        Core.projectionFieldName = (Core.Name "ident")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the variant field of hydra.coq.syntax.ExistentialVariable
existentialVariableVariant :: Phantoms.TypedTerm Syntax.ExistentialVariable -> Phantoms.TypedTerm Syntax.ExistentialVariableVariant
existentialVariableVariant x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
        Core.projectionFieldName = (Core.Name "variant")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL injection for the inside1 variant of hydra.coq.syntax.ExistentialVariableVariant
existentialVariableVariantInside1 :: Phantoms.TypedTerm Syntax.ExistentialVariableVariant
existentialVariableVariantInside1 =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariableVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inside1"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the inside2 variant of hydra.coq.syntax.ExistentialVariableVariant
existentialVariableVariantInside2 :: Phantoms.TypedTerm Syntax.ExistentialVariableVariant
existentialVariableVariantInside2 =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariableVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inside2"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the outside variant of hydra.coq.syntax.ExistentialVariableVariant
existentialVariableVariantOutside :: Phantoms.TypedTerm (Maybe Syntax.IdentArg) -> Phantoms.TypedTerm Syntax.ExistentialVariableVariant
existentialVariableVariantOutside x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariableVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "outside"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the placeholder variant of hydra.coq.syntax.ExistentialVariableVariant
existentialVariableVariantPlaceholder :: Phantoms.TypedTerm Syntax.ExistentialVariableVariant
existentialVariableVariantPlaceholder =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariableVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "placeholder"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL updater for the ident field of hydra.coq.syntax.ExistentialVariable
existentialVariableWithIdent :: Phantoms.TypedTerm Syntax.ExistentialVariable -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.ExistentialVariable
existentialVariableWithIdent original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
              Core.projectionFieldName = (Core.Name "variant")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the variant field of hydra.coq.syntax.ExistentialVariable
existentialVariableWithVariant :: Phantoms.TypedTerm Syntax.ExistentialVariable -> Phantoms.TypedTerm Syntax.ExistentialVariableVariant -> Phantoms.TypedTerm Syntax.ExistentialVariable
existentialVariableWithVariant original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.coq.syntax.FieldIdent wrapper
fieldIdent :: Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.FieldIdent
fieldIdent x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.FieldIdent"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL injection for the measure variant of hydra.coq.syntax.FixAnnot
fixAnnotMeasure :: Phantoms.TypedTerm Syntax.FixAnnot_Measure -> Phantoms.TypedTerm Syntax.FixAnnot
fixAnnotMeasure x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "measure"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the struct variant of hydra.coq.syntax.FixAnnot
fixAnnotStruct :: Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.FixAnnot
fixAnnotStruct x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "struct"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the wf variant of hydra.coq.syntax.FixAnnot
fixAnnotWf :: Phantoms.TypedTerm Syntax.FixAnnot_Wf -> Phantoms.TypedTerm Syntax.FixAnnot
fixAnnotWf x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wf"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.FixAnnot_Measure
fixAnnot_Measure :: Phantoms.TypedTerm Syntax.OneTerm -> Phantoms.TypedTerm (Maybe Syntax.Ident) -> Phantoms.TypedTerm (Maybe Syntax.OneTerm) -> Phantoms.TypedTerm Syntax.FixAnnot_Measure
fixAnnot_Measure term ident term2 =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTypedTerm ident)},
        Core.Field {
          Core.fieldName = (Core.Name "term2"),
          Core.fieldTerm = (Phantoms.unTypedTerm term2)}]}))
-- | DSL accessor for the ident field of hydra.coq.syntax.FixAnnot_Measure
fixAnnot_MeasureIdent :: Phantoms.TypedTerm Syntax.FixAnnot_Measure -> Phantoms.TypedTerm (Maybe Syntax.Ident)
fixAnnot_MeasureIdent x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
        Core.projectionFieldName = (Core.Name "ident")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.FixAnnot_Measure
fixAnnot_MeasureTerm :: Phantoms.TypedTerm Syntax.FixAnnot_Measure -> Phantoms.TypedTerm Syntax.OneTerm
fixAnnot_MeasureTerm x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the term2 field of hydra.coq.syntax.FixAnnot_Measure
fixAnnot_MeasureTerm2 :: Phantoms.TypedTerm Syntax.FixAnnot_Measure -> Phantoms.TypedTerm (Maybe Syntax.OneTerm)
fixAnnot_MeasureTerm2 x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
        Core.projectionFieldName = (Core.Name "term2")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the ident field of hydra.coq.syntax.FixAnnot_Measure
fixAnnot_MeasureWithIdent :: Phantoms.TypedTerm Syntax.FixAnnot_Measure -> Phantoms.TypedTerm (Maybe Syntax.Ident) -> Phantoms.TypedTerm Syntax.FixAnnot_Measure
fixAnnot_MeasureWithIdent original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
              Core.projectionFieldName = (Core.Name "term2")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.coq.syntax.FixAnnot_Measure
fixAnnot_MeasureWithTerm :: Phantoms.TypedTerm Syntax.FixAnnot_Measure -> Phantoms.TypedTerm Syntax.OneTerm -> Phantoms.TypedTerm Syntax.FixAnnot_Measure
fixAnnot_MeasureWithTerm original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
              Core.projectionFieldName = (Core.Name "term2")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the term2 field of hydra.coq.syntax.FixAnnot_Measure
fixAnnot_MeasureWithTerm2 :: Phantoms.TypedTerm Syntax.FixAnnot_Measure -> Phantoms.TypedTerm (Maybe Syntax.OneTerm) -> Phantoms.TypedTerm Syntax.FixAnnot_Measure
fixAnnot_MeasureWithTerm2 original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term2"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.FixAnnot_Wf
fixAnnot_Wf :: Phantoms.TypedTerm Syntax.OneTerm -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.FixAnnot_Wf
fixAnnot_Wf term ident =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTypedTerm ident)}]}))
-- | DSL accessor for the ident field of hydra.coq.syntax.FixAnnot_Wf
fixAnnot_WfIdent :: Phantoms.TypedTerm Syntax.FixAnnot_Wf -> Phantoms.TypedTerm Syntax.Ident
fixAnnot_WfIdent x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
        Core.projectionFieldName = (Core.Name "ident")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.FixAnnot_Wf
fixAnnot_WfTerm :: Phantoms.TypedTerm Syntax.FixAnnot_Wf -> Phantoms.TypedTerm Syntax.OneTerm
fixAnnot_WfTerm x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the ident field of hydra.coq.syntax.FixAnnot_Wf
fixAnnot_WfWithIdent :: Phantoms.TypedTerm Syntax.FixAnnot_Wf -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.FixAnnot_Wf
fixAnnot_WfWithIdent original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the term field of hydra.coq.syntax.FixAnnot_Wf
fixAnnot_WfWithTerm :: Phantoms.TypedTerm Syntax.FixAnnot_Wf -> Phantoms.TypedTerm Syntax.OneTerm -> Phantoms.TypedTerm Syntax.FixAnnot_Wf
fixAnnot_WfWithTerm original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL injection for the decl variant of hydra.coq.syntax.Fix
fixDecl :: Phantoms.TypedTerm Syntax.Fix_Decl -> Phantoms.TypedTerm Syntax.Fix
fixDecl x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Fix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decl"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the qual variant of hydra.coq.syntax.Fix
fixQual :: Phantoms.TypedTerm (Maybe Syntax.Fix_Qual) -> Phantoms.TypedTerm Syntax.Fix
fixQual x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Fix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qual"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.FixWith
fixWith :: Phantoms.TypedTerm [Syntax.Fix_Decl] -> Phantoms.TypedTerm (Maybe Syntax.Ident) -> Phantoms.TypedTerm Syntax.FixWith
fixWith decls for =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decls"),
          Core.fieldTerm = (Phantoms.unTypedTerm decls)},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Phantoms.unTypedTerm for)}]}))
-- | DSL accessor for the decls field of hydra.coq.syntax.FixWith
fixWithDecls :: Phantoms.TypedTerm Syntax.FixWith -> Phantoms.TypedTerm [Syntax.Fix_Decl]
fixWithDecls x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
        Core.projectionFieldName = (Core.Name "decls")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the for field of hydra.coq.syntax.FixWith
fixWithFor :: Phantoms.TypedTerm Syntax.FixWith -> Phantoms.TypedTerm (Maybe Syntax.Ident)
fixWithFor x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
        Core.projectionFieldName = (Core.Name "for")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the decls field of hydra.coq.syntax.FixWith
fixWithWithDecls :: Phantoms.TypedTerm Syntax.FixWith -> Phantoms.TypedTerm [Syntax.Fix_Decl] -> Phantoms.TypedTerm Syntax.FixWith
fixWithWithDecls original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decls"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
              Core.projectionFieldName = (Core.Name "for")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the for field of hydra.coq.syntax.FixWith
fixWithWithFor :: Phantoms.TypedTerm Syntax.FixWith -> Phantoms.TypedTerm (Maybe Syntax.Ident) -> Phantoms.TypedTerm Syntax.FixWith
fixWithWithFor original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decls"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
              Core.projectionFieldName = (Core.Name "decls")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.Fix_Decl
fix_Decl :: Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm [Syntax.Binder] -> Phantoms.TypedTerm (Maybe Syntax.FixAnnot) -> Phantoms.TypedTerm (Maybe Syntax.Type) -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.Fix_Decl
fix_Decl ident binders annot type_ term =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTypedTerm ident)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Phantoms.unTypedTerm annot)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm term)}]}))
-- | DSL accessor for the annot field of hydra.coq.syntax.Fix_Decl
fix_DeclAnnot :: Phantoms.TypedTerm Syntax.Fix_Decl -> Phantoms.TypedTerm (Maybe Syntax.FixAnnot)
fix_DeclAnnot x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
        Core.projectionFieldName = (Core.Name "annot")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the binders field of hydra.coq.syntax.Fix_Decl
fix_DeclBinders :: Phantoms.TypedTerm Syntax.Fix_Decl -> Phantoms.TypedTerm [Syntax.Binder]
fix_DeclBinders x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the ident field of hydra.coq.syntax.Fix_Decl
fix_DeclIdent :: Phantoms.TypedTerm Syntax.Fix_Decl -> Phantoms.TypedTerm Syntax.Ident
fix_DeclIdent x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
        Core.projectionFieldName = (Core.Name "ident")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.Fix_Decl
fix_DeclTerm :: Phantoms.TypedTerm Syntax.Fix_Decl -> Phantoms.TypedTerm Syntax.Term
fix_DeclTerm x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.Fix_Decl
fix_DeclType :: Phantoms.TypedTerm Syntax.Fix_Decl -> Phantoms.TypedTerm (Maybe Syntax.Type)
fix_DeclType x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the annot field of hydra.coq.syntax.Fix_Decl
fix_DeclWithAnnot :: Phantoms.TypedTerm Syntax.Fix_Decl -> Phantoms.TypedTerm (Maybe Syntax.FixAnnot) -> Phantoms.TypedTerm Syntax.Fix_Decl
fix_DeclWithAnnot original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the binders field of hydra.coq.syntax.Fix_Decl
fix_DeclWithBinders :: Phantoms.TypedTerm Syntax.Fix_Decl -> Phantoms.TypedTerm [Syntax.Binder] -> Phantoms.TypedTerm Syntax.Fix_Decl
fix_DeclWithBinders original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the ident field of hydra.coq.syntax.Fix_Decl
fix_DeclWithIdent :: Phantoms.TypedTerm Syntax.Fix_Decl -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.Fix_Decl
fix_DeclWithIdent original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.coq.syntax.Fix_Decl
fix_DeclWithTerm :: Phantoms.TypedTerm Syntax.Fix_Decl -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.Fix_Decl
fix_DeclWithTerm original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the type field of hydra.coq.syntax.Fix_Decl
fix_DeclWithType :: Phantoms.TypedTerm Syntax.Fix_Decl -> Phantoms.TypedTerm (Maybe Syntax.Type) -> Phantoms.TypedTerm Syntax.Fix_Decl
fix_DeclWithType original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL injection for the in variant of hydra.coq.syntax.Fix_Qual
fix_QualIn :: Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.Fix_Qual
fix_QualIn x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Qual"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the with variant of hydra.coq.syntax.Fix_Qual
fix_QualWith :: Phantoms.TypedTerm Syntax.FixWith -> Phantoms.TypedTerm Syntax.Fix_Qual
fix_QualWith x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Qual"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "with"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.FixpointDefinition
fixpointDefinition :: Phantoms.TypedTerm (Maybe Syntax.Locality) -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm [Syntax.Binder] -> Phantoms.TypedTerm (Maybe Syntax.FixAnnot) -> Phantoms.TypedTerm (Maybe Syntax.Type) -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm [Syntax.Fix_Decl] -> Phantoms.TypedTerm Syntax.FixpointDefinition
fixpointDefinition locality name binders annot type_ body with =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Phantoms.unTypedTerm locality)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Phantoms.unTypedTerm annot)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTypedTerm with)}]}))
-- | DSL accessor for the annot field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionAnnot :: Phantoms.TypedTerm Syntax.FixpointDefinition -> Phantoms.TypedTerm (Maybe Syntax.FixAnnot)
fixpointDefinitionAnnot x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionFieldName = (Core.Name "annot")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the binders field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionBinders :: Phantoms.TypedTerm Syntax.FixpointDefinition -> Phantoms.TypedTerm [Syntax.Binder]
fixpointDefinitionBinders x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionBody :: Phantoms.TypedTerm Syntax.FixpointDefinition -> Phantoms.TypedTerm Syntax.Term
fixpointDefinitionBody x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the locality field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionLocality :: Phantoms.TypedTerm Syntax.FixpointDefinition -> Phantoms.TypedTerm (Maybe Syntax.Locality)
fixpointDefinitionLocality x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionFieldName = (Core.Name "locality")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionName :: Phantoms.TypedTerm Syntax.FixpointDefinition -> Phantoms.TypedTerm Syntax.Ident
fixpointDefinitionName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionType :: Phantoms.TypedTerm Syntax.FixpointDefinition -> Phantoms.TypedTerm (Maybe Syntax.Type)
fixpointDefinitionType x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the with field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionWith :: Phantoms.TypedTerm Syntax.FixpointDefinition -> Phantoms.TypedTerm [Syntax.Fix_Decl]
fixpointDefinitionWith x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionFieldName = (Core.Name "with")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the annot field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionWithAnnot :: Phantoms.TypedTerm Syntax.FixpointDefinition -> Phantoms.TypedTerm (Maybe Syntax.FixAnnot) -> Phantoms.TypedTerm Syntax.FixpointDefinition
fixpointDefinitionWithAnnot original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the binders field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionWithBinders :: Phantoms.TypedTerm Syntax.FixpointDefinition -> Phantoms.TypedTerm [Syntax.Binder] -> Phantoms.TypedTerm Syntax.FixpointDefinition
fixpointDefinitionWithBinders original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionWithBody :: Phantoms.TypedTerm Syntax.FixpointDefinition -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.FixpointDefinition
fixpointDefinitionWithBody original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the locality field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionWithLocality :: Phantoms.TypedTerm Syntax.FixpointDefinition -> Phantoms.TypedTerm (Maybe Syntax.Locality) -> Phantoms.TypedTerm Syntax.FixpointDefinition
fixpointDefinitionWithLocality original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionWithName :: Phantoms.TypedTerm Syntax.FixpointDefinition -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.FixpointDefinition
fixpointDefinitionWithName original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionWithType :: Phantoms.TypedTerm Syntax.FixpointDefinition -> Phantoms.TypedTerm (Maybe Syntax.Type) -> Phantoms.TypedTerm Syntax.FixpointDefinition
fixpointDefinitionWithType original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the with field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionWithWith :: Phantoms.TypedTerm Syntax.FixpointDefinition -> Phantoms.TypedTerm [Syntax.Fix_Decl] -> Phantoms.TypedTerm Syntax.FixpointDefinition
fixpointDefinitionWithWith original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.Forall
forall_ :: Phantoms.TypedTerm Syntax.OpenBinders -> Phantoms.TypedTerm Syntax.Type -> Phantoms.TypedTerm Syntax.Forall
forall_ binders type_ =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Forall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm type_)}]}))
-- | DSL accessor for the binders field of hydra.coq.syntax.Forall
forallBinders :: Phantoms.TypedTerm Syntax.Forall -> Phantoms.TypedTerm Syntax.OpenBinders
forallBinders x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Forall"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL injection for the forall variant of hydra.coq.syntax.ForallOrFun
forallOrFunForall :: Phantoms.TypedTerm Syntax.Forall -> Phantoms.TypedTerm Syntax.ForallOrFun
forallOrFunForall x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ForallOrFun"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forall"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the fun variant of hydra.coq.syntax.ForallOrFun
forallOrFunFun :: Phantoms.TypedTerm Syntax.Fun -> Phantoms.TypedTerm Syntax.ForallOrFun
forallOrFunFun x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ForallOrFun"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fun"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL accessor for the type field of hydra.coq.syntax.Forall
forallType :: Phantoms.TypedTerm Syntax.Forall -> Phantoms.TypedTerm Syntax.Type
forallType x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Forall"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the binders field of hydra.coq.syntax.Forall
forallWithBinders :: Phantoms.TypedTerm Syntax.Forall -> Phantoms.TypedTerm Syntax.OpenBinders -> Phantoms.TypedTerm Syntax.Forall
forallWithBinders original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Forall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Forall"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.coq.syntax.Forall
forallWithType :: Phantoms.TypedTerm Syntax.Forall -> Phantoms.TypedTerm Syntax.Type -> Phantoms.TypedTerm Syntax.Forall
forallWithType original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Forall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Forall"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.Fun
fun :: Phantoms.TypedTerm Syntax.OpenBinders -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.Fun
fun binders body =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fun"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTypedTerm body)}]}))
-- | DSL accessor for the binders field of hydra.coq.syntax.Fun
funBinders :: Phantoms.TypedTerm Syntax.Fun -> Phantoms.TypedTerm Syntax.OpenBinders
funBinders x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fun"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.coq.syntax.Fun
funBody :: Phantoms.TypedTerm Syntax.Fun -> Phantoms.TypedTerm Syntax.Term
funBody x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fun"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the binders field of hydra.coq.syntax.Fun
funWithBinders :: Phantoms.TypedTerm Syntax.Fun -> Phantoms.TypedTerm Syntax.OpenBinders -> Phantoms.TypedTerm Syntax.Fun
funWithBinders original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fun"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fun"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.coq.syntax.Fun
funWithBody :: Phantoms.TypedTerm Syntax.Fun -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.Fun
funWithBody original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fun"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fun"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL injection for the explicit variant of hydra.coq.syntax.GeneralizingBinder
generalizingBinderExplicit :: Phantoms.TypedTerm Syntax.TypeclassConstraint -> Phantoms.TypedTerm Syntax.GeneralizingBinder
generalizingBinderExplicit x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.GeneralizingBinder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "explicit"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the implicitMaximallyInserted variant of hydra.coq.syntax.GeneralizingBinder
generalizingBinderImplicitMaximallyInserted :: Phantoms.TypedTerm Syntax.TypeclassConstraint -> Phantoms.TypedTerm Syntax.GeneralizingBinder
generalizingBinderImplicitMaximallyInserted x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.GeneralizingBinder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicitMaximallyInserted"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the implicitNonMaximallyInserted variant of hydra.coq.syntax.GeneralizingBinder
generalizingBinderImplicitNonMaximallyInserted :: Phantoms.TypedTerm Syntax.TypeclassConstraint -> Phantoms.TypedTerm Syntax.GeneralizingBinder
generalizingBinderImplicitNonMaximallyInserted x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.GeneralizingBinder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicitNonMaximallyInserted"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for the hydra.coq.syntax.Ident wrapper
ident :: Phantoms.TypedTerm Syntax.String_ -> Phantoms.TypedTerm Syntax.Ident
ident x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.Ident"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for hydra.coq.syntax.IdentArg
identArg :: Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.IdentArg
identArg ident term =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTypedTerm ident)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm term)}]}))
-- | DSL accessor for the ident field of hydra.coq.syntax.IdentArg
identArgIdent :: Phantoms.TypedTerm Syntax.IdentArg -> Phantoms.TypedTerm Syntax.Ident
identArgIdent x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
        Core.projectionFieldName = (Core.Name "ident")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.IdentArg
identArgTerm :: Phantoms.TypedTerm Syntax.IdentArg -> Phantoms.TypedTerm Syntax.Term
identArgTerm x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the ident field of hydra.coq.syntax.IdentArg
identArgWithIdent :: Phantoms.TypedTerm Syntax.IdentArg -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.IdentArg
identArgWithIdent original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.coq.syntax.IdentArg
identArgWithTerm :: Phantoms.TypedTerm Syntax.IdentArg -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.IdentArg
identArgWithTerm original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.If
if_ :: Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm (Maybe Syntax.ReturnAs) -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.If
if_ condition returnAs then_ else_ =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTypedTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Phantoms.unTypedTerm returnAs)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTypedTerm then_)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTypedTerm else_)}]}))
-- | DSL accessor for the condition field of hydra.coq.syntax.If
ifCondition :: Phantoms.TypedTerm Syntax.If -> Phantoms.TypedTerm Syntax.Term
ifCondition x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
        Core.projectionFieldName = (Core.Name "condition")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the else field of hydra.coq.syntax.If
ifElse :: Phantoms.TypedTerm Syntax.If -> Phantoms.TypedTerm Syntax.Term
ifElse x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the returnAs field of hydra.coq.syntax.If
ifReturnAs :: Phantoms.TypedTerm Syntax.If -> Phantoms.TypedTerm (Maybe Syntax.ReturnAs)
ifReturnAs x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
        Core.projectionFieldName = (Core.Name "returnAs")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the then field of hydra.coq.syntax.If
ifThen :: Phantoms.TypedTerm Syntax.If -> Phantoms.TypedTerm Syntax.Term
ifThen x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
        Core.projectionFieldName = (Core.Name "then")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the condition field of hydra.coq.syntax.If
ifWithCondition :: Phantoms.TypedTerm Syntax.If -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.If
ifWithCondition original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "returnAs")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the else field of hydra.coq.syntax.If
ifWithElse :: Phantoms.TypedTerm Syntax.If -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.If
ifWithElse original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "returnAs")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the returnAs field of hydra.coq.syntax.If
ifWithReturnAs :: Phantoms.TypedTerm Syntax.If -> Phantoms.TypedTerm (Maybe Syntax.ReturnAs) -> Phantoms.TypedTerm Syntax.If
ifWithReturnAs original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the then field of hydra.coq.syntax.If
ifWithThen :: Phantoms.TypedTerm Syntax.If -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.If
ifWithThen original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "returnAs")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL injection for the maximallyInserted variant of hydra.coq.syntax.ImplicitBinders
implicitBindersMaximallyInserted :: Phantoms.TypedTerm Syntax.TypeBinders -> Phantoms.TypedTerm Syntax.ImplicitBinders
implicitBindersMaximallyInserted x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ImplicitBinders"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maximallyInserted"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the nonMaximallyInserted variant of hydra.coq.syntax.ImplicitBinders
implicitBindersNonMaximallyInserted :: Phantoms.TypedTerm Syntax.TypeBinders -> Phantoms.TypedTerm Syntax.ImplicitBinders
implicitBindersNonMaximallyInserted x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ImplicitBinders"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonMaximallyInserted"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the export variant of hydra.coq.syntax.ImportQualification
importQualificationExport :: Phantoms.TypedTerm Syntax.ImportQualification
importQualificationExport =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ImportQualification"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "export"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the import variant of hydra.coq.syntax.ImportQualification
importQualificationImport :: Phantoms.TypedTerm Syntax.ImportQualification
importQualificationImport =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ImportQualification"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "import"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.coq.syntax.InductiveBody
inductiveBody :: Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm [Syntax.Binder] -> Phantoms.TypedTerm (Maybe Syntax.Type) -> Phantoms.TypedTerm [Syntax.Constructor] -> Phantoms.TypedTerm Syntax.InductiveBody
inductiveBody name binders type_ constructors =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Phantoms.unTypedTerm constructors)}]}))
-- | DSL accessor for the binders field of hydra.coq.syntax.InductiveBody
inductiveBodyBinders :: Phantoms.TypedTerm Syntax.InductiveBody -> Phantoms.TypedTerm [Syntax.Binder]
inductiveBodyBinders x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the constructors field of hydra.coq.syntax.InductiveBody
inductiveBodyConstructors :: Phantoms.TypedTerm Syntax.InductiveBody -> Phantoms.TypedTerm [Syntax.Constructor]
inductiveBodyConstructors x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
        Core.projectionFieldName = (Core.Name "constructors")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.coq.syntax.InductiveBody
inductiveBodyName :: Phantoms.TypedTerm Syntax.InductiveBody -> Phantoms.TypedTerm Syntax.Ident
inductiveBodyName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.InductiveBody
inductiveBodyType :: Phantoms.TypedTerm Syntax.InductiveBody -> Phantoms.TypedTerm (Maybe Syntax.Type)
inductiveBodyType x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the binders field of hydra.coq.syntax.InductiveBody
inductiveBodyWithBinders :: Phantoms.TypedTerm Syntax.InductiveBody -> Phantoms.TypedTerm [Syntax.Binder] -> Phantoms.TypedTerm Syntax.InductiveBody
inductiveBodyWithBinders original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "constructors")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the constructors field of hydra.coq.syntax.InductiveBody
inductiveBodyWithConstructors :: Phantoms.TypedTerm Syntax.InductiveBody -> Phantoms.TypedTerm [Syntax.Constructor] -> Phantoms.TypedTerm Syntax.InductiveBody
inductiveBodyWithConstructors original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.coq.syntax.InductiveBody
inductiveBodyWithName :: Phantoms.TypedTerm Syntax.InductiveBody -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.InductiveBody
inductiveBodyWithName original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "constructors")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.coq.syntax.InductiveBody
inductiveBodyWithType :: Phantoms.TypedTerm Syntax.InductiveBody -> Phantoms.TypedTerm (Maybe Syntax.Type) -> Phantoms.TypedTerm Syntax.InductiveBody
inductiveBodyWithType original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "constructors")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.InductiveDefinition
inductiveDefinition :: Phantoms.TypedTerm (Maybe Syntax.Locality) -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm [Syntax.InductiveBody] -> Phantoms.TypedTerm Syntax.InductiveDefinition
inductiveDefinition locality coinductive bodies =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Phantoms.unTypedTerm locality)},
        Core.Field {
          Core.fieldName = (Core.Name "coinductive"),
          Core.fieldTerm = (Phantoms.unTypedTerm coinductive)},
        Core.Field {
          Core.fieldName = (Core.Name "bodies"),
          Core.fieldTerm = (Phantoms.unTypedTerm bodies)}]}))
-- | DSL accessor for the bodies field of hydra.coq.syntax.InductiveDefinition
inductiveDefinitionBodies :: Phantoms.TypedTerm Syntax.InductiveDefinition -> Phantoms.TypedTerm [Syntax.InductiveBody]
inductiveDefinitionBodies x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
        Core.projectionFieldName = (Core.Name "bodies")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the coinductive field of hydra.coq.syntax.InductiveDefinition
inductiveDefinitionCoinductive :: Phantoms.TypedTerm Syntax.InductiveDefinition -> Phantoms.TypedTerm Bool
inductiveDefinitionCoinductive x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
        Core.projectionFieldName = (Core.Name "coinductive")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the locality field of hydra.coq.syntax.InductiveDefinition
inductiveDefinitionLocality :: Phantoms.TypedTerm Syntax.InductiveDefinition -> Phantoms.TypedTerm (Maybe Syntax.Locality)
inductiveDefinitionLocality x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
        Core.projectionFieldName = (Core.Name "locality")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the bodies field of hydra.coq.syntax.InductiveDefinition
inductiveDefinitionWithBodies :: Phantoms.TypedTerm Syntax.InductiveDefinition -> Phantoms.TypedTerm [Syntax.InductiveBody] -> Phantoms.TypedTerm Syntax.InductiveDefinition
inductiveDefinitionWithBodies original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "coinductive"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
              Core.projectionFieldName = (Core.Name "coinductive")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bodies"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the coinductive field of hydra.coq.syntax.InductiveDefinition
inductiveDefinitionWithCoinductive :: Phantoms.TypedTerm Syntax.InductiveDefinition -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Syntax.InductiveDefinition
inductiveDefinitionWithCoinductive original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "coinductive"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bodies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
              Core.projectionFieldName = (Core.Name "bodies")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the locality field of hydra.coq.syntax.InductiveDefinition
inductiveDefinitionWithLocality :: Phantoms.TypedTerm Syntax.InductiveDefinition -> Phantoms.TypedTerm (Maybe Syntax.Locality) -> Phantoms.TypedTerm Syntax.InductiveDefinition
inductiveDefinitionWithLocality original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "coinductive"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
              Core.projectionFieldName = (Core.Name "coinductive")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bodies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
              Core.projectionFieldName = (Core.Name "bodies")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.Let
let_ :: Phantoms.TypedTerm Syntax.LetBindings -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.Let
let_ bindings in_ =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Let"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTypedTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTypedTerm in_)}]}))
-- | DSL constructor for hydra.coq.syntax.LetBinder
letBinder :: Phantoms.TypedTerm Syntax.Name -> Phantoms.TypedTerm (Maybe Syntax.Type) -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.LetBinder
letBinder name type_ term =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm term)}]}))
-- | DSL accessor for the name field of hydra.coq.syntax.LetBinder
letBinderName :: Phantoms.TypedTerm Syntax.LetBinder -> Phantoms.TypedTerm Syntax.Name
letBinderName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.LetBinder
letBinderTerm :: Phantoms.TypedTerm Syntax.LetBinder -> Phantoms.TypedTerm Syntax.Term
letBinderTerm x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.LetBinder
letBinderType :: Phantoms.TypedTerm Syntax.LetBinder -> Phantoms.TypedTerm (Maybe Syntax.Type)
letBinderType x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.coq.syntax.LetBinder
letBinderWithName :: Phantoms.TypedTerm Syntax.LetBinder -> Phantoms.TypedTerm Syntax.Name -> Phantoms.TypedTerm Syntax.LetBinder
letBinderWithName original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.coq.syntax.LetBinder
letBinderWithTerm :: Phantoms.TypedTerm Syntax.LetBinder -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.LetBinder
letBinderWithTerm original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the type field of hydra.coq.syntax.LetBinder
letBinderWithType :: Phantoms.TypedTerm Syntax.LetBinder -> Phantoms.TypedTerm (Maybe Syntax.Type) -> Phantoms.TypedTerm Syntax.LetBinder
letBinderWithType original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL accessor for the bindings field of hydra.coq.syntax.Let
letBindings :: Phantoms.TypedTerm Syntax.Let -> Phantoms.TypedTerm Syntax.LetBindings
letBindings x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Let"),
        Core.projectionFieldName = (Core.Name "bindings")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL injection for the destructuring variant of hydra.coq.syntax.LetBindings
letBindingsDestructuring :: Phantoms.TypedTerm Syntax.LetDestructuring -> Phantoms.TypedTerm Syntax.LetBindings
letBindingsDestructuring x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.LetBindings"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "destructuring"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the named variant of hydra.coq.syntax.LetBindings
letBindingsNamed :: Phantoms.TypedTerm Syntax.LetNamed -> Phantoms.TypedTerm Syntax.LetBindings
letBindingsNamed x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.LetBindings"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the variant1 variant of hydra.coq.syntax.LetDestructuring
letDestructuringVariant1 :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TypedTerm Syntax.LetDestructuring
letDestructuringVariant1 x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variant1"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the variant2 variant of hydra.coq.syntax.LetDestructuring
letDestructuringVariant2 :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TypedTerm Syntax.LetDestructuring
letDestructuringVariant2 x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variant2"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the variant3 variant of hydra.coq.syntax.LetDestructuring
letDestructuringVariant3 :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TypedTerm Syntax.LetDestructuring
letDestructuringVariant3 x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variant3"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.LetDestructuring_Variant1
letDestructuring_Variant1 :: Phantoms.TypedTerm [Syntax.Name] -> Phantoms.TypedTerm (Maybe Syntax.ReturnAs) -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.LetDestructuring_Variant1
letDestructuring_Variant1 names returnAs term =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTypedTerm names)},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Phantoms.unTypedTerm returnAs)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm term)}]}))
-- | DSL accessor for the names field of hydra.coq.syntax.LetDestructuring_Variant1
letDestructuring_Variant1Names :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TypedTerm [Syntax.Name]
letDestructuring_Variant1Names x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
        Core.projectionFieldName = (Core.Name "names")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the returnAs field of hydra.coq.syntax.LetDestructuring_Variant1
letDestructuring_Variant1ReturnAs :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TypedTerm (Maybe Syntax.ReturnAs)
letDestructuring_Variant1ReturnAs x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
        Core.projectionFieldName = (Core.Name "returnAs")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.LetDestructuring_Variant1
letDestructuring_Variant1Term :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TypedTerm Syntax.Term
letDestructuring_Variant1Term x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the names field of hydra.coq.syntax.LetDestructuring_Variant1
letDestructuring_Variant1WithNames :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TypedTerm [Syntax.Name] -> Phantoms.TypedTerm Syntax.LetDestructuring_Variant1
letDestructuring_Variant1WithNames original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionFieldName = (Core.Name "returnAs")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the returnAs field of hydra.coq.syntax.LetDestructuring_Variant1
letDestructuring_Variant1WithReturnAs :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TypedTerm (Maybe Syntax.ReturnAs) -> Phantoms.TypedTerm Syntax.LetDestructuring_Variant1
letDestructuring_Variant1WithReturnAs original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.coq.syntax.LetDestructuring_Variant1
letDestructuring_Variant1WithTerm :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant1 -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.LetDestructuring_Variant1
letDestructuring_Variant1WithTerm original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionFieldName = (Core.Name "returnAs")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.LetDestructuring_Variant2
letDestructuring_Variant2 :: Phantoms.TypedTerm Syntax.Pattern -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm (Maybe Syntax.Term100) -> Phantoms.TypedTerm Syntax.LetDestructuring_Variant2
letDestructuring_Variant2 pattern term return =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTypedTerm return)}]}))
-- | DSL accessor for the pattern field of hydra.coq.syntax.LetDestructuring_Variant2
letDestructuring_Variant2Pattern :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TypedTerm Syntax.Pattern
letDestructuring_Variant2Pattern x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the return field of hydra.coq.syntax.LetDestructuring_Variant2
letDestructuring_Variant2Return :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TypedTerm (Maybe Syntax.Term100)
letDestructuring_Variant2Return x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
        Core.projectionFieldName = (Core.Name "return")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.LetDestructuring_Variant2
letDestructuring_Variant2Term :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TypedTerm Syntax.Term
letDestructuring_Variant2Term x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the pattern field of hydra.coq.syntax.LetDestructuring_Variant2
letDestructuring_Variant2WithPattern :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TypedTerm Syntax.Pattern -> Phantoms.TypedTerm Syntax.LetDestructuring_Variant2
letDestructuring_Variant2WithPattern original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionFieldName = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the return field of hydra.coq.syntax.LetDestructuring_Variant2
letDestructuring_Variant2WithReturn :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TypedTerm (Maybe Syntax.Term100) -> Phantoms.TypedTerm Syntax.LetDestructuring_Variant2
letDestructuring_Variant2WithReturn original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the term field of hydra.coq.syntax.LetDestructuring_Variant2
letDestructuring_Variant2WithTerm :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant2 -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.LetDestructuring_Variant2
letDestructuring_Variant2WithTerm original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionFieldName = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.LetDestructuring_Variant3
letDestructuring_Variant3 :: Phantoms.TypedTerm Syntax.Pattern -> Phantoms.TypedTerm Syntax.Pattern -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.Term100 -> Phantoms.TypedTerm Syntax.LetDestructuring_Variant3
letDestructuring_Variant3 pattern1 pattern2 term return =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern1"),
          Core.fieldTerm = (Phantoms.unTypedTerm pattern1)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern2"),
          Core.fieldTerm = (Phantoms.unTypedTerm pattern2)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTypedTerm return)}]}))
-- | DSL accessor for the pattern1 field of hydra.coq.syntax.LetDestructuring_Variant3
letDestructuring_Variant3Pattern1 :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TypedTerm Syntax.Pattern
letDestructuring_Variant3Pattern1 x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
        Core.projectionFieldName = (Core.Name "pattern1")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the pattern2 field of hydra.coq.syntax.LetDestructuring_Variant3
letDestructuring_Variant3Pattern2 :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TypedTerm Syntax.Pattern
letDestructuring_Variant3Pattern2 x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
        Core.projectionFieldName = (Core.Name "pattern2")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the return field of hydra.coq.syntax.LetDestructuring_Variant3
letDestructuring_Variant3Return :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TypedTerm Syntax.Term100
letDestructuring_Variant3Return x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
        Core.projectionFieldName = (Core.Name "return")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.LetDestructuring_Variant3
letDestructuring_Variant3Term :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TypedTerm Syntax.Term
letDestructuring_Variant3Term x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the pattern1 field of hydra.coq.syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithPattern1 :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TypedTerm Syntax.Pattern -> Phantoms.TypedTerm Syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithPattern1 original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern1"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "pattern2")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the pattern2 field of hydra.coq.syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithPattern2 :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TypedTerm Syntax.Pattern -> Phantoms.TypedTerm Syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithPattern2 original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "pattern1")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern2"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the return field of hydra.coq.syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithReturn :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TypedTerm Syntax.Term100 -> Phantoms.TypedTerm Syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithReturn original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "pattern1")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "pattern2")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the term field of hydra.coq.syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithTerm :: Phantoms.TypedTerm Syntax.LetDestructuring_Variant3 -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithTerm original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "pattern1")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "pattern2")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL accessor for the in field of hydra.coq.syntax.Let
letIn :: Phantoms.TypedTerm Syntax.Let -> Phantoms.TypedTerm Syntax.Term
letIn x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Let"),
        Core.projectionFieldName = (Core.Name "in")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for hydra.coq.syntax.LetNamed
letNamed :: Phantoms.TypedTerm Syntax.LetBinder -> Phantoms.TypedTerm [Syntax.Binder] -> Phantoms.TypedTerm Syntax.LetNamed
letNamed binder binders =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binder"),
          Core.fieldTerm = (Phantoms.unTypedTerm binder)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm binders)}]}))
-- | DSL accessor for the binder field of hydra.coq.syntax.LetNamed
letNamedBinder :: Phantoms.TypedTerm Syntax.LetNamed -> Phantoms.TypedTerm Syntax.LetBinder
letNamedBinder x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
        Core.projectionFieldName = (Core.Name "binder")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the binders field of hydra.coq.syntax.LetNamed
letNamedBinders :: Phantoms.TypedTerm Syntax.LetNamed -> Phantoms.TypedTerm [Syntax.Binder]
letNamedBinders x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the binder field of hydra.coq.syntax.LetNamed
letNamedWithBinder :: Phantoms.TypedTerm Syntax.LetNamed -> Phantoms.TypedTerm Syntax.LetBinder -> Phantoms.TypedTerm Syntax.LetNamed
letNamedWithBinder original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binder"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the binders field of hydra.coq.syntax.LetNamed
letNamedWithBinders :: Phantoms.TypedTerm Syntax.LetNamed -> Phantoms.TypedTerm [Syntax.Binder] -> Phantoms.TypedTerm Syntax.LetNamed
letNamedWithBinders original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
              Core.projectionFieldName = (Core.Name "binder")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the bindings field of hydra.coq.syntax.Let
letWithBindings :: Phantoms.TypedTerm Syntax.Let -> Phantoms.TypedTerm Syntax.LetBindings -> Phantoms.TypedTerm Syntax.Let
letWithBindings original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Let"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Let"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the in field of hydra.coq.syntax.Let
letWithIn :: Phantoms.TypedTerm Syntax.Let -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.Let
letWithIn original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Let"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Let"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL injection for the global variant of hydra.coq.syntax.Locality
localityGlobal :: Phantoms.TypedTerm Syntax.Locality
localityGlobal =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Locality"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "global"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the local variant of hydra.coq.syntax.Locality
localityLocal :: Phantoms.TypedTerm Syntax.Locality
localityLocal =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Locality"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "local"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.coq.syntax.Match
match :: Phantoms.TypedTerm [Syntax.CaseItem] -> Phantoms.TypedTerm (Maybe Syntax.Term100) -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm [Syntax.Equation] -> Phantoms.TypedTerm Syntax.Match
match caseItems return pipe equations =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "caseItems"),
          Core.fieldTerm = (Phantoms.unTypedTerm caseItems)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTypedTerm return)},
        Core.Field {
          Core.fieldName = (Core.Name "pipe"),
          Core.fieldTerm = (Phantoms.unTypedTerm pipe)},
        Core.Field {
          Core.fieldName = (Core.Name "equations"),
          Core.fieldTerm = (Phantoms.unTypedTerm equations)}]}))
-- | DSL accessor for the caseItems field of hydra.coq.syntax.Match
matchCaseItems :: Phantoms.TypedTerm Syntax.Match -> Phantoms.TypedTerm [Syntax.CaseItem]
matchCaseItems x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
        Core.projectionFieldName = (Core.Name "caseItems")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the equations field of hydra.coq.syntax.Match
matchEquations :: Phantoms.TypedTerm Syntax.Match -> Phantoms.TypedTerm [Syntax.Equation]
matchEquations x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
        Core.projectionFieldName = (Core.Name "equations")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the pipe field of hydra.coq.syntax.Match
matchPipe :: Phantoms.TypedTerm Syntax.Match -> Phantoms.TypedTerm Bool
matchPipe x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
        Core.projectionFieldName = (Core.Name "pipe")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the return field of hydra.coq.syntax.Match
matchReturn :: Phantoms.TypedTerm Syntax.Match -> Phantoms.TypedTerm (Maybe Syntax.Term100)
matchReturn x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
        Core.projectionFieldName = (Core.Name "return")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the caseItems field of hydra.coq.syntax.Match
matchWithCaseItems :: Phantoms.TypedTerm Syntax.Match -> Phantoms.TypedTerm [Syntax.CaseItem] -> Phantoms.TypedTerm Syntax.Match
matchWithCaseItems original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "caseItems"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pipe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "pipe")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "equations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "equations")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the equations field of hydra.coq.syntax.Match
matchWithEquations :: Phantoms.TypedTerm Syntax.Match -> Phantoms.TypedTerm [Syntax.Equation] -> Phantoms.TypedTerm Syntax.Match
matchWithEquations original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "caseItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "caseItems")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pipe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "pipe")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "equations"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the pipe field of hydra.coq.syntax.Match
matchWithPipe :: Phantoms.TypedTerm Syntax.Match -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Syntax.Match
matchWithPipe original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "caseItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "caseItems")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pipe"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "equations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "equations")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the return field of hydra.coq.syntax.Match
matchWithReturn :: Phantoms.TypedTerm Syntax.Match -> Phantoms.TypedTerm (Maybe Syntax.Term100) -> Phantoms.TypedTerm Syntax.Match
matchWithReturn original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "caseItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "caseItems")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pipe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "pipe")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "equations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "equations")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.ModuleDefinition
moduleDefinition :: Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm [Syntax.Sentence] -> Phantoms.TypedTerm Syntax.ModuleDefinition
moduleDefinition name sentences =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Phantoms.unTypedTerm sentences)}]}))
-- | DSL accessor for the name field of hydra.coq.syntax.ModuleDefinition
moduleDefinitionName :: Phantoms.TypedTerm Syntax.ModuleDefinition -> Phantoms.TypedTerm Syntax.Ident
moduleDefinitionName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the sentences field of hydra.coq.syntax.ModuleDefinition
moduleDefinitionSentences :: Phantoms.TypedTerm Syntax.ModuleDefinition -> Phantoms.TypedTerm [Syntax.Sentence]
moduleDefinitionSentences x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
        Core.projectionFieldName = (Core.Name "sentences")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.coq.syntax.ModuleDefinition
moduleDefinitionWithName :: Phantoms.TypedTerm Syntax.ModuleDefinition -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.ModuleDefinition
moduleDefinitionWithName original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
              Core.projectionFieldName = (Core.Name "sentences")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the sentences field of hydra.coq.syntax.ModuleDefinition
moduleDefinitionWithSentences :: Phantoms.TypedTerm Syntax.ModuleDefinition -> Phantoms.TypedTerm [Syntax.Sentence] -> Phantoms.TypedTerm Syntax.ModuleDefinition
moduleDefinitionWithSentences original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.coq.syntax.Name wrapper
name :: Phantoms.TypedTerm (Maybe Syntax.Ident) -> Phantoms.TypedTerm Syntax.Name
name x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.Name"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for the hydra.coq.syntax.Natural wrapper
natural :: Phantoms.TypedTerm Integer -> Phantoms.TypedTerm Syntax.Natural
natural x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.Natural"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for hydra.coq.syntax.NaturalArg
naturalArg :: Phantoms.TypedTerm Syntax.Natural -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.NaturalArg
naturalArg natural term =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "natural"),
          Core.fieldTerm = (Phantoms.unTypedTerm natural)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm term)}]}))
-- | DSL accessor for the natural field of hydra.coq.syntax.NaturalArg
naturalArgNatural :: Phantoms.TypedTerm Syntax.NaturalArg -> Phantoms.TypedTerm Syntax.Natural
naturalArgNatural x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
        Core.projectionFieldName = (Core.Name "natural")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.NaturalArg
naturalArgTerm :: Phantoms.TypedTerm Syntax.NaturalArg -> Phantoms.TypedTerm Syntax.Term
naturalArgTerm x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the natural field of hydra.coq.syntax.NaturalArg
naturalArgWithNatural :: Phantoms.TypedTerm Syntax.NaturalArg -> Phantoms.TypedTerm Syntax.Natural -> Phantoms.TypedTerm Syntax.NaturalArg
naturalArgWithNatural original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "natural"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.coq.syntax.NaturalArg
naturalArgWithTerm :: Phantoms.TypedTerm Syntax.NaturalArg -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.NaturalArg
naturalArgWithTerm original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "natural"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
              Core.projectionFieldName = (Core.Name "natural")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.NormalApplication
normalApplication :: Phantoms.TypedTerm Syntax.Term1 -> Phantoms.TypedTerm [Syntax.Arg] -> Phantoms.TypedTerm Syntax.NormalApplication
normalApplication lhs rhs =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.coq.syntax.NormalApplication
normalApplicationLhs :: Phantoms.TypedTerm Syntax.NormalApplication -> Phantoms.TypedTerm Syntax.Term1
normalApplicationLhs x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.coq.syntax.NormalApplication
normalApplicationRhs :: Phantoms.TypedTerm Syntax.NormalApplication -> Phantoms.TypedTerm [Syntax.Arg]
normalApplicationRhs x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.coq.syntax.NormalApplication
normalApplicationWithLhs :: Phantoms.TypedTerm Syntax.NormalApplication -> Phantoms.TypedTerm Syntax.Term1 -> Phantoms.TypedTerm Syntax.NormalApplication
normalApplicationWithLhs original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.coq.syntax.NormalApplication
normalApplicationWithRhs :: Phantoms.TypedTerm Syntax.NormalApplication -> Phantoms.TypedTerm [Syntax.Arg] -> Phantoms.TypedTerm Syntax.NormalApplication
normalApplicationWithRhs original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.NotationDeclaration
notationDeclaration :: Phantoms.TypedTerm Syntax.String_ -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm (Maybe Syntax.Natural) -> Phantoms.TypedTerm (Maybe String) -> Phantoms.TypedTerm Syntax.NotationDeclaration
notationDeclaration notation definition level associativity =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "notation"),
          Core.fieldTerm = (Phantoms.unTypedTerm notation)},
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Phantoms.unTypedTerm definition)},
        Core.Field {
          Core.fieldName = (Core.Name "level"),
          Core.fieldTerm = (Phantoms.unTypedTerm level)},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Phantoms.unTypedTerm associativity)}]}))
-- | DSL accessor for the associativity field of hydra.coq.syntax.NotationDeclaration
notationDeclarationAssociativity :: Phantoms.TypedTerm Syntax.NotationDeclaration -> Phantoms.TypedTerm (Maybe String)
notationDeclarationAssociativity x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
        Core.projectionFieldName = (Core.Name "associativity")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the definition field of hydra.coq.syntax.NotationDeclaration
notationDeclarationDefinition :: Phantoms.TypedTerm Syntax.NotationDeclaration -> Phantoms.TypedTerm Syntax.Term
notationDeclarationDefinition x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
        Core.projectionFieldName = (Core.Name "definition")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the level field of hydra.coq.syntax.NotationDeclaration
notationDeclarationLevel :: Phantoms.TypedTerm Syntax.NotationDeclaration -> Phantoms.TypedTerm (Maybe Syntax.Natural)
notationDeclarationLevel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
        Core.projectionFieldName = (Core.Name "level")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the notation field of hydra.coq.syntax.NotationDeclaration
notationDeclarationNotation :: Phantoms.TypedTerm Syntax.NotationDeclaration -> Phantoms.TypedTerm Syntax.String_
notationDeclarationNotation x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
        Core.projectionFieldName = (Core.Name "notation")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the associativity field of hydra.coq.syntax.NotationDeclaration
notationDeclarationWithAssociativity :: Phantoms.TypedTerm Syntax.NotationDeclaration -> Phantoms.TypedTerm (Maybe String) -> Phantoms.TypedTerm Syntax.NotationDeclaration
notationDeclarationWithAssociativity original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "notation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "notation")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "definition")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "level"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "level")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the definition field of hydra.coq.syntax.NotationDeclaration
notationDeclarationWithDefinition :: Phantoms.TypedTerm Syntax.NotationDeclaration -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.NotationDeclaration
notationDeclarationWithDefinition original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "notation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "notation")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "level"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "level")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "associativity")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the level field of hydra.coq.syntax.NotationDeclaration
notationDeclarationWithLevel :: Phantoms.TypedTerm Syntax.NotationDeclaration -> Phantoms.TypedTerm (Maybe Syntax.Natural) -> Phantoms.TypedTerm Syntax.NotationDeclaration
notationDeclarationWithLevel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "notation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "notation")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "definition")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "level"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "associativity")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the notation field of hydra.coq.syntax.NotationDeclaration
notationDeclarationWithNotation :: Phantoms.TypedTerm Syntax.NotationDeclaration -> Phantoms.TypedTerm Syntax.String_ -> Phantoms.TypedTerm Syntax.NotationDeclaration
notationDeclarationWithNotation original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "notation"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "definition")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "level"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "level")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "associativity")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.coq.syntax.Number wrapper
number :: Phantoms.TypedTerm Double -> Phantoms.TypedTerm Syntax.Number
number x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.Number"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL injection for the explicit variant of hydra.coq.syntax.OneTerm
oneTermExplicit :: Phantoms.TypedTerm Syntax.QualidAnnotated -> Phantoms.TypedTerm Syntax.OneTerm
oneTermExplicit x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.OneTerm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "explicit"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the term1 variant of hydra.coq.syntax.OneTerm
oneTermTerm1 :: Phantoms.TypedTerm Syntax.Term1 -> Phantoms.TypedTerm Syntax.OneTerm
oneTermTerm1 x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.OneTerm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term1"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the binders variant of hydra.coq.syntax.OpenBinders
openBindersBinders :: Phantoms.TypedTerm [Syntax.Binder] -> Phantoms.TypedTerm Syntax.OpenBinders
openBindersBinders x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.OpenBinders"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binders"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.coq.syntax.OpenBinders
openBindersType :: Phantoms.TypedTerm Syntax.TypeBinders -> Phantoms.TypedTerm Syntax.OpenBinders
openBindersType x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.OpenBinders"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the number variant of hydra.coq.syntax.Pattern0
pattern0Number :: Phantoms.TypedTerm Syntax.Number -> Phantoms.TypedTerm Syntax.Pattern0
pattern0Number x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the parens variant of hydra.coq.syntax.Pattern0
pattern0Parens :: Phantoms.TypedTerm [Syntax.Pattern] -> Phantoms.TypedTerm Syntax.Pattern0
pattern0Parens x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the placeholder variant of hydra.coq.syntax.Pattern0
pattern0Placeholder :: Phantoms.TypedTerm Syntax.Pattern0
pattern0Placeholder =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "placeholder"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the qualIdAndPattern variant of hydra.coq.syntax.Pattern0
pattern0QualIdAndPattern :: Phantoms.TypedTerm Syntax.QualidAndPattern -> Phantoms.TypedTerm Syntax.Pattern0
pattern0QualIdAndPattern x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualIdAndPattern"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the qualid variant of hydra.coq.syntax.Pattern0
pattern0Qualid :: Phantoms.TypedTerm Syntax.Qualid -> Phantoms.TypedTerm Syntax.Pattern0
pattern0Qualid x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualid"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the string variant of hydra.coq.syntax.Pattern0
pattern0String :: Phantoms.TypedTerm Syntax.String_ -> Phantoms.TypedTerm Syntax.Pattern0
pattern0String x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.Pattern1
pattern1 :: Phantoms.TypedTerm Syntax.Pattern0 -> Phantoms.TypedTerm (Maybe Syntax.ScopeKey) -> Phantoms.TypedTerm Syntax.Pattern1
pattern1 pattern scope =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTypedTerm scope)}]}))
-- | DSL injection for the as variant of hydra.coq.syntax.Pattern10
pattern10As :: Phantoms.TypedTerm Syntax.Pattern10_As -> Phantoms.TypedTerm Syntax.Pattern10
pattern10As x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "as"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the patterns variant of hydra.coq.syntax.Pattern10
pattern10Patterns :: Phantoms.TypedTerm Syntax.Pattern10_Patterns -> Phantoms.TypedTerm Syntax.Pattern10
pattern10Patterns x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "patterns"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the qualiid variant of hydra.coq.syntax.Pattern10
pattern10Qualiid :: Phantoms.TypedTerm Syntax.Pattern10_Qualid -> Phantoms.TypedTerm Syntax.Pattern10
pattern10Qualiid x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualiid"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.Pattern10_As
pattern10_As :: Phantoms.TypedTerm Syntax.Pattern1 -> Phantoms.TypedTerm Syntax.Name -> Phantoms.TypedTerm Syntax.Pattern10_As
pattern10_As pattern as =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTypedTerm as)}]}))
-- | DSL accessor for the as field of hydra.coq.syntax.Pattern10_As
pattern10_AsAs :: Phantoms.TypedTerm Syntax.Pattern10_As -> Phantoms.TypedTerm Syntax.Name
pattern10_AsAs x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the pattern field of hydra.coq.syntax.Pattern10_As
pattern10_AsPattern :: Phantoms.TypedTerm Syntax.Pattern10_As -> Phantoms.TypedTerm Syntax.Pattern1
pattern10_AsPattern x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the as field of hydra.coq.syntax.Pattern10_As
pattern10_AsWithAs :: Phantoms.TypedTerm Syntax.Pattern10_As -> Phantoms.TypedTerm Syntax.Name -> Phantoms.TypedTerm Syntax.Pattern10_As
pattern10_AsWithAs original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the pattern field of hydra.coq.syntax.Pattern10_As
pattern10_AsWithPattern :: Phantoms.TypedTerm Syntax.Pattern10_As -> Phantoms.TypedTerm Syntax.Pattern1 -> Phantoms.TypedTerm Syntax.Pattern10_As
pattern10_AsWithPattern original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.Pattern10_Patterns
pattern10_Patterns :: Phantoms.TypedTerm Syntax.Pattern1 -> Phantoms.TypedTerm [Syntax.Pattern1] -> Phantoms.TypedTerm Syntax.Pattern10_Patterns
pattern10_Patterns pattern patterns =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Phantoms.unTypedTerm patterns)}]}))
-- | DSL accessor for the pattern field of hydra.coq.syntax.Pattern10_Patterns
pattern10_PatternsPattern :: Phantoms.TypedTerm Syntax.Pattern10_Patterns -> Phantoms.TypedTerm Syntax.Pattern1
pattern10_PatternsPattern x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the patterns field of hydra.coq.syntax.Pattern10_Patterns
pattern10_PatternsPatterns :: Phantoms.TypedTerm Syntax.Pattern10_Patterns -> Phantoms.TypedTerm [Syntax.Pattern1]
pattern10_PatternsPatterns x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
        Core.projectionFieldName = (Core.Name "patterns")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the pattern field of hydra.coq.syntax.Pattern10_Patterns
pattern10_PatternsWithPattern :: Phantoms.TypedTerm Syntax.Pattern10_Patterns -> Phantoms.TypedTerm Syntax.Pattern1 -> Phantoms.TypedTerm Syntax.Pattern10_Patterns
pattern10_PatternsWithPattern original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
              Core.projectionFieldName = (Core.Name "patterns")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the patterns field of hydra.coq.syntax.Pattern10_Patterns
pattern10_PatternsWithPatterns :: Phantoms.TypedTerm Syntax.Pattern10_Patterns -> Phantoms.TypedTerm [Syntax.Pattern1] -> Phantoms.TypedTerm Syntax.Pattern10_Patterns
pattern10_PatternsWithPatterns original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.Pattern10_Qualid
pattern10_Qualid :: Phantoms.TypedTerm Syntax.Qualid -> Phantoms.TypedTerm [Syntax.Pattern1] -> Phantoms.TypedTerm Syntax.Pattern10_Qualid
pattern10_Qualid qualid patterns =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Phantoms.unTypedTerm qualid)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Phantoms.unTypedTerm patterns)}]}))
-- | DSL accessor for the patterns field of hydra.coq.syntax.Pattern10_Qualid
pattern10_QualidPatterns :: Phantoms.TypedTerm Syntax.Pattern10_Qualid -> Phantoms.TypedTerm [Syntax.Pattern1]
pattern10_QualidPatterns x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
        Core.projectionFieldName = (Core.Name "patterns")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the qualid field of hydra.coq.syntax.Pattern10_Qualid
pattern10_QualidQualid :: Phantoms.TypedTerm Syntax.Pattern10_Qualid -> Phantoms.TypedTerm Syntax.Qualid
pattern10_QualidQualid x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
        Core.projectionFieldName = (Core.Name "qualid")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the patterns field of hydra.coq.syntax.Pattern10_Qualid
pattern10_QualidWithPatterns :: Phantoms.TypedTerm Syntax.Pattern10_Qualid -> Phantoms.TypedTerm [Syntax.Pattern1] -> Phantoms.TypedTerm Syntax.Pattern10_Qualid
pattern10_QualidWithPatterns original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
              Core.projectionFieldName = (Core.Name "qualid")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the qualid field of hydra.coq.syntax.Pattern10_Qualid
pattern10_QualidWithQualid :: Phantoms.TypedTerm Syntax.Pattern10_Qualid -> Phantoms.TypedTerm Syntax.Qualid -> Phantoms.TypedTerm Syntax.Pattern10_Qualid
pattern10_QualidWithQualid original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
              Core.projectionFieldName = (Core.Name "patterns")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL accessor for the pattern field of hydra.coq.syntax.Pattern1
pattern1Pattern :: Phantoms.TypedTerm Syntax.Pattern1 -> Phantoms.TypedTerm Syntax.Pattern0
pattern1Pattern x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the scope field of hydra.coq.syntax.Pattern1
pattern1Scope :: Phantoms.TypedTerm Syntax.Pattern1 -> Phantoms.TypedTerm (Maybe Syntax.ScopeKey)
pattern1Scope x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
        Core.projectionFieldName = (Core.Name "scope")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the pattern field of hydra.coq.syntax.Pattern1
pattern1WithPattern :: Phantoms.TypedTerm Syntax.Pattern1 -> Phantoms.TypedTerm Syntax.Pattern0 -> Phantoms.TypedTerm Syntax.Pattern1
pattern1WithPattern original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
              Core.projectionFieldName = (Core.Name "scope")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the scope field of hydra.coq.syntax.Pattern1
pattern1WithScope :: Phantoms.TypedTerm Syntax.Pattern1 -> Phantoms.TypedTerm (Maybe Syntax.ScopeKey) -> Phantoms.TypedTerm Syntax.Pattern1
pattern1WithScope original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL injection for the pattern variant of hydra.coq.syntax.Pattern
patternPattern :: Phantoms.TypedTerm Syntax.Pattern10 -> Phantoms.TypedTerm Syntax.Pattern
patternPattern x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the term variant of hydra.coq.syntax.Pattern
patternTerm :: Phantoms.TypedTerm (Maybe Syntax.Term) -> Phantoms.TypedTerm Syntax.Pattern
patternTerm x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the number variant of hydra.coq.syntax.PrimitiveNotations
primitiveNotationsNumber :: Phantoms.TypedTerm Syntax.Number -> Phantoms.TypedTerm Syntax.PrimitiveNotations
primitiveNotationsNumber x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.PrimitiveNotations"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the string variant of hydra.coq.syntax.PrimitiveNotations
primitiveNotationsString :: Phantoms.TypedTerm Syntax.String_ -> Phantoms.TypedTerm Syntax.PrimitiveNotations
primitiveNotationsString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.PrimitiveNotations"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.Qualid
qualid :: Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm [Syntax.FieldIdent] -> Phantoms.TypedTerm Syntax.Qualid
qualid id fieldIds =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "fieldIds"),
          Core.fieldTerm = (Phantoms.unTypedTerm fieldIds)}]}))
-- | DSL constructor for hydra.coq.syntax.QualidAndPattern
qualidAndPattern :: Phantoms.TypedTerm Syntax.Qualid -> Phantoms.TypedTerm Syntax.Pattern -> Phantoms.TypedTerm Syntax.QualidAndPattern
qualidAndPattern qualid pattern =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Phantoms.unTypedTerm qualid)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm pattern)}]}))
-- | DSL accessor for the pattern field of hydra.coq.syntax.QualidAndPattern
qualidAndPatternPattern :: Phantoms.TypedTerm Syntax.QualidAndPattern -> Phantoms.TypedTerm Syntax.Pattern
qualidAndPatternPattern x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the qualid field of hydra.coq.syntax.QualidAndPattern
qualidAndPatternQualid :: Phantoms.TypedTerm Syntax.QualidAndPattern -> Phantoms.TypedTerm Syntax.Qualid
qualidAndPatternQualid x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
        Core.projectionFieldName = (Core.Name "qualid")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the pattern field of hydra.coq.syntax.QualidAndPattern
qualidAndPatternWithPattern :: Phantoms.TypedTerm Syntax.QualidAndPattern -> Phantoms.TypedTerm Syntax.Pattern -> Phantoms.TypedTerm Syntax.QualidAndPattern
qualidAndPatternWithPattern original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
              Core.projectionFieldName = (Core.Name "qualid")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the qualid field of hydra.coq.syntax.QualidAndPattern
qualidAndPatternWithQualid :: Phantoms.TypedTerm Syntax.QualidAndPattern -> Phantoms.TypedTerm Syntax.Qualid -> Phantoms.TypedTerm Syntax.QualidAndPattern
qualidAndPatternWithQualid original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.QualidAnnotated
qualidAnnotated :: Phantoms.TypedTerm Syntax.Qualid -> Phantoms.TypedTerm (Maybe Syntax.UnivAnnot) -> Phantoms.TypedTerm Syntax.QualidAnnotated
qualidAnnotated qualid univAnnot =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Phantoms.unTypedTerm qualid)},
        Core.Field {
          Core.fieldName = (Core.Name "univAnnot"),
          Core.fieldTerm = (Phantoms.unTypedTerm univAnnot)}]}))
-- | DSL accessor for the qualid field of hydra.coq.syntax.QualidAnnotated
qualidAnnotatedQualid :: Phantoms.TypedTerm Syntax.QualidAnnotated -> Phantoms.TypedTerm Syntax.Qualid
qualidAnnotatedQualid x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
        Core.projectionFieldName = (Core.Name "qualid")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the univAnnot field of hydra.coq.syntax.QualidAnnotated
qualidAnnotatedUnivAnnot :: Phantoms.TypedTerm Syntax.QualidAnnotated -> Phantoms.TypedTerm (Maybe Syntax.UnivAnnot)
qualidAnnotatedUnivAnnot x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
        Core.projectionFieldName = (Core.Name "univAnnot")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the qualid field of hydra.coq.syntax.QualidAnnotated
qualidAnnotatedWithQualid :: Phantoms.TypedTerm Syntax.QualidAnnotated -> Phantoms.TypedTerm Syntax.Qualid -> Phantoms.TypedTerm Syntax.QualidAnnotated
qualidAnnotatedWithQualid original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "univAnnot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
              Core.projectionFieldName = (Core.Name "univAnnot")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the univAnnot field of hydra.coq.syntax.QualidAnnotated
qualidAnnotatedWithUnivAnnot :: Phantoms.TypedTerm Syntax.QualidAnnotated -> Phantoms.TypedTerm (Maybe Syntax.UnivAnnot) -> Phantoms.TypedTerm Syntax.QualidAnnotated
qualidAnnotatedWithUnivAnnot original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
              Core.projectionFieldName = (Core.Name "qualid")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "univAnnot"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL accessor for the fieldIds field of hydra.coq.syntax.Qualid
qualidFieldIds :: Phantoms.TypedTerm Syntax.Qualid -> Phantoms.TypedTerm [Syntax.FieldIdent]
qualidFieldIds x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
        Core.projectionFieldName = (Core.Name "fieldIds")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the id field of hydra.coq.syntax.Qualid
qualidId :: Phantoms.TypedTerm Syntax.Qualid -> Phantoms.TypedTerm Syntax.Ident
qualidId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the fieldIds field of hydra.coq.syntax.Qualid
qualidWithFieldIds :: Phantoms.TypedTerm Syntax.Qualid -> Phantoms.TypedTerm [Syntax.FieldIdent] -> Phantoms.TypedTerm Syntax.Qualid
qualidWithFieldIds original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fieldIds"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the id field of hydra.coq.syntax.Qualid
qualidWithId :: Phantoms.TypedTerm Syntax.Qualid -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.Qualid
qualidWithId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fieldIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
              Core.projectionFieldName = (Core.Name "fieldIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.RecordBody
recordBody :: Phantoms.TypedTerm (Maybe Syntax.Ident) -> Phantoms.TypedTerm [Syntax.RecordField] -> Phantoms.TypedTerm Syntax.RecordBody
recordBody constructor fields =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Phantoms.unTypedTerm constructor)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTypedTerm fields)}]}))
-- | DSL accessor for the constructor field of hydra.coq.syntax.RecordBody
recordBodyConstructor :: Phantoms.TypedTerm Syntax.RecordBody -> Phantoms.TypedTerm (Maybe Syntax.Ident)
recordBodyConstructor x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
        Core.projectionFieldName = (Core.Name "constructor")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the fields field of hydra.coq.syntax.RecordBody
recordBodyFields :: Phantoms.TypedTerm Syntax.RecordBody -> Phantoms.TypedTerm [Syntax.RecordField]
recordBodyFields x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
        Core.projectionFieldName = (Core.Name "fields")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the constructor field of hydra.coq.syntax.RecordBody
recordBodyWithConstructor :: Phantoms.TypedTerm Syntax.RecordBody -> Phantoms.TypedTerm (Maybe Syntax.Ident) -> Phantoms.TypedTerm Syntax.RecordBody
recordBodyWithConstructor original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the fields field of hydra.coq.syntax.RecordBody
recordBodyWithFields :: Phantoms.TypedTerm Syntax.RecordBody -> Phantoms.TypedTerm [Syntax.RecordField] -> Phantoms.TypedTerm Syntax.RecordBody
recordBodyWithFields original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
              Core.projectionFieldName = (Core.Name "constructor")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.RecordDefinition
recordDefinition :: Phantoms.TypedTerm (Maybe Syntax.Locality) -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm [Syntax.Binder] -> Phantoms.TypedTerm (Maybe Syntax.Sort) -> Phantoms.TypedTerm Syntax.RecordBody -> Phantoms.TypedTerm Syntax.RecordDefinition
recordDefinition locality name binders sort body =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Phantoms.unTypedTerm locality)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Phantoms.unTypedTerm sort)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTypedTerm body)}]}))
-- | DSL accessor for the binders field of hydra.coq.syntax.RecordDefinition
recordDefinitionBinders :: Phantoms.TypedTerm Syntax.RecordDefinition -> Phantoms.TypedTerm [Syntax.Binder]
recordDefinitionBinders x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.coq.syntax.RecordDefinition
recordDefinitionBody :: Phantoms.TypedTerm Syntax.RecordDefinition -> Phantoms.TypedTerm Syntax.RecordBody
recordDefinitionBody x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the locality field of hydra.coq.syntax.RecordDefinition
recordDefinitionLocality :: Phantoms.TypedTerm Syntax.RecordDefinition -> Phantoms.TypedTerm (Maybe Syntax.Locality)
recordDefinitionLocality x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
        Core.projectionFieldName = (Core.Name "locality")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.coq.syntax.RecordDefinition
recordDefinitionName :: Phantoms.TypedTerm Syntax.RecordDefinition -> Phantoms.TypedTerm Syntax.Ident
recordDefinitionName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the sort field of hydra.coq.syntax.RecordDefinition
recordDefinitionSort :: Phantoms.TypedTerm Syntax.RecordDefinition -> Phantoms.TypedTerm (Maybe Syntax.Sort)
recordDefinitionSort x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
        Core.projectionFieldName = (Core.Name "sort")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the binders field of hydra.coq.syntax.RecordDefinition
recordDefinitionWithBinders :: Phantoms.TypedTerm Syntax.RecordDefinition -> Phantoms.TypedTerm [Syntax.Binder] -> Phantoms.TypedTerm Syntax.RecordDefinition
recordDefinitionWithBinders original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "sort")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.coq.syntax.RecordDefinition
recordDefinitionWithBody :: Phantoms.TypedTerm Syntax.RecordDefinition -> Phantoms.TypedTerm Syntax.RecordBody -> Phantoms.TypedTerm Syntax.RecordDefinition
recordDefinitionWithBody original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "sort")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the locality field of hydra.coq.syntax.RecordDefinition
recordDefinitionWithLocality :: Phantoms.TypedTerm Syntax.RecordDefinition -> Phantoms.TypedTerm (Maybe Syntax.Locality) -> Phantoms.TypedTerm Syntax.RecordDefinition
recordDefinitionWithLocality original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "sort")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.coq.syntax.RecordDefinition
recordDefinitionWithName :: Phantoms.TypedTerm Syntax.RecordDefinition -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.RecordDefinition
recordDefinitionWithName original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "sort")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the sort field of hydra.coq.syntax.RecordDefinition
recordDefinitionWithSort :: Phantoms.TypedTerm Syntax.RecordDefinition -> Phantoms.TypedTerm (Maybe Syntax.Sort) -> Phantoms.TypedTerm Syntax.RecordDefinition
recordDefinitionWithSort original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.RecordField
recordField :: Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.Type -> Phantoms.TypedTerm Syntax.RecordField
recordField name type_ =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm type_)}]}))
-- | DSL accessor for the name field of hydra.coq.syntax.RecordField
recordFieldName :: Phantoms.TypedTerm Syntax.RecordField -> Phantoms.TypedTerm Syntax.Ident
recordFieldName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.RecordField
recordFieldType :: Phantoms.TypedTerm Syntax.RecordField -> Phantoms.TypedTerm Syntax.Type
recordFieldType x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.coq.syntax.RecordField
recordFieldWithName :: Phantoms.TypedTerm Syntax.RecordField -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.RecordField
recordFieldWithName original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.coq.syntax.RecordField
recordFieldWithType :: Phantoms.TypedTerm Syntax.RecordField -> Phantoms.TypedTerm Syntax.Type -> Phantoms.TypedTerm Syntax.RecordField
recordFieldWithType original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.RequireImport
requireImport :: Phantoms.TypedTerm (Maybe Syntax.Qualid) -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm (Maybe Syntax.ImportQualification) -> Phantoms.TypedTerm [Syntax.Qualid] -> Phantoms.TypedTerm Syntax.RequireImport
requireImport from require qualification modules =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Phantoms.unTypedTerm from)},
        Core.Field {
          Core.fieldName = (Core.Name "require"),
          Core.fieldTerm = (Phantoms.unTypedTerm require)},
        Core.Field {
          Core.fieldName = (Core.Name "qualification"),
          Core.fieldTerm = (Phantoms.unTypedTerm qualification)},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Phantoms.unTypedTerm modules)}]}))
-- | DSL accessor for the from field of hydra.coq.syntax.RequireImport
requireImportFrom :: Phantoms.TypedTerm Syntax.RequireImport -> Phantoms.TypedTerm (Maybe Syntax.Qualid)
requireImportFrom x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
        Core.projectionFieldName = (Core.Name "from")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the modules field of hydra.coq.syntax.RequireImport
requireImportModules :: Phantoms.TypedTerm Syntax.RequireImport -> Phantoms.TypedTerm [Syntax.Qualid]
requireImportModules x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
        Core.projectionFieldName = (Core.Name "modules")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the qualification field of hydra.coq.syntax.RequireImport
requireImportQualification :: Phantoms.TypedTerm Syntax.RequireImport -> Phantoms.TypedTerm (Maybe Syntax.ImportQualification)
requireImportQualification x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
        Core.projectionFieldName = (Core.Name "qualification")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the require field of hydra.coq.syntax.RequireImport
requireImportRequire :: Phantoms.TypedTerm Syntax.RequireImport -> Phantoms.TypedTerm Bool
requireImportRequire x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
        Core.projectionFieldName = (Core.Name "require")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the from field of hydra.coq.syntax.RequireImport
requireImportWithFrom :: Phantoms.TypedTerm Syntax.RequireImport -> Phantoms.TypedTerm (Maybe Syntax.Qualid) -> Phantoms.TypedTerm Syntax.RequireImport
requireImportWithFrom original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "require"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "require")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "qualification")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "modules")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the modules field of hydra.coq.syntax.RequireImport
requireImportWithModules :: Phantoms.TypedTerm Syntax.RequireImport -> Phantoms.TypedTerm [Syntax.Qualid] -> Phantoms.TypedTerm Syntax.RequireImport
requireImportWithModules original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "from")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "require"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "require")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "qualification")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the qualification field of hydra.coq.syntax.RequireImport
requireImportWithQualification :: Phantoms.TypedTerm Syntax.RequireImport -> Phantoms.TypedTerm (Maybe Syntax.ImportQualification) -> Phantoms.TypedTerm Syntax.RequireImport
requireImportWithQualification original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "from")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "require"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "require")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualification"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "modules")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the require field of hydra.coq.syntax.RequireImport
requireImportWithRequire :: Phantoms.TypedTerm Syntax.RequireImport -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Syntax.RequireImport
requireImportWithRequire original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "from")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "require"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qualification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "qualification")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "modules")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.ReturnAs
returnAs :: Phantoms.TypedTerm (Maybe Syntax.Name) -> Phantoms.TypedTerm Syntax.Term100 -> Phantoms.TypedTerm Syntax.ReturnAs
returnAs as return =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTypedTerm as)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTypedTerm return)}]}))
-- | DSL accessor for the as field of hydra.coq.syntax.ReturnAs
returnAsAs :: Phantoms.TypedTerm Syntax.ReturnAs -> Phantoms.TypedTerm (Maybe Syntax.Name)
returnAsAs x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the return field of hydra.coq.syntax.ReturnAs
returnAsReturn :: Phantoms.TypedTerm Syntax.ReturnAs -> Phantoms.TypedTerm Syntax.Term100
returnAsReturn x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
        Core.projectionFieldName = (Core.Name "return")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the as field of hydra.coq.syntax.ReturnAs
returnAsWithAs :: Phantoms.TypedTerm Syntax.ReturnAs -> Phantoms.TypedTerm (Maybe Syntax.Name) -> Phantoms.TypedTerm Syntax.ReturnAs
returnAsWithAs original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
              Core.projectionFieldName = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the return field of hydra.coq.syntax.ReturnAs
returnAsWithReturn :: Phantoms.TypedTerm Syntax.ReturnAs -> Phantoms.TypedTerm Syntax.Term100 -> Phantoms.TypedTerm Syntax.ReturnAs
returnAsWithReturn original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.coq.syntax.ScopeKey wrapper
scopeKey :: Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.ScopeKey
scopeKey x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.ScopeKey"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for hydra.coq.syntax.SectionDefinition
sectionDefinition :: Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm [Syntax.Sentence] -> Phantoms.TypedTerm Syntax.SectionDefinition
sectionDefinition name sentences =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Phantoms.unTypedTerm sentences)}]}))
-- | DSL accessor for the name field of hydra.coq.syntax.SectionDefinition
sectionDefinitionName :: Phantoms.TypedTerm Syntax.SectionDefinition -> Phantoms.TypedTerm Syntax.Ident
sectionDefinitionName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the sentences field of hydra.coq.syntax.SectionDefinition
sectionDefinitionSentences :: Phantoms.TypedTerm Syntax.SectionDefinition -> Phantoms.TypedTerm [Syntax.Sentence]
sectionDefinitionSentences x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
        Core.projectionFieldName = (Core.Name "sentences")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.coq.syntax.SectionDefinition
sectionDefinitionWithName :: Phantoms.TypedTerm Syntax.SectionDefinition -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.SectionDefinition
sectionDefinitionWithName original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
              Core.projectionFieldName = (Core.Name "sentences")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the sentences field of hydra.coq.syntax.SectionDefinition
sectionDefinitionWithSentences :: Phantoms.TypedTerm Syntax.SectionDefinition -> Phantoms.TypedTerm [Syntax.Sentence] -> Phantoms.TypedTerm Syntax.SectionDefinition
sectionDefinitionWithSentences original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.Sentence
sentence :: Phantoms.TypedTerm (Maybe Syntax.Comment) -> Phantoms.TypedTerm Syntax.SentenceContent -> Phantoms.TypedTerm Syntax.Sentence
sentence comment content =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Phantoms.unTypedTerm comment)},
        Core.Field {
          Core.fieldName = (Core.Name "content"),
          Core.fieldTerm = (Phantoms.unTypedTerm content)}]}))
-- | DSL accessor for the comment field of hydra.coq.syntax.Sentence
sentenceComment :: Phantoms.TypedTerm Syntax.Sentence -> Phantoms.TypedTerm (Maybe Syntax.Comment)
sentenceComment x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
        Core.projectionFieldName = (Core.Name "comment")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the content field of hydra.coq.syntax.Sentence
sentenceContent :: Phantoms.TypedTerm Syntax.Sentence -> Phantoms.TypedTerm Syntax.SentenceContent
sentenceContent x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
        Core.projectionFieldName = (Core.Name "content")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL injection for the axiom variant of hydra.coq.syntax.SentenceContent
sentenceContentAxiom :: Phantoms.TypedTerm Syntax.AxiomDeclaration -> Phantoms.TypedTerm Syntax.SentenceContent
sentenceContentAxiom x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "axiom"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the definition variant of hydra.coq.syntax.SentenceContent
sentenceContentDefinition :: Phantoms.TypedTerm Syntax.Definition -> Phantoms.TypedTerm Syntax.SentenceContent
sentenceContentDefinition x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "definition"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the fixpoint variant of hydra.coq.syntax.SentenceContent
sentenceContentFixpoint :: Phantoms.TypedTerm Syntax.FixpointDefinition -> Phantoms.TypedTerm Syntax.SentenceContent
sentenceContentFixpoint x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fixpoint"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the inductive variant of hydra.coq.syntax.SentenceContent
sentenceContentInductive :: Phantoms.TypedTerm Syntax.InductiveDefinition -> Phantoms.TypedTerm Syntax.SentenceContent
sentenceContentInductive x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inductive"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the module variant of hydra.coq.syntax.SentenceContent
sentenceContentModule :: Phantoms.TypedTerm Syntax.ModuleDefinition -> Phantoms.TypedTerm Syntax.SentenceContent
sentenceContentModule x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "module"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the notation variant of hydra.coq.syntax.SentenceContent
sentenceContentNotation :: Phantoms.TypedTerm Syntax.NotationDeclaration -> Phantoms.TypedTerm Syntax.SentenceContent
sentenceContentNotation x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notation"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the record variant of hydra.coq.syntax.SentenceContent
sentenceContentRecord :: Phantoms.TypedTerm Syntax.RecordDefinition -> Phantoms.TypedTerm Syntax.SentenceContent
sentenceContentRecord x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the requireImport variant of hydra.coq.syntax.SentenceContent
sentenceContentRequireImport :: Phantoms.TypedTerm Syntax.RequireImport -> Phantoms.TypedTerm Syntax.SentenceContent
sentenceContentRequireImport x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "requireImport"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the section variant of hydra.coq.syntax.SentenceContent
sentenceContentSection :: Phantoms.TypedTerm Syntax.SectionDefinition -> Phantoms.TypedTerm Syntax.SentenceContent
sentenceContentSection x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "section"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the theorem variant of hydra.coq.syntax.SentenceContent
sentenceContentTheorem :: Phantoms.TypedTerm Syntax.TheoremBody -> Phantoms.TypedTerm Syntax.SentenceContent
sentenceContentTheorem x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "theorem"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL updater for the comment field of hydra.coq.syntax.Sentence
sentenceWithComment :: Phantoms.TypedTerm Syntax.Sentence -> Phantoms.TypedTerm (Maybe Syntax.Comment) -> Phantoms.TypedTerm Syntax.Sentence
sentenceWithComment original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "content"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
              Core.projectionFieldName = (Core.Name "content")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the content field of hydra.coq.syntax.Sentence
sentenceWithContent :: Phantoms.TypedTerm Syntax.Sentence -> Phantoms.TypedTerm Syntax.SentenceContent -> Phantoms.TypedTerm Syntax.Sentence
sentenceWithContent original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
              Core.projectionFieldName = (Core.Name "comment")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "content"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL injection for the prop variant of hydra.coq.syntax.Sort
sortProp :: Phantoms.TypedTerm Syntax.Sort
sortProp =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "prop"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the sProp variant of hydra.coq.syntax.Sort
sortSProp :: Phantoms.TypedTerm Syntax.Sort
sortSProp =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sProp"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the set variant of hydra.coq.syntax.Sort
sortSet :: Phantoms.TypedTerm Syntax.Sort
sortSet =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the type variant of hydra.coq.syntax.Sort
sortType :: Phantoms.TypedTerm Syntax.Sort
sortType =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the typeWithAnyUniverse variant of hydra.coq.syntax.Sort
sortTypeWithAnyUniverse :: Phantoms.TypedTerm Syntax.Sort
sortTypeWithAnyUniverse =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeWithAnyUniverse"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the typeWithUniverse variant of hydra.coq.syntax.Sort
sortTypeWithUniverse :: Phantoms.TypedTerm Syntax.Universe -> Phantoms.TypedTerm Syntax.Sort
sortTypeWithUniverse x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeWithUniverse"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for the hydra.coq.syntax.String wrapper
string :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Syntax.String_
string x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.String"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL injection for the evar variant of hydra.coq.syntax.Term0
term0Evar :: Phantoms.TypedTerm Syntax.ExistentialVariable -> Phantoms.TypedTerm Syntax.Term0
term0Evar x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "evar"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the generalizing variant of hydra.coq.syntax.Term0
term0Generalizing :: Phantoms.TypedTerm Syntax.Term0
term0Generalizing =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "generalizing"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the ltac variant of hydra.coq.syntax.Term0
term0Ltac :: Phantoms.TypedTerm Syntax.Term0
term0Ltac =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ltac"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the match variant of hydra.coq.syntax.Term0
term0Match :: Phantoms.TypedTerm Syntax.Match -> Phantoms.TypedTerm Syntax.Term0
term0Match x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the parens variant of hydra.coq.syntax.Term0
term0Parens :: Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.Term0
term0Parens x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the primitiveNotations variant of hydra.coq.syntax.Term0
term0PrimitiveNotations :: Phantoms.TypedTerm Syntax.PrimitiveNotations -> Phantoms.TypedTerm Syntax.Term0
term0PrimitiveNotations x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitiveNotations"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the qualidAnnotated variant of hydra.coq.syntax.Term0
term0QualidAnnotated :: Phantoms.TypedTerm Syntax.QualidAnnotated -> Phantoms.TypedTerm Syntax.Term0
term0QualidAnnotated x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualidAnnotated"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the record variant of hydra.coq.syntax.Term0
term0Record :: Phantoms.TypedTerm Syntax.Term0
term0Record =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the sort variant of hydra.coq.syntax.Term0
term0Sort :: Phantoms.TypedTerm Syntax.Sort -> Phantoms.TypedTerm Syntax.Term0
term0Sort x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sort"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the cast variant of hydra.coq.syntax.Term100
term100Cast :: Phantoms.TypedTerm Syntax.TypeCast -> Phantoms.TypedTerm Syntax.Term100
term100Cast x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term100"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cast"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the term10 variant of hydra.coq.syntax.Term100
term100Term10 :: Phantoms.TypedTerm Syntax.Term10 -> Phantoms.TypedTerm Syntax.Term100
term100Term10 x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term100"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term10"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the application variant of hydra.coq.syntax.Term10
term10Application :: Phantoms.TypedTerm Syntax.Application -> Phantoms.TypedTerm Syntax.Term10
term10Application x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term10"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the oneTerm variant of hydra.coq.syntax.Term10
term10OneTerm :: Phantoms.TypedTerm Syntax.OneTerm -> Phantoms.TypedTerm Syntax.Term10
term10OneTerm x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term10"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "oneTerm"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the projection variant of hydra.coq.syntax.Term1
term1Projection :: Phantoms.TypedTerm Syntax.Term1
term1Projection =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term1"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "projection"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the scope variant of hydra.coq.syntax.Term1
term1Scope :: Phantoms.TypedTerm Syntax.Term1
term1Scope =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term1"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "scope"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the term0 variant of hydra.coq.syntax.Term1
term1Term0 :: Phantoms.TypedTerm Syntax.Term0 -> Phantoms.TypedTerm Syntax.Term1
term1Term0 x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term1"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term0"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the cofix variant of hydra.coq.syntax.Term
termCofix :: Phantoms.TypedTerm Syntax.Cofix -> Phantoms.TypedTerm Syntax.Term
termCofix x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cofix"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the fix variant of hydra.coq.syntax.Term
termFix :: Phantoms.TypedTerm Syntax.Fix -> Phantoms.TypedTerm Syntax.Term
termFix x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fix"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the forallOrFun variant of hydra.coq.syntax.Term
termForallOrFun :: Phantoms.TypedTerm Syntax.ForallOrFun -> Phantoms.TypedTerm Syntax.Term
termForallOrFun x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forallOrFun"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the if variant of hydra.coq.syntax.Term
termIf :: Phantoms.TypedTerm Syntax.If -> Phantoms.TypedTerm Syntax.Term
termIf x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the let variant of hydra.coq.syntax.Term
termLet :: Phantoms.TypedTerm Syntax.Let -> Phantoms.TypedTerm Syntax.Term
termLet x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the term100 variant of hydra.coq.syntax.Term
termTerm100 :: Phantoms.TypedTerm Syntax.Term100 -> Phantoms.TypedTerm Syntax.Term
termTerm100 x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term100"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.TheoremBody
theoremBody :: Phantoms.TypedTerm Syntax.TheoremKind -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm [Syntax.Binder] -> Phantoms.TypedTerm Syntax.Type -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.TheoremBody
theoremBody kind name binders type_ proof =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Phantoms.unTypedTerm kind)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "proof"),
          Core.fieldTerm = (Phantoms.unTypedTerm proof)}]}))
-- | DSL accessor for the binders field of hydra.coq.syntax.TheoremBody
theoremBodyBinders :: Phantoms.TypedTerm Syntax.TheoremBody -> Phantoms.TypedTerm [Syntax.Binder]
theoremBodyBinders x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the kind field of hydra.coq.syntax.TheoremBody
theoremBodyKind :: Phantoms.TypedTerm Syntax.TheoremBody -> Phantoms.TypedTerm Syntax.TheoremKind
theoremBodyKind x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
        Core.projectionFieldName = (Core.Name "kind")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.coq.syntax.TheoremBody
theoremBodyName :: Phantoms.TypedTerm Syntax.TheoremBody -> Phantoms.TypedTerm Syntax.Ident
theoremBodyName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the proof field of hydra.coq.syntax.TheoremBody
theoremBodyProof :: Phantoms.TypedTerm Syntax.TheoremBody -> Phantoms.TypedTerm Syntax.Term
theoremBodyProof x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
        Core.projectionFieldName = (Core.Name "proof")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.TheoremBody
theoremBodyType :: Phantoms.TypedTerm Syntax.TheoremBody -> Phantoms.TypedTerm Syntax.Type
theoremBodyType x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the binders field of hydra.coq.syntax.TheoremBody
theoremBodyWithBinders :: Phantoms.TypedTerm Syntax.TheoremBody -> Phantoms.TypedTerm [Syntax.Binder] -> Phantoms.TypedTerm Syntax.TheoremBody
theoremBodyWithBinders original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "proof"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "proof")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the kind field of hydra.coq.syntax.TheoremBody
theoremBodyWithKind :: Phantoms.TypedTerm Syntax.TheoremBody -> Phantoms.TypedTerm Syntax.TheoremKind -> Phantoms.TypedTerm Syntax.TheoremBody
theoremBodyWithKind original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "proof"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "proof")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.coq.syntax.TheoremBody
theoremBodyWithName :: Phantoms.TypedTerm Syntax.TheoremBody -> Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.TheoremBody
theoremBodyWithName original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "proof"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "proof")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the proof field of hydra.coq.syntax.TheoremBody
theoremBodyWithProof :: Phantoms.TypedTerm Syntax.TheoremBody -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.TheoremBody
theoremBodyWithProof original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "proof"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the type field of hydra.coq.syntax.TheoremBody
theoremBodyWithType :: Phantoms.TypedTerm Syntax.TheoremBody -> Phantoms.TypedTerm Syntax.Type -> Phantoms.TypedTerm Syntax.TheoremBody
theoremBodyWithType original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "proof"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "proof")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL injection for the corollary variant of hydra.coq.syntax.TheoremKind
theoremKindCorollary :: Phantoms.TypedTerm Syntax.TheoremKind
theoremKindCorollary =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TheoremKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "corollary"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the example variant of hydra.coq.syntax.TheoremKind
theoremKindExample :: Phantoms.TypedTerm Syntax.TheoremKind
theoremKindExample =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TheoremKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "example"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lemma variant of hydra.coq.syntax.TheoremKind
theoremKindLemma :: Phantoms.TypedTerm Syntax.TheoremKind
theoremKindLemma =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TheoremKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lemma"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the proposition variant of hydra.coq.syntax.TheoremKind
theoremKindProposition :: Phantoms.TypedTerm Syntax.TheoremKind
theoremKindProposition =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TheoremKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "proposition"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the theorem variant of hydra.coq.syntax.TheoremKind
theoremKindTheorem :: Phantoms.TypedTerm Syntax.TheoremKind
theoremKindTheorem =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TheoremKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "theorem"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.coq.syntax.Type wrapper
type_ :: Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.Type
type_ x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.Type"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for hydra.coq.syntax.TypeBinders
typeBinders :: Phantoms.TypedTerm [Syntax.Name] -> Phantoms.TypedTerm Syntax.Type -> Phantoms.TypedTerm Syntax.TypeBinders
typeBinders names type_ =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTypedTerm names)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm type_)}]}))
-- | DSL accessor for the names field of hydra.coq.syntax.TypeBinders
typeBindersNames :: Phantoms.TypedTerm Syntax.TypeBinders -> Phantoms.TypedTerm [Syntax.Name]
typeBindersNames x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
        Core.projectionFieldName = (Core.Name "names")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.TypeBinders
typeBindersType :: Phantoms.TypedTerm Syntax.TypeBinders -> Phantoms.TypedTerm Syntax.Type
typeBindersType x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the names field of hydra.coq.syntax.TypeBinders
typeBindersWithNames :: Phantoms.TypedTerm Syntax.TypeBinders -> Phantoms.TypedTerm [Syntax.Name] -> Phantoms.TypedTerm Syntax.TypeBinders
typeBindersWithNames original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.coq.syntax.TypeBinders
typeBindersWithType :: Phantoms.TypedTerm Syntax.TypeBinders -> Phantoms.TypedTerm Syntax.Type -> Phantoms.TypedTerm Syntax.TypeBinders
typeBindersWithType original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.TypeCast
typeCast :: Phantoms.TypedTerm Syntax.Term10 -> Phantoms.TypedTerm Syntax.Type -> Phantoms.TypedTerm Syntax.TypeCastOperator -> Phantoms.TypedTerm Syntax.TypeCast
typeCast term type_ operator =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm operator)}]}))
-- | DSL accessor for the operator field of hydra.coq.syntax.TypeCast
typeCastOperator :: Phantoms.TypedTerm Syntax.TypeCast -> Phantoms.TypedTerm Syntax.TypeCastOperator
typeCastOperator x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL injection for the nativeCompute variant of hydra.coq.syntax.TypeCastOperator
typeCastOperatorNativeCompute :: Phantoms.TypedTerm Syntax.TypeCastOperator
typeCastOperatorNativeCompute =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TypeCastOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nativeCompute"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the normal variant of hydra.coq.syntax.TypeCastOperator
typeCastOperatorNormal :: Phantoms.TypedTerm Syntax.TypeCastOperator
typeCastOperatorNormal =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TypeCastOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the vmCompute variant of hydra.coq.syntax.TypeCastOperator
typeCastOperatorVmCompute :: Phantoms.TypedTerm Syntax.TypeCastOperator
typeCastOperatorVmCompute =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TypeCastOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vmCompute"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the term field of hydra.coq.syntax.TypeCast
typeCastTerm :: Phantoms.TypedTerm Syntax.TypeCast -> Phantoms.TypedTerm Syntax.Term10
typeCastTerm x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.TypeCast
typeCastType :: Phantoms.TypedTerm Syntax.TypeCast -> Phantoms.TypedTerm Syntax.Type
typeCastType x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the operator field of hydra.coq.syntax.TypeCast
typeCastWithOperator :: Phantoms.TypedTerm Syntax.TypeCast -> Phantoms.TypedTerm Syntax.TypeCastOperator -> Phantoms.TypedTerm Syntax.TypeCast
typeCastWithOperator original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the term field of hydra.coq.syntax.TypeCast
typeCastWithTerm :: Phantoms.TypedTerm Syntax.TypeCast -> Phantoms.TypedTerm Syntax.Term10 -> Phantoms.TypedTerm Syntax.TypeCast
typeCastWithTerm original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.coq.syntax.TypeCast
typeCastWithType :: Phantoms.TypedTerm Syntax.TypeCast -> Phantoms.TypedTerm Syntax.Type -> Phantoms.TypedTerm Syntax.TypeCast
typeCastWithType original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.TypeclassConstraint
typeclassConstraint :: Phantoms.TypedTerm (Maybe Syntax.Name) -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.TypeclassConstraint
typeclassConstraint name generalizing term =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "generalizing"),
          Core.fieldTerm = (Phantoms.unTypedTerm generalizing)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm term)}]}))
-- | DSL accessor for the generalizing field of hydra.coq.syntax.TypeclassConstraint
typeclassConstraintGeneralizing :: Phantoms.TypedTerm Syntax.TypeclassConstraint -> Phantoms.TypedTerm Bool
typeclassConstraintGeneralizing x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
        Core.projectionFieldName = (Core.Name "generalizing")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.coq.syntax.TypeclassConstraint
typeclassConstraintName :: Phantoms.TypedTerm Syntax.TypeclassConstraint -> Phantoms.TypedTerm (Maybe Syntax.Name)
typeclassConstraintName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.TypeclassConstraint
typeclassConstraintTerm :: Phantoms.TypedTerm Syntax.TypeclassConstraint -> Phantoms.TypedTerm Syntax.Term
typeclassConstraintTerm x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the generalizing field of hydra.coq.syntax.TypeclassConstraint
typeclassConstraintWithGeneralizing :: Phantoms.TypedTerm Syntax.TypeclassConstraint -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Syntax.TypeclassConstraint
typeclassConstraintWithGeneralizing original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generalizing"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.coq.syntax.TypeclassConstraint
typeclassConstraintWithName :: Phantoms.TypedTerm Syntax.TypeclassConstraint -> Phantoms.TypedTerm (Maybe Syntax.Name) -> Phantoms.TypedTerm Syntax.TypeclassConstraint
typeclassConstraintWithName original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "generalizing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
              Core.projectionFieldName = (Core.Name "generalizing")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.coq.syntax.TypeclassConstraint
typeclassConstraintWithTerm :: Phantoms.TypedTerm Syntax.TypeclassConstraint -> Phantoms.TypedTerm Syntax.Term -> Phantoms.TypedTerm Syntax.TypeclassConstraint
typeclassConstraintWithTerm original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generalizing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
              Core.projectionFieldName = (Core.Name "generalizing")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL accessor for the body of hydra.coq.syntax.Comment
unComment :: Phantoms.TypedTerm Syntax.Comment -> Phantoms.TypedTerm String
unComment x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.Comment")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.coq.syntax.FieldIdent
unFieldIdent :: Phantoms.TypedTerm Syntax.FieldIdent -> Phantoms.TypedTerm Syntax.Ident
unFieldIdent x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.FieldIdent")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.coq.syntax.Ident
unIdent :: Phantoms.TypedTerm Syntax.Ident -> Phantoms.TypedTerm Syntax.String_
unIdent x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.Ident")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.coq.syntax.Name
unName :: Phantoms.TypedTerm Syntax.Name -> Phantoms.TypedTerm (Maybe Syntax.Ident)
unName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.Name")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.coq.syntax.Natural
unNatural :: Phantoms.TypedTerm Syntax.Natural -> Phantoms.TypedTerm Integer
unNatural x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.Natural")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.coq.syntax.Number
unNumber :: Phantoms.TypedTerm Syntax.Number -> Phantoms.TypedTerm Double
unNumber x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.Number")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.coq.syntax.ScopeKey
unScopeKey :: Phantoms.TypedTerm Syntax.ScopeKey -> Phantoms.TypedTerm Syntax.Ident
unScopeKey x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.ScopeKey")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.coq.syntax.String
unString :: Phantoms.TypedTerm Syntax.String_ -> Phantoms.TypedTerm String
unString x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.String")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.coq.syntax.Type
unType :: Phantoms.TypedTerm Syntax.Type -> Phantoms.TypedTerm Syntax.Term
unType x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.Type")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.coq.syntax.UnivAnnot
unUnivAnnot :: Phantoms.TypedTerm Syntax.UnivAnnot -> Phantoms.TypedTerm [Syntax.UniverseLevel]
unUnivAnnot x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.UnivAnnot")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for the hydra.coq.syntax.UnivAnnot wrapper
univAnnot :: Phantoms.TypedTerm [Syntax.UniverseLevel] -> Phantoms.TypedTerm Syntax.UnivAnnot
univAnnot x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.UnivAnnot"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL injection for the expr variant of hydra.coq.syntax.Universe
universeExpr :: Phantoms.TypedTerm Syntax.Universe_Expr -> Phantoms.TypedTerm Syntax.Universe
universeExpr x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Universe"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expr"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the ignored variant of hydra.coq.syntax.UniverseLevel
universeLevelIgnored :: Phantoms.TypedTerm Syntax.UniverseLevel
universeLevelIgnored =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseLevel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ignored"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the prop variant of hydra.coq.syntax.UniverseLevel
universeLevelProp :: Phantoms.TypedTerm Syntax.UniverseLevel
universeLevelProp =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseLevel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "prop"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the qualid variant of hydra.coq.syntax.UniverseLevel
universeLevelQualid :: Phantoms.TypedTerm Syntax.Qualid -> Phantoms.TypedTerm Syntax.UniverseLevel
universeLevelQualid x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseLevel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualid"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the set variant of hydra.coq.syntax.UniverseLevel
universeLevelSet :: Phantoms.TypedTerm Syntax.UniverseLevel
universeLevelSet =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseLevel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the type variant of hydra.coq.syntax.UniverseLevel
universeLevelType :: Phantoms.TypedTerm Syntax.UniverseLevel
universeLevelType =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseLevel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the max variant of hydra.coq.syntax.Universe
universeMax :: Phantoms.TypedTerm [Syntax.Universe_Expr] -> Phantoms.TypedTerm Syntax.Universe
universeMax x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Universe"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "max"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the prop variant of hydra.coq.syntax.UniverseName
universeNameProp :: Phantoms.TypedTerm Syntax.UniverseName
universeNameProp =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "prop"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the qualid variant of hydra.coq.syntax.UniverseName
universeNameQualid :: Phantoms.TypedTerm Syntax.Qualid -> Phantoms.TypedTerm Syntax.UniverseName
universeNameQualid x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualid"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the set variant of hydra.coq.syntax.UniverseName
universeNameSet :: Phantoms.TypedTerm Syntax.UniverseName
universeNameSet =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.coq.syntax.Universe_Expr
universe_Expr :: Phantoms.TypedTerm Syntax.UniverseName -> Phantoms.TypedTerm (Maybe Syntax.Natural) -> Phantoms.TypedTerm Syntax.Universe_Expr
universe_Expr name number =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Phantoms.unTypedTerm number)}]}))
-- | DSL accessor for the name field of hydra.coq.syntax.Universe_Expr
universe_ExprName :: Phantoms.TypedTerm Syntax.Universe_Expr -> Phantoms.TypedTerm Syntax.UniverseName
universe_ExprName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the number field of hydra.coq.syntax.Universe_Expr
universe_ExprNumber :: Phantoms.TypedTerm Syntax.Universe_Expr -> Phantoms.TypedTerm (Maybe Syntax.Natural)
universe_ExprNumber x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
        Core.projectionFieldName = (Core.Name "number")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.coq.syntax.Universe_Expr
universe_ExprWithName :: Phantoms.TypedTerm Syntax.Universe_Expr -> Phantoms.TypedTerm Syntax.UniverseName -> Phantoms.TypedTerm Syntax.Universe_Expr
universe_ExprWithName original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
              Core.projectionFieldName = (Core.Name "number")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the number field of hydra.coq.syntax.Universe_Expr
universe_ExprWithNumber :: Phantoms.TypedTerm Syntax.Universe_Expr -> Phantoms.TypedTerm (Maybe Syntax.Natural) -> Phantoms.TypedTerm Syntax.Universe_Expr
universe_ExprWithNumber original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
