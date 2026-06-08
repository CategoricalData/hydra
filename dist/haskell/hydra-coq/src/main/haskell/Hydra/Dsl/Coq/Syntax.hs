-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.coq.syntax

module Hydra.Dsl.Coq.Syntax where
import qualified Hydra.Coq.Syntax as Syntax
import qualified Hydra.Core as Core
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL constructor for hydra.coq.syntax.AnnotatedApplication
annotatedApplication :: Typed.TypedTerm Syntax.QualidAnnotated -> Typed.TypedTerm [Syntax.Term1] -> Typed.TypedTerm Syntax.AnnotatedApplication
annotatedApplication annot terms =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Typed.unTypedTerm annot)},
        Core.Field {
          Core.fieldName = (Core.Name "terms"),
          Core.fieldTerm = (Typed.unTypedTerm terms)}]}))
-- | DSL accessor for the annot field of hydra.coq.syntax.AnnotatedApplication
annotatedApplicationAnnot :: Typed.TypedTerm Syntax.AnnotatedApplication -> Typed.TypedTerm Syntax.QualidAnnotated
annotatedApplicationAnnot x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
        Core.projectionFieldName = (Core.Name "annot")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the terms field of hydra.coq.syntax.AnnotatedApplication
annotatedApplicationTerms :: Typed.TypedTerm Syntax.AnnotatedApplication -> Typed.TypedTerm [Syntax.Term1]
annotatedApplicationTerms x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
        Core.projectionFieldName = (Core.Name "terms")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annot field of hydra.coq.syntax.AnnotatedApplication
annotatedApplicationWithAnnot :: Typed.TypedTerm Syntax.AnnotatedApplication -> Typed.TypedTerm Syntax.QualidAnnotated -> Typed.TypedTerm Syntax.AnnotatedApplication
annotatedApplicationWithAnnot original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "terms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
              Core.projectionFieldName = (Core.Name "terms")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the terms field of hydra.coq.syntax.AnnotatedApplication
annotatedApplicationWithTerms :: Typed.TypedTerm Syntax.AnnotatedApplication -> Typed.TypedTerm [Syntax.Term1] -> Typed.TypedTerm Syntax.AnnotatedApplication
annotatedApplicationWithTerms original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AnnotatedApplication"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "terms"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the annotated variant of hydra.coq.syntax.Application
applicationAnnotated :: Typed.TypedTerm Syntax.AnnotatedApplication -> Typed.TypedTerm Syntax.Application
applicationAnnotated x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Application"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotated"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the normal variant of hydra.coq.syntax.Application
applicationNormal :: Typed.TypedTerm Syntax.NormalApplication -> Typed.TypedTerm Syntax.Application
applicationNormal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Application"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ident variant of hydra.coq.syntax.Arg
argIdent :: Typed.TypedTerm Syntax.IdentArg -> Typed.TypedTerm Syntax.Arg
argIdent x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Arg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ident"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the natural variant of hydra.coq.syntax.Arg
argNatural :: Typed.TypedTerm Syntax.NaturalArg -> Typed.TypedTerm Syntax.Arg
argNatural x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Arg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "natural"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the term variant of hydra.coq.syntax.Arg
argTerm :: Typed.TypedTerm Syntax.Term1 -> Typed.TypedTerm Syntax.Arg
argTerm x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Arg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.AxiomDeclaration
axiomDeclaration :: Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.AxiomDeclaration
axiomDeclaration name type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the name field of hydra.coq.syntax.AxiomDeclaration
axiomDeclarationName :: Typed.TypedTerm Syntax.AxiomDeclaration -> Typed.TypedTerm Syntax.Ident
axiomDeclarationName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.AxiomDeclaration
axiomDeclarationType :: Typed.TypedTerm Syntax.AxiomDeclaration -> Typed.TypedTerm Syntax.Type
axiomDeclarationType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.coq.syntax.AxiomDeclaration
axiomDeclarationWithName :: Typed.TypedTerm Syntax.AxiomDeclaration -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.AxiomDeclaration
axiomDeclarationWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.coq.syntax.AxiomDeclaration
axiomDeclarationWithType :: Typed.TypedTerm Syntax.AxiomDeclaration -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.AxiomDeclaration
axiomDeclarationWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.AxiomDeclaration"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the generalizing variant of hydra.coq.syntax.Binder
binderGeneralizing :: Typed.TypedTerm Syntax.GeneralizingBinder -> Typed.TypedTerm Syntax.Binder
binderGeneralizing x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "generalizing"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the implicit variant of hydra.coq.syntax.Binder
binderImplicit :: Typed.TypedTerm Syntax.ImplicitBinders -> Typed.TypedTerm Syntax.Binder
binderImplicit x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicit"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the name variant of hydra.coq.syntax.Binder
binderName :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Binder
binderName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the pattern variant of hydra.coq.syntax.Binder
binderPattern :: Typed.TypedTerm Syntax.Pattern0 -> Typed.TypedTerm Syntax.Binder
binderPattern x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the term variant of hydra.coq.syntax.Binder
binderTerm :: Typed.TypedTerm Syntax.LetBinder -> Typed.TypedTerm Syntax.Binder
binderTerm x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.coq.syntax.Binder
binderType :: Typed.TypedTerm Syntax.TypeBinders -> Typed.TypedTerm Syntax.Binder
binderType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Binder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.CaseItem
caseItem :: Typed.TypedTerm Syntax.Term100 -> Typed.TypedTerm (Maybe Syntax.Name) -> Typed.TypedTerm (Maybe Syntax.Pattern) -> Typed.TypedTerm Syntax.CaseItem
caseItem term as in_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Typed.unTypedTerm as)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Typed.unTypedTerm in_)}]}))
-- | DSL accessor for the as field of hydra.coq.syntax.CaseItem
caseItemAs :: Typed.TypedTerm Syntax.CaseItem -> Typed.TypedTerm (Maybe Syntax.Name)
caseItemAs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the in field of hydra.coq.syntax.CaseItem
caseItemIn :: Typed.TypedTerm Syntax.CaseItem -> Typed.TypedTerm (Maybe Syntax.Pattern)
caseItemIn x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
        Core.projectionFieldName = (Core.Name "in")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.CaseItem
caseItemTerm :: Typed.TypedTerm Syntax.CaseItem -> Typed.TypedTerm Syntax.Term100
caseItemTerm x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the as field of hydra.coq.syntax.CaseItem
caseItemWithAs :: Typed.TypedTerm Syntax.CaseItem -> Typed.TypedTerm (Maybe Syntax.Name) -> Typed.TypedTerm Syntax.CaseItem
caseItemWithAs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the in field of hydra.coq.syntax.CaseItem
caseItemWithIn :: Typed.TypedTerm Syntax.CaseItem -> Typed.TypedTerm (Maybe Syntax.Pattern) -> Typed.TypedTerm Syntax.CaseItem
caseItemWithIn original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the term field of hydra.coq.syntax.CaseItem
caseItemWithTerm :: Typed.TypedTerm Syntax.CaseItem -> Typed.TypedTerm Syntax.Term100 -> Typed.TypedTerm Syntax.CaseItem
caseItemWithTerm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CaseItem"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.Cofix
cofix :: Typed.TypedTerm Syntax.CofixBody -> Typed.TypedTerm (Maybe Syntax.CofixQual) -> Typed.TypedTerm Syntax.Cofix
cofix body qual =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Typed.unTypedTerm qual)}]}))
-- | DSL accessor for the body field of hydra.coq.syntax.Cofix
cofixBody :: Typed.TypedTerm Syntax.Cofix -> Typed.TypedTerm Syntax.CofixBody
cofixBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.coq.syntax.CofixBody
cofixBody2 :: Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm [Syntax.Binder] -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.CofixBody
cofixBody2 ident binders type_ term =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Typed.unTypedTerm ident)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm term)}]}))
-- | DSL accessor for the binders field of hydra.coq.syntax.CofixBody
cofixBodyBinders :: Typed.TypedTerm Syntax.CofixBody -> Typed.TypedTerm [Syntax.Binder]
cofixBodyBinders x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the ident field of hydra.coq.syntax.CofixBody
cofixBodyIdent :: Typed.TypedTerm Syntax.CofixBody -> Typed.TypedTerm Syntax.Ident
cofixBodyIdent x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
        Core.projectionFieldName = (Core.Name "ident")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.CofixBody
cofixBodyTerm :: Typed.TypedTerm Syntax.CofixBody -> Typed.TypedTerm Syntax.Term
cofixBodyTerm x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.CofixBody
cofixBodyType :: Typed.TypedTerm Syntax.CofixBody -> Typed.TypedTerm (Maybe Syntax.Type)
cofixBodyType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the binders field of hydra.coq.syntax.CofixBody
cofixBodyWithBinders :: Typed.TypedTerm Syntax.CofixBody -> Typed.TypedTerm [Syntax.Binder] -> Typed.TypedTerm Syntax.CofixBody
cofixBodyWithBinders original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the ident field of hydra.coq.syntax.CofixBody
cofixBodyWithIdent :: Typed.TypedTerm Syntax.CofixBody -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.CofixBody
cofixBodyWithIdent original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.coq.syntax.CofixBody
cofixBodyWithTerm :: Typed.TypedTerm Syntax.CofixBody -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.CofixBody
cofixBodyWithTerm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the type field of hydra.coq.syntax.CofixBody
cofixBodyWithType :: Typed.TypedTerm Syntax.CofixBody -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.CofixBody
cofixBodyWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixBody"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL accessor for the qual field of hydra.coq.syntax.Cofix
cofixQual :: Typed.TypedTerm Syntax.Cofix -> Typed.TypedTerm (Maybe Syntax.CofixQual)
cofixQual x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
        Core.projectionFieldName = (Core.Name "qual")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the in variant of hydra.coq.syntax.CofixQual
cofixQualIn :: Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.CofixQual
cofixQualIn x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.CofixQual"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the with variant of hydra.coq.syntax.CofixQual
cofixQualWith :: Typed.TypedTerm Syntax.CofixWith -> Typed.TypedTerm Syntax.CofixQual
cofixQualWith x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.CofixQual"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "with"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.CofixWith
cofixWith :: Typed.TypedTerm [Syntax.CofixBody] -> Typed.TypedTerm (Maybe Syntax.Ident) -> Typed.TypedTerm Syntax.CofixWith
cofixWith with for =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Typed.unTypedTerm with)},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Typed.unTypedTerm for)}]}))
-- | DSL updater for the body field of hydra.coq.syntax.Cofix
cofixWithBody :: Typed.TypedTerm Syntax.Cofix -> Typed.TypedTerm Syntax.CofixBody -> Typed.TypedTerm Syntax.Cofix
cofixWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
              Core.projectionFieldName = (Core.Name "qual")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL accessor for the for field of hydra.coq.syntax.CofixWith
cofixWithFor :: Typed.TypedTerm Syntax.CofixWith -> Typed.TypedTerm (Maybe Syntax.Ident)
cofixWithFor x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
        Core.projectionFieldName = (Core.Name "for")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the qual field of hydra.coq.syntax.Cofix
cofixWithQual :: Typed.TypedTerm Syntax.Cofix -> Typed.TypedTerm (Maybe Syntax.CofixQual) -> Typed.TypedTerm Syntax.Cofix
cofixWithQual original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Cofix"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qual"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the with field of hydra.coq.syntax.CofixWith
cofixWithWith :: Typed.TypedTerm Syntax.CofixWith -> Typed.TypedTerm [Syntax.CofixBody]
cofixWithWith x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
        Core.projectionFieldName = (Core.Name "with")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the for field of hydra.coq.syntax.CofixWith
cofixWithWithFor :: Typed.TypedTerm Syntax.CofixWith -> Typed.TypedTerm (Maybe Syntax.Ident) -> Typed.TypedTerm Syntax.CofixWith
cofixWithWithFor original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
              Core.projectionFieldName = (Core.Name "with")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the with field of hydra.coq.syntax.CofixWith
cofixWithWithWith :: Typed.TypedTerm Syntax.CofixWith -> Typed.TypedTerm [Syntax.CofixBody] -> Typed.TypedTerm Syntax.CofixWith
cofixWithWithWith original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.CofixWith"),
              Core.projectionFieldName = (Core.Name "for")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.coq.syntax.Comment wrapper
comment :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Comment
comment x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.Comment"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.coq.syntax.Constructor
constructor :: Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm [Syntax.Binder] -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.Constructor
constructor name binders type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the binders field of hydra.coq.syntax.Constructor
constructorBinders :: Typed.TypedTerm Syntax.Constructor -> Typed.TypedTerm [Syntax.Binder]
constructorBinders x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.coq.syntax.Constructor
constructorName :: Typed.TypedTerm Syntax.Constructor -> Typed.TypedTerm Syntax.Ident
constructorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.Constructor
constructorType :: Typed.TypedTerm Syntax.Constructor -> Typed.TypedTerm (Maybe Syntax.Type)
constructorType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the binders field of hydra.coq.syntax.Constructor
constructorWithBinders :: Typed.TypedTerm Syntax.Constructor -> Typed.TypedTerm [Syntax.Binder] -> Typed.TypedTerm Syntax.Constructor
constructorWithBinders original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.coq.syntax.Constructor
constructorWithName :: Typed.TypedTerm Syntax.Constructor -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.Constructor
constructorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.coq.syntax.Constructor
constructorWithType :: Typed.TypedTerm Syntax.Constructor -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.Constructor
constructorWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Constructor"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.Definition
definition :: Typed.TypedTerm (Maybe Syntax.Locality) -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm [Syntax.Binder] -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.Definition
definition locality name binders type_ body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Typed.unTypedTerm locality)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the binders field of hydra.coq.syntax.Definition
definitionBinders :: Typed.TypedTerm Syntax.Definition -> Typed.TypedTerm [Syntax.Binder]
definitionBinders x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.coq.syntax.Definition
definitionBody :: Typed.TypedTerm Syntax.Definition -> Typed.TypedTerm Syntax.Term
definitionBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the locality field of hydra.coq.syntax.Definition
definitionLocality :: Typed.TypedTerm Syntax.Definition -> Typed.TypedTerm (Maybe Syntax.Locality)
definitionLocality x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
        Core.projectionFieldName = (Core.Name "locality")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.coq.syntax.Definition
definitionName :: Typed.TypedTerm Syntax.Definition -> Typed.TypedTerm Syntax.Ident
definitionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.Definition
definitionType :: Typed.TypedTerm Syntax.Definition -> Typed.TypedTerm (Maybe Syntax.Type)
definitionType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the binders field of hydra.coq.syntax.Definition
definitionWithBinders :: Typed.TypedTerm Syntax.Definition -> Typed.TypedTerm [Syntax.Binder] -> Typed.TypedTerm Syntax.Definition
definitionWithBinders original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.coq.syntax.Definition
definitionWithBody :: Typed.TypedTerm Syntax.Definition -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.Definition
definitionWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the locality field of hydra.coq.syntax.Definition
definitionWithLocality :: Typed.TypedTerm Syntax.Definition -> Typed.TypedTerm (Maybe Syntax.Locality) -> Typed.TypedTerm Syntax.Definition
definitionWithLocality original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.coq.syntax.Definition
definitionWithName :: Typed.TypedTerm Syntax.Definition -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.Definition
definitionWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.coq.syntax.Definition
definitionWithType :: Typed.TypedTerm Syntax.Definition -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.Definition
definitionWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Definition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Definition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.Document
document :: Typed.TypedTerm [Syntax.Sentence] -> Typed.TypedTerm Syntax.Document
document sentences =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Document"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Typed.unTypedTerm sentences)}]}))
-- | DSL accessor for the sentences field of hydra.coq.syntax.Document
documentSentences :: Typed.TypedTerm Syntax.Document -> Typed.TypedTerm [Syntax.Sentence]
documentSentences x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Document"),
        Core.projectionFieldName = (Core.Name "sentences")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the sentences field of hydra.coq.syntax.Document
documentWithSentences :: Typed.TypedTerm Syntax.Document -> Typed.TypedTerm [Syntax.Sentence] -> Typed.TypedTerm Syntax.Document
documentWithSentences original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Document"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.Equation
equation :: Typed.TypedTerm [[Syntax.Pattern]] -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.Equation
equation pattern term =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Equation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm term)}]}))
-- | DSL accessor for the pattern field of hydra.coq.syntax.Equation
equationPattern :: Typed.TypedTerm Syntax.Equation -> Typed.TypedTerm [[Syntax.Pattern]]
equationPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Equation"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.Equation
equationTerm :: Typed.TypedTerm Syntax.Equation -> Typed.TypedTerm Syntax.Term
equationTerm x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Equation"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the pattern field of hydra.coq.syntax.Equation
equationWithPattern :: Typed.TypedTerm Syntax.Equation -> Typed.TypedTerm [[Syntax.Pattern]] -> Typed.TypedTerm Syntax.Equation
equationWithPattern original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Equation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Equation"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.coq.syntax.Equation
equationWithTerm :: Typed.TypedTerm Syntax.Equation -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.Equation
equationWithTerm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Equation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Equation"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.ExistentialVariable
existentialVariable :: Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.ExistentialVariableVariant -> Typed.TypedTerm Syntax.ExistentialVariable
existentialVariable ident variant =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Typed.unTypedTerm ident)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Typed.unTypedTerm variant)}]}))
-- | DSL accessor for the ident field of hydra.coq.syntax.ExistentialVariable
existentialVariableIdent :: Typed.TypedTerm Syntax.ExistentialVariable -> Typed.TypedTerm Syntax.Ident
existentialVariableIdent x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
        Core.projectionFieldName = (Core.Name "ident")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the variant field of hydra.coq.syntax.ExistentialVariable
existentialVariableVariant :: Typed.TypedTerm Syntax.ExistentialVariable -> Typed.TypedTerm Syntax.ExistentialVariableVariant
existentialVariableVariant x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
        Core.projectionFieldName = (Core.Name "variant")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the inside1 variant of hydra.coq.syntax.ExistentialVariableVariant
existentialVariableVariantInside1 :: Typed.TypedTerm Syntax.ExistentialVariableVariant
existentialVariableVariantInside1 =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariableVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inside1"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the inside2 variant of hydra.coq.syntax.ExistentialVariableVariant
existentialVariableVariantInside2 :: Typed.TypedTerm Syntax.ExistentialVariableVariant
existentialVariableVariantInside2 =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariableVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inside2"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the outside variant of hydra.coq.syntax.ExistentialVariableVariant
existentialVariableVariantOutside :: Typed.TypedTerm (Maybe Syntax.IdentArg) -> Typed.TypedTerm Syntax.ExistentialVariableVariant
existentialVariableVariantOutside x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariableVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "outside"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the placeholder variant of hydra.coq.syntax.ExistentialVariableVariant
existentialVariableVariantPlaceholder :: Typed.TypedTerm Syntax.ExistentialVariableVariant
existentialVariableVariantPlaceholder =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariableVariant"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "placeholder"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL updater for the ident field of hydra.coq.syntax.ExistentialVariable
existentialVariableWithIdent :: Typed.TypedTerm Syntax.ExistentialVariable -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.ExistentialVariable
existentialVariableWithIdent original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
              Core.projectionFieldName = (Core.Name "variant")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the variant field of hydra.coq.syntax.ExistentialVariable
existentialVariableWithVariant :: Typed.TypedTerm Syntax.ExistentialVariable -> Typed.TypedTerm Syntax.ExistentialVariableVariant -> Typed.TypedTerm Syntax.ExistentialVariable
existentialVariableWithVariant original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ExistentialVariable"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variant"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.coq.syntax.FieldIdent wrapper
fieldIdent :: Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.FieldIdent
fieldIdent x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.FieldIdent"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the measure variant of hydra.coq.syntax.FixAnnot
fixAnnotMeasure :: Typed.TypedTerm Syntax.FixAnnot_Measure -> Typed.TypedTerm Syntax.FixAnnot
fixAnnotMeasure x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "measure"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the struct variant of hydra.coq.syntax.FixAnnot
fixAnnotStruct :: Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.FixAnnot
fixAnnotStruct x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "struct"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the wf variant of hydra.coq.syntax.FixAnnot
fixAnnotWf :: Typed.TypedTerm Syntax.FixAnnot_Wf -> Typed.TypedTerm Syntax.FixAnnot
fixAnnotWf x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wf"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.FixAnnot_Measure
fixAnnot_Measure :: Typed.TypedTerm Syntax.OneTerm -> Typed.TypedTerm (Maybe Syntax.Ident) -> Typed.TypedTerm (Maybe Syntax.OneTerm) -> Typed.TypedTerm Syntax.FixAnnot_Measure
fixAnnot_Measure term ident term2 =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Typed.unTypedTerm ident)},
        Core.Field {
          Core.fieldName = (Core.Name "term2"),
          Core.fieldTerm = (Typed.unTypedTerm term2)}]}))
-- | DSL accessor for the ident field of hydra.coq.syntax.FixAnnot_Measure
fixAnnot_MeasureIdent :: Typed.TypedTerm Syntax.FixAnnot_Measure -> Typed.TypedTerm (Maybe Syntax.Ident)
fixAnnot_MeasureIdent x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
        Core.projectionFieldName = (Core.Name "ident")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.FixAnnot_Measure
fixAnnot_MeasureTerm :: Typed.TypedTerm Syntax.FixAnnot_Measure -> Typed.TypedTerm Syntax.OneTerm
fixAnnot_MeasureTerm x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the term2 field of hydra.coq.syntax.FixAnnot_Measure
fixAnnot_MeasureTerm2 :: Typed.TypedTerm Syntax.FixAnnot_Measure -> Typed.TypedTerm (Maybe Syntax.OneTerm)
fixAnnot_MeasureTerm2 x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
        Core.projectionFieldName = (Core.Name "term2")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the ident field of hydra.coq.syntax.FixAnnot_Measure
fixAnnot_MeasureWithIdent :: Typed.TypedTerm Syntax.FixAnnot_Measure -> Typed.TypedTerm (Maybe Syntax.Ident) -> Typed.TypedTerm Syntax.FixAnnot_Measure
fixAnnot_MeasureWithIdent original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
              Core.projectionFieldName = (Core.Name "term2")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.coq.syntax.FixAnnot_Measure
fixAnnot_MeasureWithTerm :: Typed.TypedTerm Syntax.FixAnnot_Measure -> Typed.TypedTerm Syntax.OneTerm -> Typed.TypedTerm Syntax.FixAnnot_Measure
fixAnnot_MeasureWithTerm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
              Core.projectionFieldName = (Core.Name "term2")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the term2 field of hydra.coq.syntax.FixAnnot_Measure
fixAnnot_MeasureWithTerm2 :: Typed.TypedTerm Syntax.FixAnnot_Measure -> Typed.TypedTerm (Maybe Syntax.OneTerm) -> Typed.TypedTerm Syntax.FixAnnot_Measure
fixAnnot_MeasureWithTerm2 original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Measure"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term2"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.FixAnnot_Wf
fixAnnot_Wf :: Typed.TypedTerm Syntax.OneTerm -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.FixAnnot_Wf
fixAnnot_Wf term ident =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Typed.unTypedTerm ident)}]}))
-- | DSL accessor for the ident field of hydra.coq.syntax.FixAnnot_Wf
fixAnnot_WfIdent :: Typed.TypedTerm Syntax.FixAnnot_Wf -> Typed.TypedTerm Syntax.Ident
fixAnnot_WfIdent x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
        Core.projectionFieldName = (Core.Name "ident")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.FixAnnot_Wf
fixAnnot_WfTerm :: Typed.TypedTerm Syntax.FixAnnot_Wf -> Typed.TypedTerm Syntax.OneTerm
fixAnnot_WfTerm x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the ident field of hydra.coq.syntax.FixAnnot_Wf
fixAnnot_WfWithIdent :: Typed.TypedTerm Syntax.FixAnnot_Wf -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.FixAnnot_Wf
fixAnnot_WfWithIdent original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the term field of hydra.coq.syntax.FixAnnot_Wf
fixAnnot_WfWithTerm :: Typed.TypedTerm Syntax.FixAnnot_Wf -> Typed.TypedTerm Syntax.OneTerm -> Typed.TypedTerm Syntax.FixAnnot_Wf
fixAnnot_WfWithTerm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixAnnot_Wf"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the decl variant of hydra.coq.syntax.Fix
fixDecl :: Typed.TypedTerm Syntax.Fix_Decl -> Typed.TypedTerm Syntax.Fix
fixDecl x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Fix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decl"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the qual variant of hydra.coq.syntax.Fix
fixQual :: Typed.TypedTerm (Maybe Syntax.Fix_Qual) -> Typed.TypedTerm Syntax.Fix
fixQual x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Fix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qual"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.FixWith
fixWith :: Typed.TypedTerm [Syntax.Fix_Decl] -> Typed.TypedTerm (Maybe Syntax.Ident) -> Typed.TypedTerm Syntax.FixWith
fixWith decls for =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decls"),
          Core.fieldTerm = (Typed.unTypedTerm decls)},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Typed.unTypedTerm for)}]}))
-- | DSL accessor for the decls field of hydra.coq.syntax.FixWith
fixWithDecls :: Typed.TypedTerm Syntax.FixWith -> Typed.TypedTerm [Syntax.Fix_Decl]
fixWithDecls x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
        Core.projectionFieldName = (Core.Name "decls")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the for field of hydra.coq.syntax.FixWith
fixWithFor :: Typed.TypedTerm Syntax.FixWith -> Typed.TypedTerm (Maybe Syntax.Ident)
fixWithFor x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
        Core.projectionFieldName = (Core.Name "for")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the decls field of hydra.coq.syntax.FixWith
fixWithWithDecls :: Typed.TypedTerm Syntax.FixWith -> Typed.TypedTerm [Syntax.Fix_Decl] -> Typed.TypedTerm Syntax.FixWith
fixWithWithDecls original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decls"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
              Core.projectionFieldName = (Core.Name "for")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the for field of hydra.coq.syntax.FixWith
fixWithWithFor :: Typed.TypedTerm Syntax.FixWith -> Typed.TypedTerm (Maybe Syntax.Ident) -> Typed.TypedTerm Syntax.FixWith
fixWithWithFor original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decls"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixWith"),
              Core.projectionFieldName = (Core.Name "decls")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "for"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.Fix_Decl
fix_Decl :: Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm [Syntax.Binder] -> Typed.TypedTerm (Maybe Syntax.FixAnnot) -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.Fix_Decl
fix_Decl ident binders annot type_ term =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Typed.unTypedTerm ident)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Typed.unTypedTerm annot)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm term)}]}))
-- | DSL accessor for the annot field of hydra.coq.syntax.Fix_Decl
fix_DeclAnnot :: Typed.TypedTerm Syntax.Fix_Decl -> Typed.TypedTerm (Maybe Syntax.FixAnnot)
fix_DeclAnnot x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
        Core.projectionFieldName = (Core.Name "annot")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the binders field of hydra.coq.syntax.Fix_Decl
fix_DeclBinders :: Typed.TypedTerm Syntax.Fix_Decl -> Typed.TypedTerm [Syntax.Binder]
fix_DeclBinders x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the ident field of hydra.coq.syntax.Fix_Decl
fix_DeclIdent :: Typed.TypedTerm Syntax.Fix_Decl -> Typed.TypedTerm Syntax.Ident
fix_DeclIdent x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
        Core.projectionFieldName = (Core.Name "ident")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.Fix_Decl
fix_DeclTerm :: Typed.TypedTerm Syntax.Fix_Decl -> Typed.TypedTerm Syntax.Term
fix_DeclTerm x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.Fix_Decl
fix_DeclType :: Typed.TypedTerm Syntax.Fix_Decl -> Typed.TypedTerm (Maybe Syntax.Type)
fix_DeclType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annot field of hydra.coq.syntax.Fix_Decl
fix_DeclWithAnnot :: Typed.TypedTerm Syntax.Fix_Decl -> Typed.TypedTerm (Maybe Syntax.FixAnnot) -> Typed.TypedTerm Syntax.Fix_Decl
fix_DeclWithAnnot original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the binders field of hydra.coq.syntax.Fix_Decl
fix_DeclWithBinders :: Typed.TypedTerm Syntax.Fix_Decl -> Typed.TypedTerm [Syntax.Binder] -> Typed.TypedTerm Syntax.Fix_Decl
fix_DeclWithBinders original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the ident field of hydra.coq.syntax.Fix_Decl
fix_DeclWithIdent :: Typed.TypedTerm Syntax.Fix_Decl -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.Fix_Decl
fix_DeclWithIdent original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.coq.syntax.Fix_Decl
fix_DeclWithTerm :: Typed.TypedTerm Syntax.Fix_Decl -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.Fix_Decl
fix_DeclWithTerm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the type field of hydra.coq.syntax.Fix_Decl
fix_DeclWithType :: Typed.TypedTerm Syntax.Fix_Decl -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.Fix_Decl
fix_DeclWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Decl"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the in variant of hydra.coq.syntax.Fix_Qual
fix_QualIn :: Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.Fix_Qual
fix_QualIn x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Qual"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the with variant of hydra.coq.syntax.Fix_Qual
fix_QualWith :: Typed.TypedTerm Syntax.FixWith -> Typed.TypedTerm Syntax.Fix_Qual
fix_QualWith x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Fix_Qual"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "with"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.FixpointDefinition
fixpointDefinition :: Typed.TypedTerm (Maybe Syntax.Locality) -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm [Syntax.Binder] -> Typed.TypedTerm (Maybe Syntax.FixAnnot) -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm [Syntax.Fix_Decl] -> Typed.TypedTerm Syntax.FixpointDefinition
fixpointDefinition locality name binders annot type_ body with =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Typed.unTypedTerm locality)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Typed.unTypedTerm annot)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Typed.unTypedTerm with)}]}))
-- | DSL accessor for the annot field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionAnnot :: Typed.TypedTerm Syntax.FixpointDefinition -> Typed.TypedTerm (Maybe Syntax.FixAnnot)
fixpointDefinitionAnnot x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionFieldName = (Core.Name "annot")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the binders field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionBinders :: Typed.TypedTerm Syntax.FixpointDefinition -> Typed.TypedTerm [Syntax.Binder]
fixpointDefinitionBinders x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionBody :: Typed.TypedTerm Syntax.FixpointDefinition -> Typed.TypedTerm Syntax.Term
fixpointDefinitionBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the locality field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionLocality :: Typed.TypedTerm Syntax.FixpointDefinition -> Typed.TypedTerm (Maybe Syntax.Locality)
fixpointDefinitionLocality x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionFieldName = (Core.Name "locality")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionName :: Typed.TypedTerm Syntax.FixpointDefinition -> Typed.TypedTerm Syntax.Ident
fixpointDefinitionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionType :: Typed.TypedTerm Syntax.FixpointDefinition -> Typed.TypedTerm (Maybe Syntax.Type)
fixpointDefinitionType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the with field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionWith :: Typed.TypedTerm Syntax.FixpointDefinition -> Typed.TypedTerm [Syntax.Fix_Decl]
fixpointDefinitionWith x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
        Core.projectionFieldName = (Core.Name "with")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annot field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionWithAnnot :: Typed.TypedTerm Syntax.FixpointDefinition -> Typed.TypedTerm (Maybe Syntax.FixAnnot) -> Typed.TypedTerm Syntax.FixpointDefinition
fixpointDefinitionWithAnnot original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "with")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the binders field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionWithBinders :: Typed.TypedTerm Syntax.FixpointDefinition -> Typed.TypedTerm [Syntax.Binder] -> Typed.TypedTerm Syntax.FixpointDefinition
fixpointDefinitionWithBinders original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "with")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionWithBody :: Typed.TypedTerm Syntax.FixpointDefinition -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.FixpointDefinition
fixpointDefinitionWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "with")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the locality field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionWithLocality :: Typed.TypedTerm Syntax.FixpointDefinition -> Typed.TypedTerm (Maybe Syntax.Locality) -> Typed.TypedTerm Syntax.FixpointDefinition
fixpointDefinitionWithLocality original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "with")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionWithName :: Typed.TypedTerm Syntax.FixpointDefinition -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.FixpointDefinition
fixpointDefinitionWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "with")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionWithType :: Typed.TypedTerm Syntax.FixpointDefinition -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.FixpointDefinition
fixpointDefinitionWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "with")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the with field of hydra.coq.syntax.FixpointDefinition
fixpointDefinitionWithWith :: Typed.TypedTerm Syntax.FixpointDefinition -> Typed.TypedTerm [Syntax.Fix_Decl] -> Typed.TypedTerm Syntax.FixpointDefinition
fixpointDefinitionWithWith original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "annot")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.FixpointDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.Forall
forall_ :: Typed.TypedTerm Syntax.OpenBinders -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Forall
forall_ binders type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Forall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the binders field of hydra.coq.syntax.Forall
forallBinders :: Typed.TypedTerm Syntax.Forall -> Typed.TypedTerm Syntax.OpenBinders
forallBinders x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Forall"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the forall variant of hydra.coq.syntax.ForallOrFun
forallOrFunForall :: Typed.TypedTerm Syntax.Forall -> Typed.TypedTerm Syntax.ForallOrFun
forallOrFunForall x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ForallOrFun"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forall"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the fun variant of hydra.coq.syntax.ForallOrFun
forallOrFunFun :: Typed.TypedTerm Syntax.Fun -> Typed.TypedTerm Syntax.ForallOrFun
forallOrFunFun x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ForallOrFun"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fun"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL accessor for the type field of hydra.coq.syntax.Forall
forallType :: Typed.TypedTerm Syntax.Forall -> Typed.TypedTerm Syntax.Type
forallType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Forall"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the binders field of hydra.coq.syntax.Forall
forallWithBinders :: Typed.TypedTerm Syntax.Forall -> Typed.TypedTerm Syntax.OpenBinders -> Typed.TypedTerm Syntax.Forall
forallWithBinders original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Forall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Forall"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.coq.syntax.Forall
forallWithType :: Typed.TypedTerm Syntax.Forall -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Forall
forallWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Forall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Forall"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.Fun
fun :: Typed.TypedTerm Syntax.OpenBinders -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.Fun
fun binders body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fun"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the binders field of hydra.coq.syntax.Fun
funBinders :: Typed.TypedTerm Syntax.Fun -> Typed.TypedTerm Syntax.OpenBinders
funBinders x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fun"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.coq.syntax.Fun
funBody :: Typed.TypedTerm Syntax.Fun -> Typed.TypedTerm Syntax.Term
funBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fun"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the binders field of hydra.coq.syntax.Fun
funWithBinders :: Typed.TypedTerm Syntax.Fun -> Typed.TypedTerm Syntax.OpenBinders -> Typed.TypedTerm Syntax.Fun
funWithBinders original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fun"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fun"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.coq.syntax.Fun
funWithBody :: Typed.TypedTerm Syntax.Fun -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.Fun
funWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Fun"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Fun"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the explicit variant of hydra.coq.syntax.GeneralizingBinder
generalizingBinderExplicit :: Typed.TypedTerm Syntax.TypeclassConstraint -> Typed.TypedTerm Syntax.GeneralizingBinder
generalizingBinderExplicit x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.GeneralizingBinder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "explicit"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the implicitMaximallyInserted variant of hydra.coq.syntax.GeneralizingBinder
generalizingBinderImplicitMaximallyInserted :: Typed.TypedTerm Syntax.TypeclassConstraint -> Typed.TypedTerm Syntax.GeneralizingBinder
generalizingBinderImplicitMaximallyInserted x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.GeneralizingBinder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicitMaximallyInserted"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the implicitNonMaximallyInserted variant of hydra.coq.syntax.GeneralizingBinder
generalizingBinderImplicitNonMaximallyInserted :: Typed.TypedTerm Syntax.TypeclassConstraint -> Typed.TypedTerm Syntax.GeneralizingBinder
generalizingBinderImplicitNonMaximallyInserted x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.GeneralizingBinder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicitNonMaximallyInserted"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.coq.syntax.Ident wrapper
ident :: Typed.TypedTerm Syntax.String_ -> Typed.TypedTerm Syntax.Ident
ident x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.Ident"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.coq.syntax.IdentArg
identArg :: Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.IdentArg
identArg ident term =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Typed.unTypedTerm ident)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm term)}]}))
-- | DSL accessor for the ident field of hydra.coq.syntax.IdentArg
identArgIdent :: Typed.TypedTerm Syntax.IdentArg -> Typed.TypedTerm Syntax.Ident
identArgIdent x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
        Core.projectionFieldName = (Core.Name "ident")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.IdentArg
identArgTerm :: Typed.TypedTerm Syntax.IdentArg -> Typed.TypedTerm Syntax.Term
identArgTerm x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the ident field of hydra.coq.syntax.IdentArg
identArgWithIdent :: Typed.TypedTerm Syntax.IdentArg -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.IdentArg
identArgWithIdent original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.coq.syntax.IdentArg
identArgWithTerm :: Typed.TypedTerm Syntax.IdentArg -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.IdentArg
identArgWithTerm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ident"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.IdentArg"),
              Core.projectionFieldName = (Core.Name "ident")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.If
if_ :: Typed.TypedTerm Syntax.Term -> Typed.TypedTerm (Maybe Syntax.ReturnAs) -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.If
if_ condition returnAs then_ else_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Typed.unTypedTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Typed.unTypedTerm returnAs)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Typed.unTypedTerm then_)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm else_)}]}))
-- | DSL accessor for the condition field of hydra.coq.syntax.If
ifCondition :: Typed.TypedTerm Syntax.If -> Typed.TypedTerm Syntax.Term
ifCondition x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
        Core.projectionFieldName = (Core.Name "condition")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the else field of hydra.coq.syntax.If
ifElse :: Typed.TypedTerm Syntax.If -> Typed.TypedTerm Syntax.Term
ifElse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the returnAs field of hydra.coq.syntax.If
ifReturnAs :: Typed.TypedTerm Syntax.If -> Typed.TypedTerm (Maybe Syntax.ReturnAs)
ifReturnAs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
        Core.projectionFieldName = (Core.Name "returnAs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the then field of hydra.coq.syntax.If
ifThen :: Typed.TypedTerm Syntax.If -> Typed.TypedTerm Syntax.Term
ifThen x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
        Core.projectionFieldName = (Core.Name "then")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the condition field of hydra.coq.syntax.If
ifWithCondition :: Typed.TypedTerm Syntax.If -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.If
ifWithCondition original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "returnAs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the else field of hydra.coq.syntax.If
ifWithElse :: Typed.TypedTerm Syntax.If -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.If
ifWithElse original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "returnAs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the returnAs field of hydra.coq.syntax.If
ifWithReturnAs :: Typed.TypedTerm Syntax.If -> Typed.TypedTerm (Maybe Syntax.ReturnAs) -> Typed.TypedTerm Syntax.If
ifWithReturnAs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the then field of hydra.coq.syntax.If
ifWithThen :: Typed.TypedTerm Syntax.If -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.If
ifWithThen original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.If"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "returnAs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.If"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the maximallyInserted variant of hydra.coq.syntax.ImplicitBinders
implicitBindersMaximallyInserted :: Typed.TypedTerm Syntax.TypeBinders -> Typed.TypedTerm Syntax.ImplicitBinders
implicitBindersMaximallyInserted x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ImplicitBinders"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maximallyInserted"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the nonMaximallyInserted variant of hydra.coq.syntax.ImplicitBinders
implicitBindersNonMaximallyInserted :: Typed.TypedTerm Syntax.TypeBinders -> Typed.TypedTerm Syntax.ImplicitBinders
implicitBindersNonMaximallyInserted x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ImplicitBinders"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonMaximallyInserted"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the export variant of hydra.coq.syntax.ImportQualification
importQualificationExport :: Typed.TypedTerm Syntax.ImportQualification
importQualificationExport =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ImportQualification"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "export"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the import variant of hydra.coq.syntax.ImportQualification
importQualificationImport :: Typed.TypedTerm Syntax.ImportQualification
importQualificationImport =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.ImportQualification"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "import"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.coq.syntax.InductiveBody
inductiveBody :: Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm [Syntax.Binder] -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm [Syntax.Constructor] -> Typed.TypedTerm Syntax.InductiveBody
inductiveBody name binders type_ constructors =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Typed.unTypedTerm constructors)}]}))
-- | DSL accessor for the binders field of hydra.coq.syntax.InductiveBody
inductiveBodyBinders :: Typed.TypedTerm Syntax.InductiveBody -> Typed.TypedTerm [Syntax.Binder]
inductiveBodyBinders x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the constructors field of hydra.coq.syntax.InductiveBody
inductiveBodyConstructors :: Typed.TypedTerm Syntax.InductiveBody -> Typed.TypedTerm [Syntax.Constructor]
inductiveBodyConstructors x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
        Core.projectionFieldName = (Core.Name "constructors")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.coq.syntax.InductiveBody
inductiveBodyName :: Typed.TypedTerm Syntax.InductiveBody -> Typed.TypedTerm Syntax.Ident
inductiveBodyName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.InductiveBody
inductiveBodyType :: Typed.TypedTerm Syntax.InductiveBody -> Typed.TypedTerm (Maybe Syntax.Type)
inductiveBodyType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the binders field of hydra.coq.syntax.InductiveBody
inductiveBodyWithBinders :: Typed.TypedTerm Syntax.InductiveBody -> Typed.TypedTerm [Syntax.Binder] -> Typed.TypedTerm Syntax.InductiveBody
inductiveBodyWithBinders original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "constructors")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the constructors field of hydra.coq.syntax.InductiveBody
inductiveBodyWithConstructors :: Typed.TypedTerm Syntax.InductiveBody -> Typed.TypedTerm [Syntax.Constructor] -> Typed.TypedTerm Syntax.InductiveBody
inductiveBodyWithConstructors original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.coq.syntax.InductiveBody
inductiveBodyWithName :: Typed.TypedTerm Syntax.InductiveBody -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.InductiveBody
inductiveBodyWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "constructors")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.coq.syntax.InductiveBody
inductiveBodyWithType :: Typed.TypedTerm Syntax.InductiveBody -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.InductiveBody
inductiveBodyWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveBody"),
              Core.projectionFieldName = (Core.Name "constructors")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.InductiveDefinition
inductiveDefinition :: Typed.TypedTerm (Maybe Syntax.Locality) -> Typed.TypedTerm Bool -> Typed.TypedTerm [Syntax.InductiveBody] -> Typed.TypedTerm Syntax.InductiveDefinition
inductiveDefinition locality coinductive bodies =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Typed.unTypedTerm locality)},
        Core.Field {
          Core.fieldName = (Core.Name "coinductive"),
          Core.fieldTerm = (Typed.unTypedTerm coinductive)},
        Core.Field {
          Core.fieldName = (Core.Name "bodies"),
          Core.fieldTerm = (Typed.unTypedTerm bodies)}]}))
-- | DSL accessor for the bodies field of hydra.coq.syntax.InductiveDefinition
inductiveDefinitionBodies :: Typed.TypedTerm Syntax.InductiveDefinition -> Typed.TypedTerm [Syntax.InductiveBody]
inductiveDefinitionBodies x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
        Core.projectionFieldName = (Core.Name "bodies")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the coinductive field of hydra.coq.syntax.InductiveDefinition
inductiveDefinitionCoinductive :: Typed.TypedTerm Syntax.InductiveDefinition -> Typed.TypedTerm Bool
inductiveDefinitionCoinductive x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
        Core.projectionFieldName = (Core.Name "coinductive")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the locality field of hydra.coq.syntax.InductiveDefinition
inductiveDefinitionLocality :: Typed.TypedTerm Syntax.InductiveDefinition -> Typed.TypedTerm (Maybe Syntax.Locality)
inductiveDefinitionLocality x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
        Core.projectionFieldName = (Core.Name "locality")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the bodies field of hydra.coq.syntax.InductiveDefinition
inductiveDefinitionWithBodies :: Typed.TypedTerm Syntax.InductiveDefinition -> Typed.TypedTerm [Syntax.InductiveBody] -> Typed.TypedTerm Syntax.InductiveDefinition
inductiveDefinitionWithBodies original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "coinductive"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
              Core.projectionFieldName = (Core.Name "coinductive")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bodies"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the coinductive field of hydra.coq.syntax.InductiveDefinition
inductiveDefinitionWithCoinductive :: Typed.TypedTerm Syntax.InductiveDefinition -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.InductiveDefinition
inductiveDefinitionWithCoinductive original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "coinductive"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bodies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
              Core.projectionFieldName = (Core.Name "bodies")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the locality field of hydra.coq.syntax.InductiveDefinition
inductiveDefinitionWithLocality :: Typed.TypedTerm Syntax.InductiveDefinition -> Typed.TypedTerm (Maybe Syntax.Locality) -> Typed.TypedTerm Syntax.InductiveDefinition
inductiveDefinitionWithLocality original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "coinductive"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
              Core.projectionFieldName = (Core.Name "coinductive")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bodies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.InductiveDefinition"),
              Core.projectionFieldName = (Core.Name "bodies")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.Let
let_ :: Typed.TypedTerm Syntax.LetBindings -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.Let
let_ bindings in_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Let"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Typed.unTypedTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Typed.unTypedTerm in_)}]}))
-- | DSL constructor for hydra.coq.syntax.LetBinder
letBinder :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.LetBinder
letBinder name type_ term =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm term)}]}))
-- | DSL accessor for the name field of hydra.coq.syntax.LetBinder
letBinderName :: Typed.TypedTerm Syntax.LetBinder -> Typed.TypedTerm Syntax.Name
letBinderName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.LetBinder
letBinderTerm :: Typed.TypedTerm Syntax.LetBinder -> Typed.TypedTerm Syntax.Term
letBinderTerm x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.LetBinder
letBinderType :: Typed.TypedTerm Syntax.LetBinder -> Typed.TypedTerm (Maybe Syntax.Type)
letBinderType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.coq.syntax.LetBinder
letBinderWithName :: Typed.TypedTerm Syntax.LetBinder -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.LetBinder
letBinderWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.coq.syntax.LetBinder
letBinderWithTerm :: Typed.TypedTerm Syntax.LetBinder -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.LetBinder
letBinderWithTerm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the type field of hydra.coq.syntax.LetBinder
letBinderWithType :: Typed.TypedTerm Syntax.LetBinder -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.LetBinder
letBinderWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetBinder"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL accessor for the bindings field of hydra.coq.syntax.Let
letBindings :: Typed.TypedTerm Syntax.Let -> Typed.TypedTerm Syntax.LetBindings
letBindings x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Let"),
        Core.projectionFieldName = (Core.Name "bindings")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the destructuring variant of hydra.coq.syntax.LetBindings
letBindingsDestructuring :: Typed.TypedTerm Syntax.LetDestructuring -> Typed.TypedTerm Syntax.LetBindings
letBindingsDestructuring x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.LetBindings"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "destructuring"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the named variant of hydra.coq.syntax.LetBindings
letBindingsNamed :: Typed.TypedTerm Syntax.LetNamed -> Typed.TypedTerm Syntax.LetBindings
letBindingsNamed x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.LetBindings"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variant1 variant of hydra.coq.syntax.LetDestructuring
letDestructuringVariant1 :: Typed.TypedTerm Syntax.LetDestructuring_Variant1 -> Typed.TypedTerm Syntax.LetDestructuring
letDestructuringVariant1 x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variant1"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variant2 variant of hydra.coq.syntax.LetDestructuring
letDestructuringVariant2 :: Typed.TypedTerm Syntax.LetDestructuring_Variant2 -> Typed.TypedTerm Syntax.LetDestructuring
letDestructuringVariant2 x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variant2"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variant3 variant of hydra.coq.syntax.LetDestructuring
letDestructuringVariant3 :: Typed.TypedTerm Syntax.LetDestructuring_Variant3 -> Typed.TypedTerm Syntax.LetDestructuring
letDestructuringVariant3 x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variant3"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.LetDestructuring_Variant1
letDestructuring_Variant1 :: Typed.TypedTerm [Syntax.Name] -> Typed.TypedTerm (Maybe Syntax.ReturnAs) -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.LetDestructuring_Variant1
letDestructuring_Variant1 names returnAs term =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Typed.unTypedTerm names)},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Typed.unTypedTerm returnAs)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm term)}]}))
-- | DSL accessor for the names field of hydra.coq.syntax.LetDestructuring_Variant1
letDestructuring_Variant1Names :: Typed.TypedTerm Syntax.LetDestructuring_Variant1 -> Typed.TypedTerm [Syntax.Name]
letDestructuring_Variant1Names x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
        Core.projectionFieldName = (Core.Name "names")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the returnAs field of hydra.coq.syntax.LetDestructuring_Variant1
letDestructuring_Variant1ReturnAs :: Typed.TypedTerm Syntax.LetDestructuring_Variant1 -> Typed.TypedTerm (Maybe Syntax.ReturnAs)
letDestructuring_Variant1ReturnAs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
        Core.projectionFieldName = (Core.Name "returnAs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.LetDestructuring_Variant1
letDestructuring_Variant1Term :: Typed.TypedTerm Syntax.LetDestructuring_Variant1 -> Typed.TypedTerm Syntax.Term
letDestructuring_Variant1Term x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the names field of hydra.coq.syntax.LetDestructuring_Variant1
letDestructuring_Variant1WithNames :: Typed.TypedTerm Syntax.LetDestructuring_Variant1 -> Typed.TypedTerm [Syntax.Name] -> Typed.TypedTerm Syntax.LetDestructuring_Variant1
letDestructuring_Variant1WithNames original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionFieldName = (Core.Name "returnAs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the returnAs field of hydra.coq.syntax.LetDestructuring_Variant1
letDestructuring_Variant1WithReturnAs :: Typed.TypedTerm Syntax.LetDestructuring_Variant1 -> Typed.TypedTerm (Maybe Syntax.ReturnAs) -> Typed.TypedTerm Syntax.LetDestructuring_Variant1
letDestructuring_Variant1WithReturnAs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.coq.syntax.LetDestructuring_Variant1
letDestructuring_Variant1WithTerm :: Typed.TypedTerm Syntax.LetDestructuring_Variant1 -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.LetDestructuring_Variant1
letDestructuring_Variant1WithTerm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnAs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant1"),
              Core.projectionFieldName = (Core.Name "returnAs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.LetDestructuring_Variant2
letDestructuring_Variant2 :: Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm (Maybe Syntax.Term100) -> Typed.TypedTerm Syntax.LetDestructuring_Variant2
letDestructuring_Variant2 pattern term return =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Typed.unTypedTerm return)}]}))
-- | DSL accessor for the pattern field of hydra.coq.syntax.LetDestructuring_Variant2
letDestructuring_Variant2Pattern :: Typed.TypedTerm Syntax.LetDestructuring_Variant2 -> Typed.TypedTerm Syntax.Pattern
letDestructuring_Variant2Pattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the return field of hydra.coq.syntax.LetDestructuring_Variant2
letDestructuring_Variant2Return :: Typed.TypedTerm Syntax.LetDestructuring_Variant2 -> Typed.TypedTerm (Maybe Syntax.Term100)
letDestructuring_Variant2Return x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
        Core.projectionFieldName = (Core.Name "return")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.LetDestructuring_Variant2
letDestructuring_Variant2Term :: Typed.TypedTerm Syntax.LetDestructuring_Variant2 -> Typed.TypedTerm Syntax.Term
letDestructuring_Variant2Term x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the pattern field of hydra.coq.syntax.LetDestructuring_Variant2
letDestructuring_Variant2WithPattern :: Typed.TypedTerm Syntax.LetDestructuring_Variant2 -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.LetDestructuring_Variant2
letDestructuring_Variant2WithPattern original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionFieldName = (Core.Name "return")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the return field of hydra.coq.syntax.LetDestructuring_Variant2
letDestructuring_Variant2WithReturn :: Typed.TypedTerm Syntax.LetDestructuring_Variant2 -> Typed.TypedTerm (Maybe Syntax.Term100) -> Typed.TypedTerm Syntax.LetDestructuring_Variant2
letDestructuring_Variant2WithReturn original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the term field of hydra.coq.syntax.LetDestructuring_Variant2
letDestructuring_Variant2WithTerm :: Typed.TypedTerm Syntax.LetDestructuring_Variant2 -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.LetDestructuring_Variant2
letDestructuring_Variant2WithTerm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant2"),
              Core.projectionFieldName = (Core.Name "return")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.LetDestructuring_Variant3
letDestructuring_Variant3 :: Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.Term100 -> Typed.TypedTerm Syntax.LetDestructuring_Variant3
letDestructuring_Variant3 pattern1 pattern2 term return =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern1"),
          Core.fieldTerm = (Typed.unTypedTerm pattern1)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern2"),
          Core.fieldTerm = (Typed.unTypedTerm pattern2)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Typed.unTypedTerm return)}]}))
-- | DSL accessor for the pattern1 field of hydra.coq.syntax.LetDestructuring_Variant3
letDestructuring_Variant3Pattern1 :: Typed.TypedTerm Syntax.LetDestructuring_Variant3 -> Typed.TypedTerm Syntax.Pattern
letDestructuring_Variant3Pattern1 x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
        Core.projectionFieldName = (Core.Name "pattern1")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the pattern2 field of hydra.coq.syntax.LetDestructuring_Variant3
letDestructuring_Variant3Pattern2 :: Typed.TypedTerm Syntax.LetDestructuring_Variant3 -> Typed.TypedTerm Syntax.Pattern
letDestructuring_Variant3Pattern2 x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
        Core.projectionFieldName = (Core.Name "pattern2")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the return field of hydra.coq.syntax.LetDestructuring_Variant3
letDestructuring_Variant3Return :: Typed.TypedTerm Syntax.LetDestructuring_Variant3 -> Typed.TypedTerm Syntax.Term100
letDestructuring_Variant3Return x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
        Core.projectionFieldName = (Core.Name "return")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.LetDestructuring_Variant3
letDestructuring_Variant3Term :: Typed.TypedTerm Syntax.LetDestructuring_Variant3 -> Typed.TypedTerm Syntax.Term
letDestructuring_Variant3Term x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the pattern1 field of hydra.coq.syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithPattern1 :: Typed.TypedTerm Syntax.LetDestructuring_Variant3 -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithPattern1 original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern1"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "pattern2")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "return")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the pattern2 field of hydra.coq.syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithPattern2 :: Typed.TypedTerm Syntax.LetDestructuring_Variant3 -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithPattern2 original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "pattern1")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern2"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "return")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the return field of hydra.coq.syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithReturn :: Typed.TypedTerm Syntax.LetDestructuring_Variant3 -> Typed.TypedTerm Syntax.Term100 -> Typed.TypedTerm Syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithReturn original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "pattern1")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "pattern2")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the term field of hydra.coq.syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithTerm :: Typed.TypedTerm Syntax.LetDestructuring_Variant3 -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.LetDestructuring_Variant3
letDestructuring_Variant3WithTerm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "pattern1")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "pattern2")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetDestructuring_Variant3"),
              Core.projectionFieldName = (Core.Name "return")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL accessor for the in field of hydra.coq.syntax.Let
letIn :: Typed.TypedTerm Syntax.Let -> Typed.TypedTerm Syntax.Term
letIn x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Let"),
        Core.projectionFieldName = (Core.Name "in")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.coq.syntax.LetNamed
letNamed :: Typed.TypedTerm Syntax.LetBinder -> Typed.TypedTerm [Syntax.Binder] -> Typed.TypedTerm Syntax.LetNamed
letNamed binder binders =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binder"),
          Core.fieldTerm = (Typed.unTypedTerm binder)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm binders)}]}))
-- | DSL accessor for the binder field of hydra.coq.syntax.LetNamed
letNamedBinder :: Typed.TypedTerm Syntax.LetNamed -> Typed.TypedTerm Syntax.LetBinder
letNamedBinder x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
        Core.projectionFieldName = (Core.Name "binder")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the binders field of hydra.coq.syntax.LetNamed
letNamedBinders :: Typed.TypedTerm Syntax.LetNamed -> Typed.TypedTerm [Syntax.Binder]
letNamedBinders x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the binder field of hydra.coq.syntax.LetNamed
letNamedWithBinder :: Typed.TypedTerm Syntax.LetNamed -> Typed.TypedTerm Syntax.LetBinder -> Typed.TypedTerm Syntax.LetNamed
letNamedWithBinder original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binder"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the binders field of hydra.coq.syntax.LetNamed
letNamedWithBinders :: Typed.TypedTerm Syntax.LetNamed -> Typed.TypedTerm [Syntax.Binder] -> Typed.TypedTerm Syntax.LetNamed
letNamedWithBinders original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.LetNamed"),
              Core.projectionFieldName = (Core.Name "binder")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the bindings field of hydra.coq.syntax.Let
letWithBindings :: Typed.TypedTerm Syntax.Let -> Typed.TypedTerm Syntax.LetBindings -> Typed.TypedTerm Syntax.Let
letWithBindings original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Let"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Let"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the in field of hydra.coq.syntax.Let
letWithIn :: Typed.TypedTerm Syntax.Let -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.Let
letWithIn original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Let"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Let"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the global variant of hydra.coq.syntax.Locality
localityGlobal :: Typed.TypedTerm Syntax.Locality
localityGlobal =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Locality"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "global"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the local variant of hydra.coq.syntax.Locality
localityLocal :: Typed.TypedTerm Syntax.Locality
localityLocal =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Locality"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "local"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.coq.syntax.Match
match :: Typed.TypedTerm [Syntax.CaseItem] -> Typed.TypedTerm (Maybe Syntax.Term100) -> Typed.TypedTerm Bool -> Typed.TypedTerm [Syntax.Equation] -> Typed.TypedTerm Syntax.Match
match caseItems return pipe equations =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "caseItems"),
          Core.fieldTerm = (Typed.unTypedTerm caseItems)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Typed.unTypedTerm return)},
        Core.Field {
          Core.fieldName = (Core.Name "pipe"),
          Core.fieldTerm = (Typed.unTypedTerm pipe)},
        Core.Field {
          Core.fieldName = (Core.Name "equations"),
          Core.fieldTerm = (Typed.unTypedTerm equations)}]}))
-- | DSL accessor for the caseItems field of hydra.coq.syntax.Match
matchCaseItems :: Typed.TypedTerm Syntax.Match -> Typed.TypedTerm [Syntax.CaseItem]
matchCaseItems x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
        Core.projectionFieldName = (Core.Name "caseItems")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the equations field of hydra.coq.syntax.Match
matchEquations :: Typed.TypedTerm Syntax.Match -> Typed.TypedTerm [Syntax.Equation]
matchEquations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
        Core.projectionFieldName = (Core.Name "equations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the pipe field of hydra.coq.syntax.Match
matchPipe :: Typed.TypedTerm Syntax.Match -> Typed.TypedTerm Bool
matchPipe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
        Core.projectionFieldName = (Core.Name "pipe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the return field of hydra.coq.syntax.Match
matchReturn :: Typed.TypedTerm Syntax.Match -> Typed.TypedTerm (Maybe Syntax.Term100)
matchReturn x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
        Core.projectionFieldName = (Core.Name "return")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the caseItems field of hydra.coq.syntax.Match
matchWithCaseItems :: Typed.TypedTerm Syntax.Match -> Typed.TypedTerm [Syntax.CaseItem] -> Typed.TypedTerm Syntax.Match
matchWithCaseItems original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "caseItems"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "return")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pipe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "pipe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "equations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "equations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the equations field of hydra.coq.syntax.Match
matchWithEquations :: Typed.TypedTerm Syntax.Match -> Typed.TypedTerm [Syntax.Equation] -> Typed.TypedTerm Syntax.Match
matchWithEquations original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "caseItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "caseItems")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "return")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pipe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "pipe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "equations"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the pipe field of hydra.coq.syntax.Match
matchWithPipe :: Typed.TypedTerm Syntax.Match -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.Match
matchWithPipe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "caseItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "caseItems")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "return")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pipe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "equations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "equations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the return field of hydra.coq.syntax.Match
matchWithReturn :: Typed.TypedTerm Syntax.Match -> Typed.TypedTerm (Maybe Syntax.Term100) -> Typed.TypedTerm Syntax.Match
matchWithReturn original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "caseItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "caseItems")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pipe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "pipe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "equations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Match"),
              Core.projectionFieldName = (Core.Name "equations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.ModuleDefinition
moduleDefinition :: Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm [Syntax.Sentence] -> Typed.TypedTerm Syntax.ModuleDefinition
moduleDefinition name sentences =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Typed.unTypedTerm sentences)}]}))
-- | DSL accessor for the name field of hydra.coq.syntax.ModuleDefinition
moduleDefinitionName :: Typed.TypedTerm Syntax.ModuleDefinition -> Typed.TypedTerm Syntax.Ident
moduleDefinitionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the sentences field of hydra.coq.syntax.ModuleDefinition
moduleDefinitionSentences :: Typed.TypedTerm Syntax.ModuleDefinition -> Typed.TypedTerm [Syntax.Sentence]
moduleDefinitionSentences x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
        Core.projectionFieldName = (Core.Name "sentences")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.coq.syntax.ModuleDefinition
moduleDefinitionWithName :: Typed.TypedTerm Syntax.ModuleDefinition -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.ModuleDefinition
moduleDefinitionWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
              Core.projectionFieldName = (Core.Name "sentences")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the sentences field of hydra.coq.syntax.ModuleDefinition
moduleDefinitionWithSentences :: Typed.TypedTerm Syntax.ModuleDefinition -> Typed.TypedTerm [Syntax.Sentence] -> Typed.TypedTerm Syntax.ModuleDefinition
moduleDefinitionWithSentences original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ModuleDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.coq.syntax.Name wrapper
name :: Typed.TypedTerm (Maybe Syntax.Ident) -> Typed.TypedTerm Syntax.Name
name x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.Name"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.coq.syntax.Natural wrapper
natural :: Typed.TypedTerm Integer -> Typed.TypedTerm Syntax.Natural
natural x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.Natural"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.coq.syntax.NaturalArg
naturalArg :: Typed.TypedTerm Syntax.Natural -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.NaturalArg
naturalArg natural term =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "natural"),
          Core.fieldTerm = (Typed.unTypedTerm natural)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm term)}]}))
-- | DSL accessor for the natural field of hydra.coq.syntax.NaturalArg
naturalArgNatural :: Typed.TypedTerm Syntax.NaturalArg -> Typed.TypedTerm Syntax.Natural
naturalArgNatural x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
        Core.projectionFieldName = (Core.Name "natural")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.NaturalArg
naturalArgTerm :: Typed.TypedTerm Syntax.NaturalArg -> Typed.TypedTerm Syntax.Term
naturalArgTerm x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the natural field of hydra.coq.syntax.NaturalArg
naturalArgWithNatural :: Typed.TypedTerm Syntax.NaturalArg -> Typed.TypedTerm Syntax.Natural -> Typed.TypedTerm Syntax.NaturalArg
naturalArgWithNatural original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "natural"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.coq.syntax.NaturalArg
naturalArgWithTerm :: Typed.TypedTerm Syntax.NaturalArg -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.NaturalArg
naturalArgWithTerm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "natural"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NaturalArg"),
              Core.projectionFieldName = (Core.Name "natural")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.NormalApplication
normalApplication :: Typed.TypedTerm Syntax.Term1 -> Typed.TypedTerm [Syntax.Arg] -> Typed.TypedTerm Syntax.NormalApplication
normalApplication lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.coq.syntax.NormalApplication
normalApplicationLhs :: Typed.TypedTerm Syntax.NormalApplication -> Typed.TypedTerm Syntax.Term1
normalApplicationLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.coq.syntax.NormalApplication
normalApplicationRhs :: Typed.TypedTerm Syntax.NormalApplication -> Typed.TypedTerm [Syntax.Arg]
normalApplicationRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.coq.syntax.NormalApplication
normalApplicationWithLhs :: Typed.TypedTerm Syntax.NormalApplication -> Typed.TypedTerm Syntax.Term1 -> Typed.TypedTerm Syntax.NormalApplication
normalApplicationWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.coq.syntax.NormalApplication
normalApplicationWithRhs :: Typed.TypedTerm Syntax.NormalApplication -> Typed.TypedTerm [Syntax.Arg] -> Typed.TypedTerm Syntax.NormalApplication
normalApplicationWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NormalApplication"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.NotationDeclaration
notationDeclaration :: Typed.TypedTerm Syntax.String_ -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm (Maybe Syntax.Natural) -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.NotationDeclaration
notationDeclaration notation definition level associativity =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "notation"),
          Core.fieldTerm = (Typed.unTypedTerm notation)},
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Typed.unTypedTerm definition)},
        Core.Field {
          Core.fieldName = (Core.Name "level"),
          Core.fieldTerm = (Typed.unTypedTerm level)},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Typed.unTypedTerm associativity)}]}))
-- | DSL accessor for the associativity field of hydra.coq.syntax.NotationDeclaration
notationDeclarationAssociativity :: Typed.TypedTerm Syntax.NotationDeclaration -> Typed.TypedTerm (Maybe String)
notationDeclarationAssociativity x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
        Core.projectionFieldName = (Core.Name "associativity")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the definition field of hydra.coq.syntax.NotationDeclaration
notationDeclarationDefinition :: Typed.TypedTerm Syntax.NotationDeclaration -> Typed.TypedTerm Syntax.Term
notationDeclarationDefinition x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
        Core.projectionFieldName = (Core.Name "definition")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the level field of hydra.coq.syntax.NotationDeclaration
notationDeclarationLevel :: Typed.TypedTerm Syntax.NotationDeclaration -> Typed.TypedTerm (Maybe Syntax.Natural)
notationDeclarationLevel x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
        Core.projectionFieldName = (Core.Name "level")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the notation field of hydra.coq.syntax.NotationDeclaration
notationDeclarationNotation :: Typed.TypedTerm Syntax.NotationDeclaration -> Typed.TypedTerm Syntax.String_
notationDeclarationNotation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
        Core.projectionFieldName = (Core.Name "notation")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the associativity field of hydra.coq.syntax.NotationDeclaration
notationDeclarationWithAssociativity :: Typed.TypedTerm Syntax.NotationDeclaration -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.NotationDeclaration
notationDeclarationWithAssociativity original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "notation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "notation")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "definition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "level"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "level")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the definition field of hydra.coq.syntax.NotationDeclaration
notationDeclarationWithDefinition :: Typed.TypedTerm Syntax.NotationDeclaration -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.NotationDeclaration
notationDeclarationWithDefinition original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "notation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "notation")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "level"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "level")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "associativity")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the level field of hydra.coq.syntax.NotationDeclaration
notationDeclarationWithLevel :: Typed.TypedTerm Syntax.NotationDeclaration -> Typed.TypedTerm (Maybe Syntax.Natural) -> Typed.TypedTerm Syntax.NotationDeclaration
notationDeclarationWithLevel original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "notation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "notation")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "definition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "level"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "associativity")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the notation field of hydra.coq.syntax.NotationDeclaration
notationDeclarationWithNotation :: Typed.TypedTerm Syntax.NotationDeclaration -> Typed.TypedTerm Syntax.String_ -> Typed.TypedTerm Syntax.NotationDeclaration
notationDeclarationWithNotation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "notation"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "definition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "level"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "level")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.NotationDeclaration"),
              Core.projectionFieldName = (Core.Name "associativity")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.coq.syntax.Number wrapper
number :: Typed.TypedTerm Double -> Typed.TypedTerm Syntax.Number
number x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.Number"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the explicit variant of hydra.coq.syntax.OneTerm
oneTermExplicit :: Typed.TypedTerm Syntax.QualidAnnotated -> Typed.TypedTerm Syntax.OneTerm
oneTermExplicit x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.OneTerm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "explicit"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the term1 variant of hydra.coq.syntax.OneTerm
oneTermTerm1 :: Typed.TypedTerm Syntax.Term1 -> Typed.TypedTerm Syntax.OneTerm
oneTermTerm1 x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.OneTerm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term1"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the binders variant of hydra.coq.syntax.OpenBinders
openBindersBinders :: Typed.TypedTerm [Syntax.Binder] -> Typed.TypedTerm Syntax.OpenBinders
openBindersBinders x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.OpenBinders"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binders"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.coq.syntax.OpenBinders
openBindersType :: Typed.TypedTerm Syntax.TypeBinders -> Typed.TypedTerm Syntax.OpenBinders
openBindersType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.OpenBinders"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the number variant of hydra.coq.syntax.Pattern0
pattern0Number :: Typed.TypedTerm Syntax.Number -> Typed.TypedTerm Syntax.Pattern0
pattern0Number x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the parens variant of hydra.coq.syntax.Pattern0
pattern0Parens :: Typed.TypedTerm [Syntax.Pattern] -> Typed.TypedTerm Syntax.Pattern0
pattern0Parens x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the placeholder variant of hydra.coq.syntax.Pattern0
pattern0Placeholder :: Typed.TypedTerm Syntax.Pattern0
pattern0Placeholder =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "placeholder"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the qualIdAndPattern variant of hydra.coq.syntax.Pattern0
pattern0QualIdAndPattern :: Typed.TypedTerm Syntax.QualidAndPattern -> Typed.TypedTerm Syntax.Pattern0
pattern0QualIdAndPattern x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualIdAndPattern"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the qualid variant of hydra.coq.syntax.Pattern0
pattern0Qualid :: Typed.TypedTerm Syntax.Qualid -> Typed.TypedTerm Syntax.Pattern0
pattern0Qualid x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualid"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the string variant of hydra.coq.syntax.Pattern0
pattern0String :: Typed.TypedTerm Syntax.String_ -> Typed.TypedTerm Syntax.Pattern0
pattern0String x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.Pattern1
pattern1 :: Typed.TypedTerm Syntax.Pattern0 -> Typed.TypedTerm (Maybe Syntax.ScopeKey) -> Typed.TypedTerm Syntax.Pattern1
pattern1 pattern scope =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Typed.unTypedTerm scope)}]}))
-- | DSL injection for the as variant of hydra.coq.syntax.Pattern10
pattern10As :: Typed.TypedTerm Syntax.Pattern10_As -> Typed.TypedTerm Syntax.Pattern10
pattern10As x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "as"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the patterns variant of hydra.coq.syntax.Pattern10
pattern10Patterns :: Typed.TypedTerm Syntax.Pattern10_Patterns -> Typed.TypedTerm Syntax.Pattern10
pattern10Patterns x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "patterns"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the qualiid variant of hydra.coq.syntax.Pattern10
pattern10Qualiid :: Typed.TypedTerm Syntax.Pattern10_Qualid -> Typed.TypedTerm Syntax.Pattern10
pattern10Qualiid x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualiid"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.Pattern10_As
pattern10_As :: Typed.TypedTerm Syntax.Pattern1 -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Pattern10_As
pattern10_As pattern as =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Typed.unTypedTerm as)}]}))
-- | DSL accessor for the as field of hydra.coq.syntax.Pattern10_As
pattern10_AsAs :: Typed.TypedTerm Syntax.Pattern10_As -> Typed.TypedTerm Syntax.Name
pattern10_AsAs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the pattern field of hydra.coq.syntax.Pattern10_As
pattern10_AsPattern :: Typed.TypedTerm Syntax.Pattern10_As -> Typed.TypedTerm Syntax.Pattern1
pattern10_AsPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the as field of hydra.coq.syntax.Pattern10_As
pattern10_AsWithAs :: Typed.TypedTerm Syntax.Pattern10_As -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Pattern10_As
pattern10_AsWithAs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the pattern field of hydra.coq.syntax.Pattern10_As
pattern10_AsWithPattern :: Typed.TypedTerm Syntax.Pattern10_As -> Typed.TypedTerm Syntax.Pattern1 -> Typed.TypedTerm Syntax.Pattern10_As
pattern10_AsWithPattern original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_As"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.Pattern10_Patterns
pattern10_Patterns :: Typed.TypedTerm Syntax.Pattern1 -> Typed.TypedTerm [Syntax.Pattern1] -> Typed.TypedTerm Syntax.Pattern10_Patterns
pattern10_Patterns pattern patterns =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Typed.unTypedTerm patterns)}]}))
-- | DSL accessor for the pattern field of hydra.coq.syntax.Pattern10_Patterns
pattern10_PatternsPattern :: Typed.TypedTerm Syntax.Pattern10_Patterns -> Typed.TypedTerm Syntax.Pattern1
pattern10_PatternsPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the patterns field of hydra.coq.syntax.Pattern10_Patterns
pattern10_PatternsPatterns :: Typed.TypedTerm Syntax.Pattern10_Patterns -> Typed.TypedTerm [Syntax.Pattern1]
pattern10_PatternsPatterns x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
        Core.projectionFieldName = (Core.Name "patterns")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the pattern field of hydra.coq.syntax.Pattern10_Patterns
pattern10_PatternsWithPattern :: Typed.TypedTerm Syntax.Pattern10_Patterns -> Typed.TypedTerm Syntax.Pattern1 -> Typed.TypedTerm Syntax.Pattern10_Patterns
pattern10_PatternsWithPattern original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
              Core.projectionFieldName = (Core.Name "patterns")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the patterns field of hydra.coq.syntax.Pattern10_Patterns
pattern10_PatternsWithPatterns :: Typed.TypedTerm Syntax.Pattern10_Patterns -> Typed.TypedTerm [Syntax.Pattern1] -> Typed.TypedTerm Syntax.Pattern10_Patterns
pattern10_PatternsWithPatterns original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Patterns"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.Pattern10_Qualid
pattern10_Qualid :: Typed.TypedTerm Syntax.Qualid -> Typed.TypedTerm [Syntax.Pattern1] -> Typed.TypedTerm Syntax.Pattern10_Qualid
pattern10_Qualid qualid patterns =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Typed.unTypedTerm qualid)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Typed.unTypedTerm patterns)}]}))
-- | DSL accessor for the patterns field of hydra.coq.syntax.Pattern10_Qualid
pattern10_QualidPatterns :: Typed.TypedTerm Syntax.Pattern10_Qualid -> Typed.TypedTerm [Syntax.Pattern1]
pattern10_QualidPatterns x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
        Core.projectionFieldName = (Core.Name "patterns")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the qualid field of hydra.coq.syntax.Pattern10_Qualid
pattern10_QualidQualid :: Typed.TypedTerm Syntax.Pattern10_Qualid -> Typed.TypedTerm Syntax.Qualid
pattern10_QualidQualid x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
        Core.projectionFieldName = (Core.Name "qualid")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the patterns field of hydra.coq.syntax.Pattern10_Qualid
pattern10_QualidWithPatterns :: Typed.TypedTerm Syntax.Pattern10_Qualid -> Typed.TypedTerm [Syntax.Pattern1] -> Typed.TypedTerm Syntax.Pattern10_Qualid
pattern10_QualidWithPatterns original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
              Core.projectionFieldName = (Core.Name "qualid")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the qualid field of hydra.coq.syntax.Pattern10_Qualid
pattern10_QualidWithQualid :: Typed.TypedTerm Syntax.Pattern10_Qualid -> Typed.TypedTerm Syntax.Qualid -> Typed.TypedTerm Syntax.Pattern10_Qualid
pattern10_QualidWithQualid original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern10_Qualid"),
              Core.projectionFieldName = (Core.Name "patterns")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL accessor for the pattern field of hydra.coq.syntax.Pattern1
pattern1Pattern :: Typed.TypedTerm Syntax.Pattern1 -> Typed.TypedTerm Syntax.Pattern0
pattern1Pattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the scope field of hydra.coq.syntax.Pattern1
pattern1Scope :: Typed.TypedTerm Syntax.Pattern1 -> Typed.TypedTerm (Maybe Syntax.ScopeKey)
pattern1Scope x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
        Core.projectionFieldName = (Core.Name "scope")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the pattern field of hydra.coq.syntax.Pattern1
pattern1WithPattern :: Typed.TypedTerm Syntax.Pattern1 -> Typed.TypedTerm Syntax.Pattern0 -> Typed.TypedTerm Syntax.Pattern1
pattern1WithPattern original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
              Core.projectionFieldName = (Core.Name "scope")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the scope field of hydra.coq.syntax.Pattern1
pattern1WithScope :: Typed.TypedTerm Syntax.Pattern1 -> Typed.TypedTerm (Maybe Syntax.ScopeKey) -> Typed.TypedTerm Syntax.Pattern1
pattern1WithScope original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Pattern1"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the pattern variant of hydra.coq.syntax.Pattern
patternPattern :: Typed.TypedTerm Syntax.Pattern10 -> Typed.TypedTerm Syntax.Pattern
patternPattern x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the term variant of hydra.coq.syntax.Pattern
patternTerm :: Typed.TypedTerm (Maybe Syntax.Term) -> Typed.TypedTerm Syntax.Pattern
patternTerm x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the number variant of hydra.coq.syntax.PrimitiveNotations
primitiveNotationsNumber :: Typed.TypedTerm Syntax.Number -> Typed.TypedTerm Syntax.PrimitiveNotations
primitiveNotationsNumber x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.PrimitiveNotations"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the string variant of hydra.coq.syntax.PrimitiveNotations
primitiveNotationsString :: Typed.TypedTerm Syntax.String_ -> Typed.TypedTerm Syntax.PrimitiveNotations
primitiveNotationsString x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.PrimitiveNotations"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.Qualid
qualid :: Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm [Syntax.FieldIdent] -> Typed.TypedTerm Syntax.Qualid
qualid id fieldIds =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "fieldIds"),
          Core.fieldTerm = (Typed.unTypedTerm fieldIds)}]}))
-- | DSL constructor for hydra.coq.syntax.QualidAndPattern
qualidAndPattern :: Typed.TypedTerm Syntax.Qualid -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.QualidAndPattern
qualidAndPattern qualid pattern =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Typed.unTypedTerm qualid)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm pattern)}]}))
-- | DSL accessor for the pattern field of hydra.coq.syntax.QualidAndPattern
qualidAndPatternPattern :: Typed.TypedTerm Syntax.QualidAndPattern -> Typed.TypedTerm Syntax.Pattern
qualidAndPatternPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the qualid field of hydra.coq.syntax.QualidAndPattern
qualidAndPatternQualid :: Typed.TypedTerm Syntax.QualidAndPattern -> Typed.TypedTerm Syntax.Qualid
qualidAndPatternQualid x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
        Core.projectionFieldName = (Core.Name "qualid")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the pattern field of hydra.coq.syntax.QualidAndPattern
qualidAndPatternWithPattern :: Typed.TypedTerm Syntax.QualidAndPattern -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.QualidAndPattern
qualidAndPatternWithPattern original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
              Core.projectionFieldName = (Core.Name "qualid")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the qualid field of hydra.coq.syntax.QualidAndPattern
qualidAndPatternWithQualid :: Typed.TypedTerm Syntax.QualidAndPattern -> Typed.TypedTerm Syntax.Qualid -> Typed.TypedTerm Syntax.QualidAndPattern
qualidAndPatternWithQualid original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAndPattern"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.QualidAnnotated
qualidAnnotated :: Typed.TypedTerm Syntax.Qualid -> Typed.TypedTerm (Maybe Syntax.UnivAnnot) -> Typed.TypedTerm Syntax.QualidAnnotated
qualidAnnotated qualid univAnnot =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Typed.unTypedTerm qualid)},
        Core.Field {
          Core.fieldName = (Core.Name "univAnnot"),
          Core.fieldTerm = (Typed.unTypedTerm univAnnot)}]}))
-- | DSL accessor for the qualid field of hydra.coq.syntax.QualidAnnotated
qualidAnnotatedQualid :: Typed.TypedTerm Syntax.QualidAnnotated -> Typed.TypedTerm Syntax.Qualid
qualidAnnotatedQualid x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
        Core.projectionFieldName = (Core.Name "qualid")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the univAnnot field of hydra.coq.syntax.QualidAnnotated
qualidAnnotatedUnivAnnot :: Typed.TypedTerm Syntax.QualidAnnotated -> Typed.TypedTerm (Maybe Syntax.UnivAnnot)
qualidAnnotatedUnivAnnot x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
        Core.projectionFieldName = (Core.Name "univAnnot")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the qualid field of hydra.coq.syntax.QualidAnnotated
qualidAnnotatedWithQualid :: Typed.TypedTerm Syntax.QualidAnnotated -> Typed.TypedTerm Syntax.Qualid -> Typed.TypedTerm Syntax.QualidAnnotated
qualidAnnotatedWithQualid original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "univAnnot"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
              Core.projectionFieldName = (Core.Name "univAnnot")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the univAnnot field of hydra.coq.syntax.QualidAnnotated
qualidAnnotatedWithUnivAnnot :: Typed.TypedTerm Syntax.QualidAnnotated -> Typed.TypedTerm (Maybe Syntax.UnivAnnot) -> Typed.TypedTerm Syntax.QualidAnnotated
qualidAnnotatedWithUnivAnnot original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualid"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.QualidAnnotated"),
              Core.projectionFieldName = (Core.Name "qualid")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "univAnnot"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the fieldIds field of hydra.coq.syntax.Qualid
qualidFieldIds :: Typed.TypedTerm Syntax.Qualid -> Typed.TypedTerm [Syntax.FieldIdent]
qualidFieldIds x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
        Core.projectionFieldName = (Core.Name "fieldIds")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the id field of hydra.coq.syntax.Qualid
qualidId :: Typed.TypedTerm Syntax.Qualid -> Typed.TypedTerm Syntax.Ident
qualidId x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the fieldIds field of hydra.coq.syntax.Qualid
qualidWithFieldIds :: Typed.TypedTerm Syntax.Qualid -> Typed.TypedTerm [Syntax.FieldIdent] -> Typed.TypedTerm Syntax.Qualid
qualidWithFieldIds original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fieldIds"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the id field of hydra.coq.syntax.Qualid
qualidWithId :: Typed.TypedTerm Syntax.Qualid -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.Qualid
qualidWithId original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fieldIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Qualid"),
              Core.projectionFieldName = (Core.Name "fieldIds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.RecordBody
recordBody :: Typed.TypedTerm (Maybe Syntax.Ident) -> Typed.TypedTerm [Syntax.RecordField] -> Typed.TypedTerm Syntax.RecordBody
recordBody constructor fields =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Typed.unTypedTerm constructor)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Typed.unTypedTerm fields)}]}))
-- | DSL accessor for the constructor field of hydra.coq.syntax.RecordBody
recordBodyConstructor :: Typed.TypedTerm Syntax.RecordBody -> Typed.TypedTerm (Maybe Syntax.Ident)
recordBodyConstructor x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
        Core.projectionFieldName = (Core.Name "constructor")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the fields field of hydra.coq.syntax.RecordBody
recordBodyFields :: Typed.TypedTerm Syntax.RecordBody -> Typed.TypedTerm [Syntax.RecordField]
recordBodyFields x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
        Core.projectionFieldName = (Core.Name "fields")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the constructor field of hydra.coq.syntax.RecordBody
recordBodyWithConstructor :: Typed.TypedTerm Syntax.RecordBody -> Typed.TypedTerm (Maybe Syntax.Ident) -> Typed.TypedTerm Syntax.RecordBody
recordBodyWithConstructor original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the fields field of hydra.coq.syntax.RecordBody
recordBodyWithFields :: Typed.TypedTerm Syntax.RecordBody -> Typed.TypedTerm [Syntax.RecordField] -> Typed.TypedTerm Syntax.RecordBody
recordBodyWithFields original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordBody"),
              Core.projectionFieldName = (Core.Name "constructor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.RecordDefinition
recordDefinition :: Typed.TypedTerm (Maybe Syntax.Locality) -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm [Syntax.Binder] -> Typed.TypedTerm (Maybe Syntax.Sort) -> Typed.TypedTerm Syntax.RecordBody -> Typed.TypedTerm Syntax.RecordDefinition
recordDefinition locality name binders sort body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Typed.unTypedTerm locality)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Typed.unTypedTerm sort)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the binders field of hydra.coq.syntax.RecordDefinition
recordDefinitionBinders :: Typed.TypedTerm Syntax.RecordDefinition -> Typed.TypedTerm [Syntax.Binder]
recordDefinitionBinders x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.coq.syntax.RecordDefinition
recordDefinitionBody :: Typed.TypedTerm Syntax.RecordDefinition -> Typed.TypedTerm Syntax.RecordBody
recordDefinitionBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the locality field of hydra.coq.syntax.RecordDefinition
recordDefinitionLocality :: Typed.TypedTerm Syntax.RecordDefinition -> Typed.TypedTerm (Maybe Syntax.Locality)
recordDefinitionLocality x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
        Core.projectionFieldName = (Core.Name "locality")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.coq.syntax.RecordDefinition
recordDefinitionName :: Typed.TypedTerm Syntax.RecordDefinition -> Typed.TypedTerm Syntax.Ident
recordDefinitionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the sort field of hydra.coq.syntax.RecordDefinition
recordDefinitionSort :: Typed.TypedTerm Syntax.RecordDefinition -> Typed.TypedTerm (Maybe Syntax.Sort)
recordDefinitionSort x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
        Core.projectionFieldName = (Core.Name "sort")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the binders field of hydra.coq.syntax.RecordDefinition
recordDefinitionWithBinders :: Typed.TypedTerm Syntax.RecordDefinition -> Typed.TypedTerm [Syntax.Binder] -> Typed.TypedTerm Syntax.RecordDefinition
recordDefinitionWithBinders original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "sort")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.coq.syntax.RecordDefinition
recordDefinitionWithBody :: Typed.TypedTerm Syntax.RecordDefinition -> Typed.TypedTerm Syntax.RecordBody -> Typed.TypedTerm Syntax.RecordDefinition
recordDefinitionWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "sort")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the locality field of hydra.coq.syntax.RecordDefinition
recordDefinitionWithLocality :: Typed.TypedTerm Syntax.RecordDefinition -> Typed.TypedTerm (Maybe Syntax.Locality) -> Typed.TypedTerm Syntax.RecordDefinition
recordDefinitionWithLocality original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "sort")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.coq.syntax.RecordDefinition
recordDefinitionWithName :: Typed.TypedTerm Syntax.RecordDefinition -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.RecordDefinition
recordDefinitionWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "sort")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the sort field of hydra.coq.syntax.RecordDefinition
recordDefinitionWithSort :: Typed.TypedTerm Syntax.RecordDefinition -> Typed.TypedTerm (Maybe Syntax.Sort) -> Typed.TypedTerm Syntax.RecordDefinition
recordDefinitionWithSort original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "locality"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "locality")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.RecordField
recordField :: Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.RecordField
recordField name type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the name field of hydra.coq.syntax.RecordField
recordFieldName :: Typed.TypedTerm Syntax.RecordField -> Typed.TypedTerm Syntax.Ident
recordFieldName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.RecordField
recordFieldType :: Typed.TypedTerm Syntax.RecordField -> Typed.TypedTerm Syntax.Type
recordFieldType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.coq.syntax.RecordField
recordFieldWithName :: Typed.TypedTerm Syntax.RecordField -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.RecordField
recordFieldWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.coq.syntax.RecordField
recordFieldWithType :: Typed.TypedTerm Syntax.RecordField -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.RecordField
recordFieldWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RecordField"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.RequireImport
requireImport :: Typed.TypedTerm (Maybe Syntax.Qualid) -> Typed.TypedTerm Bool -> Typed.TypedTerm (Maybe Syntax.ImportQualification) -> Typed.TypedTerm [Syntax.Qualid] -> Typed.TypedTerm Syntax.RequireImport
requireImport from require qualification modules =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Typed.unTypedTerm from)},
        Core.Field {
          Core.fieldName = (Core.Name "require"),
          Core.fieldTerm = (Typed.unTypedTerm require)},
        Core.Field {
          Core.fieldName = (Core.Name "qualification"),
          Core.fieldTerm = (Typed.unTypedTerm qualification)},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Typed.unTypedTerm modules)}]}))
-- | DSL accessor for the from field of hydra.coq.syntax.RequireImport
requireImportFrom :: Typed.TypedTerm Syntax.RequireImport -> Typed.TypedTerm (Maybe Syntax.Qualid)
requireImportFrom x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
        Core.projectionFieldName = (Core.Name "from")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modules field of hydra.coq.syntax.RequireImport
requireImportModules :: Typed.TypedTerm Syntax.RequireImport -> Typed.TypedTerm [Syntax.Qualid]
requireImportModules x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
        Core.projectionFieldName = (Core.Name "modules")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the qualification field of hydra.coq.syntax.RequireImport
requireImportQualification :: Typed.TypedTerm Syntax.RequireImport -> Typed.TypedTerm (Maybe Syntax.ImportQualification)
requireImportQualification x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
        Core.projectionFieldName = (Core.Name "qualification")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the require field of hydra.coq.syntax.RequireImport
requireImportRequire :: Typed.TypedTerm Syntax.RequireImport -> Typed.TypedTerm Bool
requireImportRequire x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
        Core.projectionFieldName = (Core.Name "require")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the from field of hydra.coq.syntax.RequireImport
requireImportWithFrom :: Typed.TypedTerm Syntax.RequireImport -> Typed.TypedTerm (Maybe Syntax.Qualid) -> Typed.TypedTerm Syntax.RequireImport
requireImportWithFrom original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "require"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "require")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "qualification")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "modules")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the modules field of hydra.coq.syntax.RequireImport
requireImportWithModules :: Typed.TypedTerm Syntax.RequireImport -> Typed.TypedTerm [Syntax.Qualid] -> Typed.TypedTerm Syntax.RequireImport
requireImportWithModules original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "from")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "require"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "require")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "qualification")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the qualification field of hydra.coq.syntax.RequireImport
requireImportWithQualification :: Typed.TypedTerm Syntax.RequireImport -> Typed.TypedTerm (Maybe Syntax.ImportQualification) -> Typed.TypedTerm Syntax.RequireImport
requireImportWithQualification original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "from")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "require"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "require")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "qualification"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "modules")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the require field of hydra.coq.syntax.RequireImport
requireImportWithRequire :: Typed.TypedTerm Syntax.RequireImport -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.RequireImport
requireImportWithRequire original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "from")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "require"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "qualification"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "qualification")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.RequireImport"),
              Core.projectionFieldName = (Core.Name "modules")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.ReturnAs
returnAs :: Typed.TypedTerm (Maybe Syntax.Name) -> Typed.TypedTerm Syntax.Term100 -> Typed.TypedTerm Syntax.ReturnAs
returnAs as return =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Typed.unTypedTerm as)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Typed.unTypedTerm return)}]}))
-- | DSL accessor for the as field of hydra.coq.syntax.ReturnAs
returnAsAs :: Typed.TypedTerm Syntax.ReturnAs -> Typed.TypedTerm (Maybe Syntax.Name)
returnAsAs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the return field of hydra.coq.syntax.ReturnAs
returnAsReturn :: Typed.TypedTerm Syntax.ReturnAs -> Typed.TypedTerm Syntax.Term100
returnAsReturn x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
        Core.projectionFieldName = (Core.Name "return")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the as field of hydra.coq.syntax.ReturnAs
returnAsWithAs :: Typed.TypedTerm Syntax.ReturnAs -> Typed.TypedTerm (Maybe Syntax.Name) -> Typed.TypedTerm Syntax.ReturnAs
returnAsWithAs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
              Core.projectionFieldName = (Core.Name "return")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the return field of hydra.coq.syntax.ReturnAs
returnAsWithReturn :: Typed.TypedTerm Syntax.ReturnAs -> Typed.TypedTerm Syntax.Term100 -> Typed.TypedTerm Syntax.ReturnAs
returnAsWithReturn original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.ReturnAs"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.coq.syntax.ScopeKey wrapper
scopeKey :: Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.ScopeKey
scopeKey x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.ScopeKey"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.coq.syntax.SectionDefinition
sectionDefinition :: Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm [Syntax.Sentence] -> Typed.TypedTerm Syntax.SectionDefinition
sectionDefinition name sentences =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Typed.unTypedTerm sentences)}]}))
-- | DSL accessor for the name field of hydra.coq.syntax.SectionDefinition
sectionDefinitionName :: Typed.TypedTerm Syntax.SectionDefinition -> Typed.TypedTerm Syntax.Ident
sectionDefinitionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the sentences field of hydra.coq.syntax.SectionDefinition
sectionDefinitionSentences :: Typed.TypedTerm Syntax.SectionDefinition -> Typed.TypedTerm [Syntax.Sentence]
sectionDefinitionSentences x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
        Core.projectionFieldName = (Core.Name "sentences")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.coq.syntax.SectionDefinition
sectionDefinitionWithName :: Typed.TypedTerm Syntax.SectionDefinition -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.SectionDefinition
sectionDefinitionWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
              Core.projectionFieldName = (Core.Name "sentences")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the sentences field of hydra.coq.syntax.SectionDefinition
sectionDefinitionWithSentences :: Typed.TypedTerm Syntax.SectionDefinition -> Typed.TypedTerm [Syntax.Sentence] -> Typed.TypedTerm Syntax.SectionDefinition
sectionDefinitionWithSentences original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.SectionDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sentences"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.Sentence
sentence :: Typed.TypedTerm (Maybe Syntax.Comment) -> Typed.TypedTerm Syntax.SentenceContent -> Typed.TypedTerm Syntax.Sentence
sentence comment content =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Typed.unTypedTerm comment)},
        Core.Field {
          Core.fieldName = (Core.Name "content"),
          Core.fieldTerm = (Typed.unTypedTerm content)}]}))
-- | DSL accessor for the comment field of hydra.coq.syntax.Sentence
sentenceComment :: Typed.TypedTerm Syntax.Sentence -> Typed.TypedTerm (Maybe Syntax.Comment)
sentenceComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
        Core.projectionFieldName = (Core.Name "comment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the content field of hydra.coq.syntax.Sentence
sentenceContent :: Typed.TypedTerm Syntax.Sentence -> Typed.TypedTerm Syntax.SentenceContent
sentenceContent x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
        Core.projectionFieldName = (Core.Name "content")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the axiom variant of hydra.coq.syntax.SentenceContent
sentenceContentAxiom :: Typed.TypedTerm Syntax.AxiomDeclaration -> Typed.TypedTerm Syntax.SentenceContent
sentenceContentAxiom x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "axiom"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the definition variant of hydra.coq.syntax.SentenceContent
sentenceContentDefinition :: Typed.TypedTerm Syntax.Definition -> Typed.TypedTerm Syntax.SentenceContent
sentenceContentDefinition x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "definition"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the fixpoint variant of hydra.coq.syntax.SentenceContent
sentenceContentFixpoint :: Typed.TypedTerm Syntax.FixpointDefinition -> Typed.TypedTerm Syntax.SentenceContent
sentenceContentFixpoint x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fixpoint"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the inductive variant of hydra.coq.syntax.SentenceContent
sentenceContentInductive :: Typed.TypedTerm Syntax.InductiveDefinition -> Typed.TypedTerm Syntax.SentenceContent
sentenceContentInductive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inductive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the module variant of hydra.coq.syntax.SentenceContent
sentenceContentModule :: Typed.TypedTerm Syntax.ModuleDefinition -> Typed.TypedTerm Syntax.SentenceContent
sentenceContentModule x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "module"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the notation variant of hydra.coq.syntax.SentenceContent
sentenceContentNotation :: Typed.TypedTerm Syntax.NotationDeclaration -> Typed.TypedTerm Syntax.SentenceContent
sentenceContentNotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the record variant of hydra.coq.syntax.SentenceContent
sentenceContentRecord :: Typed.TypedTerm Syntax.RecordDefinition -> Typed.TypedTerm Syntax.SentenceContent
sentenceContentRecord x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the requireImport variant of hydra.coq.syntax.SentenceContent
sentenceContentRequireImport :: Typed.TypedTerm Syntax.RequireImport -> Typed.TypedTerm Syntax.SentenceContent
sentenceContentRequireImport x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "requireImport"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the section variant of hydra.coq.syntax.SentenceContent
sentenceContentSection :: Typed.TypedTerm Syntax.SectionDefinition -> Typed.TypedTerm Syntax.SentenceContent
sentenceContentSection x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "section"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the theorem variant of hydra.coq.syntax.SentenceContent
sentenceContentTheorem :: Typed.TypedTerm Syntax.TheoremBody -> Typed.TypedTerm Syntax.SentenceContent
sentenceContentTheorem x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.SentenceContent"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "theorem"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL updater for the comment field of hydra.coq.syntax.Sentence
sentenceWithComment :: Typed.TypedTerm Syntax.Sentence -> Typed.TypedTerm (Maybe Syntax.Comment) -> Typed.TypedTerm Syntax.Sentence
sentenceWithComment original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "content"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
              Core.projectionFieldName = (Core.Name "content")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the content field of hydra.coq.syntax.Sentence
sentenceWithContent :: Typed.TypedTerm Syntax.Sentence -> Typed.TypedTerm Syntax.SentenceContent -> Typed.TypedTerm Syntax.Sentence
sentenceWithContent original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Sentence"),
              Core.projectionFieldName = (Core.Name "comment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "content"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the prop variant of hydra.coq.syntax.Sort
sortProp :: Typed.TypedTerm Syntax.Sort
sortProp =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "prop"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the sProp variant of hydra.coq.syntax.Sort
sortSProp :: Typed.TypedTerm Syntax.Sort
sortSProp =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sProp"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the set variant of hydra.coq.syntax.Sort
sortSet :: Typed.TypedTerm Syntax.Sort
sortSet =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the type variant of hydra.coq.syntax.Sort
sortType :: Typed.TypedTerm Syntax.Sort
sortType =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the typeWithAnyUniverse variant of hydra.coq.syntax.Sort
sortTypeWithAnyUniverse :: Typed.TypedTerm Syntax.Sort
sortTypeWithAnyUniverse =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeWithAnyUniverse"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the typeWithUniverse variant of hydra.coq.syntax.Sort
sortTypeWithUniverse :: Typed.TypedTerm Syntax.Universe -> Typed.TypedTerm Syntax.Sort
sortTypeWithUniverse x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Sort"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeWithUniverse"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.coq.syntax.String wrapper
string :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.String_
string x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.String"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the evar variant of hydra.coq.syntax.Term0
term0Evar :: Typed.TypedTerm Syntax.ExistentialVariable -> Typed.TypedTerm Syntax.Term0
term0Evar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "evar"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the generalizing variant of hydra.coq.syntax.Term0
term0Generalizing :: Typed.TypedTerm Syntax.Term0
term0Generalizing =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "generalizing"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the ltac variant of hydra.coq.syntax.Term0
term0Ltac :: Typed.TypedTerm Syntax.Term0
term0Ltac =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ltac"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the match variant of hydra.coq.syntax.Term0
term0Match :: Typed.TypedTerm Syntax.Match -> Typed.TypedTerm Syntax.Term0
term0Match x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the parens variant of hydra.coq.syntax.Term0
term0Parens :: Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.Term0
term0Parens x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primitiveNotations variant of hydra.coq.syntax.Term0
term0PrimitiveNotations :: Typed.TypedTerm Syntax.PrimitiveNotations -> Typed.TypedTerm Syntax.Term0
term0PrimitiveNotations x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitiveNotations"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the qualidAnnotated variant of hydra.coq.syntax.Term0
term0QualidAnnotated :: Typed.TypedTerm Syntax.QualidAnnotated -> Typed.TypedTerm Syntax.Term0
term0QualidAnnotated x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualidAnnotated"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the record variant of hydra.coq.syntax.Term0
term0Record :: Typed.TypedTerm Syntax.Term0
term0Record =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the sort variant of hydra.coq.syntax.Term0
term0Sort :: Typed.TypedTerm Syntax.Sort -> Typed.TypedTerm Syntax.Term0
term0Sort x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term0"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sort"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the cast variant of hydra.coq.syntax.Term100
term100Cast :: Typed.TypedTerm Syntax.TypeCast -> Typed.TypedTerm Syntax.Term100
term100Cast x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term100"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cast"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the term10 variant of hydra.coq.syntax.Term100
term100Term10 :: Typed.TypedTerm Syntax.Term10 -> Typed.TypedTerm Syntax.Term100
term100Term10 x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term100"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term10"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the application variant of hydra.coq.syntax.Term10
term10Application :: Typed.TypedTerm Syntax.Application -> Typed.TypedTerm Syntax.Term10
term10Application x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term10"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the oneTerm variant of hydra.coq.syntax.Term10
term10OneTerm :: Typed.TypedTerm Syntax.OneTerm -> Typed.TypedTerm Syntax.Term10
term10OneTerm x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term10"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "oneTerm"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the projection variant of hydra.coq.syntax.Term1
term1Projection :: Typed.TypedTerm Syntax.Term1
term1Projection =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term1"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "projection"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the scope variant of hydra.coq.syntax.Term1
term1Scope :: Typed.TypedTerm Syntax.Term1
term1Scope =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term1"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "scope"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the term0 variant of hydra.coq.syntax.Term1
term1Term0 :: Typed.TypedTerm Syntax.Term0 -> Typed.TypedTerm Syntax.Term1
term1Term0 x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term1"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term0"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the cofix variant of hydra.coq.syntax.Term
termCofix :: Typed.TypedTerm Syntax.Cofix -> Typed.TypedTerm Syntax.Term
termCofix x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cofix"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the fix variant of hydra.coq.syntax.Term
termFix :: Typed.TypedTerm Syntax.Fix -> Typed.TypedTerm Syntax.Term
termFix x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fix"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the forallOrFun variant of hydra.coq.syntax.Term
termForallOrFun :: Typed.TypedTerm Syntax.ForallOrFun -> Typed.TypedTerm Syntax.Term
termForallOrFun x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forallOrFun"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the if variant of hydra.coq.syntax.Term
termIf :: Typed.TypedTerm Syntax.If -> Typed.TypedTerm Syntax.Term
termIf x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the let variant of hydra.coq.syntax.Term
termLet :: Typed.TypedTerm Syntax.Let -> Typed.TypedTerm Syntax.Term
termLet x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the term100 variant of hydra.coq.syntax.Term
termTerm100 :: Typed.TypedTerm Syntax.Term100 -> Typed.TypedTerm Syntax.Term
termTerm100 x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term100"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.coq.syntax.TheoremBody
theoremBody :: Typed.TypedTerm Syntax.TheoremKind -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm [Syntax.Binder] -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.TheoremBody
theoremBody kind name binders type_ proof =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Typed.unTypedTerm kind)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm binders)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "proof"),
          Core.fieldTerm = (Typed.unTypedTerm proof)}]}))
-- | DSL accessor for the binders field of hydra.coq.syntax.TheoremBody
theoremBodyBinders :: Typed.TypedTerm Syntax.TheoremBody -> Typed.TypedTerm [Syntax.Binder]
theoremBodyBinders x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
        Core.projectionFieldName = (Core.Name "binders")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the kind field of hydra.coq.syntax.TheoremBody
theoremBodyKind :: Typed.TypedTerm Syntax.TheoremBody -> Typed.TypedTerm Syntax.TheoremKind
theoremBodyKind x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
        Core.projectionFieldName = (Core.Name "kind")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.coq.syntax.TheoremBody
theoremBodyName :: Typed.TypedTerm Syntax.TheoremBody -> Typed.TypedTerm Syntax.Ident
theoremBodyName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the proof field of hydra.coq.syntax.TheoremBody
theoremBodyProof :: Typed.TypedTerm Syntax.TheoremBody -> Typed.TypedTerm Syntax.Term
theoremBodyProof x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
        Core.projectionFieldName = (Core.Name "proof")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.TheoremBody
theoremBodyType :: Typed.TypedTerm Syntax.TheoremBody -> Typed.TypedTerm Syntax.Type
theoremBodyType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the binders field of hydra.coq.syntax.TheoremBody
theoremBodyWithBinders :: Typed.TypedTerm Syntax.TheoremBody -> Typed.TypedTerm [Syntax.Binder] -> Typed.TypedTerm Syntax.TheoremBody
theoremBodyWithBinders original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "proof"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "proof")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the kind field of hydra.coq.syntax.TheoremBody
theoremBodyWithKind :: Typed.TypedTerm Syntax.TheoremBody -> Typed.TypedTerm Syntax.TheoremKind -> Typed.TypedTerm Syntax.TheoremBody
theoremBodyWithKind original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "proof"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "proof")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.coq.syntax.TheoremBody
theoremBodyWithName :: Typed.TypedTerm Syntax.TheoremBody -> Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.TheoremBody
theoremBodyWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "proof"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "proof")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the proof field of hydra.coq.syntax.TheoremBody
theoremBodyWithProof :: Typed.TypedTerm Syntax.TheoremBody -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.TheoremBody
theoremBodyWithProof original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "proof"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the type field of hydra.coq.syntax.TheoremBody
theoremBodyWithType :: Typed.TypedTerm Syntax.TheoremBody -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TheoremBody
theoremBodyWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "binders")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "proof"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TheoremBody"),
              Core.projectionFieldName = (Core.Name "proof")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the corollary variant of hydra.coq.syntax.TheoremKind
theoremKindCorollary :: Typed.TypedTerm Syntax.TheoremKind
theoremKindCorollary =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TheoremKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "corollary"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the example variant of hydra.coq.syntax.TheoremKind
theoremKindExample :: Typed.TypedTerm Syntax.TheoremKind
theoremKindExample =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TheoremKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "example"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lemma variant of hydra.coq.syntax.TheoremKind
theoremKindLemma :: Typed.TypedTerm Syntax.TheoremKind
theoremKindLemma =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TheoremKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lemma"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the proposition variant of hydra.coq.syntax.TheoremKind
theoremKindProposition :: Typed.TypedTerm Syntax.TheoremKind
theoremKindProposition =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TheoremKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "proposition"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the theorem variant of hydra.coq.syntax.TheoremKind
theoremKindTheorem :: Typed.TypedTerm Syntax.TheoremKind
theoremKindTheorem =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TheoremKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "theorem"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.coq.syntax.Type wrapper
type_ :: Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.Type
type_ x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.Type"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.coq.syntax.TypeBinders
typeBinders :: Typed.TypedTerm [Syntax.Name] -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypeBinders
typeBinders names type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Typed.unTypedTerm names)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the names field of hydra.coq.syntax.TypeBinders
typeBindersNames :: Typed.TypedTerm Syntax.TypeBinders -> Typed.TypedTerm [Syntax.Name]
typeBindersNames x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
        Core.projectionFieldName = (Core.Name "names")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.TypeBinders
typeBindersType :: Typed.TypedTerm Syntax.TypeBinders -> Typed.TypedTerm Syntax.Type
typeBindersType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the names field of hydra.coq.syntax.TypeBinders
typeBindersWithNames :: Typed.TypedTerm Syntax.TypeBinders -> Typed.TypedTerm [Syntax.Name] -> Typed.TypedTerm Syntax.TypeBinders
typeBindersWithNames original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.coq.syntax.TypeBinders
typeBindersWithType :: Typed.TypedTerm Syntax.TypeBinders -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypeBinders
typeBindersWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeBinders"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coq.syntax.TypeCast
typeCast :: Typed.TypedTerm Syntax.Term10 -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypeCastOperator -> Typed.TypedTerm Syntax.TypeCast
typeCast term type_ operator =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm operator)}]}))
-- | DSL accessor for the operator field of hydra.coq.syntax.TypeCast
typeCastOperator :: Typed.TypedTerm Syntax.TypeCast -> Typed.TypedTerm Syntax.TypeCastOperator
typeCastOperator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the nativeCompute variant of hydra.coq.syntax.TypeCastOperator
typeCastOperatorNativeCompute :: Typed.TypedTerm Syntax.TypeCastOperator
typeCastOperatorNativeCompute =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TypeCastOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nativeCompute"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the normal variant of hydra.coq.syntax.TypeCastOperator
typeCastOperatorNormal :: Typed.TypedTerm Syntax.TypeCastOperator
typeCastOperatorNormal =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TypeCastOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the vmCompute variant of hydra.coq.syntax.TypeCastOperator
typeCastOperatorVmCompute :: Typed.TypedTerm Syntax.TypeCastOperator
typeCastOperatorVmCompute =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.TypeCastOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vmCompute"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the term field of hydra.coq.syntax.TypeCast
typeCastTerm :: Typed.TypedTerm Syntax.TypeCast -> Typed.TypedTerm Syntax.Term10
typeCastTerm x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coq.syntax.TypeCast
typeCastType :: Typed.TypedTerm Syntax.TypeCast -> Typed.TypedTerm Syntax.Type
typeCastType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the operator field of hydra.coq.syntax.TypeCast
typeCastWithOperator :: Typed.TypedTerm Syntax.TypeCast -> Typed.TypedTerm Syntax.TypeCastOperator -> Typed.TypedTerm Syntax.TypeCast
typeCastWithOperator original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the term field of hydra.coq.syntax.TypeCast
typeCastWithTerm :: Typed.TypedTerm Syntax.TypeCast -> Typed.TypedTerm Syntax.Term10 -> Typed.TypedTerm Syntax.TypeCast
typeCastWithTerm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.coq.syntax.TypeCast
typeCastWithType :: Typed.TypedTerm Syntax.TypeCast -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypeCast
typeCastWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeCast"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coq.syntax.TypeclassConstraint
typeclassConstraint :: Typed.TypedTerm (Maybe Syntax.Name) -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.TypeclassConstraint
typeclassConstraint name generalizing term =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "generalizing"),
          Core.fieldTerm = (Typed.unTypedTerm generalizing)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm term)}]}))
-- | DSL accessor for the generalizing field of hydra.coq.syntax.TypeclassConstraint
typeclassConstraintGeneralizing :: Typed.TypedTerm Syntax.TypeclassConstraint -> Typed.TypedTerm Bool
typeclassConstraintGeneralizing x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
        Core.projectionFieldName = (Core.Name "generalizing")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.coq.syntax.TypeclassConstraint
typeclassConstraintName :: Typed.TypedTerm Syntax.TypeclassConstraint -> Typed.TypedTerm (Maybe Syntax.Name)
typeclassConstraintName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coq.syntax.TypeclassConstraint
typeclassConstraintTerm :: Typed.TypedTerm Syntax.TypeclassConstraint -> Typed.TypedTerm Syntax.Term
typeclassConstraintTerm x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the generalizing field of hydra.coq.syntax.TypeclassConstraint
typeclassConstraintWithGeneralizing :: Typed.TypedTerm Syntax.TypeclassConstraint -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.TypeclassConstraint
typeclassConstraintWithGeneralizing original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generalizing"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.coq.syntax.TypeclassConstraint
typeclassConstraintWithName :: Typed.TypedTerm Syntax.TypeclassConstraint -> Typed.TypedTerm (Maybe Syntax.Name) -> Typed.TypedTerm Syntax.TypeclassConstraint
typeclassConstraintWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "generalizing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
              Core.projectionFieldName = (Core.Name "generalizing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.coq.syntax.TypeclassConstraint
typeclassConstraintWithTerm :: Typed.TypedTerm Syntax.TypeclassConstraint -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.TypeclassConstraint
typeclassConstraintWithTerm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generalizing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.TypeclassConstraint"),
              Core.projectionFieldName = (Core.Name "generalizing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the body of hydra.coq.syntax.Comment
unComment :: Typed.TypedTerm Syntax.Comment -> Typed.TypedTerm String
unComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.Comment")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.coq.syntax.FieldIdent
unFieldIdent :: Typed.TypedTerm Syntax.FieldIdent -> Typed.TypedTerm Syntax.Ident
unFieldIdent x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.FieldIdent")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.coq.syntax.Ident
unIdent :: Typed.TypedTerm Syntax.Ident -> Typed.TypedTerm Syntax.String_
unIdent x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.Ident")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.coq.syntax.Name
unName :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm (Maybe Syntax.Ident)
unName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.Name")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.coq.syntax.Natural
unNatural :: Typed.TypedTerm Syntax.Natural -> Typed.TypedTerm Integer
unNatural x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.Natural")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.coq.syntax.Number
unNumber :: Typed.TypedTerm Syntax.Number -> Typed.TypedTerm Double
unNumber x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.Number")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.coq.syntax.ScopeKey
unScopeKey :: Typed.TypedTerm Syntax.ScopeKey -> Typed.TypedTerm Syntax.Ident
unScopeKey x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.ScopeKey")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.coq.syntax.String
unString :: Typed.TypedTerm Syntax.String_ -> Typed.TypedTerm String
unString x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.String")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.coq.syntax.Type
unType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Term
unType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.Type")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.coq.syntax.UnivAnnot
unUnivAnnot :: Typed.TypedTerm Syntax.UnivAnnot -> Typed.TypedTerm [Syntax.UniverseLevel]
unUnivAnnot x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coq.syntax.UnivAnnot")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.coq.syntax.UnivAnnot wrapper
univAnnot :: Typed.TypedTerm [Syntax.UniverseLevel] -> Typed.TypedTerm Syntax.UnivAnnot
univAnnot x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coq.syntax.UnivAnnot"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the expr variant of hydra.coq.syntax.Universe
universeExpr :: Typed.TypedTerm Syntax.Universe_Expr -> Typed.TypedTerm Syntax.Universe
universeExpr x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Universe"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expr"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ignored variant of hydra.coq.syntax.UniverseLevel
universeLevelIgnored :: Typed.TypedTerm Syntax.UniverseLevel
universeLevelIgnored =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseLevel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ignored"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the prop variant of hydra.coq.syntax.UniverseLevel
universeLevelProp :: Typed.TypedTerm Syntax.UniverseLevel
universeLevelProp =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseLevel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "prop"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the qualid variant of hydra.coq.syntax.UniverseLevel
universeLevelQualid :: Typed.TypedTerm Syntax.Qualid -> Typed.TypedTerm Syntax.UniverseLevel
universeLevelQualid x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseLevel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualid"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the set variant of hydra.coq.syntax.UniverseLevel
universeLevelSet :: Typed.TypedTerm Syntax.UniverseLevel
universeLevelSet =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseLevel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the type variant of hydra.coq.syntax.UniverseLevel
universeLevelType :: Typed.TypedTerm Syntax.UniverseLevel
universeLevelType =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseLevel"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the max variant of hydra.coq.syntax.Universe
universeMax :: Typed.TypedTerm [Syntax.Universe_Expr] -> Typed.TypedTerm Syntax.Universe
universeMax x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.Universe"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "max"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the prop variant of hydra.coq.syntax.UniverseName
universeNameProp :: Typed.TypedTerm Syntax.UniverseName
universeNameProp =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "prop"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the qualid variant of hydra.coq.syntax.UniverseName
universeNameQualid :: Typed.TypedTerm Syntax.Qualid -> Typed.TypedTerm Syntax.UniverseName
universeNameQualid x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qualid"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the set variant of hydra.coq.syntax.UniverseName
universeNameSet :: Typed.TypedTerm Syntax.UniverseName
universeNameSet =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coq.syntax.UniverseName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.coq.syntax.Universe_Expr
universe_Expr :: Typed.TypedTerm Syntax.UniverseName -> Typed.TypedTerm (Maybe Syntax.Natural) -> Typed.TypedTerm Syntax.Universe_Expr
universe_Expr name number =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Typed.unTypedTerm number)}]}))
-- | DSL accessor for the name field of hydra.coq.syntax.Universe_Expr
universe_ExprName :: Typed.TypedTerm Syntax.Universe_Expr -> Typed.TypedTerm Syntax.UniverseName
universe_ExprName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the number field of hydra.coq.syntax.Universe_Expr
universe_ExprNumber :: Typed.TypedTerm Syntax.Universe_Expr -> Typed.TypedTerm (Maybe Syntax.Natural)
universe_ExprNumber x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
        Core.projectionFieldName = (Core.Name "number")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.coq.syntax.Universe_Expr
universe_ExprWithName :: Typed.TypedTerm Syntax.Universe_Expr -> Typed.TypedTerm Syntax.UniverseName -> Typed.TypedTerm Syntax.Universe_Expr
universe_ExprWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
              Core.projectionFieldName = (Core.Name "number")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the number field of hydra.coq.syntax.Universe_Expr
universe_ExprWithNumber :: Typed.TypedTerm Syntax.Universe_Expr -> Typed.TypedTerm (Maybe Syntax.Natural) -> Typed.TypedTerm Syntax.Universe_Expr
universe_ExprWithNumber original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.syntax.Universe_Expr"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
