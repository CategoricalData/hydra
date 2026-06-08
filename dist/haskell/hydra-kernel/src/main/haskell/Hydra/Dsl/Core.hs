-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.core

module Hydra.Dsl.Core where
import qualified Hydra.Core as Core
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.Map as M
import qualified Data.Set as S
-- | DSL constructor for hydra.core.AnnotatedTerm
annotatedTerm :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.AnnotatedTerm
annotatedTerm body annotation =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Typed.unTypedTerm annotation)}]}))
-- | DSL accessor for the annotation field of hydra.core.AnnotatedTerm
annotatedTermAnnotation :: Typed.TypedTerm Core.AnnotatedTerm -> Typed.TypedTerm Core.Term
annotatedTermAnnotation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
        Core.projectionFieldName = (Core.Name "annotation")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.core.AnnotatedTerm
annotatedTermBody :: Typed.TypedTerm Core.AnnotatedTerm -> Typed.TypedTerm Core.Term
annotatedTermBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotation field of hydra.core.AnnotatedTerm
annotatedTermWithAnnotation :: Typed.TypedTerm Core.AnnotatedTerm -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.AnnotatedTerm
annotatedTermWithAnnotation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the body field of hydra.core.AnnotatedTerm
annotatedTermWithBody :: Typed.TypedTerm Core.AnnotatedTerm -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.AnnotatedTerm
annotatedTermWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
              Core.projectionFieldName = (Core.Name "annotation")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.core.AnnotatedType
annotatedType :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.AnnotatedType
annotatedType body annotation =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.AnnotatedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Typed.unTypedTerm annotation)}]}))
-- | DSL accessor for the annotation field of hydra.core.AnnotatedType
annotatedTypeAnnotation :: Typed.TypedTerm Core.AnnotatedType -> Typed.TypedTerm Core.Term
annotatedTypeAnnotation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedType"),
        Core.projectionFieldName = (Core.Name "annotation")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.core.AnnotatedType
annotatedTypeBody :: Typed.TypedTerm Core.AnnotatedType -> Typed.TypedTerm Core.Type
annotatedTypeBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedType"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotation field of hydra.core.AnnotatedType
annotatedTypeWithAnnotation :: Typed.TypedTerm Core.AnnotatedType -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.AnnotatedType
annotatedTypeWithAnnotation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.AnnotatedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedType"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the body field of hydra.core.AnnotatedType
annotatedTypeWithBody :: Typed.TypedTerm Core.AnnotatedType -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.AnnotatedType
annotatedTypeWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.AnnotatedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedType"),
              Core.projectionFieldName = (Core.Name "annotation")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.core.Application
application :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Application
application function argument =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Application"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Typed.unTypedTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Typed.unTypedTerm argument)}]}))
-- | DSL accessor for the argument field of hydra.core.Application
applicationArgument :: Typed.TypedTerm Core.Application -> Typed.TypedTerm Core.Term
applicationArgument x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Application"),
        Core.projectionFieldName = (Core.Name "argument")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the function field of hydra.core.Application
applicationFunction :: Typed.TypedTerm Core.Application -> Typed.TypedTerm Core.Term
applicationFunction x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Application"),
        Core.projectionFieldName = (Core.Name "function")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.core.ApplicationType
applicationType :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.ApplicationType
applicationType function argument =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.ApplicationType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Typed.unTypedTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Typed.unTypedTerm argument)}]}))
-- | DSL accessor for the argument field of hydra.core.ApplicationType
applicationTypeArgument :: Typed.TypedTerm Core.ApplicationType -> Typed.TypedTerm Core.Type
applicationTypeArgument x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.ApplicationType"),
        Core.projectionFieldName = (Core.Name "argument")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the function field of hydra.core.ApplicationType
applicationTypeFunction :: Typed.TypedTerm Core.ApplicationType -> Typed.TypedTerm Core.Type
applicationTypeFunction x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.ApplicationType"),
        Core.projectionFieldName = (Core.Name "function")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the argument field of hydra.core.ApplicationType
applicationTypeWithArgument :: Typed.TypedTerm Core.ApplicationType -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.ApplicationType
applicationTypeWithArgument original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.ApplicationType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.ApplicationType"),
              Core.projectionFieldName = (Core.Name "function")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the function field of hydra.core.ApplicationType
applicationTypeWithFunction :: Typed.TypedTerm Core.ApplicationType -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.ApplicationType
applicationTypeWithFunction original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.ApplicationType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.ApplicationType"),
              Core.projectionFieldName = (Core.Name "argument")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the argument field of hydra.core.Application
applicationWithArgument :: Typed.TypedTerm Core.Application -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Application
applicationWithArgument original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Application"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Application"),
              Core.projectionFieldName = (Core.Name "function")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the function field of hydra.core.Application
applicationWithFunction :: Typed.TypedTerm Core.Application -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Application
applicationWithFunction original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Application"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Application"),
              Core.projectionFieldName = (Core.Name "argument")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.core.Binding
binding :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (Maybe Core.TypeScheme) -> Typed.TypedTerm Core.Binding
binding name term typeScheme =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Binding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "typeScheme"),
          Core.fieldTerm = (Typed.unTypedTerm typeScheme)}]}))
-- | DSL accessor for the name field of hydra.core.Binding
bindingName :: Typed.TypedTerm Core.Binding -> Typed.TypedTerm Core.Name
bindingName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.core.Binding
bindingTerm :: Typed.TypedTerm Core.Binding -> Typed.TypedTerm Core.Term
bindingTerm x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeScheme field of hydra.core.Binding
bindingTypeScheme :: Typed.TypedTerm Core.Binding -> Typed.TypedTerm (Maybe Core.TypeScheme)
bindingTypeScheme x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
        Core.projectionFieldName = (Core.Name "typeScheme")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.core.Binding
bindingWithName :: Typed.TypedTerm Core.Binding -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Binding
bindingWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Binding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeScheme"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
              Core.projectionFieldName = (Core.Name "typeScheme")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.core.Binding
bindingWithTerm :: Typed.TypedTerm Core.Binding -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Binding
bindingWithTerm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Binding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeScheme"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
              Core.projectionFieldName = (Core.Name "typeScheme")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeScheme field of hydra.core.Binding
bindingWithTypeScheme :: Typed.TypedTerm Core.Binding -> Typed.TypedTerm (Maybe Core.TypeScheme) -> Typed.TypedTerm Core.Binding
bindingWithTypeScheme original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Binding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeScheme"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.core.CaseAlternative
caseAlternative :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.CaseAlternative
caseAlternative name handler =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.CaseAlternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "handler"),
          Core.fieldTerm = (Typed.unTypedTerm handler)}]}))
-- | DSL accessor for the handler field of hydra.core.CaseAlternative
caseAlternativeHandler :: Typed.TypedTerm Core.CaseAlternative -> Typed.TypedTerm Core.Term
caseAlternativeHandler x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.CaseAlternative"),
        Core.projectionFieldName = (Core.Name "handler")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.core.CaseAlternative
caseAlternativeName :: Typed.TypedTerm Core.CaseAlternative -> Typed.TypedTerm Core.Name
caseAlternativeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.CaseAlternative"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the handler field of hydra.core.CaseAlternative
caseAlternativeWithHandler :: Typed.TypedTerm Core.CaseAlternative -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.CaseAlternative
caseAlternativeWithHandler original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.CaseAlternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.CaseAlternative"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "handler"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.core.CaseAlternative
caseAlternativeWithName :: Typed.TypedTerm Core.CaseAlternative -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.CaseAlternative
caseAlternativeWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.CaseAlternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "handler"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.CaseAlternative"),
              Core.projectionFieldName = (Core.Name "handler")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.core.CaseStatement
caseStatement :: Typed.TypedTerm Core.Name -> Typed.TypedTerm (Maybe Core.Term) -> Typed.TypedTerm [Core.CaseAlternative] -> Typed.TypedTerm Core.CaseStatement
caseStatement typeName default_ cases =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.CaseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm default_)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Typed.unTypedTerm cases)}]}))
-- | DSL accessor for the cases field of hydra.core.CaseStatement
caseStatementCases :: Typed.TypedTerm Core.CaseStatement -> Typed.TypedTerm [Core.CaseAlternative]
caseStatementCases x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
        Core.projectionFieldName = (Core.Name "cases")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the default field of hydra.core.CaseStatement
caseStatementDefault :: Typed.TypedTerm Core.CaseStatement -> Typed.TypedTerm (Maybe Core.Term)
caseStatementDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeName field of hydra.core.CaseStatement
caseStatementTypeName :: Typed.TypedTerm Core.CaseStatement -> Typed.TypedTerm Core.Name
caseStatementTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
        Core.projectionFieldName = (Core.Name "typeName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cases field of hydra.core.CaseStatement
caseStatementWithCases :: Typed.TypedTerm Core.CaseStatement -> Typed.TypedTerm [Core.CaseAlternative] -> Typed.TypedTerm Core.CaseStatement
caseStatementWithCases original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.CaseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the default field of hydra.core.CaseStatement
caseStatementWithDefault :: Typed.TypedTerm Core.CaseStatement -> Typed.TypedTerm (Maybe Core.Term) -> Typed.TypedTerm Core.CaseStatement
caseStatementWithDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.CaseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeName field of hydra.core.CaseStatement
caseStatementWithTypeName :: Typed.TypedTerm Core.CaseStatement -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.CaseStatement
caseStatementWithTypeName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.CaseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.core.EitherType
eitherType :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.EitherType
eitherType left right =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.EitherType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Typed.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Typed.unTypedTerm right)}]}))
-- | DSL accessor for the left field of hydra.core.EitherType
eitherTypeLeft :: Typed.TypedTerm Core.EitherType -> Typed.TypedTerm Core.Type
eitherTypeLeft x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.EitherType"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the right field of hydra.core.EitherType
eitherTypeRight :: Typed.TypedTerm Core.EitherType -> Typed.TypedTerm Core.Type
eitherTypeRight x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.EitherType"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the left field of hydra.core.EitherType
eitherTypeWithLeft :: Typed.TypedTerm Core.EitherType -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.EitherType
eitherTypeWithLeft original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.EitherType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.EitherType"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the right field of hydra.core.EitherType
eitherTypeWithRight :: Typed.TypedTerm Core.EitherType -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.EitherType
eitherTypeWithRight original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.EitherType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.EitherType"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.core.Field
field :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Field
field name term =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm term)}]}))
-- | DSL accessor for the name field of hydra.core.Field
fieldName :: Typed.TypedTerm Core.Field -> Typed.TypedTerm Core.Name
fieldName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Field"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.core.Field
fieldTerm :: Typed.TypedTerm Core.Field -> Typed.TypedTerm Core.Term
fieldTerm x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Field"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.core.FieldType
fieldType :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.FieldType
fieldType name type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.FieldType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the name field of hydra.core.FieldType
fieldTypeName :: Typed.TypedTerm Core.FieldType -> Typed.TypedTerm Core.Name
fieldTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.FieldType"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.core.FieldType
fieldTypeType :: Typed.TypedTerm Core.FieldType -> Typed.TypedTerm Core.Type
fieldTypeType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.FieldType"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.core.FieldType
fieldTypeWithName :: Typed.TypedTerm Core.FieldType -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.FieldType
fieldTypeWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.FieldType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.FieldType"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.core.FieldType
fieldTypeWithType :: Typed.TypedTerm Core.FieldType -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.FieldType
fieldTypeWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.FieldType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.FieldType"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.core.Field
fieldWithName :: Typed.TypedTerm Core.Field -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Field
fieldWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Field"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.core.Field
fieldWithTerm :: Typed.TypedTerm Core.Field -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Field
fieldWithTerm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Field"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the float32 variant of hydra.core.FloatType
floatTypeFloat32 :: Typed.TypedTerm Core.FloatType
floatTypeFloat32 =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.FloatType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float32"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the float64 variant of hydra.core.FloatType
floatTypeFloat64 :: Typed.TypedTerm Core.FloatType
floatTypeFloat64 =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.FloatType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float64"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the float32 variant of hydra.core.FloatValue
floatValueFloat32 :: Typed.TypedTerm Float -> Typed.TypedTerm Core.FloatValue
floatValueFloat32 x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.FloatValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float32"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the float64 variant of hydra.core.FloatValue
floatValueFloat64 :: Typed.TypedTerm Double -> Typed.TypedTerm Core.FloatValue
floatValueFloat64 x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.FloatValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float64"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.core.ForallType
forallType :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.ForallType
forallType parameter body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.ForallType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Typed.unTypedTerm parameter)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.core.ForallType
forallTypeBody :: Typed.TypedTerm Core.ForallType -> Typed.TypedTerm Core.Type
forallTypeBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.ForallType"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the parameter field of hydra.core.ForallType
forallTypeParameter :: Typed.TypedTerm Core.ForallType -> Typed.TypedTerm Core.Name
forallTypeParameter x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.ForallType"),
        Core.projectionFieldName = (Core.Name "parameter")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.core.ForallType
forallTypeWithBody :: Typed.TypedTerm Core.ForallType -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.ForallType
forallTypeWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.ForallType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.ForallType"),
              Core.projectionFieldName = (Core.Name "parameter")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the parameter field of hydra.core.ForallType
forallTypeWithParameter :: Typed.TypedTerm Core.ForallType -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.ForallType
forallTypeWithParameter original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.ForallType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.ForallType"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.core.FunctionType
functionType :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.FunctionType
functionType domain codomain =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Typed.unTypedTerm domain)},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Typed.unTypedTerm codomain)}]}))
-- | DSL accessor for the codomain field of hydra.core.FunctionType
functionTypeCodomain :: Typed.TypedTerm Core.FunctionType -> Typed.TypedTerm Core.Type
functionTypeCodomain x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.FunctionType"),
        Core.projectionFieldName = (Core.Name "codomain")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the domain field of hydra.core.FunctionType
functionTypeDomain :: Typed.TypedTerm Core.FunctionType -> Typed.TypedTerm Core.Type
functionTypeDomain x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.FunctionType"),
        Core.projectionFieldName = (Core.Name "domain")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the codomain field of hydra.core.FunctionType
functionTypeWithCodomain :: Typed.TypedTerm Core.FunctionType -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.FunctionType
functionTypeWithCodomain original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.FunctionType"),
              Core.projectionFieldName = (Core.Name "domain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the domain field of hydra.core.FunctionType
functionTypeWithDomain :: Typed.TypedTerm Core.FunctionType -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.FunctionType
functionTypeWithDomain original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.FunctionType"),
              Core.projectionFieldName = (Core.Name "codomain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.core.Injection
injection :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Field -> Typed.TypedTerm Core.Injection
injection typeName field =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Injection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Typed.unTypedTerm field)}]}))
-- | DSL accessor for the field field of hydra.core.Injection
injectionField :: Typed.TypedTerm Core.Injection -> Typed.TypedTerm Core.Field
injectionField x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
        Core.projectionFieldName = (Core.Name "field")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeName field of hydra.core.Injection
injectionTypeName :: Typed.TypedTerm Core.Injection -> Typed.TypedTerm Core.Name
injectionTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
        Core.projectionFieldName = (Core.Name "typeName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the field field of hydra.core.Injection
injectionWithField :: Typed.TypedTerm Core.Injection -> Typed.TypedTerm Core.Field -> Typed.TypedTerm Core.Injection
injectionWithField original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Injection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the typeName field of hydra.core.Injection
injectionWithTypeName :: Typed.TypedTerm Core.Injection -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Injection
injectionWithTypeName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Injection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
              Core.projectionFieldName = (Core.Name "field")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the bigint variant of hydra.core.IntegerType
integerTypeBigint :: Typed.TypedTerm Core.IntegerType
integerTypeBigint =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bigint"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the int16 variant of hydra.core.IntegerType
integerTypeInt16 :: Typed.TypedTerm Core.IntegerType
integerTypeInt16 =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int16"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the int32 variant of hydra.core.IntegerType
integerTypeInt32 :: Typed.TypedTerm Core.IntegerType
integerTypeInt32 =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int32"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the int64 variant of hydra.core.IntegerType
integerTypeInt64 :: Typed.TypedTerm Core.IntegerType
integerTypeInt64 =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int64"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the int8 variant of hydra.core.IntegerType
integerTypeInt8 :: Typed.TypedTerm Core.IntegerType
integerTypeInt8 =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int8"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the uint16 variant of hydra.core.IntegerType
integerTypeUint16 :: Typed.TypedTerm Core.IntegerType
integerTypeUint16 =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint16"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the uint32 variant of hydra.core.IntegerType
integerTypeUint32 :: Typed.TypedTerm Core.IntegerType
integerTypeUint32 =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint32"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the uint64 variant of hydra.core.IntegerType
integerTypeUint64 :: Typed.TypedTerm Core.IntegerType
integerTypeUint64 =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint64"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the uint8 variant of hydra.core.IntegerType
integerTypeUint8 :: Typed.TypedTerm Core.IntegerType
integerTypeUint8 =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint8"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bigint variant of hydra.core.IntegerValue
integerValueBigint :: Typed.TypedTerm Integer -> Typed.TypedTerm Core.IntegerValue
integerValueBigint x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bigint"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the int16 variant of hydra.core.IntegerValue
integerValueInt16 :: Typed.TypedTerm I.Int16 -> Typed.TypedTerm Core.IntegerValue
integerValueInt16 x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int16"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the int32 variant of hydra.core.IntegerValue
integerValueInt32 :: Typed.TypedTerm Int -> Typed.TypedTerm Core.IntegerValue
integerValueInt32 x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int32"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the int64 variant of hydra.core.IntegerValue
integerValueInt64 :: Typed.TypedTerm I.Int64 -> Typed.TypedTerm Core.IntegerValue
integerValueInt64 x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int64"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the int8 variant of hydra.core.IntegerValue
integerValueInt8 :: Typed.TypedTerm I.Int8 -> Typed.TypedTerm Core.IntegerValue
integerValueInt8 x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int8"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the uint16 variant of hydra.core.IntegerValue
integerValueUint16 :: Typed.TypedTerm Int -> Typed.TypedTerm Core.IntegerValue
integerValueUint16 x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint16"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the uint32 variant of hydra.core.IntegerValue
integerValueUint32 :: Typed.TypedTerm I.Int64 -> Typed.TypedTerm Core.IntegerValue
integerValueUint32 x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint32"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the uint64 variant of hydra.core.IntegerValue
integerValueUint64 :: Typed.TypedTerm Integer -> Typed.TypedTerm Core.IntegerValue
integerValueUint64 x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint64"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the uint8 variant of hydra.core.IntegerValue
integerValueUint8 :: Typed.TypedTerm I.Int16 -> Typed.TypedTerm Core.IntegerValue
integerValueUint8 x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint8"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.core.Lambda
lambda :: Typed.TypedTerm Core.Name -> Typed.TypedTerm (Maybe Core.Type) -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Lambda
lambda parameter domain body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Typed.unTypedTerm parameter)},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Typed.unTypedTerm domain)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.core.Lambda
lambdaBody :: Typed.TypedTerm Core.Lambda -> Typed.TypedTerm Core.Term
lambdaBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the domain field of hydra.core.Lambda
lambdaDomain :: Typed.TypedTerm Core.Lambda -> Typed.TypedTerm (Maybe Core.Type)
lambdaDomain x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
        Core.projectionFieldName = (Core.Name "domain")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the parameter field of hydra.core.Lambda
lambdaParameter :: Typed.TypedTerm Core.Lambda -> Typed.TypedTerm Core.Name
lambdaParameter x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
        Core.projectionFieldName = (Core.Name "parameter")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.core.Lambda
lambdaWithBody :: Typed.TypedTerm Core.Lambda -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Lambda
lambdaWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
              Core.projectionFieldName = (Core.Name "parameter")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
              Core.projectionFieldName = (Core.Name "domain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the domain field of hydra.core.Lambda
lambdaWithDomain :: Typed.TypedTerm Core.Lambda -> Typed.TypedTerm (Maybe Core.Type) -> Typed.TypedTerm Core.Lambda
lambdaWithDomain original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
              Core.projectionFieldName = (Core.Name "parameter")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the parameter field of hydra.core.Lambda
lambdaWithParameter :: Typed.TypedTerm Core.Lambda -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Lambda
lambdaWithParameter original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
              Core.projectionFieldName = (Core.Name "domain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.core.Let
let_ :: Typed.TypedTerm [Core.Binding] -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Let
let_ bindings body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Let"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Typed.unTypedTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the bindings field of hydra.core.Let
letBindings :: Typed.TypedTerm Core.Let -> Typed.TypedTerm [Core.Binding]
letBindings x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Let"),
        Core.projectionFieldName = (Core.Name "bindings")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.core.Let
letBody :: Typed.TypedTerm Core.Let -> Typed.TypedTerm Core.Term
letBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Let"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the bindings field of hydra.core.Let
letWithBindings :: Typed.TypedTerm Core.Let -> Typed.TypedTerm [Core.Binding] -> Typed.TypedTerm Core.Let
letWithBindings original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Let"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Let"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.core.Let
letWithBody :: Typed.TypedTerm Core.Let -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Let
letWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Let"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Let"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the binary variant of hydra.core.Literal
literalBinary :: Typed.TypedTerm B.ByteString -> Typed.TypedTerm Core.Literal
literalBinary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the boolean variant of hydra.core.Literal
literalBoolean :: Typed.TypedTerm Bool -> Typed.TypedTerm Core.Literal
literalBoolean x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the decimal variant of hydra.core.Literal
literalDecimal :: Typed.TypedTerm Sci.Scientific -> Typed.TypedTerm Core.Literal
literalDecimal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decimal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the float variant of hydra.core.Literal
literalFloat :: Typed.TypedTerm Core.FloatValue -> Typed.TypedTerm Core.Literal
literalFloat x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the integer variant of hydra.core.Literal
literalInteger :: Typed.TypedTerm Core.IntegerValue -> Typed.TypedTerm Core.Literal
literalInteger x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the string variant of hydra.core.Literal
literalString :: Typed.TypedTerm String -> Typed.TypedTerm Core.Literal
literalString x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the binary variant of hydra.core.LiteralType
literalTypeBinary :: Typed.TypedTerm Core.LiteralType
literalTypeBinary =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the boolean variant of hydra.core.LiteralType
literalTypeBoolean :: Typed.TypedTerm Core.LiteralType
literalTypeBoolean =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the decimal variant of hydra.core.LiteralType
literalTypeDecimal :: Typed.TypedTerm Core.LiteralType
literalTypeDecimal =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decimal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the float variant of hydra.core.LiteralType
literalTypeFloat :: Typed.TypedTerm Core.FloatType -> Typed.TypedTerm Core.LiteralType
literalTypeFloat x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the integer variant of hydra.core.LiteralType
literalTypeInteger :: Typed.TypedTerm Core.IntegerType -> Typed.TypedTerm Core.LiteralType
literalTypeInteger x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the string variant of hydra.core.LiteralType
literalTypeString :: Typed.TypedTerm Core.LiteralType
literalTypeString =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.core.MapType
mapType :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.MapType
mapType keys values =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.MapType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Typed.unTypedTerm keys)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Typed.unTypedTerm values)}]}))
-- | DSL accessor for the keys field of hydra.core.MapType
mapTypeKeys :: Typed.TypedTerm Core.MapType -> Typed.TypedTerm Core.Type
mapTypeKeys x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.MapType"),
        Core.projectionFieldName = (Core.Name "keys")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the values field of hydra.core.MapType
mapTypeValues :: Typed.TypedTerm Core.MapType -> Typed.TypedTerm Core.Type
mapTypeValues x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.MapType"),
        Core.projectionFieldName = (Core.Name "values")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the keys field of hydra.core.MapType
mapTypeWithKeys :: Typed.TypedTerm Core.MapType -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.MapType
mapTypeWithKeys original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.MapType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.MapType"),
              Core.projectionFieldName = (Core.Name "values")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the values field of hydra.core.MapType
mapTypeWithValues :: Typed.TypedTerm Core.MapType -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.MapType
mapTypeWithValues original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.MapType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.MapType"),
              Core.projectionFieldName = (Core.Name "keys")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.core.Name wrapper
name :: Typed.TypedTerm String -> Typed.TypedTerm Core.Name
name x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.core.PairType
pairType :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.PairType
pairType first second =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.PairType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Typed.unTypedTerm first)},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Typed.unTypedTerm second)}]}))
-- | DSL accessor for the first field of hydra.core.PairType
pairTypeFirst :: Typed.TypedTerm Core.PairType -> Typed.TypedTerm Core.Type
pairTypeFirst x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.PairType"),
        Core.projectionFieldName = (Core.Name "first")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the second field of hydra.core.PairType
pairTypeSecond :: Typed.TypedTerm Core.PairType -> Typed.TypedTerm Core.Type
pairTypeSecond x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.PairType"),
        Core.projectionFieldName = (Core.Name "second")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the first field of hydra.core.PairType
pairTypeWithFirst :: Typed.TypedTerm Core.PairType -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.PairType
pairTypeWithFirst original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.PairType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.PairType"),
              Core.projectionFieldName = (Core.Name "second")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the second field of hydra.core.PairType
pairTypeWithSecond :: Typed.TypedTerm Core.PairType -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.PairType
pairTypeWithSecond original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.PairType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.PairType"),
              Core.projectionFieldName = (Core.Name "first")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.core.Projection
projection :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Projection
projection typeName fieldName =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Typed.unTypedTerm fieldName)}]}))
-- | DSL accessor for the fieldName field of hydra.core.Projection
projectionFieldName :: Typed.TypedTerm Core.Projection -> Typed.TypedTerm Core.Name
projectionFieldName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Projection"),
        Core.projectionFieldName = (Core.Name "fieldName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeName field of hydra.core.Projection
projectionTypeName :: Typed.TypedTerm Core.Projection -> Typed.TypedTerm Core.Name
projectionTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Projection"),
        Core.projectionFieldName = (Core.Name "typeName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the fieldName field of hydra.core.Projection
projectionWithFieldName :: Typed.TypedTerm Core.Projection -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Projection
projectionWithFieldName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Projection"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the typeName field of hydra.core.Projection
projectionWithTypeName :: Typed.TypedTerm Core.Projection -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Projection
projectionWithTypeName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Projection"),
              Core.projectionFieldName = (Core.Name "fieldName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.core.Record
record :: Typed.TypedTerm Core.Name -> Typed.TypedTerm [Core.Field] -> Typed.TypedTerm Core.Record
record typeName fields =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Record"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Typed.unTypedTerm fields)}]}))
-- | DSL accessor for the fields field of hydra.core.Record
recordFields :: Typed.TypedTerm Core.Record -> Typed.TypedTerm [Core.Field]
recordFields x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Record"),
        Core.projectionFieldName = (Core.Name "fields")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeName field of hydra.core.Record
recordTypeName :: Typed.TypedTerm Core.Record -> Typed.TypedTerm Core.Name
recordTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Record"),
        Core.projectionFieldName = (Core.Name "typeName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the fields field of hydra.core.Record
recordWithFields :: Typed.TypedTerm Core.Record -> Typed.TypedTerm [Core.Field] -> Typed.TypedTerm Core.Record
recordWithFields original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Record"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Record"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the typeName field of hydra.core.Record
recordWithTypeName :: Typed.TypedTerm Core.Record -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Record
recordWithTypeName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Record"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Record"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the annotated variant of hydra.core.Term
termAnnotated :: Typed.TypedTerm Core.AnnotatedTerm -> Typed.TypedTerm Core.Term
termAnnotated x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotated"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the application variant of hydra.core.Term
termApplication :: Typed.TypedTerm Core.Application -> Typed.TypedTerm Core.Term
termApplication x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the cases variant of hydra.core.Term
termCases :: Typed.TypedTerm Core.CaseStatement -> Typed.TypedTerm Core.Term
termCases x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cases"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the either variant of hydra.core.Term
termEither :: Typed.TypedTerm (Either Core.Term Core.Term) -> Typed.TypedTerm Core.Term
termEither x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "either"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the inject variant of hydra.core.Term
termInject :: Typed.TypedTerm Core.Injection -> Typed.TypedTerm Core.Term
termInject x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inject"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the lambda variant of hydra.core.Term
termLambda :: Typed.TypedTerm Core.Lambda -> Typed.TypedTerm Core.Term
termLambda x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the let variant of hydra.core.Term
termLet :: Typed.TypedTerm Core.Let -> Typed.TypedTerm Core.Term
termLet x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the list variant of hydra.core.Term
termList :: Typed.TypedTerm [Core.Term] -> Typed.TypedTerm Core.Term
termList x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the literal variant of hydra.core.Term
termLiteral :: Typed.TypedTerm Core.Literal -> Typed.TypedTerm Core.Term
termLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the map variant of hydra.core.Term
termMap :: Typed.TypedTerm (M.Map Core.Term Core.Term) -> Typed.TypedTerm Core.Term
termMap x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the optional variant of hydra.core.Term
termOptional :: Typed.TypedTerm (Maybe Core.Term) -> Typed.TypedTerm Core.Term
termOptional x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "optional"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the pair variant of hydra.core.Term
termPair :: Typed.TypedTerm (Core.Term, Core.Term) -> Typed.TypedTerm Core.Term
termPair x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the project variant of hydra.core.Term
termProject :: Typed.TypedTerm Core.Projection -> Typed.TypedTerm Core.Term
termProject x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "project"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the record variant of hydra.core.Term
termRecord :: Typed.TypedTerm Core.Record -> Typed.TypedTerm Core.Term
termRecord x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the set variant of hydra.core.Term
termSet :: Typed.TypedTerm (S.Set Core.Term) -> Typed.TypedTerm Core.Term
termSet x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typeApplication variant of hydra.core.Term
termTypeApplication :: Typed.TypedTerm Core.TypeApplicationTerm -> Typed.TypedTerm Core.Term
termTypeApplication x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeApplication"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typeLambda variant of hydra.core.Term
termTypeLambda :: Typed.TypedTerm Core.TypeLambda -> Typed.TypedTerm Core.Term
termTypeLambda x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeLambda"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unit variant of hydra.core.Term
termUnit :: Typed.TypedTerm Core.Term
termUnit =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unwrap variant of hydra.core.Term
termUnwrap :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Term
termUnwrap x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unwrap"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variable variant of hydra.core.Term
termVariable :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Term
termVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the wrap variant of hydra.core.Term
termWrap :: Typed.TypedTerm Core.WrappedTerm -> Typed.TypedTerm Core.Term
termWrap x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrap"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the annotated variant of hydra.core.Type
typeAnnotated :: Typed.TypedTerm Core.AnnotatedType -> Typed.TypedTerm Core.Type
typeAnnotated x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotated"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the application variant of hydra.core.Type
typeApplication :: Typed.TypedTerm Core.ApplicationType -> Typed.TypedTerm Core.Type
typeApplication x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.core.TypeApplicationTerm
typeApplicationTerm :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.TypeApplicationTerm
typeApplicationTerm body type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the body field of hydra.core.TypeApplicationTerm
typeApplicationTermBody :: Typed.TypedTerm Core.TypeApplicationTerm -> Typed.TypedTerm Core.Term
typeApplicationTermBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.core.TypeApplicationTerm
typeApplicationTermType :: Typed.TypedTerm Core.TypeApplicationTerm -> Typed.TypedTerm Core.Type
typeApplicationTermType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.core.TypeApplicationTerm
typeApplicationTermWithBody :: Typed.TypedTerm Core.TypeApplicationTerm -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.TypeApplicationTerm
typeApplicationTermWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.core.TypeApplicationTerm
typeApplicationTermWithType :: Typed.TypedTerm Core.TypeApplicationTerm -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.TypeApplicationTerm
typeApplicationTermWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the simple variant of hydra.core.TypeClassConstraint
typeClassConstraintSimple :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.TypeClassConstraint
typeClassConstraintSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.TypeClassConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the either variant of hydra.core.Type
typeEither :: Typed.TypedTerm Core.EitherType -> Typed.TypedTerm Core.Type
typeEither x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "either"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the forall variant of hydra.core.Type
typeForall :: Typed.TypedTerm Core.ForallType -> Typed.TypedTerm Core.Type
typeForall x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forall"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the function variant of hydra.core.Type
typeFunction :: Typed.TypedTerm Core.FunctionType -> Typed.TypedTerm Core.Type
typeFunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.core.TypeLambda
typeLambda :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.TypeLambda
typeLambda parameter body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeLambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Typed.unTypedTerm parameter)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.core.TypeLambda
typeLambdaBody :: Typed.TypedTerm Core.TypeLambda -> Typed.TypedTerm Core.Term
typeLambdaBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeLambda"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the parameter field of hydra.core.TypeLambda
typeLambdaParameter :: Typed.TypedTerm Core.TypeLambda -> Typed.TypedTerm Core.Name
typeLambdaParameter x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeLambda"),
        Core.projectionFieldName = (Core.Name "parameter")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.core.TypeLambda
typeLambdaWithBody :: Typed.TypedTerm Core.TypeLambda -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.TypeLambda
typeLambdaWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeLambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeLambda"),
              Core.projectionFieldName = (Core.Name "parameter")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the parameter field of hydra.core.TypeLambda
typeLambdaWithParameter :: Typed.TypedTerm Core.TypeLambda -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.TypeLambda
typeLambdaWithParameter original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeLambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeLambda"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the list variant of hydra.core.Type
typeList :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type
typeList x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the literal variant of hydra.core.Type
typeLiteral :: Typed.TypedTerm Core.LiteralType -> Typed.TypedTerm Core.Type
typeLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the map variant of hydra.core.Type
typeMap :: Typed.TypedTerm Core.MapType -> Typed.TypedTerm Core.Type
typeMap x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the optional variant of hydra.core.Type
typeOptional :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type
typeOptional x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "optional"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the pair variant of hydra.core.Type
typePair :: Typed.TypedTerm Core.PairType -> Typed.TypedTerm Core.Type
typePair x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the record variant of hydra.core.Type
typeRecord :: Typed.TypedTerm [Core.FieldType] -> Typed.TypedTerm Core.Type
typeRecord x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.core.TypeScheme
typeScheme :: Typed.TypedTerm [Core.Name] -> Typed.TypedTerm Core.Type -> Typed.TypedTerm (Maybe (M.Map Core.Name Core.TypeVariableConstraints)) -> Typed.TypedTerm Core.TypeScheme
typeScheme variables body constraints =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeScheme"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Typed.unTypedTerm variables)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Typed.unTypedTerm constraints)}]}))
-- | DSL accessor for the body field of hydra.core.TypeScheme
typeSchemeBody :: Typed.TypedTerm Core.TypeScheme -> Typed.TypedTerm Core.Type
typeSchemeBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the constraints field of hydra.core.TypeScheme
typeSchemeConstraints :: Typed.TypedTerm Core.TypeScheme -> Typed.TypedTerm (Maybe (M.Map Core.Name Core.TypeVariableConstraints))
typeSchemeConstraints x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
        Core.projectionFieldName = (Core.Name "constraints")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the variables field of hydra.core.TypeScheme
typeSchemeVariables :: Typed.TypedTerm Core.TypeScheme -> Typed.TypedTerm [Core.Name]
typeSchemeVariables x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
        Core.projectionFieldName = (Core.Name "variables")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.core.TypeScheme
typeSchemeWithBody :: Typed.TypedTerm Core.TypeScheme -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.TypeScheme
typeSchemeWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeScheme"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
              Core.projectionFieldName = (Core.Name "variables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the constraints field of hydra.core.TypeScheme
typeSchemeWithConstraints :: Typed.TypedTerm Core.TypeScheme -> Typed.TypedTerm (Maybe (M.Map Core.Name Core.TypeVariableConstraints)) -> Typed.TypedTerm Core.TypeScheme
typeSchemeWithConstraints original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeScheme"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
              Core.projectionFieldName = (Core.Name "variables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the variables field of hydra.core.TypeScheme
typeSchemeWithVariables :: Typed.TypedTerm Core.TypeScheme -> Typed.TypedTerm [Core.Name] -> Typed.TypedTerm Core.TypeScheme
typeSchemeWithVariables original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeScheme"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the set variant of hydra.core.Type
typeSet :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type
typeSet x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the union variant of hydra.core.Type
typeUnion :: Typed.TypedTerm [Core.FieldType] -> Typed.TypedTerm Core.Type
typeUnion x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unit variant of hydra.core.Type
typeUnit :: Typed.TypedTerm Core.Type
typeUnit =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the variable variant of hydra.core.Type
typeVariable :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Type
typeVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.core.TypeVariableConstraints
typeVariableConstraints :: Typed.TypedTerm [Core.TypeClassConstraint] -> Typed.TypedTerm Core.TypeVariableConstraints
typeVariableConstraints classes =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeVariableConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Typed.unTypedTerm classes)}]}))
-- | DSL accessor for the classes field of hydra.core.TypeVariableConstraints
typeVariableConstraintsClasses :: Typed.TypedTerm Core.TypeVariableConstraints -> Typed.TypedTerm [Core.TypeClassConstraint]
typeVariableConstraintsClasses x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeVariableConstraints"),
        Core.projectionFieldName = (Core.Name "classes")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the classes field of hydra.core.TypeVariableConstraints
typeVariableConstraintsWithClasses :: Typed.TypedTerm Core.TypeVariableConstraints -> Typed.TypedTerm [Core.TypeClassConstraint] -> Typed.TypedTerm Core.TypeVariableConstraints
typeVariableConstraintsWithClasses original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeVariableConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the void variant of hydra.core.Type
typeVoid :: Typed.TypedTerm Core.Type
typeVoid =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "void"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the wrap variant of hydra.core.Type
typeWrap :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type
typeWrap x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrap"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL accessor for the body of hydra.core.Name
unName :: Typed.TypedTerm Core.Name -> Typed.TypedTerm String
unName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.core.Name")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.core.WrappedTerm
wrappedTerm :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.WrappedTerm
wrappedTerm typeName body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.WrappedTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.core.WrappedTerm
wrappedTermBody :: Typed.TypedTerm Core.WrappedTerm -> Typed.TypedTerm Core.Term
wrappedTermBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeName field of hydra.core.WrappedTerm
wrappedTermTypeName :: Typed.TypedTerm Core.WrappedTerm -> Typed.TypedTerm Core.Name
wrappedTermTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
        Core.projectionFieldName = (Core.Name "typeName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.core.WrappedTerm
wrappedTermWithBody :: Typed.TypedTerm Core.WrappedTerm -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.WrappedTerm
wrappedTermWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.WrappedTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the typeName field of hydra.core.WrappedTerm
wrappedTermWithTypeName :: Typed.TypedTerm Core.WrappedTerm -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.WrappedTerm
wrappedTermWithTypeName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.WrappedTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
