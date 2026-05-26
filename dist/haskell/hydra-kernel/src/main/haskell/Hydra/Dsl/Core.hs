-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.core

module Hydra.Dsl.Core where
import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.Map as M
import qualified Data.Set as S
-- | DSL constructor for hydra.core.AnnotatedTerm
annotatedTerm :: Phantoms.TTerm Core.Term -> Phantoms.TTerm (M.Map Core.Name Core.Term) -> Phantoms.TTerm Core.AnnotatedTerm
annotatedTerm body annotation =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Phantoms.unTTerm annotation)}]}))
-- | DSL accessor for the annotation field of hydra.core.AnnotatedTerm
annotatedTermAnnotation :: Phantoms.TTerm Core.AnnotatedTerm -> Phantoms.TTerm (M.Map Core.Name Core.Term)
annotatedTermAnnotation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
        Core.projectionFieldName = (Core.Name "annotation")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body field of hydra.core.AnnotatedTerm
annotatedTermBody :: Phantoms.TTerm Core.AnnotatedTerm -> Phantoms.TTerm Core.Term
annotatedTermBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the annotation field of hydra.core.AnnotatedTerm
annotatedTermWithAnnotation :: Phantoms.TTerm Core.AnnotatedTerm -> Phantoms.TTerm (M.Map Core.Name Core.Term) -> Phantoms.TTerm Core.AnnotatedTerm
annotatedTermWithAnnotation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the body field of hydra.core.AnnotatedTerm
annotatedTermWithBody :: Phantoms.TTerm Core.AnnotatedTerm -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.AnnotatedTerm
annotatedTermWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
              Core.projectionFieldName = (Core.Name "annotation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.core.AnnotatedType
annotatedType :: Phantoms.TTerm Core.Type -> Phantoms.TTerm (M.Map Core.Name Core.Term) -> Phantoms.TTerm Core.AnnotatedType
annotatedType body annotation =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.AnnotatedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Phantoms.unTTerm annotation)}]}))
-- | DSL accessor for the annotation field of hydra.core.AnnotatedType
annotatedTypeAnnotation :: Phantoms.TTerm Core.AnnotatedType -> Phantoms.TTerm (M.Map Core.Name Core.Term)
annotatedTypeAnnotation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedType"),
        Core.projectionFieldName = (Core.Name "annotation")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body field of hydra.core.AnnotatedType
annotatedTypeBody :: Phantoms.TTerm Core.AnnotatedType -> Phantoms.TTerm Core.Type
annotatedTypeBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedType"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the annotation field of hydra.core.AnnotatedType
annotatedTypeWithAnnotation :: Phantoms.TTerm Core.AnnotatedType -> Phantoms.TTerm (M.Map Core.Name Core.Term) -> Phantoms.TTerm Core.AnnotatedType
annotatedTypeWithAnnotation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.AnnotatedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedType"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the body field of hydra.core.AnnotatedType
annotatedTypeWithBody :: Phantoms.TTerm Core.AnnotatedType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.AnnotatedType
annotatedTypeWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.AnnotatedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedType"),
              Core.projectionFieldName = (Core.Name "annotation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.core.Application
application :: Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Application
application function argument =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Application"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm argument)}]}))
-- | DSL accessor for the argument field of hydra.core.Application
applicationArgument :: Phantoms.TTerm Core.Application -> Phantoms.TTerm Core.Term
applicationArgument x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Application"),
        Core.projectionFieldName = (Core.Name "argument")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the function field of hydra.core.Application
applicationFunction :: Phantoms.TTerm Core.Application -> Phantoms.TTerm Core.Term
applicationFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Application"),
        Core.projectionFieldName = (Core.Name "function")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.core.ApplicationType
applicationType :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.ApplicationType
applicationType function argument =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.ApplicationType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm argument)}]}))
-- | DSL accessor for the argument field of hydra.core.ApplicationType
applicationTypeArgument :: Phantoms.TTerm Core.ApplicationType -> Phantoms.TTerm Core.Type
applicationTypeArgument x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.ApplicationType"),
        Core.projectionFieldName = (Core.Name "argument")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the function field of hydra.core.ApplicationType
applicationTypeFunction :: Phantoms.TTerm Core.ApplicationType -> Phantoms.TTerm Core.Type
applicationTypeFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.ApplicationType"),
        Core.projectionFieldName = (Core.Name "function")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the argument field of hydra.core.ApplicationType
applicationTypeWithArgument :: Phantoms.TTerm Core.ApplicationType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.ApplicationType
applicationTypeWithArgument original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.ApplicationType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.ApplicationType"),
              Core.projectionFieldName = (Core.Name "function")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the function field of hydra.core.ApplicationType
applicationTypeWithFunction :: Phantoms.TTerm Core.ApplicationType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.ApplicationType
applicationTypeWithFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.ApplicationType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.ApplicationType"),
              Core.projectionFieldName = (Core.Name "argument")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the argument field of hydra.core.Application
applicationWithArgument :: Phantoms.TTerm Core.Application -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Application
applicationWithArgument original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Application"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Application"),
              Core.projectionFieldName = (Core.Name "function")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the function field of hydra.core.Application
applicationWithFunction :: Phantoms.TTerm Core.Application -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Application
applicationWithFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Application"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Application"),
              Core.projectionFieldName = (Core.Name "argument")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.core.Binding
binding :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Term -> Phantoms.TTerm (Maybe Core.TypeScheme) -> Phantoms.TTerm Core.Binding
binding name term typeScheme =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Binding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "typeScheme"),
          Core.fieldTerm = (Phantoms.unTTerm typeScheme)}]}))
-- | DSL accessor for the name field of hydra.core.Binding
bindingName :: Phantoms.TTerm Core.Binding -> Phantoms.TTerm Core.Name
bindingName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the term field of hydra.core.Binding
bindingTerm :: Phantoms.TTerm Core.Binding -> Phantoms.TTerm Core.Term
bindingTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeScheme field of hydra.core.Binding
bindingTypeScheme :: Phantoms.TTerm Core.Binding -> Phantoms.TTerm (Maybe Core.TypeScheme)
bindingTypeScheme x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
        Core.projectionFieldName = (Core.Name "typeScheme")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.core.Binding
bindingWithName :: Phantoms.TTerm Core.Binding -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Binding
bindingWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Binding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeScheme"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
              Core.projectionFieldName = (Core.Name "typeScheme")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the term field of hydra.core.Binding
bindingWithTerm :: Phantoms.TTerm Core.Binding -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Binding
bindingWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Binding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeScheme"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
              Core.projectionFieldName = (Core.Name "typeScheme")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeScheme field of hydra.core.Binding
bindingWithTypeScheme :: Phantoms.TTerm Core.Binding -> Phantoms.TTerm (Maybe Core.TypeScheme) -> Phantoms.TTerm Core.Binding
bindingWithTypeScheme original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Binding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeScheme"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.core.CaseStatement
caseStatement :: Phantoms.TTerm Core.Name -> Phantoms.TTerm (Maybe Core.Term) -> Phantoms.TTerm [Core.Field] -> Phantoms.TTerm Core.CaseStatement
caseStatement typeName default_ cases =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.CaseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm cases)}]}))
-- | DSL accessor for the cases field of hydra.core.CaseStatement
caseStatementCases :: Phantoms.TTerm Core.CaseStatement -> Phantoms.TTerm [Core.Field]
caseStatementCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
        Core.projectionFieldName = (Core.Name "cases")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the default field of hydra.core.CaseStatement
caseStatementDefault :: Phantoms.TTerm Core.CaseStatement -> Phantoms.TTerm (Maybe Core.Term)
caseStatementDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeName field of hydra.core.CaseStatement
caseStatementTypeName :: Phantoms.TTerm Core.CaseStatement -> Phantoms.TTerm Core.Name
caseStatementTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
        Core.projectionFieldName = (Core.Name "typeName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the cases field of hydra.core.CaseStatement
caseStatementWithCases :: Phantoms.TTerm Core.CaseStatement -> Phantoms.TTerm [Core.Field] -> Phantoms.TTerm Core.CaseStatement
caseStatementWithCases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.CaseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the default field of hydra.core.CaseStatement
caseStatementWithDefault :: Phantoms.TTerm Core.CaseStatement -> Phantoms.TTerm (Maybe Core.Term) -> Phantoms.TTerm Core.CaseStatement
caseStatementWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.CaseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeName field of hydra.core.CaseStatement
caseStatementWithTypeName :: Phantoms.TTerm Core.CaseStatement -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.CaseStatement
caseStatementWithTypeName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.CaseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.core.EitherType
eitherType :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.EitherType
eitherType left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.EitherType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))
-- | DSL accessor for the left field of hydra.core.EitherType
eitherTypeLeft :: Phantoms.TTerm Core.EitherType -> Phantoms.TTerm Core.Type
eitherTypeLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.EitherType"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the right field of hydra.core.EitherType
eitherTypeRight :: Phantoms.TTerm Core.EitherType -> Phantoms.TTerm Core.Type
eitherTypeRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.EitherType"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the left field of hydra.core.EitherType
eitherTypeWithLeft :: Phantoms.TTerm Core.EitherType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.EitherType
eitherTypeWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.EitherType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.EitherType"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the right field of hydra.core.EitherType
eitherTypeWithRight :: Phantoms.TTerm Core.EitherType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.EitherType
eitherTypeWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.EitherType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.EitherType"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.core.Field
field :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Field
field name term =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)}]}))
-- | DSL accessor for the name field of hydra.core.Field
fieldName :: Phantoms.TTerm Core.Field -> Phantoms.TTerm Core.Name
fieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Field"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the term field of hydra.core.Field
fieldTerm :: Phantoms.TTerm Core.Field -> Phantoms.TTerm Core.Term
fieldTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Field"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.core.FieldType
fieldType :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.FieldType
fieldType name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.FieldType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))
-- | DSL accessor for the name field of hydra.core.FieldType
fieldTypeName :: Phantoms.TTerm Core.FieldType -> Phantoms.TTerm Core.Name
fieldTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.FieldType"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.core.FieldType
fieldTypeType :: Phantoms.TTerm Core.FieldType -> Phantoms.TTerm Core.Type
fieldTypeType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.FieldType"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.core.FieldType
fieldTypeWithName :: Phantoms.TTerm Core.FieldType -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.FieldType
fieldTypeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.FieldType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.FieldType"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.core.FieldType
fieldTypeWithType :: Phantoms.TTerm Core.FieldType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.FieldType
fieldTypeWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.FieldType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.FieldType"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.core.Field
fieldWithName :: Phantoms.TTerm Core.Field -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Field
fieldWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Field"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the term field of hydra.core.Field
fieldWithTerm :: Phantoms.TTerm Core.Field -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Field
fieldWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Field"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the float32 variant of hydra.core.FloatType
floatTypeFloat32 :: Phantoms.TTerm Core.FloatType
floatTypeFloat32 =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.FloatType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float32"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the float64 variant of hydra.core.FloatType
floatTypeFloat64 :: Phantoms.TTerm Core.FloatType
floatTypeFloat64 =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.FloatType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float64"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the float32 variant of hydra.core.FloatValue
floatValueFloat32 :: Phantoms.TTerm Float -> Phantoms.TTerm Core.FloatValue
floatValueFloat32 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.FloatValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float32"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the float64 variant of hydra.core.FloatValue
floatValueFloat64 :: Phantoms.TTerm Double -> Phantoms.TTerm Core.FloatValue
floatValueFloat64 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.FloatValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float64"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.core.ForallType
forallType :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.ForallType
forallType parameter body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.ForallType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Phantoms.unTTerm parameter)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.core.ForallType
forallTypeBody :: Phantoms.TTerm Core.ForallType -> Phantoms.TTerm Core.Type
forallTypeBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.ForallType"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the parameter field of hydra.core.ForallType
forallTypeParameter :: Phantoms.TTerm Core.ForallType -> Phantoms.TTerm Core.Name
forallTypeParameter x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.ForallType"),
        Core.projectionFieldName = (Core.Name "parameter")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.core.ForallType
forallTypeWithBody :: Phantoms.TTerm Core.ForallType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.ForallType
forallTypeWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.ForallType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.ForallType"),
              Core.projectionFieldName = (Core.Name "parameter")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the parameter field of hydra.core.ForallType
forallTypeWithParameter :: Phantoms.TTerm Core.ForallType -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.ForallType
forallTypeWithParameter original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.ForallType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.ForallType"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.core.FunctionType
functionType :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.FunctionType
functionType domain codomain =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm domain)},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Phantoms.unTTerm codomain)}]}))
-- | DSL accessor for the codomain field of hydra.core.FunctionType
functionTypeCodomain :: Phantoms.TTerm Core.FunctionType -> Phantoms.TTerm Core.Type
functionTypeCodomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.FunctionType"),
        Core.projectionFieldName = (Core.Name "codomain")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the domain field of hydra.core.FunctionType
functionTypeDomain :: Phantoms.TTerm Core.FunctionType -> Phantoms.TTerm Core.Type
functionTypeDomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.FunctionType"),
        Core.projectionFieldName = (Core.Name "domain")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the codomain field of hydra.core.FunctionType
functionTypeWithCodomain :: Phantoms.TTerm Core.FunctionType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.FunctionType
functionTypeWithCodomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.FunctionType"),
              Core.projectionFieldName = (Core.Name "domain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the domain field of hydra.core.FunctionType
functionTypeWithDomain :: Phantoms.TTerm Core.FunctionType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.FunctionType
functionTypeWithDomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.FunctionType"),
              Core.projectionFieldName = (Core.Name "codomain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.core.Injection
injection :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Field -> Phantoms.TTerm Core.Injection
injection typeName field =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Injection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Phantoms.unTTerm field)}]}))
-- | DSL accessor for the field field of hydra.core.Injection
injectionField :: Phantoms.TTerm Core.Injection -> Phantoms.TTerm Core.Field
injectionField x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
        Core.projectionFieldName = (Core.Name "field")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeName field of hydra.core.Injection
injectionTypeName :: Phantoms.TTerm Core.Injection -> Phantoms.TTerm Core.Name
injectionTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
        Core.projectionFieldName = (Core.Name "typeName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the field field of hydra.core.Injection
injectionWithField :: Phantoms.TTerm Core.Injection -> Phantoms.TTerm Core.Field -> Phantoms.TTerm Core.Injection
injectionWithField original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Injection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the typeName field of hydra.core.Injection
injectionWithTypeName :: Phantoms.TTerm Core.Injection -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Injection
injectionWithTypeName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Injection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
              Core.projectionFieldName = (Core.Name "field")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the bigint variant of hydra.core.IntegerType
integerTypeBigint :: Phantoms.TTerm Core.IntegerType
integerTypeBigint =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bigint"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the int16 variant of hydra.core.IntegerType
integerTypeInt16 :: Phantoms.TTerm Core.IntegerType
integerTypeInt16 =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int16"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the int32 variant of hydra.core.IntegerType
integerTypeInt32 :: Phantoms.TTerm Core.IntegerType
integerTypeInt32 =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int32"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the int64 variant of hydra.core.IntegerType
integerTypeInt64 :: Phantoms.TTerm Core.IntegerType
integerTypeInt64 =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int64"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the int8 variant of hydra.core.IntegerType
integerTypeInt8 :: Phantoms.TTerm Core.IntegerType
integerTypeInt8 =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int8"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the uint16 variant of hydra.core.IntegerType
integerTypeUint16 :: Phantoms.TTerm Core.IntegerType
integerTypeUint16 =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint16"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the uint32 variant of hydra.core.IntegerType
integerTypeUint32 :: Phantoms.TTerm Core.IntegerType
integerTypeUint32 =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint32"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the uint64 variant of hydra.core.IntegerType
integerTypeUint64 :: Phantoms.TTerm Core.IntegerType
integerTypeUint64 =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint64"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the uint8 variant of hydra.core.IntegerType
integerTypeUint8 :: Phantoms.TTerm Core.IntegerType
integerTypeUint8 =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint8"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bigint variant of hydra.core.IntegerValue
integerValueBigint :: Phantoms.TTerm Integer -> Phantoms.TTerm Core.IntegerValue
integerValueBigint x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bigint"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the int16 variant of hydra.core.IntegerValue
integerValueInt16 :: Phantoms.TTerm I.Int16 -> Phantoms.TTerm Core.IntegerValue
integerValueInt16 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int16"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the int32 variant of hydra.core.IntegerValue
integerValueInt32 :: Phantoms.TTerm Int -> Phantoms.TTerm Core.IntegerValue
integerValueInt32 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int32"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the int64 variant of hydra.core.IntegerValue
integerValueInt64 :: Phantoms.TTerm I.Int64 -> Phantoms.TTerm Core.IntegerValue
integerValueInt64 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int64"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the int8 variant of hydra.core.IntegerValue
integerValueInt8 :: Phantoms.TTerm I.Int8 -> Phantoms.TTerm Core.IntegerValue
integerValueInt8 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int8"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the uint16 variant of hydra.core.IntegerValue
integerValueUint16 :: Phantoms.TTerm Int -> Phantoms.TTerm Core.IntegerValue
integerValueUint16 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint16"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the uint32 variant of hydra.core.IntegerValue
integerValueUint32 :: Phantoms.TTerm I.Int64 -> Phantoms.TTerm Core.IntegerValue
integerValueUint32 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint32"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the uint64 variant of hydra.core.IntegerValue
integerValueUint64 :: Phantoms.TTerm Integer -> Phantoms.TTerm Core.IntegerValue
integerValueUint64 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint64"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the uint8 variant of hydra.core.IntegerValue
integerValueUint8 :: Phantoms.TTerm I.Int16 -> Phantoms.TTerm Core.IntegerValue
integerValueUint8 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint8"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.core.Lambda
lambda :: Phantoms.TTerm Core.Name -> Phantoms.TTerm (Maybe Core.Type) -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Lambda
lambda parameter domain body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Phantoms.unTTerm parameter)},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm domain)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.core.Lambda
lambdaBody :: Phantoms.TTerm Core.Lambda -> Phantoms.TTerm Core.Term
lambdaBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the domain field of hydra.core.Lambda
lambdaDomain :: Phantoms.TTerm Core.Lambda -> Phantoms.TTerm (Maybe Core.Type)
lambdaDomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
        Core.projectionFieldName = (Core.Name "domain")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the parameter field of hydra.core.Lambda
lambdaParameter :: Phantoms.TTerm Core.Lambda -> Phantoms.TTerm Core.Name
lambdaParameter x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
        Core.projectionFieldName = (Core.Name "parameter")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.core.Lambda
lambdaWithBody :: Phantoms.TTerm Core.Lambda -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Lambda
lambdaWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
              Core.projectionFieldName = (Core.Name "parameter")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
              Core.projectionFieldName = (Core.Name "domain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the domain field of hydra.core.Lambda
lambdaWithDomain :: Phantoms.TTerm Core.Lambda -> Phantoms.TTerm (Maybe Core.Type) -> Phantoms.TTerm Core.Lambda
lambdaWithDomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
              Core.projectionFieldName = (Core.Name "parameter")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the parameter field of hydra.core.Lambda
lambdaWithParameter :: Phantoms.TTerm Core.Lambda -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Lambda
lambdaWithParameter original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
              Core.projectionFieldName = (Core.Name "domain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.core.Let
let_ :: Phantoms.TTerm [Core.Binding] -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Let
let_ bindings body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Let"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the bindings field of hydra.core.Let
letBindings :: Phantoms.TTerm Core.Let -> Phantoms.TTerm [Core.Binding]
letBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Let"),
        Core.projectionFieldName = (Core.Name "bindings")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body field of hydra.core.Let
letBody :: Phantoms.TTerm Core.Let -> Phantoms.TTerm Core.Term
letBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Let"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the bindings field of hydra.core.Let
letWithBindings :: Phantoms.TTerm Core.Let -> Phantoms.TTerm [Core.Binding] -> Phantoms.TTerm Core.Let
letWithBindings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Let"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Let"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the body field of hydra.core.Let
letWithBody :: Phantoms.TTerm Core.Let -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Let
letWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Let"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Let"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the binary variant of hydra.core.Literal
literalBinary :: Phantoms.TTerm B.ByteString -> Phantoms.TTerm Core.Literal
literalBinary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the boolean variant of hydra.core.Literal
literalBoolean :: Phantoms.TTerm Bool -> Phantoms.TTerm Core.Literal
literalBoolean x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the decimal variant of hydra.core.Literal
literalDecimal :: Phantoms.TTerm Sci.Scientific -> Phantoms.TTerm Core.Literal
literalDecimal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decimal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the float variant of hydra.core.Literal
literalFloat :: Phantoms.TTerm Core.FloatValue -> Phantoms.TTerm Core.Literal
literalFloat x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the integer variant of hydra.core.Literal
literalInteger :: Phantoms.TTerm Core.IntegerValue -> Phantoms.TTerm Core.Literal
literalInteger x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the string variant of hydra.core.Literal
literalString :: Phantoms.TTerm String -> Phantoms.TTerm Core.Literal
literalString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the binary variant of hydra.core.LiteralType
literalTypeBinary :: Phantoms.TTerm Core.LiteralType
literalTypeBinary =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the boolean variant of hydra.core.LiteralType
literalTypeBoolean :: Phantoms.TTerm Core.LiteralType
literalTypeBoolean =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the decimal variant of hydra.core.LiteralType
literalTypeDecimal :: Phantoms.TTerm Core.LiteralType
literalTypeDecimal =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decimal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the float variant of hydra.core.LiteralType
literalTypeFloat :: Phantoms.TTerm Core.FloatType -> Phantoms.TTerm Core.LiteralType
literalTypeFloat x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the integer variant of hydra.core.LiteralType
literalTypeInteger :: Phantoms.TTerm Core.IntegerType -> Phantoms.TTerm Core.LiteralType
literalTypeInteger x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the string variant of hydra.core.LiteralType
literalTypeString :: Phantoms.TTerm Core.LiteralType
literalTypeString =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.core.MapType
mapType :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.MapType
mapType keys values =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.MapType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Phantoms.unTTerm keys)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTTerm values)}]}))
-- | DSL accessor for the keys field of hydra.core.MapType
mapTypeKeys :: Phantoms.TTerm Core.MapType -> Phantoms.TTerm Core.Type
mapTypeKeys x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.MapType"),
        Core.projectionFieldName = (Core.Name "keys")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the values field of hydra.core.MapType
mapTypeValues :: Phantoms.TTerm Core.MapType -> Phantoms.TTerm Core.Type
mapTypeValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.MapType"),
        Core.projectionFieldName = (Core.Name "values")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the keys field of hydra.core.MapType
mapTypeWithKeys :: Phantoms.TTerm Core.MapType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.MapType
mapTypeWithKeys original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.MapType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.MapType"),
              Core.projectionFieldName = (Core.Name "values")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the values field of hydra.core.MapType
mapTypeWithValues :: Phantoms.TTerm Core.MapType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.MapType
mapTypeWithValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.MapType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.MapType"),
              Core.projectionFieldName = (Core.Name "keys")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.core.Name wrapper
name :: Phantoms.TTerm String -> Phantoms.TTerm Core.Name
name x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.core.PairType
pairType :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.PairType
pairType first second =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.PairType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm first)},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Phantoms.unTTerm second)}]}))
-- | DSL accessor for the first field of hydra.core.PairType
pairTypeFirst :: Phantoms.TTerm Core.PairType -> Phantoms.TTerm Core.Type
pairTypeFirst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.PairType"),
        Core.projectionFieldName = (Core.Name "first")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the second field of hydra.core.PairType
pairTypeSecond :: Phantoms.TTerm Core.PairType -> Phantoms.TTerm Core.Type
pairTypeSecond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.PairType"),
        Core.projectionFieldName = (Core.Name "second")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the first field of hydra.core.PairType
pairTypeWithFirst :: Phantoms.TTerm Core.PairType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.PairType
pairTypeWithFirst original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.PairType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.PairType"),
              Core.projectionFieldName = (Core.Name "second")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the second field of hydra.core.PairType
pairTypeWithSecond :: Phantoms.TTerm Core.PairType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.PairType
pairTypeWithSecond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.PairType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.PairType"),
              Core.projectionFieldName = (Core.Name "first")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.core.Projection
projection :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Projection
projection typeName fieldName =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Phantoms.unTTerm fieldName)}]}))
-- | DSL accessor for the fieldName field of hydra.core.Projection
projectionFieldName :: Phantoms.TTerm Core.Projection -> Phantoms.TTerm Core.Name
projectionFieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Projection"),
        Core.projectionFieldName = (Core.Name "fieldName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeName field of hydra.core.Projection
projectionTypeName :: Phantoms.TTerm Core.Projection -> Phantoms.TTerm Core.Name
projectionTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Projection"),
        Core.projectionFieldName = (Core.Name "typeName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the fieldName field of hydra.core.Projection
projectionWithFieldName :: Phantoms.TTerm Core.Projection -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Projection
projectionWithFieldName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Projection"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the typeName field of hydra.core.Projection
projectionWithTypeName :: Phantoms.TTerm Core.Projection -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Projection
projectionWithTypeName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Projection"),
              Core.projectionFieldName = (Core.Name "fieldName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.core.Record
record :: Phantoms.TTerm Core.Name -> Phantoms.TTerm [Core.Field] -> Phantoms.TTerm Core.Record
record typeName fields =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Record"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)}]}))
-- | DSL accessor for the fields field of hydra.core.Record
recordFields :: Phantoms.TTerm Core.Record -> Phantoms.TTerm [Core.Field]
recordFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Record"),
        Core.projectionFieldName = (Core.Name "fields")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeName field of hydra.core.Record
recordTypeName :: Phantoms.TTerm Core.Record -> Phantoms.TTerm Core.Name
recordTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Record"),
        Core.projectionFieldName = (Core.Name "typeName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the fields field of hydra.core.Record
recordWithFields :: Phantoms.TTerm Core.Record -> Phantoms.TTerm [Core.Field] -> Phantoms.TTerm Core.Record
recordWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Record"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Record"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the typeName field of hydra.core.Record
recordWithTypeName :: Phantoms.TTerm Core.Record -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Record
recordWithTypeName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Record"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Record"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the annotated variant of hydra.core.Term
termAnnotated :: Phantoms.TTerm Core.AnnotatedTerm -> Phantoms.TTerm Core.Term
termAnnotated x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotated"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the application variant of hydra.core.Term
termApplication :: Phantoms.TTerm Core.Application -> Phantoms.TTerm Core.Term
termApplication x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the cases variant of hydra.core.Term
termCases :: Phantoms.TTerm Core.CaseStatement -> Phantoms.TTerm Core.Term
termCases x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cases"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the either variant of hydra.core.Term
termEither :: Phantoms.TTerm (Either Core.Term Core.Term) -> Phantoms.TTerm Core.Term
termEither x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "either"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the inject variant of hydra.core.Term
termInject :: Phantoms.TTerm Core.Injection -> Phantoms.TTerm Core.Term
termInject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inject"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the lambda variant of hydra.core.Term
termLambda :: Phantoms.TTerm Core.Lambda -> Phantoms.TTerm Core.Term
termLambda x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the let variant of hydra.core.Term
termLet :: Phantoms.TTerm Core.Let -> Phantoms.TTerm Core.Term
termLet x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the list variant of hydra.core.Term
termList :: Phantoms.TTerm [Core.Term] -> Phantoms.TTerm Core.Term
termList x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the literal variant of hydra.core.Term
termLiteral :: Phantoms.TTerm Core.Literal -> Phantoms.TTerm Core.Term
termLiteral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the map variant of hydra.core.Term
termMap :: Phantoms.TTerm (M.Map Core.Term Core.Term) -> Phantoms.TTerm Core.Term
termMap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the maybe variant of hydra.core.Term
termMaybe :: Phantoms.TTerm (Maybe Core.Term) -> Phantoms.TTerm Core.Term
termMaybe x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maybe"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the pair variant of hydra.core.Term
termPair :: Phantoms.TTerm (Core.Term, Core.Term) -> Phantoms.TTerm Core.Term
termPair x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the project variant of hydra.core.Term
termProject :: Phantoms.TTerm Core.Projection -> Phantoms.TTerm Core.Term
termProject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "project"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the record variant of hydra.core.Term
termRecord :: Phantoms.TTerm Core.Record -> Phantoms.TTerm Core.Term
termRecord x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the set variant of hydra.core.Term
termSet :: Phantoms.TTerm (S.Set Core.Term) -> Phantoms.TTerm Core.Term
termSet x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the typeApplication variant of hydra.core.Term
termTypeApplication :: Phantoms.TTerm Core.TypeApplicationTerm -> Phantoms.TTerm Core.Term
termTypeApplication x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeApplication"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the typeLambda variant of hydra.core.Term
termTypeLambda :: Phantoms.TTerm Core.TypeLambda -> Phantoms.TTerm Core.Term
termTypeLambda x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeLambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unit variant of hydra.core.Term
termUnit :: Phantoms.TTerm Core.Term
termUnit =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unwrap variant of hydra.core.Term
termUnwrap :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Term
termUnwrap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unwrap"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the variable variant of hydra.core.Term
termVariable :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Term
termVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the wrap variant of hydra.core.Term
termWrap :: Phantoms.TTerm Core.WrappedTerm -> Phantoms.TTerm Core.Term
termWrap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrap"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the annotated variant of hydra.core.Type
typeAnnotated :: Phantoms.TTerm Core.AnnotatedType -> Phantoms.TTerm Core.Type
typeAnnotated x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotated"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the application variant of hydra.core.Type
typeApplication :: Phantoms.TTerm Core.ApplicationType -> Phantoms.TTerm Core.Type
typeApplication x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.core.TypeApplicationTerm
typeApplicationTerm :: Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.TypeApplicationTerm
typeApplicationTerm body type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))
-- | DSL accessor for the body field of hydra.core.TypeApplicationTerm
typeApplicationTermBody :: Phantoms.TTerm Core.TypeApplicationTerm -> Phantoms.TTerm Core.Term
typeApplicationTermBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.core.TypeApplicationTerm
typeApplicationTermType :: Phantoms.TTerm Core.TypeApplicationTerm -> Phantoms.TTerm Core.Type
typeApplicationTermType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.core.TypeApplicationTerm
typeApplicationTermWithBody :: Phantoms.TTerm Core.TypeApplicationTerm -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.TypeApplicationTerm
typeApplicationTermWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.core.TypeApplicationTerm
typeApplicationTermWithType :: Phantoms.TTerm Core.TypeApplicationTerm -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.TypeApplicationTerm
typeApplicationTermWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the simple variant of hydra.core.TypeClassConstraint
typeClassConstraintSimple :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.TypeClassConstraint
typeClassConstraintSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.TypeClassConstraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the either variant of hydra.core.Type
typeEither :: Phantoms.TTerm Core.EitherType -> Phantoms.TTerm Core.Type
typeEither x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "either"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the forall variant of hydra.core.Type
typeForall :: Phantoms.TTerm Core.ForallType -> Phantoms.TTerm Core.Type
typeForall x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forall"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the function variant of hydra.core.Type
typeFunction :: Phantoms.TTerm Core.FunctionType -> Phantoms.TTerm Core.Type
typeFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.core.TypeLambda
typeLambda :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.TypeLambda
typeLambda parameter body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeLambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Phantoms.unTTerm parameter)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.core.TypeLambda
typeLambdaBody :: Phantoms.TTerm Core.TypeLambda -> Phantoms.TTerm Core.Term
typeLambdaBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeLambda"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the parameter field of hydra.core.TypeLambda
typeLambdaParameter :: Phantoms.TTerm Core.TypeLambda -> Phantoms.TTerm Core.Name
typeLambdaParameter x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeLambda"),
        Core.projectionFieldName = (Core.Name "parameter")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.core.TypeLambda
typeLambdaWithBody :: Phantoms.TTerm Core.TypeLambda -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.TypeLambda
typeLambdaWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeLambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeLambda"),
              Core.projectionFieldName = (Core.Name "parameter")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the parameter field of hydra.core.TypeLambda
typeLambdaWithParameter :: Phantoms.TTerm Core.TypeLambda -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.TypeLambda
typeLambdaWithParameter original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeLambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeLambda"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the list variant of hydra.core.Type
typeList :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type
typeList x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the literal variant of hydra.core.Type
typeLiteral :: Phantoms.TTerm Core.LiteralType -> Phantoms.TTerm Core.Type
typeLiteral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the map variant of hydra.core.Type
typeMap :: Phantoms.TTerm Core.MapType -> Phantoms.TTerm Core.Type
typeMap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the maybe variant of hydra.core.Type
typeMaybe :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type
typeMaybe x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maybe"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the pair variant of hydra.core.Type
typePair :: Phantoms.TTerm Core.PairType -> Phantoms.TTerm Core.Type
typePair x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the record variant of hydra.core.Type
typeRecord :: Phantoms.TTerm [Core.FieldType] -> Phantoms.TTerm Core.Type
typeRecord x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.core.TypeScheme
typeScheme :: Phantoms.TTerm [Core.Name] -> Phantoms.TTerm Core.Type -> Phantoms.TTerm (Maybe (M.Map Core.Name Core.TypeVariableMetadata)) -> Phantoms.TTerm Core.TypeScheme
typeScheme variables body constraints =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeScheme"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Phantoms.unTTerm variables)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTTerm constraints)}]}))
-- | DSL accessor for the body field of hydra.core.TypeScheme
typeSchemeBody :: Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm Core.Type
typeSchemeBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the constraints field of hydra.core.TypeScheme
typeSchemeConstraints :: Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm (Maybe (M.Map Core.Name Core.TypeVariableMetadata))
typeSchemeConstraints x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
        Core.projectionFieldName = (Core.Name "constraints")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the variables field of hydra.core.TypeScheme
typeSchemeVariables :: Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm [Core.Name]
typeSchemeVariables x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
        Core.projectionFieldName = (Core.Name "variables")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.core.TypeScheme
typeSchemeWithBody :: Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.TypeScheme
typeSchemeWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeScheme"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
              Core.projectionFieldName = (Core.Name "variables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the constraints field of hydra.core.TypeScheme
typeSchemeWithConstraints :: Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm (Maybe (M.Map Core.Name Core.TypeVariableMetadata)) -> Phantoms.TTerm Core.TypeScheme
typeSchemeWithConstraints original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeScheme"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
              Core.projectionFieldName = (Core.Name "variables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the variables field of hydra.core.TypeScheme
typeSchemeWithVariables :: Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm [Core.Name] -> Phantoms.TTerm Core.TypeScheme
typeSchemeWithVariables original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeScheme"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the set variant of hydra.core.Type
typeSet :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type
typeSet x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the union variant of hydra.core.Type
typeUnion :: Phantoms.TTerm [Core.FieldType] -> Phantoms.TTerm Core.Type
typeUnion x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unit variant of hydra.core.Type
typeUnit :: Phantoms.TTerm Core.Type
typeUnit =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the variable variant of hydra.core.Type
typeVariable :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Type
typeVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.core.TypeVariableMetadata
typeVariableMetadata :: Phantoms.TTerm [Core.TypeClassConstraint] -> Phantoms.TTerm Core.TypeVariableMetadata
typeVariableMetadata classes =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeVariableMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Phantoms.unTTerm classes)}]}))
-- | DSL accessor for the classes field of hydra.core.TypeVariableMetadata
typeVariableMetadataClasses :: Phantoms.TTerm Core.TypeVariableMetadata -> Phantoms.TTerm [Core.TypeClassConstraint]
typeVariableMetadataClasses x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeVariableMetadata"),
        Core.projectionFieldName = (Core.Name "classes")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the classes field of hydra.core.TypeVariableMetadata
typeVariableMetadataWithClasses :: Phantoms.TTerm Core.TypeVariableMetadata -> Phantoms.TTerm [Core.TypeClassConstraint] -> Phantoms.TTerm Core.TypeVariableMetadata
typeVariableMetadataWithClasses original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeVariableMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the void variant of hydra.core.Type
typeVoid :: Phantoms.TTerm Core.Type
typeVoid =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "void"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the wrap variant of hydra.core.Type
typeWrap :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type
typeWrap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrap"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL accessor for the body of hydra.core.Name
unName :: Phantoms.TTerm Core.Name -> Phantoms.TTerm String
unName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.core.Name")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.core.WrappedTerm
wrappedTerm :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.WrappedTerm
wrappedTerm typeName body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.WrappedTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.core.WrappedTerm
wrappedTermBody :: Phantoms.TTerm Core.WrappedTerm -> Phantoms.TTerm Core.Term
wrappedTermBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeName field of hydra.core.WrappedTerm
wrappedTermTypeName :: Phantoms.TTerm Core.WrappedTerm -> Phantoms.TTerm Core.Name
wrappedTermTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
        Core.projectionFieldName = (Core.Name "typeName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.core.WrappedTerm
wrappedTermWithBody :: Phantoms.TTerm Core.WrappedTerm -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.WrappedTerm
wrappedTermWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.WrappedTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
              Core.projectionFieldName = (Core.Name "typeName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the typeName field of hydra.core.WrappedTerm
wrappedTermWithTypeName :: Phantoms.TTerm Core.WrappedTerm -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.WrappedTerm
wrappedTermWithTypeName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.WrappedTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
