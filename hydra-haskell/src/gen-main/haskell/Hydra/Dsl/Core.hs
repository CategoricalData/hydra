-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.core

module Hydra.Dsl.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

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

annotatedTermAnnotation :: Phantoms.TTerm Core.AnnotatedTerm -> Phantoms.TTerm (M.Map Core.Name Core.Term)
annotatedTermAnnotation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
        Core.projectionField = (Core.Name "annotation")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotatedTermBody :: Phantoms.TTerm Core.AnnotatedTerm -> Phantoms.TTerm Core.Term
annotatedTermBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotatedTermWithAnnotation :: Phantoms.TTerm Core.AnnotatedTerm -> Phantoms.TTerm (M.Map Core.Name Core.Term) -> Phantoms.TTerm Core.AnnotatedTerm
annotatedTermWithAnnotation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
              Core.projectionField = (Core.Name "annotation")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

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

annotatedTypeAnnotation :: Phantoms.TTerm Core.AnnotatedType -> Phantoms.TTerm (M.Map Core.Name Core.Term)
annotatedTypeAnnotation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedType"),
        Core.projectionField = (Core.Name "annotation")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotatedTypeBody :: Phantoms.TTerm Core.AnnotatedType -> Phantoms.TTerm Core.Type
annotatedTypeBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedType"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotatedTypeWithAnnotation :: Phantoms.TTerm Core.AnnotatedType -> Phantoms.TTerm (M.Map Core.Name Core.Term) -> Phantoms.TTerm Core.AnnotatedType
annotatedTypeWithAnnotation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.AnnotatedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedType"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedType"),
              Core.projectionField = (Core.Name "annotation")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

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

applicationArgument :: Phantoms.TTerm Core.Application -> Phantoms.TTerm Core.Term
applicationArgument x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Application"),
        Core.projectionField = (Core.Name "argument")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationFunction :: Phantoms.TTerm Core.Application -> Phantoms.TTerm Core.Term
applicationFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Application"),
        Core.projectionField = (Core.Name "function")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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

applicationTypeArgument :: Phantoms.TTerm Core.ApplicationType -> Phantoms.TTerm Core.Type
applicationTypeArgument x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.ApplicationType"),
        Core.projectionField = (Core.Name "argument")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationTypeFunction :: Phantoms.TTerm Core.ApplicationType -> Phantoms.TTerm Core.Type
applicationTypeFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.ApplicationType"),
        Core.projectionField = (Core.Name "function")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationTypeWithArgument :: Phantoms.TTerm Core.ApplicationType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.ApplicationType
applicationTypeWithArgument original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.ApplicationType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.ApplicationType"),
              Core.projectionField = (Core.Name "function")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.ApplicationType"),
              Core.projectionField = (Core.Name "argument")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

applicationWithArgument :: Phantoms.TTerm Core.Application -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Application
applicationWithArgument original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Application"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Application"),
              Core.projectionField = (Core.Name "function")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Application"),
              Core.projectionField = (Core.Name "argument")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

binding :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Term -> Phantoms.TTerm (Maybe Core.TypeScheme) -> Phantoms.TTerm Core.Binding
binding name term type_ =
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
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

bindingName :: Phantoms.TTerm Core.Binding -> Phantoms.TTerm Core.Name
bindingName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bindingTerm :: Phantoms.TTerm Core.Binding -> Phantoms.TTerm Core.Term
bindingTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
        Core.projectionField = (Core.Name "term")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bindingType :: Phantoms.TTerm Core.Binding -> Phantoms.TTerm (Maybe Core.TypeScheme)
bindingType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

bindingWithTerm :: Phantoms.TTerm Core.Binding -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Binding
bindingWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Binding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

bindingWithType :: Phantoms.TTerm Core.Binding -> Phantoms.TTerm (Maybe Core.TypeScheme) -> Phantoms.TTerm Core.Binding
bindingWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Binding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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

caseStatementCases :: Phantoms.TTerm Core.CaseStatement -> Phantoms.TTerm [Core.Field]
caseStatementCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
        Core.projectionField = (Core.Name "cases")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseStatementDefault :: Phantoms.TTerm Core.CaseStatement -> Phantoms.TTerm (Maybe Core.Term)
caseStatementDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
        Core.projectionField = (Core.Name "default")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseStatementTypeName :: Phantoms.TTerm Core.CaseStatement -> Phantoms.TTerm Core.Name
caseStatementTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
        Core.projectionField = (Core.Name "typeName")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseStatementWithCases :: Phantoms.TTerm Core.CaseStatement -> Phantoms.TTerm [Core.Field] -> Phantoms.TTerm Core.CaseStatement
caseStatementWithCases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.CaseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
              Core.projectionField = (Core.Name "default")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

caseStatementWithDefault :: Phantoms.TTerm Core.CaseStatement -> Phantoms.TTerm (Maybe Core.Term) -> Phantoms.TTerm Core.CaseStatement
caseStatementWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.CaseStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
              Core.projectionField = (Core.Name "cases")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
              Core.projectionField = (Core.Name "default")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
              Core.projectionField = (Core.Name "cases")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

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

eitherTypeLeft :: Phantoms.TTerm Core.EitherType -> Phantoms.TTerm Core.Type
eitherTypeLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.EitherType"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

eitherTypeRight :: Phantoms.TTerm Core.EitherType -> Phantoms.TTerm Core.Type
eitherTypeRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.EitherType"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.EitherType"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

eitherTypeWithRight :: Phantoms.TTerm Core.EitherType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.EitherType
eitherTypeWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.EitherType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.EitherType"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

eliminationRecord :: Phantoms.TTerm Core.Projection -> Phantoms.TTerm Core.Elimination
eliminationRecord x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Elimination"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

eliminationUnion :: Phantoms.TTerm Core.CaseStatement -> Phantoms.TTerm Core.Elimination
eliminationUnion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Elimination"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

eliminationWrap :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Elimination
eliminationWrap x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Elimination"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrap"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

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

fieldName :: Phantoms.TTerm Core.Field -> Phantoms.TTerm Core.Name
fieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Field"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldTerm :: Phantoms.TTerm Core.Field -> Phantoms.TTerm Core.Term
fieldTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Field"),
        Core.projectionField = (Core.Name "term")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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

fieldTypeName :: Phantoms.TTerm Core.FieldType -> Phantoms.TTerm Core.Name
fieldTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.FieldType"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldTypeType :: Phantoms.TTerm Core.FieldType -> Phantoms.TTerm Core.Type
fieldTypeType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.FieldType"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.FieldType"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldTypeWithType :: Phantoms.TTerm Core.FieldType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.FieldType
fieldTypeWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.FieldType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.FieldType"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Field"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldWithTerm :: Phantoms.TTerm Core.Field -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Field
fieldWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Field"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

floatTypeBigfloat :: Phantoms.TTerm Core.FloatType
floatTypeBigfloat =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.FloatType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bigfloat"),
        Core.fieldTerm = Core.TermUnit}}))

floatTypeFloat32 :: Phantoms.TTerm Core.FloatType
floatTypeFloat32 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.FloatType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float32"),
        Core.fieldTerm = Core.TermUnit}}))

floatTypeFloat64 :: Phantoms.TTerm Core.FloatType
floatTypeFloat64 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.FloatType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float64"),
        Core.fieldTerm = Core.TermUnit}}))

floatValueBigfloat :: Phantoms.TTerm Double -> Phantoms.TTerm Core.FloatValue
floatValueBigfloat x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.FloatValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bigfloat"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

floatValueFloat32 :: Phantoms.TTerm Float -> Phantoms.TTerm Core.FloatValue
floatValueFloat32 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.FloatValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float32"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

floatValueFloat64 :: Phantoms.TTerm Double -> Phantoms.TTerm Core.FloatValue
floatValueFloat64 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.FloatValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float64"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

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

forallTypeBody :: Phantoms.TTerm Core.ForallType -> Phantoms.TTerm Core.Type
forallTypeBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.ForallType"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forallTypeParameter :: Phantoms.TTerm Core.ForallType -> Phantoms.TTerm Core.Name
forallTypeParameter x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.ForallType"),
        Core.projectionField = (Core.Name "parameter")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forallTypeWithBody :: Phantoms.TTerm Core.ForallType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.ForallType
forallTypeWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.ForallType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.ForallType"),
              Core.projectionField = (Core.Name "parameter")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.ForallType"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionElimination :: Phantoms.TTerm Core.Elimination -> Phantoms.TTerm Core.Function
functionElimination x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Function"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "elimination"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

functionLambda :: Phantoms.TTerm Core.Lambda -> Phantoms.TTerm Core.Function
functionLambda x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Function"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

functionPrimitive :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Function
functionPrimitive x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Function"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

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

functionTypeCodomain :: Phantoms.TTerm Core.FunctionType -> Phantoms.TTerm Core.Type
functionTypeCodomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.FunctionType"),
        Core.projectionField = (Core.Name "codomain")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionTypeDomain :: Phantoms.TTerm Core.FunctionType -> Phantoms.TTerm Core.Type
functionTypeDomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.FunctionType"),
        Core.projectionField = (Core.Name "domain")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionTypeWithCodomain :: Phantoms.TTerm Core.FunctionType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.FunctionType
functionTypeWithCodomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.FunctionType"),
              Core.projectionField = (Core.Name "domain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.FunctionType"),
              Core.projectionField = (Core.Name "codomain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

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

injectionField :: Phantoms.TTerm Core.Injection -> Phantoms.TTerm Core.Field
injectionField x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
        Core.projectionField = (Core.Name "field")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

injectionTypeName :: Phantoms.TTerm Core.Injection -> Phantoms.TTerm Core.Name
injectionTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
        Core.projectionField = (Core.Name "typeName")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

injectionWithField :: Phantoms.TTerm Core.Injection -> Phantoms.TTerm Core.Field -> Phantoms.TTerm Core.Injection
injectionWithField original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Injection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
              Core.projectionField = (Core.Name "field")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

integerTypeBigint :: Phantoms.TTerm Core.IntegerType
integerTypeBigint =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bigint"),
        Core.fieldTerm = Core.TermUnit}}))

integerTypeInt16 :: Phantoms.TTerm Core.IntegerType
integerTypeInt16 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int16"),
        Core.fieldTerm = Core.TermUnit}}))

integerTypeInt32 :: Phantoms.TTerm Core.IntegerType
integerTypeInt32 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int32"),
        Core.fieldTerm = Core.TermUnit}}))

integerTypeInt64 :: Phantoms.TTerm Core.IntegerType
integerTypeInt64 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int64"),
        Core.fieldTerm = Core.TermUnit}}))

integerTypeInt8 :: Phantoms.TTerm Core.IntegerType
integerTypeInt8 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int8"),
        Core.fieldTerm = Core.TermUnit}}))

integerTypeUint16 :: Phantoms.TTerm Core.IntegerType
integerTypeUint16 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint16"),
        Core.fieldTerm = Core.TermUnit}}))

integerTypeUint32 :: Phantoms.TTerm Core.IntegerType
integerTypeUint32 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint32"),
        Core.fieldTerm = Core.TermUnit}}))

integerTypeUint64 :: Phantoms.TTerm Core.IntegerType
integerTypeUint64 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint64"),
        Core.fieldTerm = Core.TermUnit}}))

integerTypeUint8 :: Phantoms.TTerm Core.IntegerType
integerTypeUint8 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint8"),
        Core.fieldTerm = Core.TermUnit}}))

integerValueBigint :: Phantoms.TTerm Integer -> Phantoms.TTerm Core.IntegerValue
integerValueBigint x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bigint"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

integerValueInt16 :: Phantoms.TTerm I.Int16 -> Phantoms.TTerm Core.IntegerValue
integerValueInt16 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int16"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

integerValueInt32 :: Phantoms.TTerm Int -> Phantoms.TTerm Core.IntegerValue
integerValueInt32 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int32"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

integerValueInt64 :: Phantoms.TTerm I.Int64 -> Phantoms.TTerm Core.IntegerValue
integerValueInt64 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int64"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

integerValueInt8 :: Phantoms.TTerm I.Int8 -> Phantoms.TTerm Core.IntegerValue
integerValueInt8 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int8"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

integerValueUint16 :: Phantoms.TTerm Int -> Phantoms.TTerm Core.IntegerValue
integerValueUint16 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint16"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

integerValueUint32 :: Phantoms.TTerm I.Int64 -> Phantoms.TTerm Core.IntegerValue
integerValueUint32 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint32"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

integerValueUint64 :: Phantoms.TTerm Integer -> Phantoms.TTerm Core.IntegerValue
integerValueUint64 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint64"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

integerValueUint8 :: Phantoms.TTerm I.Int16 -> Phantoms.TTerm Core.IntegerValue
integerValueUint8 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uint8"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

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

lambdaBody :: Phantoms.TTerm Core.Lambda -> Phantoms.TTerm Core.Term
lambdaBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaDomain :: Phantoms.TTerm Core.Lambda -> Phantoms.TTerm (Maybe Core.Type)
lambdaDomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
        Core.projectionField = (Core.Name "domain")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaParameter :: Phantoms.TTerm Core.Lambda -> Phantoms.TTerm Core.Name
lambdaParameter x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
        Core.projectionField = (Core.Name "parameter")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaWithBody :: Phantoms.TTerm Core.Lambda -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Lambda
lambdaWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
              Core.projectionField = (Core.Name "parameter")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
              Core.projectionField = (Core.Name "domain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

lambdaWithDomain :: Phantoms.TTerm Core.Lambda -> Phantoms.TTerm (Maybe Core.Type) -> Phantoms.TTerm Core.Lambda
lambdaWithDomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
              Core.projectionField = (Core.Name "parameter")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
              Core.projectionField = (Core.Name "domain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

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

letBindings :: Phantoms.TTerm Core.Let -> Phantoms.TTerm [Core.Binding]
letBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Let"),
        Core.projectionField = (Core.Name "bindings")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letBody :: Phantoms.TTerm Core.Let -> Phantoms.TTerm Core.Term
letBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Let"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Let"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letWithBody :: Phantoms.TTerm Core.Let -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Let
letWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Let"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Let"),
              Core.projectionField = (Core.Name "bindings")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

literalBinary :: Phantoms.TTerm B.ByteString -> Phantoms.TTerm Core.Literal
literalBinary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalBoolean :: Phantoms.TTerm Bool -> Phantoms.TTerm Core.Literal
literalBoolean x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalFloat :: Phantoms.TTerm Core.FloatValue -> Phantoms.TTerm Core.Literal
literalFloat x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalInteger :: Phantoms.TTerm Core.IntegerValue -> Phantoms.TTerm Core.Literal
literalInteger x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalString :: Phantoms.TTerm String -> Phantoms.TTerm Core.Literal
literalString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalTypeBinary :: Phantoms.TTerm Core.LiteralType
literalTypeBinary =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = Core.TermUnit}}))

literalTypeBoolean :: Phantoms.TTerm Core.LiteralType
literalTypeBoolean =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = Core.TermUnit}}))

literalTypeFloat :: Phantoms.TTerm Core.FloatType -> Phantoms.TTerm Core.LiteralType
literalTypeFloat x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalTypeInteger :: Phantoms.TTerm Core.IntegerType -> Phantoms.TTerm Core.LiteralType
literalTypeInteger x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalTypeString :: Phantoms.TTerm Core.LiteralType
literalTypeString =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = Core.TermUnit}}))

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

mapTypeKeys :: Phantoms.TTerm Core.MapType -> Phantoms.TTerm Core.Type
mapTypeKeys x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.MapType"),
        Core.projectionField = (Core.Name "keys")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mapTypeValues :: Phantoms.TTerm Core.MapType -> Phantoms.TTerm Core.Type
mapTypeValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.MapType"),
        Core.projectionField = (Core.Name "values")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.MapType"),
              Core.projectionField = (Core.Name "values")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

mapTypeWithValues :: Phantoms.TTerm Core.MapType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.MapType
mapTypeWithValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.MapType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.MapType"),
              Core.projectionField = (Core.Name "keys")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

name :: Phantoms.TTerm String -> Phantoms.TTerm Core.Name
name x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

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

pairTypeFirst :: Phantoms.TTerm Core.PairType -> Phantoms.TTerm Core.Type
pairTypeFirst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.PairType"),
        Core.projectionField = (Core.Name "first")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pairTypeSecond :: Phantoms.TTerm Core.PairType -> Phantoms.TTerm Core.Type
pairTypeSecond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.PairType"),
        Core.projectionField = (Core.Name "second")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.PairType"),
              Core.projectionField = (Core.Name "second")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pairTypeWithSecond :: Phantoms.TTerm Core.PairType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.PairType
pairTypeWithSecond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.PairType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.PairType"),
              Core.projectionField = (Core.Name "first")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

projection :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Projection
projection typeName field =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Phantoms.unTTerm field)}]}))

projectionField :: Phantoms.TTerm Core.Projection -> Phantoms.TTerm Core.Name
projectionField x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Projection"),
        Core.projectionField = (Core.Name "field")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionTypeName :: Phantoms.TTerm Core.Projection -> Phantoms.TTerm Core.Name
projectionTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Projection"),
        Core.projectionField = (Core.Name "typeName")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionWithField :: Phantoms.TTerm Core.Projection -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Projection
projectionWithField original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Projection"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

projectionWithTypeName :: Phantoms.TTerm Core.Projection -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Projection
projectionWithTypeName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Projection"),
              Core.projectionField = (Core.Name "field")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

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

recordFields :: Phantoms.TTerm Core.Record -> Phantoms.TTerm [Core.Field]
recordFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Record"),
        Core.projectionField = (Core.Name "fields")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordTypeName :: Phantoms.TTerm Core.Record -> Phantoms.TTerm Core.Name
recordTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.Record"),
        Core.projectionField = (Core.Name "typeName")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordWithFields :: Phantoms.TTerm Core.Record -> Phantoms.TTerm [Core.Field] -> Phantoms.TTerm Core.Record
recordWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Record"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Record"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.Record"),
              Core.projectionField = (Core.Name "fields")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

termAnnotated :: Phantoms.TTerm Core.AnnotatedTerm -> Phantoms.TTerm Core.Term
termAnnotated x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotated"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termApplication :: Phantoms.TTerm Core.Application -> Phantoms.TTerm Core.Term
termApplication x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termEither :: Phantoms.TTerm (Either Core.Term Core.Term) -> Phantoms.TTerm Core.Term
termEither x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "either"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termFunction :: Phantoms.TTerm Core.Function -> Phantoms.TTerm Core.Term
termFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termLet :: Phantoms.TTerm Core.Let -> Phantoms.TTerm Core.Term
termLet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termList :: Phantoms.TTerm [Core.Term] -> Phantoms.TTerm Core.Term
termList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termLiteral :: Phantoms.TTerm Core.Literal -> Phantoms.TTerm Core.Term
termLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termMap :: Phantoms.TTerm (M.Map Core.Term Core.Term) -> Phantoms.TTerm Core.Term
termMap x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termMaybe :: Phantoms.TTerm (Maybe Core.Term) -> Phantoms.TTerm Core.Term
termMaybe x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maybe"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termPair :: Phantoms.TTerm (Core.Term, Core.Term) -> Phantoms.TTerm Core.Term
termPair x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termRecord :: Phantoms.TTerm Core.Record -> Phantoms.TTerm Core.Term
termRecord x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termSet :: Phantoms.TTerm (S.Set Core.Term) -> Phantoms.TTerm Core.Term
termSet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termTypeApplication :: Phantoms.TTerm Core.TypeApplicationTerm -> Phantoms.TTerm Core.Term
termTypeApplication x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeApplication"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termTypeLambda :: Phantoms.TTerm Core.TypeLambda -> Phantoms.TTerm Core.Term
termTypeLambda x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeLambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termUnion :: Phantoms.TTerm Core.Injection -> Phantoms.TTerm Core.Term
termUnion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termUnit :: Phantoms.TTerm Core.Term
termUnit =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))

termVariable :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Term
termVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

termWrap :: Phantoms.TTerm Core.WrappedTerm -> Phantoms.TTerm Core.Term
termWrap x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrap"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeAnnotated :: Phantoms.TTerm Core.AnnotatedType -> Phantoms.TTerm Core.Type
typeAnnotated x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotated"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeApplication :: Phantoms.TTerm Core.ApplicationType -> Phantoms.TTerm Core.Type
typeApplication x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

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

typeApplicationTermBody :: Phantoms.TTerm Core.TypeApplicationTerm -> Phantoms.TTerm Core.Term
typeApplicationTermBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeApplicationTermType :: Phantoms.TTerm Core.TypeApplicationTerm -> Phantoms.TTerm Core.Type
typeApplicationTermType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeApplicationTermWithType :: Phantoms.TTerm Core.TypeApplicationTerm -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.TypeApplicationTerm
typeApplicationTermWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeEither :: Phantoms.TTerm Core.EitherType -> Phantoms.TTerm Core.Type
typeEither x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "either"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeForall :: Phantoms.TTerm Core.ForallType -> Phantoms.TTerm Core.Type
typeForall x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forall"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeFunction :: Phantoms.TTerm Core.FunctionType -> Phantoms.TTerm Core.Type
typeFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

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

typeLambdaBody :: Phantoms.TTerm Core.TypeLambda -> Phantoms.TTerm Core.Term
typeLambdaBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeLambda"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeLambdaParameter :: Phantoms.TTerm Core.TypeLambda -> Phantoms.TTerm Core.Name
typeLambdaParameter x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeLambda"),
        Core.projectionField = (Core.Name "parameter")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeLambdaWithBody :: Phantoms.TTerm Core.TypeLambda -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.TypeLambda
typeLambdaWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeLambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeLambda"),
              Core.projectionField = (Core.Name "parameter")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeLambda"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeList :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type
typeList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeLiteral :: Phantoms.TTerm Core.LiteralType -> Phantoms.TTerm Core.Type
typeLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeMap :: Phantoms.TTerm Core.MapType -> Phantoms.TTerm Core.Type
typeMap x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeMaybe :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type
typeMaybe x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maybe"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typePair :: Phantoms.TTerm Core.PairType -> Phantoms.TTerm Core.Type
typePair x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeRecord :: Phantoms.TTerm [Core.FieldType] -> Phantoms.TTerm Core.Type
typeRecord x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeScheme :: Phantoms.TTerm [Core.Name] -> Phantoms.TTerm Core.Type -> Phantoms.TTerm (Maybe (M.Map Core.Name Core.TypeVariableMetadata)) -> Phantoms.TTerm Core.TypeScheme
typeScheme variables type_ constraints =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeScheme"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Phantoms.unTTerm variables)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTTerm constraints)}]}))

typeSchemeConstraints :: Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm (Maybe (M.Map Core.Name Core.TypeVariableMetadata))
typeSchemeConstraints x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
        Core.projectionField = (Core.Name "constraints")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeSchemeType :: Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm Core.Type
typeSchemeType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeSchemeVariables :: Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm [Core.Name]
typeSchemeVariables x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
        Core.projectionField = (Core.Name "variables")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeSchemeWithConstraints :: Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm (Maybe (M.Map Core.Name Core.TypeVariableMetadata)) -> Phantoms.TTerm Core.TypeScheme
typeSchemeWithConstraints original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeScheme"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
              Core.projectionField = (Core.Name "variables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeSchemeWithType :: Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.TypeScheme
typeSchemeWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeScheme"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
              Core.projectionField = (Core.Name "variables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeSchemeWithVariables :: Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm [Core.Name] -> Phantoms.TTerm Core.TypeScheme
typeSchemeWithVariables original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeScheme"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeSet :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type
typeSet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeUnion :: Phantoms.TTerm [Core.FieldType] -> Phantoms.TTerm Core.Type
typeUnion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeUnit :: Phantoms.TTerm Core.Type
typeUnit =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))

typeVariable :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Type
typeVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeVariableMetadata :: Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Core.TypeVariableMetadata
typeVariableMetadata classes =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeVariableMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Phantoms.unTTerm classes)}]}))

typeVariableMetadataClasses :: Phantoms.TTerm Core.TypeVariableMetadata -> Phantoms.TTerm (S.Set Core.Name)
typeVariableMetadataClasses x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.TypeVariableMetadata"),
        Core.projectionField = (Core.Name "classes")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeVariableMetadataWithClasses :: Phantoms.TTerm Core.TypeVariableMetadata -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Core.TypeVariableMetadata
typeVariableMetadataWithClasses original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.TypeVariableMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "classes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeVoid :: Phantoms.TTerm Core.Type
typeVoid =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "void"),
        Core.fieldTerm = Core.TermUnit}}))

typeWrap :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type
typeWrap x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wrap"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unName :: Phantoms.TTerm Core.Name -> Phantoms.TTerm String
unName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.core.Name")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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

wrappedTermBody :: Phantoms.TTerm Core.WrappedTerm -> Phantoms.TTerm Core.Term
wrappedTermBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

wrappedTermTypeName :: Phantoms.TTerm Core.WrappedTerm -> Phantoms.TTerm Core.Name
wrappedTermTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
        Core.projectionField = (Core.Name "typeName")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

wrappedTermWithBody :: Phantoms.TTerm Core.WrappedTerm -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.WrappedTerm
wrappedTermWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.WrappedTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
              Core.projectionField = (Core.Name "typeName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
